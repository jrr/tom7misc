// Experimental code for generating sparse image features that are
// suited to the training data. This can certainly be done generically
// (and more principledly) by the training process on random features,
// but it seems good to start with better quality features?

// This code generates the requested number of features to a file,
// which can then be added to a model with widen.exe (not yet implemented!)

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <unordered_map>
#include <cmath>
#include <chrono>
#include <algorithm>
#include <tuple>
#include <utility>
#include <set>
#include <vector>
#include <map>
#include <unordered_set>
#include <deque>
#include <shared_mutex>

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "util.h"
#include "threadutil.h"
#include "randutil.h"
#include "base/macros.h"
#include "color-util.h"
#include "image.h"
#include "lines.h"

#include "loadfonts.h"
#include "network.h"

#include "timer.h"
#include "font-problem.h"
#include "autoparallel.h"


#define ANSI_RED "\x1B[1;31;40m"
#define ANSI_GREY "\x1B[1;30;40m"
#define ANSI_BLUE "\x1B[1;34;40m"
#define ANSI_CYAN "\x1B[1;36;40m"
#define ANSI_YELLOW "\x1B[1;33;40m"
#define ANSI_GREEN "\x1B[1;32;40m"
#define ANSI_WHITE "\x1B[1;37;40m"
#define ANSI_PURPLE "\x1B[1;35;40m"
#define ANSI_RESET "\x1B[m"
#define ANSI_CLEAR_SCREEN "\x1B[2J\x1B[H"

using namespace std;

using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;

using uint32 = uint32_t;
using uint64 = uint64_t;

static constexpr int NUM_EVAL_EXAMPLES = 26 * 4096;
// static constexpr int NUM_EVAL_EXAMPLES = 26 * 16; // FIXME just to test


static constexpr int ENOUGH_FONTS = 2 * (NUM_EVAL_EXAMPLES / 26);
static constexpr int MAX_FONTS = ENOUGH_FONTS;

// To select features, we generate a bunch of candidates, rank them
// according to some criteria, and then take the best. There are two
// phases:

// First, e.g. for 0.25, we generate 4x the target (whatever's needed
// for the distance step below) number of examples, then keep the best
// 25% of those.
static constexpr float TAKE_BEST_STDDEV = 0.25f;
static_assert(TAKE_BEST_STDDEV > 0.0f && TAKE_BEST_STDDEV <= 1.0f);

// Second, e.g. for 0.10, we generate 10x the target (the requested
// number of output features) and then find 10% of those to maximize
// their distance from one another.
static constexpr float TAKE_BEST_DISTANCE = 0.10f;
static_assert(TAKE_BEST_DISTANCE > 0.0f && TAKE_BEST_DISTANCE <= 1.0f);

static constexpr int NUM_DIST_TRIES = 2000;

static bool should_die = false;
static std::shared_mutex should_die_m;

std::shared_mutex print_mutex;
#define Printf(fmt, ...) do {                           \
    WriteMutexLock Printf_ml(&print_mutex);             \
    printf(fmt, ##__VA_ARGS__);                         \
    fflush(stdout);                                     \
  } while (0);

template<class C>
static void DeleteElements(C *cont) {
  for (auto &elt : *cont) {
    delete elt;
  }
  cont->clear();
}

static uint8 FloatByte(float f) {
  int x = roundf(f * 255.0f);
  return std::clamp(x, 0, 255);
}

static void CheckIsPermutation(const std::vector<int> &perm) {
  int radix = perm.size();
  vector<bool> found(radix, false);
  for (int i = 0; i < perm.size(); i++) {
    int x = perm[i];
    CHECK(x >= 0 && x < radix) << i << " " << x;
    CHECK(!found[x]) << i << " " << x;
    found[x] = true;
  }
  // (which implies found[i] for all i)
}

static std::tuple<uint8, uint8, uint8> FloatColor(float f) {
  if (f > 0.0f) {
    uint8 v = FloatByte(f);
    return {0, v, 20};
  } else {
    uint8 v = FloatByte(-f);
    return {v, 0, 20};
  }
}

static constexpr float ByteFloat(uint8 b) {
  return b * (1.0 / 255.0f);
}

static constexpr FontProblem::SDFConfig SDF_CONFIG;
static constexpr int SDF_SIZE = SDF_CONFIG.sdf_size;

// SDF is square with an edge this length.
static constexpr int INPUT_LAYER_SIZE = SDF_SIZE * SDF_SIZE;

static SDFLoadFonts *load_fonts = nullptr;

struct TrainingExample {
  vector<float> input;
  // in [0, 25].
  char letter;
};

// Generate num_output_features "good" features that are different from one another.
static void Generate(const std::string &model_filename,
                     int num_output_features, bool lowercasing) {
  std::unique_ptr<Network> net(Network::ReadNetworkBinary(model_filename));
  CHECK(net.get() != nullptr) << model_filename;

  const int num_features = num_output_features / (TAKE_BEST_STDDEV * TAKE_BEST_DISTANCE);
  const int num_best_features = num_output_features / TAKE_BEST_STDDEV;

  printf("Need to generate %d candidate features\n", num_features);
  
  ArcFour rc(StringPrintf("%lld,%lld,%lld,%s",
                          (int64)time(nullptr),
                          net->rounds,
                          net->Bytes(),
                          model_filename.c_str()));
  
  CHECK(net->channels[0] == 1) << "Only single-channel inputs are supported";
  const int width = net->width[0];
  const int height = net->height[0];
  printf("Input image is %dx%d\n", width, height);
  
  const int feature_size = net->layers[0].indices_per_node;
  printf("Feature size (=ipn of first layer): %d\n", feature_size);
  
  AutoParallelComp eval_comp(32, 500, false, "autoparallel.eval-feature.txt");

  CHECK(load_fonts != nullptr) << "LoadFonts must be created first.";
  // First, pause until we have enough fonts.
  // XXX since we now just get one batch and keep it, this can be simplified to
  // just wait until we have loaded ENOUGH_FONTS, stop, and use those
  for (;;) {
    int64 num_fonts = 0;
    {
      ReadMutexLock ml(&load_fonts->fonts_m);
      num_fonts = load_fonts->fonts.size();
      if (num_fonts >= ENOUGH_FONTS)
        break;
    }

    Printf("Not enough training data loaded yet (%d/%d)!\n",
           num_fonts, ENOUGH_FONTS);
    std::this_thread::sleep_for(1s);
    if (ReadWithLock(&should_die_m, &should_die))
      return;
  }

  vector<TrainingExample> examples;
  examples.reserve(NUM_EVAL_EXAMPLES);

  double gen_examples_ms = 0.0, activate_ms = 0.0;
  
  auto PopulateExampleFromFont = [&rc](bool lowercase_input,
                                       const SDFLoadFonts::Font &f,
                                       TrainingExample *example) {
      auto FillSDF = [](float *buffer, const ImageA &img) {
          CHECK(img.Width() == SDF_SIZE);
          CHECK(img.Height() == SDF_SIZE);
          for (int y = 0; y < SDF_SIZE; y++) {
            for (int x = 0; x < SDF_SIZE; x++) {
              int idx = y * SDF_SIZE + x;
              buffer[idx] = ByteFloat(img.GetPixel(x, y));
            }
          }
        };

      const int letter = RandTo(&rc, 26);
      example->letter = letter;
      // indices into Font::sdfs, which is a-z then A-Z.
      const int input_idx = lowercase_input ? letter : 26 + letter;
      
      example->input.resize(SDF_SIZE * SDF_SIZE);      
      FillSDF(example->input.data(), f.sdfs[input_idx]);      
    };

  struct ScoredFeature {
    unordered_map<int, float> inputs;
    vector<float> centered_act;
    double mean = 0.0;
    double stdev = 0.0;
  };
  
  vector<ScoredFeature> features;

  printf(ANSI_CLEAR_SCREEN);

  // Use the same vector of examples for each, so that we can compare
  // their ability to distinguish examples.
  while (examples.size() < NUM_EVAL_EXAMPLES) {
    Timer gen_examples;
    TrainingExample example;
    {
      ReadMutexLock ml(&load_fonts->fonts_m);
      const int idx = RandTo(&rc, load_fonts->fonts.size());

      // Here if we are lowercasing, the input should be uppercase.
      PopulateExampleFromFont(!lowercasing, load_fonts->fonts[idx], &example);
    }
    examples.emplace_back(std::move(example));
    gen_examples_ms += gen_examples.MS();
  }
  
  for (int feature_num = 0; feature_num < num_features; feature_num++) {
    // Move to beginning of screen
    // printf("\x1B[0;0H");
    // printf("Have %d examples    \n", (int)examples.size());
    
    // Make a random feature, which is a set of distinct
    // indices mapped to a weight.
    
    std::unordered_map<int, float> feature;
    // TODO: Support multiple geometries here!

    // First thing we try is balanced line segments. We generate two
    // random, nonoverlapping line segments, one positively weighted,
    // and one negative. Since we need to generate feature_size
    // pixels, we accumulate points near each segment so that both
    // sides have about the same number of inputs. The weights are
    // +1/size and -1/size so that the expected total is 0 on random
    // inputs.

    // XXX given that we just do distance tests below, we might get
    // better results by generating float endpoints?
    auto GetTwoLines = [&rc, width, height]() ->
      pair<tuple<int, int, int, int>,
           tuple<int, int, int, int>> {
      for (;;) {
        // First segment.
        int x0 = RandTo(&rc, width);
        int y0 = RandTo(&rc, height);
        int x1 = RandTo(&rc, width);
        int y1 = RandTo(&rc, height);

        /*
          TODO: Consder generating the second segment
          to be the same length?
          int dx = (x1 - x0);
          int dy = (y1 - y0);
          float len = sqrtf(dx * dx + dy * dy);
        */
        
        int x2 = RandTo(&rc, width);
        int y2 = RandTo(&rc, height);
        int x3 = RandTo(&rc, width);
        int y3 = RandTo(&rc, height);   

        if (!LineIntersection(x0, y0, x1, y1,
                              x2, y2, x3, y3).has_value()) {
          return {{x0, y0, x1, y1}, {x2, y2, x3, y3}};
        }
      }
    };

    auto [line1, line2] = GetTwoLines();

    // Order all inputs by their distance to these line segments.
    vector<pair<int, float>> pospts, negpts;
    pospts.reserve(width * height);
    negpts.reserve(width * height);    
    
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int idx = y * width + x;

        float dpos = PointLineDistance(std::get<0>(line1),
                                       std::get<1>(line1),
                                       std::get<2>(line1),
                                       std::get<3>(line1),
                                       x, y);
        float dneg = PointLineDistance(std::get<0>(line2),
                                       std::get<1>(line2),
                                       std::get<2>(line2),
                                       std::get<3>(line2),
                                       x, y);
        pospts.emplace_back(idx, dpos);
        negpts.emplace_back(idx, dneg); 
      }
    }

    auto ByDist = [](const pair<int, float> &a,
                     const pair<int, float> &b) {
        return a.second < b.second;
      };

    // In case of ties, start from a shuffled array to
    // avoid weird biases.
    Shuffle(&rc, &pospts);
    Shuffle(&rc, &negpts);    
    std::sort(pospts.begin(), pospts.end(), ByDist);
    std::sort(negpts.begin(), negpts.end(), ByDist);    
    
    int negleft = feature_size / 2;
    // TODO: I think it would be better if the weights were reduced
    // as we got further from the line, right?
    // With feature_size = 1 this can be infinite, but we won't use it
    // in that case.
    const float negweight = -1 / (float)negleft;
    int posleft = feature_size - negleft;
    const float posweight = 1 / (float)posleft;
    
    int pi = 0, ni = 0;
    while (negleft > 0 || posleft > 0) {
      auto Next = [&feature](int &i, int &left,
                             vector<pair<int, float>> &pts, float weight) {
          if (left > 0) {
            for (;;) {
              CHECK(i < pts.size());
              auto [idx, dist_] = pts[i];
              i++;

              if (feature.find(idx) == feature.end()) {
                feature[idx] = weight;
                left--;
                return;
              }
            }
          }
        };

      Next(pi, posleft, pospts, posweight);
      Next(ni, negleft, negpts, negweight);
    }
   

    // Run the feature on each of the examples, in parallel.
    Timer activate_timer;
    CHECK(!examples.empty());
    vector<float> activations =
      eval_comp.ParallelMap(
          examples,
          [&feature](const TrainingExample &example) {
            float v = 0.0f;
            for (auto [idx, w] : feature) {
              v += example.input[idx] * w;
            }
            return v;
          });

    // eval_comp.PrintHisto();
    
    double mean_activation = 0.0;
    for (float f : activations) mean_activation += f;
    mean_activation /= activations.size();

    double sqerr = 0.0;
    for (float f : activations) {
      double dd = (f - mean_activation);
      sqerr += dd * dd;
    }
    double stdev = sqrt(sqerr);
    activate_ms += activate_timer.MS();

    // Apply bias.
    for (float &a : activations) a -= mean_activation;
    
    features.push_back({.inputs = std::move(feature),
                        .centered_act = std::move(activations),
                        .mean = mean_activation,
                        .stdev = stdev});
    printf("Now have %d features    \n", (int)features.size());
  }

  CHECK(features.size() == num_features);
  // get the ranked ones.
  vector<int> ranked;
  for (int i = 0; i < num_features; i++) ranked.push_back(i);
  // Sort by stdev descending.
  std::sort(ranked.begin(), ranked.end(),
            [&features](int a, int b) {
              return features[a].stdev > features[b].stdev;
            });

  std::unordered_set<int> best;
  for (int i = 0; i < num_best_features; i++) {
    best.insert(ranked[i]);
  }

  printf("Got the best %d features (maximizing std dev)\n",
         (int)best.size());

  auto MakeImage = [width, height](
      const vector<ScoredFeature> &features,
      // highlight items in this set
      const std::unordered_set<int> &best,
      const string &filename) {

      const int num_features = features.size();
      // XXX debugs
      static constexpr int SCALE = 2;
      static constexpr int TOP = 1;
      static constexpr int LEFT = 1;
      static constexpr int RIGHT = 2;
      static constexpr int BOTTOM = 20;
      const int FW = 1920 / (width * SCALE + LEFT + RIGHT);
      const int FH = 1080 / (height * SCALE + TOP + BOTTOM);

      ImageRGBA img(FW * (width * SCALE + LEFT + RIGHT),
                    FH * (height * SCALE + TOP + BOTTOM));
      img.Clear32(0x000000FF);
      for (int fy = 0; fy < FH; fy++) {
        for (int fx = 0; fx < FW; fx++) {
          int fn = fy * FW + fx;
          if (fn >= num_features) continue;
          ImageRGBA fimg(width, height);
          fimg.Clear32(((fx + fy) & 1) ? 0x000020FF : 0x000060FF);

          const ScoredFeature &feature = features[fn];
          for (const auto [idx, weight] : feature.inputs) {
            int x = idx % width;
            int y = idx / width;
            uint32 color = weight > 0 ? 0x00FF00FF : 0xFF0000FF;
            fimg.BlendPixel32(x, y, color);
          }

          int startx = fx * (width * SCALE + LEFT + RIGHT) + LEFT;
          int starty = fy * (height * SCALE + TOP + BOTTOM) + TOP;
          ImageRGBA sfimg = fimg.ScaleBy(SCALE);
          if (best.find(fn) != best.end()) {
            img.BlendBox32(startx - 1, starty - 1, width * SCALE + 2, height * SCALE + 2,
                           0xCC00CCFF, 0xCC00CC7F);
          }
          img.BlendImage(startx, starty, sfimg);
          img.BlendText32(startx, starty + height * SCALE + 1, 0xCCCCCCFF,
                          StringPrintf("%.3f", feature.mean));
          img.BlendText32(startx, starty + height * SCALE + 10, 0x888888FF,
                          StringPrintf("%.3f", feature.stdev));
        }
      }
      img.Save(filename);
    };
    
  MakeImage(features, best, "makefeatures.png");


  // Restrict to the best ones.

  {
    vector<ScoredFeature> tmp;
    for (int idx : best) tmp.push_back(std::move(features[idx]));
    features = std::move(tmp);
  }

  CHECK(features.size() == num_best_features);
  
  // The features with the highest variance on examples might
  // nonetheless be highly correlated with one another (in fact this
  // seems very common). So next, find features that are different
  // from one another, as measured by the distance of their activation
  // vectors for the examples.

  // Pairwise distances. Could store only when a < b, but it just gets
  // complicated for little (?) gain.
  vector<float> dists(num_best_features * num_best_features, 0.0f);
  auto SetDistAt = [num_best_features, &dists](int a, int b, float d) {
      dists[a * num_best_features + b] = d;
    };
  auto DistAt = [num_best_features, &dists](int a, int b) -> float {
      return dists[a * num_best_features + b];
    };
  for (int a = 0; a < num_best_features; a++) {
    // Diagonal already set correctly to 0.0.
    for (int b = 0; b < a; b++) {
      double sqdist = 0.0;
      const vector<float> &aact = features[a].centered_act;
      const vector<float> &bact = features[b].centered_act;      
      CHECK_EQ(aact.size(), bact.size());
      for (int i = 0; i < aact.size(); i++) {
        float d = aact[i] - bact[i];
        sqdist += d * (double)d;
      }

      float dist = sqrt(sqdist);
      SetDistAt(a, b, dist);
      SetDistAt(b, a, dist);
    }
  }

  std::shared_mutex best_m;
  vector<int> best_out;
  double best_dist = 0.0;

  // static constexpr int NUM_THREADS = 6;
  static constexpr int NUM_THREADS = 6;
  static constexpr int TRIES_PER_THREAD = NUM_DIST_TRIES / NUM_THREADS;
  vector<ArcFour *> rcs;
  for (int i = 0; i < NUM_THREADS; i++) rcs.push_back(Substream(&rc, i));

  const int nbf = num_best_features;
  const int nof = num_output_features;
  Timer dist_timer;
  ParallelFan(NUM_THREADS, [&DistAt, &best_m, &best_out, &best_dist, &rcs, nbf, nof](
      int thread_id) {
      CHECK(thread_id < rcs.size());
      ArcFour *rc = rcs[thread_id];
      printf("Thread %d: %02x\n", thread_id, rc->Byte());
      
      for (int iter = 0; iter < TRIES_PER_THREAD; iter++) {
        // Generate a random permutation. The first num_output_features in
        // the permutation is the current selection.

        
        vector<int> perm;
        perm.reserve(nbf);
        for (int i = 0; i < nbf; i++) perm.push_back(i);
        Shuffle(rc, &perm);

        auto GetDist = [&DistAt, &perm, nof]() {
            double dist = 0.0;
            for (int i = 0; i < nof; i++) {
              for (int j = 0; j < i; j++) {
                // ok with i = j
                dist += DistAt(perm[i], perm[j]);
              }
            }
            return dist;
          };
        
        // Compute initial distance.
        double dist = GetDist();

        // Save the permutation if it is the global best, using double-checked lock.
        auto SaveBest = [&best_m, &best_out, &best_dist, nof, &dist, &perm]() {
            if (dist > ReadWithLock(&best_m, &best_dist)) {
              WriteMutexLock ml(&best_m);
              if (dist > best_dist) {
                best_out.clear();
                for (int i = 0; i < nof; i++) {
                  best_out.push_back(perm[i]);
                }
                best_dist = dist;
                printf("New best total distance: %.2f\n", dist);
              } else {
                printf("(lost race %.2f!)\n", dist);
              }
            }
          };

        SaveBest();

        // XXX Now try to hill-climb.

        CheckIsPermutation(perm);
        
        int64 climbed = 0, swapped = 0;
        bool improved = false;
        do {
          improved = false;
          // for each currently selected feature
          for (int src = 0; src < nof; src++) {
            // consider swapping it with unselected ones...
            for (int dst = nof; dst < nbf; dst++) {
              double new_dist = dist;
              // subtract old, add new
              for (int u = 0; u < nof; u++) {
                if (u != src) {
                  new_dist -= DistAt(perm[u], perm[src]);
                  new_dist += DistAt(perm[u], perm[dst]);
                }
              }

              if (new_dist > dist) {
                // better! do it
                if (false)
                  printf("%d Swap #%lld: %d and %d, dist %.2f -> %.2f\n",
                         thread_id, swapped,
                         src, dst,
                         dist, new_dist);
                int old = perm[src];
                perm[src] = perm[dst];
                perm[dst] = old;
                dist = new_dist;
                // continue hill-climbing
                swapped++;

                if (false) {
                  // Sanity checks.
                  CheckIsPermutation(perm);
                  double rdist = GetDist();
                  CHECK(fabs(dist - rdist) < 0.1) << dist << " vs " << rdist;
                }
                
                improved = true;
              } else {
                if (false)
                  printf("%d Swap %d and %d, dist %.2f -> %.2f WORSE\n",
                         thread_id,
                         src, dst,
                         dist, new_dist);
              }
            }
            // printf("[%d] Done src %d/%d %.2f\n", thread_id, src, nof, dist);
          }

          if (improved) SaveBest();
          climbed++;
        } while (improved);

        printf("Thread %d finished %d/%d iters, %lld passes %lld swaps to %.2f\n",
               thread_id, iter + 1, TRIES_PER_THREAD,
               climbed, swapped,
               dist);
      }

      
    });
  const double dist_ms = dist_timer.MS();

  std::unordered_set<int> best_out_set;
  for (int i : best_out) best_out_set.insert(i);
  printf("Max distance was %.2f, with these feature ids:\n", best_dist);
  for (int i : best_out) printf("%d, ", i);
  printf("\n");
  
  MakeImage(features, best_out_set, "distfeatures.png");

  eval_comp.PrintHisto();
  
  Timer make_timer;
  {
    // Now save 'em. We make a fake one-layer network since the features
    // have the same serialization needs as a hidden layer (no coincidence here!)
    vector<int> nn = {width * height, num_output_features};
    vector<int> ii = {feature_size};
    vector<TransferFunction> tfs = {LEAKY_RELU};
    Network save(nn, ii, tfs);
    save.width = {width, num_output_features};
    save.height = {height, 1};
    save.channels = {1, 1};
    save.renderstyle = {RENDERSTYLE_FLAT, RENDERSTYLE_FLAT};
    CHECK(save.layers.size() == 1);
    Network::Layer *layer = &save.layers[0];
    layer->type = LAYER_SPARSE;
    layer->indices.clear();
    layer->weights.clear();
    layer->biases.clear();
    for (int f : best_out) {
      CHECK(f >= 0 && f < features.size());
      const ScoredFeature &feature = features[f];
      // Subtract the mean as the bias, so that it's centered at 0.
      // (Might be better to use median, so we separate # of examples,
      // rather than mass?)
      layer->biases.push_back(-feature.mean);
      // PERF sort 'em? I guess widen.exe will do it.
      for (const auto [idx, weight] : feature.inputs) {
        layer->indices.push_back(idx);
        layer->weights.push_back(weight);
      }
    }
      
    save.ReallocateInvertedIndices();
    Network::ComputeInvertedIndices(&save, 8);
    save.StructuralCheck();
    Network::SaveNetworkBinary(save, "makefeatures.val");    
  }
  const double make_ms = make_timer.MS();
  
  
  printf("gen examples: %.2fs\n"
         "activations: %.2fs\n"
         "dist: %.2fs\n"
         "make/save net: %.2fs\n",
         gen_examples_ms / 1000.0,
         activate_ms / 1000.0,
         dist_ms / 1000.0,
         make_ms / 1000.0);
}

int main(int argc, char **argv) {
  // XXX to cc-lib
  // Try to enable ANSI sequences. mintty supports these well, but we
  // actually compile as a native window application, so we would use
  // windows routines to change the console output (e.g.
  // SetConsoleCursorPosition). In the modern world these are actually
  // deprecated; windows prefers you to use control sequences like ANSI.
  // But these are only enabled if VIRTUAL_TERMINAL_PROCSESING is on.
  HANDLE stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
  DWORD out_mode;
  if (stdout_handle != INVALID_HANDLE_VALUE &&
      GetConsoleMode(stdout_handle, &out_mode)) {
    // TODO: Could save old value and try to restore it on exit?
    out_mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    (void)SetConsoleMode(stdout_handle, out_mode);
  }
  
  // Start loading fonts in background.
  load_fonts = new SDFLoadFonts(
      []() { return ReadWithLock(&should_die_m, &should_die); },
      SDF_CONFIG,
      12,
      MAX_FONTS);

  Generate("net1.val", 100, false);

  WriteWithLock(&should_die_m, &should_die, true);
  
  return 0;
}

