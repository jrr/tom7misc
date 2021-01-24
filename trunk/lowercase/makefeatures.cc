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

static constexpr int MAX_FONTS = 200'000;
static constexpr int ENOUGH_FONTS = 2 * (NUM_EVAL_EXAMPLES / 26);

static bool should_die = false;
static std::shared_mutex should_die_m;

std::shared_mutex print_mutex;
#define Printf(fmt, ...) do {				\
    WriteMutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);				\
    fflush(stdout);					\
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

static void Generate(const std::string &model_filename,
		     int num_features, bool lowercasing) {
  std::unique_ptr<Network> net(Network::ReadNetworkBinary(model_filename));
  CHECK(net.get() != nullptr) << model_filename;

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
  for (;;) {
    {
      ReadMutexLock ml(&load_fonts->fonts_m);
      if (load_fonts->fonts.size() >= ENOUGH_FONTS)
	break;
    }

    Printf("Not enough training data loaded yet!\n");
    std::this_thread::sleep_for(1s);
    if (ReadWithLock(&should_die_m, &should_die))
      return;
  }
  Printf("Training thread has enough fonts to start creating examples.\n");

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
    double mean = 0.0;
    double stdev = 0.0;
  };
  
  vector<ScoredFeature> features;

  printf(ANSI_CLEAR_SCREEN);
    
  for (int feature_num = 0; feature_num < num_features; feature_num++) {
    // Move to beginning of screen
    printf("\x1B[0;0H");

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

    printf("Have %d examples    \n", (int)examples.size());
    
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

    eval_comp.PrintHisto();
    
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
    
    features.push_back({.inputs = std::move(feature),
			.mean = mean_activation,
			.stdev = stdev});
    printf("Now have %d features    \n", (int)features.size());

			    
    // Shuffle and and randomly discard examples so that
    // features aren't always generated from the same set.
    Shuffle(&rc, &examples);
    examples.resize(examples.size() / 2);
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
  for (int i = 0; i < num_features * 0.1; i++) {
    best.insert(ranked[i]);
  }

  // TODO: The features with the highest variance on examples might
  // nonetheless be highly correlated with one another.
  
  // XXX debugs
  static constexpr int SCALE = 2;
  static constexpr int FW = 32;
  static constexpr int FH = 14;
  static constexpr int TOP = 1;
  static constexpr int LEFT = 1;
  static constexpr int RIGHT = 2;
  static constexpr int BOTTOM = 20;
  ImageRGBA img(FW * (width * SCALE + LEFT + RIGHT), FH * (height * SCALE + TOP + BOTTOM));
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
      img.BlendText32(startx, starty + height * SCALE, 0xCCCCCCFF,
		      StringPrintf("%.3f", feature.mean));
      img.BlendText32(startx, starty + height * SCALE + 9, 0x888888FF,
		      StringPrintf("%.3f", feature.stdev));
    }
  }
  img.Save("makefeatures.png");

  eval_comp.PrintHisto();
  
  printf("gen examples: %.2fs\n"
	 "activations: %.2fs\n",
	 gen_examples_ms / 1000.0,
         activate_ms / 1000.0);
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

  Generate("net0.val", 500, true);

  WriteWithLock(&should_die_m, &should_die, true);
  
  return 0;
}

