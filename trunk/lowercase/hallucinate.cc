
#ifdef WIN32
#include <windows.h>
#endif

#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>

#include "timer.h"
#include "font-problem.h"
#include "network.h"
#include "opt/opt.h"
#include "image.h"
#include "threadutil.h"
#include "base/stringprintf.h"
#include "lines.h"



using namespace std;

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

// If true, find the input that maximizes the prediction
// for the target letter, and minimizes others.
// Otherwise find the "perfect input" that is closest
// to the vector [0, 0, ... 0, 1, 0, ... 0].
static constexpr bool FIND_MAX = false;


static constexpr FontProblem::SDFConfig SDF_CONFIG = {};

static constexpr double SQRT_2 =
  1.414213562373095048801688724209698;

// Lower penalty is better.
template<class Pred>
static inline double LetterPenalty(int target_letter,
                                   const Pred &pred) {
  if (FIND_MAX) {
    double score = 0.0;
    for (int i = 0; i < 26; i++) {
      double p = pred(i);
      if (i == target_letter) {
        score += p;
      } else {
        // Other letters should look very unlikely
        score -= p;
      }
    }
    // want lower score
    return -score;
  } else {
    // Distance from [0, .. 0, 1, 0, ... 0]
    double penalty = 0.0;
    for (int i = 0; i < 26; i++) {
      double p = pred(i);
      if (i == target_letter) {
        penalty += fabs(1.0 - p);
      } else {
        penalty += fabs(p);
      }
    }
    return penalty;
  }
}

// Rejects SDFs that are too bright on the edges.
static double BrightPenalty(const vector<double> &inputs) {
  static constexpr int sdf_size = SDF_CONFIG.sdf_size;
  static constexpr double OFF = 0.25;
  double penalty = 0.0;
  auto Look = [&inputs, &penalty](int x, int y) {
      double d = inputs[y * sdf_size + x];
      if (d > OFF) penalty += d - OFF;
    };
  for (int y = 0; y < sdf_size; y++) {
    Look(0, y);
    Look(sdf_size - 1, y);
  }
  // Don't look at corners twice.
  for (int x = 1; x < sdf_size - 1; x++) {
    Look(x, 0);
    Look(x, sdf_size - 1);
  }
  return penalty;
}

// Rejects SDFs that don't have enough "on" pixels, or
// too many of them.
static double BoringPenalty(const vector<double> &inputs) {
  static constexpr int sdf_size = SDF_CONFIG.sdf_size;
  static constexpr double ON = (double)SDF_CONFIG.onedge_value / 255.0;
  static constexpr int ENOUGH = 0.10 * (sdf_size * sdf_size);
  static constexpr int TOO_MUCH = 0.50 * (sdf_size * sdf_size);
  int count = 0;
  for (double d : inputs) if (d >= ON) count++;
  if (count < ENOUGH) return ENOUGH - count;
  else if (count > TOO_MUCH) return count - TOO_MUCH;
  else return 0.0;
}

// Reject SDFs that don't obey the triangle inequality, using
// local checks (just the 9-connected neighbors of each pixel).
// If feasible, returns 0.0.
// Otherwise, a penalty that should reduce as it gets closer
// to the feasible region.
static double TrianglePenalty(const vector<double> &inputs) {
  static constexpr int sdf_size = SDF_CONFIG.sdf_size;
  // maximum falloff difference of two adjacent pixels
  static constexpr double MAX_AXIAL =
    SDF_CONFIG.falloff_per_pixel / 255.0;
  static constexpr double MAX_DIAG =
    (SDF_CONFIG.falloff_per_pixel * SQRT_2) / 255.0;


  double penalty = 0.0;
  // we just check the right, down, and down-right neighbors.
  for (int y = 0; y < sdf_size - 1; y++) {
    for (int x = 0; x < sdf_size - 1; x++) {
      double me = inputs[y * sdf_size + x];
      double r = inputs[y * sdf_size + x + 1];
      double d = inputs[(y + 1) * sdf_size + x];
      double dr = inputs[(y + 1) * sdf_size + x + 1];

      double er = fabs(r - me);
      double ed = fabs(d - me);
      double edr = fabs(dr - me);
      if (er > MAX_AXIAL) penalty += (er - MAX_AXIAL);
      if (ed > MAX_AXIAL) penalty += (ed - MAX_AXIAL);
      if (edr > MAX_DIAG) penalty += (edr - MAX_DIAG);
    }
  }

  return penalty;
}

// Generate input values directly, hoping that it's an SDF,
// with penalties if it is not (or too boring, etc).
static ImageA DirectOptimizeSDF(const Network &net,
                                int target_letter) {
  constexpr int sdf_size = SDF_CONFIG.sdf_size;

  std::function<double(const std::vector<double> &inputs)> IsLetter =
    [&net, target_letter](
        const std::vector<double> &inputs) -> double {

      const double bright_penalty = BrightPenalty(inputs);

      const double boring_penalty = BoringPenalty(inputs);
      // PERF return early? Probably better to adjust both
      // of these at once because they are cheap.

      const double triangle_penalty = TrianglePenalty(inputs);
      // Could add the penalty in, but it's probably
      // better to just be fast so we can do more iterations.
      if (bright_penalty > 0.0 ||
          boring_penalty > 0.0 ||
          triangle_penalty > 0.0) {
        CHECK(bright_penalty >= 0.0);
        CHECK(boring_penalty >= 0.0);
        CHECK(triangle_penalty >= 0.0);
        return
          1000000.0 * (boring_penalty + bright_penalty) +
          1000.0 * triangle_penalty;
      }

      constexpr int sdf_size = SDF_CONFIG.sdf_size;
      CHECK(inputs.size() == sdf_size * sdf_size);
      Stimulation stim{net};
      for (int i = 0; i < sdf_size * sdf_size; i++)
        stim.values[0][i] = inputs[i];
      net.RunForward(&stim);
      const std::vector<float> &output = stim.values.back();
      CHECK(output.size() >= sdf_size * sdf_size + 26);
      // negate, because we want higher predicted values
      printf(".");

      return LetterPenalty(target_letter,
                           [&output](int i) {
                             return output[sdf_size * sdf_size + i];
                           });
    };

  const std::vector<double> lbs(sdf_size * sdf_size, 0.0);
  const std::vector<double> ubs(sdf_size * sdf_size, 1.0);
  Timer optimize_timer;
  const auto [best, score] =
    Opt::Minimize(sdf_size * sdf_size,
                  IsLetter,
                  lbs,
                  ubs,
                  // When looking for 'max g',
                  // The score of the letter we generate
                  // seems to track with about 1/1000 of this.
                  // I think this suggests that we can drive
                  // the prediction really high with lots more
                  // iterations...
                  3600000,
                  1, 20);
  double opt_ms = optimize_timer.MS();

  printf("Result scored %.5f in %.2f\n", -score, opt_ms / 1000.0);

  // Make SDF image of it so that we can render.
  CHECK(best.size() == sdf_size * sdf_size);
  ImageA best_sdf(sdf_size, sdf_size);
  for (int y = 0; y < sdf_size; y++) {
    for (int x = 0; x < sdf_size; x++) {
      int idx = y * sdf_size + x;
      uint8 v = FontProblem::FloatByte(best[idx]);
      best_sdf.SetPixel(x, y, v);
    }
  }

  return best_sdf;
}

// True if the point is inside the given triangle.
// Note this code generalizes to a polygon. Maybe we should
// just optimize a polygon?
static bool PointInside(float x0, float y0,
                        float x1, float y1,
                        float x2, float y2,
                        float x, float y) {
  std::array<std::pair<float, float>, 3> pts =
    {make_pair(x0, y0), make_pair(x1, y1), make_pair(x2, y2)};

  bool odd = false;
  int jdx = 3 - 1;
  for (int idx = 0; idx < 3; idx++) {
    const auto [ix, iy] = pts[idx];
    const auto [jx, jy] = pts[jdx];
    if ((iy > y) != (jy > y)) {
      if (x < ((jx - ix) * (y - iy) / (jy - iy) + ix)) {
        odd = !odd;
      }
    }

    jdx = idx;
  }
  return odd;
}

// Generate an SDF by generating a bitmap from some
// parameterized shapes.
static ImageA DrawBitmapShapes(const Network &net,
                               int target_letter) {
  constexpr int BITMAP_SIZE = 36 * 3;

  constexpr int NUM_TRIANGLES = 6; // 9;
  constexpr int NUM_CIRCLES = 4;
  constexpr int NUM_BOXES = 6; // 4;

  // three vertices
  constexpr int TRIANGLE_SIZE = 3 * 2;
  // center and radius
  constexpr int CIRCLE_SIZE = 2 + 1;
  // two corner vertices
  constexpr int BOX_SIZE = 2 * 2;

  constexpr int TRIANGLE_START = 0;
  constexpr int CIRCLE_START = TRIANGLE_START + NUM_TRIANGLES * TRIANGLE_SIZE;
  constexpr int BOX_START = CIRCLE_START + NUM_CIRCLES * CIRCLE_SIZE;

  constexpr int N = BOX_START + NUM_BOXES * BOX_SIZE;

  auto MakeSDF = [](const std::vector<double> &inputs) {
      // Put in local coordinates. PERF: Could instead just generate
      // in these native coordinates...
      vector<float> scaled_inputs;
      scaled_inputs.reserve(N);
      for (double d : inputs) scaled_inputs.push_back(d * BITMAP_SIZE);

      ImageA bitmap(BITMAP_SIZE, BITMAP_SIZE);

      // Draw the shapes.
      for (int y = 0; y < BITMAP_SIZE; y++) {
        for (int x = 0; x < BITMAP_SIZE; x++) {
          // For each pixel, count how many shapes it's in.
          int count = 0;
          for (int i = 0; i < NUM_TRIANGLES; i++) {
            if (PointInside(
                    scaled_inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 0],
                    scaled_inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 1],
                    scaled_inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 2],
                    scaled_inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 3],
                    scaled_inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 4],
                    scaled_inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 5],
                    x, y)) {
              count++;
            }
          }
          for (int i = 0; i < NUM_CIRCLES; i++) {
            float cx = scaled_inputs[CIRCLE_START + i * CIRCLE_SIZE + 0];
            float cy = scaled_inputs[CIRCLE_START + i * CIRCLE_SIZE + 1];
            // Could precompute the squaring here, or just say
            // the radius is squared to begin with.
            float r = scaled_inputs[CIRCLE_START + i * CIRCLE_SIZE + 2];

            float dx = x - cx;
            float dy = y - cy;
            if (dx * dx + dy * dy < r * r) count++;
          }

          for (int i = 0; i < NUM_BOXES; i++) {
            float x0 = scaled_inputs[BOX_START + i * BOX_SIZE + 0];
            float y0 = scaled_inputs[BOX_START + i * BOX_SIZE + 1];
            float x1 = scaled_inputs[BOX_START + i * BOX_SIZE + 2];
            float y1 = scaled_inputs[BOX_START + i * BOX_SIZE + 3];

            // Allow inside-out rectangles.
            if (x0 > x1) std::swap(x0, x1);
            if (y0 > y1) std::swap(y0, y1);
            if (x >= x0 && x <= x1 &&
                y >= y0 && y <= y1) count++;
          }

          bitmap.SetPixel(x, y, (count & 1) ? 0xFF : 0x00);
        }
      }

      return FontProblem::SDFFromBitmap(SDF_CONFIG, bitmap);
    };

  int num_calls = 0;
  std::function<double(const std::vector<double> &inputs)> IsLetter =
    [&net, target_letter, &MakeSDF, &num_calls](
        const std::vector<double> &inputs) -> double {

      ImageA sdf = MakeSDF(inputs);
      const auto [out_sdf, pred] =
        FontProblem::RunSDFModel(net, SDF_CONFIG, sdf);

      double penalty =
        LetterPenalty(target_letter,
                      [&pred](int i) {
                        return pred[i];
                      });

      num_calls++;
      if (num_calls % 1000 == 0)
        printf("[%c] %d calls, penalty: %.4f\n",
               target_letter + 'a', num_calls, penalty);

      return penalty;
    };

  // All coordinates in [0,1].
  // We could support triangle and circle vertices outside
  // the image, which would give us some more expressive
  // power, but not power that we want (in fact we should
  // probably constrain these to be closer to the origin?)
  const std::vector<double> lbs(N, 0.0);
  const std::vector<double> ubs(N, 1.0);
  Timer optimize_timer;
  const auto [best, penalty] =
    Opt::Minimize(N,
                  IsLetter,
                  lbs,
                  ubs,
                  10000,
                  1, 30);
  double opt_ms = optimize_timer.MS();

  printf("Result penalty %.5f in %.2f\n", penalty, opt_ms / 1000.0);
  CHECK(best.size() == N);
  return MakeSDF(best);
}


// Generate an SDF by generating the SDF directly from
// some parameterized shapes. Should be equivalent to the
// above but just faster.
static ImageA DrawSDFShapes(const Network &net,
                            int target_letter) {
  constexpr int sdf_size = SDF_CONFIG.sdf_size;
  constexpr int NUM_TRIANGLES = 0; // 6; // 9;
  constexpr int NUM_CIRCLES = 0; // 4;
  constexpr int NUM_BOXES = 2; // 6; // 4;

  // three vertices
  constexpr int TRIANGLE_SIZE = 3 * 2;
  // center and radius
  constexpr int CIRCLE_SIZE = 2 + 1;
  // two corner vertices
  constexpr int BOX_SIZE = 2 * 2;

  constexpr int TRIANGLE_START = 0;
  constexpr int CIRCLE_START = TRIANGLE_START + NUM_TRIANGLES * TRIANGLE_SIZE;
  constexpr int BOX_START = CIRCLE_START + NUM_CIRCLES * CIRCLE_SIZE;

  constexpr int N = BOX_START + NUM_BOXES * BOX_SIZE;

  // EVEN_ODD works fine, but I think >0 mode needs some other approach...
  // the interior segments should be ignored for distance sake. It can't
  // just be an operation over independent SDFs either (imagine two
  // rectangles that are very slightly overlapping). I think I'd need
  // to do some CSG stuff before computing this.
  constexpr bool EVEN_ODD = true;

  // Unlike the above, arguments are in [0, sdf_size].
  auto MakeSDF = [](const std::vector<double> &inputs) {
      ImageA sdf(sdf_size, sdf_size);

      // Draw the shapes.
      for (int y = 0; y < sdf_size; y++) {
        for (int x = 0; x < sdf_size; x++) {

          // In even-odd mode, the distance to any edge (each one
          // is a transition.)
          float min_dist = sdf_size * sdf_size * 2;
          // In nonzero mode, we compute the signed distance
          // as we go.
          float signed_dist = 0.0f;
          int count_inside = 0;
          for (int i = 0; i < NUM_TRIANGLES; i++) {
            float x0 = inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 0];
            float y0 = inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 1];
            float x1 = inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 2];
            float y1 = inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 3];
            float x2 = inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 4];
            float y2 = inputs[TRIANGLE_START + i * TRIANGLE_SIZE + 5];

            bool inside = PointInside(x0, y0, x1, y1, x2, y2, x, y);

            if (EVEN_ODD) {
              min_dist = std::min({
                  PointLineDistance(x0, y0, x1, y1, x, y),
                  PointLineDistance(x1, y1, x2, y2, x, y),
                  PointLineDistance(x2, y2, x0, y0, x, y),
                  min_dist});
              if (inside) count_inside++;
            } else {
              // inside any?
              if (inside) {
                // We're inside so distance will be non-negative.
                signed_dist = std::max({
                  PointLineDistance(x0, y0, x1, y1, x, y),
                  PointLineDistance(x1, y1, x2, y2, x, y),
                  PointLineDistance(x2, y2, x0, y0, x, y),
                  signed_dist});
              } else {
                // Otherwise, non-positive.
                signed_dist = std::min({
                    -PointLineDistance(x0, y0, x1, y1, x, y),
                    -PointLineDistance(x1, y1, x2, y2, x, y),
                    -PointLineDistance(x2, y2, x0, y0, x, y),
                    -signed_dist});
              }
            }
          }

          for (int i = 0; i < NUM_CIRCLES; i++) {
            float cx = inputs[CIRCLE_START + i * CIRCLE_SIZE + 0];
            float cy = inputs[CIRCLE_START + i * CIRCLE_SIZE + 1];
            // Could precompute the squaring here, or just say
            // the radius is squared to begin with.
            float r = inputs[CIRCLE_START + i * CIRCLE_SIZE + 2];

            float dx = x - cx;
            float dy = y - cy;
            float dist = sqrtf(dx * dx + dy * dy);

            bool inside = dist < r;

            if (EVEN_ODD) {
              min_dist = std::min({
                  fabs(dist - r),
                  min_dist});
              if (inside) count_inside++;
            } else {
              if (inside) {
                signed_dist = std::max(dist, signed_dist);
              } else {
                signed_dist = std::min(-dist, signed_dist);
              }
            }
          }

          for (int i = 0; i < NUM_BOXES; i++) {
            float x0 = inputs[BOX_START + i * BOX_SIZE + 0];
            float y0 = inputs[BOX_START + i * BOX_SIZE + 1];
            float x1 = inputs[BOX_START + i * BOX_SIZE + 2];
            float y1 = inputs[BOX_START + i * BOX_SIZE + 3];

            // Allow inside-out rectangles.
            if (x0 > x1) std::swap(x0, x1);
            if (y0 > y1) std::swap(y0, y1);

            bool inside = (x >= x0 && x <= x1 &&
                           y >= y0 && y <= y1);

            // PERF: Box distance test could be faster via case
            // analysis for the 9 regions, although it's plausible
            // the compiler can simplify redundant tests in here...
            float dist = std::min({
                PointHorizLineDistance(x0, y0, x1, x, y),
                PointVertLineDistance(x1, y0, y1, x, y),
                PointHorizLineDistance(x1, y1, x0, x, y),
                PointVertLineDistance(x0, y1, y0, x, y)});

            if (EVEN_ODD) {
              min_dist = std::min(dist, min_dist);
              if (inside) count_inside++;
            } else {
              if (inside) {
                signed_dist = std::max(dist, signed_dist);
              } else {
                signed_dist = std::min(-dist, signed_dist);
              }
            }
          }

          float pixel_dist = 0.0f;
          if (EVEN_ODD) {
            pixel_dist = (0 == (count_inside & 1)) ? -min_dist : min_dist;
          } else {
            pixel_dist = signed_dist;
          }

          float falloff = SDF_CONFIG.falloff_per_pixel * pixel_dist;
          uint8 v = std::clamp((int)roundf(SDF_CONFIG.onedge_value + falloff),
                               0, 255);
          sdf.SetPixel(x, y, v);
        }
      }

      return sdf;
    };

  int num_calls = 0;
  std::function<double(const std::vector<double> &inputs)> IsLetter =
    [&net, target_letter, &MakeSDF, &num_calls](
        const std::vector<double> &inputs) -> double {

      ImageA sdf = MakeSDF(inputs);
      const auto [out_sdf, pred] =
        FontProblem::RunSDFModel(net, SDF_CONFIG, sdf);

      double penalty =
        LetterPenalty(target_letter,
                      [&pred](int i) {
                        return pred[i];
                      });

      num_calls++;
      if (num_calls % 1000 == 0)
        printf("[%c] %d calls, penalty: %.4f\n",
               target_letter + 'a', num_calls, penalty);

      return penalty;
    };

  // All coordinates in [0,sdf_size].
  const std::vector<double> lbs(N, 0.0);
  const std::vector<double> ubs(N, (double)sdf_size);
  Timer optimize_timer;
  const auto [best, penalty] =
    Opt::Minimize(N,
                  IsLetter,
                  lbs,
                  ubs,
                  // 10000,
                  // 5000,
                  100,
                  1,
                  10 // 240
                  );
  double opt_ms = optimize_timer.MS();

  printf("Result penalty %.5f in %.2f\n", penalty, opt_ms / 1000.0);
  CHECK(best.size() == N);
  return MakeSDF(best);
}



int main(int argc, char **argv) {
  #ifdef WIN32
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  // XXX make command-line option
  const string model_file = "net0.val";

  std::unique_ptr<Network> net;
  net.reset(Network::ReadNetworkBinary(model_file));

  Timer all_timer;
  std::mutex text_m;
  string text = StringPrintf("From model %s, find %s\n",
                             model_file.c_str(),
                             FIND_MAX ? "max" : "perfect");

  ParallelComp(
      26,
      [&net, &text_m, &text](int target_letter) {
        Timer letter_timer;
        // ImageA best_sdf = DirectOptimizeSDF(*net, target_letter);
        // ImageA best_sdf = DrawBitmapShapes(*net, target_letter);
        ImageA best_sdf = DrawSDFShapes(*net, target_letter);

        // Pass same net twice, we'll just use the "lowercase"
        FontProblem::GenResult result =
          FontProblem::GenImages(SDF_CONFIG, *net, *net, best_sdf, 6);


        ImageRGBA out(result.input.Width(),
                      result.input.Height() * 2 + 12);
        out.BlendImage(0, 0, result.input);
        out.BlendImage(0, result.input.Height(), result.low);

        for (int i = 0; i < 26; i++) {
          const uint8 v = FontProblem::FloatByte(result.low_pred[i]);
          string s = "A";
          s[0] += i;
          out.BlendText(9 * i,
                        result.input.Height() * + 2,
                        v, v, 0xFF, 0xFF,
                        s);
        }


        char c = 'a' + target_letter;

        string filename = StringPrintf("hallucinated-%c.png", c);
        out.Save(filename);
        double letter_ms = letter_timer.MS();

        {
          MutexLock ml(&text_m);
          printf("*** FINISHED '%c' in %.2f sec ***\n",
                 c, letter_ms / 1000.0);
          StringAppendF(&text, "- %c in %.2f sec -\n",
                        c, letter_ms / 1000.0);
          for (int i = 0; i < 26; i++) {
            StringAppendF(&text,
                          "%d. '%c': %.3f%s\n", i, 'a' + i,
                          result.low_pred[i],
                          (i == target_letter) ? " <--" : "");
          }
        }

      },
      6);
  const double all_total_s = (all_timer.MS() / 1000.0);
  const int all_m = all_total_s / 60.0;
  const double all_s = fmod(all_total_s, 60.0);
  string timing = StringPrintf("\nTook %.2f sec = %d m %.2f sec\n",
                               all_total_s, all_m, all_s);
  text += timing;
  printf("%s", timing.c_str());

  Util::WriteFile("hallucinated.txt", text);
  return 0;
}
