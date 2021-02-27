
#include "font-problem.h"

#include <algorithm>
#include <cmath>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "base/stringprintf.h"

#include "network.h"

#include "image.h"
#include "ttf.h"
#include "threadutil.h"
#include "lines.h"
#include "arcfour.h"
#include "randutil.h"
#include "timer.h"
#include "opt/opt.h"

#include "network-util.h"

using namespace std;

using Contour = TTF::Contour;
using uint32 = uint32_t;

#define EXTRA_CHECKS false

static constexpr int NUM_COLORS = 4;
static uint32 COLORS[NUM_COLORS] = {
  0xFFFF00FF,
  0x00FF00FF,
  0x00FFFFFF,
  0xFF00FFFF,
};

template<class C, class K>
static bool ContainsKey(const C &c, const K &k) {
  return c.find(k) != c.end();
}

static void DrawFloats(const vector<int> &row_max_points,
                       const vector<float> &values,
                       int startx, int starty,
                       int nominal_char_size,
                       ImageRGBA *img) {

  auto DrawPath = [nominal_char_size, img, startx, starty, &values](
      int idx, int num_pts, uint32 color) -> int {

      // The startx/starty values (in the first two slots)
      // are now ignored. We draw a closed loop starting
      // with the last segment's endpoint.
      float x = values[idx + 2 + (num_pts - 1) * 4 + 2];
      float y = values[idx + 2 + (num_pts - 1) * 4 + 3];

      const double sqerr = 1.0f / (nominal_char_size *
                                   nominal_char_size);

      auto Line = [nominal_char_size,
                   img, startx, starty, color](float x1, float y1,
                                               float x2, float y2) {
          img->BlendLine32(
              startx + x1 * nominal_char_size,
              starty + y1 * nominal_char_size,
              startx + x2 * nominal_char_size,
              starty + y2 * nominal_char_size,
              color);
        };

      for (int i = 0; i < num_pts; i++) {
        float cx = values[idx + 2 + i * 4 + 0];
        float cy = values[idx + 2 + i * 4 + 1];
        float dx = values[idx + 2 + i * 4 + 2];
        float dy = values[idx + 2 + i * 4 + 3];

        for (const auto [xx, yy] :
               TesselateQuadraticBezier<double>(
                   x, y, cx, cy, dx, dy, sqerr)) {
          Line(x, y, xx, yy);
          x = xx;
          y = yy;
        }
      }

      return idx + 2 + (num_pts * 4);
    };

  int next_idx = 0;
  for (int i = 0; i < row_max_points.size(); i++) {
    next_idx =
      DrawPath(next_idx, row_max_points[i], COLORS[i % NUM_COLORS]);
  }
}

void FontProblem::RenderVector(const string &font_filename,
                               const Network &net,
                               const vector<int> &row_max_points,
                               const string &out_filename) {

  static constexpr int WIDTH = 1920;
  static constexpr int HEIGHT = 1080;

  static constexpr int LETTER_WIDTH = 71; // 73;
  static constexpr int LETTER_X_MARGIN = 2;
  static constexpr int LETTER_HEIGHT = 84; // 73;
  static constexpr int LETTER_Y_MARGIN = 2;
  static constexpr int LEFT_MARGIN = 12;
  static constexpr int TOP_MARGIN = 28;

  static constexpr int NUM_ITERS = 12;

  TTF ttf{font_filename};
  ImageRGBA img{WIDTH, HEIGHT};

  img.Clear32(0x000000FF);

  // threaded?
  for (int letter = 0; letter < 26; letter++) {
    const int startx =
      LEFT_MARGIN + (LETTER_WIDTH + LETTER_X_MARGIN) * letter;
    const int codepoint = 'A' + letter;
    Stimulation stim{net};
    // We assume the given font fits for evaluation!
    if (!FillVector(&ttf, codepoint, row_max_points,
                    stim.values[0].data())) {
      for (const TTF::Contour &contour :
             TTF::MakeOnlyBezier(ttf.GetContours(codepoint))) {
        printf("FAIL: contour length %d\n", (int)contour.paths.size());
      }
      CHECK(false) << "Eval font doesn't fit in input vector?? "
                   << (char)codepoint;
    }

    for (int iter = 0; iter < NUM_ITERS; iter++) {
      const int starty =
        TOP_MARGIN + iter * (LETTER_HEIGHT + LETTER_Y_MARGIN);
      img.BlendRect32(startx, starty, LETTER_WIDTH, LETTER_HEIGHT,
                      0x222222FF);

      DrawFloats(row_max_points,
                 stim.values[0],
                 startx, starty,
                 LETTER_HEIGHT,
                 &img);

      // (XXX Don't bother if this is the last round)
      if (iter == NUM_ITERS - 1)
        break;

      net.RunForward(&stim);

      vector<float> *input = &stim.values[0];
      const vector<float> &output = stim.values[stim.values.size() - 1];

      for (int i = 0; i < input->size(); i++) {
        (*input)[i] = output[i];
      }
    }
  }

  img.BlendText2x32(LEFT_MARGIN, 4, 0xCCCCCCFF,
                    StringPrintf("Round %lld   Examples %lld   Bytes %lld",
                                 net.rounds, net.examples, net.Bytes()));
  img.Save(out_filename);
}

// XXX maybe should share this code with training?
static void SDFFillVector(
    // Actually, could drop this param, which is just used for sanity checking
    const FontProblem::SDFConfig &config,
    const ImageA &sdf,
    vector<float> *buffer) {
  CHECK(sdf.Width() == config.sdf_size);
  CHECK(sdf.Height() == config.sdf_size);
  CHECK(buffer->size() >= config.sdf_size * config.sdf_size);

  int idx = 0;
  for (int y = 0; y < sdf.Height(); y++) {
    for (int x = 0; x < sdf.Width(); x++) {
      (*buffer)[idx++] = sdf.GetPixel(x, y) / 255.0f;
    }
  }
}

ImageA FontProblem::SDFGetImage(const SDFConfig &config,
                                const vector<float> &buffer) {
  CHECK(buffer.size() >= config.sdf_size * config.sdf_size);
  ImageA img(config.sdf_size, config.sdf_size);
  for (int y = 0; y < config.sdf_size; y++) {
    for (int x = 0; x < config.sdf_size; x++) {
      img.SetPixel(x, y, FloatByte(buffer[y * config.sdf_size + x]));
    }
  }
  return img;
}

ImageA FontProblem::SDFThresholdAA(uint8 onedge_value,
                                   const ImageA &sdf,
                                   int scale) {
  if (scale == 1) {
    // No need for resampling step.
    ImageA ret(sdf.Width(), sdf.Height());
    for (int y = 0; y < ret.Height(); y++) {
      for (int x = 0; x < ret.Width(); x++) {
        uint8 v = sdf.GetPixel(x, y);
        ret.SetPixel(x, y, v >= onedge_value ? 0xFF : 0x00);
      }
    }
    return ret;
  } else {
    // Resample to scaled size.
    ImageA big = sdf.ResizeBilinear(sdf.Height() * scale, sdf.Width() * scale);
    ImageA ret(sdf.Width(), sdf.Height());
    for (int y = 0; y < sdf.Height(); y++) {
      for (int x = 0; x < sdf.Width(); x++) {
        uint32 count = 0;
        for (int xo = 0; xo < scale; xo++) {
          for (int yo = 0; yo < scale; yo++) {
            uint8 v = big.GetPixel(x * scale + xo, y * scale + yo);
            if (v >= onedge_value) count += 0xFF;
          }
        }

        count /= (scale * scale);
        ret.SetPixel(x, y, count);
      }
    }
    return ret;
  }
}

ImageA FontProblem::SDFThresholdAAFloat(float onedge_value,
                                        const ImageA &sdf,
                                        int width,
                                        int height,
                                        int quality,
                                        float gamma) {
  const int scale = quality;
  // Resample to scaled size.
  // PERF we could sample directly from the 8-bit image
  ImageF sdff = ImageF(sdf);
  if (gamma != 1.0f) {
    for (int y = 0; y < sdff.Height(); y++) {
      for (int x = 0; x < sdff.Width(); x++) {
        sdff.SetPixel(x, y, powf(sdff.GetPixel(x, y), gamma));
      }
    }
  }

  ImageF big = sdff.ResizeBilinear(width * scale, height * scale);
  ImageA ret(width, height);
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      uint32 count = 0;
      for (int yo = 0; yo < scale; yo++) {
        for (int xo = 0; xo < scale; xo++) {
          float f = big.GetPixel(x * scale + xo, y * scale + yo);
          if (f >= onedge_value) count += 0xFF;
        }
      }

      uint8 v = std::roundf(count / (float)(scale * scale));
      ret.SetPixel(x, y, v);
    }
  }
  return ret;
}


void FontProblem::RenderSDF(
    const std::string &font_filename,
    const Network &make_lowercase,
    const Network &make_uppercase,
    const SDFConfig &config,
    const std::string &base_out_filename) {
  Timer timer;

  static constexpr int WIDTH = 1920;
  static constexpr int HEIGHT = 1080;

  static constexpr int LETTER_WIDTH = 64;
  static constexpr int LETTER_X_MARGIN = 2;
  static constexpr int LETTER_HEIGHT = 64;
  static constexpr int LETTER_Y_MARGIN = 2;
  static constexpr int LEFT_MARGIN = 12;
  static constexpr int TOP_MARGIN = 28;

  static constexpr int NUM_ITERS = 12;

  TTF ttf{font_filename};

  static constexpr char CHARS[] =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  vector<ImageA> letters;
  letters.resize(26 * 2);
  ParallelComp(26 * 2,
               [&config, &font_filename, &ttf, &letters](int64_t idx) {
                 int c = CHARS[idx];
                 CHECK(c != 0);
                 std::optional<ImageA> sdf =
                   ttf.GetSDF(c, config.sdf_size,
                              config.pad_top, config.pad_bot, config.pad_left,
                              config.onedge_value, config.falloff_per_pixel);
                 CHECK(sdf.has_value()) << font_filename << "char " << (char)c;
                 letters[idx] = std::move(sdf.value());
               }, 13);

  printf("Generated SDFs for %s in %.2fs\n",
         font_filename.c_str(), timer.MS() / 1000.0);

  auto Iterative = [&config, &letters](const Network &net, bool lowercasing,
                                       const string &outfile) {
      ImageRGBA img{WIDTH, HEIGHT};
      img.Clear32(0x000000FF);

      std::mutex loss_m;
      double total_loss = 0.0;

      // This runs in parallel, so be careful that drawing accesses disjoint
      // pixels per letter.
      ParallelComp
        (26,
         [&config, &letters, &net, lowercasing,
          &img, &loss_m, &total_loss](int64_t letter) {
           double letter_loss = 0.0;
           const int startx =
             LEFT_MARGIN + (LETTER_WIDTH + LETTER_X_MARGIN) * letter;

           [[maybe_unused]]
             const int codepoint = (lowercasing ? 'A' : 'z') + letter;
           const int letter_idx = (lowercasing ? 26 : 0) + letter;
           const int expected_idx = (lowercasing ? 0 : 26) + letter;
           Stimulation stim{net};
           SDFFillVector(config, letters[letter_idx], &stim.values[0]);

           // fill just the input portion; don't worry about the 26
           // letter predictors
           vector<float> expected;
           expected.resize(stim.values[0].size());
           SDFFillVector(config, letters[expected_idx], &expected);


           for (int iter = 0; iter < NUM_ITERS; iter++) {
             const int starty =
               TOP_MARGIN + iter * (LETTER_HEIGHT + LETTER_Y_MARGIN);
             /*
               img.BlendRect32(startx, starty, LETTER_WIDTH, LETTER_HEIGHT,
               0x222222FF);
             */

           // In this iterated eval, the uppercase values definitely
           // get much larger than the onedge value, and maybe
           // symmetrically for the lowercasing. So we could imagine
           // some kind of scaling of the SDF's brightness is
           // happening in both directions. I guess the real training
           // examples should dampen this from getting out of control,
           // but there are stable ways for the uppercasing and
           // lowercasing networks to just agree on some scaling
           // outside of the "real" examples. Anyway, we may find
           // that the shapes are more interesting if we tune the
           // threshold when iterating. This is probably best done
           // on the native floats than the uint8s though.

           // {(uint8)(config.onedge_value * 1.05), 0xFFFF00FF}

           auto GetLayers = [&config, lowercasing, letter, iter]() {
               // Could tweak this separtely for upper- and lowercase
               // models, iteration number...
               // float th1 = pow(0.95, iter);
               // float th2 = pow(0.975, iter);
               float th1 = 0.95f;
               float th2 = 0.975f;

               return vector<pair<float, uint32>>
                 {{config.onedge_value * th1 / 255.0f, 0x440000FF},
                  {config.onedge_value * th2 / 255.0f, 0x66229FFF},
                  {config.onedge_value / 255.0f, 0xFFFFFFFF}};
             };

           const vector<pair<float, uint32>> layers = GetLayers();


           ImageA sdf = SDFGetImage(config, stim.values[0]);

           ImageRGBA thresh = ThresholdImageMulti(sdf, layers,
                                                  config.sdf_size,
                                                  config.sdf_size,
                                                  4, 1.0f);

           img.BlendImage(startx, starty, thresh);

           // (Don't bother if this is the last round)
           if (iter == NUM_ITERS - 1)
             break;

           net.RunForward(&stim);

           vector<float> *input = &stim.values[0];
           const vector<float> &output = stim.values[stim.values.size() - 1];

           // We only have expected results (from the original font) for the
           // first iteration.
           if (iter == 0) {
             CHECK(output.size() >= expected.size());
             for (int i = 0; i < expected.size(); i++) {
               letter_loss += fabs(output[i] - expected[i]);
             }
           }

           for (int i = 0; i < input->size(); i++) {
             (*input)[i] = output[i];
           }
         }

         {
           MutexLock ml(&loss_m);
           total_loss += letter_loss;
         }
       }, 4);

      img.BlendText2x32(
          LEFT_MARGIN, 4, 0xAAAACCFF,
          StringPrintf(
              "Round %lld   Examples %lld   Bytes %lld  Loss %.4f  (Make %s)",
              net.rounds, net.examples, net.Bytes(),
              total_loss,
              lowercasing ? "lowercase" : "uppercase"));

      img.Save(outfile);
    };

  // These are slow so do them in parallel.
  std::thread lthread(
      [&]() {
        Iterative(make_lowercase, true, base_out_filename + ".lower.png");
      });
  Iterative(make_uppercase, false, base_out_filename + ".upper.png");
  lthread.join();
  printf("Evaluated (to %s.(upp,low)er.png) in %.2fs\n",
         base_out_filename.c_str(), timer.MS() / 1000.0);
}


bool FontProblem::GetRows(const TTF *ttf, int codepoint,
                          const std::vector<int> &row_max_points,
                          std::vector<TTF::Contour> *contours) {
  *contours =
    TTF::MakeOnlyBezier(
        TTF::NormalizeOrder(ttf->GetContours(codepoint),
                            0.0f, 0.0f));

  // Only room for three contours.
  if (contours->size() > row_max_points.size())
    return false;

  // Also don't allow empty characters.
  if (contours->empty())
    return false;

  auto ByPathSizeDesc = [](const Contour &a, const Contour &b) {
      return b.paths.size() < a.paths.size();
    };

  std::sort(contours->begin(), contours->end(), ByPathSizeDesc);

  for (int i = 0; i < contours->size(); i++) {
    if ((*contours)[i].paths.size() > row_max_points[i]) {
      return false;
    }
  }

  // We need something to put in rows if there are fewer than
  // the maximum number of contours.
  // We treat this as an empty path starting at 0,0.
  while (contours->size() < row_max_points.size()) {
    TTF::Contour degenerate;
    contours->push_back(degenerate);
  }

  return true;
}

bool FontProblem::FillVector(const TTF *ttf, int codepoint,
                             const std::vector<int> &row_max_points,
                             float *buffer) {

  std::vector<TTF::Contour> contours;
  if (!GetRows(ttf, codepoint, row_max_points, &contours))
    return false;


  // All right, all is well!

  // TODO: Consider random transform of input data; ideal network
  // should be robust against small translations, scaling, even
  // rotation.

  auto PopulateRow = [buffer](int row_start,
                              // Not including start position.
                              int max_pts,
                              const Contour &contour) {
      constexpr int HDR = 2;
      buffer[row_start + 0] = contour.StartX();
      buffer[row_start + 1] = contour.StartY();
      for (int i = 0; i < max_pts; i++) {
        // When we run out of real points, pad with degenerate
        // zero-length curves at the start point.
        float cx = i < contour.paths.size() ?
                       contour.paths[i].cx : contour.StartX();
        float cy = i < contour.paths.size() ?
                       contour.paths[i].cy : contour.StartY();
        float x = i < contour.paths.size() ?
                      contour.paths[i].x : contour.StartX();
        float y = i < contour.paths.size() ?
                      contour.paths[i].y : contour.StartY();

        buffer[row_start + HDR + i * 4 + 0] = cx;
        buffer[row_start + HDR + i * 4 + 1] = cy;
        buffer[row_start + HDR + i * 4 + 2] = x;
        buffer[row_start + HDR + i * 4 + 3] = y;
      }

      return row_start + HDR + 4 * max_pts;
    };


  int next_idx = 0;
  for (int i = 0; i < contours.size(); i++) {
    next_idx = PopulateRow(next_idx, row_max_points[i], contours[i]);
  }

  return true;
}

void FontProblem::FillExpectedVector(
    ArcFour *rc,
    const std::vector<int> &row_max_points,
    const std::vector<TTF::Contour> &expected_contours,
    const std::vector<float> &predicted,
    std::vector<float> *buffer) {

  #if 0
  printf("FillExpectedVector. RMP: ");
  for (int i : row_max_points) {
    printf(" %d", i);
  }
  printf("\n%d contours, %d predicted, %d buffer",
         (int)expected_contours.size(),
         (int)predicted.size(),
         (int)buffer->size());
  fflush(stdout);
  #endif

  if (EXTRA_CHECKS) {
    // Mark as -inf so we can check everything gets initialized.
    int size = BufferSizeForPoints(row_max_points);
    CHECK(buffer->size() >= size);
    for (int i = 0; i < size; i++) {
      (*buffer)[i] = -std::numeric_limits<float>::infinity();
    }
  }

  CHECK_EQ(expected_contours.size(), row_max_points.size());
  // Each row is independent.
  for (int row = 0; row < row_max_points.size(); row++) {
    CHECK(row < expected_contours.size());
    const TTF::Contour &contour = expected_contours[row];
    // printf("  Contour %d\n", row); fflush(stdout);

    // This is the index of the points in both the predicted
    // and parallel output buffers.
    int start_idx = 0;
    for (int j = 0; j < row; j++)
      start_idx += 2 + row_max_points[j] * 4;

    // The path can actually be empty. In this case the
    // values should all be zero.
    if (contour.paths.empty()) {
      // .. but again allow the start point to be anything.
      (*buffer)[start_idx + 0] = predicted[start_idx + 0];
      (*buffer)[start_idx + 1] = predicted[start_idx + 1];

      for (int a = 0; a < row_max_points[row]; a++) {
        const int idx = start_idx + 2 + a * 4;
        (*buffer)[idx + 0] = 0.0f;
        (*buffer)[idx + 1] = 0.0f;
        (*buffer)[idx + 2] = 0.0f;
        (*buffer)[idx + 3] = 0.0f;
      }

      continue;
    }

    // Otherwise, the font is expected to be a closed loop.
    // We then ignore the start point.
    CHECK(contour.paths.back().x == contour.StartX());
    CHECK(contour.paths.back().y == contour.StartY());

    // Put in Point format for BestLoopAssignment.
    vector<Point> expected;
    for (int j = 0; j < contour.paths.size(); j++) {
      CHECK(j < contour.paths.size());
      expected.emplace_back(contour.paths[j].x,
                            contour.paths[j].y);
    }

    // And now the actual points, from the predictions.

    // Again we actually ignore the start coordinates.
    vector<Point> actual;
    // actual.emplace_back(predicted[start_idx], predicted[start_idx + 1]);
    for (int j = 0; j < row_max_points[row]; j++) {
      const int idx = start_idx + 2 + 4 * j;
      CHECK(idx + 3 < predicted.size());
      // Skip the control points for this step.
      actual.emplace_back(predicted[idx + 2],
                          predicted[idx + 3]);
    }

    CHECK(expected.size() <= actual.size())
      << expected.size() << " " << actual.size();

    // printf("  BestLoopAssignment...\n"); fflush(stdout);
    LoopAssignment assn = BestLoopAssignment(rc, expected, actual);
    // printf("  Got assn...\n"); fflush(stdout);

    // Now we want to populate the "expected" results in the
    // output buffer, but rotated and padded to be favorable to the
    // order that was predicted.

    // We don't actually use the start coordinates in the output
    // any more. So just copy the predicted values (whatever they are)
    // so that we don't penalize "mistakes" here.
    (*buffer)[start_idx + 0] = predicted[start_idx + 0];
    (*buffer)[start_idx + 1] = predicted[start_idx + 1];

    // Output parallels the structure of the prediction.
    // The location of the endpoint in the buffer.
    auto PointIdx = [start_idx](int a) {
        return start_idx +
          // skip start points
          2 +
          // control*2, coord*2
          (4 * a) + 2;
      };

    // The location of the control point leading into the
    // point a.
    auto ControlIdx = [start_idx](int a) {
        return start_idx +
          // skip start points
          2 +
          (4 * a) + 0;
      };

    // a will be the index into the actual points.
    int a = assn.point0;
    for (int e = 0; e < expected.size(); e++) {
      // In the general case, several actual points are mapped
      // to this expected point.

      // Sanity check that we're looking at the right point.
      CHECK(e < contour.paths.size()) << e;
      CHECK_EQ(expected[e].first, contour.paths[e].x);
      CHECK_EQ(expected[e].second, contour.paths[e].y);
      const float expected_x = contour.paths[e].x;
      const float expected_y = contour.paths[e].y;
      const float expected_cx = contour.paths[e].cx;
      const float expected_cy = contour.paths[e].cy;

      const int num = assn.groups[e];
      for (int i = 0; i < num; i++) {
        CHECK(a >= 0 && a < actual.size()) << a;
        // The expected location of the point is just the
        // point it's mapped to.
        const int pidx = PointIdx(a);
        (*buffer)[pidx + 0] = expected_x;
        (*buffer)[pidx + 1] = expected_y;

        const int cidx = ControlIdx(a);
        if (i == 0) {
          // The first point in each group gets a proper
          // control point. It is the control point from
          // the corresponding expected point.
          (*buffer)[cidx + 0] = expected_cx;
          (*buffer)[cidx + 1] = expected_cy;
        } else {
          // Duplicates should just use the point itself
          // as the control point, since they represent a
          // 0-length curve.
          (*buffer)[cidx + 0] = expected_x;
          (*buffer)[cidx + 1] = expected_y;
        }

        // Advance actual index and wrap around.
        a++;
        if (a == actual.size()) a = 0;
      }

      // printf("  ... finished contour\n"); fflush(stdout);
    }
  }

  if (EXTRA_CHECKS) {
    // Mark as -inf so we can check everything gets initialized.
    int size = BufferSizeForPoints(row_max_points);
    CHECK(buffer->size() >= size);
    for (int i = 0; i < size; i++) {
      CHECK((*buffer)[i] !=
            -std::numeric_limits<float>::infinity()) << i;
    }
  }
}

int FontProblem::BufferSizeForPoints(const std::vector<int> &row_max_points) {
  int size = 0;
  for (int r : row_max_points)
    size += 2 + (r * 4);
  return size;
}

// Initially tried doing this exactly with dynamic programming. But it
// didn't work out (core issue is that it's not just a matter of
// computing the "best error" for each cell, but also what the
// next_point is. But there is no clear best choice, since as you use
// the solution to that subproblem you may find points that are very
// close to a next_point that you didn't choose. So there are some
// non-local effects.)
//
// We don't need the solution to be exact, and even with an exact
// solution to that mapping problem, the final result would not have
// been, anyway.
//
// New approach is designed to be fast and heuristic. Try out random
// assignments and improve them locally (moving any point to a neighbor)
// until we can't any more. Return the best one.

using Point = FontProblem::Point;
static float SqDistance(const Point &a, const Point &b) {
  const float dx = a.first - b.first;
  const float dy = a.second - b.second;
  return dx * dx + dy * dy;
}


FontProblem::LoopAssignment
FontProblem::BestLoopAssignment(ArcFour *rc,
                                const vector<Point> &expected,
                                const vector<Point> &actual) {
  const int num_expected = expected.size();
  const int num_actual = actual.size();

  // Since we want to find the overall best match, we need to do
  // |e|*|a| distance computations, so we might as well cache that
  // distance matrix.
  vector<float> distances(num_expected * num_actual, 0.0f);
  auto DistanceAt = [&distances, num_expected](int e, int a) -> float & {
      return distances[num_expected * a + e];
    };

  // For each expected point, its single closest actual point.
  vector<int> closest_a;
  closest_a.reserve(num_expected);
  for (int e = 0; e < num_expected; e++) {
    int ca = -1;
    float closest_dist = std::numeric_limits<float>::infinity();
    for (int a = 0; a < num_actual; a++) {
      const float dist = sqrtf(SqDistance(expected[e], actual[a]));
      DistanceAt(e, a) = dist;
      if (dist < closest_dist) {
        ca = a;
      }
    }
    CHECK(ca >= 0) << ca;
    closest_a.push_back(ca);
  }

  auto Score = [&actual, &expected, &DistanceAt](
      const LoopAssignment &assn) -> float {
      // here we have like
      //        0       1 2
      //        x       y z      <- expected
      //    a b c d e f g h i j  <- actual
      //    0 1 2 3 4 5 6 7 8 9
      // This would be represented with point0 = 2,
      // and groups = {4, 1, 5}.

      if (EXTRA_CHECKS) {
        int total = 0;
        for (int g : assn.groups) total += g;
        CHECK(total == actual.size()) << total << " " << actual.size();
      }

      float err = 0.0f;
      int a = assn.point0;
      for (int e = 0; e < expected.size(); e++) {
        int num = assn.groups[e];
        for (int i = 0; i < num; i++) {
          err += DistanceAt(e, a);
          a++;
          if (a == actual.size()) a = 0;
        }
      }
      return err;
    };


  static constexpr int NUM_ATTEMPTS = 10;

  // Overall best seen.
  LoopAssignment best_assignment{(int)expected.size()};
  float best_error = std::numeric_limits<float>::infinity();

  // (This might be overkill. It wasn't working well, but it was
  // actually just a typo in the distance function.)
  for (int anchor = 0; anchor < num_actual; anchor++) {
    // Try each rotation of the actual loop NUM_ATTEMPTS times.
    for (int attempt = 0; attempt < NUM_ATTEMPTS; attempt++) {
      LoopAssignment assn{(int)expected.size()};
      assn.point0 = anchor;

      // The assignment is initialized to all ones. Distribute
      // the extra so that the sum is the same size as expected.
      int extra = actual.size() - expected.size();
      while (extra--)
        assn.groups[RandTo32(rc, assn.groups.size())]++;


      float current_score = Score(assn);
      bool improved = false;
      do {
        improved = false;
        // Iteratively try to improve by moving points to neighbors.
        for (int i = 0; i < assn.groups.size(); i++) {
          const int next_i = i < assn.groups.size() - 1 ? i + 1 : 0;

          // PERF: This inner loop could be much more efficient if
          // we updated the score incrementally (it should only affect
          // the two points), but it's fiddly.

          // Move point forward.
          if (assn.groups[i] > 1) {
            assn.groups[i]--;
            assn.groups[next_i]++;
            // float new_score = ScorePlus(current_score, assn, i, next_i);
            float new_score = Score(assn);
            if (new_score < current_score) {
              improved = true;
              current_score = new_score;
              // No point in trying to move mass backward then, because
              // we just put it there.
              continue;
            } else {
              // Undo.
              assn.groups[i]++;
              assn.groups[next_i]--;
            }
          }

          // And backward.
          if (assn.groups[next_i] > 1) {
            assn.groups[i]++;
            assn.groups[next_i]--;

            float new_score = Score(assn);
            if (new_score < current_score) {
              improved = true;
              current_score = new_score;
            } else {
              // Undo.
              assn.groups[i]--;
              assn.groups[next_i]++;
            }
          }
        }

      } while (improved);


      // Local maximum.
      if (current_score < best_error) {
        best_assignment = assn;
        best_error = current_score;
      }
    }
  }

  return best_assignment;
}

// Call 1 white and 0 black, with this image being a white letter on
// a black background.
//
// Putting aside efficiency, this is easier than the same computation
// from vectors. For each pixel in the output, find its corresponding
// point in the input bitmap. Get that color. Now, find the closest
// pixel of the opposite color (treating the outside edge as black).
// This gives us the distance to the edge. The sign is determined by
// the color of the start pixel.
ImageA FontProblem::SDFFromBitmap(const SDFConfig &config,
                                  const ImageA &img) {

  const int sdf_size = config.sdf_size;
  CHECK(img.Width() == img.Height());
  const int size = img.Width();
  const float scale = size / (float)sdf_size;
  if (false)
    printf("%d img to %d SDF; Scale: %.3f\n",
           size,
           sdf_size,
           scale);

  // true = white = inside letter
  auto Color = [&img, size](int x, int y) {
      // XXX or an explicit bounds test outside?
      if (x < 0 || y < 0 || x >= size || y >= size) return false;
      return img.GetPixel(x, y) > 0;
    };

  // PERF: It basically comes down to wanting this fast function from
  // a pixel to its closest pixel of the opposite color.
  // for doing this faster:
  //
  //  - use a quadtree, which lets us reject a lot of the space
  //    quickly
  //  - fill in the table, but when we reach a pixel with a known
  //    value (that's the same color as us; otherwise we'd be done
  //    anyway), we can use its closest pixel to establish a new
  //    bound using the triangle inequality.

  auto GetSqDistanceTo = [&Color, size](int x, int y, bool c,
                                        int squared_bound) -> int {
      int min_sqdist = squared_bound;

      // we want to also search 1 pixel beyond the edge
      const int ysize = std::max(size - y, y + 1);
      const int xsize = std::max(size - x, x + 1);

      // Instead of scanning from top to bottom, use larger and larger
      // offsets but try both positive and negative. Goal is to be
      // able to exit when the pixels being tested must be outside
      // our current bound.
      for (int dy = 0; dy <= ysize; dy++) {
        const int dys = dy * dy;
        // dy is always getting bigger; if we already found a pixel
        // closer than this distance, just counting the vertical,
        // we can't improve.
        if (dys >= min_sqdist) break;

        // We'll explore dy and dx in both direction.
        // Note this harmlessly tests pixels twice when dy or dx is
        // 0, but it seems better to avoid the branching?
        // Seems to be a better tradeoff to do the outer loop out
        // here so we can skip the entire x loop sometimes (when
        // outside the image entirely).
        for (int sy : {-dy, +dy}) {
          int yy = y + sy;
          if (yy < -1 || yy > size) continue;

          for (int dx = 0; dx <= xsize; dx++) {
            const int dxs = dx * dx;
            // can do a similar test here but it is not
            // faster?
            if (dys + dxs >= min_sqdist) break;

            // might be faster to check bounds here rather
            // than in Color, although it gets a bit gross.
            for (int sx : {-dx, +dx}) {
              const int xx = x + sx;
              if (Color(xx, yy) == c) {
                if (dys + dxs < min_sqdist) {
                  min_sqdist = dys + dxs;
                }
              }
            }
          }
        }
      }
      // CHECK(min_sqdist <= squared_bound) <<
      //     min_sqdist << " vs " << squared_bound;
      return min_sqdist;
    };

  const float oscale = 1.0f / scale;
  ImageA sdf(sdf_size, sdf_size);

  // As we populate the SDF, we can use the previous computed pixel
  // (that was searching for the same color) to give us a bound on the
  // search. OK for this to span rows, although then the bounds get
  // momentarily bad. Start with "infinite" though.
  const int MAX_DIST = size * 2;
  struct Last {
    // Position of the last pixel for which we
    // got a distance, for the given color.
    int x, y;
    // The actual distance we computed for that pixel.
    float dist;
  };
  std::array<Last, 2> last = {Last{.x = 0, .y = 0, .dist = (float)MAX_DIST},
                              Last{.x = 0, .y = 0, .dist = (float)MAX_DIST}};

  for (int sy = 0; sy < sdf_size; sy++) {
    for (int sx = 0; sx < sdf_size; sx++) {
      // Sample the center of the pixel.
      // (This seems to work worse--shifts the
      // image relative to what we get if we
      // generate an SDF from the vector data for
      // the same image. Models unfortunately seem
      // to be very sensitive to position, too.)
      // int ix = roundf((sx + 0.5f) * scale);
      // int iy = roundf((sy + 0.5f) * scale);

      // .. so sample the top-left corner of the pixel.
      int ix = roundf((float)sx * scale);
      int iy = roundf((float)sy * scale);

      const bool color = Color(ix, iy);

      // Whatever we computed for the last pixel of the same color
      // gives us a bound using the triangle inequality (worst case
      // is our distance to that pixel, plus its distance to the
      // goal).
      Last &prev = last[color ? 1 : 0];
      int dx = ix - prev.x;
      int dy = iy - prev.y;
      float bound = prev.dist + sqrtf(dx * dx + dy * dy);
      int sq_bound = ceilf(bound * bound);
      // printf("dx %d dy %d prev %.2f bound %.2f sq %d\n",
      // dx, dy, prev.dist, bound, sq_bound);

      const int sqdist = GetSqDistanceTo(ix, iy, !color, sq_bound);
      const float dist = sqrtf(sqdist);

      // Save distance for next time we search this color.
      prev.x = ix;
      prev.y = iy;
      prev.dist = dist;

      // Distance in sdf space.
      const float sdf_dist = dist * oscale * config.falloff_per_pixel;

      const float signed_dist = color ? sdf_dist : -sdf_dist;
      int value = roundf((float)config.onedge_value + signed_dist);
      sdf.SetPixel(sx, sy, std::clamp(value, 0, 255));
    }
  }

  return sdf;
}


ImageA FontProblem::SDF36From8x8(Image8x8 img) {
  constexpr int sdf_size = 36;
  // nominal size of image.
  constexpr int size = 18;

  constexpr float FALLOFF_PER_PIXEL = 15.0f;
  constexpr uint8 ONEDGE = 220u;
  constexpr float scale = size / (float)sdf_size;

  constexpr int LEFT = 4;
  constexpr int TOP = 6;
  constexpr int WIDTH = 8;
  constexpr int HEIGHT = 8;
  if (false)
    printf("%d img to %d SDF; Scale: %.3f\n",
           size,
           sdf_size,
           scale);

  // true = white = inside letter
  auto Color = [&img](int x, int y) {
      // XXX or an explicit bounds test outside?
      if (x < LEFT || y < TOP ||
          x >= (LEFT + WIDTH) ||
          y >= (TOP + HEIGHT)) return false;
      return img.GetPixel(x - LEFT, y - TOP) > 0;
    };


  // Only need to search from LEFT - 1 to LEFT - 1 + WIDTH + 2, etc.
  auto GetSqDistanceTo = [&Color, size](int x, int y, bool c,
                                        int squared_bound) -> int {
      int min_sqdist = squared_bound;

      // min and max (inclusive) ranges to search for pixels.
      const int min_x = LEFT - 1;
      const int max_x = LEFT + WIDTH + 1;
      const int min_y = TOP - 1;
      const int max_y = TOP + HEIGHT + 1;

      // we do a symmetric expansion, but only need to cover the
      // rectangle above. find the maximum axis-aligned distance to
      // any of those points from x/y.
      const int xsize = std::max(abs(x - min_x), abs(x - max_x));
      const int ysize = std::max(abs(y - min_y), abs(y - max_y));
      // Instead of scanning from top to bottom, use larger and larger
      // offsets but try both positive and negative. Goal is to be
      // able to exit when the pixels being tested must be outside
      // our current bound.
      for (int dy = 0; dy <= ysize; dy++) {
        const int dys = dy * dy;
        // dy is always getting bigger; if we already found a pixel
        // closer than this distance, just counting the vertical,
        // we can't improve.
        if (dys >= min_sqdist) break;

        // We'll explore dy and dx in both direction.
        // Note this harmlessly tests pixels twice when dy or dx is
        // 0, but it seems better to avoid the branching?
        // Seems to be a better tradeoff to do the outer loop out
        // here so we can skip the entire x loop sometimes (when
        // outside the image entirely).
        for (int sy : {-dy, +dy}) {
          int yy = y + sy;
          if (yy < TOP - 1 || yy > TOP + HEIGHT) continue;

          for (int dx = 0; dx <= xsize; dx++) {
            const int dxs = dx * dx;
            // can do a similar test here but it is not
            // faster?
            if (dys + dxs >= min_sqdist) break;

            // might be faster to check bounds here rather
            // than in Color, although it gets a bit gross.
            for (int sx : {-dx, +dx}) {
              const int xx = x + sx;
              if (Color(xx, yy) == c) {
                if (dys + dxs < min_sqdist) {
                  min_sqdist = dys + dxs;
                }
              }
            }
          }
        }
      }
      // CHECK(min_sqdist <= squared_bound) <<
      //     min_sqdist << " vs " << squared_bound;
      return min_sqdist;
    };

  constexpr float oscale = 1.0f / scale;
  ImageA sdf(sdf_size, sdf_size);

  // As we populate the SDF, we can use the previous computed pixel
  // (that was searching for the same color) to give us a bound on the
  // search. OK for this to span rows, although then the bounds get
  // momentarily bad. Start with "infinite" though.
  const int MAX_DIST = size * 2;
  struct Last {
    // Position of the last pixel for which we
    // got a distance, for the given color.
    int x, y;
    // The actual distance we computed for that pixel.
    float dist;
  };
  std::array<Last, 2> last = {Last{.x = 0, .y = 0, .dist = (float)MAX_DIST},
                              Last{.x = 0, .y = 0, .dist = (float)MAX_DIST}};

  for (int sy = 0; sy < sdf_size; sy++) {
    for (int sx = 0; sx < sdf_size; sx++) {
      int ix = roundf((float)sx * scale);
      int iy = roundf((float)sy * scale);

      const bool color = Color(ix, iy);

      // Whatever we computed for the last pixel of the same color
      // gives us a bound using the triangle inequality (worst case
      // is our distance to that pixel, plus its distance to the
      // goal).
      Last &prev = last[color ? 1 : 0];
      int dx = ix - prev.x;
      int dy = iy - prev.y;
      float bound = prev.dist + sqrtf(dx * dx + dy * dy);
      int sq_bound = ceilf(bound * bound);
      // printf("dx %d dy %d prev %.2f bound %.2f sq %d\n",
      // dx, dy, prev.dist, bound, sq_bound);

      const int sqdist = GetSqDistanceTo(ix, iy, !color, sq_bound);
      const float dist = sqrtf(sqdist);

      // Save distance for next time we search this color.
      prev.x = ix;
      prev.y = iy;
      prev.dist = dist;

      // Distance in sdf space.
      const float sdf_dist = dist * oscale * FALLOFF_PER_PIXEL;

      const float signed_dist = color ? sdf_dist : -sdf_dist;
      int value = roundf((float)ONEDGE + signed_dist);
      sdf.SetPixel(sx, sy, std::clamp(value, 0, 255));
    }
  }

  return sdf;
}



std::pair<ImageA, std::array<float, 26>>
FontProblem::RunSDFModel(const Network &net,
                         const SDFConfig &config,
                         const ImageA &sdf_input) {
  const int sdf_size = config.sdf_size;
  Stimulation stim{net};
  SDFFillVector(config, sdf_input, &stim.values[0]);
  net.RunForward(&stim);
  const std::vector<float> &output = stim.values.back();
  CHECK(output.size() >= sdf_size * sdf_size + 26);
  ImageA sdf_output = SDFGetImage(config, output);
  std::array<float, 26> letters;
  for (int i = 0; i < 26; i++)
    letters[i] = output[sdf_size * sdf_size + i];
  return make_pair(sdf_output, letters);
}

std::vector<float>
FontProblem::RunSDFModelPredOnly(const Network &net,
                                 const SDFConfig &config,
                                 const ImageA &sdf_input) {
  Stimulation stim{net};
  SDFFillVector(config, sdf_input, &stim.values[0]);
  net.RunForward(&stim);
  const std::vector<float> &output = stim.values.back();
  CHECK(output.size() == 26);
  return std::move(stim.values.back());
}


ImageRGBA FontProblem::ThresholdImageMulti(
    const ImageA &sdf,
    const std::vector<pair<float, uint32>> &layers,
    int out_width, int out_height,
    int quality,
    float gamma) {
  ImageRGBA out(out_width, out_height);

  for (const auto &[onedge_value, color] : layers) {
    ImageA thresh = SDFThresholdAAFloat(onedge_value, sdf,
                                        out_width, out_height,
                                        quality,
                                        gamma);
    const uint8 r = 0xFF & (color >> 24);
    const uint8 g = 0xFF & (color >> 16);
    const uint8 b = 0xFF & (color >> 8);
    out.BlendImage(0, 0, thresh.AlphaMaskRGBA(r, g, b));
  }

  return out;
}

FontProblem::Gen5Result FontProblem::Gen5(
    const FontProblem::SDFConfig &config,
    const Network &make_lowercase,
    const Network &make_uppercase,
    const ImageA &sdf) {
  Gen5Result result;
  result.config = config;
  result.input = sdf;

  auto RunTo = [&config](
      const Network &net, const ImageA &sdf,
      ImageA *out,
      std::array<float, 26> *pred_out) {
      auto [out_sdf, pred] = FontProblem::RunSDFModel(net, config, sdf);
      if (out != nullptr) *out = std::move(out_sdf);
      if (pred_out != nullptr) *pred_out = std::move(pred);
    };

  // First iter.
  RunTo(make_lowercase, sdf, &result.low, &result.low_pred);
  RunTo(make_uppercase, sdf, &result.up, &result.up_pred);
  // Second iter.
  RunTo(make_uppercase, result.low, &result.low_up, nullptr);
  RunTo(make_lowercase, result.up, &result.up_low, nullptr);
  return result;
}

FontProblem::Gen5ImagesResult FontProblem::Gen5Images(
    const FontProblem::SDFConfig &config,
    const Network &make_lowercase,
    const Network &make_uppercase,
    const ImageA &sdf,
    int scale,
    int quality,
    float gamma_low,
    float gamma_up) {
  Gen5Result res = Gen5(config, make_lowercase, make_uppercase, sdf);
  return Gen5Images(res, scale, quality, gamma_low, gamma_up);
}

FontProblem::Gen5ImagesResult FontProblem::Gen5Images(
    const Gen5Result &gr,
    int scale,
    int quality,
    float gamma_low,
    float gamma_up) {
  const int sdf_size = gr.config.sdf_size;
  const int THRESHOFF = sdf_size * scale;

  // SDF and then thresholded image, side by side
  Gen5ImagesResult result(sdf_size * scale * 2, sdf_size * scale);
  result.input.BlendImage(0, 0,
                          gr.input.GreyscaleRGBA().ScaleBy(scale));
  ImageA thresh = FontProblem::SDFThresholdAAFloat(
      gr.config.onedge_value / 255.0f,
      gr.input,
      sdf_size * scale, sdf_size * scale,
      quality, 1.0f);
  result.input.BlendImage(THRESHOFF, 0, thresh.GreyscaleRGBA());

  result.low_pred = gr.low_pred;
  result.up_pred = gr.up_pred;

  const vector<pair<float, uint32>> layers =
    {{gr.config.onedge_value * 0.95f / 255.0f, 0x440000FF},
     {gr.config.onedge_value * 0.975f / 255.0f, 0x66229FFF},
     {gr.config.onedge_value / 255.0f, 0xFFFFFFFF}};

  auto DrawTo = [scale, quality, sdf_size, THRESHOFF, &layers](
      const ImageA &sdf,
      float gamma,
      ImageRGBA *out) {
      out->BlendImage(0, 0, sdf.GreyscaleRGBA().ScaleBy(scale));

      ImageRGBA out_thresh = FontProblem::ThresholdImageMulti(
          sdf,
          layers,
          sdf_size * scale, sdf_size * scale,
          quality,
          gamma);
      out->BlendImage(THRESHOFF, 0, out_thresh);
    };

  // First iter.
  DrawTo(gr.low, gamma_low, &result.low);
  DrawTo(gr.up,  gamma_up,  &result.up);

  // Second iter.
  // Note gamma_low even when make_uppercase; the gamma_low parameter is
  // about the "low side", and vice versa.
  DrawTo(gr.low_up, gamma_low, &result.low_up);
  DrawTo(gr.up_low, gamma_up,  &result.up_low);
  return result;
}

Network *FontProblem::MakePredOnlyNetwork(const SDFConfig &config,
                                          const Network &net) {
  Network *ret = Network::Clone(net);

  const int sdf_size = config.sdf_size;
  // Act on the last layer.
  const int layer_idx = net.num_layers - 1;
  EZLayer ez(*ret, layer_idx);

  std::vector<EZLayer::Node> new_nodes;
  new_nodes.reserve(26);
  for (int i = 0; i < 26; i++) {
    new_nodes.push_back(ez.nodes[sdf_size * sdf_size + i]);
  }
  ez.nodes = std::move(new_nodes);
  ez.MakeWidthHeight();
  ez.Repack(ret, layer_idx);
  ret->ReallocateInvertedIndices();
  Network::ComputeInvertedIndices(ret, 6);
  return ret;
}


// Imagine a series of nested islands, where the border is a transition
// (along the grid edges, 4-connected) between less than onedge_value
// and greater-or-equal to onedge_value. The nesting alternates "sea"
// and "land", and an island can contain zero or more child islands.
//
// Goal is to fill in every pixel of the SDF with an equivalence class
// id and parent pointer. The ids distinguish connected components in
// the case that there are multiple at the same depth.
//
// The outside of the SDF is defined as sea--island zero--which may
// connect with pixels inside the image. We initialize by creating
// this boundary around the SDF and doing the flood-fill inside it;
// as we continue we will do lower depths first, which gives the
// algorithm its direction (and termination condition).
//
// In the flood fill, we pick a pixel with the lowest depth and expand.
// Pixels in the queue already have an equivalence class (but maybe not
// a final one) and parent pointer assigned. For their connected
// neighbors:
//  - If there's a border:
//     - If already processed, should be nothing to do. (It should
//       end up being our parent or child, but maybe not resolved yet?)
//     - Otherwise, because we do this in depth order, this is
//       a child of ours. Set it at depth+1, parent pointing to us,
//       and a new equivalence class.
//  - If there's no border:
//     - If already processed, join the equivalence classes.
//     - If not, join the equivalence classes, copy our parent pointer
//       and depth to it, and enqueue.
//
// And actually it seems that the parent pointer is a property of the
// equivalence class, although it's not known until a pixel is
// processed.
namespace {
struct Todo {
  void Add(int depth, int idx) {
    m[depth].push_back(idx);
  }

  bool HasNext() const {
    return !m.empty();
  }

  std::pair<int, int> Next() {
    CHECK(!m.empty());
    auto it = m.begin();
    const int depth = it->first;
    const int idx = it->second.back();
    it->second.pop_back();
    if (it->second.empty()) {
      m.erase(it);
    }
    return {depth, idx};
  }

  // Map of depth to to-do list for that depth.
  // Representation invariant: no vectors are empty.
  std::map<int, vector<int>> m;
};

struct IslandFinder {
  // Bitmap >0 for land. We locally pad it.
  // Run "Fill" after constructing.
  explicit IslandFinder(const ImageA &bitmap) :
    img(Preprocess(bitmap)),
    radix(img.Width() * img.Height()) {
    for (int i = 0; i < radix; i++) {
      eqclass.push_back(-1);
      classinfo.push_back(nullopt);
    }
  }

  int Height() const { return img.Height(); }
  int Width() const { return img.Width(); }

  std::pair<int, int> GetXY(int index) const {
    return make_pair(index % img.Width(), index / img.Width());
  }
  int Index(int x, int y) const {
    return y * img.Width() + x;
  }

  bool IsLand(int x, int y) const {
    if (x < 0 || y < 0 || x >= img.Width() || y >= img.Height())
      return false;
    else return img.GetPixel(x, y) > 0;
  }

  // Information about a pixel, which should be eventually
  // shared with the full equivalence class.
  struct Info {
    Info(int d, int pc) : depth(d), parent_class(pc) {}
    int depth = -1;
    // Note: need to resolve this with GetClass, as the
    // parent could be unioned with something later (is
    // it actually possible? well in any case, it is right
    // to look up the class).

    // XXX Do I even need parents?
    int parent_class = -1;
  };

  bool Visited(int idx) const {
    return classinfo[idx].has_value();
  }

  // For each pixel, nullopt if we have not yet visited it.
  // If we have visited, its depth (will never change) and
  // parent pixel (must canonicalize via GetClass).
  vector<optional<Info>> classinfo;

  void SetInfo(int idx, int depth, int parent) {
    CHECK(!classinfo[idx].has_value()) << idx << " depth "
                                       << depth << " par " << parent;
    classinfo[idx].emplace(depth, parent);
  }

  Info GetInfo(int idx) const {
    CHECK(classinfo[idx].has_value());
    return classinfo[idx].value();
  }

  // Return the equivalence class that this index currently
  // belongs in.
  int GetClass(int idx) {
    CHECK(idx >= 0 && idx < eqclass.size());
    if (eqclass[idx] == -1) return idx;
    else return eqclass[idx] = GetClass(eqclass[idx]);
  }

  void Union(int aidx, int bidx) {
    CHECK(aidx >= 0 && aidx < eqclass.size());
    CHECK(bidx >= 0 && bidx < eqclass.size());

    CHECK(Visited(aidx));
    CHECK(Visited(bidx));

    int a_class = GetClass(aidx);
    int b_class = GetClass(bidx);
    if (a_class != b_class) {
      CHECK(Visited(a_class));
      CHECK(Visited(b_class));
      // Check that the classes are compatible.
      CHECK(classinfo[a_class].value().depth ==
            classinfo[b_class].value().depth);
      // It's probably the cases that the parent
      // classes are actually equal too?
      eqclass[a_class] = b_class;
    }
  }

  void Fill() {
    const int TOP_LEFT = Index(0, 0);
    // Parent of outermost region is itself.
    SetInfo(TOP_LEFT, 0, TOP_LEFT);

    // Start by marking the border as visited, and part of the same
    // island. This prevents us from going off the map, and ensures
    // that the outer region is all connected.
    // Left and right edges.
    auto MarkBoundary = [this, TOP_LEFT](int x, int y) {
        int idx = Index(x, y);
        SetInfo(idx, 0, TOP_LEFT);
        Union(TOP_LEFT, idx);
      };

    for (int y = 0; y < Height(); y++) {
      if (y != 0)
        MarkBoundary(0, y);
      MarkBoundary(Width() - 1, y);
    }
    // Top and bottom edges.
    for (int x = 1; x < Width() - 1; x++) {
      MarkBoundary(x, 0);
      MarkBoundary(x, Height() - 1);
    }

    // To kick off the process, we start with the pixel at (1,1). It
    // must be sea because we padded the whole thing with two pixels
    // of sea.
    CHECK(!IsLand(1, 1));

    const int start_idx = Index(1, 1);
    SetInfo(start_idx, 0, TOP_LEFT);
    Union(TOP_LEFT, start_idx);

    Todo todo;
    todo.Add(0, Index(1, 1));

    // Now, the steady state flood-fill.
    while (todo.HasNext()) {
      const auto [src_depth, src_idx] = todo.Next();
      CHECK(Visited(src_idx));

      const int src_parent = GetInfo(src_idx).parent_class;

      const auto [src_x, src_y] = GetXY(src_idx);
      for (const auto [dx, dy] : initializer_list<pair<int, int>>{
          {-1, 0}, {1, 0}, {0, -1}, {0, 1}}) {
        const int x = src_x + dx;
        const int y = src_y + dy;
        const int idx = Index(x, y);

        const bool has_border =
          IsLand(src_x, src_y) != IsLand(x, y);

        if (has_border) {
          if (Visited(idx)) {
            // Nothing to do here, although we can sanity check...
            const int depth = GetInfo(idx).depth;
            // We could be reaching this pixel the second time (like if
            // it's a corner, or on a one-wide strip), or we could be
            // looking out of the outer edge of our own region.
            CHECK(depth == src_depth + 1 ||
                  depth == src_depth - 1);
          } else {
            // Otherwise, we found part of an island within this one.
            SetInfo(idx, src_depth + 1, src_idx);
            todo.Add(src_depth + 1, idx);
          }
        } else {
          // If we haven't visited it yet, initialize at the same
          // depth and put it in the queue.
          if (!Visited(idx)) {
            SetInfo(idx, src_depth, src_parent);
            todo.Add(src_depth, idx);
          }

          // And either way, since we are connected neighbors, join
          // the equivalence classes.
          Union(src_idx, idx);
        }
      }
    }
  }

  // Get the output of this process as three components:
  //  - Two bitmaps the size of the original bitmap:
  //      (removing the padding used for internal purposes).
  //    - Depth map. Value is the depth.
  //    - Equivalence classes. Value is arbitrary unique id,
  //      though depth zero will be equivalence class 0.
  //  - A map from each equivalence class in the second image
  //    to its parent equivalence class. The exception is 0,
  //    which has no parent and does not appear in the map.
  // Since the output is 8-bit, there can only be up to 255
  // in each case.
  std::tuple<ImageA, ImageA, std::map<uint8, uint8>> GetMaps() {
    const int w = img.Width() - 4;
    const int h = img.Height() - 4;
    ImageA depth(w, h);
    ImageA eq(w, h);
    std::map<uint8, uint8> parent;

    const int zero_id = GetClass(Index(0, 0));
    std::unordered_map<int, uint8> toeq = {{zero_id, 0}};
    uint8 next_id = 1;
    auto GetEq = [&toeq, &next_id](int eqc) {
        auto it = toeq.find(eqc);
        if (it == toeq.end()) {
          CHECK(next_id < 255);
          uint8 ret = next_id++;
          toeq[eqc] = ret;
          return ret;
        } else {
          return it->second;
        }
      };

    for (int y = 0; y < h; y++) {
      for (int x = 0; x < h; x++) {
        int yy = y + 2;
        int xx = x + 2;
        int idx = Index(xx, yy);
        CHECK(Visited(idx)) << "bug? or call Fill";
        Info info = GetInfo(idx);
        CHECK(info.depth >= 0 && info.depth < 256) << info.depth;
        depth.SetPixel(x, y, info.depth);
        const uint8 eq8 = GetEq(GetClass(idx));
        eq.SetPixel(x, y, eq8);
        // Don't set parent for class 0.
        if (eq8 != 0) {
          CHECK(info.parent_class >= 0);
          const uint8 pq8 = GetEq(GetClass(info.parent_class));
          auto it = parent.find(eq8);
          if (it != parent.end()) {
            CHECK(it->second == pq8) << "Inconsistent parents for "
              "equivalence class " << GetClass(idx) << " = " << eq8 <<
              ": " << GetClass(info.parent_class) << " = " << pq8 <<
              " but already had (something) = " << it->second << "!";
          } else {
            parent[eq8] = pq8;
          }
        }
      }
    }
    return make_tuple(depth, eq, parent);
  }

  // During or after Fill(), use this to generate a visualization of
  // that process.
  ImageRGBA DebugBitmap() {
    ImageRGBA out(img.Width(), img.Height());
    out.Clear32(0x000000FF);
    std::vector<uint16_t> COLORS_LEFT = {
      0xFF00,
      0x00FF,
      0x4400,
      0x0044,
      0x9900,
      0x0099,
      0xAA00,
      0x00AA,
      0x44FF,
      0xFF44,
      0x4444,
      0x9999,
      0xAAAA,
    };
    std::unordered_map<int, uint16> colors;
    for (int y = 0; y < img.Height(); y++) {
      for (int x = 0; x < img.Width(); x++) {
        int idx = Index(x, y);
        if (Visited(idx)) {
          Info info = GetInfo(idx);
          uint8 r = info.depth * 0x20;
          // green and blue from eq class.
          int eq = GetClass(idx);
          uint16 gb = 0;
          auto c = colors.find(eq);
          if (c == colors.end()) {
            CHECK(!COLORS_LEFT.empty());
            gb = COLORS_LEFT.back();
            colors[eq] = gb;
            COLORS_LEFT.pop_back();
          } else {
            gb = c->second;
          }
          uint8 g = (gb >> 8) & 0xFF;
          uint8 b = gb & 0xFF;
          out.SetPixel(x, y, r, g, b, 0xFF);
        } else {
          out.SetPixel32(x, y, 0xFF0000FF);
        }
      }
    }
    return out;
  }

private:
  // Pre-processed bitmap with pixels >0 for land, =0 for sea. The
  // image must be padded such that it has two pixels of sea on every
  // side.
  const ImageA img;
  const int radix;

  // Same size as image. Gives the pixel's current equivalence
  // class; each one starts in its own (value -1) to begin.
  // Union-find-like algorithm.
  vector<int> eqclass;

  static ImageA Preprocess(const ImageA &bitmap) {
    ImageA ret(bitmap.Width() + 4, bitmap.Height() + 4);
    ret.Clear(0);
    ret.BlendImage(2, 2, bitmap);
    return ret;
  }
};

}  // namespace


// For the tracing procedure we will do some operations on the
// SDF, including negating it. It's easiest to work with if
// we don't have to use integral values or worry about the
// configured onedge_value or falloff. Use floats and normalize
// the SDF so that the onedge_value is 0.5f and the falloff is
// 0.1 per pixel. (Would probably be cleaner if we used a center
// of zero, but ImageF clips to [0, 1].)
static constexpr float EDGE = 0.5f;
static constexpr float FALLOFF = 0.1f;
static ImageF NormalizeSDF(const FontProblem::SDFConfig &config,
                           const ImageA &sdf) {
  ImageF out(sdf.Width(), sdf.Height());

  auto Map = [&config](uint8 v) {
      // Convert to [0,1] nominal scale.
      const float f = v / 255.0f;
      // Center at zero.
      const float fcentered = f - (config.onedge_value / 255.0f);
      // Falloff in [0,1] nominal scale.
      const float ffalloff = config.falloff_per_pixel / 255.0f;
      // Now scale so that falloff is 0.1f.
      const float fscaled = fcentered / (ffalloff / FALLOFF);
      // Now recenter at 0.5f and saturate.
      return std::clamp(fscaled + EDGE, 0.0f, 1.0f);
    };

  for (int y = 0; y < sdf.Height(); y++) {
    for (int x = 0; x < sdf.Width(); x++) {
      out.SetPixel(x, y, Map(sdf.GetPixel(x, y)));
    }
  }
  return out;
}

// Core of the problem: Trace a single pixel blob in a normalized
// SDF, producing a single clockwise contour.
static TTF::Contour VectorizeOne(
    // Normalized. The interior is > 0.5f, the exterior is < 0.5f,
    // and the falloff is 0.1f.
    const ImageF &sdf,
    // Bitmap of the same size. A contiguous non-empty region
    // >0 is the shape to trace.
    const ImageA &bitmap) {

  auto InBlob = [&bitmap](int x, int y) -> bool {
      if (x < 0 || y <0 || x >= bitmap.Width() || y >= bitmap.Height())
        return false;
      return bitmap.GetPixel(x, y) > 0;
    };

  // First, find a pixel inside the blob.
  // This pixel has the property that there is no pixel with
  // a smaller y coordinate, which is also in the blob.
  const auto [startpx, startpy] = [&bitmap, InBlob]() ->
    std::pair<int, int> {
      for (int y = 0; y < bitmap.Height(); y++) {
        for (int x = 0; x < bitmap.Width(); x++) {
          if (InBlob(x, y)) return make_pair(x, y);
        }
      }
      CHECK(false) << "VectorizeOne requires a non-empty bitmap!";
  }();

  // The first phase is to wind around the pixel blob's exterior,
  // always maintaining a direction and a pair of pixels, one in,
  // and one out. The starting pixel we just found is such an
  // example we scanned from top to bottom. We'll be done when we
  // return to the start pixel.
  CHECK(!InBlob(startpx, startpy - 1)) << "Need the uppermost pixel "
    "in this column.";

  printf("Start: %d,%d\n", startpx, startpy);

  // Discrete direction. The code below is written for a pattern
  // where we are moving right, with the blob down, and the
  // exterior up (which is the start condition), but it naturally
  // rotates to the other directions.
  enum Dir {
    UP,
    DOWN,
    LEFT,
    RIGHT,
  };

  // Get the orthogonal "normal" direction, which is up for Right.
  auto Normal = [](Dir d) {
      switch (d) {
      case RIGHT: return UP;
      case LEFT: return DOWN;
      case DOWN: return RIGHT;
      case UP: return LEFT;
      }
      CHECK(false) << "Bad dir";
    };

  auto TurnCCW = Normal;

  auto TurnCW = [](Dir d) {
      switch (d) {
      case RIGHT: return DOWN;
      case DOWN: return LEFT;
      case LEFT: return UP;
      case UP: return RIGHT;
      }
      CHECK(false) << "Bad dir";
    };

  auto Move = [](int x, int y, Dir d) -> pair<int, int> {
    switch (d) {
    case RIGHT: return make_pair(x + 1, y);
    case LEFT: return make_pair(x - 1, y);
    case DOWN: return make_pair(x, y + 1);
    case UP: return make_pair(x, y - 1);
    }
    CHECK(false) << "Bad dir";
  };

  auto MoveF = [](float x, float y, Dir d, float r) -> pair<float, float> {
    switch (d) {
    case RIGHT: return make_pair(x + r, y);
    case LEFT: return make_pair(x - r, y);
    case DOWN: return make_pair(x, y + r);
    case UP: return make_pair(x, y - r);
    }
    CHECK(false) << "Bad dir";
  };


  // Pixel we're currently looking at.
  int px = startpx;
  int py = startpy;

  // Direction we're currently heading. We are at the top of the
  // blob, so go right for clockwise. (It seems any local top
  // would work; compare for example the inner top edges of an 's'
  // shape.)
  Dir right = RIGHT;

  vector<std::pair<float, float>> edge_points;
  for (;;) {
    Dir up = Normal(right);
    const auto [upx, upy] = Move(px, py, up);
    // Invariant is that we are on the edge, so px,py is
    // in the blob and the pixel "above" it is not.
    CHECK(InBlob(px, py));
    CHECK(!InBlob(upx, upy));

    // First, trace a line straight up to find the point (in
    // sampled bilinear space) where we hit 0.5, the onedge value.
    // This point will be on our contour.

    // We could solve for this point since the bilinear sampling is
    // a simple formula, but even easier is to just search (and note
    // that we have to deal with boundary cases like the edge of
    // the image). The edge of the pixel will be between this pixel
    // and the one above it, because it is outside the blob.
    // (Perhaps in practice this could be violated, since we can't
    // quite guarantee that the bitmap and SDF are in agreement; they
    // are modified in the recursive decompositino below. But this
    // keeps us close to the blob, which is desirable for other reasons!)

    const float r = Opt::Minimize1D(
        [&sdf, &MoveF, px, py, up](double r) -> double {
          const auto [ex, ey] = MoveF((float)px, (float)py, up, r);
          // Get close to EDGE.
          return fabs(sdf.SampleBilinear(ex, ey) - EDGE);
        },
        // Up to 1 pixel along the normal.
        0.0, 1.0, 1000).first;

    printf("from %d,%d r=%.2f\n", px, py, r);

    const auto [ex, ey] = MoveF((float)px, (float)py, up, r);
    edge_points.emplace_back(ex, ey);

    // We proceed by case analysis on the pixel grid.

    // +--+--+
    // |  |a?|
    // +--+--+
    // |##|b?|
    // +--+--+

    const auto [ax, ay] = Move(upx, upy, right);
    const auto [bx, by] = Move(px, py, right);
    const bool a = InBlob(ax, ay);
    const bool b = InBlob(bx, by);

    if (!a && b) {
      // +--+--+
      // |  |a |
      // +--+--+
      // |##|b#|
      // +--+--+
      // Just continue in the same direction.
      px = bx;
      py = by;
    } else if (!a && !b) {
      // +--+--+
      // |  |a |
      // +--+--+
      // |##|b |
      // +--+--+
      // Make a 90 degree turn around this pixel, but
      // stay on it.
      right = TurnCW(right);
    } else {
      CHECK(a);
      // +--+--+
      // |  |a#|
      // +--+--+
      // |##|b?|
      // +--+--+
      // Don't care what b is (we are using 4-connectivity);
      // if it's open we'll get there separately.

      // (This case might produce weird inverted corners.
      // Might be better to also look at the pixel above a
      // and keep going right in shallow cases.)
      px = ax;
      py = ay;
      right = TurnCCW(right);
    }

    // Consider the case of a single pixel. We should
    // only end when we approach it with right = RIGHT, right?
    if (px == startpx &&
        py == startpy && right == RIGHT)  {
      printf("Loop finished!\n");
      break;
    }
  }

  // Return straight lines between these edge points.
  // TODO: Clean this up, use beziers!
  TTF::Contour ret;
  for (const auto [ex, ey] : edge_points) {
    ret.paths.emplace_back(ex, ey);
  }

  return ret;
}

vector<TTF::Contour> FontProblem::VectorizeSDF(
    const FontProblem::SDFConfig &config,
    const ImageA &sdf8,
    ImageRGBA *islands) {

  const auto [depth, eqclass, parentmap] = [&config, &sdf8, islands](){
      // Make thresholded bitmap.
      ImageA bitmap(sdf8.Width(), sdf8.Height());
      for (int y = 0; y < sdf8.Height(); y++) {
        for (int x = 0; x < sdf8.Width(); x++) {
          bitmap.SetPixel(x, y,
                          sdf8.GetPixel(x, y) >= config.onedge_value ?
                          0xFF : 0x00);
        }
      }

      IslandFinder finder(bitmap);
      finder.Fill();

      if (islands != nullptr) *islands = finder.DebugBitmap();

      return finder.GetMaps();
    }();

  // Get the depth of each equivalence class that occurs, and the
  // maximum depth.
  std::unordered_map<uint8, uint8> eqclass_depth;
  int max_depth = 0;
  for (int y = 0; y < depth.Height(); y++) {
    for (int x = 0; x < depth.Width(); x++) {
      uint8 d = depth.GetPixel(x, y);
      uint8 eqc = eqclass.GetPixel(x, y);
      auto it = eqclass_depth.find(eqc);
      if (it == eqclass_depth.end()) {
        eqclass_depth[eqc] = d;
      } else {
        CHECK(it->second == d) << "Inconsistent depth for eqclass "
                               << eqc << " at " << x << "," << y;
      }
      max_depth = std::max((int)d, max_depth);
    }
  }

  // Assuming the depth of eqc is at least d, get the ancestor
  // that is at depth exactly d.
  auto GetAncestorAtDepth =
    [&parentmap, &eqclass_depth](uint8 d, uint8 eqc) -> uint8 {
      for (;;) {
        if (d == 0) return 0;
        auto dit = eqclass_depth.find(eqc);
        CHECK(dit != eqclass_depth.end()) << eqc << " has no depth?";
        if (dit->second == d) return eqc;

        auto pit = parentmap.find(eqc);
        CHECK(pit != parentmap.end()) << eqc << " has no parent?";
        eqc = pit->second;
      }
    };


  // Next up, generate a series of nested contours.
  // For each equivalence class, first simplify matters by
  // filling its interior so that it is never less than
  // the onedge value; we'll deal with those cutouts separately.
  // Similarly, make sure that the exterior is all less than
  // the onedge value.
  //
  // When recursing, we can negate the SDF's interior so that we
  // always generate these contours as though tracing the outside of
  // land (and then we can reverse the contour if it's a cutout).

  // We should probably be subtracting portions of the SDF that we
  // have already processed (or that are part of a separate segment)
  // so that we don't get confused. Not sure what the right way is?
  // The pixels of the equivalence class do also set a bound on the
  // distance for all pixels in the image. So we could use that to
  // modify the SDF to reduce confusion, although it's not going to
  // completely remove it, and might not address the cases that matter
  // at all. I guess it would prevent us from getting more than a
  // certain distance from the eqclass, which is at least a bound
  // on the error.
  // Since it would be imperfect anyway, this first version is not
  // doing it at all.

  // Work with normalized SDF to simplify matters a bit.
  ImageF sdf = NormalizeSDF(config, sdf8);

  // No contour for depth 0.
  for (int d = 1; d <= max_depth; d++) {
    printf("DEPTH %d\n", d);
    // Get the equivalence classes at this depth, paired with the set
    // of (inclusive) descendants of that class (we want to remove
    // these holes when tracing the contour to simplify our lives).
    // We'll run the routine below on each one.
    std::map<uint8, std::set<uint8>> eqclasses;
    for (int y = 0; y < eqclass.Height(); y++) {
      for (int x = 0; x < eqclass.Height(); x++) {
        if ((int)depth.GetPixel(x, y) >= d) {
          uint8 eqc = eqclass.GetPixel(x, y);
          // This must exist because the depth is at least d, and
          // d > 0.
          uint8 ancestor = GetAncestorAtDepth(d, eqc);
          eqclasses[ancestor].insert(eqc);
        }
      }
    }

    for (const auto &[this_eqc, descendants] : eqclasses) {
      // Simplified bitmap.
      ImageA bitmap(eqclass.Width(), eqclass.Height());
      for (int y = 0; y < eqclass.Height(); y++) {
        for (int x = 0; x < eqclass.Width(); x++) {
          uint8 eqc = eqclass.GetPixel(x, y);
          bool inside = ContainsKey(descendants, eqc);
          bitmap.SetPixel(x, y, inside ? 0xFF : 0x00);
        }
      }

      /*
      printf("Class %d includes: ", (int)this_eqc);
      for (uint8 c : descendants) printf(" %d", (int)c);
      printf("\n");
      bitmap.GreyscaleRGBA().Save(
          StringPrintf("eqclass-%d.png", (int)this_eqc));
      */

      TTF::Contour contour = VectorizeOne(sdf, bitmap);

    }
  }

  return {};
}
