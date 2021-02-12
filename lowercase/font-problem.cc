
#include "font-problem.h"

#include <vector>
#include <string>

#include "base/stringprintf.h"

#include "network.h"

#include "image.h"
#include "ttf.h"
#include "threadutil.h"
#include "lines.h"
#include "arcfour.h"
#include "randutil.h"
#include "timer.h"

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
    ImageA ret(sdf.Height(), sdf.Width());
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
    ImageA ret(sdf.Height(), sdf.Width());
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
               float th1 = 0.95;
               float th2 = 0.975f;
               
               return vector<pair<uint8, uint32>>
                 {{(uint8)(config.onedge_value * th1), 0x440000FF},
                  {(uint8)(config.onedge_value * th2), 0x66229FFF},
                  {(uint8)(config.onedge_value), 0xFFFFFFFF}};
             };
           
           const vector<pair<uint8, uint32>> layers = GetLayers();

             
             ImageA sdf = SDFGetImage(config, stim.values[0]);

             ImageRGBA thresh = ThresholdImageMulti(sdf, layers, 4);

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
    TTF::Contour degenerate(0.0f, 0.0f);
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
      buffer[row_start + 0] = contour.startx;
      buffer[row_start + 1] = contour.starty;
      for (int i = 0; i < max_pts; i++) {
        // When we run out of real points, pad with degenerate
        // zero-length curves at the start point.
        float cx = i < contour.paths.size() ?
                       contour.paths[i].cx : contour.startx;
        float cy = i < contour.paths.size() ?
                       contour.paths[i].cy : contour.starty;
        float x = i < contour.paths.size() ?
                      contour.paths[i].x : contour.startx;
        float y = i < contour.paths.size() ?
                      contour.paths[i].y : contour.starty;

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
    CHECK(contour.paths.back().x == contour.startx);
    CHECK(contour.paths.back().y == contour.starty);

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

  auto GetDistanceTo = [&Color, size](int x, int y, bool c) -> float {
      // PERF insane to do this by searching the whole array!!
      int min_sqdist = size * size * 2;

      for (int yy = -1; yy <= size; yy++) {
        const int dy = yy - y;
        const int dys = dy * dy;

        // If dy is getting bigger and we already found a pixel
        // closer than this distance, we can't improve.
        if (dy > 0 && dys >= min_sqdist) break;
        
        for (int xx = -1; xx <= size; xx++) {
          const int dx = xx - x;
          const int dxs = dx * dx;
          // can do a similar test here but it is not
          // faster
          // if (dx > 0 && dys + dxs >= min_sqdist) break;
          
          if (Color(xx, yy) == c) {
            if (dys + dxs < min_sqdist) {
              min_sqdist = dys + dxs;
              // we will not find a closer point in this row!
              if (dx > 0) break;
            }
          }
        }
      }
      return sqrtf(min_sqdist);
    };
  
  const float oscale = 1.0f / scale;
  ImageA sdf(sdf_size, sdf_size);
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
      
      bool color = Color(ix, iy);

      const float dist = GetDistanceTo(ix, iy, !color);
      // Distance in sdf space.
      const float sdf_dist = dist * oscale * config.falloff_per_pixel;

      const float signed_dist = color ? sdf_dist : -sdf_dist;
      int value = roundf((float)config.onedge_value + signed_dist);
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


ImageRGBA FontProblem::ThresholdImageMulti(
    const ImageA &sdf,
    const std::vector<pair<uint8, uint32>> &layers,
    int scale) {
  ImageRGBA out(sdf.Width(), sdf.Height());

  for (const auto &[onedge_value, color] : layers) {
    ImageA thresh = SDFThresholdAA(onedge_value, sdf, scale);
    const uint8 r = 0xFF & (color >> 24);
    const uint8 g = 0xFF & (color >> 16);
    const uint8 b = 0xFF & (color >> 8);    
    out.BlendImage(0, 0, thresh.AlphaMaskRGBA(r, g, b));
  }

  return out;
}
