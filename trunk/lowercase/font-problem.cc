
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

using namespace std;

using Contour = TTF::Contour;
using uint32 = uint32_t;

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
      float x = values[idx + 0];
      float y = values[idx + 1];

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

  // XXX do this threaded
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

bool FontProblem::FillVector(const TTF *ttf, int codepoint,
			     const std::vector<int> &row_max_points,
			     float *buffer) {
  std::vector<TTF::Contour> contours =
    TTF::MakeOnlyBezier(
	TTF::NormalizeOrder(ttf->GetContours(codepoint),
			    0.0f, 0.0f));

  // Only room for three contours.
  // XXX: Perhaps we should reject the entire font if it will
  // ever fail these tests; otherwise we are biasing against
  // characters with more contours (e.g. B).
  if (contours.size() > row_max_points.size())
    return false;

  // Also don't allow empty characters.
  if (contours.empty())
    return false;

  auto ByPathSizeDesc = [](const Contour &a, const Contour &b) {
      return b.paths.size() < a.paths.size();
    };

  std::sort(contours.begin(), contours.end(), ByPathSizeDesc);

  for (int i = 0; i < contours.size(); i++) {
    if (contours[i].paths.size() > row_max_points[i]) {
      return false;
    }
  }
  
  // We need something to put in rows if there are fewer than
  // the maximum number of contours.
  // We treat this as an empty path starting at 0,0.
  while (contours.size() < row_max_points.size()) {
    TTF::Contour degenerate(0.0f, 0.0f);
    contours.push_back(degenerate);
  }


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
  return dx * dx + dy + dy;
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

  int closest_a = -1;
  int closest_e = -1;
  float closest_dist = std::numeric_limits<float>::infinity();
  for (int a = 0; a < num_actual; a++) {
    for (int e = 0; e < num_expected; e++) {
      const float dist = sqrtf(SqDistance(expected[e], actual[a]));
      DistanceAt(e, a) = dist;
      if (dist < closest_dist) {
	closest_a = a;
	closest_e = e;
      }
    }
  }

  // (PERF: Not actually using this)
  CHECK(closest_a >= 0 && closest_e >= 0)
    << closest_a << " " << closest_e;

  auto Score = [&actual, &expected, &DistanceAt](
      const LoopAssignment &assn) -> float {
      // here we have like
      //        0       1 2
      //        x       y z      <- expected
      //    a b c d e f g h i j  <- actual
      //    0 1 2 3 4 5 6 7 8 9 
      // This would be represented with point0 = 2,
      // and groups = {4, 1, 5}.

      // PERF sanity check
      {
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
  
  LoopAssignment best_assignment{(int)expected.size()};
  float best_error = std::numeric_limits<float>::infinity();
  for (int attempt = 0; attempt < NUM_ATTEMPTS; attempt++) {
    // Make a random assignment.
    LoopAssignment assn{(int)expected.size()};
    assn.point0 = RandTo32(rc, actual.size());
    // The assignment is initialized to all ones. Distribute
    // the extra so that the sum is the same size as expected.
    int extra = actual.size() - expected.size();
    while (extra--)
      assn.groups[RandTo32(rc, assn.groups.size())]++;

    float current_score = Score(assn);
    bool improved = false;
    do {
      // Iteratively try to improve by moving points to neighbors.
      for (int i = 0; i < assn.groups.size(); i++) {
	int next_i = i < assn.groups.size() - 1 ? i + 1 : 0;

	// Move point forward.
	if (assn.groups[i] > 1) {
	  assn.groups[i]--;
	  assn.groups[next_i]++;
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

  return best_assignment;
}
