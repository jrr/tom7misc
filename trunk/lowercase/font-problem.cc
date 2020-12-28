
#include "font-problem.h"

#include <vector>
#include <string>

#include "base/stringprintf.h"

#include "network.h"

#include "image.h"
#include "ttf.h"
#include "threadutil.h"
#include "lines.h"

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

namespace {
struct HashPair {
  std::size_t operator()(const std::pair<int, int> &p) const {
    const auto [a, b] = p;
    return std::size_t(a) * 4294967291 + b;
  }
};
}

// Code for computing the error between a predicted vector shape ("loop")
// and the expected one.
//
// When we predict a loop it is of fixed size, but the expected
// font's loop is generally smaller. The predicted loop needs to
// follow the same sequence of vertices, but to use up its extra
// points, it is permitted to duplicate them. (At first I was asking
// it to always duplicate the start point at the end of the loop,
// but this may be more rigid than we want.) Alas we cannot "just"
// compute some geometric distance between the two loops, because we
// also need to attribute the error to specific points (including
// Bezier control points), including the error's derivative.
//
// Takes an expected loop and actual loop as a series of points 
// (for this code, we can just think of the edges as straight lines).
// Finds a mapping from each expected point to some actual point,
// such that:
//   - the expected points appear in strictly increasing order
//       (modulo the loop)
//   - the error is minimized:
//       - for each actual point in the domain of the mapping,
//         its Euclidean distance to the expected point
//       - also, all the unmapped points between it and the
//         previously mapped point.
//
// The output is a vector the same length as expected, which gives
// the index of the mapped point in actual.
using Point = FontProblem::Point;
vector<int> FontProblem::BestLoopMapping(const vector<Point> &expected,
					 const vector<Point> &actual) {
  const int num_expected = expected.size();
  const int num_actual = actual.size();

  // The first expected point will be mapped to some actual point. So
  // without loss of generality we'll try all rotations of the actual
  // vector, but then require that the 0th element be the destination
  // of the 0th expected point. The final result is the one of those
  // with the minimal error.

  int best_a_rotation = 0;
  float best_error = std::numeric_limits<float>::infinity();

  for (int a_rotation = 0; a_rotation < actual.size(); a_rotation++) {
    // PERF don't actually compute this; just wrap the subscript
    // calls.
    vector<Point> rot_actual;
    rot_actual.reserve(actual.size());
    for (int i = 0; i < actual.size(); i++) {
      rot_actual.push_back(actual[(i + a_rotation) % actual.size()]);
    }

    // OK, now expected[0] maps to actual[0], and we want to find
    // the best mapping for the rest. This is a natural dynamic
    // programming problem: We can tabulate the score yielding
    // the "best assignment of the suffix of expected to
    // the suffix of actual", with a table size of
    // O(|expected| * |actual|). Let e be the current index into
    // the expected vector, a the same for the (rotated) actual
    // vector. Then erest and arest are the number of elements that
    // remain.
    // Boundary conditions:
    //    - If erest=arest, then we must do the single 1:1 mapping possible.
    //    - If erest > arest, then it is impossible (infinite error).
    //    - If erest = 0, then all the points in arest are measured
    //      against the 0th point (already assigned in the outer loop),
    //      because of wraparound.
    // Otherwise, the general case. We can either assign the expected
    // point at the head to the actual point at head or not. If we
    // assign it,

    // Actually just kidding: We should go from the end. Let e
    // be the index from the end of the array
    // (starting at expected.size() - 1) and likewise a. We also
    // keep track of a bound > a; this span of points have not
    // been assigned yet. When we make an assignment, we include
    // the error for all these points (against the next assignment,
    // which has already been made; it begins as the wrapped-around
    // assignment of point 0). Either we're adding some more error
    // against that next point, or committing the current error and

    // XXX PERF use a dense vector.
    struct Info {
      // best error for this cell. Already includes the error from
      // points that get grouped to the next one.
      float err = 0.0f;
      // the point that intermediate points get measured against,
      // if they are not assigned to.
      Point next_point = {0.0f, 0.0f};
    };
    std::unordered_map<std::pair<int, int>, Info> table;
    auto Has = [&table](int e, int a) {
	return table.find({e, a}) != table.end();
      };
    auto At = [&table](int e, int a) -> float & {
	return table[{e, a}];
      };

    // e ranges from 0 to expected.size()
    // a ranges from 0 to actual.size()
    //
    //    
    //       qrstuvwx ""
    //      b      -- -
    //      c    $1 - -
    //      d    23 * -
    //     ""<<<<<<<& i
    //
    // Here we have the string of actual points (p)qrstuvwx as
    // columns, and expected (a)bcd as rows. The first letter is
    // already assigned, so it is left off. Thinking about the cell
    // *, we're computing the best way of mapping the string "d"
    // to the string "x". The only way to do this is to make the
    // assignment.
    //   - Since we do, that cell gets its next_point set to d, and the
    //     error is the distance between x and d, plus any carried
    //     error (there is none).
    // 
    // The cells marked - are impossible (infinite error) because
    // they ask us to assign more expected points (e.g. "cd") to fewer
    // actual (e.g. "x"). Note that the rightmost column (where actual
    // is the empty string is "-" except for the bottom-right diagonal.
    // This cell, "i", is the empty assignment, so its error is 0.0,
    // and the next_point is the assignment we started with (a->p; not
    // pictured).
    //
    // The cell marked & asks us to assign "" to the string "x". There
    // is just one way to do this, which is to skip "x" (it gets grouped
    // into the next_point from the cell to the right). And so we measure
    // the error against that next_point (and propagate it), as well
    // as add the error.
    //
    // The cells marked < do the same. The ones off the diagonal (e.g.
    // the bottom left one) can't be used in the solution.
    //
    // Finally, a general case, the cell $. It will be built from
    // neighbors 1, 2, 3. In this cell we're being asked to find the
    // best assignment of "cd" to "uvwx". We have two choices here;
    // assign c to u or not.
    //   - From cell 3, we have a solution to the subproblem "d" to "vwx".
    //     We can use this to solve the current problem by assigning c to u.
    //     We take its error, and since we made an assignment, we don't
    //     need its next_point. The next_point becomes this one.
    //   - From cell 2, we have a solution to the subproblem "d" to "uvwx".
    //     This is not useful here because we must assign "c" to something,
    //     but there's nothing to assign it 
    //   - From cell 1, a solution to the subproblem "cd" to "vwx".
    //     Here we don't assign anything to "u" and then get a solution
    //     to the current problem. We propagate the next_point from 1,
    //     and add error from "u" against that next_point.
    //
    // So we only use the cell to the right and down-right.
    //
    // So we can state that every cell contains the best error achievable
    // for that subproblem, but how do we know that the next_point is the
    // "best" next_point? When looking at $, "d" could have been assigned
    // to "w" or "x" with some (minimal) resulting error, but what if we
    // chose "w" and yet "c" is much closer to "x" than "w"? Now we'll have
    // to incur a large error, which may be larger than the savings from
    // the other points? Basically it seems like THIS APPROACH IS JUST
    // BUGGY.
    //
    // Since we'll go from the end of the vector, we need to fill
    // the right column (e = expected.size()) and bottom row
    // (a = actual.size()) first.

    // For the right column, the error is zero. The next point
    // is the assigned point, expected[0].
    for (int i = 0; i < actual.size(); i++) {
      Info &info = At(expected.size(), i);
      info.err = 0.0f;
      info.next_point = expected[0];
    }
    
    // For the bottom row, 
    
  }
}
