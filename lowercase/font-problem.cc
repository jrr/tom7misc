
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
