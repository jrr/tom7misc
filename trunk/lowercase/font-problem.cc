
#include "font-problem.h"

#include <vector>
#include <string>

#include "network.h"

#include "image.h"
#include "ttf.h"
#include "threadutil.h"

using namespace std;

using Contour = TTF::Contour;

void FontProblem::RenderVector(const string &font_filename,
			       const Network &net,
			       const vector<int> &row_max_points) {

  static constexpr int WIDTH = 1920;
  static constexpr int HEIGHT = 1080;
  
  TTF ttf{font_filename};
  ImageRGBA img{WIDTH, HEIGHT};

  img.Clear32(0x000000FF);

  Stimulation stim;
    
}

#if 0
		const vector<float> &values = stim.values[l];
		sdlutil::FillRectRGB(screen, xstart, ystart,
				     NOMINAL_CHAR_SIZE,
				     NOMINAL_CHAR_SIZE,
				     40, 40, 40);

		constexpr double sqerr = 1.0f / (NOMINAL_CHAR_SIZE *
						 NOMINAL_CHAR_SIZE);
		
		auto DrawPath = [xstart, ystart, &values](
		    int idx, int num_pts,
		    uint8 r, uint8 g, uint8 b) {
		    float x = values[idx + 0];
		    float y = values[idx + 1];

		    auto Line = [xstart, ystart,
				 r, g, b](float x1, float y1,
					  float x2, float y2) {
			sdlutil::drawclipline(
			    screen,
			    xstart + x1 * NOMINAL_CHAR_SIZE,
			    ystart + y1 * NOMINAL_CHAR_SIZE,
			    xstart + x2 * NOMINAL_CHAR_SIZE,
			    ystart + y2 * NOMINAL_CHAR_SIZE,
			    r, g, b);
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
		  };

		DrawPath(0, ROW0_MAX_PTS, 0xFF, 0xFF, 0x00);
		DrawPath(2 + ROW0_MAX_PTS * 4,
			 ROW1_MAX_PTS, 0x00, 0xFF, 0x00);
		DrawPath(2 + ROW0_MAX_PTS * 4 +
			 2 + ROW1_MAX_PTS * 4,
			 ROW2_MAX_PTS, 0x00, 0xFF, 0xFF);
#endif



bool FontProblem::FillVector(const TTF *ttf, int codepoint,
			     std::vector<int> &row_max_points,
			     float *buffer) {
  std::vector<TTF::Contour> contours =
    TTF::MakeOnlyBezier(ttf->GetContours(codepoint));

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

  // We need something to put in rows if there are fewer than
  // the maximum number of contours.
  // We treat this as an empty path starting at 0,0.
  while (contours.size() < row_max_points.size()) {
    TTF::Contour degenerate(0.0f, 0.0f);
    contours.push_back(degenerate);
  }

  for (int i = 0; i < contours.size(); i++) {
    if (contours[i].paths.size() > row_max_points[i]) {
      return false;
    }
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
