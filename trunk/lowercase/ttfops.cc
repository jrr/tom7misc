
#include "ttfops.h"

#include <cstdint>
#include <vector>
#include <utility>
#include <optional>
#include <tuple>
#include <cmath>

#include "opt/opt.h"

using namespace std;

using uint8 = uint8_t;

std::optional<std::tuple<double, double, double>>
TTFOps::GetSameCase(const TTF &ttf) {
  struct Pair {
    vector<pair<float, float>> ucase, lcase;
  };

  vector<Pair> pairs;

  for (char upper = 'A'; upper <= 'Z'; upper++) {
    const char lower = upper | 32;
    vector<TTF::Contour> ucase = ttf.GetContours(upper);
    vector<TTF::Contour> lcase = ttf.GetContours(lower);

    // Must have the same number of contours.
    if (ucase.size() != lcase.size()) {
      /*
      printf("Char '%c'/'%c': %d contours vs %d contours\n",
	     upper, lower, (int)ucase.size(), (int)lcase.size());
      */
      return nullopt;
    }

    Pair p;		  
    // Each contour must have the same number of paths
    // (and be in the same order).
    for (int i = 0; i < ucase.size(); i++) {
      const TTF::Contour &ucont = ucase[i];
      const TTF::Contour &lcont = lcase[i];
      p.ucase.emplace_back(ucont.startx, ucont.starty);
      p.lcase.emplace_back(lcont.startx, lcont.starty);

      const auto &up = ucont.paths;
      const auto &lp = lcont.paths;
      if (up.size() != lp.size()) {
	/*
	printf("Char '%c'/'%c': contour %d: %d paths vs %d paths\n",
	       upper, lower, i, (int)up.size(), (int)lp.size());
	*/
	return nullopt;
      }

      for (int j = 0; j < up.size(); j++) {
	// And same point type.
	if (up[j].type != lp[j].type) {
	  /*
	  printf("Char '%c'/'%c': contour %d point %d type mismatch\n",
		 upper, lower, i, j);
	  */
	  return nullopt;
	} else {
	  switch (up[j].type) {
	  case TTF::PathType::LINE:
	    p.ucase.emplace_back(up[j].x, up[j].y);
	    p.lcase.emplace_back(lp[j].x, lp[j].y);
	    break;
	  case TTF::PathType::BEZIER:
	    p.ucase.emplace_back(up[j].x, up[j].y);
	    p.ucase.emplace_back(up[j].cx, up[j].cy);
	    p.lcase.emplace_back(lp[j].x, lp[j].y);
	    p.lcase.emplace_back(lp[j].cx, lp[j].cy);
	    break;
	  }
	}
      }
    }

    // There have to be some points??
    if (p.ucase.empty()) {
      /*
      printf("empty points\n");
      */
      return nullopt;
    }
    CHECK(p.ucase.size() == p.lcase.size());

    pairs.push_back(std::move(p));
  }

  // All characters are structurally the same. Can
  // we find a linear transformation now?

  // Same scale factor applied to all points.
  auto GetErr = [&pairs](const std::array<double, 2> &args) {
      const auto [xscale, yscale] = args;
      double total_err = 0.0;
      for (const Pair &p : pairs) {
	// Each pair can have its own translation, though.
	// We just try to directly compute the best translation
	// on each pass.
	double tdx = 0.0f, tdy = 0.0f;
	for (int i = 0; i < p.ucase.size(); i++) {
	  const auto [ux, uy] = p.ucase[i];
	  const auto [lx, ly] = p.lcase[i];
	  tdx += lx - xscale * ux;
	  tdy += ly - yscale * uy;
	}

	// Average displacement doesn't actually minimize
	// error, but all we're really trying to do here
	// is to account for discretization error because
	// the underlying coordinates are int16. The
	// expectation is that the error is either really
	// close to zero or terrible.
	const double dx = tdx / p.ucase.size();
	const double dy = tdy / p.ucase.size();		    

	double err = 0.0f;
	for (int i = 0; i < p.ucase.size(); i++) {
	  const auto [ux, uy] = p.ucase[i];
	  const auto [lx, ly] = p.lcase[i];

	  // computed point
	  const double cx = xscale * ux + dx;
	  const double cy = yscale * uy + dy;

	  double ex = cx - lx;
	  double ey = cy - ly;
	  err += sqrt((ex * ex) + (ey * ey));
	}
	total_err += err;
      }

      return total_err;
    };

  const auto [best_scale, best_err] =
    Opt::Minimize<2>(GetErr, {0.1, 0.1}, {10.0, 10.0}, 1000);

  const auto [sx, sy] = best_scale;

  return {make_tuple(sx, sy, best_err)};
}


double TTFOps::CharBitmapDifference(const TTF &ttf,
				    int c1, int c2,
				    float scale,
				    float xscale2, float yscale2,
				    float xmov2, float ymov2) {
  
  const stbtt_fontinfo *info = ttf.Font();
  CHECK(info != nullptr);

  float stb_scale = stbtt_ScaleForPixelHeight(info, scale);

  /*
  printf("BitDiff '%c' vs '%c' scale %.2f  scale2  %.5f %.5f mov2 %.5f %.5f\n"
	 "stb_scale: %.5f\n",
	 c1, c2, scale, xscale2, yscale2, xmov2, ymov2,
	 stb_scale);
  */  
  int width1, height1, xoff1, yoff1;
  uint8 *bit1 = stbtt_GetCodepointBitmapSubpixel(info,
						 // uniform scale
						 stb_scale, stb_scale,
						 // unshifted
						 0.0f, 0.0f,
						 c1,
						 &width1, &height1,
						 &xoff1, &yoff1);
  if (!bit1) return 1.0;
  CHECK(bit1 != nullptr);
  
#if 0
  int int_x = xmov2, int_y = ymov2;
  const float subpixel_x = xmov2 - int_x;
  const float subpixel_y = ymov2 - int_y;
#endif
  
  // Get integral part and remainder (always in [0, 1)).
  auto SubPx = [](float f) -> pair<int, float> {
    int i = floorf(f);
    return {i, f - i};
  };

  const auto [int_x, subpixel_x] = SubPx(xmov2);
  const auto [int_y, subpixel_y] = SubPx(ymov2);

  #if 0
  printf("(got %dx%d) bitmap2(%.16f,%.16f,%.16f,%.16f)\n", width1, height1,
	 stb_scale * xscale2, stb_scale * yscale2,
	 subpixel_x, subpixel_y);
  #endif
  int width2, height2, xoff2, yoff2;
  uint8 *bit2 = stbtt_GetCodepointBitmapSubpixel(info,
						 stb_scale * xscale2, stb_scale * yscale2,
						 subpixel_x, subpixel_y,
						 c2,
						 &width2, &height2,
						 &xoff2, &yoff2);
  if (!bit2) {
    // bit1 was successfully allocated though
    stbtt_FreeBitmap(bit1, nullptr);    
    return 1.0;
  }
  CHECK(bit2 != nullptr);
  
  // Here we're working in a coordinate space where 0,0 is the origin for both
  // characters, and the scale is in pixels of the rendered bitmaps. The top left
  // of the bitmap (and the bounding box containing both bitmaps) is typically
  // negative:
  const int minx = std::min(xoff1, int_x + xoff2);
  const int miny = std::min(yoff1, int_y + yoff2);
  // (one past the right, bottom)
  const int maxx = std::max(width1 + xoff1,  width2 + int_x + xoff2);
  const int maxy = std::max(height1 + yoff1, height2 + int_y + yoff2);

  // Width and height of this bounding box.
  const int bbw = maxx - minx, bbh = maxy - miny;

  auto GetPx = [](const uint8 *bm, int width, int height, int x, int y) -> uint8 {
      // Empty outside the bitmap itself.
      if (x < 0 || y < 0 ||
	  x >= width || y >= height) return 0;
      return bm[y * width + x];
    };

  // For each pixel in the second bitmap, absolute difference between it
  // and the the aligned pixel in the first. Max difference per pixel 255.
  int numer255 = 0;
  const int denom = width2 * height2;
  // x,y now pixel coordinates in the bounding box.
  for (int y = 0; y < bbh; y++) {
    for (int x = 0; x < bbw; x++) {
      // coordinates in the shared font space
      const int ox = x + minx;
      const int oy = y + miny;

      // And coordinates within each bitmap (but they may actually be
      // out of bounds).
      const int x1 = ox - xoff1;
      const int y1 = oy - yoff1;

      const int x2 = ox - (xoff2 + int_x);
      const int y2 = oy - (yoff2 + int_y);

      // Pixel values for each bitmap.
      // Here 0 means transparent.
      const uint8 v1 = GetPx(bit1, width1, height1, x1, y1);
      const uint8 v2 = GetPx(bit2, width2, height2, x2, y2);

      if (x2 >= 0 && y2 >= 0 &&
	  x2 < width2 && y2 < height2) {
	numer255 += abs((int)v2 - (int)v1);
      }
    }
  }

  const double err = numer255 / (denom * 255.0);

  stbtt_FreeBitmap(bit1, nullptr);
  stbtt_FreeBitmap(bit2, nullptr);
  return err;
}


double TTFOps::TotalAlphabetDifference(const TTF &ttf,
				       float bitmap_scale,
				       int iters_per_char,
				       float threshold) {
  double total_best_error = 0.0;

  // for (int c1 = 'A'; c1 <= 'Z'; c1++) {
  {
    int c1 = 'D';
    int c2 = c1 | 32;

    [[maybe_unused]]
    int calls = 0;
    auto GetErr = [&ttf, c1, c2, bitmap_scale, &calls](const std::array<double, 4> &args) {
	const auto [xscale2, yscale2, xoff2, yoff2] = args;
	calls++;
	return CharBitmapDifference(ttf,
				    c1, c2,
				    bitmap_scale,
				    xscale2, yscale2,
				    xoff2, yoff2);
      };

/*
    printf("'%c' vs '%c'...\n", c1, c2);
*/
    // PERF: If it were possible to provide a guess here, we could probably
    // provide a very good guess (start with identity, then use previous char).
    const auto [args, err] = 
//      Opt::Minimize<4>(GetErr, {0.8, 0.8, -10.0, -10.0}, {1.2, 1.2, 10.0, 10.0},
//		       iters_per_char);
      Opt::Minimize<4>(GetErr, {0.33, 0.33, -50.0, -50.0}, {3.0, 3.0, 50.0, 50.0},
		       iters_per_char);

//     printf("Called %d times.\n", calls);

    total_best_error += err;
    if (total_best_error >= threshold)
      return total_best_error;
  }
  
  return total_best_error;
}


