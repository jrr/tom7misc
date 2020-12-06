
#ifndef _LOWERCASE_TTF_H
#define _LOWERCASE_TTF_H

#include <string>
#include <vector>
#include <cstdint>
#include <optional>

#include "stb_truetype.h"
#include "util.h"
#include "base/logging.h"
#include "image.h"

// TODO: Only expose normalized coordinates...
struct TTF {
  using string = std::string;
  
  TTF(const string &filename) {
    ttf_bytes = Util::ReadFileBytes(filename);
    CHECK(!ttf_bytes.empty()) << filename;

    int offset = stbtt_GetFontOffsetForIndex(ttf_bytes.data(), 0);
    CHECK(offset != -1);
    CHECK(stbtt_InitFont(&font, ttf_bytes.data(), offset)) <<
      "Failed to load " << filename;

    /*
      stbtt_GetFontBoundingBox(&font, &min_x, &min_y, &max_x, &max_y);
      printf("Bounding box for all chars: %d,%d -> %d,%d\n",
      min_x, min_y, max_x, max_y);
      max_height = max_y - min_y;
      norm = 1.0f / max_height;
    */

    stbtt_GetFontVMetrics(&font, &native_ascent, &native_descent, &native_linegap);

    // We use a normalized representation like this:
    //
    //      0,0
    //      +------------------------ ascent = 0.0
    //      |
    //      |
    //      |      ,---,
    //      |     |     |
    //      |     `.___.`
    //      |
    //      |       ___
    //      |      /   |
    //      |      `.  |
    //      |       |  |
    //      |       |  |
    //      |       |  |
    //      |       |  |
    //      +-------+--+------------- baseline = 0.75
    //      |       |  |
    //      |      /   ;
    //      |     /   /
    //      |    /   /
    //     -+---`   /
    //    | |      /
    //    `-+------  ---------------- descent = 1.0
    //
    // Note, y-axis is flipped.
    //
    // Note the coordinates can be negative or greater than 1.0, but
    // they are "nominally" in [0,1] vertically. It is normal for
    // horizontal coordinates to be significantly larger than 1.

    // Note that native_descent is frequently wrong in fonts (it is positive).
    // I don't try to fix that here.
    
    int height = native_ascent - native_descent;
    norm = 1.0f / height;
    baseline = native_ascent * norm;

    printf("native ascent %d descent %d linegap %d.\n"
	   "norm = %.5f  baseline = %.5f   lineheight %.5f\n",
	   native_ascent, native_descent, native_linegap,
	   norm, baseline, NormLineHeight());

    #if 0
    int os2ascent, os2descent, os2lg;
    if (stbtt_GetFontVMetricsOS2(&font, &os2ascent, &os2descent, &os2lg)) {
      printf("OS/2 versions: %d %d %d\n", os2ascent, os2descent, os2lg);
    }
    #endif
  }

  // font uses "y positive up" coordinates.
  // ascent is the coordinate above the baseline (typically positive) and
  // descent is the coordinate below the baseline (typically negative).
  int native_ascent = 0, native_descent = 0;
  int native_linegap = 0;
  float norm = 0.0;
  float baseline = 0.0f;
  // By definition: ascent = 0.0, descent = 0.0
  
  // Normalizes an input coordinate (which is usually int16) to be
  // *nominally* in the unit rectangle. 
  pair<float, float> Norm(float x, float y) {
    // x coordinate is easy; just scale by the same factor.
    x = norm * x;
    // y is flipped (want +y downward) and offset (want ascent to be 0.0).

    // flip around baseline
    y = -y;
    // baseline (0) becomes ascent
    y += native_ascent;
    y = norm * y;
    return {x, y};
  }

  // amount to advance from one line of text to the next. This would be +1.0 by
  // definition except that we also take into account the "line gap".
  float NormLineHeight() {
    // Note: Lots of fonts have an incorrect descent (i.e., positive when it should
    // be negative). Maybe it's worth just heuristically taking +abs(native_descent)?
    int native = (native_ascent - native_descent) + native_linegap;
    // ( = native / (native_ascent - native_descent)
    return native * norm;
  }
  
  // Not cached, so this does a lot more allocation than you probably want.
  ImageA GetChar(char c, int size) {
    int width, height;
    uint8 *bitmap = stbtt_GetCodepointBitmap(&font, 0, stbtt_ScaleForPixelHeight(&font, size),
					     c, &width, &height, 0, 0);
    CHECK(bitmap != nullptr) << "Character " << (char)c << " size " << size;

    const int bytes = width * height;
    std::vector<uint8_t> ret;
    ret.resize(bytes);
    memcpy(ret.data(), bitmap, bytes);
    stbtt_FreeBitmap(bitmap, nullptr);
    return ImageA(std::move(ret), width, height);
  }

  // Pass DrawPixel(int x, int y, uint8 v) which should do the pixel blending.
  // XXX y position is the baseline, I think, but I have not really tested this.
  template<class DP>
  void BlitString(int x, int y, int size_px,
		  const string &text, const DP &DrawPixel,
		  bool subpixel = true) {
    const float scale = stbtt_ScaleForPixelHeight(&font, size_px);

    const int baseline = [&]() {
	int ascent = 0;
	stbtt_GetFontVMetrics(&font, &ascent, 0, 0);
	return (int) (ascent * scale);
      }();

    const int ypos = y + baseline;
    // Should stay integral if subpixel is false.
    float xpos = x;
    for (int idx = 0; idx < (int)text.size(); idx++) {

      int advance = 0, left_side_bearing = 0;
      stbtt_GetCodepointHMetrics(&font, text[idx], &advance, &left_side_bearing);

      int bitmap_w = 0, bitmap_h = 0;
      int xoff = 0, yoff = 0;
      uint8 *bitmap = nullptr;      
      if (subpixel) {
	const float x_shift = xpos - (float) floor(xpos);
	constexpr float y_shift = 0.0f;
	bitmap = stbtt_GetCodepointBitmapSubpixel(&font, scale, scale,
						  x_shift, y_shift,
						  text[idx],
						  &bitmap_w, &bitmap_h,
						  &xoff, &yoff);
      } else {
	bitmap = stbtt_GetCodepointBitmap(&font, scale, scale,
					  text[idx],
					  &bitmap_w, &bitmap_h,
					  &xoff, &yoff);
      }
      if (bitmap != nullptr) {
	for (int yy = 0; yy < bitmap_h; yy++) {
	  for (int xx = 0; xx < bitmap_w; xx++) {
	    DrawPixel(xpos + xx + xoff, ypos + yy + yoff, bitmap[yy * bitmap_w + xx]);
	  }
	}
	stbtt_FreeBitmap(bitmap, nullptr);
      }
	
      xpos += advance * scale;
      if (text[idx + 1] != '\0') {
	xpos += scale * stbtt_GetCodepointKernAdvance(&font, text[idx], text[idx + 1]);
      }
      
      if (!subpixel) {
	// Or floor?
	xpos = roundf(xpos);
      }
    }
  }

  // Uses SCREEN COORDINATES.
  // Measure the nominal width and height of the string using the same method as above.
  // (This does not mean that all pixels lie within the rectangle.)
  std::pair<int, int> MeasureString(const string &text, int size_px, bool subpixel = true) {
    const float scale = stbtt_ScaleForPixelHeight(&font, size_px);

    int ascent = 0, descent = 0, line_gap = 0;
    stbtt_GetFontVMetrics(&font, &ascent, &descent, &line_gap);

    float xpos = 0.0f;
    for (int idx = 0; idx < (int)text.size(); idx++) {

      int advance = 0, left_side_bearing = 0;
      stbtt_GetCodepointHMetrics(&font, text[idx], &advance, &left_side_bearing);

      xpos += advance * scale;
      if (text[idx + 1] != '\0') {
	xpos += scale * stbtt_GetCodepointKernAdvance(&font, text[idx], text[idx + 1]);
      }
      
      if (!subpixel) {
	// Or floor?
	xpos = roundf(xpos);
      }
    }
    return {xpos, ascent - descent + line_gap};
  }

  enum class PathType {
    LINE,
    // Quadratic bezier (one control point).
    BEZIER,
  };

  struct NativePath {
    PathType type = PathType::LINE;
    int x = 0, y = 0;
    // For Bezier curves.
    int cx = 0, cy = 0;

    NativePath(int x, int y) : type(PathType::LINE), x(x), y(y) {}
    NativePath(int x, int y, int cx, int cy) :
      type(PathType::BEZIER), x(x), y(y), cx(cx), cy(cy) {}
  };
  
  // Note: Source integer coordinates are int16.
  // XXX is this expected to be closed?
  struct NativeContour {
    int startx = 0, starty = 0;
    std::vector<NativePath> paths;
    NativeContour(int startx, int starty) : startx(startx), starty(starty) {}
  };

  // Can be empty (e.g. for 'space' character)
  std::vector<NativeContour> GetNativeContours(int codepoint) {
    stbtt_vertex *vertices = nullptr;
    const int n = stbtt_GetCodepointShape(&font, codepoint, &vertices);
    if (n == 0) return {};
    CHECK(vertices != nullptr);

    CHECK(vertices[0].type == STBTT_vmove) << "All shapes should start with a moveto?";
    std::optional<NativeContour> cur;
    std::vector<NativeContour> out;
    for (int i = 0; i < n; i++) {
      const stbtt_vertex &v = vertices[i];
      switch (v.type) {
      case STBTT_vmove:
	if (cur.has_value()) {
	  out.emplace_back(cur.value());
	}
	cur.emplace(v.x, v.y);
	break;
      case STBTT_vline:
	CHECK(cur.has_value());
	cur.value().paths.emplace_back(v.x, v.y);
	break;
      case STBTT_vcurve:
	// printf("vcurve\n");
	CHECK(cur.has_value());
	cur.value().paths.emplace_back(v.x, v.y, v.cx, v.cy);
	break;
      case STBTT_vcubic:
	CHECK(false) << "Expected only quadratic splines in TTF :(";
	break;
      }
    }
    CHECK(cur.has_value());
    out.emplace_back(cur.value());
    
    stbtt_FreeShape(&font, vertices);
    return out;
  }

  // Normalized path.
  struct Path {
    PathType type = PathType::LINE;
    float x = 0.0f, y = 0.0f;
    // For Bezier curves.
    float cx = 0.0f, cy = 0.0f;

    Path(float x, float y) : type(PathType::LINE), x(x), y(y) {}
    Path(float x, float y, float cx, float cy) :
      type(PathType::BEZIER), x(x), y(y), cx(cx), cy(cy) {}
  };
  
  // XXX is this expected to be closed?
  struct Contour {
    float startx = 0.0f, starty = 0.0f;
    std::vector<Path> paths;
    Contour(float startx, float starty) : startx(startx), starty(starty) {}
  };

  std::vector<Contour> GetContours(int codepoint) {
    std::vector<NativeContour> ncs = GetNativeContours(codepoint);
    std::vector<Contour> out;
    out.reserve(ncs.size());
    for (const NativeContour &nc : ncs) {
      const auto [sx, sy] = Norm(nc.startx, nc.starty);
      Contour c{sx, sy};
      c.paths.reserve(nc.paths.size());
      for (const NativePath &np : nc.paths) {
	switch (np.type) {
	case PathType::LINE: {
	  const auto [x, y] = Norm(np.x, np.y);
	  c.paths.emplace_back(x, y);
	  break;
	}
	case PathType::BEZIER: {
	  const auto [x, y] = Norm(np.x, np.y);
	  const auto [cx, cy] = Norm(np.cx, np.cy);
	  c.paths.emplace_back(x, y, cx, cy);
	  break;
	}
	}
      }
      out.emplace_back(std::move(c));
    }
    return out;
  }
  
  // c2 may be 0 for no kerning.
  float NormKernAdvance(char c1, char c2) {
    int advance = 0;
    stbtt_GetCodepointHMetrics(&font, c1, &advance, nullptr);
    if (c2 != 0) {
      advance += stbtt_GetCodepointKernAdvance(&font, c1, c2);
    }
    return Norm(advance, 1.0f).first;
  }
  
private:
  std::vector<uint8_t> ttf_bytes;
  stbtt_fontinfo font;
};

#endif
