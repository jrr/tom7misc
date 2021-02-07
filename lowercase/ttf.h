
#ifndef _LOWERCASE_TTF_H
#define _LOWERCASE_TTF_H

#include <string>
#include <vector>
#include <cstdint>
#include <optional>
#include <cmath>

#include "stb_truetype.h"
#include "util.h"
#include "base/logging.h"
#include "image.h"

// TODO: Split to .cc and .h
// TODO: Only expose normalized coordinates...
struct TTF {
  using string = std::string;

  const stbtt_fontinfo *Font() const { return &font; }
  
  explicit TTF(const string &filename) {
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

    stbtt_GetFontVMetrics(
        &font, &native_ascent, &native_descent, &native_linegap);

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

    /*
    printf("native ascent %d descent %d linegap %d.\n"
           "norm = %.5f  baseline = %.5f   lineheight %.5f\n",
           native_ascent, native_descent, native_linegap,
           norm, baseline, NormLineHeight());
    */

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
  pair<float, float> Norm(float x, float y) const {
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
  float NormLineHeight() const {
    // Note: Lots of fonts have an incorrect descent (i.e., positive
    // when it should be negative). Maybe it's worth just
    // heuristically taking +abs(native_descent)?
    int native = (native_ascent - native_descent) + native_linegap;
    // ( = native / (native_ascent - native_descent)
    return native * norm;
  }
  
  // Not cached, so this does a lot more allocation than you probably want.
  ImageA GetChar(char c, int size) {
    int width, height;
    uint8_t *bitmap =
      stbtt_GetCodepointBitmap(
          &font, 0, stbtt_ScaleForPixelHeight(&font, size), c,
          &width, &height, 0, 0);
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
      stbtt_GetCodepointHMetrics(
          &font, text[idx], &advance, &left_side_bearing);

      int bitmap_w = 0, bitmap_h = 0;
      int xoff = 0, yoff = 0;
      uint8_t *bitmap = nullptr;      
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
            DrawPixel(xpos + xx + xoff, ypos + yy + yoff,
                      bitmap[yy * bitmap_w + xx]);
          }
        }
        stbtt_FreeBitmap(bitmap, nullptr);
      }
        
      xpos += advance * scale;
      if (text[idx + 1] != '\0') {
        xpos += scale *
          stbtt_GetCodepointKernAdvance(&font, text[idx], text[idx + 1]);
      }
      
      if (!subpixel) {
        // Or floor?
        xpos = roundf(xpos);
      }
    }
  }

  // Uses SCREEN COORDINATES.
  // Measure the nominal width and height of the string using the same
  // method as above. (This does not mean that all pixels lie within
  // the rectangle.)
  std::pair<int, int>
  MeasureString(const string &text, int size_px, bool subpixel = true) {
    const float scale = stbtt_ScaleForPixelHeight(&font, size_px);

    int ascent = 0, descent = 0, line_gap = 0;
    stbtt_GetFontVMetrics(&font, &ascent, &descent, &line_gap);

    float xpos = 0.0f;
    for (int idx = 0; idx < (int)text.size(); idx++) {

      int advance = 0, left_side_bearing = 0;
      stbtt_GetCodepointHMetrics(
          &font, text[idx], &advance, &left_side_bearing);

      xpos += advance * scale;
      if (text[idx + 1] != '\0') {
        xpos += scale *
          stbtt_GetCodepointKernAdvance(&font, text[idx], text[idx + 1]);
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

  // There can be clockwise and counterclockwise (thinking about non-
  // self-intersecting) contours. Clockwise increases the winding count
  // by 1; counter-clockwise decreases it.
  // 
  // TTF uses the nonzero winding rule: As a ray passes from a point
  // (pixel) to infinity (any direction works), the sum of contours it
  // crosses gives the winding number. If zero, it is "white" (outside
  // the shape). If nonzero, it is "black".
  //
  // Because negative is also nonzero, a shape without holes like an
  // 'm' can be drawn with either winding order (XXX check), although
  // clockwise is normative.
  //
  // XXX is this expected to be closed?
  // (TODO: They are always closed. We can probably simplify some code
  // by making StartX() and StartY() which just return the end point
  // of the last path.)
  struct Contour {
    float startx = 0.0f, starty = 0.0f;
    std::vector<Path> paths;
    Contour(float startx, float starty) : startx(startx), starty(starty) {}
  };

  std::vector<Contour> GetContours(int codepoint) const {
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

  // For a vector of contours describing a shape, convert any lines
  // into equivalent Bezier curves (with the control point at the
  // midpoint of the line). Beziers are 
  static std::vector<Contour> MakeOnlyBezier(
      const std::vector<Contour> &contours) {
    std::vector<Contour> out;
    out.reserve(contours.size());
    for (const Contour &c : contours) {
      Contour o{c.startx, c.starty};
      float x = c.startx, y = c.starty;
      for (const Path &p : c.paths) {
        switch (p.type) {
        case PathType::LINE: {
          // This is equivalent to a Bezier curve where the control
          // point lies (anywhere) on the line segment. The nicest
          // choice seems to be the midpoint.
          float mx = (p.x + x) * 0.5f;
          float my = (p.y + y) * 0.5f;
          o.paths.emplace_back(p.x, p.y, mx, my);
          x = p.x;
          y = p.y;
          break;
        }
        case PathType::BEZIER:
          o.paths.emplace_back(p.x, p.y, p.cx, p.cy);
          x = p.x;
          y = p.y;
          break;
        }
      }
      out.emplace_back(std::move(o));
    }
    return out;
  }

  // A contour is just a loop so it can equivalently start at any of
  // its points. This picks the point closest to the given origin.
  // Doesn't change winding orders.
  //
  // Requires that the last path end exactly on the start point. This will
  // be the
  static std::vector<Contour> NormalizeOrder(
      const std::vector<Contour> &contours,
      float origin_x, float origin_y) {
    auto SqDist = [origin_x, origin_y](float x, float y) {
        float dx = (x - origin_x);
        float dy = (y - origin_y);
        return (dx * dx) + (dy * dy);
      };
    std::vector<Contour> out;
    out.reserve(contours.size());
    for (const Contour &c : contours) {
      // index (of end point) closest to the origin point.
      // -1 means the start point was already closest.
      int bestidx = -1;
      float best_sqerr = SqDist(c.startx, c.starty);
      for (int i = 0; i < c.paths.size(); i++) {
        // Both LINE and BEZIER have an end point.
        const float sqerr = SqDist(c.paths[i].x, c.paths[i].y);
        if (sqerr < best_sqerr) {
          bestidx = i;
          best_sqerr = sqerr;
        }
      }
      
      if (bestidx == -1) {
        out.push_back(c);
      } else {
        // Some end point was closer than the current start.
        // Start there instead.
        float x = c.paths[bestidx].x;
        float y = c.paths[bestidx].y;
        Contour r{x, y};
        // Paths that come after it.
        for (int i = bestidx + 1; i < c.paths.size(); i++) {
          r.paths.push_back(c.paths[i]);
          x = c.paths[i].x;
          y = c.paths[i].y;
        }
        // When we get to the end, we assume a closed path.
        // (If not, we could always insert a LINE here.
        CHECK_EQ(c.startx, x) << c.startx << " " << x;
        CHECK_EQ(c.starty, y) << c.starty << " " << y;
        // Because the path was closed, our cursor is already
        // on startx, starty.
        for (int i = 0; i <= bestidx; i++) {
          r.paths.push_back(c.paths[i]);
          x = c.paths[i].x;
          y = c.paths[i].y;
        }
        out.push_back(std::move(r));
      }
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


  // Get SDF for the character. This is tuned for ML applications, not
  // graphics.
  //
  //         sdf_size
  //  +---------------------+
  //  |      |              |
  //  |     pad top         | 
  //  |      |              | s 
  //  |     +---------+     | d
  //  |     |         :     | f
  //  |     |         : h   | _
  //  |-pad-|         : t   | s
  //  | left|         :     | i
  //  |     +---------+     | z
  //  |   origin   |        | e
  //  |           pad bot   |
  //  |            |        |  
  //  +---------------------+
  //
  // The output image will be sdf_size * sdf_size if successful. The
  // character is rendered at the nominal pixel height ht, which
  // is sdf_size - pad * 2.
  // The character is registered with its origin at (pad, pad + ht);
  // this means that two SDFs drawn on top of one another have the
  // character in the correct relative position. Padding allows the
  // character to extend outside the box (this is normal: a
  // character like w is wider than its nominal height; a character
  // like j typically descends below the origin and to its left; even
  // o typically drops slightly below the baseline and left edge.)
  // However, if pixels that are inside the character (sdf value >=
  // onedge_value) fall outside the bitmap, nullopt is returned.
  // Internally we render the SDF with lots of padding so that the
  // entire output should be filled with the actual distance function,
  // but of course it must be cropped at the output's edges.
  //
  // onedge_value and falloff_per_pixel are the same as in the stb_truetype
  // lib. A value ~200 seems good for onedge_value because exterior
  // distances are typically much higher than interior (especially with padding).
  // Unclear what a good falloff_per_pixel is, but it probably makes
  // sense to use the full dynamic range. The longest edge is from the
  // center to a corner (sqrt(2) * sdf_size / 2), which would be a
  // falloff of (sqrt(2) * sdf_size / 2) / 200 = 0.0035 * sdf_size.
  // Alternatively, we could take the center box (ht * ht) to be
  // the nominal character's edge (or at least the closest we "normally"
  // get to the sdf edge), and expect to get to zero along the
  // shortest edge; this distance is "pad". So we'd have 200 / pad.
  // So, suggestion is 0.0035 * sdf_size <= falloff <= 200 / pad
  // for an onedge_value of 200.
  //
  // Padding can actually be specified separately for top, bottom, and left.
  // (Pad right would not actually do anything.)
  // left = bottom and 1:3 ratio of top:bottom seems to be generally efficient.
  std::optional<ImageA> GetSDF(char c,
                               int sdf_size,
                               int pad_top, int pad_bot, int pad_left,
                               uint8_t onedge_value,
                               float falloff_per_pixel) const {
    CHECK(sdf_size > 0);
    CHECK(pad_top >= 0 && pad_top < sdf_size);
    CHECK(pad_left >= 0 && pad_left < sdf_size);
    CHECK(pad_bot >= 0 && pad_bot < sdf_size);
    const int ht = sdf_size - (pad_top + pad_bot);
    CHECK(ht > 0);

    // Location of origin in output.
    const int sdf_ox = pad_left;
    const int sdf_oy = pad_top + ht;

    // PERF: Internally, we pad with sdf_size on every side, which guarantees
    // we'll have enough pixels as long as the origin is inside the
    // bitmap. But this is probably overkill!
    const int stb_pad = sdf_size;
    const float stb_scale = stbtt_ScaleForPixelHeight(&font, ht);

    int width, height, xoff, yoff;
    uint8_t *bit = stbtt_GetCodepointSDF(&font,
                                         stb_scale,
                                         c,
                                         // padding
                                         stb_pad,
                                         // onedge value
                                         onedge_value,
                                         // falloff per pixel
                                         falloff_per_pixel,
                                         &width, &height,
                                         &xoff, &yoff);
    // This just fails for some fonts?
    if (bit == nullptr)
      return {};
      
    // printf("stb stf: %dx%d, off %d %d\n", width, height, xoff, yoff);
    
    ImageA out{sdf_size, sdf_size};

    // Now fill the whole sdf image.
    // sx,sy are pixel coordinates in the output sdf.
    // bx,by are pixel coordinates in the stb sdf bitmap.
    // Note xoff/yoff are typically negative (whereas sdf_o(x,y) are positive;
    // the coordinates of the origin within the bitmap.
    for (int sy = 0; sy < sdf_size; sy++) {
      const int by = (sy - sdf_oy) + (-yoff);
      // output sdf isn't covered
      if (by < 0 || by >= height) {
        stbtt_FreeSDF(bit, nullptr);
        return {};
      }
      
      for (int sx = 0; sx < sdf_size; sx++) {
        const int bx = (sx - sdf_ox) + (-xoff);

        // output sdf isn't covered
        if (bx < 0 || bx >= width) {
          stbtt_FreeSDF(bit, nullptr);
          return {};
        }
        out.SetPixel(sx, sy, bit[by * width + bx]);
      }
    }

    // XXX check that there are not interior points that fall outside
    // the output sdf.    
    // here sx,sy,bx,by have the same meaning as above, but we loop
    // over bitmap pixels and convert to sdf pixels.
    for (int by = 0; by < height; by++) {
      const int sy = by + yoff + sdf_oy;
      for (int bx = 0; bx < width; bx++) {
        const int sx = bx + xoff + sdf_ox;

        uint8_t v = bit[by * width + bx];
        if (v >= onedge_value) {
          if (sy < 0 || sy >= sdf_size ||
              sx < 0 || sx >= sdf_size) {
            // Some interior pixel was outside the output SDF.
            // Would need more padding (or something else is weird).
            stbtt_FreeSDF(bit, nullptr);
            return {};
          }
        }
      }
    }
    
    stbtt_FreeSDF(bit, nullptr);
    return {out};
  }
  
private:

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
  std::vector<NativeContour> GetNativeContours(int codepoint) const {
    stbtt_vertex *vertices = nullptr;
    const int n = stbtt_GetCodepointShape(&font, codepoint, &vertices);
    if (n == 0) return {};
    CHECK(vertices != nullptr);

    CHECK(vertices[0].type == STBTT_vmove) <<
      "All shapes should start with a moveto?";
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
  

  std::vector<uint8_t> ttf_bytes;
  stbtt_fontinfo font;

  TTF(const TTF &other) = delete;
  TTF &operator =(const TTF &other) = delete;
};

#endif
