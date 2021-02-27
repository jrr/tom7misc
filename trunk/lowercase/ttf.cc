#include "ttf.h"

#include <string>
#include <vector>
#include <optional>

#include "base/logging.h"

TTF::TTF(const string &filename) {
  ttf_bytes = Util::ReadFileBytes(filename);
  CHECK(!ttf_bytes.empty()) << filename;

  int offset = stbtt_GetFontOffsetForIndex(ttf_bytes.data(), 0);
  CHECK(offset != -1);
  CHECK(stbtt_InitFont(&font, ttf_bytes.data(), offset)) <<
    "Failed to load " << filename;
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


std::pair<int, int>
TTF::MeasureString(const string &text, int size_px, bool subpixel) const {
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


std::vector<TTF::Contour> TTF::GetContours(int codepoint) const {
  std::vector<NativeContour> ncs = GetNativeContours(codepoint);
  std::vector<Contour> out;
  out.reserve(ncs.size());
  for (const NativeContour &nc : ncs) {
    Contour c;
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

    const auto [sx, sy] = Norm(nc.StartX(), nc.StartY());
    CHECK_EQ(c.StartX(), sx);
    CHECK_EQ(c.StartY(), sy);
    out.emplace_back(std::move(c));
  }
  return out;
}


std::vector<TTF::Contour> TTF::MakeOnlyBezier(
    const std::vector<Contour> &contours) {
  std::vector<Contour> out;
  out.reserve(contours.size());
  for (const Contour &c : contours) {
    Contour o;
    float x = c.StartX(), y = c.StartY();
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

std::vector<TTF::Contour> TTF::NormalizeOrder(
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
    float best_sqerr = SqDist(c.StartX(), c.StartY());
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
      Contour r;
      // Paths that come after it.
      for (int i = bestidx + 1; i < c.paths.size(); i++) {
        r.paths.push_back(c.paths[i]);
        x = c.paths[i].x;
        y = c.paths[i].y;
      }
      // When we get to the end, we assume a closed path.
      // (If not, we could always insert a LINE here.
      CHECK_EQ(c.StartX(), x) << c.StartX() << " " << x;
      CHECK_EQ(c.StartY(), y) << c.StartY() << " " << y;
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

std::optional<ImageA> TTF::GetSDF(char c,
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

  // Check that there are not interior points that fall outside
  // the output sdf.
  // Here sx,sy,bx,by have the same meaning as above, but we loop
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


// Can be empty (e.g. for 'space' character)
std::vector<TTF::NativeContour> TTF::GetNativeContours(int codepoint) const {
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
      cur.emplace();
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

// TODO: Guts of GetGlyphSDF working on Contours, so that we
// can interactively make fonts and then make predictions from them.
// It looks basically tractable. Ideally, we would share the code
// with GetSDF.

