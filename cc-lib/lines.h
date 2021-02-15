// Generate lines using some classic graphics algorithms.
// Templated on integer/float types since sometimes it is helpful
// to use lower-precision types in embedded applications.

#ifndef _CC_LIB_LINES_H
#define _CC_LIB_LINES_H

#include <utility>
#include <cmath>
#include <tuple>
#include <type_traits>
#include <vector>
#include <optional>
#include <functional>

// Generate lines with Bresenham's algorithm. Use like this:
//
// /* Draw a line from (3, 4) to (-7, 8). */
// for (const std::pair<int, int> point : Line<int>{3, 4, -7, 8}) {
//   int x = point.first, y = point.second;
//   drawpixel(x, y);
// }
//
// C++17:
// for (auto [x, y] : Line<int>{3, 4, -7, 8})
//   drawpixel(x, y);
template<class Int>
class Line {
 public:
  static_assert(std::is_integral<Int>::value, "Line<T> requires integral T.");
  Line(Int x0, Int y0, Int x1, Int y1);

  // This iterator is only designed for ranged-for loops; the operators
  // may have counter-intuitive behavior.
  struct iterator {
    std::pair<Int, Int> operator *() const { return {x, y}; }
    void operator ++();
    bool operator !=(const iterator &other) const;
   private:
    iterator(const Line &parent) : parent(parent) {}
    const Line &parent;
    Int x = 0, y = 0;
    Int frac = 0;
    friend class Line;
  };
  iterator begin() const;
  iterator end() const;

 private:
  // All members morally const.
  const Int x0, y0, x1, y1;
  Int dx, dy;
  Int stepx, stepy;
  Int start_frac;
};

// Anti-aliased line using Wu's algorithm. The pixel to plot is
// accompanied by a brightness fraction in [0, 1].
// This is not suited for an iterator-based interface since it treats
// the endpoints separately, and draws two pixels per iteration.
// Not possible to return early.
//
// /* Draw a line from (1.0, 3.1) to (-7, 8.5), using single-precision
//    floats (deduced from args) and int output. */
// LineAA::Draw<int>(1.0f, 3.1f, -7.0f, 8.5f,
//                   [](int x, int y, float f) {
//                       blendpixel(x, y, f * 255.0f);
//                   });
class LineAA {
public:
  template<class Int, class Float, class Fn>
  static void Draw(Float x0, Float y0, Float x1, Float y1, Fn drawpixel);
};



// Template implementations follow.

template<class Int>
Line<Int>::Line(Int x0, Int y0, Int x1, Int y1) :
  x0(x0), y0(y0), x1(x1), y1(y1) {
  dy = y1 - y0;
  dx = x1 - x0;

  if (dy < 0) {
    dy = -dy;
    stepy = -1;
  } else {
    stepy = 1;
  }

  if (dx < 0) {
    dx = -dx;
    stepx = -1;
  } else {
    stepx = 1;
  }

  dy <<= 1;
  dx <<= 1;

  if (dx > dy) {
    start_frac = dy - (dx >> 1);
  } else {
    start_frac = dx - (dy >> 1);
  }
}

template<class Int>
typename Line<Int>::iterator Line<Int>::begin() const {
  iterator it{*this};
  it.x = x0;
  it.y = y0;
  it.frac = start_frac;
  return it;
}

template<class Int>
typename Line<Int>::iterator Line<Int>::end() const {
  iterator it{*this};

  // One step beyond the end point, so that the line includes
  // (x1, y1).
  if (dx > dy) {
    it.x = x1 + stepx;
  } else {
    it.y = y1 + stepy;
  }
  return it;
}

template<class Int>
bool Line<Int>::iterator::operator !=(const iterator &other) const {
  return parent.dx > parent.dy ?
    x != other.x :
    y != other.y;
}

template<class Int>
void Line<Int>::iterator::operator ++() {
  if (parent.dx > parent.dy) {
    if (frac >= 0) {
      y += parent.stepy;
      frac -= parent.dx;
    }
    x += parent.stepx;
    frac += parent.dy;
  } else {
    if (frac >= 0) {
      x += parent.stepx;
      frac -= parent.dy;
    }
    y += parent.stepy;
    frac += parent.dx;
  }
}


// TODO: There may be some problem with the endpoint drawing;
// there seem to be discontinuities when drawing a polyline.
// (Could also be a problem with ImageRGBA::BlendPixel?)
template<class Int, class Float, class Fn>
void LineAA::Draw(Float x0, Float y0, Float x1, Float y1, Fn drawpixel) {
  static_assert(std::is_integral<Int>::value,
                "LineAA<T,F> requires integral T.");
  static_assert(std::is_floating_point<Float>::value,
                "LineAA<T,F> requires floating-point F.");

  // floor and round are each overloaded on float and double.
  auto ipart = [](Float x) -> Int { return Int(std::floor(x)); };
  auto round = [](Float x) -> Float { return std::round(x); };
  auto fpart = [](Float x) -> Float { return x - std::floor(x); };
  auto rfpart = [fpart](Float x) -> Float { return Float(1.0f) - fpart(x); };

  const bool steep = std::abs(y1 - y0) > std::abs(x1 - x0);
  if (steep) {
    std::swap(x0,y0);
    std::swap(x1,y1);
  }
  if (x0 > x1) {
    std::swap(x0,x1);
    std::swap(y0,y1);
  }

  const Float dx = x1 - x0;
  const Float dy = y1 - y0;
  const Float gradient = (dx == 0) ? 1 : dy / dx;

  Int xpx11;
  Float intery;
  {
    const Float xend = round(x0);
    const Float yend = y0 + gradient * (xend - x0);
    const Float xgap = rfpart(x0 + 0.5);
    xpx11 = Int(xend);
    const Int ypx11 = ipart(yend);
    if (steep) {
      drawpixel(ypx11, xpx11, rfpart(yend) * xgap);
      drawpixel(ypx11 + 1, xpx11, fpart(yend) * xgap);
    } else {
      drawpixel(xpx11, ypx11, rfpart(yend) * xgap);
      drawpixel(xpx11, ypx11 + 1, fpart(yend) * xgap);
    }
    intery = yend + gradient;
  }

  Int xpx12;
  {
    const Float xend = round(x1);
    const Float yend = y1 + gradient * (xend - x1);
    const Float xgap = rfpart(x1 + 0.5);
    xpx12 = Int(xend);
    const Int ypx12 = ipart(yend);
    if (steep) {
      drawpixel(ypx12, xpx12, rfpart(yend) * xgap);
      drawpixel(ypx12 + 1, xpx12, fpart(yend) * xgap);
    } else {
      drawpixel(xpx12, ypx12, rfpart(yend) * xgap);
      drawpixel(xpx12, ypx12 + 1, fpart(yend) * xgap);
    }
  }

  if (steep) {
    for (Int x = xpx11 + 1; x < xpx12; x++) {
      drawpixel(ipart(intery), x, rfpart(intery));
      drawpixel(ipart(intery) + 1, x, fpart(intery));
      intery += gradient;
    }
  } else {
    for (Int x = xpx11 + 1; x < xpx12; x++) {
      drawpixel(x, ipart(intery), rfpart(intery));
      drawpixel(x, ipart(intery) + 1, fpart(intery));
      intery += gradient;
    }
  }
}

// Compute the point of intersection between two line segments
// (given as their endpoints), or return nullopt if they do
// not intersect.
//
// (Note that even for double inputs, this does some float
// calculations and returns float. TODO: Could make it
// use double (or long double) if inputs are that type, with
// significant added trickery.)
// Ported from sml-lib.
template<class Num = float>
std::optional<std::pair<float, float>> LineIntersection(
    // First segment
    Num p0x, Num p0y, Num p1x, Num p1y,
    // Second segment
    Num p2x, Num p2y, Num p3x, Num p3y) {

  const auto s1x = p1x - p0x;
  const auto s1y = p1y - p0y;
  const auto s2x = p3x - p2x;
  const auto s2y = p3y - p2y;

  const auto l1 = p0x - p2x;
  const auto l2 = p0y - p2y;
  const float denom = s1x * s2y - s2x * s1y;

  const float s = (s1x * l2 - s1y * l1) / denom;

  if (s >= 0.0f && s <= 1.0f) {
    const float t = (s2x * l2 - s2y * l1) / denom;

    if (t >= 0.0f && t <= 1.0f) {
      return {{(float)p0x + (t * s1x),
               (float)p0y + (t * s1y)}};
    }
  }
  return std::nullopt;
}

// Return the closest point (to x,y) on the given line segment.
// It may be one of the endpoints.
inline std::pair<float, float>
ClosestPointOnSegment(
    // Line segment
    float x0, float y0, float x1, float y1,
    // Point to test
    float x, float y) {
  auto SqDist = [](float x0, float y0,
                   float x1, float y1) {
      const float dx = x1 - x0;
      const float dy = y1 - y0;
      return dx * dx + dy * dy;
    };

  const float sqlen = SqDist(x0, y0, x1, y1);
  if (sqlen == 0.0) {
    // Degenerate case where line segment is just a point,
    // so there is only one choice.
    return {x0, y0};
  }

  const float tf = ((x - x0) * (x1 - x0) + (y - y0) * (y1 - y0)) / sqlen;
  // Make sure it is on the segment.
  const float t = std::max(0.0f, std::min(1.0f, tf));
  // Closest point, which is on the segment.

  const float xx = x0 + t * (x1 - x0);
  const float yy = y0 + t * (y1 - y0);
  return {xx, yy};
}

// Return the minimum distance between the point and the line segment.
inline float PointLineDistance(
    // Line segment
    float x0, float y0, float x1, float y1,
    // Point to test
    float x, float y) {

  const auto [xx, yy] = ClosestPointOnSegment(x0, y0, x1, y1, x, y);
  const float dx = x - xx;
  const float dy = y - yy;
  return sqrtf(dx * dx + dy * dy);
}

// Same, but for a line that's known to be horizontal.
inline float PointHorizLineDistance(
    // Line segment
    float x0, float y0, float x1, /* y1 = y0 */
    // Point to test
    float x, float y) {
  // Put in order so that x0 < x1.
  if (x0 > x1) std::swap(x0, x1);
  const float dy = y0 - y;
  if (x <= x0) {
    // Distance is to left left.
    const float dx = x0 - x;
    return sqrtf(dx * dx + dy * dy);
  } else if (x >= x1) {
    // To right vertex.
    const float dx = x1 - x;
    return sqrtf(dx * dx + dy * dy);
  } else {
    // Perpendicular to segment itself.
    return fabsf(dy);
  }
}

// ... and vertical.
inline float PointVertLineDistance(
    // Line segment
    float x0, float y0, /* x1 = x0 */ float y1,
    // Point to test
    float x, float y) {
  // Put in order so that y0 < y1.
  if (y0 > y1) std::swap(y0, y1);
  const float dx = x0 - x;
  if (y <= y0) {
    // Distance is to top vertex.
    const float dy = y0 - y;
    return sqrtf(dx * dx + dy * dy);
  } else if (y >= y1) {
    // To bottom corner.
    const float dy = y1 - y;
    return sqrtf(dx * dx + dy * dy);
  } else {
    // Perpendicular to segment itself.
    return fabsf(dx);
  }
}


// Return a vector of endpoints, not including the start point (but
// including the end), to draw as individual line segments in order to
// approximate the given quadratic Bezier curve.

// Num should work as integral (then all math is integral) or
// floating-point types.
template<class Num = float>
std::vector<std::pair<Num, Num>> TesselateQuadraticBezier(
    // starting vertex
    Num x0, Num y0,
    // control point
    Num x1, Num y1,
    // end point
    Num x2, Num y2,
    Num max_error_squared = Num(2),
    int max_depth = 16) {

  static_assert(std::is_arithmetic<Num>::value,
                "TesselateQuadraticBezier needs an integral or floating-point "
                "template argument.");

  std::vector<std::pair<Num, Num>> out;
  std::function<void(Num, Num, Num, Num, Num, Num, int)> Rec =
    [&out, max_error_squared, &Rec](Num x0, Num y0,
                                    Num x1, Num y1,
                                    Num x2, Num y2,
                                    int max_depth) {
      // This is based on public-domain code from stb_truetype, thanks!

      // Midpoint of the curve.
      // ("Midpoint" here likely means t/2, not the geometric midpoint?
      // So this might be overly conservative, in that we might have
      // a good approximation to a line but not pass near the line's
      // midpoint at the curve's midpoint. (Consider the case where the
      // control point is on the line, near one of the endpoints.))
      const Num mx = (x0 + (x1 * 2) + x2) / 4;
      const Num my = (y0 + (y1 * 2) + y2) / 4;

      // Midpoint of a straight line.
      const Num lx = (x0 + x2) / 2;
      const Num ly = (y0 + y2) / 2;

      // Error.
      const Num dx = lx - mx;
      const Num dy = ly - my;
      const Num error = (dx * dx) + (dy * dy);

      if (error > max_error_squared && max_depth > 0) {
        Rec(x0, y0, (x0 + x1) / 2, (y0 + y1) / 2, mx, my, max_depth - 1);
        Rec(mx, my, (x1 + x2) / 2, (y1 + y2) / 2, x2, y2, max_depth - 1);
      } else {
        // Otherwise, emit a straight line.
        out.emplace_back(x2, y2);
      }
    };

  Rec(x0, y0, x1, y1, x2, y2, max_depth);
  return out;
}



#endif
