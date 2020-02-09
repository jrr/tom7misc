// Generate lines using some classic graphics algorithms.
// Templated on integer/float types since sometimes it is helpful
// to use lower-precision types in embedded applications.

#ifndef __CC_LIB_LINES_H
#define __CC_LIB_LINES_H

#include <utility>
#include <cmath>
#include <tuple>
#include <type_traits>

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


#endif
