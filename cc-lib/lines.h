// Generate lines with Bresenham's algorithm. Use like this:
//
// #include <utility>
//
// /* Draw a line from (3, 4) to (-7, 8). */
// for (const std::pair<int, int> point : Line<int>{3, 4, -7, 8}) {
//   int x = point.first, y = point.second;
//   drawpixel(x, y);
// }

#ifndef __CC_LIB_LINES_H
#define __CC_LIB_LINES_H

#include <utility>
#include <type_traits>

template<class Int>
class Line {
 public:
  static_assert(std::is_integral<Int>::value, "Line<T> requires integral T.");
  Line(Int x0, Int y0, Int x1, Int y1);
  bool Next(Int *x, Int *y);

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

#endif
