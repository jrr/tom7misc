#include "bounds.h"

#include <limits>
#include <utility>
#include <algorithm>

Bounds::Bounds() {}

void Bounds::Bound(double x, double y) {
  auto B = [](double p, double *min, double *max) {
    if (p < *min) *min = p;
    if (p > *max) *max = p;
  };

  B(x, &minx, &maxx);
  B(y, &miny, &maxy);
  is_empty = false;
}
void Bounds::Bound(std::pair<double, double> p) {
  Bound(p.first, p.second);
}

bool Bounds::Empty() const { return is_empty; }

double Bounds::MinX() const { return minx; }
double Bounds::MinY() const { return miny; }
double Bounds::MaxX() const { return maxx; }
double Bounds::MaxY() const { return maxy; }

double Bounds::OffsetX(double x) const { return x - minx; }
double Bounds::OffsetY(double y) const { return y - miny; }

double Bounds::Width() const { return OffsetX(MaxX()); }
double Bounds::Height() const { return OffsetY(MaxY()); }

void Bounds::Union(const Bounds &other) {
  if (other.Empty()) return;
  Bound(other.MinX(), other.MinY());
  Bound(other.MinX(), other.MaxY());
  Bound(other.MaxX(), other.MinY());
  Bound(other.MaxX(), other.MaxY());
}

void Bounds::AddMargin(double d) {
  maxx += d;
  maxy += d;
  minx -= d;
  miny -= d;
}

void Bounds::AddMarginFrac(double f) {
  const double r = f * std::max(Width(), Height());
  AddMargin(r);
}

double Bounds::Scaler::ScaleX(double x) const {
  return (x + xoff) * xs;
}
double Bounds::Scaler::ScaleY(double y) const {
  return (y + yoff) * ys;
}
std::pair<double, double>
Bounds::Scaler::Scale(std::pair<double, double> p) const {
  return {ScaleX(p.first), ScaleY(p.second)};
}

Bounds::Scaler Bounds::Stretch(double neww, double newh) const {
  const double oldw = maxx - minx;
  const double oldh = maxy - miny;
  Scaler ret;
  ret.xoff = 0.0 - minx;
  ret.yoff = 0.0 - miny;
  ret.xs = neww / oldw;
  ret.ys = newh / oldh;
  ret.width = oldw;
  ret.height = oldh;
  return ret;
}

Bounds::Scaler Bounds::Scaler::FlipY() const {
  Scaler ret = *this;
  // screen_y = (yoff + y) * ys
  // but we want
  // flip_y = (yoff + (height - y)) * ys
  //        = ys * yoff + ys * height - ys * y
  //        = ys (yoff + height) - ys * y
  // so if we negate ys, then
  //        = - nys (yoff + height) + nys * y
  // and make yoff be -(yoff + height), then
  //        = nys * nyoff + nys * y
  //        = (nyoff + y) * nys
  ret.ys = -ys;
  ret.yoff = -(yoff + height);
  return ret;
}
