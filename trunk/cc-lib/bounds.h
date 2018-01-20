
#ifndef __CCLIB_BOUNDS_H
#define __CCLIB_BOUNDS_H

#include <limits>
#include <utility>

/* Imperative 2D bounding box, after sml-lib. */
struct Bounds {
  // With no points.
  Bounds();

  // Expand the bounding box to contain the point.
  void Bound(double x, double y);

  // Returns true if no points have been added. When the bounding box
  // is empty, several functions below should not be called.
  bool Empty() const;

  double MinX() const;
  double MinY() const;
  double MaxX() const;
  double MaxY() const;

  // The offset of the input point within the bounding box (thinking
  // of MinX/MinY as 0,0).
  double OffsetX(double x) const;
  double OffsetY(double y) const;

  double Width() const;
  double Height() const;

  // Modifies 'this'. 'this' and 'other' may be empty.
  void Union(const Bounds &other);

  // Adds a fixed-sized margin around the entire bounds, in absolute units.
  // Must be non-empty. d must be non-negative.
  void AddMargin(double d);

  // Add a margin that's a fraction of the longest dimension. If
  // empty, does nothing. f must be non-negative.
  void AddMarginFrac(double f);

  // A common thing to do is collect some points into a bounding box,
  // which we then want to represent as a graphic of a different
  // scale and origin, like a 1000x1000 pixel box whose bottom left is
  // 0,0 (call these "screen coordinates").
  // 
  // This type is a transformation conveniently derived from
  // the bounds and desired screen coordinates. (It's called a scaler
  // but it also involves at least a translation. TODO: Allow flipping
  // the coordinate system vertically, too.) The scaler is immutable,
  // even if the bounds it was derived from is modified.
  struct Scaler {
    // Maps a point from the original coordinates (i.e., what was inserted
    // into bounds) to screen coordinates.
    double ScaleX(double x) const;
    double ScaleY(double y) const;
    std::pair<double, double> Scale(std::pair<double, double> p) const;
  private:
    friend class Bounds;
    double xoff = 0.0, yoff = 0.0;
    double xs = 1.0, ys = 1.0;
  };

  // Make the bounding box as large as possible without modifying its
  // aspect ratio.
  // TODO: Scaler ScaleToFit(double w, double h) const;
  // Make the bounding box fit the screen, stretching as necessary.
  Scaler Stretch(double w, double h) const;

private:
  bool is_empty = true;
  double minx = std::numeric_limits<double>::infinity();
  double miny = std::numeric_limits<double>::infinity();
  double maxx = -std::numeric_limits<double>::infinity();
  double maxy = -std::numeric_limits<double>::infinity();
};

#endif
