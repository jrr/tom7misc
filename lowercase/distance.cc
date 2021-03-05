
// THIS FILE ONLY:
// C++ port of BezierUtil.as from Degrafa. Follows the same
// MIT-like license as below with the additional (c) 2021 Tom Murphy VII.

// NOTE: This doesn't seem to work correctly! I likely made a
// superficial mistake porting it, or perhaps it depends more than I
// expect on the ControlPolygonFlatEnough call (which I left out since
// I thought it's just an optimization). But it eluded me. Instead I
// found a public domain C version of the same algorithm and switched
// to that.


////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2008 The Degrafa Team : http://www.Degrafa.com/team
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Programmed by: Jim Armstrong
//
// This software is derived from source containing the following
// copyright notice
//
// copyright (c) 2006-2007, Jim Armstrong.  All Rights Reserved.
//
// This software program is supplied 'as is' without any warranty,
// express, implied, or otherwise, including without limitation all
// warranties of merchantability or fitness for a particular purpose.
// Jim Armstrong shall not be liable for any special incidental, or
// consequential damages, including, without limitation, lost
// revenues, lost profits, or loss of prospective economic advantage,
// resulting from the use or misuse of this software program.
//
////////////////////////////////////////////////////////////////////////////////

static inline int GetLinearIndex(int n, int row, int col) {
  return row * n + col;
}

static std::array<std::pair<float, float>, 4>
ToBezierForm(float px, float py,
             array<pair<float, float>, 3> v) {
  // compute control points of the polynomial resulting from the inner
  // product of B(t)-P and B'(t), constructing the result as a Bezier
  // curve of order 2n-1, where n is the degree of B(t).

  // Degree of B(t)
  static constexpr int n = 2;
  static_assert(n == v.size() - 1);
  // degree of B(t) . P
  static constexpr int degree = 2 * n - 1;
  static_assert(degree == 3);

  // V(i) - P
  array<pair<float, float>, v.size()> c;
  for (int i = 0; i < v.size(); i++) {
    const auto [vx, vy] = v[i];
    c[i] = make_pair(vx - px, vy - py);
  }

  // var s:Number = Number(n);
  // V(i+1) - V(i)
  array<pair<float, float>, v.size() - 1> d;
  for (int i = 0; i < n; i++) {
    const auto [vsx, vsy] = v[i];
    const auto [vex, vey] = v[i + 1];
    d[i] = make_pair(n * (vex - vsx), n * (vey - vsy));
  }


  array<float, n * (n + 1)> cd;
  // inner product table
  for (int row = 0; row < n; row++) {
    const auto [dx, dy] = d[row];

    for (int col = 0; col < n + 1; col++) {
      const auto [cx, cy] = c[col];
      const int k = GetLinearIndex(n + 1, row, col);
      CHECK(k < cd.size());
      cd[k] = dx * cx + dy * cy;

      // this was just a bug I guess
      // k++;
    }
  }

  // Control points for Bezier curve whose zeros represent candidates
  // for closest point to the input parametric curve.
  array<pair<float, float>, degree + 1> w;
  constexpr float dinv = 1.0f / degree;
  for (int i = 0; i <= degree; i++) {
    w[i] = make_pair(i * dinv, 0.0f);
  }

  // Pre-computed coefficients for cubic bezier
  static constexpr array<float, 6> z = {
    1.0f, 2.0f/3.0f, 1.0f/3.0f, 1.0f/3.0f, 2.0f/3.0f, 1.0f
  };

  // Accumulate y-coords of the control points along the skew diagonal
  // of the (n-1) x n matrix of c.d and z values.
  constexpr int m = n - 1;
  for (int k = 0; k <= n+m; k++) {
    int lb = std::max(0, k - m);
    int ub = std::min(k, n);

    for (int i = lb; i <= ub; i++) {
      int j = k - i;
      int idx = GetLinearIndex(n + 1, j, i);
      CHECK(idx < cd.size());
      CHECK(idx < z.size());
      CHECK(i + j < w.size());
      w[i + j].second += cd[idx] * z[idx];
    }
  }

  return w;
}


// How many times does the Bezier curve cross the horizontal axis?
// The number of roots is less than or equal to this count.
static inline int CrossingCount(const std::vector<pair<float, float>> &v,
                                int degree) {
  int ncrossings = 0;

  bool oldsign = v[0].second < 0;
  for (int i = 1; i <= degree; i++) {
    CHECK(i < v.size());
    int sign = v[i].second < 0;
    if (sign != oldsign)
      ncrossings++;
    oldsign = sign;
  }

  return ncrossings;
}


static pair<
  vector<pair<float, float>>,
  vector<pair<float, float>>>
Subdivide(const vector<pair<float, float>> &c, float t) {
  const int degree = c.size() - 1;
  const int n = degree + 1;

  vector<pair<float, float>> p = c;
  const float t1 = 1.0 - t;

  if (false)
  printf("Subdivide([arr] len %d, %.5f.\n", (int)c.size(), t);

  for (int i = 1; i <= degree; i++) {
    for (int j = 0; j <= degree - i; j++) {
      const int ij = GetLinearIndex(n, i, j);
      const int im1j = GetLinearIndex(n, i - 1, j);
      const int im1jp1 = GetLinearIndex(n, i - 1, j + 1);
      CHECK(im1j < p.size()) << im1j << " vs " << p.size()
                             << " when i=" << i << " j=" << j
                             << " and n=" << n;
      CHECK(im1jp1 < p.size());
      const auto [pim1jx, pim1jy] = p[im1j];
      const auto [pim1jp1x, pim1jp1y] = p[im1jp1];
      // PERF!
      if (ij >= p.size()) p.resize(ij + 1);
      p[ij] = make_pair(
          t1 * pim1jx + t * pim1jp1x,
          t1 * pim1jy + t * pim1jp1y);
    }
  }

  vector<pair<float, float>> left(degree + 1);
  for (int j = 0; j<=degree; j++) {
    int index = GetLinearIndex(n, j, 0);
    left[j] = p[index];
  }

  vector<pair<float, float>> right(degree + 1);
  for(int j = 0; j<=degree; j++) {
    int index = GetLinearIndex(n, degree - j, j);
    right[j] = p[index];
  }

  return make_pair(left, right);
}

// Return roots in [0,1] of a polynomial in
// Bernstein-Bezier form.
static std::vector<float> FindRoots(
    // PERF
    const std::vector<pair<float, float>> &w,
    int degree,
    int depth) {

  if (false)
  printf("FindRoots([arr] len %d, %d, %d)\n",
         (int)w.size(),
         degree, depth);

  static constexpr int MAX_DEPTH = 64;
  int m = 2 * degree - 1;

  CHECK(depth < 70);

  switch (CrossingCount(w, degree)) {
  case 0:
    return {};
  case 1:
    // Unique solution. Stop recursion and return the midpoint
    // when we get deep enough.
    if (depth >= MAX_DEPTH) {
      return {0.5f * (w[0].first + w[m].first)};
    }

    // PERF check for linearity too!

    break;
  default:
    break;
  }

  // Otherwise, subdivide and recurse.
  const auto [left, right] = Subdivide(w, 0.5f);
  CHECK(left.size() == right.size());
  CHECK(left.size() == w.size());

  // t-values of roots
  vector<float> out;

  for (const float t : FindRoots(left, degree, depth + 1))
    out.push_back(t);
  for (const float t : FindRoots(right, degree, depth + 1))
    out.push_back(t);

  return out;
}


static std::tuple<float, float, float>
DistanceFromPointToBezier(
    // The point to test
    float px, float py,
    // Bezier start point
    float sx, float sy,
    // Bezier ontrol point
    float cx, float cy,
    // Bezier end point
    float ex, float ey) {

  auto Bezier = [sx, sy, cx, cy, ex, ey](float t) ->
    std::pair<float, float> {
    float tt = t * t;
    // Get point on curve at t:
    float omt = 1.0 - t;
    float omtt = omt * omt;
    float bx = omtt * sx  +  2.0 * omt * t * cx  +  tt * ex;
    float by = omtt * sy  +  2.0 * omt * t * cy  +  tt * ey;
    return make_pair(bx, by);
  };

  auto Dist = [](float x1, float y1, float x2, float y2) {
      float dx = x2 - x1;
      float dy = y2 - y1;
      return sqrtf(dx * dx + dy * dy);
    };

  // record distances from point to endpoints
  // var p:Point       = _curve.pointAt(0);
  const auto [p0x, p0y] = Bezier(0.0f);
  const float d0 = Dist(p0x, p0y, px, py);

  const auto [p1x, p1y] = Bezier(1.0f);
  const float d1 = Dist(p1x, p1y, px, py);

  constexpr int n = 2;

  std::array<std::pair<float, float>, 3> v = {
    make_pair(sx, sy), make_pair(cx, cy), make_pair(ex, ey),
  };

  // instead of power form, convert the function whose zeros are
  // required to Bezier form
  std::array<pair<float, float>, 4> w = ToBezierForm(px, py, v);

  std::vector<pair<float, float>> ww;
  ww.reserve(4);
  for (const auto &v : w) ww.push_back(v);

  // Find roots of the Bezier curve with control points stored in 'w'
  // (algorithm is recursive, this is root depth of 0)
  vector<float> roots = FindRoots(ww, 2 * n - 1, 0);

  // PERF could be comparing squared distances
  float best = 0.0f;
  float bestx = 0.0f, besty = 0.0f;
  if (d0 < d1) {
    best = d0;
    bestx = p0x;
    besty = p0y;
  } else {
    best = d1;
    bestx = p1x;
    besty = p1y;
  }

  for (float t : roots) {
    // Only consider roots on the curve, and we already
    // looked at the end points.
    if (t > 0.0f && t < 1.0f) {
      const auto [rx, ry] = Bezier(t);
      const float d = Dist(rx, ry, px, py);
      if (d < best) {
        best = d;
        bestx = rx;
        besty = ry;
      }
    }
  }

  return make_tuple(bestx, besty, best);
}
