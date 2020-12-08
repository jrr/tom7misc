
#include "ttfops.h"

#include <vector>
#include <utility>
#include <optional>
#include <tuple>
#include <cmath>

#include "opt/opt.h"

using namespace std;

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
