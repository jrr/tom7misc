
#ifndef _LOWERCASE_TTFOPS_H
#define _LOWERCASE_TTFOPS_H

#include <optional>
#include <tuple>

#include "ttf.h"

struct TTFOps {

  // If the uppercase and lowercase characters have the same number
  // of points with the same structure (line/bezier sequence),
  // return the (xscale, yscale, score) that minimizes the error.
  // Translation for each character is not returned.
  // Caller should do some thresholding on score, since we produce
  // a result whenever an alignment is possible, even if it is bad.
  static std::optional<std::tuple<double, double, double>>
  GetSameCase(const TTF &ttf);
  
};


#endif
