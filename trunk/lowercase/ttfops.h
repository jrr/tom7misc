
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
  //
  // This requires the characters to be structurally equivalent.
  // BitmapDifference is a slower but more robust check for the
  // *appearance* being equivalent.
  static std::optional<std::tuple<double, double, double>>
  GetSameCase(const TTF &ttf);

  // Render the two characters to bitmaps at the given scale, and then
  // return the difference as the fraction of pixels in the second
  // bitmap that are the same in the first (will be in [0, 1]).
  static double CharBitmapDifference(const TTF &ttf,
                                     int c1, int c2,
                                     // Determines the base bitmap size for both
                                     // characters. c1 is unstretched.
                                     float scale,
                                     // Additional scale for c2, which can stretch it.
                                     // (we use scale * xscale2, scale * yscale2)
                                     float xscale2, float yscale2,
                                     // Offsets for c2. Maybe depends on scale?
                                     float xmov2, float ymov2);

  // Returns the best CharBitmapDifference for each letter A-Z vs a-z.
  // Maximum return value is 26, if every letter disagrees on every pixel.
  // Passing in a threshold returns early once we know we'll exceed that threshold,
  // saving time.
  // Increasing bitmap_scale and iters_per_char increase accuracy, but also cost.
  // Defaults seem to be fine.
  static double TotalAlphabetDifference(const TTF &ttf,
                                        float bitmap_scale = 200.0f,
                                        int iters_per_char = 1000,
                                        float threshold = 26.0);
};


#endif
