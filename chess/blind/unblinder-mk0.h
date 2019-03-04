
#ifndef __UNBLINDER_MK0_H
#define __UNBLINDER_MK0_H

#include <string>

#include "unblinder.h"

struct UnblinderMk0 {
  // If single_king is true, we force there to be exactly one king per
  // side by first using the overall greatest king score to give its
  // location, and then excluding it from later predictions.
  static Unblinder *LoadFromFile(const std::string &filename);

  UnblinderMk0() = delete;
};

#endif
