
#ifndef __UNBLINDER_MK0_H
#define __UNBLINDER_MK0_H

#include <string>

#include "unblinder.h"

struct UnblinderMk0 {
  static Unblinder *LoadFromFile(const std::string &filename);

  UnblinderMk0() = delete;
};

#endif
