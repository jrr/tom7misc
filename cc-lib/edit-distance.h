
#ifndef __CCLIB_EDIT_DISTANCE_H
#define __CCLIB_EDIT_DISTANCE_H

#include <string>

struct EditDistance {

  // For the case where substitutions, insertions, and deletions are
  // all cost 1.
  static int Distance(const std::string &s1, const std::string &s2);

  // TODO: Parameterized versions.

 private:
  EditDistance() = delete;
};

#endif
