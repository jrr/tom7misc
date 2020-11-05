
#ifndef _CC_LIB_EDIT_DISTANCE_H
#define _CC_LIB_EDIT_DISTANCE_H

#include <string>

struct EditDistance {

  // For the case where substitutions, insertions, and deletions are
  // all cost 1.
  static int Distance(const std::string &s1, const std::string &s2);

  // Same, but returns min(Distance(s1, s2), threshold). This is much
  // faster for small thresholds.
  static int Ukkonen(const std::string &s1, const std::string &s2,
		     int threshold);
  
  // TODO: Parameterized versions.

 private:
  EditDistance() = delete;
  EditDistance(const EditDistance &) = delete;
};

#endif
