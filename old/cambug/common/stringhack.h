/* Copyright 2000 Tom 7: See COPYING for distribution license */

#ifndef __TOM7_STRINGHACK_H
#define __TOM7_STRINGHACK_H

#include <string>
using namespace std;

/* sorry */
inline const char * operator-(const string & rhs) { return rhs.c_str(); }

string stringf (const char * format, ...);
/* __attribute__((format(printf,1,2))); */

inline bool startswith(string big, string little) {
  return (big.find(little) == 0);
}

inline string restfrom(string s, int i) {
  return s.substr(i, s.length() - i);
}

#endif
