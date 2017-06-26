
#ifndef __TOM7_STAMP_H
#define __TOM7_STAMP_H

typedef int stamp;

inline stamp new_stamp () {
  static int ctr;
  return ++ctr;
}

inline int stamp_map_cmp (void * k1, void * k2) {
  return *(int*)k1 - *(int*)k2;
}

#endif
