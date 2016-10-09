
#ifndef __EXTENT_H
#define __EXTENT_H

/* provides 'auto'-style deallocation
   for pointers with 'destroy' method. */

// TODO: Delete this class. Use std::unique_ptr and use destructors
// instead of destroy.

template <class P>
struct Extent {
  P *ptr;

  Extent(P *p) : ptr(p) {}

  void release() { ptr = 0; }

  void replace(P *p) { ptr = p; }

  ~Extent() { if (ptr) ptr->destroy(); }
};

#endif
