
#ifndef __EXTENT_H
#define __EXTENT_H

/* provides 'auto'-style deallocation
   for pointers with 'destroy' method. */

// TODO: Delete this class. Use std::unique ptr and use destructors
// instead of destroy.

template <class P>
struct Extent {
  
  P * ptr;

  Extent(P * p) : ptr(p) {}

  void release() { ptr = 0; }

  void replace(P * p) { ptr = p; }

  ~Extent() { if (ptr) ptr->destroy(); }

};

/* for destructor */
template <class P>
struct Extentd {
  
  P * ptr;

  Extentd(P * p) : ptr(p) {}

  void release() { ptr = 0; }

  void replace(P * p) { ptr = p; }

  ~Extentd() { if (ptr) delete ptr; }

};

/* for destructor, array of */
template <class P>
struct Extentda {
  
  P * ptr;

  Extentda(P * p) : ptr(p) {}

  void release() { ptr = 0; }

  void replace(P * p) { ptr = p; }

  ~Extentda() { if (ptr) delete [] ptr; }

};

/* for call to 'free' */
template <class P>
struct Extentf {
  P * ptr;
  Extentf(P * p) : ptr(p) {}
  void release() { ptr = 0; }
  void replace(P * p) { ptr = p; }
  ~Extentf() { if (ptr) free(ptr); }
};

#endif
