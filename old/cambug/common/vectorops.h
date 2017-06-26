
/* defines a bunch of useful operations on vectors. 
   most of these deserve to be rewritten for a better-designed
   vector class!
*/

#ifndef __TOM7_VECTOROPS_H
#define __TOM7_VECTOROPS_H

#include "common/conlog.h"

extern conlog * log;

/* #include <windows.h>*/

/* destructive vector append */
template <class T>
inline vector<T> operator+=(vector<T> & lhs, const vector<T> & rhs) {
  int s = lhs.size();
  int r = rhs.size();
  lhs.resize(s + r);
  for(int i=0;i < r; i++) {
    lhs[s+i] = rhs[i];
  }
  return lhs;
}

/* grab a subvector of a vector 
   e is the NUMBER OF BYTES
 */
template <class T>
inline vector<T> vsub(vector<T> & in, int s, int e) {

  log->logf("vsub s=%d e=%d size=%d\r\n", s, e, in.size());
#if 0
  if () {

    log->logf("Assertion failed, s > e || s > size || e > size! \r\n");
    /* XXX */ 
    vector<T> oo(0);
    log->logf("vsub out (1).\r\n");
    return oo; 
  }
#endif
  vector<T> out;
  out.resize(e);
  /*  out.reserve(e); */
  for(int i=0;i < e; i++) {
    out[i] = in[i+s];
  }

  log->logf("vsub out (2).\r\n");
  return out;
}

/* XXX: this is bad, bad! (bounded) leak alert. This should be in the
   class and deleted when the object dies. But that would require
   rewriting vector...

   this returns a flat array of each element in the vector. The
   lifetime of this array is until the next call to the same instance
   of this function. I use this in sock.cpp to ::send the array.
*/
template <class T>
T * varray(vector<T> & msg) {
  static T * carray = 0;

  if (carray) free(carray); /* free it from last time */
  
  carray = (T*)malloc(msg.size ());

  for (int i = 0; i < msg.size () ; i ++) {
    carray[i] = msg[i];
  }
  return carray;
}

/* create a vector of unsigned chars (ucharv)
   from a string */
inline vector<unsigned char> stov(string in) {
  vector<unsigned char> v(in.length());

  for(unsigned int i = 0; i < in.length(); i++) {
    v[i] = in[i];
  }
  return v;
}

/* go back to a string. This will terminate if it sees 0. */
inline string vtos(vector<unsigned char> in) {
  string out;

  for(unsigned int i = 0; i < in.size(); i++) {
    if (in[i]) out += in[i];
    else return out;
  }
  return out;
}

template <class T>
inline vector<T> mkvec(T * c, int size) {
  vector<T> d(size);
  for(int i=0;i<size;i++) {
    d[i] = c[i];
  }
  return d;
}


#endif
