\
#ifndef __TOM7_MAP_H
#define __TOM7_MAP_H

#include <stdlib.h>
#include <string.h>

#include "common/stringhack.h"

/* abstract templated maps with key K and item I
   
   pass in comparison function cmp 

   k1 <  k2             --> -1 (or anything negative)
   k1 == k2             -->  0
   k1  > k2             -->  1 (or anything positive)
*/

inline int scomp(void * l, void * r) {
  return strcmp(-*(string*)l,
		-*(string*)r);
}

inline int ccomp(void * l, void * r) {
  return strcmp(*(char**)l,
		*(char**)r);
}

/* FIXME: subtract instead */
inline int icomp(void * l, void * r) {
  if (*(int*)l > *(int*)r) return 1;
  else if (*(int*)l < *(int*)r) return -1;
  else return 0;
}

/* standard pointer compare */
inline int pcomp(void * l, void * r) {
  return (*(int**)l - *(int**)r);
}

#define tmpl <class K, class I, int (*C)(void * k1, void * k2)>

template tmpl
class map {
 public:
  /* empty map */
  map () {};

  /* default destructor on K and I,
     but better app a destructor
     if I is a pointer. */
  ~map () {};

  /* singleton */
  map (K key, I val) {};

  /* insert into this map (or update previous key) */
  virtual void insert (K key, I val) = 0;

  /* find item by key, true when found, result in out */
	  virtual bool find (K key, I & out) = 0;

  /* in-order application of f */
  virtual void app ( void (*f) (K k, I i, void * v), void * v) = 0;

  /* destructive remove of matching key,
     ret true if something removed, false otherwise */
  virtual bool remove ( K key ) = 0;

};

/* system map creation functions,
   should use the best map available */
/*
  template tmpl
  map<K,I,C> * newmap(); 
  
  template tmpl
  map<K,I,C> * newmap(K k, I i);
*/

#define BESTMAP binmap

#undef tmpl

#endif
