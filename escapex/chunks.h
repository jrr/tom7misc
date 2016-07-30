
#ifndef __CHUNKS_H
#define __CHUNKS_H

#include "escapex.h"
#include "util.h"

/* associative chunk database. */

/* contents of database */
enum chunktype : uint32 { CT_INT32, CT_BOOL, CT_STRING, };
struct chunk {
  chunktype type;
  uint32 key;

  string tostring();
  static chunk * fromstring(string);
  chunk(uint32, int32);
  chunk(uint32, bool);
  chunk(uint32, string);
  virtual ~chunk() {}

  /* only one will make sense, depending on type */
  int i;
  string s;
};

/* database itself */
struct chunks {

  /* create a blank db with no chunks */
  static chunks * create();

  /* revive marshalled chunks */
  static chunks * fromstring(string s);

  /* marshall to string */
  virtual string tostring();

  /* returns 0 if not present */
  virtual chunk * get(uint key);

  /* replace existing chunk, if present.
     takes ownership of chunk in any case */
  virtual void insert(chunk * data);

  virtual ~chunks() {}
  virtual void destroy();

  private:
  ptrlist<chunk> * data;
  static int compare(chunk * l, chunk * r);
};


#endif
