
#ifndef __CHUNKS_H
#define __CHUNKS_H

#include "escapex.h"
#include "util.h"

/* associative chunk database. */

/* contents of database */
enum ChunkType : uint32 { CT_INT32, CT_BOOL, CT_STRING, };
struct Chunk {
  ChunkType type;
  uint32 key;

  string tostring();
  static Chunk *fromstring(const string &s);
  Chunk(uint32, int32);
  Chunk(uint32, bool);
  Chunk(uint32, string);
  virtual ~Chunk() {}

  /* only one will make sense, depending on type */
  int i;
  string s;
};

/* database itself */
struct Chunks {

  /* create a blank db with no chunks */
  static Chunks *create();

  /* revive marshalled chunks */
  static Chunks *fromstring(const string &s);

  /* marshall to string */
  virtual string tostring();

  /* returns 0 if not present */
  virtual Chunk *get(uint32 key);

  /* replace existing chunk, if present.
     takes ownership of chunk in any case */
  virtual void insert(Chunk *data);

  virtual ~Chunks() {}
  virtual void destroy();

  private:
  PtrList<Chunk> *data;
  static int compare(Chunk *l, Chunk *r);
};


#endif
