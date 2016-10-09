
#ifndef __CHUNKS_H
#define __CHUNKS_H

#include "base.h"
#include "ptrlist.h"

/* associative chunk database. */

/* contents of database */
enum ChunkType : uint32 { CT_INT32, CT_BOOL, CT_STRING, };
struct Chunk {
  ChunkType type;
  uint32 key;

  string ToString();
  static Chunk *FromString(const string &s);
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
  static std::unique_ptr<Chunks> Create();

  /* revive marshalled chunks */
  static std::unique_ptr<Chunks> FromString(const string &s);

  /* marshall to string */
  virtual string ToString();

  /* returns 0 if not present */
  virtual Chunk *Get(uint32 key);

  /* replace existing chunk, if present.
     takes ownership of chunk in any case */
  virtual void Insert(Chunk *data);

  virtual ~Chunks();

 private:
  PtrList<Chunk> *data = nullptr;
  static int Compare(Chunk *l, Chunk *r);
};


#endif
