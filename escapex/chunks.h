
#ifndef _ESCAPE_CHUNKS_H
#define _ESCAPE_CHUNKS_H

#include <cstdint>
#include <string>
#include <optional>
#include <memory>
#include <vector>
#include <map>

#include "base.h"

/* associative chunk database. */

/* contents of database */
enum ChunkType : uint32 { CT_INVALID = 0xFFFFFFFF,
                          CT_INT32 = 0, CT_BOOL = 1, CT_STRING = 2, };
struct Chunk {
  ChunkType type = CT_INVALID;
  uint32_t key = 0;

  std::string ToString() const;
  static std::optional<Chunk> FromString(const std::string &s);
  Chunk() {}
  Chunk(uint32_t, int32_t);
  Chunk(uint32_t, bool);
  Chunk(uint32_t, std::string);
  virtual ~Chunk() {}

  /* only one will make sense, depending on type */
  int32_t i = 0;
  std::string s;
};

/* database itself */
struct Chunks {
  /* create a blank db with no chunks */
  static std::unique_ptr<Chunks> Create();

  /* revive marshalled chunks */
  static std::unique_ptr<Chunks> FromString(const std::string &s);

  /* marshall to string */
  virtual std::string ToString() const;

  /* returns nullptr if not present.
     Pointer is owned by Chunks and is invalidated by Insert,
     destructor, etc. */
  virtual const Chunk *Get(uint32_t key) const;

  /* replace existing chunk, if present. */
  virtual void Insert(const Chunk &data);

  virtual ~Chunks();

 private:
  Chunks() {}
  Chunks(const std::vector<Chunk> &data);

  // Map key is chunk's key.
  std::map<uint32_t, Chunk> data;
};


#endif
