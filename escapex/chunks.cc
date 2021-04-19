
#include "chunks.h"

#include <cstdint>
#include <string>
#include <optional>
#include <memory>
#include <vector>
#include <map>

#include "base.h"
#include "escape-util.h"
#include "bytes.h"

using int32 = int32_t;
using uint32 = uint32_t;

Chunk::Chunk(uint32 k, int32 ii) : type(CT_INT32), key(k), i(ii) {}
Chunk::Chunk(uint32 k, bool bb) : type(CT_BOOL), key(k), i(bb) {}
Chunk::Chunk(uint32 k, string ss) : type(CT_STRING), key(k), i(0), s(ss) {}

string Chunk::ToString() const {
  /* key and type first, then data.
     the size of string data must be deduced from some other
     source. */
  // printf("tostring(%p): key %d, type %d, i %d, b %d, s %");

  string res = BigEndian32((uint32)key) + BigEndian32((uint32)type);
  switch (type) {
  case CT_BOOL:
  case CT_INT32: return res + BigEndian32(i);
  case CT_STRING: return res + s;
  default: break;
  }
  abort();
}

std::optional<Chunk> Chunk::FromString(const string &s) {
  /* no type field */
  if (s.length() < 8) return {};
  uint32 idx = 0;
  uint32 key = (unsigned int)ReadBigEndian32(s, idx);
  ChunkType ty = (ChunkType)ReadBigEndian32(s, idx);

  switch (ty) {
  case CT_INVALID:
    return {};
  case CT_BOOL:
    if (s.length() < 12) return {};
    else return {Chunk(key, (bool)ReadBigEndian32(s, idx))};
  case CT_INT32:
    if (s.length() < 12) return {};
    else return {Chunk(key, (int)ReadBigEndian32(s, idx))};
  case CT_STRING:
    return {Chunk(key, (string)s.substr(8, s.length() - 8))};
  default:
    return {};
  }
}

std::unique_ptr<Chunks> Chunks::Create() {
  return std::unique_ptr<Chunks>(new Chunks);
}

Chunks::~Chunks() {}

const Chunk *Chunks::Get(uint32 k) const {
  auto it = data.find(k);
  if (it == data.end())
    return nullptr;

  return &it->second;
}

string Chunks::ToString() const {
  string op;
  for (const auto &[key_, chunk] : data) {
    const string c = chunk.ToString();
    op += BigEndian32(c.length());
    op += c;
  }

  return op;
}

/* exhausts the input string */
std::unique_ptr<Chunks> Chunks::FromString(const string &s) {
  uint32 idx = 0;
  vector<Chunk> dat;

  while (idx < s.length()) {
    int len = ReadBigEndian32(s, idx);

    if (idx + len > s.length()) {
      printf("bad length\n");
      return nullptr;
    }
    string ch = s.substr(idx, len); idx += len;

    if (const std::optional<Chunk> c = Chunk::FromString(ch)) {
      dat.push_back(c.value());
    } else {
      return nullptr;
    }
  }

  return std::unique_ptr<Chunks>(new Chunks(dat));
}

Chunks::Chunks(const std::vector<Chunk> &data_vec) {
  for (const Chunk &chunk : data_vec) {
    data[chunk.key] = chunk;
  }
}

void Chunks::Insert(const Chunk &chunk) {
  data[chunk.key] = chunk;
}
