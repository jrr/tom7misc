#include "common.h"

#include <cstdint>

#include "base/logging.h"
#include "packedgame.h"

using namespace std;

using uint64 = uint64_t;
using uint32 = uint32_t;
using uint16 = uint16_t;
using uint8 =  uint8_t;

static bool ShouldNormalize(const CommonMap::MoveCounts &mc) {
  for (const auto &c : mc)
    if (c.second >= (1 << 20))
      return true;
  return false;
}

static bool AllEqual(const CommonMap::MoveCounts &mc) {
  if (mc.empty()) return true;

  const uint32 count = mc.begin()->second;
  
  for (const auto &c : mc)
    if (c.second != count)
      return false;

  return true;
}

CommonMap::CommonMap(const string &filename) {
  vector<uint8> bytes = Util::ReadFileBytes(filename);
  CHECK(!bytes.empty()) << filename;

  int idx = 0;
  auto GetByte = [&idx, &bytes]() -> uint8 {
      CHECK(idx < bytes.size()) << idx << " over " << bytes.size();
      uint8 b = bytes[idx++];
      return b;
    };
  auto Get16 = [&GetByte]() -> uint16 {
      uint16 ret = GetByte();
      ret <<= 8;
      ret |= GetByte();
      return ret;
    };
  auto Get32 = [&GetByte]() -> uint32 {
      uint32 ret = 0LL;
      for (int i = 0; i < 4; i++) {
	ret <<= 8;
	ret |= GetByte();
      }
      return ret;
    };
  auto Get64 = [&GetByte]() -> uint64 {
      uint64 ret = 0LL;
      for (int i = 0; i < 8; i++) {
	ret <<= 8;
	ret |= GetByte();
      }
      return ret;
    };

  while (idx < bytes.size()) {
    uint64 ph = Get64();
    CHECK(positions.find(ph) == positions.end()) << ph;
    MoveCounts *mc = &positions[ph];
    const uint16 num = Get16();
    if (num & (1 << 15)) {
      // Flat.
      for (int i = 0; i < (num & ~(1 << 15)); i++) {
	uint16 pm = Get16();
	CHECK(mc->find(pm) == mc->end()) << idx << ", " << pm;
	(*mc)[pm] = 1;
      }
    } else {
      for (int i = 0; i < num; i++) {
	uint32 row = Get32();
	uint16 pm = row >> 20;
	uint32 count = row & ((1 << 20) - 1);
	CHECK(mc->find(pm) == mc->end()) << idx << ", " << pm;
	(*mc)[pm] = count;
      }
    }
  }
}
    

// File format is as follows.
// 64-bit hash describes the position,
// then 16 bits gives the number of nonzero moves in that position.
// Each move record is 32 bits. The top 12 bits are the packed move
// (PackedGame::UnpackMove) id, and the lower 20 bits are the count.
// (Move counts are normalized so that it always fits in 20 bits).
void CommonMap::WriteFile(const string &filename) {
  FILE *f = fopen(filename.c_str(), "wb");
  CHECK(f) << filename;
  
  auto Write64 = [&](uint64 w) {
      uint8 hbytes[8];
      for (int i = 0; i < 8; i++) {
	hbytes[i] = (w >> (56 - (i * 8))) & 0xFF;
      }
      fwrite(&hbytes[0], 8, 1, f);
    };

  auto Write32 = [&](uint32 w) {
      uint8 hbytes[8];
      for (int i = 0; i < 4; i++) {
	hbytes[i] = (w >> (24 - (i * 8))) & 0xFF;
      }
      fwrite(&hbytes[0], 4, 1, f);
    };

  auto Write16 = [&](uint16 w) {
      uint8 hbytes[2];
      hbytes[0] = (w >> 8) & 0xFF;
      hbytes[1] = w & 0xFF;
      fwrite(&hbytes[0], 2, 1, f);
    };

  int normalized = 0, all_eq = 0;
  for (const auto &p : positions) {
    Write64(p.first);
    const MoveCounts &mc = p.second;

    if (ShouldNormalize(mc)) {
      Write16(mc.size());
      normalized++;
      double total_mass = 0.0;
      for (const auto &c : mc)
	total_mass += c.second;
      
      for (const auto &c : mc) {
	const uint32 packed_move = c.first;
	double f = (double)c.second / total_mass;
	uint32 norm_count = f * ((1 << 20) - 1);
	// Don't allow counts of zero, and don't filter them because
	// we already wrote the size.
	if (norm_count == 0) norm_count++;
	CHECK(norm_count < (1 << 20)) << norm_count;
	const uint32 row = (packed_move << 20) | norm_count;
	Write32(row);
      }
      
    } else if (AllEqual(mc)) {
      // If counts are all equal (typical because it's sparse and
      // the counts are exactly 1) then don't store counts. Just
      // write packed moves as 16 bit words. (PERF: Could even
      // use PackedGame here..)
      all_eq++;
      Write16((1 << 15) | mc.size());

      for (const auto &c : mc) {
	const uint16 packed_move = c.first;
	Write16(packed_move);
      }
      
    } else {
      Write16(mc.size());
      
      for (const auto &c : mc) {
	const uint32 packed_move = c.first;
	CHECK(c.second < (1 << 20)) << c.second;
	const uint32 row = (packed_move << 20) | c.second;
	Write32(row);
      }
    }
  }

  fclose(f);
  fprintf(stderr, "Wrote to %s, %d normalized, %d eq\n",
	  filename.c_str(), normalized, all_eq);
}
