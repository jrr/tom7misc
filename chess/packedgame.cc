#include "packedgame.h"

#include "chess.h"

using uint8 = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;
using namespace std;

vector<uint8> PackedGame::Serialize() const {
  // PERF could avoid copying by storing it this way in the
  // first place...
  // Serialized format is result byte, 32-bits for num moves, then
  // packed move vector.
  CHECK(num_moves < 0x7FFFFFFE) << num_moves;
  vector<uint8> ret;
  ret.reserve(1 + 4 + packed_moves.size());
  ret.push_back(ResultByte(result));
  ret.push_back((num_moves >> 24) & 0xFF);
  ret.push_back((num_moves >> 16) & 0xFF);
  ret.push_back((num_moves >> 8) & 0xFF);
  ret.push_back(num_moves & 0xFF);
  ret.insert(ret.end(), packed_moves.begin(), packed_moves.end());
  return ret;
}

uint64_t PackedGame::HashCode() const {
  // PERF: We can do this without constructing the serialized
  // vector, though it's not clear it'd actually be faster.
  const vector<uint8_t> bytes = Serialize();
  const vector<uint8_t> v = SHA256::HashVector(bytes);
  return ((uint64_t)v[0] << 56) |
    ((uint64_t)v[1] << 48) |
    ((uint64_t)v[2] << 40) |
    ((uint64_t)v[3] << 32) |
    ((uint64_t)v[4] << 24) |
    ((uint64_t)v[5] << 16) |
    ((uint64_t)v[6] << 8)  |
    (uint64_t)v[7];
}

vector<pair<uint64_t, PackedGame>>
PackedGame::SplitFile(const vector<uint8_t> &contents) {
  int idx = 0;
  auto GetByte = [&idx, &contents]() {
      CHECK(idx < contents.size()) << idx << " over " << contents.size();
      uint8 b = contents[idx++];
      /*
      fprintf(stderr, "[idx %d] = %02x\n", idx - 1, b);
      fflush(stderr);
      */
      return b;
    };
  auto Get32 = [&GetByte]() {
      uint32 ret = 0LL;
      for (int i = 0; i < 4; i++) {
	ret <<= 8;
	ret |= GetByte();
      }
      return ret;
    };
  auto Get64 = [&GetByte]() {
      uint64 ret = 0LL;
      for (int i = 0; i < 8; i++) {
	ret <<= 8;
	ret |= GetByte();
      }
      return ret;
    };

  vector<pair<uint64_t, PackedGame>> ret;
  
  while (idx < contents.size()) {
    const uint64 hc = Get64();
    const uint8 result_byte = GetByte();
    const uint32 num_moves = Get32();
    // Number of packed bytes expected.
    const int num_triples = (num_moves >> 1);
    // If odd (incomplete), we have 2 of the three bytes in the triple.
    const int packed_size = num_triples * 3 + ((num_moves & 1) ? 2 : 0);

    /*
    fprintf(stderr, "hc %llx, %d moves, %d triples, %d packed [idx %d]\n",
	    hc, num_moves, num_triples, packed_size, idx);
    */
    
    PackedGame pg;
    switch (result_byte) {
    case 0b10: pg.result = Result::WHITE_WINS; break;
    case 0b01: pg.result = Result::BLACK_WINS; break;
    case 0b00: pg.result = Result::DRAW; break;
    default: LOG(FATAL) << "Bad result byte " << result_byte;
    }
    pg.num_moves = num_moves;
    pg.packed_moves.resize(packed_size);
    CHECK(idx + packed_size <= contents.size()) << (idx + packed_size) <<
      " vs " << contents.size();
    memcpy(&pg.packed_moves[0], &contents[idx], packed_size);
    idx += packed_size;
    ret.emplace_back(hc, std::move(pg));
  }
  return ret;
}
