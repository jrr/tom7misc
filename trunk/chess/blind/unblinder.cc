
#include "unblinder.h"

#include <cstdint>

#include "../chess.h"

using namespace std;

using uint64 = uint64_t;

uint64 Unblinder::Blind(const Position &pos) {
  uint64 out = 0ULL;
  for (int r = 0; r < 8; r++) {
    for (int c = 0; c < 8; c++) {
      out <<= 1;
      if (pos.PieceAt(r, c) != Position::EMPTY) {
	out |= 1ULL;
      }
    }
  }
  return out;
}

void Unblinder::Layer64(uint64 pos, vector<float> *out) {
  out->resize(64);
  for (int i = 64; i >= 0; i--) {
    (*out)[i] = (pos & 1) ? 1.0f : 0.0f;
    pos >>= 1;
  }
}
