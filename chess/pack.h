
// Packs any potentially legal moves (as a pair of squares) into 1792
// consecutive indices, and the inverse.

#ifndef _PACK_H
#define _PACK_H

#include <tuple>

static constexpr int PACK_SIZE = 1792;

int Pack(int sr, int sc, int dr, int dc);
std::tuple<int, int, int, int> Unpack(int packed_idx);
void Unpack(int packed_idx, int *sr, int *sc, int *dr, int *dc);

#endif
