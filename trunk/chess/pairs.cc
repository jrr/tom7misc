
// Simple ditty to enumerate the pairs of square that can possibly
// have legal moves between them. The answer is 1792, which is about
// 43% of the dense 64^2.
// Used to generate pack.cc.

#include <cmath>
#include <cstdio>
#include <vector>

// #include "chess.h"

using namespace std;

bool CouldMove(int sr, int sc, int dr, int dc) {
  // No self-moves.
  if (sr == dr && sc == dc)
    return false;

  // Horizontal and vertical ok.
  if (sr == dr || sc == dc)
    return true;

  // Diagonal ok
  if (std::abs(dr - sr) == std::abs(dc - sc))
    return true;

  // Within 2 squares. This covers king (1 square, and
  // castling is represented as moving two spaces),
  // pawn (double moves, captures, en passant) and knight
  // moves. This does not overcount, because within this
  // box, queen and knight cover the entire thing (in fact
  // with no overlap!)
  return std::abs(dr - sr) <= 2 && std::abs(dc - sc) <= 2;
}

static void PrintVec(const vector<int> &v, int w) {
  int row_left = w;
  printf("  ");
  for (int i : v) {
    printf("%d,", i);
    row_left--;
    if (row_left == 0) {
      printf("\n  ");
      row_left = w;
    } else {
      printf(" ");
    }
  }
}

int main (int argc, char **argv) {
  int count = 0;
  // index is sr * 512 + sc * 64 + dr * 8 + dc
  vector<int> pack;
  vector<int> unpack;

  for (int sr = 0; sr < 8; sr++) {
    for (int sc = 0; sc < 8; sc++) {
      for (int dr = 0; dr < 8; dr++) {
        for (int dc = 0; dc < 8; dc++) {
          if (CouldMove(sr, sc, dr, dc)) {
            pack.push_back(count);
            // unpack.emplace_back(sr, sc, dr, dc);
            unpack.push_back(sr * 512 + sc * 64 + dr * 8 + dc);
            count++;
          } else {
            pack.push_back(-1);
          }
        }
      }
    }
  }

  printf("constexpr int16 pack_table[4096] = {\n");
  PrintVec(pack, 8);
  printf("};\n\n"
         "constexpr int16 unpack_table[%d] = {\n",
         count);
  PrintVec(unpack, 12);
  printf("};\n\n");

  // fprintf(stderr, "Total: %d\n", count);
  return 0;
}
