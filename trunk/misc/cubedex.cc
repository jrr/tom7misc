
#include <array>
#include <functional>
#include <stdio.h>

using namespace std;

static constexpr int EMPTY = 0;
static constexpr int TRI = 1;
static constexpr int SQUARE = 2;
static constexpr int CIRC = 3;

static constexpr array<int, 4> WEIGHTS = { 0, 2, 3, 6, };

static constexpr array<int, 4> NUM = { 1, 4, 3, 1, };

static inline bool Okay(const array<int, 9> &board) {
  const auto [a, b, c,
	      d, _, f,
	      g, h, i] = board;

  return (a + d + g) == (c + f + i) &&
    (a + b + c) == (g + h + i) &&
    true;
    /*
    (a + b + d) == (f + h + i) &&
    (d + g + h) == (b + c + f); */
}

int main(int argc, char **argv) {

  std::function<void(array<int, 4> &,
		     array<int, 9> &,
		     int)> Rec = [&Rec](array<int, 4> &left,
					array<int, 9> &board,
					int idx) {
     if (idx == board.size()) {
	if (Okay(board)) {
	  const auto [a, b, c,
		      d, e, f,
		      g, h, i] = board;
	  printf("Yes:\n"
		 "%d %d %d\n"
		 "%d %d %d\n"
		 "%d %d %d\n",
		 a, b, c,
		 d, e, f,
		 g, h, i);
	  exit(0);
	}
      } else {
	for (int i = 0; i < 4; i++) {
	  if (left[i] > 0) {
	    left[i]--;
	    board[idx] = WEIGHTS[i];
	    Rec(left, board, idx + 1);
	    // No need to clean up board; we will overwrite.
	    left[i]++;
	  }
	}
      }
    };
  
  array<int, 4> left = NUM;
  array<int, 9> board = {0, 0, 0,
			 0, 0, 0,
			 0, 0, 0};
  Rec(left, board, 0);
  printf("Not found..?\n");
  return -1;
}
