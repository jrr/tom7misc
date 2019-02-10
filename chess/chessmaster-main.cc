
#include "chessmaster.h"

#include <string>

#include "../cc-lib/base/logging.h"

#include "chess.h"


using namespace std;

void TestWhiteCheckmate() {
  Chessmaster master(1);

  Position pos;
  // White's only move is to mate black (Qg2).
  Position::ParseFEN("8/8/6Q1/8/7p/7k/4n1b1/7K w - - 2 9", &pos);
  
  printf("%s\n\n", pos.BoardString().c_str());
  
  Position::Move move = master.GetMove(pos);
  string ms = pos.ShortMoveString(move);
  CHECK(ms == "Qxg2") << ms;
}

void TestBlackCheckmate() {
  Chessmaster master(1);

  Position pos;
  // White's only move is to mate black (Qg2).
  Position::ParseFEN("8/8/8/3b4/4b2p/7k/4n1Q1/7K b - - 2 9", &pos);
  
  printf("%s\n\n", pos.BoardString().c_str());
  
  Position::Move move = master.GetMove(pos);
  string ms = pos.ShortMoveString(move);
  CHECK(ms == "Bxg2") << ms;
}

int main(int argc, char **argv) {
  TestWhiteCheckmate();
  TestBlackCheckmate();

  #if 0
  Chessmaster master(1);

  Position pos;
  // Position::ParseFEN("r1b1kb1r/pp1n1ppp/4pn2/3pN1B1/3P4/2P5/Pq1N1PPP/R2QKB1R w KQkq - 2 9",
  // &pos);

  // White's only move is to mate black (Qg2).
  Position::ParseFEN("8/8/6Q1/8/7p/7k/4n1b1/7K w - - 2 9", &pos);
  
  printf("%s\n\n", pos.BoardString().c_str());
  
  Position::Move move = master.GetMove(pos);

  if (move.src_row == 0 &&
      move.src_col == 0 &&
      move.dst_row == 0 &&
      move.dst_col == 0) {
    printf("\nFound no move :(\n");
  } else {
    printf("\nMove: %s\n", pos.ShortMoveString(move).c_str());
  }
#endif
  
  return 0;

}
