
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

  Chessmaster master(2);
  
  // Failures here are normal if we have a mated position.
  Position pos;
  int movenum = 1;
  for (;;) {
    Position::Move move = master.GetMove(pos);
    CHECK(pos.IsLegal(move)) << pos.LongMoveString(move);
    if (!pos.BlackMove()) {
      printf(" %d.", movenum);
      movenum++;
    }
    printf(" %s", pos.ShortMoveString(move).c_str());
    fflush(stdout);
    pos.ApplyMove(move);
  }
    
  return 0;
}
