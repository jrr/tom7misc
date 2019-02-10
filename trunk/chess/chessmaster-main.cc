
#include "chessmaster.h"

int main(int argc, char **argv) {
  Chessmaster master(1);

  Position pos;
  Position::ParseFEN("r1b1kb1r/pp1n1ppp/4pn2/3pN1B1/3P4/2P5/Pq1N1PPP/R2QKB1R w KQkq - 2 9",
		     &pos);

  printf("%s\n\n", pos.BoardString().c_str());
  
  master.GetMove(pos);

  return 0;
}
