
#include "chessmaster.h"

#include <string>
#include <memory>

#include "../cc-lib/base/logging.h"

#include "chess.h"
#include "player.h"
#include "blind-player.h"

using namespace std;

int main(int argc, char **argv) {
  std::unique_ptr<Player> white_player{BlindYolo()};
  std::unique_ptr<Player> black_player{Chessmaster1()};

  std::unique_ptr<PlayerGame> white{white_player->CreateGame()};
  std::unique_ptr<PlayerGame> black{black_player->CreateGame()};

  Position pos;

  int movenum = 1;
  bool black_turn = false;
  for (;;) {
    Position::Move move =
      black_turn ? black->GetMove(pos) : white->GetMove(pos);
    CHECK(pos.IsLegal(move)) << pos.LongMoveString(move);
    if (!black_turn) {
      printf(" %d.", movenum);
      movenum++;
    }
    printf(" %s", pos.ShortMoveString(move).c_str());
    fflush(stdout);
    white->ForceMove(pos, move);
    black->ForceMove(pos, move);
    pos.ApplyMove(move);
    black_turn = !black_turn;
  }

  return 0;
}
