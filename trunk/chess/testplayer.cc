
#include "chessmaster.h"

#include <string>
#include <memory>
#include <cstdint>
#include <utility>

#include "../cc-lib/base/logging.h"

#include "chess.h"
#include "player.h"
#include "blind-player.h"

using namespace std;

struct TextExplainer : public Explainer {
  explicit TextExplainer(Position pos) : pos(pos) {}
  void SetScoredMoves(
      const vector<tuple<Position::Move, int64_t, string>> &v) override {
    for (const auto &p : v) {
      Position::Move move = std::get<0>(p);
      string m = "(ILLEGAL)";
      if (pos.IsLegal(move)) m = pos.ShortMoveString(move);
      printf("  %s %lld %s\n", m.c_str(),
	     std::get<1>(p),
	     std::get<2>(p).c_str());
    }
    fflush(stdout);
  }

  Position pos;
};

int main(int argc, char **argv) {
  std::unique_ptr<Player> white_player{BlindSpycheck()};
  std::unique_ptr<Player> black_player{MinOpponentMoves()};

  std::unique_ptr<PlayerGame> white{white_player->CreateGame()};
  std::unique_ptr<PlayerGame> black{black_player->CreateGame()};

  Position pos;

  int movenum = 1;
  bool black_turn = false;
  for (;;) {
    TextExplainer explainer{pos};
    Position::Move move =
      black_turn ?
      black->GetMove(pos, &explainer) : white->GetMove(pos, &explainer);
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
