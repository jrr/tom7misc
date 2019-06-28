
#include "chessmaster.h"

#include <string>
#include <memory>
#include <cstdint>
#include <utility>

#include "../cc-lib/base/logging.h"

#include "chess.h"
#include "player.h"
#include "blind-player.h"
#include "almanac-player.h"
#include "numeric-player.h"
#include "fates.h"
#include "fate-player.h"

// #define VERBOSE

using namespace std;

struct TextExplainer : public Explainer {
  explicit TextExplainer(Position pos) : pos(pos) {}
  void SetScoredMoves(
      const vector<tuple<Position::Move, int64_t, string>> &v) override {
    printf("\nSetScoredMoves\n");
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

  void SetMessage(const string &s) override {
    printf("\nMessage: [%s]\n", s.c_str());
    fflush(stdout);
  }

  void SetPosition(const Position &pos) override {
    printf("%s\n", pos.BoardString().c_str());
    fflush(stdout);
  }

  void SetGraphic(int w, int h,
		  const std::vector<uint8> &rgba) override {
    CHECK(rgba.size() == w * h * 4);
    printf("(got %dx%d graphic)\n", w, h);
    fflush(stdout);
  }

  Position pos;
};

int main(int argc, char **argv) {
  std::unique_ptr<Player> white_player{RationalPi()};
  std::unique_ptr<Player> black_player{RationalE()};

  // #define VERBOSE
  
  static constexpr int NUM_LOOPS = 1;
  
  for (int loops = 0; loops < NUM_LOOPS; loops++) {
    std::unique_ptr<PlayerGame> white{white_player->CreateGame()};
    std::unique_ptr<PlayerGame> black{black_player->CreateGame()};
    Fates fates;
    
    Position pos;

    int movenum = 1;
    bool black_turn = false;
    int stale_moves = 0;
    while (pos.HasLegalMoves()) {
  #ifdef VERBOSE
      printf("\n----\n");
      TextExplainer text_explainer{pos};
      Explainer *explainer = &text_explainer;
  #else 
      Explainer *explainer = nullptr;
  #endif
      
      Position::Move move =
	black_turn ?
	black->GetMove(pos, explainer) : white->GetMove(pos, explainer);
      CHECK(pos.IsLegal(move)) << pos.LongMoveString(move);

      if (pos.IsCapturing(move) ||
	  pos.IsPawnMove(move)) {
	stale_moves = 0;
      } else {
	stale_moves++;
	if (stale_moves > 150) break;
      }


      if (!black_turn) {
  #ifdef VERBOSE
	printf(" %d.", movenum);
  #endif
	movenum++;
      }
  #ifdef VSEBOSE
      printf(" %s", pos.ShortMoveString(move).c_str());
      fflush(stdout);
  #endif
      white->ForceMove(pos, move);
      black->ForceMove(pos, move);
      fates.Update(pos, move);
      pos.ApplyMove(move);
      black_turn = !black_turn;
    }

#if 1 || defined(VERBOSE)
    printf("\nGame over:\n%s\n", pos.BoardString().c_str());
    #endif
  }
  printf("Ran %d games.\n", NUM_LOOPS);
  fflush(stdout);
  
  return 0;
}
