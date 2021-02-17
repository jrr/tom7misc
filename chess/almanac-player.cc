
#include "almanac-player.h"

#include "player.h"
#include "chess.h"
#include "common.h"

#include "arcfour.h"
#include "player-util.h"
#include "packedgame.h"

using Move = Position::Move;
using namespace std;

namespace {

// Singleton "common position" map.
const CommonMap &GetCommon() {
  static CommonMap *the_map =
    new CommonMap("d:\\chess\\packed\\common_map.bin");
  return *the_map;
}


struct AlmanacPopularPlayer : public StatelessPlayer {
  AlmanacPopularPlayer() : rc(PlayerUtil::GetSeed()), cm(GetCommon()) {
    rc.Discard(800);
  }

  // Plays the most popular historically-played move in each position,
  // breaking ties randomly.
  Move MakeMove(const Position &orig_pos, Explainer *explainer) override {
    Position pos = orig_pos;

    uint64 hc = PositionHash{}(pos);

    auto it = cm.positions.find(hc);

    auto RandomMove = [this, explainer, &pos, hc]() {
        if (explainer) explainer->SetMessage(
            StringPrintf("%llx new position", hc));
        std::vector<Move> legal = pos.GetLegalMoves();
        CHECK(!legal.empty());
        return legal[RandTo32(&rc, legal.size())];
      };
    if (it == cm.positions.end()) {
      return RandomMove();
    }

    // These moves are all supposed to be legal, but it is possible to
    // have hash collisions, so validate them.
    const CommonMap::MoveCounts &mc = it->second;
    int best_count = 0;
    vector<Move> best_moves;
    for (const auto &c : mc) {
      Position::Move move = PackedGame::UnpackMove(c.first);
      if (pos.IsLegal(move)) {
        if (c.second > best_count) {
          best_count = c.second;
          best_moves = {move};
        } else if (c.second == best_count) {
          best_moves.push_back(move);
        }
      }
    }

    if (best_moves.empty()) return RandomMove();

    Move chosen_move = best_moves.size() == 1 ?
      best_moves[0] :
      best_moves[RandTo32(&rc, best_moves.size())];

    if (explainer) {
      vector<tuple<Position::Move, int64_t, string>> scored_moves;
      string illegal;
      for (const auto &c : mc) {
        Position::Move move = PackedGame::UnpackMove(c.first);
        if (pos.IsLegal(move)) {
          scored_moves.emplace_back(move, c.second,
                                    Position::MoveEq(move, chosen_move) ?
                                    (string)" <- " : (string)"");
        } else {
          illegal += Position::DebugMoveString(move) + " ";
        }
      }
      std::sort(scored_moves.begin(), scored_moves.end(),
                [](const tuple<Position::Move, int64_t, string> &a,
                   const tuple<Position::Move, int64_t, string> &b) {
                  return std::get<1>(b) < std::get<1>(a);
                });

      if (scored_moves.empty()) {
        // This should be impossible given that we got here...!
        explainer->SetMessage("no legal moves?! (Illegal: " + illegal + ")");
      } else {
        explainer->SetScoredMoves(scored_moves);
        if (!illegal.empty()) explainer->SetMessage(
            (string)"Illegal: " + illegal);
      }
    }

    return chosen_move;
  }

  string Name() const override { return "almanac_popular"; }
  string Desc() const override {
    return "Choose randomly among the most popular historically-played "
      "moves in the position.";
  }

  ArcFour rc;
  const CommonMap &cm;
};

}  // namespace

Player *AlmanacPopular() {
  return new MakeStateless<AlmanacPopularPlayer>;
}
