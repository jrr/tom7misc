
#include "player.h"

#include <string>
#include <mutex>
#include <cstdint>
#include <memory>

#include "../cc-lib/randutil.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/util.h"

#include "player-util.h"
#include "numeric-player.h"

using namespace std;
using int64 = int64_t;
using Move = Position::Move;

namespace {

struct BinaryDigits {
  explicit BinaryDigits(const string &filename) {
    string contents = Util::ReadFile(filename);
    CHECK(!contents.empty()) << filename;

    // Skip "3." in 3.14159...
    CHECK(contents[1] == '.') << filename;
    bits.reserve((contents.size() - 2) * 4);
    for (int i = 2; i < contents.size(); i++) {
      // Values are actually ASCII hex digits.
      uint8 c = contents[i];
      CHECK(Util::IsHexDigit(c)) << filename;
      const uint8 h = Util::HexDigitValue(c);
      // One "digit" is four bits.
      bits.push_back(!!(h & 0b1000));
      bits.push_back(!!(h & 0b0100));
      bits.push_back(!!(h & 0b0010));
      bits.push_back(!!(h & 0b0001));
    }
  }

  vector<bool> bits;
};

const vector<bool> &GetPi() {
  static BinaryDigits *pi = new BinaryDigits("pi.txt");
  return pi->bits;
}

const vector<bool> &GetE() {
  static BinaryDigits *e = new BinaryDigits("e.txt");
  return e->bits;
}

template<const vector<bool> &(*F)(), const char *(*CONSTANT)()>
struct NumericPlayer : public Player {
  NumericPlayer() : bits(F()) {}

  struct NumericGame : public PlayerGame {
    explicit NumericGame(const NumericPlayer &p) : player(p) {}
    
    // Most canonical way to order these is asciibetically by their
    // algebraic move names.
    struct LabeledMove {
      Move m;
      string move_string;
    };

    // PERF: Could cache the previous call so that GetMove/ForceMove
    // pair doesn't need to repeat the work.
    void InternalMove(const Position &orig_pos,
		      Position::Move *m, int *bits_used,
		      Explainer *explainer) {
      Position pos = orig_pos;

      std::vector<LabeledMove> labeled;
      std::vector<Position::Move> moves = pos.GetLegalMoves();
      CHECK(!moves.empty());
      if (moves.size() == 1) {
	*m = moves[0];
	*bits_used = 0;
	if (explainer != nullptr) {
	  explainer->SetMessage(
	      StringPrintf("Forced move: %s",
			   pos.ShortMoveString(moves[0]).c_str()));
	}
	return;
      }
      
      labeled.reserve(moves.size());
      for (const Move &m : moves) {
	LabeledMove lm;
	lm.m = m;
	lm.move_string = pos.ShortMoveString(m);
	labeled.push_back(lm);
      }
      
      std::sort(labeled.begin(),
		labeled.end(),
		[](const LabeledMove &a,
		   const LabeledMove &b) {
		  return a.move_string < b.move_string;
		});

      int num_moves = labeled.size();
      // Mask all ones, and no more than 2047 (2^12 - 1), since there
      // are fewer than this many distinct possible moves in ANY
      // position.
      uint32 mask = num_moves - 1;
      mask |= mask >> 1;
      mask |= mask >> 2;
      mask |= mask >> 4;
      mask |= mask >> 8;
      mask |= mask >> 16;

      auto GetNumBits = [mask]() {
	  for (int i = 0; i < 13; i++) {
	    if (!(mask & (1 << i))) {
	      return i;
	    }
	  }
	  LOG(FATAL) << "Impossible " << mask;
	  return 0;
	};
      
      const int num_bits = GetNumBits();
      CHECK((1 << num_bits) - 1 == mask) << num_bits << " " << mask;

      CHECK(stream_idx < player.bits.size() - 16) << "Ran out of bits!! "
						  << stream_idx;
      int used = 0, rejections = 0;
      for (;;) {
	uint32 r = 0u;
	for (int i = 0; i < num_bits; i++) {
	  r <<= 1;
	  r |= player.bits[stream_idx + used + i];
	}
	used += num_bits;
	if (r < num_moves) {
	  *bits_used = used;
	  *m = labeled[r].m;
	  if (explainer != nullptr) {
	    explainer->SetMessage(
		StringPrintf("@%d Used %d bits (%d rejections) to select "
			     "from %d moves:",
			     stream_idx, used, rejections, num_moves));
	    vector<tuple<Position::Move, int64_t, string>> vec;
	    for (int i = 0; i < labeled.size(); i++) {
	      vec.emplace_back(labeled[i].m, i,
			       i == r ?
			       StringPrintf("%s <-",
					    labeled[i].move_string.c_str()) :
			       labeled[i].move_string);
	    }
	    explainer->SetScoredMoves(vec);
	  }
	  return;
	}
	rejections++;
      }
    }
    
    // Conceptually we just want to use some of the bits to choose a
    // move. This is complicated by the get/force interface for
    // stateful players, alas. In any given state, we will use a
    // deterministic number of bits depending on the number of legal
    // moves and the position in the stream. This can be anything from
    // 0 (single move forced) to arbitrarily many if we get unlucky
    // with the rejection sampling! ForceMove ignores the move but
    // advances the stream this number of bits. GetMove computes the
    // move but does not advance the stream. 
    void ForceMove(const Position &pos, Position::Move m_ignored) override {
      Position::Move move_ignored;
      int bits_used = 0;
      InternalMove(pos, &move_ignored, &bits_used, nullptr);
      stream_idx += bits_used;
    }
    
    Move GetMove(const Position &pos, Explainer *explainer) override {
      Position::Move move;
      int bits_used = 0;
      InternalMove(pos, &move, &bits_used, explainer);
      return move;
    }

    int stream_idx = 0;
    const NumericPlayer &player;
  };

  PlayerGame *CreateGame() override {
    return new NumericGame(*this);
  }
  
  string Name() const override {
    return StringPrintf("numeric_%s", CONSTANT());
  }
  string Desc() const override {
    return StringPrintf(
	"Choose the nth legal move, using bits from the binary "
	"expansion of %s", CONSTANT());
  }

  const vector<bool> &bits;
};

const char *PiName() { return "pi"; }
const char *EName() { return "e"; }

}  // namespace



Player *NumericPi() {
  return new NumericPlayer<GetPi, PiName>;
}

Player *NumericE() {
  return new NumericPlayer<GetE, EName>;
}
