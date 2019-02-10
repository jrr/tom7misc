
#include "player-util.h"

#include <string>
#include <mutex>
#include <vector>

#include "chess.h"
#include "../cc-lib/base/stringprintf.h"

using Move = Position::Move;
using int64 = int64_t;
using namespace std;

static std::mutex seed_m;
string PlayerUtil::GetSeed() {
  static int64 counter = 0LL;
  int64 c = 0LL;
  seed_m.lock();
  c = counter++;
  seed_m.unlock();

  int64 t = time(nullptr);
  return StringPrintf("s%lld.%lld", c, t);
}

bool PlayerUtil::ParseLongMove(const string &move_s, bool black, Move *m) {
  if (move_s.size() != 4 && move_s.size() != 5)
    return false;

  if (move_s[0] < 'a' || move_s[0] > 'h')
    return false;
  if (move_s[1] < '1' || move_s[1] > '8')
    return false;
  if (move_s[2] < 'a' || move_s[2] > 'h')
    return false;
  if (move_s[3] < '1' || move_s[3] > '8')
    return false;

  m->src_col = move_s[0] - 'a';
  m->src_row = 7 - (move_s[1] - '1');
  m->dst_col = move_s[2] - 'a';
  m->dst_row = 7 - (move_s[3] - '1');

  if (move_s.size() == 5) {
    uint8 my_mask = black ? Position::BLACK : Position::WHITE;
    switch (move_s[4]) {
    case 'q':
      m->promote_to = Position::QUEEN | my_mask;
      break;
    case 'n':
      m->promote_to = Position::KNIGHT | my_mask;
      break;
    case 'b':
      m->promote_to = Position::BISHOP | my_mask;
      break;
    case 'r':
      m->promote_to = Position::ROOK | my_mask;
      break;
    default:
      return false;
    }
  }

  return true;
}


EvalResultPlayer::EvalResultPlayer() : rc(PlayerUtil::GetSeed()) {
  rc.Discard(800);
}


Move EvalResultPlayer::MakeMove(const Position &orig_pos) {
  Position pos = orig_pos;
  std::vector<LabeledMove> labeled;
  for (const Move &m : pos.GetLegalMoves()) {
    LabeledMove lm;
    lm.m = m;
    lm.r = Rand32(&rc);
    pos.MoveExcursion(m,
		      [this, &pos, &lm]() {
			lm.penalty = PositionPenalty(&pos);
			return 0;
		      });
    labeled.push_back(lm);
  }
  CHECK(!labeled.empty());

  return PlayerUtil::GetBest(
      labeled,
      [](const LabeledMove &a,
	 const LabeledMove &b) {
	if (a.penalty != b.penalty)
	  return a.penalty < b.penalty;
	
	return a.r < b.r;
      }).m;
}
