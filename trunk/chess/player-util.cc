
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
