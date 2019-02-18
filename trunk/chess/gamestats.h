#ifndef __GAMESTATS_H
#define __GAMESTATS_H

#include <shared_mutex>
#include <cstdint>

#include "base/logging.h"
#include "chess.h"

// Whole purpose of running over games is to collect these stats. So
// that we can estimate variance, we also randomly assign players to
// one of G groups (by hash of username) and accumulate a given game
// into the white player's bucket. So there are actually G Stats
// objects.
static constexpr int NUM_BUCKETS = 32;
static_assert(! (NUM_BUCKETS & (NUM_BUCKETS - 1)),
	      "Must be a power of two");
static constexpr int64_t NUM_BUCKETS_MASK = NUM_BUCKETS - 1;


// Results of a single game.
struct GameStats {
  static constexpr uint8_t DIED = 0b10000000;
  static constexpr uint8_t POS_MASK = 0b00111111;
  // Initialized to their start squares (alive) in standard position.
  uint8_t fates[32] = { 0,  1,  2,  3,  4,  5,  6,  7,
		        8,  9, 10, 11, 12, 13, 14, 15,
		       48, 49, 50, 51, 52, 53, 54, 55,
		       56, 57, 58, 59, 60, 61, 62, 63, };

  static constexpr uint8_t BLACK_QUEEN = 3;
  static constexpr uint8_t WHITE_QUEEN = 27;
  
  void Update(const Position &pos, const Position::Move &move) {
    const uint8 src_pos = move.src_row * 8 + move.src_col;
    const uint8 dst_pos = move.dst_row * 8 + move.dst_col;
    for (int i = 0; i < 32; i++) {
      // Move the living piece.
      if (fates[i] == src_pos) {
	fates[i] = dst_pos;
      } else if (fates[i] == dst_pos) {
	// There's a piece in the destination square; it is captured.
	fates[i] |= GameStats::DIED;
      }
    }
	
    // Also handle castling. We can assume the move is legal,
    // so if it's a king moving two spaces, we know where the
    // rooks are and where they're going.
    if (src_pos == 4 && pos.PieceAt(0, 4) ==
	(Position::BLACK | Position::KING)) {
      if (dst_pos == 2) {
	fates[0] = 3;
      } else if (dst_pos == 6) {
	fates[7] = 5;
      }
    } else if (src_pos == 60 && pos.PieceAt(7, 4) ==
	       (Position::WHITE | Position::KING)) {
      if (dst_pos == 58) {
	fates[24] = 59;
      } else if (dst_pos == 62) {
	fates[31] = 61;
      }
    }

    // If it was an en passant capture, need to kill the captured
    // pawn. The loop above did not, because the captured pawn
    // is not on the destination square.
    if (((move.src_row == 3 && move.dst_row == 2) ||
	 (move.src_row == 4 && move.dst_row == 5)) &&
	move.src_col != move.dst_col &&
	pos.PieceAt(move.dst_row, move.dst_col) == Position::EMPTY &&
	(pos.PieceAt(move.src_row, move.src_col) & Position::TYPE_MASK) ==
	Position::PAWN) {
      // en passant capture.
      // If row 3, then white is capturing black, which is on the row
      // below the dst pos. Otherwise, the row above.
      const uint8 cap_pos =
	(move.src_row == 3) ? dst_pos + 8 : dst_pos - 8;
      for (int i = 0; i < 32; i++) {
	if (fates[i] == cap_pos) {
	  fates[i] |= GameStats::DIED;
	  return;
	}
      }
      CHECK(false) << "Apparent en passant capture, but no piece "
	"was at " << cap_pos << " to be captured.\n" <<
	pos.BoardString() << "\nwith move: " <<
	move.src_row << " " << move.src_col << " -> " <<
	move.dst_row << " " << move.dst_col;
    }
  }
};

struct PieceStats {
  int64_t died_on[64] = {};
  int64_t survived_on[64] = {};
};

struct Stats {
  std::shared_mutex m;
  int64_t num_games = 0LL;
  PieceStats pieces[32] = {};
  void AddGame(const GameStats &gs) {
    m.lock();
    num_games++;
    for (int i = 0; i < 32; i++) {
      const uint8_t fate = gs.fates[i];
      if (fate & GameStats::DIED) {
	pieces[i].died_on[fate & GameStats::POS_MASK]++;
      } else {
	pieces[i].survived_on[fate & GameStats::POS_MASK]++;
      }
    }
    m.unlock();
  }
};


#endif
