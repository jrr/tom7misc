
// SIGBOVIK: "Is this the longest chess game?"

// XXX TODO: Note that formally, "the game is drawn when
// a position has arisen in which neither player can checkmate
// the opponent's king with any series of legal moves."
// "This immediately ends the game," so this adds some subtlety
// to the final position.
//  - If we place the queen right next to the opponent king,
//    who captures it and leaves us with insufficient material,
//    then the game actually ended right before this capture if
//    the capture was forced. (The only legal move leads to
//    a situation which is drawn, and so there is no sequence
//    of legal moves leading to a checkmate as above.)
//  - More confusingly, even if the king can avoid capture (leaving us
//    with the queen on the board; sufficient material), if this move
//    is actually the 75th one, then all legal moves draw the game:
//    Either by capturing (now insufficient material) or by reaching 75
//    moves without a capture or pawn move.
// So, the specific slow game below is probably technically wrong, since
// it ends with the king taking an adjacent queen.
//
// Expert mode here would be the longest draw? Can it be as long as the
// longest win?

#include <functional>
#include <memory>
#include <unordered_map>
#include <cstdint>
#include <vector>
#include <string>
#include <map>
#include <utility>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"

#include "chess.h"
#include "pgn.h"

using namespace std;

using Move = Position::Move;
using int64 = int64_t;

template<class T>
using PositionMap = std::unordered_map<Position, T, PositionHash, PositionEq>;

// Generate a "proof game" of a longest possible chess game.
// Chess can't go forever even if neither player is interested
// in a draw; the following conditions automatically cause
// a draw:
//  1. The same position is repeated 5 times. [1]
//  2. 75 half-moves are played by each player without a pawn 
//     move or capture.
//  3. There is insufficient material for even a "helpmate" (for
//     example, two kings). [Actually this is not quite right;
//     see the TODO at the top of the file.]
//
// ([1] Actually really? The rules say "the same position has
// appeared, as in 9.2b, for at least five consecutive alternate moves
// by each player.") I think this is pretty weird, but perhaps
// "alternate" means a pair like Nf3 Ng1. The "consecutive" requirement
// here is especially surprising, since of course there are always
// intervening positions, and the threefold repetion claim does not
// require them to be consecutive. Anyway: We'll just produce a game
// where no position appears 5 times, which would also mean that
// "consecutive alternate" would not apply, whatever it is.)
//
// Note in rule 2 that pawn moves and captures are examples of
// irreversible moves, and also ones that we can only perform a finite
// number of times. Because they are irreversible, it is impossible to
// repeat positions (rule 1) across such a move. Because we can only
// do them a finite number of times, we will want to maximize the
// number we do. So the longest game will have the form of 75+74
// non-capturing and non-pawn moves (without repeating 5 times),
// followed by a pawn move or capture; then the same again as many
// times as we can, without triggering rule 3. The best we can do with
// rule 3 is to have two kings and one other piece (since two kings
// is clearly insufficient material). We'll do a single queen with
// two kings, since this is sufficient for a helpmate. The final
// move of the game can be the queen being captured (on the 75th
// move), or mate if we like.

// So we capture all 30 pieces, giving us 30*150 (half) moves,
// plus P*150 for P pawn moves. Naively P=16*6, with each of the 16
// pawns moving one square at a time to the back rank. But the
// pawns need to cross each other: Can we actually promote each
// one?
//
// Well, we can very likely promote each one; the white A pawn
// opposing the black A pawn can move into the B file (clearing the
// way for black's A pawn) by capturing. Getting white's pieces out of
// the way like this uses up a lot of black pieces (by getting
// captured), but we will have plenty due to all the promotion. Alas,
// the problem is that the capture we use to move out of the row is
// both a capture *and* a pawn move, so it wastes 150 moves against our
// naive maximum. It doesn't seem that we can avoid this, since a pawn
// can only move out of its column by capturing (including en passant)
// and there would be no way for a pawn to pass through the opposing
// pawn without one of them moving out of the column. Note that when
// the white pawn moves from the A file to the B file, it should do so
// behind the black B pawn. In this case it clears itself to promote
// and clears the black A pawn too. So it would seem that we need to
// do this 8 times, each freeing two pawns. That gives us (30 + 16*6 -
// 8) * 150 moves, (+/- off-by-one problems), which is 17700 half-moves.
// Pretty good.

// The way we'll find this game is to manually generate a slow game,
// that is, one where we make make 16*6 pawn moves (but 8 are captures)
// and capture all the pieces except for a single queen, and then
// capture it, too. Then all that remains is to artificially slow
// down this game even more, by inserting 75+74 half-moves that get us back
// to the position we're already in (without repeating 5 times). The
// hypothesis is that such sequences are numerous, so we can find
// them easily (perhaps by just randomly moving a piece around, or
// perhaps by lazily generating some kind of graph structure).

// Split into critical segments.
// Critical moves are the moments where we either move a pawn or
// capture a piece (and sometimes both; see above). We don't need to
// care about what these are. We just want to put the board in
// exactly the position before the move, and then execute it. We
// don't need to worry about repeating a position that occurred
// before or after a critical move, because this is impossible.
// Therefore, we'll be able to work on each critical section
// independently.
struct Critical {
  // The position before executing any moves.
  Position start_pos;
  // Moves, not including the critical one.
  // No captures or pawn moves are allowed here, nor n-fold repetition.
  vector<Move> moves;
  // The position immediately before executing the critical move.
  Position end_pos;
  // The critical move, which will be a capture or pawn move.
  Move critical_move;
};


static vector<Critical> MakeCritical() {
  // Made manually.
  // XXX it seems that for parity's sake, we might need to either
  // alternate which side is making the critical move, or minimize
  // the number of switches?
  //
  // TODO:
  // Looks like it's the latter; critical moves should be made 
  // consecutively by the same player.
  // Since we need to move all of black's pawns and all of white's
  // (plus the captures), we will need to switch from one player
  // doing consecutive critical moves to the other at least once.
  // But due to the way pawns interact, it seems it must be more
  // than that?
  // 
  // Also looks like the first pawn move should be made by black with
  // white having moved one knight to an opposite color. So the first
  // phase of critical moves should be made by black.
  string slowgame_fen = 
    "1. Nf3 e6 2. Rg1 e5 3. Rh1 e4 4. Rg1 g6 5. Rh1 g5 6. Rg1 g4 "
    "7. Rh1 c6 8. Rg1 c5 9. Rh1 c4 10. Rg1 a6 11. Rh1 a5 12. Rg1 a4 "
    "13. Rh1 Ra7 14. Rg1 Na6 15. Rh1 Nc7 16. Rg1 Bg7 17. Rh1 Qf6 "
    "18. Rg1 Qg6 19. Rh1 Ra5 20. Rg1 Rb5 21. Rh1 Rb3 22. Rg1 Nf6 "
    "23. Rh1 Nh5 24. Rg1 Nf4 25. Rh1 Nh3 26. Rg1 Bf8 27. Nd4 Qf5 "
    "28. Rh1 Qf3 29. Rg1 Rg8 30. Rh1 Rg5 31. Nb5 Rd5 32. Na7 Rdd3 "
    "33. Nc6 b6 34. Na5 bxa5 35. axb3 Ne6 36. cxd3 Nc7 37. exf3 Ne6 "
    "38. gxh3 Ng7 39. b4 Ne6 40. b5 Ng7 41. b6 Ne6 42. b7 Ng7 "
    "43. b8=R Ne6 44. Rb5 Ng7 45. b3 Ne6 46. b4 Ng7 47. Rg5 Ne6 "
    "48. b5 Ng7 49. b6 Ne6 50. b7 Ng7 51. b8=B Ne6 52. Be5 Nc7 "
    "53. Ba3 Ne6 54. Bxf8 Ng7 55. Nc3 Ne6 56. Nd5 Ng7 57. Nb6 Ne6 "
    "58. Nxc8 Ng7 59. Bfxg7 Kd8 60. Rb1 Ke8 61. Rb5 Kd8 62. Rc5 Ke8 "
    "63. Qb3 Kd8 64. Be2 Ke8 65. Bd1 Kd8 66. Bc2 d6 67. Qb8 dxc5 "
    "68. Bb3 f6 69. Bc2 fxe5 70. Bb3 h6 71. Bc2 hxg5 72. Bb3 a3 "
    "73. Bc2 a2 74. Bb3 a1=N 75. Bh6 Nxb3 76. Bg7 a4 77. Bh6 a3 "
    "78. Bg7 a2 79. Bh6 a1=N 80. Bg7 Na5 81. Qb7 Nxb7 82. Bh6 c3 "
    "83. Bg7 c2 84. Bh6 c1=N 85. Bg7 c4 86. Bh6 c3 87. Bg7 c2 "
    "88. Bh6 Ncb3 89. Bg7 c1=N 90. Bf8 N3a5 91. Kd1 Na2 92. Bb4 Nxb4 "
    "93. Kc1 Nd5 94. Kb2 Nf4 95. Ka2 Ng2 96. Re1 Nxe1 97. Ka3 Ng2 "
    "98. Ka4 e3 99. Ka3 e2 100. Ka4 e1=N 101. Ka3 e4 102. Ka4 Nec2 "
    "103. Kb5 Nce3 104. Ka4 Nf5 105. Kb4 Nc4 106. Kb5 Ne5 107. Ka4 Nbd6 "
    "108. Ka5 Nxc8 109. Ka4 Nc2 110. Ka5 Ncd4 111. Ka4 Ne6 112. Ka5 e3 "
    "113. Ka4 e2 114. Ka5 e1=R 115. Ka4 Ngh4 116. Ka5 g3 117. Ka4 Nhg6 "
    "118. Ka5 g4 119. Ka4 g2 120. Ka5 g1=R 121. Ka4 g3 122. Ka5 Rgf1 "
    "123. Ka4 g2 124. Ka5 g1=R 125. h4 Kc7 126. h5 Kb8 127. h6 Ka8 "
    "128. h7 Kb8 129. h8=B Ka8 130. Bxe5 Rg3 131. Bxg3 Ne5 132. Bxe5 Neg7 "
    "133. Bxg7 Nd4 134. Bxd4 Rg1 135. f4 Ref1 136. f3 Re1 137. f5 Ref1 "
    "138. f6 Re1 139. f7 Ref1 140. f8=R Re1 141. Re8 Ref1 142. Bxg1 Re1 "
    "143. f4 Rf1 144. f5 Re1 145. f6 Rf1 146. f7 Re1 147. f8=N Re2 "
    "148. h3 Re1 149. h4 Re2 150. h5 Re1 151. h6 Re2 152. h7 Re1 "
    "153. h8=Q Re2 154. d4 Re1 155. d5 Re2 156. d6 Re1 157. d7 Re2 "
    "158. d8=B Re3 159. d3 Re2 160. Bh4 Re3 161. d4 Re2 162. d5 Re3 "
    "163. d6 Re2 164. d7 Re3 165. d8=N Ka7 166. Ka4 Ka6 167. Kb4 Ne7 "
    "168. Kc4 Rg3 169. Bxg3 Ng8 170. Qxg8 Ka5 171. Kd3 Ka6 172. Ke2 Ka5 "
    "173. Kf2 Ka6 174. Kg2 Ka5 175. Bc5 Kb5 176. Bh2 Kxc5 177. Re6 Kd5 "
    "178. Nh7 Kc5 179. Rd6 Kb5 180. Ra6 Kxa6 181. Nb7 Kxb7 182. Qa2 Kc6 "
    "183. Bd6 Kxd6 184. Nf6 Ke7 185. Nd7 Kxd7 186. Kf3 Ke7 187. Kf4 Kf6 "
    "188. Qf2 Kg6 189. Qg2+ Kh5 190. Ke3 Kh6 191. Qg7+ Kxg7";
  
  vector<Move> moves;
  {
    vector<PGN::Move> movestrings;
    CHECK(PGN::ParseMoves(slowgame_fen, &movestrings));
    Position pos;
    for (const auto &ms : movestrings) {
      Move m;
      CHECK(pos.ParseMove(ms.move.c_str(), &m)) << ms.move;
      CHECK(pos.IsLegal(m));
      moves.push_back(m);
      pos.ApplyMove(m);
    }
  }

  vector<Critical> crits;
  int pawn_moves = 0, captures = 0;
  Position pos;
  Critical crit;
  crit.start_pos = pos;
  for (const Move &m : moves) {
    const bool is_pawn = pos.IsPawnMove(m);
    const bool is_capturing = pos.IsCapturing(m);
    if (is_pawn) pawn_moves++;
    if (is_capturing) captures++;

    const bool is_critical = is_pawn || is_capturing;
    if (is_critical) {
      crit.end_pos = pos;
      crit.critical_move = m;
      crits.push_back(crit);
      pos.ApplyMove(m);
      crit.start_pos = pos;
      crit.moves.clear();
    } else {
      crit.moves.push_back(m);
      pos.ApplyMove(m);
    }
  }
  // The last move should have been critical.
  CHECK(crit.moves.empty());
  // This particular game ends with a draw from insufficient material.
  CHECK(!pos.IsInCheck());
  CHECK(!pos.IsMated());


  printf("Slow game: %d pawn moves, %d captures\n"
	 "Critical sections: %d\n",
	 pawn_moves, captures,
	 (int)crits.size());
  CHECK(crits.size() == 118);
  return crits;
}

// Try making an excursion from orig_pos and returning to orig_pos
// that consists of 2 moves per player. Does not visit a position
// that already has 4 occurrences in 'seen'. If successful, populate
// the series of moves in 'excursion' and update the 'seen' map to
// account for the new positions (any intermediate ones, plus another
// occurrence of orig_pos).
static bool MakeEvenExcursion(const Position &orig_pos,
			      ArcFour *rc,
			      PositionMap<int> *seen,
			      vector<Move> *excursion) {
  // If we've already visited this position 4 times, then this is
  // not possible.
  if ((*seen)[orig_pos] >= 4)
    return false;

  Position pos = orig_pos;
  // Otherwise, we just do this by moving a piece out and back.
  vector<Move> cur_moves = pos.GetLegalMoves();
  // Try these in a random order, which should help avoid
  // repeating positions.
  Shuffle(rc, &cur_moves);
  for (const Move &cur_move : cur_moves) {
    // These are irreversible.
    if (pos.IsCapturing(cur_move) ||
	pos.IsPawnMove(cur_move) ||
	pos.IsCastling(cur_move))
      continue;
    Position pos2 = pos;
    pos2.ApplyMove(cur_move);
    // Too many repeats?
    if ((*seen)[pos2] >= 4)
      continue;

    // Same, for the opponent.
    vector<Move> opp_moves = pos2.GetLegalMoves();
    Shuffle(rc, &opp_moves);
    for (const Move &opp_move : opp_moves) {
      if (pos2.IsCapturing(opp_move) ||
	  pos2.IsPawnMove(opp_move) ||
	  pos2.IsCastling(opp_move))
	continue;
      Position pos3 = pos2;
      pos3.ApplyMove(opp_move);
      if ((*seen)[pos3] >= 4)
	continue;

      auto Rev = [](Move m) -> Move {
	Move ret;
	ret.src_row = m.dst_row;
	ret.src_col = m.dst_col;
	ret.dst_row = m.src_row;
	ret.dst_col = m.src_col;
	return ret;
      };

      // Now unwind the moves, if possible.
      Move rcur_move = Rev(cur_move);
      Position pos4 = pos3;
      if (!pos4.IsLegal(rcur_move))
	continue;
      pos4.ApplyMove(rcur_move);
      if ((*seen)[pos4] >= 4)
	continue;

      Move ropp_move = Rev(opp_move);
      Position pos5 = pos4;
      if (!pos5.IsLegal(ropp_move))
	continue;
      pos5.ApplyMove(ropp_move);
      if ((*seen)[pos5] >= 4)
	continue;
      // And this should now equal the original pos. Note that we are
      // a bit too conservative here because we don't even allow the
      // excursion to change en passant or castling state, even though
      // we know our game won't castle or capture en passant. So if we
      // fail to find excursions, this is one way to give us a bit
      // more flexibility.
      if (PositionEq{}(orig_pos, pos5)) {
	// Got one! Commit.
	printf("Excursion: %s %s  %s %s\n",
	       pos.ShortMoveString(cur_move).c_str(),
	       pos2.ShortMoveString(opp_move).c_str(),
	       pos3.ShortMoveString(rcur_move).c_str(),
	       pos4.ShortMoveString(ropp_move).c_str());

	(*seen)[pos2]++;
	(*seen)[pos3]++;
	(*seen)[pos4]++;
	(*seen)[pos5]++;
	*excursion = {cur_move, opp_move, rcur_move, ropp_move};
	return true;
      }
    }
  }

  return false;
}

// Odd excursions.
//
// Knights can't do this, because they always move to a square of the
// opposite color. So don't consider them. This does pose a problem
// for the first two moves of the game, since until the pawns are out,
// we only have knight moves and rook moves (supposing we never need
// to castle) between two adjacent squares (which has the same
// problem). Say that the rule was actually "both players have made 4
// moves w/o pawn or capture." Then at start we have nf3 (1w0b) nc6
// (1w1b) ng1 (2w1b) nb8 (2w2b) nf3 (3w2b) nc6 (3w3b) and now it is
// legal for white to play nf3 (4w3b -- note that only white has
// played 4, not black; rule is *both*) and so now black can play
// a critical move with e.g. a6. 
static bool MakeOddExcursion(const Position &orig_pos,
			     ArcFour *rc,
			     PositionMap<int> *seen,
			     vector<Move> *excursion) {
  // Here, both players need to play three moves. We'll
  // play them with the same piece each time.
  
  // If we've already visited this position 4 times, then this is
  // not possible.
  if ((*seen)[orig_pos] >= 4)
    return false;

  Position pos = orig_pos;
  vector<Move> cur_moves = pos.GetLegalMoves();
  Shuffle(rc, &cur_moves);
  for (const Move &cur_move : cur_moves) {
    const uint8 t = pos.MovePieceType(cur_move);
    if (t == Position::KNIGHT || t == Position::PAWN) continue;
    if (pos.IsCapturing(cur_move) || pos.IsCastling(cur_move)) continue;

    Position pos2 = pos;
    pos2.ApplyMove(cur_move);
    // Too many repeats?
    if ((*seen)[pos2] >= 4)
      continue;

    vector<Move> opp_moves = pos2.GetLegalMoves();
    Shuffle(rc, &opp_moves);
    for (const Move &opp_move : opp_moves) {
      const uint8 ot = pos2.MovePieceType(opp_move);
      if (ot == Position::KNIGHT || ot == Position::PAWN) continue;
      if (pos2.IsCapturing(opp_move) || pos2.IsCastling(opp_move)) continue;

      Position pos3 = pos2;
      pos3.ApplyMove(opp_move);
      // Too many repeats?
      if ((*seen)[pos3] >= 4)
	continue;

      // Both players have moved once. Now again, but only consider
      // the piece we moved the first time.

      // PERF: A version of GetLegalMoves that gave me the moves with
      // a particular source square would be much faster here.
      vector<Move> cur2_moves = pos3.GetLegalMoves();
      Shuffle(rc, &cur2_moves);
      for (const Move &cur2_move : cur2_moves) {
	if (cur2_move.src_row == cur_move.dst_row &&
	    cur2_move.src_col == cur_move.dst_col) {

	  Position pos4 = pos3;
	  pos4.ApplyMove(cur2_move);
	  if ((*seen)[pos4] >= 4)
	    continue;

	  vector<Move> opp2_moves = pos4.GetLegalMoves();
	  Shuffle(rc, &opp2_moves);
	  for (const Move &opp2_move : opp2_moves) {
	    if (opp2_move.src_row == opp_move.dst_row &&
		opp2_move.src_col == opp_move.dst_col) {

	      Position pos5 = pos4;
	      pos5.ApplyMove(opp2_move);
	      if ((*seen)[pos5] >= 4)
		continue;

	      // Now, both players have moved twice. Try moving back
	      // to the starting position.

	      // Close the triangle, by moving from the destination of
	      // move b to the source of move a.
	      auto Triangle = [](Move ma, Move mb) -> Move {
		  Move ret;
		  ret.src_row = mb.dst_row;
		  ret.src_col = mb.dst_col;
		  ret.dst_row = ma.src_row;
		  ret.dst_col = ma.src_col;
		  return ret;
		};
	      
	      // Now unwind the moves, if possible.
	      Move rcur_move = Triangle(cur_move, cur2_move);
	      Position pos6 = pos5;
	      if (!pos6.IsLegal(rcur_move))
		continue;
	      pos6.ApplyMove(rcur_move);
	      if ((*seen)[pos6] >= 4)
		continue;

	      Move ropp_move = Triangle(opp_move, opp2_move);
	      Position pos7 = pos6;
	      if (!pos7.IsLegal(ropp_move))
		continue;
	      pos7.ApplyMove(ropp_move);
	      if ((*seen)[pos7] >= 4)
		continue;
	      
	      // Like in the Even case.
	      if (PositionEq{}(orig_pos, pos7)) {
		// Got one! Commit.
		printf("Odd Excursion: %s %s  %s %s  %s %s\n",
		       pos.ShortMoveString(cur_move).c_str(),
		       pos2.ShortMoveString(opp_move).c_str(),
		       pos3.ShortMoveString(cur2_move).c_str(),
		       pos4.ShortMoveString(opp2_move).c_str(),
		       pos5.ShortMoveString(rcur_move).c_str(),
		       pos6.ShortMoveString(ropp_move).c_str());

		(*seen)[pos2]++;
		(*seen)[pos3]++;
		(*seen)[pos4]++;
		(*seen)[pos5]++;
		(*seen)[pos6]++;
		(*seen)[pos7]++;
		*excursion = {cur_move, opp_move,
			      cur2_move, opp2_move,
			      rcur_move, ropp_move};
		return true;
	      }
	    }
	  }
	}
      }
    }
  }
    
  // printf("No odd excursion...\n");
  return false;
}

// Then looking at a pair of critical moves A and B, we already have a
// sequence of moves (maybe empty) that transitions from the state
// after move A is executed to the state before B is executed. Our
// goal will be to expand this move list to exactly 75+74 in length,
// without repeating. We can do this by just repeatedly adding short
// sequences that are net no-ops somewhere within the existing
// sequence, probably.
static void Expand(Critical *crit) {
  ArcFour rc((string)"expand" + crit->start_pos.ToFEN(1, 2));

  // Target length of the move list.
  // XXX need to check that this is correct.
  static constexpr int TARGET_SIZE = 149;

  // Count of times we've seen each position. No need to consider
  // the position after the critical move, since it cannot repeat
  // any position within the critical section (due to being an
  // irreversible move).
  PositionMap<int> seen;

  // Also note some subtlety with the starting position: It does not
  // formally count as having "appeared," but we insert it anyway.
  // This just causes us our game to be a little more conservative.
  {
    Position pos = crit->start_pos;
    seen[pos]++;
    for (const Move &m : crit->moves) {
      pos.ApplyMove(m);
      seen[pos]++;
      CHECK(seen[pos] < 5);
    }
    // Sanity check.
    CHECK(PositionEq{}(pos, crit->end_pos));
  }

  enum TryExcursionResult {
    NEXT_LOOP,
    NOT_FOUND,
  };

  // Repeatedly look for a position where we can make an excursion
  // and return to the same position we're already in, without
  // repeating.
  while (crit->moves.size() < TARGET_SIZE) {
    int slack = TARGET_SIZE - crit->moves.size();
    // Shortest (non-empty) excursion is for each player to move
    // a piece and then move that piece back, which takes four
    // half-moves.
    if (slack < 4) {
      printf("Finished with slack of %d\n", slack);
      return;
    }
      
    auto TryExcursions = [crit, &rc, &seen](
	std::function<bool(const Position &,
			   ArcFour *,
			   PositionMap<int> *,
			   vector<Move> *)> MakeExcursion) {
      // Otherwise, try to insert an excursion.
      Position pos = crit->start_pos;
      for (int idx = 0; idx <= crit->moves.size(); idx++) {
	vector<Move> excursion;

	if (MakeExcursion(pos, &rc, &seen, &excursion)) {
	  vector<Move> newmoves;
	  newmoves.reserve(crit->moves.size() + excursion.size());
	  for (int i = 0; i < idx; i++) {
	    newmoves.push_back(crit->moves[i]);
	  }
	  for (const Move &em : excursion) newmoves.push_back(em);
	  for (int i = idx; i < crit->moves.size(); i++) {
	    newmoves.push_back(crit->moves[i]);
	  }
	  crit->moves = std::move(newmoves);

	  return NEXT_LOOP;
	}

	if (idx < crit->moves.size())
	  pos.ApplyMove(crit->moves[idx]);
      }
      return NOT_FOUND;
    };

    // TODO: we need to do at least one 3-move excursion if slack/2 is
    // odd.
    if ((slack / 2) & 1) {
      auto odd = TryExcursions(MakeOddExcursion);
      if (odd == NEXT_LOOP)
	continue;
      // If not found, fall through to Even.
    }

    auto even = TryExcursions(MakeEvenExcursion);
    if (even == NEXT_LOOP)
      continue;

    if (even == NOT_FOUND) {
      printf("Failed to find a place for an excursion :(\n");
      for (const auto &p : seen) {
	printf("\n%d times:\n%s\n", p.second, p.first.BoardString().c_str());
      }
      return;
    }

  }
  printf("Finished with slack of 0 [perfect!]\n");
}

static void MakeLongest() {
  vector<Critical> crits = MakeCritical();
  for (Critical &crit : crits) Expand(&crit);
  // Expand(&crits[0]);

  PositionMap<int> seen;

  int white_noncritical = 0, black_noncritical = 0;
  map<int, int> slack_histo;
  int total_slack = 0;

  Position pos;
  
  vector<Move> final_moves;
  // XXX more verification to do here, like 75-move counter

  for (const Critical &crit : crits) {
    CHECK(PositionEq{}(pos, crit.start_pos));
    for (const Move &m : crit.moves) {
      CHECK(pos.IsLegal(m));
      CHECK(!pos.IsPawnMove(m));
      CHECK(!pos.IsCapturing(m));
      if (pos.BlackMove()) {
	black_noncritical++;
      } else {
	white_noncritical++;
      }
      CHECK(black_noncritical < 75 || white_noncritical < 75);
      pos.ApplyMove(m);
      seen[pos]++;
      CHECK(seen[pos] < 5);
      final_moves.push_back(m);
    }
    CHECK(PositionEq{}(pos, crit.end_pos));
    CHECK(pos.IsLegal(crit.critical_move));
    CHECK(pos.IsPawnMove(crit.critical_move) ||
	  pos.IsCapturing(crit.critical_move));
    printf("Critical move with w %d, b %d\n",
	   white_noncritical, black_noncritical);
    int move_slack = 149 - (white_noncritical + black_noncritical);
    slack_histo[move_slack]++;
    total_slack += move_slack;
    
    white_noncritical = black_noncritical = 0;
    pos.ApplyMove(crit.critical_move);
    seen[pos]++;
    CHECK(seen[pos] < 5);
    final_moves.push_back(crit.critical_move);
  }

  string pgn = "[Event \"Slow game\"]\n\n";
  {
    int move_num = 1;
    Position p;
    for (const Move &m : final_moves) {
      if (!p.BlackMove()) {
	StringAppendF(&pgn, " %d. %s", move_num,
		      p.ShortMoveString(m).c_str());
      } else {
	StringAppendF(&pgn, " %s",
		      p.ShortMoveString(m).c_str());
	move_num++;
      }
      p.ApplyMove(m);
    }
  }
  Util::WriteFile("slow.pgn", pgn);
  
  for (const auto &p : slack_histo)
    printf("Slack %d: %d time(s)\n", p.first, p.second);

  printf("Total slack: %d\n"
	 "Final game is %d moves, ending with:\n%s\n",
	 total_slack,
	 (int)final_moves.size(),
	 pos.BoardString().c_str());
}

int main(int argc, char **argv) {
  MakeLongest();

  return 0;
}

/*
    "1. a3 Nc6 2. a4 h6 3. Nf3 h5 4. Ng5 h4 5. a5 e6 6. d3 Ne5 7. Bf4 Nc4 "
    "8. g3 b6 9. g4 b5 10. Bg3 Nb6 11. axb6 hxg3 12. b7 g2 13. b8=Q g1=Q "
    "14. Ra6 b4 15. c3 Rh3 16. c4 Re3 17. c5 Re5 18. Rb6 a6 19. Rb7 a5 "
    "20. Rb6 a4 21. Rb7 a3 22. Rb6 a2 23. Ra6 a1=Q 24. Ra3 bxa3 25. Nd2 a2 "
    "26. b3 Ra6 27. b4 Rb6 28. cxb6 Qb1 29. b7 a1=N 30. Qa7 c6 31. b8=N c5 "
    "32. b5 c4 33. b6 c3 34. d4 Rc5 35. dxc5 c2 36. h3 Ba6 37. b7 c1=Q "
    "38. Nc6 Qc4 39. Nxc4 Qb2 40. Nxb2 d6 41. b8=N d5 42. Qc2 d4 43. f3 d3 "
    "44. h4 d2+ 45. Kd1 Qh2 46. Qd3 Nb3 47. Nxd8 Bxd3 48. exd3 Na5 "
    "49. Kc2 d1=N+ 50. Kc1 e5 51. c6 e4 52. c7 e3 53. c8=N e2 54. Ndc6 Nxc6 "
    "55. h5 e1=N 56. h6 g6 57. h7 Qe2 58. Rh5 gxh5 59. h8=N h4 60. Ne4 h3 "
    "61. g5 h2 62. g6 h1=Q 63. g7 Nf6 64. g8=N Ng4 65. fxg4 Qexf1 "
    "66. Nxc6 Qxe4 67. Nxd1 f6 68. g5 f5 69. g6 f4 70. g7 Bb4 71. Nxb4 Qxb4 "
    "72. d4 Qa6 73. Qxa6 f3 74. d5 f2 75. d6 Qh4 76. Nh6 Qxh6+ 77. Kb1 Qxh8 "
    "78. g8=B Kd8 79. d7 Kc7 80. d8=N Qxg8 81. Qa1 Ng2 82. Nc6 Kxc6 "
    "83. Ne3 Qxc8 84. Nxg2 f1=B 85. Qh8 Kd5 86. Ka2 Bxg2 87. Qxc8 Bh3 "
    "88. Qxh3 Ke4 89. Qe3+ Kxe3";
*/
