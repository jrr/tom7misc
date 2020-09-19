
// See tom7.org/chess/longest.pdf for a lengthy description of this,
// in SIGBOVIK 2020's "Is this the longest chess game?"
//
// (By the way, the title of this paper has a subtle joke, which since
// nobody will get it I will explain here: In SIGBOVIK 2019 there were
// FIVE papers about chess but the editors did not put them together
// in a "Chess" track (too obvious!!). Nonetheless there WAS a "Chess"
// track, which contained five papers devoted to the equally
// competitive game of writing the shortest SIGBOVIK paper, with
// titles like "Is this the tiniest SIGBOVIK paper ever?"; "On the
// shortness of SIGBOVIK papers", etc. The title here is thus a nod to
// this. Also I am not totally sure that this is the longest chess
// game.)

#include <functional>
#include <memory>
#include <unordered_map>
#include <cstdint>
#include <vector>
#include <string>
#include <map>
#include <utility>
#include <set>

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

static constexpr bool VERBOSE = false;

// Can be either 75 or 50. 75 is correct by FIDE rules, but 50 can be used
// to compare with other solutions.
static constexpr int MOVE_RULE = 50;

// If true, keep randomly trying to expand until the slack is either 0 or 1.
static constexpr bool OPTIMIZE = true;

// Don't even allow threefold repetition!
static constexpr int POS_MAX = 10;

static bool PositionEqIgnoringCastlingEp(const Position &pos1,
					 const Position &pos2) {
  // Must be same player's move.
  if (pos1.BlackMove() != pos2.BlackMove())
    return false;

  // And pieces must be the same, but allowing ROOK == C_ROOK.
  for (int r = 0; r < 8; r++) {
    for (int c = 0; c < 8; c++) {
      uint8 p1 = pos1.PieceAt(r, c);
      uint8 p2 = pos2.PieceAt(r, c);
      if ((p1 & Position::COLOR_MASK) !=
	  (p2 & Position::COLOR_MASK)) return false;
      uint8 t1 = p1 & Position::TYPE_MASK;
      uint8 t2 = p2 & Position::TYPE_MASK;
      if (t1 == Position::C_ROOK) t1 = Position::ROOK;
      if (t2 == Position::C_ROOK) t2 = Position::ROOK;
      if (t1 != t2) return false;
    }
  }
  return true;
}

// (Note: The paper linked above has a more careful description of the
// strategy I took; the following is my original notes for posterity.)
//
// Generate a "proof game" of a longest possible chess game.
// Chess can't go forever even if neither player is interested
// in a draw; the following conditions automatically cause
// a draw:
//  1. The same position is repeated 5 times. [1]
//  2. 75 half-moves are played by each player without a pawn 
//     move or capture.
//  3. There is insufficient material for even a "helpmate" (for
//     example, two kings). [Actually this is not quite right;
//     see the section on "dead positions" in the paper.]
//
// ([1] Actually really? The rules say "the same position has
// appeared, as in 9.2b, for at least five consecutive alternate moves
// by each player.") I think this is pretty weird, but perhaps
// "alternate" means a pair like Nf3 Ng1. The "consecutive" requirement
// here is especially surprising, since of course there are always
// intervening positions, and the threefold repetion claim does not
// require them to be consecutive. Anyway: We'll just produce a game
// where no position appears 5 times, which would also mean that
// "consecutive alternate" would not apply, whatever it is. Note "consecutive"
// seems to be gone from the rules in 2018. It's just
// "... the same position has appeared, as in 9.2.2 at least five times.")
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
// move--but see the note at the top of this file), or mate if we like.

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
// pawn without one of them moving out of the column.
// Just doubling up pawns into pairs, with white on even files and black
// on odd (or vice versa), will clear all of them. (The previous thought
// here about capturing "behind" a pawn was not necessary/correct.)
// This is 4 captures for each side to free all the pawns.
// That gives us (30 + 16*6 -
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

// Split into critical segments. Critical moves are the moments where
// we either move a pawn or capture a piece (and sometimes both; see
// above). (Checkmate is also allowed as a critical move, since if the
// 75th move by both players is checkmate, the game ends in mate, not
// a draw.) We don't need to care about what these are. We just want
// to put the board in exactly the position before the move, and then
// execute it. We don't need to worry about repeating a position that
// occurred before or after a critical move, because this is
// impossible. Therefore, we'll be able to work on each critical
// section independently.
struct Critical {
  // The position before executing any moves.
  Position start_pos;
  // Moves, not including the critical one.
  // No captures or pawn moves are allowed here, nor n-fold repetition.
  vector<Move> moves;
  // The position immediately before executing the critical move.
  Position end_pos;
  // The critical move, which will be a capture or pawn move (or mate).
  Move critical_move;
};


static vector<Critical> MakeCritical() {
  // Made manually.
  // 
  // To minimize the amount of slack we incur later, we should make
  // critical moves consecutively by the same player as much as possible.
  // Each time we switch back and forth, it costs us one move.
  // 
  // Also looks like the first pawn move should be made by black with
  // white having moved one knight to an opposite color. So the first
  // phase of critical moves should be made by black.
#if 0
  string slowgame_pgn = R"(
    1. Nf3 e6 2. Rg1 e5 3. Rh1 e4 4. Rg1 g6 5. Rh1 g5 6. Rg1 g4 
    7. Rh1 c6 8. Rg1 c5 9. Rh1 c4 10. Rg1 a6 11. Rh1 a5 12. Rg1 a4 
    13. Rh1 Ra7 14. Rg1 Na6 15. Rh1 Nc7 16. Rg1 Bg7 17. Rh1 Qf6 
    18. Rg1 Qg6 19. Rh1 Ra5 20. Rg1 Rb5 21. Rh1 Rb3 22. Rg1 Nf6 
    23. Rh1 Nh5 24. Rg1 Nf4 25. Rh1 Nh3 26. Rg1 Bf8 27. Nd4 Qf5 
    28. Rh1 Qf3 29. Rg1 Rg8 30. Rh1 Rg5 31. Nb5 Rd5 32. Na7 Rdd3 
    33. Nc6 b6 34. Na5 bxa5 35. axb3 Ne6 36. cxd3 Nc7 37. exf3 Ne6 
    38. gxh3 Ng7 39. b4 Ne6 40. b5 Ng7 41. b6 Ne6 42. b7 Ng7 
    43. b8=R Ne6 44. Rb5 Ng7 45. b3 Ne6 46. b4 Ng7 47. Rg5 Ne6 
    48. b5 Ng7 49. b6 Ne6 50. b7 Ng7 51. b8=B Ne6 52. Be5 Nc7 
    53. Ba3 Ne6 54. Bxf8 Ng7 55. Nc3 Ne6 56. Nd5 Ng7 57. Nb6 Ne6 
    58. Nxc8 Ng7 59. Bfxg7 Kd8 60. Rb1 Ke8 61. Rb5 Kd8 62. Rc5 Ke8 
    63. Qb3 Kd8 64. Be2 Ke8 65. Bd1 Kd8 66. Bc2 d6 67. Qb8 dxc5 
    68. Bb3 f6 69. Bc2 fxe5 70. Bb3 h6 71. Bc2 hxg5 72. Bb3 a3 
    73. Bc2 a2 74. Bb3 a1=N 75. Bh6 Nxb3 76. Bg7 a4 77. Bh6 a3 
    78. Bg7 a2 79. Bh6 a1=N 80. Bg7 Na5 81. Qb7 Nxb7 82. Bh6 c3 
    83. Bg7 c2 84. Bh6 c1=N 85. Bg7 c4 86. Bh6 c3 87. Bg7 c2 
    88. Bh6 Ncb3 89. Bg7 c1=N 90. Bf8 N3a5 91. Kd1 Na2 92. Bb4 Nxb4 
    93. Kc1 Nd5 94. Kb2 Nf4 95. Ka2 Ng2 96. Re1 Nxe1 97. Ka3 Ng2 
    98. Ka4 e3 99. Ka3 e2 100. Ka4 e1=N 101. Ka3 e4 102. Ka4 Nec2 
    103. Kb5 Nce3 104. Ka4 Nf5 105. Kb4 Nc4 106. Kb5 Ne5 107. Ka4 Nbd6 
    108. Ka5 Nxc8 109. Ka4 Nc2 110. Ka5 Ncd4 111. Ka4 Ne6 112. Ka5 e3 
    113. Ka4 e2 114. Ka5 e1=R 115. Ka4 Ngh4 116. Ka5 g3 117. Ka4 Nhg6 
    118. Ka5 g4 119. Ka4 g2 120. Ka5 g1=R 121. Ka4 g3 122. Ka5 Rgf1 
    123. Ka4 g2 124. Ka5 g1=R 125. h4 Kc7 126. h5 Kb8 127. h6 Ka8 
    128. h7 Kb8 129. h8=B Ka8 130. Bxe5 Rg3 131. Bxg3 Ne5 132. Bxe5 Neg7 
    133. Bxg7 Nd4 134. Bxd4 Rg1 135. f4 Ref1 136. f3 Re1 137. f5 Ref1 
    138. f6 Re1 139. f7 Ref1 140. f8=R Re1 141. Re8 Ref1 142. Bxg1 Re1 
    143. f4 Rf1 144. f5 Re1 145. f6 Rf1 146. f7 Re1 147. f8=N Re2 
    148. h3 Re1 149. h4 Re2 150. h5 Re1 151. h6 Re2 152. h7 Re1 
    153. h8=Q Re2 154. d4 Re1 155. d5 Re2 156. d6 Re1 157. d7 Re2 
    158. d8=B Re3 159. d3 Re2 160. Bh4 Re3 161. d4 Re2 162. d5 Re3 
    163. d6 Re2 164. d7 Re3 165. d8=N Ka7 166. Ka4 Ka6 167. Kb4 Ne7 
    168. Kc4 Rg3 169. Bxg3 Ng8 170. Qxg8 Ka5 171. Kd3 Ka6 172. Ke2 Ka5 
    173. Kf2 Ka6 174. Kg2 Ka5 175. Bc5 Kb5 176. Bh2 Kxc5 177. Re6 Kd5 
    178. Nh7 Kc5 179. Rd6 Kb5 180. Ra6 Kxa6 181. Nb7 Kxb7 182. Qa2 Kc6 
    183. Bd6 Kxd6 184. Nf6 Ke7 185. Nd7 Kxd7 186. Kf3 Ke7 187. Kf4 Kf6 
    188. Qf2 Kg6 189. Qg2+ Kh5 190. Ke3 Kh6 191. Qg7+ Kxg7
  )";
#endif

#if 0
  // This one is close, but black mates white on their 75th move, with
  // white having played 74 non-critical moves. So we have 1 slack.
  string slowgame_pgn = R"(
    1. Nf3 b6 2. Nc3 g6 3. Ne5 g5 4. Nd5 b5
    5. Nc4 bxc4 6. Nf4 gxf4 7. Rg1 c6 8. Rh1 c5
    9. Rg1 f6 10. Rh1 f5 11. b3 Nc6 12. b4 Nd4
    13. b5 Nb3 14. cxb3 Bb7 15. a3 Rc8 16. a4 Rc6
    17. a5 Rd6 18. a6 Re6 19. axb7 Rd6 20. b8=N Re6
    21. b6 Nh6 22. b7 Ng4 23. Nc6 Bh6 24. b8=Q Kf8
    25. b4 Ke8 26. b5 Kf8 27. Qe5 Ke8 28. b6 Kf8
    29. b7 Ke8 30. b8=N Bg5 31. h3 Bh4 32. hxg4 Bg3
    33. fxg3 Qc8 34. g5 Qa6 35. Nb4 Kd8 36. Qe3 Kc7
    37. g6 Kb6 38. g7 Rf6 39. g8=Q Re6 40. g4 Rf6
    41. g5 Re6 42. g6 Rf6 43. g7 Re6 44. Qf8 Rf6
    45. g8=N Rd6 46. Nf6 Qb5 47. Nh5 Rg8 48. g3 Rgg6
    49. g4 Rge6 50. g5 Rf6 51. g6 Rc6 52. g7 Qa6
    53. g8=N Qa5 54. d3 Qa6 55. d4 Qa5 56. d5 Qa6
    57. dxc6 Qa5 58. c7 Qa6 59. c8=R Qa4 60. Qh3 Qa6
    61. e3 Qa5 62. e4 Qa6 63. e5 Qa5 64. exf6 Qa6
    65. Qg7 Qa5 66. f7 Qa6 67. Re8 Qa5 68. f8=R Qa6
    69. Qdg4 Qa5 70. Nc6 Qa6 71. Ne5 Qa5 72. Nf3 Qa6
    73. Nd3 Qa5+ 74. Ke2 Qa6 75. Nf2 Qa5 76. Qh8 Qa3
    77. Nd1 Qa2+ 78. Nd2 Qa3 79. Kf2 Qa2 80. Kg1 Qa3
    81. Rxa3 f3 82. Kh2 f2 83. Bd3 f1=R 84. Qf4 Rxf4
    85. Ng7 h6 86. Be4 Rxe4 87. Ne6 f4 88. Nf6 f3
    89. Ng8 f2 90. Nf6 f1=B 91. Rd3 Bxd3 92. Bb2 a6
    93. Bc3 a5 94. Bb2 a4 95. Bc3 a3 96. Bb2 a2
    97. Bc3 a1=R 98. Ba5+ Rxa5 99. Nf4 c3 100. Ne2 c2
    101. Nf4 c1=N 102. Ne2 c4 103. Ng4 Nxe2 104. Nge3 c3
    105. Nf5 c2 106. Nfe3 c1=R 107. Re1 h5 108. Qf3 h4
    109. Kg2 h3+ 110. Kf2 h2 111. Qfh5 h1=R 112. Ng4 Rxg4
    113. Nc4+ Bxc4 114. Nb2 d6 115. Rg1 d5 116. Rg2 d4
    117. Kf3 d3 118. Kf2 d2 119. Qh2 d1=R 120. Kf3 Nc3
    121. Kf2 Rg8 122. Kf3 e6 123. Kf4 e5+ 124. Kf5 e4+
    125. Kf6 e3 126. Rg3 e2 127. Rg2 e1=Q 128. Nd3 Rxd3
    129. Rg1 Rgxg1 130. Q8h3 Rxh3 131. Ra8 Rxa8 132. Rb8+ Rxb8
    133. Qe2 Nxe2 134. Ke5 Be6 135. Kxe6 Nc3+ 136. Kd6 Nd5
    137. Kxd5 Qh4 138. Ke5 Ka5 139. Kd5 Rc6 140. Kxc6 Rb7
    141. Kxb7 Rc3 142. Kb8 Rc7 143. Kxc7 Rd1 144. Kc6 Rd7
    145. Kxd7 Re1 146. Kd6 Re5 147. Kxe5 Kb6 148. Kd5 Qh2
    149. Ke6 Qh4 150. Ke5 Kc6 151. Ke6 Kc7 152. Kf7 Kd6
    153. Ke8 Qe7#
  )";
#endif

  // (from the paper)
  // Good! Mate with 3 slack! I think this is the longest possible!
  string slowgame_pgn = R"(
    1. Nf3 b6 2. Nc3 g6 3. Ne5 g5 4. Nd5 b5
    5. Nc4 bxc4 6. Nf4 gxf4 7. Rg1 c6 8. Rh1 c5
    9. Rg1 f6 10. Rh1 f5 11. b3 Nc6 12. b4 Nd4
    13. b5 Nb3 14. cxb3 Bb7 15. a3 Rc8 16. a4 Rc6
    17. a5 Rd6 18. a6 Re6 19. axb7 Rd6 20. b8=N Re6
    21. b6 Nh6 22. b7 Ng4 23. Nc6 Bh6 24. b8=Q Kf8
    25. b4 Ke8 26. b5 Kf8 27. Qe5 Ke8 28. b6 Kf8
    29. b7 Ke8 30. b8=N Bg5 31. h3 Bh4 32. hxg4 Bg3
    33. fxg3 Qc8 34. g5 Qa6 35. Nb4 Kd8 36. Qe3 Kc7
    37. g6 Kb6 38. g7 Rf6 39. g8=Q Re6 40. g4 Rf6
    41. g5 Re6 42. g6 Rf6 43. g7 Re6 44. Qf8 Rf6
    45. g8=N Rd6 46. Nf6 Qb5 47. Nh5 Rg8 48. g3 Rgg6
    49. g4 Rge6 50. g5 Rf6 51. g6 Rc6 52. g7 Qa6
    53. g8=N Qa5 54. d3 Qa6 55. d4 Qa5 56. d5 Qa6
    57. dxc6 Qa5 58. c7 Qa6 59. c8=R Qa4 60. Qh3 Qa6
    61. e3 Qa5 62. e4 Qa6 63. e5 Qa5 64. exf6 Qa6
    65. Qg7 Qa5 66. f7 Qa6 67. Re8 Qa5 68. f8=R Qa6
    69. Qdg4 Qa5 70. Nc6 Qa6 71. Ne5 Qa5 72. Nf3 Qa6
    73. Nd3 Qa5+ 74. Ke2 Qa6 75. Nf2 Qa5 76. Qh8 Qa3
    77. Nd1 Qa2+ 78. Nd2 Qa3 79. Kf2 Qa2 80. Kg1 Qa3
    81. Rxa3 f3 82. Kh2 f2 83. Bd3 f1=R 84. Qf4 Rxf4
    85. Ng7 h6 86. Be4 Rxe4 87. Ne6 f4 88. Nf6 f3
    89. Ng8 f2 90. Nf6 f1=B 91. Rd3 Bxd3 92. Bb2 a6
    93. Bc3 a5 94. Bb2 a4 95. Bc3 a3 96. Bb2 a2
    97. Bc3 a1=R 98. Ba5+ Rxa5 99. Nf4 c3 100. Ne2 c2
    101. Nf4 c1=N 102. Ne2 c4 103. Ng4 Nxe2 104. Nge3 c3
    105. Nf5 c2 106. Nfe3 c1=R 107. Re1 h5 108. Qf3 h4
    109. Kg2 h3+ 110. Kf2 h2 111. Qfh5 h1=R 112. Ng4 Rxg4
    113. Nc4+ Bxc4 114. Nb2 d6 115. Rg1 d5 116. Rg2 d4
    117. Kf3 d3 118. Kf2 d2 119. Qh2 d1=R 120. Kf3 Nc3
    121. Kf2 Rg8 122. Kf3 e6 123. Kf4 e5+ 124. Kf5 e4+
    125. Kf6 e3 126. Rg3 e2 127. Rg2 e1=Q 128. Nd3 Rxd3
    129. Rg1 Rgxg1 130. Q8h3 Rxh3 131. Ra8 Rxa8 132. Rb8+ Rxb8
    133. Qxb8+ Ka5 134. Qg3 Bf7 135. Qxe1 Rg5 136. Qxc1 Ka6
    137. Qxc3 Kb6 138. Qxh3 Kc5 139. Kxf7 Kc6 140. Qxh1+ Kd7
    141. Kf6 Ke8 142. Qg2 Rg8 143. Ke6 Rg7 144. Qxg7 Kd8
    145. Qd7#
)";

  // XXX
  slowgame_pgn = R"(
1. Nf3 Nf6 2. Nc3 Nc6 3. Nd4 Nd5 4. Ne4 b6
5. Rb1 Ncb4 6. Rg1 g6 7. Ng3 Nf6 8. Ngf5 Nbd5
9. Ne3 Nf4 10. Nc4 Ne4 11. Ne6 Nd5 12. Nf4 Nef6
13. Rh1 b5 14. Ra1 bxc4 15. Rg1 g5 16. Rb1 gxf4
17. Rh1 Nh5 18. Ra1 c6 19. Rb1 c5 20. Ra1 f6
21. Rg1 f5 22. b3
)";
  
  #if 0
  // Draw game, just 3 slack! But this game ends with the
  // last move being forced, which means that it is actually
  // over prior to the forced move.
  string slowgame_pgn = R"(
    1. Nf3 b6 2. Nc3 g6 3. Ne5 g5 4. Nd5 b5
    5. Nc4 bxc4 6. Nf4 gxf4 7. Rg1 c6 8. Rh1 c5
    9. Rg1 f6 10. Rh1 f5 11. b3 Nc6 12. b4 Nd4
    13. b5 Nb3 14. cxb3 Bb7 15. a3 Rc8 16. a4 Rc6
    17. a5 Rd6 18. a6 Re6 19. axb7 Rd6 20. b8=N Re6
    21. b6 Nh6 22. b7 Ng4 23. Nc6 Bh6 24. b8=Q Kf8
    25. b4 Ke8 26. b5 Kf8 27. Qe5 Ke8 28. b6 Kf8
    29. b7 Ke8 30. b8=N Bg5 31. h3 Bh4 32. hxg4 Bg3
    33. fxg3 Qc8 34. g5 Qa6 35. Nb4 Kd8 36. Qe3 Kc7
    37. g6 Kb6 38. g7 Rf6 39. g8=Q Re6 40. g4 Rf6
    41. g5 Re6 42. g6 Rf6 43. g7 Re6 44. Qf8 Rf6
    45. g8=N Rd6 46. Nf6 Qb5 47. Nh5 Rg8 48. g3 Rgg6
    49. g4 Rge6 50. g5 Rf6 51. g6 Rc6 52. g7 Qa6
    53. g8=N Qa5 54. d3 Qa6 55. d4 Qa5 56. d5 Qa6
    57. dxc6 Qa5 58. c7 Qa6 59. c8=R Qa4 60. Qh3 Qa6
    61. e3 Qa5 62. e4 Qa6 63. e5 Qa5 64. exf6 Qa6
    65. Qg7 Qa5 66. f7 Qa6 67. Re8 Qa5 68. f8=R Qa6
    69. Qdg4 Qa5 70. Nc6 Qa6 71. Ne5 Qa5 72. Nf3 Qa6
    73. Nd3 Qa5+ 74. Ke2 Qa6 75. Nf2 Qa5 76. Qh8 Qa3
    77. Nd1 Qa2+ 78. Nd2 Qa3 79. Kf2 Qa2 80. Kg1 Qa3
    81. Rxa3 f3 82. Kh2 f2 83. Bd3 f1=R 84. Qf4 Rxf4
    85. Ng7 h6 86. Be4 Rxe4 87. Ne6 f4 88. Nf6 f3
    89. Ng8 f2 90. Nf6 f1=B 91. Rd3 Bxd3 92. Bb2 a6
    93. Bc3 a5 94. Bb2 a4 95. Bc3 a3 96. Bb2 a2
    97. Bc3 a1=R 98. Ba5+ Rxa5 99. Nf4 c3 100. Ne2 c2
    101. Nf4 c1=N 102. Ne2 c4 103. Ng4 Nxe2 104. Nge3 c3
    105. Nf5 c2 106. Nfe3 c1=R 107. Re1 h5 108. Qf3 h4
    109. Kg2 h3+ 110. Kf2 h2 111. Qfh5 h1=R 112. Ng4 Rxg4
    113. Nc4+ Bxc4 114. Nb2 d6 115. Rg1 d5 116. Rg2 d4
    117. Kf3 d3 118. Kf2 d2 119. Qh2 d1=R 120. Kf3 Nc3
    121. Kf2 Rg8 122. Kf3 e6 123. Kf4 e5+ 124. Kf5 e4+
    125. Kf6 e3 126. Rg3 e2 127. Rg2 e1=Q 128. Nd3 Rxd3
    129. Rg1 Rgxg1 130. Q8h3 Rxh3 131. Ra8 Rxa8 132. Rb8+ Rxb8
    133. Qe2 Nxe2 134. Ke5 Be6 135. Kxe6 Nc3+ 136. Kd6 Nd5
    137. Kxd5 Qh4 138. Ke5 Ka5 139. Kd5 Rc6 140. Kxc6 Rb7
    141. Kxb7 Rc3 142. Kb8 Rc7 143. Kxc7 Rd1 144. Kc6 Rd7
    145. Kxd7 Re1 146. Kd6 Re5 147. Kxe5 Kb4 148. Kd5 Kc3
    149. Kc5 Qd8 150. Kc6 Kc4 151. Kb7 Kc5 152. Ka7 Kb4
    153. Kb7 Qh4 154. Ka7 Qd8 155. Ka6 Kc5 156. Kb7 Kd6
    157. Ka7 Kc7 158. Ka6 Qa8+ 159. Kb5 Qc6+ 160. Ka5 Qc5+
    161. Ka4 Qb4+ 162. Kxb4
)";
#endif
  
  vector<Move> moves;
  {
    vector<PGN::Move> movestrings;
    CHECK(PGN::ParseMoves(slowgame_pgn, &movestrings));
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
  bool has_checkmate = false;
  for (const Move &m : moves) {
    const bool is_pawn = pos.IsPawnMove(m);
    const bool is_capturing = pos.IsCapturing(m);
    const bool is_checkmate = pos.MoveExcursion(m, [&](){
	return pos.IsMated();
      });
    if (is_pawn) pawn_moves++;
    if (is_capturing) captures++;
    if (is_checkmate) has_checkmate = true;
    
    const bool is_critical = is_pawn || is_capturing || is_checkmate;
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
  #if 0
  // This particular game ends with a draw from insufficient material.
  CHECK(!pos.IsInCheck());
  CHECK(!pos.IsMated());
  #endif
  
  const char *maybe_checkmate = "";
  if (has_checkmate) maybe_checkmate = "(ends in checkmate)\n";
  
  printf("Slow game: %d moves. %d pawn moves, %d captures\n"
	 "%s"
	 "Critical sections: %d\n",
	 (int)moves.size(),
	 pawn_moves, captures,
	 maybe_checkmate,
	 (int)crits.size());

  // This should be the case for a maximal input game.
  // CHECK(crits.size() == 118) << crits.size();
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
  if ((*seen)[orig_pos] >= POS_MAX)
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
    if ((*seen)[pos2] >= POS_MAX)
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
      if ((*seen)[pos3] >= POS_MAX)
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
      if ((*seen)[pos4] >= POS_MAX)
	continue;

      Move ropp_move = Rev(opp_move);
      Position pos5 = pos4;
      if (!pos5.IsLegal(ropp_move))
	continue;
      pos5.ApplyMove(ropp_move);
      if ((*seen)[pos5] >= POS_MAX)
	continue;
      // And this should now equal the original pos. Note that we
      // ignore castling and e.p. state when comparing positions, since
      // we know that the input does not do this. It gives us a bit
      // more flexibility, especially in the early game where we may only
      // be able to move rooks.
      if (PositionEqIgnoringCastlingEp(orig_pos, pos5)) {
	// Got one! Commit.
	if (VERBOSE)
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
  if ((*seen)[orig_pos] >= POS_MAX)
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
    if ((*seen)[pos2] >= POS_MAX)
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
      if ((*seen)[pos3] >= POS_MAX)
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
	  if ((*seen)[pos4] >= POS_MAX)
	    continue;

	  vector<Move> opp2_moves = pos4.GetLegalMoves();
	  Shuffle(rc, &opp2_moves);
	  for (const Move &opp2_move : opp2_moves) {
	    if (opp2_move.src_row == opp_move.dst_row &&
		opp2_move.src_col == opp_move.dst_col) {

	      Position pos5 = pos4;
	      pos5.ApplyMove(opp2_move);
	      if ((*seen)[pos5] >= POS_MAX)
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
	      if ((*seen)[pos6] >= POS_MAX)
		continue;

	      Move ropp_move = Triangle(opp_move, opp2_move);
	      Position pos7 = pos6;
	      if (!pos7.IsLegal(ropp_move))
		continue;
	      pos7.ApplyMove(ropp_move);
	      if ((*seen)[pos7] >= POS_MAX)
		continue;
	      
	      // Like in the Even case.
	      if (PositionEqIgnoringCastlingEp(orig_pos, pos7)) {
		// Got one! Commit.
		if (VERBOSE)
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
//
// Returns the amount of slack.
static int Expand(Critical *crit, ArcFour *rc) {
  // Target length of the move list.
  // The ideal amount here is 75+74 moves (if black will make the critical
  // move), or 74+75 moves (if white will); this is the maximum number
  // without forcing a draw.
  static constexpr int TARGET_SIZE = (MOVE_RULE * 2) - 1;

  // Count of times we've seen each position. No need to consider
  // the position after the critical move, since it cannot repeat
  // any position within the critical section (due to being an
  // irreversible move).
  PositionMap<int> seen;

  // Also note some subtlety with the starting position: It does not
  // formally count as having "appeared," but we insert it anyway.
  // This just causes us our game to be a little more conservative
  // about what positions it's willing to visit, but it will have the
  // same final length.
  {
    Position pos = crit->start_pos;
    seen[pos]++;
    for (const Move &m : crit->moves) {
      pos.ApplyMove(m);
      seen[pos]++;
      CHECK(seen[pos] < 5);
    }
    // Sanity check.
    CHECK(PositionEqIgnoringCastlingEp(pos, crit->end_pos));
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
      return slack;
    }
      
    auto TryExcursions = [crit, rc, &seen](
	std::function<bool(const Position &,
			   ArcFour *,
			   PositionMap<int> *,
			   vector<Move> *)> MakeExcursion) {
      // Otherwise, try to insert an excursion.
      Position pos = crit->start_pos;
      for (int idx = 0; idx <= crit->moves.size(); idx++) {
	vector<Move> excursion;

	if (MakeExcursion(pos, rc, &seen, &excursion)) {
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

    // We need to do at least one 3-move excursion if slack/2 is odd.
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
      return slack;
    }

  }
  printf("Finished with slack of 0 [perfect!]\n");
  return 0;
}

static string CriticalString(const Critical &crit) {
  string moves, crit_move, end_pos;
  {
    Position p = crit.start_pos;
    for (const Move m : crit.moves) {
      moves += p.ShortMoveString(m) + " ";
      p.ApplyMove(m);
    }
    crit_move = p.ShortMoveString(crit.critical_move);
  }
  
  return StringPrintf("ExpandMulti. Critical:\n"
		      "== start ==\n"
		      "%s"
		      "== moves ==\n"
		      "%s\n"
		      "== end ==\n"
		      "%s"
		      "== critical ==\n"
		      "%s\n",
		      crit.start_pos.BoardString().c_str(),
		      moves.c_str(),
		      crit.end_pos.BoardString().c_str(),
		      crit_move.c_str());
}

// If OPTIMIZE is true, then keep calling Expand until we get a slack of 1.
// This may not terminate for certain input PGNs.
static void ExpandMulti(Critical *crit) {
  ArcFour rc((string)"expand" + crit->start_pos.ToFEN(1, 2));

  printf("%s", CriticalString(*crit).c_str());
  
  Critical orig = *crit;
  for (int tries = 0; true; tries++) {
    int slack = Expand(crit, &rc);
    // Good!
    if (slack <= 1 || !OPTIMIZE)
      return;

    printf("Got with slack %d:\n%s\n", slack,
	   CriticalString(*crit).c_str());
    
    printf("Slack %d. Try again (%d)...\n", slack, tries);
    // Replace and try again.
    *crit = orig;
  }
}


static void MakeLongest() {
  vector<Critical> crits = MakeCritical();
  for (Critical &crit : crits) ExpandMulti(&crit);
  // Expand(&crits[0]);

  PositionMap<int> seen;

  int white_noncritical = 0, black_noncritical = 0;
  map<int, int> slack_histo;
  int total_slack = 0;

  Position pos;
  
  vector<Move> final_moves;
  // Indices in final_moves that are critical moves.
  std::set<int> critical_indices;
  
  for (const Critical &crit : crits) {
    CHECK(PositionEqIgnoringCastlingEp(pos, crit.start_pos));
    for (const Move &m : crit.moves) {
      CHECK(pos.IsLegal(m));
      CHECK(!pos.IsPawnMove(m));
      CHECK(!pos.IsCapturing(m));
      if (pos.BlackMove()) {
	black_noncritical++;
      } else {
	white_noncritical++;
      }
      CHECK(black_noncritical < MOVE_RULE || white_noncritical < MOVE_RULE);
      pos.ApplyMove(m);
      seen[pos]++;
      CHECK(seen[pos] < 5);
      final_moves.push_back(m);
    }
    CHECK(PositionEqIgnoringCastlingEp(pos, crit.end_pos));
    CHECK(pos.IsLegal(crit.critical_move));

    // Check that the critical move is indeed critical, which
    // licenses us to reset the noncritical counters.
    const bool is_pawn_move = pos.IsPawnMove(crit.critical_move);
    const bool is_capturing = pos.IsCapturing(crit.critical_move);
    const bool is_checkmate = pos.MoveExcursion(crit.critical_move,
						[&](){
						  return pos.IsMated();
						});

    CHECK(is_pawn_move || is_capturing || is_checkmate);
    int move_slack = (MOVE_RULE * 2 - 1) -
        (white_noncritical + black_noncritical);
    printf("Critical move (%s%s%s) with w %d, b %d",
	   (is_pawn_move ? "p" : ""),
	   (is_capturing ? "x" : ""),
	   (is_checkmate ? "++" : ""),
	   white_noncritical, black_noncritical);
    if (move_slack > 0) printf(" (slack %d)\n", move_slack);
    else printf("\n");
    slack_histo[move_slack]++;
    total_slack += move_slack;
    
    white_noncritical = black_noncritical = 0;
    pos.ApplyMove(crit.critical_move);
    seen[pos]++;
    // (Maybe could hard-code 5 here? That's the rule!)
    CHECK(seen[pos] < (POS_MAX + 1));
    critical_indices.insert((int)final_moves.size());
    final_moves.push_back(crit.critical_move);
  }

  {
    string pgn =
      "[Event \"Is this the longest Chess game?\"]\n"
      "[Site \"SIGBOVIK 2020\"]\n"
      "[White \"Dr. Tom Murphy VII Ph.D.\"]\n"
      "[Black \"Dr. Tom Murphy VII Ph.D.\"]\n"
      "[Result \"1-0\"]\n"
      "\n";
    int move_num = 1;
    Position p;
    for (const Move &m : final_moves) {
      string ms = p.ShortMoveString(m) +
	p.PGNMoveSuffix(m);
      if (move_num == 1786) {
	printf("Board:\n%s\n%s\nMove: %s\n",
	       p.BoardString().c_str(),
	       p.ToFEN(1, 1).c_str(),
	       ms.c_str());
      }
      
      if (!p.BlackMove()) {
	StringAppendF(&pgn, " %d. %s", move_num,
		      ms.c_str());
      } else {
	StringAppendF(&pgn, " %s",
		      ms.c_str());
	move_num++;
      }
      p.ApplyMove(m);
    }
    Util::WriteFile("longest.pgn", pgn);
  }

  {
    // was 12 pages, 427176 bytes
    
    string tex;
    int move_num = 1;
    Position p;
    for (int i = 0; i < final_moves.size(); i++) {
      const Move &m = final_moves[i];
      const bool is_critical =
	critical_indices.find(i) != critical_indices.end();
      string ms = p.ShortMoveString(m) + p.PGNMoveSuffix(m);
      if (true) {
	bool black = p.BlackMove();
	string rest = ms.substr(1);
	switch (ms[0]) {
	case 'Q':
	  ms = (string)(black ? "\\Queen " : "\\queen ") + rest;
	  break;
	case 'K':
	  ms = (string)(black ? "\\King " : "\\king ") + rest;
	  break;
	case 'N':
	  ms = (string)(black ? "\\Knight " : "\\knight ") + rest;
	  break;
	case 'R':
	  ms = (string)(black ? "\\Rook " : "\\rook ") + rest;
	  break;
	case 'B':
	  ms = (string)(black ? "\\Bishop " : "\\bishop ") + rest;
	  break;
	}

      }
      if (!p.BlackMove()) {
	StringAppendF(&tex, " %d. ", move_num);
      } else {
	StringAppendF(&tex, " ");
	move_num++;
      }

      if (is_critical) StringAppendF(&tex, "{\\scriptsize\\bf[");
      StringAppendF(&tex, "%s", ms.c_str());
      if (is_critical) StringAppendF(&tex, "]}");

      if (i % 10 == 0) StringAppendF(&tex, "\n");
      
      p.ApplyMove(m);
    }
    Util::WriteFile("papers/slow.tex", tex);
  }
  
  if (false) {
    int move_num = 1;
    Position p;
    int white_noncritical = 0, black_noncritical = 0;
    for (int i = 0; i < final_moves.size(); i++) {
      const Move &m = final_moves[i];
      const bool is_pawn_move = p.IsPawnMove(m);
      const bool is_capturing = p.IsCapturing(m);
      const bool critical = is_pawn_move || is_capturing;
      if (critical) white_noncritical = black_noncritical = 0;
      
      if (i % 10 == 0) {
	printf("\n%s\n%s\n", p.BoardString().c_str(),
	       p.ToFEN(1, 1).c_str());
      }
      
      if (!p.BlackMove()) {
	if (!critical) white_noncritical++;
	printf(" %d. %s [%d %d]", move_num,
	       p.ShortMoveString(m).c_str(),
	       white_noncritical, black_noncritical);
      } else {
	if (!critical) black_noncritical++;
	printf(" %s [%d %d]",
	       p.ShortMoveString(m).c_str(),
	       white_noncritical, black_noncritical);
	move_num++;
      }
      p.ApplyMove(m);
    }
  }
  
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
