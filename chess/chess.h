
#ifndef __CHESS_H
#define __CHESS_H

#include <cstdint>
#include <initializer_list>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

using uint8 = std::uint8_t;
using uint32 = std::uint32_t;

#define INDEX_KING true

// Alternatives to consider:
//  - there are a maximum of 32 pieces on the board.
//    store each piece's position.
//  - introduce a 7th piece (we have bits to do it) to
//    represent special states: In one of the corners,
//    it represents a rook that can still be castled
//    with. In the middle two ranks, it represents
//    a pawn that just did a double move and is
//    eligible for en passant capture.

// For PGN spec, see https://www.chessclub.com/help/PGN-spec

// Packed representation; 35 bytes.
// TODO: Probably should separate out some of these static
// methods into just like a "Chess" class or namespace.
struct Position {
  enum Type : uint8 {
    PAWN = 1,
    KNIGHT = 2,
    BISHOP = 3,
    ROOK = 4,
    QUEEN = 5,
    KING = 6,
    C_ROOK = 7,
  };

  static constexpr uint8 EMPTY = 0U;
  
  static constexpr uint8 BLACK = 0b1000U;
  static constexpr uint8 WHITE = 0b0000U;
  // Maybe we should distinguish between type and piece?
  static constexpr uint8 TYPE_MASK = 0b0111U;
  static constexpr uint8 COLOR_MASK = 0b1000U;

  // Row 0 is the top row of the board, black's back
  // rank, aka. rank 8. We try to use "row" to mean
  // this zero-based top-to-bottom notion and "rank"
  // to mean the 1-based bottom-to-top version from
  // standard chess notation.
  //
  // Each row is 32 bits:
  // 4 bits per column,
  // big-endian (col A = 0 is most significant nybble).
  // Within each 4 bits:
  //   0 = empty
  //   MSB of 0 = white
  //   MSB of 1 = black
  //   lowest 3 bits from the piece enum.

  uint8 PieceAt(int row, int col) const {
    const uint32 r = rows[row];
    const uint32 p = r >> (4 * (7 - col));
    return p & 0b1111;
  }

  inline void SetPiece(int row, int col, uint8 p) {
    #if INDEX_KING
    if (p == (BLACK | KING)) {
      black_king = (row << 3) | col;
    } else if (p == (WHITE | KING)) {
      white_king = (row << 3) | col;
    }
    #endif
    uint32 &r = rows[row];
    uint32 mask = 0xF0000000 >> (col * 4);
    uint32 shift = p << (4 * (7 - col));
    r = (r & ~mask) | shift;
  }

  struct Move {
    // Castling is represented as the king moving two spaces to
    // its destination square.
    uint8 src_row = 0, src_col = 0;
    uint8 dst_row = 0, dst_col = 0;
    // zero unless a pawn promotion. contains the white/black mask.
    uint8 promote_to = 0;
  };
  inline static bool MoveEq(const Move &a, const Move &b) {
    return a.src_row == b.src_row && a.src_col == b.src_col &&
      a.dst_row == b.dst_row && a.dst_col == b.dst_col &&
      a.promote_to == b.promote_to;
  }

  // Assumes the move is legal. Doesn't attempt to do any
  // disambiguation; returns stuff like Qd3d5 and e2e4.
  // Does not annotate with check or checkmate marks.
  std::string LongMoveString(Move m) const;

  // PGN-style move, disambiguated. Doesn't annotate with
  // check/checkmate marks.
  std::string ShortMoveString(Move m);

  // Like e2->e4 or a7->a8=Q, independent of board state. Does not
  // need the move to be legal as long it is in bounds.
  static std::string DebugMoveString(Move m);
  
  // Show a 2D ASCII board.
  std::string BoardString() const;
  // Using capital letters for white, lowercase for black. Empty is space.
  static char HumanPieceChar(uint8 piece);
  static std::string HumanPieceString(uint8 piece);
  // Same but returns C or c for a castleable rook and - for empty.
  static char DebugPieceChar(uint8 piece);
  // Distinguishes black and white pieces. Space for empty.
  static const char *HTMLEntity(uint8 piece);
  
  // TODO: This does not handle castling and en passant correctly.
  // It ignores the move counts.
  // e.g. rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
  static bool ParseFEN(const char *fen, Position *pos);

  // Requires the caller to specify the move clock (number of half-moves
  // since a pawn move or capture) and full move count (starts at 1
  // and incremented after black moves), since we don't keep track
  // of those.
  std::string ToFEN(int halfmove_clock, int fullmove_number) const;
  
  // Parse a PGN-style move m in the current board state.
  // A move is the "Nc3!?" part of PGN. Move numbers, evaluations,
  // etc. should not be included. Does not do syntactic validation
  // (e.g. of trailing garbage).
  //  - Note that unlike FEN, piece letters are always capitalized.
  //    This distinguishes B for bishop from b for b-file.
  //  - Check and mood suffixes (+, #, !?) are ignored.
  //  - Any piece can be overqualified (Bf1g2 = Bfg2 = B1g2 = Bg2)
  //    but cannot be ambiguous. This routine may return an arbitrary
  //    legal move if the move string is ambiguous.
  //  - If the move is not valid (e.g. it asks to move Ra1a2 but
  //    there is no rook on a1) then this routine may return an
  //    arbitrary legal move (e.g. moving the queen on a1 to a2).
  //    For valid and legal moves, it returns the correct move.
  //  - Returns false if the move is not understood. Otherwise,
  //    returns true and sets the arguments to give the source
  //    and destination of the move.
  //  - The move string may be terminated by \0 or whitespace.
  bool ParseMove(const char *m, Move *move);

  // Assuming the move is legal, is it a castling move?
  bool IsCastling(Move m) const;

  // Assuming the move is legal, is it an en passant capture?
  bool IsEnPassant(Move m) const;

  // The following two are used in 50/75-move draw rules.
  // Assuming the move is legal, is it a pawn move?
  bool IsPawnMove(Move m) const {
    return (PieceAt(m.src_row, m.src_col) & TYPE_MASK) == PAWN;
  }
  // Assuming the move is legal, is it a capturing move?
  bool IsCapturing(Move m) const {
    return IsEnPassant(m) ||
      PieceAt(m.dst_row, m.dst_col) != EMPTY;
  }
  
  // Is the move legal in this current board state? The move must be
  // well-formed (positions within the board).
  bool IsLegal(Move m);

  // Apply the move to the current board, modifying it in place.
  // IsLegal(move) must be true or the result is undefined.
  void ApplyMove(Move m);
  
  // Get the row, col with the current player's king.
  inline std::pair<int, int> GetCurrentKing() const {
    return GetKing(!!(bits & BLACK_MOVE));
  }

  inline std::pair<int, int> GetKing(bool blackmove) const {
    #if INDEX_KING
    const uint8 pos = KING_POS_MASK & (blackmove ? black_king : white_king);
    return std::pair<int, int>(pos >> 3, pos & 7);

    #else

    const uint8 king = blackmove ? (BLACK | KING) : (WHITE | KING);

    for (int r = 0; r < 8; r++)
      for (int c = 0; c < 8; c++)
	if (PieceAt(r, c) == king)
	  return {r, c};

    // Invalid board -- could assert here.
    return {0, 0};
    #endif
  }

  // True if castling is still allowed. This is just the state of
  // whether the king and appropriate rook have moved; it doesn't
  // matter whose turn it is nor whether castling would actually be
  // legal (check, pieces in the way, etc.).
  bool CanStillCastle(bool white, bool king_side) const {
    if (white) {
      return (WHITE | C_ROOK) == PieceAt(7, king_side ? 7 : 0);
    } else {
      return (BLACK | C_ROOK) == PieceAt(0, king_side ? 7 : 0);
    }
  }
  
  // True if the current player is in check.
  bool IsInCheck();

  // True if the current player is checkmated.
  bool IsMated();

  // Return true if there are any legal moves. This is faster than
  // enumerating them all. If there are none, then the player is
  // either checkmated (IsInCheck) or stalemated.
  bool HasLegalMoves();
  // Get all the legal moves.
  std::vector<Move> GetLegalMoves();

  // Number of distinct legal moves. Note that different promotions
  // count as different moves.
  int NumLegalMoves();
  
  // Returns 0 if mated, 1 if exactly one, 2 if 2 or more.
  int ExactlyOneLegalMove();
  
  // Returns true if the indicated square is attacked (by the other
  // player) in the current position. "Attacked" here means an otherwise
  // unrestricted piece would be able to move in its fashion to capture
  // on that square, not considering check.
  // The square is typically unoccupied but it need not be.
  bool Attacked(int r, int c) const;

  template<class F>
  auto SetExcursion(int r, int c, uint8 piece, const F &f) -> decltype(f()) {
    const uint8 old = PieceAt(r, c);
    SetPiece(r, c, piece);
    auto ret = f();
    SetPiece(r, c, old);
    return ret;
  }

  // Apply the move to the current board state, and execute the
  // function with that state applied. Return the return value
  // of the function after undoing the applied move. As above,
  // the move must be legal.
  template<class F>
  auto MoveExcursion(Move m, const F &f) -> decltype(f()) {
    // PERF: Modify in place.
    Position copy = *this;
    ApplyMove(m);
    auto ret = f();
    *this = copy;
    return ret;
  }

  uint8 MovePieceType(Move m) const {
    return PieceAt(m.src_row, m.src_col) & TYPE_MASK;
  }
  
  bool BlackMove() const {
    return !!(bits & BLACK_MOVE);
  }

  void SetBlackMove(bool black_move) {
    if (black_move) {
      bits |= BLACK_MOVE;
    } else {
      bits &= ~BLACK_MOVE;
    }
  }

  inline static bool IsBlackSquare(int r, int c) {
    return !!((r ^ c) & 1);
  }
  
 private:
  // XXX document
  // Note: does not work for castling, e.p., other weird stuff?
  bool NotIntoCheck(Move m);
  
  // Whose move?
  static constexpr uint8 BLACK_MOVE = 0b10000000U;
  // True if the previous move was a pawn double move.
  static constexpr uint8 DOUBLE =     0b00001000U;
  // Then, the column number of the pawn's double move
  // (or zero otherwise). The pawn always moves into
  // the 4th or 5th rank as appropriate for the side.
  static constexpr uint8 PAWN_COL =   0b00000111U;

  // Starting position.
  uint32 rows[8] = {
       (uint32(BLACK | C_ROOK) << 28) |
       (uint32(BLACK | KNIGHT) << 24) |
       (uint32(BLACK | BISHOP) << 20) |
       (uint32(BLACK | QUEEN)  << 16) |
       (uint32(BLACK | KING)   << 12) |
       (uint32(BLACK | BISHOP) <<  8) |
       (uint32(BLACK | KNIGHT) <<  4) |
       (uint32(BLACK | C_ROOK) <<  0),

       (uint32(BLACK | PAWN) << 28) |
       (uint32(BLACK | PAWN) << 24) |
       (uint32(BLACK | PAWN) << 20) |
       (uint32(BLACK | PAWN) << 16) |
       (uint32(BLACK | PAWN) << 12) |
       (uint32(BLACK | PAWN) <<  8) |
       (uint32(BLACK | PAWN) <<  4) |
       (uint32(BLACK | PAWN) <<  0),

       0u, 0u, 0u, 0u,

       (uint32(WHITE | PAWN) << 28) |
       (uint32(WHITE | PAWN) << 24) |
       (uint32(WHITE | PAWN) << 20) |
       (uint32(WHITE | PAWN) << 16) |
       (uint32(WHITE | PAWN) << 12) |
       (uint32(WHITE | PAWN) <<  8) |
       (uint32(WHITE | PAWN) <<  4) |
       (uint32(WHITE | PAWN) <<  0),

       (uint32(WHITE | C_ROOK) << 28) |
       (uint32(WHITE | KNIGHT) << 24) |
       (uint32(WHITE | BISHOP) << 20) |
       (uint32(WHITE | QUEEN)  << 16) |
       (uint32(WHITE | KING)   << 12) |
       (uint32(WHITE | BISHOP) <<  8) |
       (uint32(WHITE | KNIGHT) <<  4) |
       (uint32(WHITE | C_ROOK) <<  0),
  };

  // Starting position. White's move, no en passant capture available.
  uint8 bits = 0u;

  // TODO: can use the last two bits here for castling status...
  #if INDEX_KING
  static constexpr uint8 KING_POS_MASK = 0b00111111;
  uint8 white_king = 60u, black_king = 4u;
  #endif

  friend struct PositionHash;
  friend struct PositionEq;
};

struct PositionEq {
  constexpr bool operator()(const Position &a,
			    const Position &b) const {
    return a.bits == b.bits &&
      a.rows[0] == b.rows[0] &&
      a.rows[1] == b.rows[1] &&
      a.rows[2] == b.rows[2] &&
      a.rows[3] == b.rows[3] &&
      a.rows[4] == b.rows[4] &&
      a.rows[5] == b.rows[5] &&
      a.rows[6] == b.rows[6] &&
      a.rows[7] == b.rows[7];
  };
};

struct PositionHash {
  constexpr std::size_t operator ()(const Position &p) const {
    uint64_t res = 0x3141572653589ULL;
    res += p.bits;
    res = (res << 11) | (res >> (64 - 11));
    res ^= ((uint64_t)p.rows[0] << 32) | p.rows[1];
    res *= 0x31337;
    res = (res << 43) | (res >> (64 - 43));
    res ^= ((uint64_t)p.rows[2] << 32) | p.rows[3];
    res *= 65537;
    res = (res << 7) | (res >> (64 - 7));
    res ^= ((uint64_t)p.rows[4] << 32) | p.rows[5];
    res *= 0x10FA3010359;
    res = (res << 19) | (res >> (64 - 19));
    res ^= ((uint64_t)p.rows[6] << 32) | p.rows[7];
    return res;
  }
};

#endif
