
#ifndef __CHESS_H
#define __CHESS_H

#include <cstdint>
#include <initializer_list>

using uint8 = std::uint8_t;
using uint32 = std::uint32_t;

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

// Packed representation; 33 bytes.
struct Position {
  enum Piece : uint8 {
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
  uint32 rows[8] = { 0 };

  uint8 PieceAt(int row, int col) const {
    const uint32 r = rows[row];
    const uint32 p = r >> (4 * (7 - col));
    return p & 0b1111;
  }

  void SetPiece(int row, int col, uint8 p) {
    uint32 &r = rows[row];
    uint32 mask = 0b1111'0000'0000'0000'0000'0000'0000'0000 >>
      (col * 4);
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
  bool ParseMove(const char *m, Move *move) {
    const bool blackmove = !!(bits & BLACK_MOVE);
    const uint8 my_mask = blackmove ? BLACK : WHITE;
    // const uint8 your_mask = blackmove ? WHITE : BLACK;

    if (m[0] == 'O' && m[1] == '-' && m[2] == 'O') {
      // Castling move. The king must be on its home square for
      // this to possibly be legal.
      const int row = blackmove ? 0 : 7;
      if (PieceAt(row, 4) != (my_mask | KING))
	return false;
      
      move->src_row = row;
      move->dst_row = row;
      move->src_col = 4;

      // Queen-side?
      if (m[3] == '-' && m[4] == 'O') {
	move->dst_col = 2;
      } else {
	move->dst_col = 6;
      }

      if (IsLegal(*move)) {
	return true;
      } else {
	return false;
      }
    }

    // Need to find the end of the move string up front.
    // This does not include any '=P' promotion string.
    int len = 0;
    {
      auto IsMoveChar =
	[](char c) {
	  switch (c) {
	  case 'Q':
	  case 'K':
	  case 'N':
	  case 'R':
	  case 'B':
	  case 'x':
	    return true;
	  default:
	    return (c >= 'a' && c <= 'h') ||
	      (c >= '1' && c <= '8');
	  }
	};

      while (IsMoveChar(m[len])) len++;
    }

    // Invalid!
    if (len < 2)
      return false;

    // The move must end with the destination square.
    char dr = m[len - 1];
    char dc = m[len - 2];
    if (dc < 'a' || dc > 'h') return false;
    if (dr < '1' || dr > '8') return false;
    move->dst_row = 7 - (dr - '1');
    move->dst_col = dc - 'a';

    // Also parse promotion string if it's there.
    if (m[len] == '=') {
      switch (m[len + 1]) {
      case 'Q': move->promote_to = QUEEN | my_mask; break;
      case 'N': move->promote_to = KNIGHT | my_mask; break;
      case 'R': move->promote_to = ROOK | my_mask; break;
      case 'B': move->promote_to = BISHOP | my_mask; break;
      default:
	return false;
      }
    }

    // Determine the type of piece being moved.
    // (This is without the color mask.)
    uint8 src_type = 0;
    int idx = 0;
    switch (m[idx]) {
    case 'Q': src_type = QUEEN; idx++; break;
    case 'K': src_type = KING; idx++; break;
    case 'N': src_type = KNIGHT; idx++; break;
    case 'R': src_type = ROOK; idx++; break;
    case 'B': src_type = BISHOP; idx++; break;
    default:
      if (m[idx] >= 'a' && m[idx] <= 'h') {
	src_type = PAWN;
      } else {
	return false;
      }
    }

    // Now between idx and len - 2, we have:
    //  optional disambiguation
    //  'x' if a capturing move
    // x doesn't really help disambiguate, since the
    // destination square either has a piece on it
    // or not (well, except en passant, but the move
    // is still unambiguous in this case.)

    // 255 here means unspecified.
    uint8 src_row = 255;
    uint8 src_col = 255;

    bool capturing = true;
    for (; idx < len - 2; idx++) {
      char c = m[idx];
      if (c >= 'a' && c <= 'h') {
	src_col = c - 'a';
      } else if (c >= '1' && c <= '8') {
	src_row = 7 - (c - '1');
      } else if (c == 'x') {
	capturing = true;
      } else {
	// No other character is allowed here.
	return false;
      }
    }

    // Rare in the wild, but if the move is fully disambiguated,
    // then we can just test its legality and be done.
    // (Note that this doesn't test whether the piece in the
    // source position actually matches. It's a little tricky
    // because of C_ROOK as usual. However, if the move is
    // well-formed, we will always do the right thing.)
    if (src_row != 255 && src_col != 255) {
      move->src_row = src_row;
      move->src_col = src_col;
      return IsLegal(*move);
    }

    // Whenever there's ambiguity (so in all the cases below), we
    // need to check that the source square actually contains the
    // indicated piece. Otherwise, we could be proposing a legal
    // move to IsLegal, but it's only legal because the piece there
    // is something else.
    
    // Checks the diagonals that can attack dst_row, dst_col, as a
    // bishop or queen does.
    auto Diagonal =
      [&](uint8 expected_type) {
	for (const int dr : { -1, 1 }) {
	  for (const int dc : { -1, 1 }) {
	    for (int r = (int)move->dst_row + dr,
		     c = (int)move->dst_col + dc;
		 r >= 0 && c >= 0 && r < 8 && c < 8;
		 r += dr, c += dc) {
	      if (src_row == 255 || src_row == r) {
		if (src_col == 255 || src_col == c) {
		  if (PieceAt(r, c) == (my_mask | expected_type)) {
		    move->src_row = r;
		    move->src_col = c;
		    if (IsLegal(*move)) {
		      return true;
		    }
		  }
		}
	      }
	    }
	  }
	}
      };
    
    // Otherwise, we need to search for the piece making the move.
    // This is based on the way the source type moves.
    switch (src_type) {
    case PAWN:
      // Pawn is definitely the trickiest, because there are
      // several strange cases. But much of the real work is in IsLegal.
      // Note that promotion (if any) was already parsed into the move.
      
      // The capturing case is actually easier...
      if (capturing) {
	// PGN requires specifying the source column for a pawn capturing
	// move.
	if (src_col == 255)
	  return false;

	// Although there is a lot of complexity to this capturing move,
	// we actually have all the information we need to propose it
	// to IsLegal (so the complexity is implemented there). The pawn
	// always captures one square diagonally, even en passant.

	move->src_col = src_col;
	if (blackmove) {
	  if (move->dst_row < 2)
	    return false;
	  
	  move->src_row = move->dst_row - 1;
	  return IsLegal(*move);
	} else {
	  if (move->dst_row >= 6)
	    return false;

	  move->src_row = move->dst_row + 1;
	  return IsLegal(*move);
	}
	
      } else {
	// Not capturing.
	
	// A move like "h4" is allowed with pawns on both h2 and h3,
	// or as a double-move with pawn just on h2. We have to check
	// that the move is legal anyway, so here we just check the two
	// trailing squares, in order.
	if (blackmove) {
	  if (move->dst_row < 2)
	    return false;

	  move->src_col = move->dst_col;
	  if (PieceAt(move->dst_row - 1, move->dst_col) == (my_mask | PAWN)) {
	    move->src_row = move->dst_row - 1;
	    return IsLegal(*move);
	  }

	  if (PieceAt(move->dst_row - 2, move->dst_col) == (my_mask | PAWN)) {
	    move->src_row = move->dst_row - 2;
	    return IsLegal(*move);
	  }

	  return false;
	} else {
	  if (move->dst_row >= 6)
	    return false;

	  move->src_col = move->dst_col;
	  if (PieceAt(move->dst_row + 1, move->dst_col) == (my_mask | PAWN)) {
	    move->src_row = move->dst_row + 1;
	    return IsLegal(*move);
	  }

	  if (PieceAt(move->dst_row + 2, move->dst_col) == (my_mask | PAWN)) {
	    move->src_row = move->dst_row + 2;
	    return IsLegal(*move);
	  }

	  return false;
	}
      }
      
      return false;

    case KING:
      // This is the easiest. There can only be one king, and
      // castling was already handled. So we just need to find
      // the king on one of the 8 adjacent squares.
      for (int r = (int)move->dst_row - 1; r <= move->dst_row + 1; r++) {
	if (r >= 0 && r < 8) {
	  for (int c = (int)move->dst_col - 1; c <= move->dst_col + 1; c++) {
	    if (c >= 0 && c < 8) {
	      if (r != move->dst_row || c != move->dst_col) {
		if (PieceAt(r, c) == (my_mask | KING)) {
		  move->src_row = r;
		  move->src_col = c;
		  return IsLegal(*move);
		}
	      }
	    }
	  }
	}
      }
      
      // Nearby king not found?!
      return false;

    case QUEEN:

      if (Diagonal(QUEEN))
	return true;

      // Vertical.
      for (int r = 0; r < 8; r++) {
	if (r != move->dst_row && (src_row == 255 || src_row == r)) {
	  int c = move->dst_col;
	  const uint8 p = PieceAt(r, c);
	  if (p == (my_mask | QUEEN)) {
	    move->src_col = c;
	    move->src_row = r;
	    if (IsLegal(*move))
	      return true;
	  }
	}
      }

      // Horizontal.
      for (int c = 0; c < 8; c++) {
	if (c != move->dst_col && (src_col == 255 || src_col == c)) {
	  int r = move->dst_row;
	  const uint8 p = PieceAt(r, c);
	  if (p == (my_mask | QUEEN)) {
	    move->src_col = c;
	    move->src_row = r;
	    if (IsLegal(*move))
	      return true;
	  }
	}
      }

      return false;

    case C_ROOK:
    case ROOK:
      // Castling rook is a representation trick; for this purpose
      // they move exactly the same way.

      // There is some temptation to reuse the rook logic in queens.
      // But note: Disambiguation sometimes leaves us with a single
      // choice for the rook source, but this logic doesn't work for
      // queens! Also note that the loop below has to check for
      // both ROOK and C_ROOK.
      
      // If the rook is changing columns, then it is moving
      // horizontally along the dst row, and the source is
      // uniquely determined.
      if (src_col != 255 && src_col != move->dst_col) {
	move->src_col = src_col;
	move->src_row = move->dst_row;
	return IsLegal(*move);
      }

      // Same if it is changing rows.
      if (src_row != 255 && src_row != move->dst_row) {
	move->src_col = move->dst_col;
	move->src_row = src_row;
	return IsLegal(*move);
      }

      // General case:
      
      // Vertical.
      // PERF: Possibly a cheaper way to do this sort of loop is to
      // start at dst_row + 1, then check 7 rows, and loop mod 8?
      for (int r = 0; r < 8; r++) {
	if (r != move->dst_row && (src_row == 255 || src_row == r)) {
	  int c = move->dst_col;
	  const uint8 p = PieceAt(r, c);
	  if (p == (my_mask | ROOK) || p == (my_mask | C_ROOK)) {
	    move->src_col = c;
	    move->src_row = r;
	    if (IsLegal(*move))
	      return true;
	  }
	}
      }

      // Horizontal.
      for (int c = 0; c < 8; c++) {
	if (c != move->dst_col && (src_col == 255 || src_col == c)) {
	  int r = move->dst_row;
	  const uint8 p = PieceAt(r, c);
	  if (p == (my_mask | ROOK) || p == (my_mask | C_ROOK)) {
	    move->src_col = c;
	    move->src_row = r;
	    if (IsLegal(*move))
	      return true;
	  }
	}
      }

      return false;

    case KNIGHT: {

      auto KnightMove =
	[&](int dr, int dc) {
	  const int r = (int)move->dst_row + dr;
	  if (r < 0 || r >= 8) return false;
	  if (src_row != 255 && src_row != r) return false;
	  const int c = (int)move->dst_col + dc;
	  if (c < 0 || c >= 8) return false;
	  if (src_col != 255 && src_col != c) return false;
	  move->src_col = c;
	  move->src_row = r;
	  return IsLegal(*move);
	};
      
      for (const int udr : { -1, 1 }) {
	for (const int udc : { -1, 1 }) {
	  // (1,2) then (2,1).
	  if (KnightMove(udr, 2 * udc))
	    return true;
	  if (KnightMove(2 * udr, udc))
	    return true;
	}
      }

      return false;
    }
    case BISHOP:

      // PERF: Some optimizations when we have disambiguation
      // are possible, but we still need to check multiple cells
      // in that case. Here we just do the two diagonal loops.

      return Diagonal(BISHOP);

    default:;
    }
    // Bug!
    return false;
  }

  // XXX distinguish "IsAllowed" from "IsLegal"?
  // Actually the in-check condition is faster if we just implement
  // it directly; it's not that hard.
  
  // Is the move legal in this current board state? The move must be
  // well-formed (positions within the board).
  bool IsLegal(Move m) {
    const bool blackmove = !!(bits & BLACK_MOVE);
    const uint8 my_mask = blackmove ? BLACK : WHITE;
    const uint8 your_mask = blackmove ? WHITE : BLACK;

    // XXX assert bounds for debugging at least?
    
    // TODO: Perhaps this gets established in each case below?
    if (m.src_row == m.dst_row &&
	m.src_col == m.dst_col) return false;

    const uint8 src_piece = PieceAt(m.src_row, m.src_col);
    const uint8 src_type = src_piece & TYPE_MASK;
    // Can only move my pieces.
    if ((src_piece & COLOR_MASK) != my_mask)
      return false;

    // If capturing, it must be an opponent piece!
    const uint8 dst_piece = PieceAt(m.dst_row, m.dst_col);
    const uint8 dst_type = dst_piece & TYPE_MASK;
    if (dst_type != EMPTY &&
	(dst_piece & COLOR_MASK) != your_mask)
      return false;

    // It is never actually legal to capture the opponent's
    // king, and this helps avoid complexity when testing whether
    // we're in check.
    if (dst_type == KING)
      return false;

    // When we are in check, only some moves are legal. We test this
    // by checking whether the move leaves us in check, since we also
    // need to check whether we move into check anyway. Failing to
    // "get out of check" can equivalently be thought of as "making a
    // move that leaves the king in check."
    
    switch (src_type) {
    case PAWN:
    case BISHOP:
    case KNIGHT:
    case ROOK:
    case QUEEN: {
      break;
    }
    case KING: {
      const int dr = (int)m.dst_row - (int)m.src_row;
      const int dc = (int)m.dst_col - (int)m.src_col;
      if (dr == 0 && dc == 0) return false;
      if (dr >= -1 && dr <= 1 &&
	  dc >= -1 && dc <= 1) {
	return NotIntoCheck(m);
      } else if (dr == 0 && dc == -2) {
	if (m.src_col != 4)
	  return false;

	// No castling out of check.
	if (Attacked(m.src_row, m.src_col))
	  return false;
	
	if (blackmove) {
	  // black queenside
	  if (m.src_row != 0)
	    return false;

	  if (PieceAt(0, 0) != (BLACK | C_ROOK))
	    return false;

	  if (PieceAt(0, 1) != EMPTY ||
	      PieceAt(0, 2) != EMPTY ||
	      PieceAt(0, 3) != EMPTY)
	    return false;

	  // Can't move "through" or "into" check.
	  //
	  // Note that we don't need to remove the king (or move the
	  // rook) to do these tests. For "through": If an attack
	  // passes through the king's square (e.g. by a rook on the
	  // back rank) then the king is also in check. For "into": If
	  // the rook blocks the check on the back rank (after
	  // castling) then castling was illegal to begin with because
	  // the king started in check.
	  if (Attacked(0, 2) ||
	      Attacked(0, 3))
	    return false;

	  return true;
	  
	  /*
	  SetExcursion(
	      0, 0, EMPTY,
	      [&]() {
		SetExcursion(
		    0, 4, EMPTY,
		    [&]() {
		      SetExcursion(
			  0, 2, BLACK | KING,
			  [&]() {
			    SetExcursion(
				0, 3, BLACK | ROOK,
				// No need to un-C-rook the other rook
				// (if applicable) nor unset en passant
				// flags because they cannot affect
				// whether this square is attacked.
				)})})});
				*/
		      
			 

	} else {
	  // white queenside
	  if (m.src_row != 7)
	    return false;

	  if (PieceAt(7, 0) != (WHITE | C_ROOK))
	    return false;

	  if (PieceAt(7, 1) != EMPTY ||
	      PieceAt(7, 2) != EMPTY ||
	      PieceAt(7, 3) != EMPTY)
	    return false;
	  // TODO
	}

	  
      } else if (dr == 0 && dc == 2) {
	if (m.src_col != 4)
	  return false;
	// TODO...
      }
      return false;
    }
      
    default:
    case EMPTY: return false;
    }
    
    return false;
  }

  // XXX TODO?!
  bool NotIntoCheck(Move m) const { return false; }
  bool Attacked(int r, int c) const { return false; }
  
  template<class F>
  auto SetExcursion(int r, int c, uint8 piece, const F &f) -> decltype(f()) {
    const uint8 old = PieceAt(r, c);
    SetPiece(r, c, piece);
    auto ret = f();
    SetPiece(r, c, old);
    return ret;
  }
    
  // Apply the move to the current board, modifying it in place.
  // IsLegal(move) must be true or the result is undefined.
  void ApplyMove(Move m) {
    // TODO
  }

  // Apply the move to the current board state, and execute the
  // function with that state applied. Return the return value
  // of the function after undoing the applied move. As above,
  // the move must be legal.
  // (Privately, this function is also used to test if a move
  // is legal, by provisionally executing it and looking for
  // checks against the king.)
  template<class F>
  auto MoveExcursion(Move m, const F &f) -> decltype(f()) {
    // TODO
    return f();
  }
  
  #if 0
  // Never returns C_ROOK.
  uint8 SimplePieceAt(int row, int col) const {
    uint8 p = PieceAt(row, col);
    // XXX no, because it also has color bits for example
    return p == C_ROOK ? ROOK : p;
  }
  #endif
  
  // Whose move?
  static constexpr uint8 BLACK_MOVE = 0b10000000U;
  // True if the previous move was a pawn double move.
  static constexpr uint8 DOUBLE =     0b00001000U;
  // Then, the column number of the pawn's double move
  // (or zero otherwise). The pawn always moves into
  // the 4th or 5th row as appropriate for the side.
  static constexpr uint8 PAWN_COL =   0b00000111U;

  uint8 bits = 0;
};

#if 0
struct PiecePosition {
  enum Type : uint8 {
    EMPTY = 0,
    PAWN = 1,
    KNIGHT = 2,
    BISHOP = 3,
    ROOK = 4,
    QUEEN = 5,
    KING = 6,
  };
  struct Piece {
    // 6 bits stores exactly the 64 board indices.
    uint8 pos : 6;
    uint8 type : 3;
  };

  Piece 
  
};
#endif

#endif
