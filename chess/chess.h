
#ifndef __CHESS_H
#define __CHESS_H

#include <cstdint>
#include <initializer_list>
#include <string>
#include <tuple>
#include <utility>

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

// XXX TODO: Delete all this debugging printing.
#define IFDEBUG if (true) {} else
// #define IFDEBUG 

// Packed representation; 33 bytes.
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

  void SetPiece(int row, int col, uint8 p) {
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

  // Show a 2D ASCII board.
  std::string BoardString() const;
  // Using capital letters for white, lowercase for black. Empty is space.
  static char HumanPieceChar(uint8 piece);
  // Same but returns C or c for a castleable rook and - for empty.
  static char DebugPieceChar(uint8 piece);
  // Distinguishes black and white pieces. Space for empty.
  static const char *HTMLEntity(uint8 piece);
  
  // TODO: Parse FEN.
  // e.g. rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
  static bool ParseFEN(const char *fen, Position *pos);

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
    IFDEBUG printf("\n== %s ==\n", m);

    const bool blackmove = !!(bits & BLACK_MOVE);
    const uint8 my_color = blackmove ? BLACK : WHITE;
    // const uint8 your_color = blackmove ? WHITE : BLACK;

    if (m[0] == 'O' && m[1] == '-' && m[2] == 'O') {
      // Castling move. The king must be on its home square for
      // this to possibly be legal.
      const int row = blackmove ? 0 : 7;
      if (PieceAt(row, 4) != (my_color | KING))
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

      return IsLegal(*move);
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

    IFDEBUG printf("len: %d (%c %c)\n", len, m[len - 2], m[len - 1]);
    
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
      case 'Q': move->promote_to = QUEEN | my_color; break;
      case 'N': move->promote_to = KNIGHT | my_color; break;
      case 'R': move->promote_to = ROOK | my_color; break;
      case 'B': move->promote_to = BISHOP | my_color; break;
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

    IFDEBUG printf("[%d] src_type: %d\n", idx, src_type);
    
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

    bool capturing = false;
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

    IFDEBUG printf("src row/col %d/%d%s\n", src_row, src_col,
		   capturing ? " (capt)" : "");
    
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
	      IFDEBUG printf("Diag drdc %d %d, r c %d %d\n",
			     dr, dc, r, c);
	      if (src_row == 255 || src_row == r) {
		if (src_col == 255 || src_col == c) {
		  // PERF could exit early if the square is not empty?
		  if (PieceAt(r, c) == (my_color | expected_type)) {
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
	return false;
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
	IFDEBUG printf("Non-capturing pawn %d,%d -> %d,%d\n",
		       src_row, src_col,  move->dst_row, move->dst_col);
	
	// A move like "h4" is allowed with pawns on both h2 and h3,
	// or as a double-move with pawn just on h2. We have to check
	// that the move is legal anyway, so here we just check the two
	// trailing squares, in order.
	if (blackmove) {
	  if (move->dst_row < 2)
	    return false;

	  move->src_col = move->dst_col;
	  if (PieceAt(move->dst_row - 1, move->dst_col) == (my_color | PAWN)) {
	    move->src_row = move->dst_row - 1;
	    return IsLegal(*move);
	  }

	  if (PieceAt(move->dst_row - 2, move->dst_col) == (my_color | PAWN)) {
	    move->src_row = move->dst_row - 2;
	    return IsLegal(*move);
	  }

	  return false;
	} else {
	  if (move->dst_row >= 6)
	    return false;

	  move->src_col = move->dst_col;
	  if (PieceAt(move->dst_row + 1, move->dst_col) == (my_color | PAWN)) {
	    move->src_row = move->dst_row + 1;
	    return IsLegal(*move);
	  }

	  if (PieceAt(move->dst_row + 2, move->dst_col) == (my_color | PAWN)) {
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
		if (PieceAt(r, c) == (my_color | KING)) {
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

      if (Diagonal(QUEEN)) {
	IFDEBUG printf("Q diagonal OK.\n");
	return true;
      }

      // Vertical.
      if (src_col == 255 || src_col == move->dst_col) {
	for (int r = 0; r < 8; r++) {
	  IFDEBUG printf("Q vert %d\n", r);
	  if (r != move->dst_row && (src_row == 255 || src_row == r)) {
	    int c = move->dst_col;
	    const uint8 p = PieceAt(r, c);
	    if (p == (my_color | QUEEN)) {
	      move->src_col = c;
	      move->src_row = r;
	      if (IsLegal(*move))
		return true;
	    }
	  }
	}
      }

      // Horizontal.
      if (src_row == 255 || src_row == move->dst_row) {
	for (int c = 0; c < 8; c++) {
	  IFDEBUG printf("Q horiz %d\n", c);
	  if (c != move->dst_col && (src_col == 255 || src_col == c)) {
	    int r = move->dst_row;
	    const uint8 p = PieceAt(r, c);
	    if (p == (my_color | QUEEN)) {
	      move->src_col = c;
	      move->src_row = r;
	      if (IsLegal(*move))
		return true;
	    }
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

      IFDEBUG printf("Rook generic to %d %d\n",
		     move->dst_row, move->dst_col);
      // General case:
      
      // Vertical.
      // PERF: Possibly a cheaper way to do this sort of loop is to
      // start at dst_row + 1, then check 7 rows, and loop mod 8?
      for (int r = 0; r < 8; r++) {
	if (r != move->dst_row && (src_row == 255 || src_row == r)) {
	  int c = move->dst_col;
	  const uint8 p = PieceAt(r, c);
	  IFDEBUG printf(" v @ %d %d is %d\n", r, c, p);
	  if (p == (my_color | ROOK) || p == (my_color | C_ROOK)) {
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
	  IFDEBUG printf(" h @ %d %d is %d\n", r, c, p);
	  if (p == (my_color | ROOK) || p == (my_color | C_ROOK)) {
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
  
  // Is the move legal in this current board state? The move must be
  // well-formed (positions within the board).
  bool IsLegal(Move m) {
    const bool blackmove = !!(bits & BLACK_MOVE);
    const uint8 my_color = blackmove ? BLACK : WHITE;
    const uint8 your_color = blackmove ? WHITE : BLACK;

    IFDEBUG printf("IsLegal? %d,%d -> %d,%d",
		   m.src_row, m.src_col,
		   m.dst_row, m.dst_col);
    IFDEBUG if (m.promote_to) printf(" =%d", m.promote_to);
    IFDEBUG printf("\n");
    
    // XXX assert bounds for debugging at least?
    
    // Below we assume no self-moves.
    if (m.src_row == m.dst_row &&
	m.src_col == m.dst_col) return false;

    const uint8 src_piece = PieceAt(m.src_row, m.src_col);
    const uint8 src_type = src_piece & TYPE_MASK;
    // Can only move my pieces.
    if ((src_piece & COLOR_MASK) != my_color)
      return false;

    // If capturing, it must be an opponent piece!
    const uint8 dst_piece = PieceAt(m.dst_row, m.dst_col);
    const uint8 dst_type = dst_piece & TYPE_MASK;
    if (dst_type != EMPTY &&
	(dst_piece & COLOR_MASK) != your_color)
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

    auto GetDir =
      [](uint8 src, uint8 dst) {
	// assert src != dst
	return src < dst ? 1 : -1;
      };

    auto BishopMove =
      [&]() {
	const int vr = (int)m.dst_row - (int)m.src_row;
	const int vc = (int)m.dst_col - (int)m.src_col;

	// Need to move the same number of spaces along one of the
	// four diagonals.
	int dist = abs(vr);
	if (dist != abs(vc))
	  return false;

	// assert vr != 0 && vc != 0

	// PERF: Since we know everything is in bounds, we can do
	// these loops faster by using raw indices (i.e. 0--63) and a
	// single delta.

	int dr = GetDir(m.src_row, m.dst_row);
	int dc = GetDir(m.src_col, m.dst_col);

	for (int r = (int)m.src_row + dr, c = (int)m.src_col + dc;
	     --dist;
	     r += dr, c += dc) {
	  if (PieceAt(r, c) != EMPTY) {
	    return false;
	  }
	}

	return true;
      };
    
    auto RookMove =
      [&]() {
	if (m.src_row == m.dst_row &&
	    m.src_col != m.dst_col) {
	  // Moving horizontally.
	  const int d = GetDir(m.src_col, m.dst_col);
	  IFDEBUG printf("Horiz %d 'rook' in row %d\n", d, m.src_row);
	  for (int c = (int)m.src_col + d; c != (int)m.dst_col; c += d)
	    if (PieceAt(m.src_row, c) != EMPTY)
	      return false;
	  return true;
	    
	} else if (m.src_row != m.dst_row &&
		   m.src_col == m.dst_col) {
	  // Moving vertically.
	  const int d = GetDir(m.src_row, m.dst_row);
	  IFDEBUG printf("Vert %d 'rook' in col %d\n", d, m.src_col);
	  for (int r = (int)m.src_row + d; r != (int)m.dst_row; r += d)
	    if (PieceAt(r, m.src_col) != EMPTY)
	      return false;

	  return true;
	}
	return false;
      };

    // Only pawn can promote.
    if (m.promote_to != 0 && src_type != PAWN)
      return false;

    auto LegalPromotion =
      [](uint8 type) {
	switch (type) {
	case QUEEN:
	case ROOK:
	case BISHOP:
	case KNIGHT:
	  return true;
	default:
	  return false;
	}
      };
    
    switch (src_type) {
    case PAWN:

      IFDEBUG printf("IsLegal Pawn\n");
      // Promotion can apply with both capturing and non-capturing
      // moves, so make sure it is legal first.
      if (blackmove) {
	if (m.dst_row == 7) {
	  return (m.promote_to & COLOR_MASK) == BLACK &&
	    LegalPromotion(m.promote_to & TYPE_MASK);
	} else {
	  if (m.promote_to != 0)
	    return false;
	}
      } else {
	if (m.dst_row == 0) {
	  return (m.promote_to & COLOR_MASK) == WHITE &&
	    LegalPromotion(m.promote_to & TYPE_MASK);
	} else {
	  if (m.promote_to != 0)
	    return false;
	}
      }
      
      if (m.src_col == m.dst_col) {
	IFDEBUG printf("IsLegal: Non-capturing pawn.\n");
	// Normal non-capturing move. Destination
	// must be empty.
	if (PieceAt(m.dst_row, m.dst_col) != EMPTY)
	  return false;

	int dr = (int)m.dst_row - (int)m.src_row;
	if (blackmove) {
	  if (dr == 1)
	    return NotIntoCheck(m);

	  if (dr == 2) {
	    if (m.src_row != 1)
	      return false;

	    if (PieceAt(2, m.dst_col) != EMPTY)
	      return false;

	    return NotIntoCheck(m);
	  }
	  
	  return false;
	} else {
	  IFDEBUG printf("IsLegal: white %d\n", dr);
	  if (dr == -1)
	    return NotIntoCheck(m);

	  if (dr == -2) {
	    if (m.src_row != 6)
	      return false;

	    if (PieceAt(5, m.dst_col) != EMPTY)
	      return false;

	    return NotIntoCheck(m);
	  }
	  
	  return false;
	}
	
      } else {
	// Capturing move.
	int dc = (int)m.dst_col - (int)m.src_col;
	// Capturing is always one square diagonally, even en passant.
	if (abs(dc) != 1)
	  return false;
	int dr = (int)m.dst_row - (int)m.src_row;
	if (dr != (blackmove ? 1 : -1))
	  return false;

	IFDEBUG printf("Pawn capture dr=%d, dc=%d %s%d\n", dr, dc,
		       (bits & DOUBLE) ? "double " : "",
		       (bits & PAWN_COL));
	
	// En passant captures.
	if (PieceAt(m.dst_row, m.dst_col) == EMPTY) {
	  if ((bits & DOUBLE) &&
	      m.dst_col == (bits & PAWN_COL) &&
	      m.dst_row == (blackmove ? 5 : 2) &&
	      // In principle this could be an assertion.
	      // The captured piece is actually on the source row,
	      // which is the same as dst_row - dr.
	      PieceAt(m.dst_row - dr, m.dst_col) == (your_color | PAWN)) {
	    IFDEBUG printf("en passant capture..\n");
	    // NotIntoCheck moves the capturing pawn to the destination
	    // square, but we also need to remove the captured pawn.
	    return SetExcursion(m.dst_row - dr, m.dst_col, EMPTY,
				[&]() { return NotIntoCheck(m); });
	  } else {
	    return false;
	  }
	}

	// Otherwise, a normal capture. There must be a piece to
	// capture (we already tested above that if there is a piece
	// on the destination square, it has the opponent color).
	if (PieceAt(m.dst_row, m.dst_col) == EMPTY)
	  return false;

	return NotIntoCheck(m);
      }
      break;

    case KNIGHT: {

      // This is super easy because there isn't any need to check for
      // pieces "in the way."
      const int adr = abs((int)m.src_row - (int)m.dst_row);
      const int adc = abs((int)m.src_col - (int)m.dst_col);
      IFDEBUG printf("IsLegal Knight: %d %d\n", adr, adc);
      
      return ((adr == 1 && adc == 2) ||
	      (adr == 2 && adc == 1)) &&
	NotIntoCheck(m);
    }
      
    case BISHOP:
      return BishopMove() && NotIntoCheck(m);

    case C_ROOK:
    case ROOK:
      return RookMove() && NotIntoCheck(m);
      
    case QUEEN:
      return (RookMove() || BishopMove()) && NotIntoCheck(m);

    case KING: {
      const int dr = (int)m.dst_row - (int)m.src_row;
      const int dc = (int)m.dst_col - (int)m.src_col;
      IFDEBUG printf("King move dr/dc %d %d\n", dr, dc);
      if (dr == 0 && dc == 0) return false;
      if (dr >= -1 && dr <= 1 &&
	  dc >= -1 && dc <= 1) {
	return NotIntoCheck(m);

      } else if (dr == 0 && dc == -2) {
	// Queenside castling.
	if (m.src_col != 4)
	  return false;

	// No castling out of check.
	if (Attacked(m.src_row, m.src_col))
	  return false;

	// Appropriate back rank for the color.
	const int row = blackmove ? 0 : 7;

	if (m.src_row != row)
	  return false;

	if (PieceAt(row, 0) != (my_color | C_ROOK))
	  return false;

	if (PieceAt(row, 1) != EMPTY ||
	    PieceAt(row, 2) != EMPTY ||
	    PieceAt(row, 3) != EMPTY)
	  return false;

	// Can't move "through" or "into" check.
	//
	// Note that we don't need to remove the king (or move the
	// rook) to do these tests, because we already tested that
	// the king is not in check in its home square. Only a
	// back-rank check (like from a white rook on f8) would
	// be relevant (discovered by removing the king from its
	// home square, or blocked by moving the rook to its
	// destination square), but this type of attack is excluded
	// because the king is not currently in check.
	if (Attacked(row, 2) ||
	    Attacked(row, 3))
	  return false;

	return true;
	  
      } else if (dr == 0 && dc == 2) {
	// Kingside. Mimics the logic above.

	if (m.src_col != 4)
	  return false;

	if (Attacked(m.src_row, m.src_col)) {
	  return false;
	}

	const int row = blackmove ? 0 : 7;

	if (PieceAt(row, 7) != (my_color | C_ROOK))
	  return false;

	if (PieceAt(row, 6) != EMPTY ||
	    PieceAt(row, 5) != EMPTY)
	  return false;

	if (Attacked(row, 6) ||
	    Attacked(row, 5))
	  return false;

	return true;
      }
      return false;
    }
      
    default:
    case EMPTY: return false;
    }
    
    return false;
  }

  // Get the row, col with the current player's king.
  std::pair<int, int> GetKing() const {
    const bool blackmove = !!(bits & BLACK_MOVE);
    const uint8 my_color = blackmove ? BLACK : WHITE;

    for (int r = 0; r < 8; r++)
      for (int c = 0; c < 8; c++)
	if (PieceAt(r, c) == (my_color | KING))
	  return {r, c};

    // Invalid board -- could assert here.
    return {0, 0};
  }

  // Returns true if the indicated square is attacked (by the other
  // player) in the current position. "Attacked" here means an otherwise
  // unrestricted piece would be able to move in its fashion to capture
  // on that square, not considering check. Does not include en passant.
  // The square is typically unoccupied.
  bool Attacked(int r, int c) const {
    const bool blackmove = !!(bits & BLACK_MOVE);
    const uint8 attacker_color = blackmove ? WHITE : BLACK;

    IFDEBUG printf("Attacked %d,%d? With board:\n%s\n",
		   r, c, BoardString().c_str());
    
    // TODO this logic can move into the vector search below.
    // Check the pawn attacks since these are not symmetric.
    if (blackmove) {
      if (r < 7) {
	if (c > 0 && PieceAt(r + 1, c - 1) == (attacker_color | PAWN))
	  return true;
	if (c < 7 && PieceAt(r + 1, c + 1) == (attacker_color | PAWN))
	  return true;
      }
    } else {
      if (r > 0) {
	if (c > 0 && PieceAt(r - 1, c - 1) == (attacker_color | PAWN))
	  return true;
	if (c < 7 && PieceAt(r - 1, c + 1) == (attacker_color | PAWN))
	  return true;
      }
    }

    // Now knight moves.
    auto HasKnight =
      [&](int r, int c) {
	if (r < 0 || r >= 8) return false;
	if (c < 0 || c >= 8) return false;
	return PieceAt(r, c) == (attacker_color | KNIGHT);
      };

    for (const int udr : { -1, 1 }) {
      for (const int udc : { -1, 1 }) {
	// (1,2) then (2,1).
	if (HasKnight(r + udr, c + 2 * udc))
	  return true;
	if (HasKnight(r + 2 * udr, c + udc))
	  return true;
      }
    }

    // Now check queen moves for pieces that can make those moves.
    // Note that for this purpose, king is equivalently a queen that
    // can only move 1 square!
    for (const int dr : { -1, 0, 1 }) {
      for (const int dc : { -1, 0, 1 }) {
	// Skip the degenerate vector.
	if (dr == 0 && dc == 0) continue;
	
	// Along any dimension, we 
	for (int rr = r + dr, cc = c + dc;
	     rr >= 0 && cc >= 0 && rr < 8 && cc < 8;
	     rr += dr, cc += dc) {
	  const uint8 p = PieceAt(rr, cc);
	  IFDEBUG printf("v %d,%d = %d,%d (%c)\n",
			 dr, dc, rr, cc, DebugPieceChar(p));

	  if ((p & COLOR_MASK) == attacker_color) {
	    switch (p & TYPE_MASK) {
	    case PAWN:
	      // XXX TODO: actually this is quite easy to just do here.
	      // Move the special-case logic from above here.
	      break;
	    case KING:
	      if (abs(rr - r) <= 1 && abs(cc - c) <= 1)
		return true;
	      break;
	    case QUEEN:
	      return true;
	    case ROOK:
	    case C_ROOK:
	      if (dc == 0 || dr == 0)
		return true;
	      break;
	    case BISHOP:
	      if (dc != 0 && dr != 0)
		return true;
	      break;
	    case KNIGHT:
	      // Knights don't attack on these vectors. We already
	      // checked them above.
	      break;
	    }
	  }
	    
	  // Any other contents would block a potential attack.
	  // Move on to the next vector.
	  if (p != EMPTY)
	    break;
	}
      }
    }
      
    return false;
  }

  
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
    // Here we can assume the move is legal, so it simplifies our
    // lives somewhat. But there are still a few special cases,
    // and more bookkeeping.

    // To start, clear en passant state, since any move invalidates
    // it.
    bits &= ~(DOUBLE | PAWN_COL);

    uint8 source_piece = PieceAt(m.src_row, m.src_col);
    // If moving the king (this includes castling), remove the ability
    // to castle.
    if ((source_piece & TYPE_MASK) == KING) {
      const bool blackmove = !!(bits & BLACK_MOVE);
      if (blackmove) {
	if (PieceAt(0, 0) == (BLACK | C_ROOK))
	  SetPiece(0, 0, BLACK | ROOK);
	if (PieceAt(0, 7) == (BLACK | C_ROOK))
	  SetPiece(0, 7, BLACK | ROOK);
      } else {
	if (PieceAt(7, 0) == (WHITE | C_ROOK))
	  SetPiece(7, 0, WHITE | ROOK);
	if (PieceAt(7, 7) == (WHITE | C_ROOK))
	  SetPiece(7, 7, WHITE | ROOK);
      }
    }

    // If moving a castleable rook, it becomes a regular rook.
    if ((source_piece & TYPE_MASK) == C_ROOK)
      source_piece = (COLOR_MASK & source_piece) | ROOK;

    
    // And speaking of which, test for the strange en passant
    // case first.
    if ((source_piece & TYPE_MASK) == PAWN &&
	m.src_col != m.dst_col &&
	PieceAt(m.dst_row, m.dst_col) == EMPTY) {
      // Pawn move, not vertical, and into empty space. If it is
      // legal then it is en passant.
      // Here the source row is where the captured piece resided
      // (next to the capturing pawn, in the destination column).
      SetPiece(m.src_row, m.dst_col, EMPTY);

      // If a double pawn move, set the en passant state...
    } else if ((source_piece & TYPE_MASK) == PAWN &&
	       ((m.src_row == 1 && m.dst_row == 3) ||
		(m.src_row == 6 && m.dst_row == 4))) {

      bits |= (DOUBLE | m.dst_col);
      
      // And then for castling...
    } else if ((source_piece & TYPE_MASK) == KING &&
	       m.src_col == 4 &&
	       (m.dst_col == 6 || m.dst_col == 2)) {
      // ... move the rook as well.
      if (m.dst_col == 6) {
	// King-side.
	SetPiece(m.dst_row, 7, EMPTY);
	SetPiece(m.dst_row, 5, ROOK | (COLOR_MASK & source_piece));
      } else {
	// Queen-side.
	SetPiece(m.dst_row, 0, EMPTY);
	SetPiece(m.dst_row, 3, ROOK | (COLOR_MASK & source_piece));
      }
    }
    
    if (m.promote_to != 0) {
      // This has the color already present.
      source_piece = m.promote_to;
    }

    // All moves clear the source square and populate the destination.
    SetPiece(m.src_row, m.src_col, EMPTY);
    SetPiece(m.dst_row, m.dst_col, source_piece);

    // And pass to the other player.
    bits ^= BLACK_MOVE;
  }

  // Apply the move to the current board state, and execute the
  // function with that state applied. Return the return value
  // of the function after undoing the applied move. As above,
  // the move must be legal.
  template<class F>
  auto MoveExcursion(Move m, const F &f) -> decltype(f()) {
    // TODO
    return f();
  }
  
 private:
  // XXX document
  // Note: does not work for castling, e.p., other weird stuff?
  bool NotIntoCheck(Move m) {
    // For the sake of testing for check, we can ignore promotion.
    const uint8 p = PieceAt(m.src_row, m.src_col);
    return
      SetExcursion(
	  m.src_row, m.src_col, EMPTY,
	  [&]() {
	    return
	      SetExcursion(
		  m.dst_row, m.dst_col, p,
		  [&]() {
		    int r, c;
		    std::tie(r, c) = GetKing();
		    IFDEBUG printf("King at %d,%d\n", r, c);
		    bool attacked = Attacked(r, c);
		    IFDEBUG printf("King is %s\n",
				   attacked ? "attacked" : "not attacked");
		    return !attacked;
		  });
	  });
  }

  
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
};

#endif
