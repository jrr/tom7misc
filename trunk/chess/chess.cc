
#include "chess.h"

#include <string>
#include <cstdint>

using namespace std;

using Move = Position::Move;

// XXX TODO: Delete all this debugging printing.
#define IFDEBUG if (true) {} else
// #define IFDEBUG 

bool Position::ParseFEN(const char *fen, Position *pos) {
  int idx = 0;
    
  // First part fills in the pieces.
  auto InitBoard =
    [fen, pos, &idx]() {
      int row = 0, col = 0;
      for (;; idx++) {
	const char c = fen[idx];
	if (c == 0) return false;

	if (col == 8) {
	  if (row == 7) {
	    // This is the only successful termination.
	    idx++;
	    return c == ' ';
	  } else {
	    if (c != '/')
	      return false;

	    col = 0;
	    row++;
	  }
	} else {
	  if (c >= '1' && c <= '8') {
	    int n = c - '0';
	    while (n--) {
	      if (col > 7)
		return false;
	      pos->SetPiece(row, col, EMPTY);
	      col++;
	    }
	  } else {
	    uint8 p = 0;
	    switch (c) {
	    case 'R': p = WHITE | ROOK; break;
	    case 'r': p = BLACK | ROOK; break;
	    case 'N': p = WHITE | KNIGHT; break;
	    case 'n': p = BLACK | KNIGHT; break;
	    case 'B': p = WHITE | BISHOP; break;
	    case 'b': p = BLACK | BISHOP; break;
	    case 'Q': p = WHITE | QUEEN; break;
	    case 'q': p = BLACK | QUEEN; break;
	    case 'K': p = WHITE | KING; break;
	    case 'k': p = BLACK | KING; break;
	    case 'P': p = WHITE | PAWN; break;
	    case 'p': p = BLACK | PAWN; break;
	    default: return false;
	    }
	    pos->SetPiece(row, col, p);
	    col++;
	  }
	}
      }
    };

  auto InitMeta =
    [fen, pos, &idx]() {
      const char whose_move = fen[idx++];
      switch (whose_move) {
      case 'w':
	pos->bits = 0u;
	break;
      case 'b':
	pos->bits = BLACK_MOVE;
	break;
      default:
	return false;
      }

      if (fen[idx++] != ' ')
	return false;

      for (;;) {
	const char c = fen[idx++];
	switch (c) {
	default:
	  // This includes the NUL terminator.
	  return false;
	  
	case 'K':
	  if (pos->PieceAt(7, 7) != (WHITE | ROOK))
	    return false;
	  pos->SetPiece(7, 7, WHITE | C_ROOK);
	  break;
	case 'Q':
	  if (pos->PieceAt(7, 0) != (WHITE | ROOK))
	    return false;
	  pos->SetPiece(7, 0, WHITE | C_ROOK);
	  break;
	case 'k':
	  if (pos->PieceAt(0, 7) != (BLACK | ROOK))
	    return false;
	  pos->SetPiece(0, 7, BLACK | C_ROOK);
	  break;
	case 'q':
	  if (pos->PieceAt(0, 0) != (BLACK | ROOK))
	    return false;
	  pos->SetPiece(0, 0, BLACK | C_ROOK);
	  break;

	case '-':
	  // Used when the field is blank.
	  break;
	  
	case ' ':
	  break;
	}
	if (c == ' ') break;
      }

      // TODO: en passant state
      // (ignoring move counts here.)
	
      return true;
    };
    
  return InitBoard() && InitMeta();
}

char Position::HumanPieceChar(uint8 p) {
  const char lower = (p & BLACK) ? 32 : 0;
  switch (p & TYPE_MASK) {
  case PAWN: return 'P' | lower;
  case KNIGHT: return 'N' | lower;
  case BISHOP: return 'B' | lower;
  case C_ROOK:
  case ROOK: return 'R' | lower;
  case QUEEN: return 'Q' | lower;
  case KING: return 'K' | lower;
  case EMPTY: return ' ';
  default: return '?';
  }
}

string Position::HumanPieceString(uint8 p) {
  string s = " ";
  s[0] = HumanPieceChar(p);
  return s;
}

char Position::DebugPieceChar(uint8 p) {
  const char lower = (p & BLACK) ? 32 : 0;
  switch (p & TYPE_MASK) {
  case PAWN: return 'P' | lower;
  case KNIGHT: return 'N' | lower;
  case BISHOP: return 'B' | lower;
  case C_ROOK: return 'C' | lower;
  case ROOK: return 'R' | lower;
  case QUEEN: return 'Q' | lower;
  case KING: return 'K' | lower;
  case EMPTY: return '-';
  default: return '?';
  }
}

const char *Position::HTMLEntity(uint8 p) {
  if (p & BLACK) {
    switch (p & TYPE_MASK) {
    case PAWN: return "&#9823;";
    case KNIGHT: return "&#9822;";
    case BISHOP: return "&#9821;";
    case C_ROOK:
    case ROOK: return "&#9820;";
    case QUEEN: return "&#9819;";
    case KING: return "&#9818;";
    case EMPTY: return " ";
    default: return "?";
    }
  } else {
    switch (p & TYPE_MASK) {
    case PAWN: return "&#9817;";
    case KNIGHT: return "&#9816;";
    case BISHOP: return "&#9815;";
    case C_ROOK: 
    case ROOK: return "&#9814;";
    case QUEEN: return "&#9813;";
    case KING: return "&#9812;";
    case EMPTY: return " ";
    default: return "?";
    }
  }
}

std::string Position::BoardString() const {
  string ret;
  for (int r = 0; r < 8; r++) {
    string row = ". . . . . . . .\n";
    for (int c = 0; c < 8; c++) {
      uint8 p = PieceAt(r, c);
      row[c * 2] = DebugPieceChar(p);
    }
    ret += row;
  }
  // TODO: ep state?
  return ret;
}

bool Position::ParseMove(const char *m, Move *move) {
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

bool Position::IsLegal(Move m) {
  const bool blackmove = !!(bits & BLACK_MOVE);
  const uint8 my_color = blackmove ? BLACK : WHITE;
  const uint8 your_color = blackmove ? WHITE : BLACK;

  IFDEBUG printf("IsLegal? %d,%d -> %d,%d",
		 m.src_row, m.src_col,
		 m.dst_row, m.dst_col);
  IFDEBUG if (m.promote_to) printf(" =%d", m.promote_to);
  IFDEBUG printf("\n");

  if (m.src_row >= 8) return false;
  if (m.src_col >= 8) return false;
  if (m.dst_row >= 8) return false;
  if (m.dst_col >= 8) return false;
  
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
	if ((m.promote_to & COLOR_MASK) != BLACK)
	  return false;
	if (!LegalPromotion(m.promote_to & TYPE_MASK))
	  return false;
      } else {
	if (m.promote_to != 0)
	  return false;
      }
    } else {
      if (m.dst_row == 0) {
	if ((m.promote_to & COLOR_MASK) != WHITE)
	  return false;
	if (!LegalPromotion(m.promote_to & TYPE_MASK))
	  return false;
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

bool Position::Attacked(int r, int c) const {
  const bool blackmove = !!(bits & BLACK_MOVE);
  const uint8 attacker_color = blackmove ? WHITE : BLACK;

  IFDEBUG printf("Attacked %d,%d? With board:\n%s\n",
		 r, c, BoardString().c_str());

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

  // Now check all the queen move vectors (moving outward from the
  // queried square) for opponent pieces. This covers all the
  // non-jumping piece types -- note that for this purpose, king is
  // equivalently a queen that can only move 1 square!
  for (const int dr : { -1, 0, 1 }) {
    for (const int dc : { -1, 0, 1 }) {
      // Skip the degenerate vector.
      if (dr == 0 && dc == 0) continue;

      for (int rr = r + dr, cc = c + dc;
	   rr >= 0 && cc >= 0 && rr < 8 && cc < 8;
	   rr += dr, cc += dc) {
	const uint8 p = PieceAt(rr, cc);
	IFDEBUG printf("v %d,%d = %d,%d (%c)\n",
		       dr, dc, rr, cc, DebugPieceChar(p));

	if ((p & COLOR_MASK) == attacker_color) {
	  switch (p & TYPE_MASK) {
	  case PAWN:
	    // Ignoring en passant (which is not handled by this
	    // function), pawns can only capture diagonally one
	    // square, and only in a single direction.

	    // Leaving -1 and 1 as valid.
	    if (dc == 0)
	      break;

	    // And we have to be exactly one row removed in the
	    // correct direction for the color. Note several
	    // ways to get this sign wrong: We are tracing from
	    // the attacked piece towards the attacked; rows are
	    // indexed in the opposite order as in algebraic
	    // notation, etc.
	    if (attacker_color == BLACK) {
	      if (rr - r == -1) return true;
	    } else {
	      if (rr - r == 1) return true;
	    }
	    
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

void Position::ApplyMove(Move m) {
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
  // Note: This updates the position of the king if source_piece is
  // one of the two kings (and we are using king indexing).
  SetPiece(m.dst_row, m.dst_col, source_piece);

  // And pass to the other player.
  bits ^= BLACK_MOVE;
}


bool Position::NotIntoCheck(Move m) {
  // For the sake of testing for check, we can ignore promotion.
  const uint8 p = PieceAt(m.src_row, m.src_col);
  // If we are moving the king, the inner SetPiece manages to modify
  // the location of the king on the way into the Attacked test, and
  // the outer one restores it on the way out.
  return
    SetExcursion(
	m.src_row, m.src_col, EMPTY,
	[&]() {
	  return
	    SetExcursion(
		m.dst_row, m.dst_col, p,
		[&]() {
		  int r, c;
		  std::tie(r, c) = GetCurrentKing();
		  IFDEBUG printf("King at %d,%d\n", r, c);
		  bool attacked = Attacked(r, c);
		  IFDEBUG printf("King is %s\n",
				 attacked ? "attacked" : "not attacked");
		  return !attacked;
		});
	});
}

string Position::ToFEN(int halfmove_clock, int fullmove_number) const {
  string ret;

  for (int r = 0; r < 8; r++) {
    int empty_run = 0;
    auto FlushRun =
      [&ret, &empty_run]() {
	if (empty_run > 0) {
	  ret.push_back('0' + empty_run);
	  empty_run = 0;
	}
      };
    for (int c = 0; c < 8; c++) {
      uint8 p = PieceAt(r, c);
      if (p == EMPTY) {
	empty_run++;
      } else {
	FlushRun();
	ret.push_back(HumanPieceChar(p));
      }
    }
    FlushRun();
    if (r != 7) ret.push_back('/');
  }

  // Whose move?
  ret.push_back(' ');
  ret.push_back(BlackMove() ? 'b' : 'w');
  ret.push_back(' ');

  {
    string cast;
    if (CanStillCastle(true, true)) cast.push_back('K');
    if (CanStillCastle(true, false)) cast.push_back('Q');
    if (CanStillCastle(false, true)) cast.push_back('k');
    if (CanStillCastle(false, false)) cast.push_back('q');
    if (cast.empty()) cast = "-";
    ret += cast;
  }

  ret.push_back(' ');
  if (0 != (bits & DOUBLE)) {
    ret.push_back('a' + (bits & PAWN_COL));
    // If it's black's move, then white just
    // did the double move, so the spot "behind"
    // the pawn is on the 3rd rank (else 6th).
    ret.push_back(BlackMove() ? '3' : '6');
  } else {
    ret.push_back('-');
  }

  ret.push_back(' ');
  ret += to_string(halfmove_clock);
  ret.push_back(' ');
  ret += to_string(fullmove_number);

  return ret;
}

bool Position::IsInCheck() {
  int kingr, kingc;
  std::tie(kingr, kingc) = GetCurrentKing();
  return Attacked(kingr, kingc);
}

bool Position::IsMated() {
  return IsInCheck() && !HasLegalMoves();
}

bool Position::IsCastling(Move m) const {
  if (m.src_col != 4)
    return false;
  if (m.dst_col != 6 &&
      m.dst_col != 2)
    return false;

  if (m.src_row != 0 &&
      m.src_row != 7)
    return false;

  return (PieceAt(m.src_row, m.src_col) & TYPE_MASK) == KING;
}

bool Position::IsEnPassant(Move m) const {
  // Must be capturing.
  if (m.src_col == m.dst_col) return false;

  // Must be a pawn moving.
  const uint8 p = PieceAt(m.src_row, m.src_col);
  if ((p & TYPE_MASK) != PAWN) return false;

  // And the destination square must be empty.
  const uint8 d = PieceAt(m.dst_row, m.dst_col);
  return d == EMPTY;
}

string Position::LongMoveString(Move m) const {
  if (IsCastling(m)) {
    return m.dst_col < m.src_col ? "O-O-O" : "O-O";
  }

  const bool capture = IsEnPassant(m) ||
    PieceAt(m.dst_row, m.dst_col) != EMPTY;

  const uint8 p = PieceAt(m.src_row, m.src_col) & TYPE_MASK;
  const string piece = p == PAWN ? "" : HumanPieceString(p);

  const string promote =
    m.promote_to ? "=" + HumanPieceString(m.promote_to) : "";

  char buf[16] = {};
  sprintf(buf,
	  "%s%c%c%s%c%c%s",
	  piece.c_str(),
	  'a' + m.src_col,
	  '1' + (7 - m.src_row),
	  capture ? "x" : "",
	  'a' + m.dst_col,
	  '1' + (7 - m.dst_row),
	  promote.c_str());
  return (string)buf;
}

string Position::ShortMoveString(Move m) {
  if (IsCastling(m)) {
    return m.dst_col < m.src_col ? "O-O-O" : "O-O";
  }

  const bool capture = IsEnPassant(m) ||
    PieceAt(m.dst_row, m.dst_col) != EMPTY;

  // Should be uppercase.
  const string promote =
    m.promote_to ? "=" + HumanPieceString(m.promote_to & TYPE_MASK) : "";
  
  // type of piece being moved
  const uint8 type = PieceAt(m.src_row, m.src_col) & TYPE_MASK;
  if (type == PAWN) {
    // If a pawn, the result is never ambiguous!

    string ret;
    if (capture) {
      ret += ('a' + m.src_col);
      ret += 'x';
    }
    
    ret += ('a' + m.dst_col);
    ret += ('1' + (7 - m.dst_row));
    ret += promote;
    return ret;

  } else {
    // If a normal piece (say, a rook), then check to see if the move
    // is ambiguous. Find all rooks on the board that could legally
    // move to the same square.
    bool need_disamb = false;
    bool same_row = false, same_col = false;
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	if ((r != m.src_row || c != m.src_col) &&
	    (PieceAt(r, c) & TYPE_MASK) == type) {
	  Move alt = m;
	  alt.src_row = r;
	  alt.src_col = c;
	  if (IsLegal(alt)) {
	    need_disamb = true;

	    if (c == m.src_col)
	      same_col = true;

	    if (r == m.src_row)
	      same_row = true;
	  }
	}
      }
    }
    
    string ret = HumanPieceString(type);
    if (need_disamb) {
      if (same_col && same_row) {
	ret += ('a' + m.src_col);
	ret += ('1' + (7 - m.src_row));
      } else if (same_col) {
	ret += ('1' + (7 - m.src_row));
      } else {
	// (Also prefer column disambiguation in the case where it is
	// neither same row nor same col.)
	ret += ('a' + m.src_col);
      }
    }
    if (capture)
      ret += 'x';

    ret += ('a' + m.dst_col);
    ret += ('1' + (7 - m.dst_row));
    ret += promote;
    return ret;
  }
}

namespace {
// Various ways of collecting moves, used in the template below.
// If Push returns true, we should stop collecting.

struct VectorMC {
  VectorMC(std::vector<Move> *moves) : moves(moves) {}
  bool Push(Move m) {
    moves->push_back(m);
    return false;
  }
  std::vector<Move> *moves = nullptr;
};

struct AnyMC {
  bool Push(Move m) {
    any = true;
    return true;
  }
  bool any = false;
};

struct OneMC {
  bool Push(Move m) {
    count++;
    // Once we reach two, we know the answer will be false, so stop.
    return count > 1;
  }
  int count = 0;
};

struct CountMC {
  bool Push(Move m) {
    count++;
    return false;
  }
  int count = 0;
};

#define MOVEGEN_STOP_PIECE true

template<class MoveCollector>
static void CollectLegalMoves(Position &pos, MoveCollector &mc) {
  const bool blackmove = pos.BlackMove();
  const uint8 my_color = blackmove ? Position::BLACK : Position::WHITE;
  
#define TRY_MOVE(move) do {		 \
    if (pos.IsLegal(m)) {		 \
      if (mc.Push(m)) return;            \
    }					 \
  } while(0)

  // Note that there are several places in this function where we create
  // out of bounds moves, either by e.g. adding a small number to column 7
  // or subtracting from row 0 (which wraps around since the fields
  // are unsigned). This is safe since IsLegalMove checks that moves are
  // in bounds, but something to be careful about.
  for (int srcr = 0; srcr < 8; srcr++) {
    for (int srcc = 0; srcc < 8; srcc++) {
      const uint8 p = pos.PieceAt(srcr, srcc);
      if ((p & Position::COLOR_MASK) == my_color) {
	Move m;
	m.src_row = srcr;
	m.src_col = srcc;
	
	switch (p & Position::TYPE_MASK) {
	case Position::EMPTY:
	  break;

	case Position::PAWN: {
	  int dr = blackmove ? 1 : -1;
	  
	  if (srcr == (blackmove ? 1 : 6)) {
	    // Try double moves.
	    m.dst_col = srcc;
	    m.dst_row = srcr + dr + dr;
	    TRY_MOVE(m);
	  }

	  // Covers regular advancement, capture, promotion.
	  for (int dc = -1; dc <= 1; dc++) {
	    m.dst_col = srcc + dc;
	    m.dst_row = srcr + dr;
	    if (m.dst_row == (blackmove ? 7 : 0)) {
	      m.promote_to = my_color | Position::KNIGHT;
	      TRY_MOVE(m);
	      m.promote_to = my_color | Position::BISHOP;
	      TRY_MOVE(m);
	      m.promote_to = my_color | Position::ROOK;
	      TRY_MOVE(m);
	      m.promote_to = my_color | Position::QUEEN;
	      TRY_MOVE(m);
	      m.promote_to = 0;
	    } else {
	      TRY_MOVE(m);
	    }
	  }
	  break;
	}

	case Position::KNIGHT:
	  for (const int udr : { -1, 1 }) {
	    for (const int udc : { -1, 1 }) {
	      // (1,2) then (2,1).
	      m.dst_col = srcc + 2 * udc;
	      m.dst_row = srcr + udr;
	      TRY_MOVE(m);

	      m.dst_col = srcc + udc;
	      m.dst_row = srcr + 2 * udr;
	      TRY_MOVE(m);
	    }
	  }
	  break;

	case Position::C_ROOK:
	case Position::ROOK:

	  // Horizontal.
	  m.dst_row = srcr;
	  for (int dstc = 0; dstc < 8; dstc++) {
	    m.dst_col = dstc;
	    TRY_MOVE(m);
	  }

	  // Vertical.
	  m.dst_col = srcc;
	  for (int dstr = 0; dstr < 8; dstr++) {
	    m.dst_row = dstr;
	    TRY_MOVE(m);
	  }

	  break;
	  
	case Position::BISHOP:

	  for (const int udr : { -1, 1 }) {
	    for (const int udc : { -1, 1 }) {
	      int dr = m.src_row + udr;
	      int dc = m.src_col + udc;
	      while (dr >= 0 && dc >= 0 && dr < 8 && dc < 8) {
		m.dst_row = dr;
		m.dst_col = dc;
		TRY_MOVE(m);
		#if MOVEGEN_STOP_PIECE
		if (pos.PieceAt(dr, dc) != Position::EMPTY)
		  break;
		#endif
		dr += udr;
		dc += udc;
	      }
	    }
	  }
	  break;
	  
	case Position::QUEEN:

	  // Horizontal.
	  m.dst_row = srcr;
	  for (int dstc = 0; dstc < 8; dstc++) {
	    m.dst_col = dstc;
	    TRY_MOVE(m);
	  }

	  // Vertical.
	  m.dst_col = srcc;
	  for (int dstr = 0; dstr < 8; dstr++) {
	    m.dst_row = dstr;
	    TRY_MOVE(m);
	  }

	  // Diagonals.
	  for (const int udr : { -1, 1 }) {
	    for (const int udc : { -1, 1 }) {
	      int dr = m.src_row + udr;
	      int dc = m.src_col + udc;
	      while (dr >= 0 && dc >= 0 && dr < 8 && dc < 8) {
		m.dst_row = dr;
		m.dst_col = dc;
		TRY_MOVE(m);
		#if MOVEGEN_STOP_PIECE
		if (pos.PieceAt(dr, dc) != Position::EMPTY)
		  break;
		#endif
		dr += udr;
		dc += udc;
	      }
	    }
	  }

	  break;
	  
	case Position::KING:
	  if (srcr == (blackmove ? 0 : 7) &&
	      srcc == 4) {
	    // Try castling too.
	    m.dst_col = 2;
	    m.dst_row = srcr;
	    TRY_MOVE(m);

	    m.dst_col = 6;
	    m.dst_row = srcr;
	    TRY_MOVE(m);
	  }
	    
	  for (int dc = -1; dc <= 1; dc++) {
	    for (int dr = -1; dr <= 1; dr++) {
	      if (dc != 0 || dr != 0) {
		m.dst_col = srcc + dc;
		m.dst_row = srcr + dr;
		TRY_MOVE(m);
	      }
	    }
	  }
	  break;
	}
      }
    }
  }
}

}  // namespace

std::vector<Move> Position::GetLegalMoves() {
  std::vector<Move> moves;
  VectorMC vmc{&moves};
  CollectLegalMoves(*this, vmc);
  return moves;
}

bool Position::HasLegalMoves() {
  AnyMC amc;
  CollectLegalMoves(*this, amc);
  return amc.any;
}

int Position::ExactlyOneLegalMove() {
  OneMC omc;
  CollectLegalMoves(*this, omc);
  return omc.count;
}

int Position::NumLegalMoves() {
  CountMC cmc;
  CollectLegalMoves(*this, cmc);
  return cmc.count;
}
