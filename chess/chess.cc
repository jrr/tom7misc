
#include "chess.h"

#include <string>
#include <cstdint>

using namespace std;

// TODO: Move some functions (that should not be inlined) into this cc
// file.

// static
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

// static
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

// static
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
