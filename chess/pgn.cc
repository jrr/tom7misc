
#include "pgn.h"

#include <string>
#include <vector>

#include "re2.h"
#include "base/logging.h"

using namespace std;
using StringPiece = re2::StringPiece;

#define ANY_NOT_CLOSING_QUOTE "(?:[^\n\"\\\\]|\\\"|\\\\)*"
static constexpr const char *META_LINE_RE =
  " *\\[([A-Za-z0-9]+) +\""
  "(" ANY_NOT_CLOSING_QUOTE ")"
  "\"\\] *\n";

static constexpr const char *MOVE_RE =
  // Like "18. " or "9... "
  "\\s*(?:[0-9]+(?:[.]|[.][.][.])\\s+)?"
  // The actual move text. This
  // includes characters for e.g. O-O-O#,
  // Nh3?!, and terminators like 1/2-1/2, *, 0-1.
  "([-xKQPNRBa-h1-8=+!?#O0/*]+)"
  // Post-move comments.
  "\\s*((?:{[^}]*})?)";

  // Just whitespace...
static constexpr const char *END_RE = "\\s*";

PGNParser::PGNParser() :
  meta_line_re{META_LINE_RE},
  move_re{MOVE_RE},
  end_re{END_RE} {}

// static
bool PGNParser::Parse(const string &s, PGN *pgn) const {
  // Matches the text inside double-quotes in PGN. Repeatedly,
  // anything but a backslash or double quote, or an escaped
  // double-quote, or an escaped backslash.

  re2::StringPiece input(s);
  string key, value;

  // If text ends without termination marker, treat this
  // as OTHER.
  pgn->result = PGN::OTHER;
  
  while (RE2::Consume(&input, meta_line_re, &key, &value)) {
    // TODO: Actually need to unquote " and \ in value.
    // printf("[%s] [%s]\n", key.c_str(), value.c_str());
    pgn->meta[key] = value;
  }

  // Now read moves.
  // Since the game can end on white's move, we simply allow
  // a move marker before white or black's move.
    
  string move, post;
  while (RE2::Consume(&input, move_re, &move, &post)) {
    // printf("[%s] + [%s]\n", move.c_str(), post.c_str());

    if (move == "1-0") {
      pgn->result = PGN::WHITE_WINS;
      return true;
    } else if (move == "1/2-1/2") {
      pgn->result = PGN::DRAW;
      return true;
    } else if (move == "0-1") {
      pgn->result = PGN::BLACK_WINS;
      return true;
    } else if (move == "*") {
      pgn->result = PGN::OTHER;
      return true;
    }
      
    pgn->moves.emplace_back(move);
  }

  // Make sure we consumed the whole input.
  return RE2::Consume(&input, end_re) && input.empty();
}

bool PGN::Parse(const string &s, PGN *pgn) {
  PGNParser parser;
  return parser.Parse(s, pgn);
}
