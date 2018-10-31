
#include "pgn.h"

#include <string>
#include <vector>

#include "re2.h"
#include "base/logging.h"

using namespace std;
using StringPiece = re2::StringPiece;

// static
bool PGN::Parse(const string &s, PGN *pgn) {
  // Matches the text inside double-quotes in PGN. Repeatedly,
  // anything but a backslash or double quote, or an escaped
  // double-quote, or an escaped backslash.
# define ANY_NOT_CLOSING_QUOTE "(?:[^\n\"\\\\]|\\\"|\\\\)*"
  LazyRE2 meta_line_re = { " *\\[([A-Za-z0-9]+) +\""
			   "(" ANY_NOT_CLOSING_QUOTE ")"
			   "\"\\] *\n" };

  CHECK(RE2::FullMatch("fo\\\"oo", ANY_NOT_CLOSING_QUOTE));
  
  CHECK(RE2::FullMatch("[hello \"world\"]\n",
		       *meta_line_re));
  
  re2::StringPiece input(s);
  string key, value;
  
  while (RE2::Consume(&input, *meta_line_re, &key, &value)) {
    // TODO: Actually need to unquote " and \ in value.
    // printf("[%s] [%s]\n", key.c_str(), value.c_str());
    pgn->meta[key] = value;
  }

  // Now read moves.
  // Since the game can end on white's move, we simply allow
  // a move marker before white or black's move.
  
  LazyRE2 move_re = {
		     // Like "18. " or "9... "
		     "\\s*(?:[0-9]+(?:[.]|[.][.][.])\\s+)?"
		     // The actual move text. This
		     // includes characters for e.g. O-O-O#,
		     // Nh3?!, and terminators like 1/2-1/2, *, 0-1.
		     "([-xKQPNRBa-h1-8=+!?#O0/*]+)"
		     // Post-move comments.
		     "\\s*((?:{[^}]*})?)" };

  // Just whitespace...
  LazyRE2 end_re = { "\\s*" };
  
  string move, post;
  while (RE2::Consume(&input, *move_re, &move, &post)) {
    // printf("[%s] + [%s]\n", move.c_str(), post.c_str());

    if (move == "1-0" || move == "1/2-1/2" || move == "0-1" || move == "*")
      return true;
      
    pgn->moves.emplace_back(move);
  }

  // Make sure we consumed the whole input.
  return RE2::Consume(&input, *end_re) && input.empty();
}
