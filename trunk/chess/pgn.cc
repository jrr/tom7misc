
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
  pgn->result = PGN::Result::OTHER;
  
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
      pgn->result = PGN::Result::WHITE_WINS;
      return true;
    } else if (move == "1/2-1/2") {
      pgn->result = PGN::Result::DRAW;
      return true;
    } else if (move == "0-1") {
      pgn->result = PGN::Result::BLACK_WINS;
      return true;
    } else if (move == "*") {
      pgn->result = PGN::Result::OTHER;
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

int PGN::MetaInt(const string &key, int default_value) const {
  auto it = meta.find(key);
  if (it == meta.end()) return default_value;
  return atoi(it->second.c_str());
}

PGN::Termination PGN::GetTermination() const {
  auto it = meta.find("Termination");
  if (it == meta.end()) return PGN::Termination::OTHER;

  const string &term = it->second;
  if (term == "Normal") return PGN::Termination::NORMAL;
  else if (term == "Time forfeit") return PGN::Termination::TIME_FORFEIT;
  else if (term == "Abandoned") return PGN::Termination::ABANDONED;

  return PGN::Termination::OTHER;
}

std::pair<int, int> PGN::GetTimeControl() const {
  auto it = meta.find("TimeControl");
  if (it == meta.end()) return {0, 0};

  const string &ctl = it->second;
  if (ctl == "-") return {0, 0};

  size_t plus = ctl.find('+');
  if (plus == string::npos) return {0, 0};
  return std::make_pair(atoi(ctl.substr(0, plus).c_str()),
			atoi(ctl.substr(plus + 1, string::npos).c_str()));
}

PGN::TimeClass PGN::GetTimeClass() const {
  auto it = meta.find("TimeControl");
  if (it == meta.end()) return TimeClass::UNKNOWN;

  const string &ctl = it->second;
  if (ctl == "-") return TimeClass::CORRESPONDENCE;

  size_t plus = ctl.find('+');
  if (plus == string::npos) return TimeClass::UNKNOWN;

  
  const int time_start = atoi(ctl.substr(0, plus).c_str());
  const int time_inc = atoi(ctl.substr(plus + 1, string::npos).c_str());

  // lichess definitions; see
  // lichess.org/qa/47/how-are-classical-blitz-and-bullet-defined
  // and note that avg moves changed from 30 to 40 in 2015.
  const int total_seconds = time_start + time_inc * 40;

  if (total_seconds < 30) return TimeClass::ULTRA_BULLET;
  if (total_seconds < 3 * 60) return TimeClass::BULLET;
  if (total_seconds < 8 * 60) return TimeClass::BLITZ;
  if (total_seconds < 25 * 60) return TimeClass::RAPID;
  return TimeClass::CLASSICAL;
}
