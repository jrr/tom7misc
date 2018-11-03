
#ifndef __PGN_H
#define __PGN_H

#include <unordered_map>
#include <vector>
#include <string>

#include "re2.h"

struct PGN {

  enum Result {
    WHITE_WINS,
    BLACK_WINS,
    DRAW,
    OTHER,
  };

  // If you are parsing a large number of PGNs, it is slightly
  // faster to make a PGNParser instance and reuse it.
  static bool Parse(const std::string &s, PGN *pgn);
  
  std::unordered_map<std::string, std::string> meta;
  struct Move {
    Move(std::string m) : move(std::move(m)) {}
    // The actual move, like "Nxh4".
    std::string move;
    // TODO: If present, annotations like clock, eval.
  };
  // The moves of the game. White moves are at even indices.
  // Does not include the terminating event like 1-0.
  std::vector<Move> moves;
  Result result;
};

struct PGNParser {
  // Parses a subset of the PGN language. Returns false upon failure.
  bool Parse(const std::string &s, PGN *pgn) const;
  PGNParser();
  
private:
  const RE2 meta_line_re, move_re, end_re;
};

#endif
