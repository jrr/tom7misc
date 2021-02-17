
#ifndef _TOURNAMENT_DB_H
#define _TOURNAMENT_DB_H

#include <utility>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <cstdint>
#include <string>
#include <functional>

// Cell in the matrix. The row plays as white; the col as black.
struct Cell {
  int64_t white_wins = 0, white_losses = 0, draws = 0;
  // As PGN. Empty means no example collected.
  std::string example_win, example_loss, example_draw;
};

struct OutcomeKeyHash {
  std::size_t operator ()(const std::pair<std::string, std::string> &p) const {
    size_t ha = std::hash<std::string>()(p.first);
    size_t hb = std::hash<std::string>()(p.second);
    return (ha * 65537) + hb;
  }
};

// Where key is Player::Name()s as (white,black).
using Outcomes = std::unordered_map<std::pair<std::string, std::string>,
                                    Cell, OutcomeKeyHash>;

struct TournamentDB {
  static Outcomes LoadFromFile(
      const std::string &filename,
      const std::unordered_set<std::string> &ignore = {});
  static void SaveToFile(const Outcomes &outcomes,
                         const std::string &filename);

  // Merge all the outcomes in source into dest, modifying it in place.
  static void MergeInto(const Outcomes &source, Outcomes *dest);
};

#endif
