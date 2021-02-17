#ifndef _COMMON_H
#define _COMMON_H

#include <string>
#include <vector>
#include <cstdint>
#include <unordered_set>
#include <unordered_map>

#include "util.h"
#include "chess.h"

// Set of positions that are "common."
struct CommonSet {
  explicit CommonSet(const std::string &filename) {
    std::vector<uint64_t> v = Util::ReadUint64File(filename);
    for (const uint64_t w : v) positions.insert(w);
  }

  inline bool Contains(const Position &pos) const {
    return Contains(PositionHash{}(pos));
  }

  inline bool Contains(uint64_t h) const {
    return positions.find(h) != positions.end();
  }

  std::unordered_set<uint64_t> positions;
};

// Positions with move distributions.
struct CommonMap {
  CommonMap() {}
  explicit CommonMap(const std::string &filename);

  // Create map with empty move counts from set.
  explicit CommonMap(const CommonSet &cs) {
    for (const uint64_t w : cs.positions) {
      positions[w] = {};
    }
  }

  // TODO: Read/write populated map.

  void MergeFrom(const CommonMap &other) {
    for (const auto &p : other.positions) {
      const MoveCounts &src = p.second;
      MoveCounts *dest = &positions[p.first];
      for (const auto &c : src) {
        (*dest)[c.first] += c.second;
      }
    }
  }

  void WriteFile(const std::string &filename);

  // Packed move to count.
  // PERF: This can be represented a lot more efficiently.
  using MoveCounts = std::unordered_map<uint16_t, uint32_t>;
  std::unordered_map<uint64_t, MoveCounts> positions;
};

#endif
