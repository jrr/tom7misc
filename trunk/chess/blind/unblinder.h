#ifndef __UNBLINDER_H
#define __UNBLINDER_H

#include <cstdint>
#include <string>
#include <vector>

#include "../chess.h"

struct ArcFour;

// Interface. Should be thread-safe.
struct Unblinder {
  // Utilities.
  static uint64_t Blind(const Position &pos);
  static void Layer64(uint64_t pos, std::vector<float> *v);
  
  // Note that the resulting position may be invalid (e.g. multiple
  // black kings). If single_king is true, then constructs positions
  // such that there is exactly on king per side (but could still
  // be invalid because of e.g. mutual check).
  virtual Position Unblind(bool single_king, uint64_t pos) const = 0;

  // Sample n predicted positions that are valid. Valid means:
  //  - It is black's move iff blackmove is true
  //  - There is exactly one king per side
  //  - The current opponent is not in check
  //  - Castling flags are only set if castling could be possible.
  /*
  virtual std::vector<Position> SampleValid(
      ArcFour *rc, uint64_t pos, bool blackmove, int n) const = 0;
  */
  
  virtual std::string ModelInfo() const = 0;
  
  // TODO: Nice to have:
  //   - Visualize state to SDL surface or rgba array even.
  //   - Soft version of output (i.e. probabilities)
  
  virtual ~Unblinder() {}
};

#endif
