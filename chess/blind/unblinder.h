#ifndef __UNBLINDER_H
#define __UNBLINDER_H

#include <cstdint>
#include <string>
#include <vector>

#include "../chess.h"

// Interface. Should be thread-safe.
struct Unblinder {
  // Utilities.
  static uint64_t Blind(const Position &pos);
  static void Layer64(uint64_t pos, std::vector<float> *v);
  
  // Note that the resulting position may be invalid (e.g. multiple
  // black kings).
  virtual Position Unblind(uint64_t pos) const = 0;

  virtual std::string ModelInfo() const = 0;
  
  // TODO: Nice to have:
  //   - Visualize state to SDL surface or rgba array even.
  //   - Soft version of output (i.e. probabilities)
  
  virtual ~Unblinder() {}
};

#endif
