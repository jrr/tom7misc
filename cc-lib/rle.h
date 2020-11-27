// Run Length Encoding compression. This is an extremely simple
// byte-based format. Each record starts with a single control byte
// that either indicates a run or anti-run. When a run of length n,
// then the next byte is repeated n times. When an anti-run of length
// n, the next n bytes are output verbatim. Compression is currently
// greedy, though it may be possible to get better ratios using a
// dynamic programming approach.
//
// Since a run or anti-run of length 0 is strictly wasteful, there is
// some subtlety to the coding of the control byte; see the
// implementation for details.

#ifndef _CC_LIB_RLE_H
#define _CC_LIB_RLE_H

#include <vector>
#include <cstdint>

struct RLE {
  using uint8 = uint8_t;
  static constexpr uint8 DEFAULT_CUTOFF = 128;

  static std::vector<uint8> Compress(const std::vector<uint8> &in);
  // Aborts if encoding is invalid.
  static std::vector<uint8> Decompress(const std::vector<uint8> &in);

  // Same, but give the maximum anti-run length; this trades off the
  // efficiency of representing runs with the efficiency of represting
  // anti-runs (with all of the left-over lengths). Encoding and
  // decoding must agree on this value. Note that since a run or
  // anti-run of length 0 is strictly wasteful (of the next byte),
  // we treat every value as a run or anti-run of length value+1.
  static std::vector<uint8> CompressEx(const std::vector<uint8> &in,
				       uint8 run_cutoff);
  // Returns true on success, clears and modifies out to contain the
  // decoded bytes. On failure, out will be in a valid but unspecified
  // state.
  static bool DecompressEx(const std::vector<uint8> &in,
			   uint8 run_cutoff,
			   std::vector<uint8> *out);
};

#endif
