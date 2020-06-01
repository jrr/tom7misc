#include <stdio.h>
#include <cstdint>
#include <utility>
#include <algorithm>
#include <unordered_set>

#include "base/logging.h"
#include "cryptrand.h"

using uint64 = uint64_t;
using int64 = int64_t;
using uint8 = uint8_t;
using uint16 = uint16_t;

int main(int argc, char **argv) {
  CryptRand cr;
  uint64 w = cr.Word64();
  printf("This should be a different value each time: %llx\n", w);

  uint64 w2 = cr.Word64();
  CHECK(w != w2) << "Got same 64-bit value twice in a row, "
    "which should basically never happen:\n" << w << "\n" << w2;
  
  // Really simple distribution tests.
  std::unordered_map<uint8, int64> bits;
  std::unordered_map<uint8, int64> bytes;

  auto IncBits = [&bits](uint8 b) {
      for (int i = 0; i < 8; i++) {
	bits[b & 1]++;
	b >>= 1;
      }
  };

  static constexpr int TRIALS = 1000000;
  for (int i = 0; i < TRIALS; i++) {
    if (i % 1000 == 0) printf("%d/%d (%.2f%%)...\n",
			      i, TRIALS, (i * 100.0) / TRIALS);
    uint64 ww = cr.Word64();
    uint8 a = ww & 0xFF; ww >>= 8;
    uint8 b = ww & 0xFF; ww >>= 8;
    uint8 c = ww & 0xFF; ww >>= 8;
    uint8 d = ww & 0xFF; ww >>= 8;
    uint8 e = ww & 0xFF; ww >>= 8;
    uint8 f = ww & 0xFF; ww >>= 8;
    uint8 g = ww & 0xFF; ww >>= 8;
    uint8 h = ww & 0xFF; ww >>= 8;
    bytes[a]++;
    bytes[b]++;
    bytes[c]++;
    bytes[d]++;
    bytes[e]++;
    bytes[f]++;
    bytes[g]++;
    bytes[h]++;
    for (uint8 byte : {a, b, c, d, e, f, g, h})
      IncBits(byte);
  }

  printf("0 bits: %lld (%.4f%%)\n"
	 "1 bits: %lld (%.4f%%)\n",
	 bits[false], (bits[false] * 100.0) / (TRIALS * 64.0),
	 bits[true], (bits[true] * 100.0) / (TRIALS * 64.0));

  std::vector<std::pair<uint8, int64>> all;
  for (auto [byte, count] : bytes) all.emplace_back(byte, count);
  CHECK(all.size() == 256) << "Some bytes were never output after "
    "a million 64-bit words, which should basically never happen!";
  std::sort(all.begin(),
	    all.end(),
	    [](const std::pair<uint8, int64> a,
	       const std::pair<uint8, int64> b) {
	      if (a.second == b.second)
		return a.first < b.first;
	      return a.second < b.second;
	    });

  auto PrintOne = [&all](int idx) {
    printf("%02x x %lld (%.2f%%)\n",
	   all[idx].first,
	   all[idx].second, (100.0 * all[idx].second) / (TRIALS * 8.0));
    };
  for (int i = 0; i < 8; i++)
    PrintOne(i);
  printf("...\n");
  for (int i = 0; i < 8; i++)
    PrintOne(256 - 8 + i);
  
  return 0;
}
