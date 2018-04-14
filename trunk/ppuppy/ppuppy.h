
#ifndef __PPUPPY_H
#define __PPUPPY_H

#include <cstdint>

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using uint64 = uint64_t;
using int32 = int32_t;
using int64 = int64_t;

// input pins, as wired on red solder board
static constexpr uint8 PIN_RD = 12;
static constexpr uint8 PIN_A0 = 14;
static constexpr uint8 PIN_A1 = 15;
static constexpr uint8 PIN_A2 = 16;
static constexpr uint8 PIN_A3 = 17;
static constexpr uint8 PIN_A4 = 18;
static constexpr uint8 PIN_A5 = 19;
static constexpr uint8 PIN_A6 = 20;
static constexpr uint8 PIN_A7 = 21;
static constexpr uint8 PIN_A8 = 22;
static constexpr uint8 PIN_A9 = 23;
static constexpr uint8 PIN_A13 = 26;

// output pins, wired to bus transciever
static constexpr uint8 POUT_D0 = 2;
static constexpr uint8 POUT_D1 = 3;
static constexpr uint8 POUT_D2 = 4;
static constexpr uint8 POUT_D3 = 5;
static constexpr uint8 POUT_D4 = 6;
// Use 6 next, of course. But 7 is used on address side.

inline uint32 Encode(uint8 byte) {
  static_assert(POUT_D0 == 2, "hard-coded for performance");
  static_assert(POUT_D1 == 3, "hard-coded for performance");
  static_assert(POUT_D2 == 4, "hard-coded for performance");
  static_assert(POUT_D3 == 5, "hard-coded for performance");
  // Just four bits supported right now.
  uint32 word = (uint32)(byte & 15) << 2;
  return word;
}

#endif
