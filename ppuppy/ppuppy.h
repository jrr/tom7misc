
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

// NES controller bits
#define RIGHT    0x01
#define LEFT     0x02
#define DOWN     0x04
#define UP       0x08
#define START    0x10
#define SELECT   0x20
#define B_BUTTON 0x40
#define A_BUTTON 0x80

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
static constexpr uint8 POUT_D5 = 7;
static constexpr uint8 POUT_D6 = 8;
static constexpr uint8 POUT_D7 = 9;

inline uint32 Encode(uint8 byte) {
  static_assert(POUT_D0 == 2, "hard-coded for performance");
  static_assert(POUT_D1 == 3, "hard-coded for performance");
  static_assert(POUT_D2 == 4, "hard-coded for performance");
  static_assert(POUT_D3 == 5, "hard-coded for performance");
  static_assert(POUT_D4 == 6, "hard-coded for performance");
  static_assert(POUT_D5 == 7, "hard-coded for performance");
  static_assert(POUT_D6 == 8, "hard-coded for performance");
  static_assert(POUT_D7 == 9, "hard-coded for performance");
  return (uint32)(byte) << 2;
}

#endif
