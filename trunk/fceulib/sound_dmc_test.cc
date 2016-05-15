
// This little mini test helped me simplify the DMC accumulation loop
// in sound.cc (when sound is turned off) to equivalent code with no
// loop. It's sort of fiddly since there are negative moduluses and
// things like that...

#include <cstdint>
#include <utility>
#include <tuple>

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"

using int32 = int32_t;
using uint8 = uint8_t;

namespace {
struct State {
  int32 acc = 1;
  uint32 period = 0;
  uint8 bitcount = 0;
  uint8 havedma = 0;
};
}

static string PrintState(const State &a) {
  return StringPrintf("[acc %d, per %d, bit %02x, have %02x]",
		      a.acc, a.period, a.bitcount, a.havedma);
}

static State Original(State s, int cycles) {
  s.acc -= cycles;
  
  while (s.acc <= 0) {
    s.acc += s.period;
    s.bitcount = (s.bitcount + 1) & 7;

    if (s.bitcount == 0) {
      s.havedma = 0;
    }
  }
  
  return s;
}

static State Simplified(State s, int cycles) {
  int32 tmp = s.acc - cycles;
  
  while (tmp <= 0) {
    tmp += s.period;
    s.bitcount = (s.bitcount + 1) & 7;

    if (s.bitcount == 0) {
      s.havedma = 0;
    }
  }

  // a = (a/b)*b + a%b
  //
  // let u = (acc - cycles)
  // then u + k * period  =  f   (where 0 < f <= period)
  // k is the number of executions of the loop   (k >= 0)
  // f is the final accumulator we want.
  //
  // integer division by some k,

  // ok so -2 % 54       ... (want 52)
  // and we know   (-2 div 54) * 54 + -2 % 54  == -2.
  // this is                0 * 54 + -2 % 54 == -2
  // so that means                   -2 % 54 == -2?
  
  int32 t = s.acc - cycles;
  // printf("t: %d - %d = %d\n", s.acc, cycles, t);
  if (t <= 0) {
    // be careful about signedness here!
    int32 u = t % (int)s.period;
    // printf("u: %d %% %d = %d\n", t, (int)s.period, u);

    // Now u is a negative number (or zero). This is great
    // because we want a result strictly greater than 0.
    s.acc = u + (int)s.period;
    // s.acc = ((s.acc - cycles) + (s.period << 9)) % s.period;
    // if (s.acc == 0) s.acc = (int)s.period;
  } else {
    s.acc = t;
  }

  return s;
}


// From x6502.
static constexpr uint8 CycTable[256] = {
    /*0x00*/ 7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
    /*0x10*/ 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    /*0x20*/ 6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
    /*0x30*/ 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    /*0x40*/ 6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
    /*0x50*/ 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    /*0x60*/ 6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
    /*0x70*/ 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    /*0x80*/ 2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    /*0x90*/ 2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
    /*0xA0*/ 2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    /*0xB0*/ 2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
    /*0xC0*/ 2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    /*0xD0*/ 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    /*0xE0*/ 2, 6, 3, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    /*0xF0*/ 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
};

// from sound.cc, mergint the ntsc and pal dma period tables
static constexpr uint32 NTSCandPALtables[0x20] = {
    428, 380, 340, 320, 286, 254, 226, 214,
    190, 160, 142, 128, 106, 84,  72,  54,
    398, 354, 316, 298, 276, 236, 210, 198,
    176, 148, 132, 118, 98, 78, 66, 50,
};

static std::pair<State, int32> RandomExample(ArcFour *rc) {
  // Cycles should be positive (in fact, it looks like it is always
  // at least 2) because it always contains the value of the last
  // instruction we executed (including perhaps some additional cycles
  // caused by DMA or off-page reads).
  const int32 cycles = CycTable[rc->Byte()] +
    // off-page read
    ((rc->Byte() < 12) ? 1 : 0) +
    // Write to $4014, triggering long dma
    ((rc->Byte() < 12) ? 512 : 0);

  State example;
  // Accumulator is always positive by invariant. In normal cases
  // it's also less than the period, but the period can switch
  // because of a DMC status write.
  example.acc =
    (rc->Byte() < 128) ?
    (1 + NTSCandPALtables[rc->Byte() & 31]) :
    (1 + (rc->Byte() & 127));

  // The period can change any time via writes to the DMC status
  // registers. It's always one of the values in the NTSC or
  // PAL tables, though.
  example.period = NTSCandPALtables[rc->Byte() & 31];

  // Might consider letting this use the full width, but only
  // test the low 3.
  example.bitcount = rc->Byte() & 7;
  example.havedma = (rc->Byte() >= 12);

  return make_pair(example, cycles);
}

int main() {
  ArcFour rc{"dmctest2"};
  printf("2 / 54 = %d. -2 / 54 = %d. 2 / -54 = %d.\n",
	  2 / 54, -2 / 54, 2 / -54);
  printf("-2 %% 54 = %d.\n", -2 % 54);
#define COUNT 10000000
  for (int i = 0; i < COUNT; i++) {
    State example;
    int32 cycles;
    std::tie(example, cycles) = RandomExample(&rc);

    State expected = Original(example, cycles);
    State actual = Simplified(example, cycles);

    CHECK(PrintState(expected) == PrintState(actual)) <<
      "On this example:\n" << 
      PrintState(example) << "  for " << cycles << " cycles\n"
      "\nNot equal: \n" <<
      PrintState(expected) << "  (expected) vs.\n" <<
      PrintState(actual) << "  (actual) on input:\n";


    if (0 == i % 400000) {
      printf("%d%%\n", (int)((100 * i) / COUNT));
    }
  }
  printf("OK.\n");
}
