
#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>

#include <utility>
#include <tuple>
#include <string>
#include <cstdint>
#include <vector>

#include "bcm2835.h"
#include "base/logging.h"
#include "arcfour.h"

#include "nandy.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;

static constexpr uint8 NAN3 = 0b011;
static constexpr uint8 INF3 = 0b010;

// Yield to OS (so that we can ctrl-c, process ethernet, etc.)
inline void Yield() {
  struct timespec t;
  t.tv_sec = 0;
  // 1 microsecond.
  t.tv_nsec = 1 * 1000;
  nanosleep(&t, nullptr);
}

// Low 18 bits all set.
static constexpr uint32 OUTPUT_MASK = 0b111111111111111111;


static void Init() {
  CHECK(bcm2835_init());

  vector<int> outputs = {
    0, 1, 2,
    3, 4, 5,
    6, 7, 8,
    9, 10, 11,
    12, 13, 14,
    15, 16, 17,
  };
  vector<int> inputs = {18, 19, 20,
			21, 22, 23,
			24, 25, 26};

  for (int pin : inputs) {
    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_INPT);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);
  }

  for (int pin : outputs) {
    CHECK((1 << pin) & OUTPUT_MASK) << pin;
    bcm2835_gpio_fsel(pin, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_set_pud(pin, BCM2835_GPIO_PUD_OFF);
  }
  
  printf("I/O initialized.\n");
}

static uint8 GetNand(uint8 in_a, uint8 in_b) {
  // Write 6 binary3-coded floats to the board, and return
  // 3. Each of a-f encodes a single "bit" of a binary3 number
  // as either nan or inf, and so to does the result.
  auto GetInnerNand = [](uint8 a, uint8 b, uint8 c,
			 uint8 d, uint8 e, uint8 f) ->
    std::tuple<uint8, uint8, uint8> {
#   define GET(z, b) ((uint32_t)(((z) >> (b)) & 1))
    // distribute the three bits of z to bit positions msb, mb, lsb.
#   define DISTRIBUTE(z, msb, mb, lsb) \
    ((GET(z, 2) << (msb)) | (GET(z, 1) << (mb)) | (GET(z, 0) << (lsb)))

    const uint32 value =
	DISTRIBUTE(a, 2, 1, 0) |
	DISTRIBUTE(b, 5, 4, 3) |
	DISTRIBUTE(c, 8, 7, 6) |
	DISTRIBUTE(d, 11, 10, 9) |
	DISTRIBUTE(e, 14, 13, 12) |
	DISTRIBUTE(f, 17, 16, 15);
    
    // Write x,y to the appropriate output pins.
    bcm2835_gpio_write_mask(value, OUTPUT_MASK);
    
    // Now delay a bit...
    {
      struct timespec t;
      t.tv_sec = 0;
      // 1500 usec = 1.5 ms
      t.tv_nsec = 1500 * 1000;
      nanosleep(&t, nullptr);
    }
    const uint32 inputs = bcm2835_gpio_lev_multi();

#   define THREE(v, msb, mb, lsb) \
    ((GET(v, msb) << 2) | (GET(v, mb) << 1) | (GET(v, lsb)))

    const uint8 g = THREE(inputs, 20, 19, 18);
    const uint8 h = THREE(inputs, 23, 22, 21);
    const uint8 i = THREE(inputs, 26, 25, 24);
    return std::make_tuple(g, h, i);
  };
  
  auto Bits = [](uint8 v) -> string {
    string ret = "...";
    ret[0] = (v & 0b100) ? '1' : '0';
    ret[1] = (v & 0b010) ? '1' : '0';
    ret[2] = (v & 0b001) ? '1' : '0';
    return ret;
  };
  
  uint8 g, h, i;
  std::tie(g, h, i) = GetInnerNand(in_a & 0b100 ? INF3 : NAN3,
				   in_a & 0b010 ? INF3 : NAN3,
				   in_a & 0b001 ? INF3 : NAN3,
				   in_b & 0b100 ? INF3 : NAN3,
				   in_b & 0b010 ? INF3 : NAN3,
				   in_b & 0b001 ? INF3 : NAN3);

  uint8 result =
    ((g == INF3) ? 0b100 : 0b000) |
    ((h == INF3) ? 0b010 : 0b000) |
    ((i == INF3) ? 0b001 : 0b000);
  return result;
}

int main(int argc, char **argv) {
  Init();

  printf("Compute nandwork...\n");
  Nandy::Nandwork work = Nandy::MakeNandwork();
  printf("Got nandwork.\n");

  Nandy nandy;
  {
    ArcFour rc{StringPrintf("%lld", time(nullptr))};
    auto Rand3 = [&rc]() { return Binary3(rc.Byte() & 0b111); };
    for (int i = 0; i < Nandy::MEM_SIZE; i++) {
      nandy.MEM[i] = Rand3();
    }
  }
  printf("Created random NANDY 1000\n");
  
  
  for (;;) {
    int64_t start = time(nullptr);
    vector<bool> values = nandy.InitializeBools(work);
    // Actually do the work.
    for (int i = work.num_inputs; i < work.gates.size(); i++) {
      const Nandy::Gate &gate = work.gates[i];
      uint8 a3 = (values)[gate.src_a] ? INF3 : NAN3;
      uint8 b3 = (values)[gate.src_b] ? INF3 : NAN3;
      uint8 res3 = GetNand(a3, b3);
      bool res = res3 == INF3;
      values.push_back(res);
      Yield();
      if ((i % 1000) == 0)
	printf("%d/%d\n", i, (int)work.gates.size());
    }

    nandy.CopyBoolsToState(work, values);
    int64_t done = time(nullptr);
    printf("Finished step in %lld seconds\n", done - start);
  }
  
  CHECK(bcm2835_close());
  return 0;
}
