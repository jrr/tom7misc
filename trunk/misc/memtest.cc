
// Very simple memory stress-tester for 64-bit Windows machines
// with large amounts of RAM and cores.
//
// Run like ./memtest.exe 120 60 20
// for a machine with 128GB RAM and 64 hyperthreads,
// to run 20 times.
// If you want to test every last byte, and non-random patterns,
// you'll need to use some other program.

#include <vector>
#include <utility>

#include <cstdint>
#include <stdio.h>
#include <stdlib.h>

#include <windows.h>

#include "../cc-lib/arcfour.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"

using uint64 = uint64_t;
using int64 = int64_t;

using namespace std;

static_assert(sizeof (size_t) == sizeof (uint64),
	      "size_t needs to be 64 bit");

static inline Hash(uint64 seed1, uint64 seed2, uint64 input) {
  // This is basically from CityHash, but instead of reusing the
  // high 64 twice, we take two different seeds.
  static constexpr uint64 kMul = 0x9ddfea08eb382d69ULL;
  uint64 a = (seed1 ^ input) * kMul;
  a ^= (a >> 47);
  uint64 b = (seed2 ^ a) * kMul;
  b ^= (b >> 47);
  b *= kMul;
  return b;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Give a number of gigabytes on the command "
	    "line to test.");
    return -1;
  }

  constexpr size_t ONE_GIG = (1024LL * 1024LL * 1024LL);
  constexpr size_t GIG_IN_64 = ONE_GIG / 8;
  
  const int gb = atoi(argv[1]);
  const size_t bytes = (size_t)gb * ONE_GIG;
  printf("Allocating %d GB = %lld bytes\n", gb, bytes);
  CHECK(bytes % (sizeof (uint64)) == 0);

  int threads = 6;
  if (argc >= 3) {
    threads = atoi(argv[2]);
  }
  printf("Using %d threads.\n", threads);
  CHECK(threads > 0);

  int times = 10;
  if (argc >= 4) {
    times = atoi(argv[3]);
  }
  printf("Running %d times.\n", times);
  CHECK(times > 0);
  
  fflush(stdout);

  vector<uint64*> chunks;
  for (int i = 0; i < gb; i++) {
    size_t alloc = ONE_GIG;
    uint64 *data = (uint64*)_aligned_malloc(alloc, sizeof (uint64));
    CHECK(data != nullptr);
    chunks.push_back(data);
    printf("Allocated %lld at %p...\n", alloc, data);
    fflush(stdout);
  }

  string seed = StringPrintf("seed %llx", time(nullptr));
  printf("Seed: [%s]\n", seed.c_str());
  ArcFour rc(seed);
  for (int i = 0; i < times; i++) {
    const uint64 seed = Rand64(&rc);
    printf("[%d/%d] Setting memory (hash test):\n",
	   i + 1, times);
    fflush(stdout);
    ParallelComp(
	chunks.size(),
	[&chunks, seed](int chunk) {
	  for (int idx = 0; idx < GIG_IN_64; idx++)
	    chunks[chunk][idx] = Hash(seed, (uint64)chunks[chunk], idx);
	},
	threads);

    printf("[%d/%d] Checking its contents:\n",
	   i + 1, times);
    fflush(stdout);
    ParallelComp(
	chunks.size(),
	// GIG_IN_64,
	[&chunks, seed](int chunk) {
	  for (int idx = 0; idx < GIG_IN_64; idx++) {
	    CHECK(chunks[chunk][idx] ==
		  Hash(seed, (uint64)chunks[chunk], idx));
	  }
	},
	threads);

    printf("[%d/%d] OK!\n", i + 1, times);
    fflush(stdout);
  } 

  for (uint64 *p : chunks) {
    printf("Freeing at %p...\n", p);
    fflush(stdout);
    _aligned_free(p);
  }
  return 0;
}
