
#include "emulator.h"

#include <string>
#include <vector>
#include <memory>
#include <sys/time.h>
#include <sstream>
#include <unistd.h>
#include <cstdio>

#include "base/logging.h"
#include "test-util.h"
#include "arcfour.h"
#include "rle.h"
#include "simplefm2.h"
#include "base/stringprintf.h"
#include "stb_image_write.h"

#include "x6502.h"

#include <mutex>
#include <thread>

#include "tracing.h"

#define ANSI_RED "\x1B[1;31;40m"
#define ANSI_GREY "\x1B[1;30;40m"
#define ANSI_BLUE "\x1B[1;34;40m"
#define ANSI_CYAN "\x1B[1;36;40m"
#define ANSI_YELLOW "\x1B[1;33;40m"
#define ANSI_GREEN "\x1B[1;32;40m"
#define ANSI_WHITE "\x1B[1;37;40m"
#define ANSI_PURPLE "\x1B[1;35;40m"
#define ANSI_RESET "\x1B[m"
// Clear screen and go to 0,0
#define ANSI_CLS "\x1b[2J\x1b[;H"

static int64 TimeUsec() {
  timeval tv;
  gettimeofday(&tv, nullptr);
  return tv.tv_sec * 1000000LL + tv.tv_usec;
}

struct Timer {
  Timer() : start_time(TimeUsec()) {}
  const int64 start_time;
  int64 GetUsec() const { return TimeUsec() - start_time; }
  double GetSeconds() const {
    return (TimeUsec() - start_time) / 1000000.0;
  }
};

int main(int argc, char **argv) {
  string romdir = "roms/";
  double startup_seconds;

  Timer startup_timer;
  // TODO: This is not really fair since it counts all the IO.
  std::unique_ptr<Emulator> emu(Emulator::Create("mario.nes"));
  startup_seconds = startup_timer.GetSeconds();

  vector<uint8> movie = SimpleFM2::ReadInputs("mario-tom.fm2");

  Timer exec_timer;
  // After the first execution of the movie, not clear that this will
  // be doing anything interesting...
  for (int i = 0; i < 5; i++) {
    for (const uint8 input : movie) {
      emu->StepFull(input, 0);
    }
  }
  double exec_seconds = exec_timer.GetSeconds();

  X6502 *x6502 = emu->GetFC()->X;
  int64 old_cycles = 0LL;
  for (int i = 0; i <= 0xFFFF; i++) {
    old_cycles += x6502->pc_histo[i];
  }

  int64 aot_entries = 0LL;
  for (int i = 0; i < 0xFFFF; i++) {
    aot_entries += x6502->entered_aot[i];
  }
  
  #if 0
  for (int i = 0; i <= 0xFFFF; i++) {
    int64 exec = x6502->pc_histo[i];
    if (exec > 0) {
      printf("%4x: %lld\n", i, exec);
    }
  }

  for (int i = 0; i < 0xFFFF; i++) {
    int64 exec = x6502->entered_aot[i];
    if (exec > 0) {
      printf("AOT(%d%s): %lld\n", i, i == 1023 ? "+" : "", exec);
    }
  }
  
  for (int i = 0; i < 1024; i++) {
    int64 exec = x6502->cycles_histo[i];
    if (exec > 0) {
      printf("Run(%d%s): %lld\n", i, i == 1023 ? "+" : "", exec);
    }
  }
  #endif

  for (int i = 0; i < 256; i++) {
    if (x6502->unimpl_inst[i] > 0) {
      printf("Inst 0x%02x: %lld\n", i, x6502->unimpl_inst[i]);
    }
  }
  
 
  uint64 ram_checksum = emu->RamChecksum();
  uint64 img_checksum = emu->ImageChecksum();
  fprintf(stderr,
	  "RAM checksum: %llx\n"
	  "Img checksum: %llx\n",
	  ram_checksum,
	  img_checksum);

  fprintf(stderr,
	  "Old cycles: %lld\n"
	  "AOT entries: %lld\n", old_cycles, aot_entries);
  fprintf(stderr,
          "Startup time: %.4fs\n"
          "Exec time:    %.4fs\n",
          startup_seconds, exec_seconds);

  static constexpr uint64 expected_ram = 0xaf57274ece679455ULL;
  static constexpr uint64 expected_img = 0xc3e8723a5a0d4020ULL;
  int status = 0;
  if (ram_checksum != expected_ram) {
    fprintf(stderr, "*** Ram checksum mismatch. Wanted %llx!\n",
	    expected_ram);
    status = -1;
  }
  if (img_checksum != expected_img) {
    fprintf(stderr, "*** Img checksum mismatch. Wanted %llx!\n",
	    expected_img);
    status = -1;
  }
  
  return status;
}
