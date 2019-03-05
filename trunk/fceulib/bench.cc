
#include "emulator.h"

#include <cmath>
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
#include "simplefm7.h"
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

static constexpr uint64 expected_ram = 0xb9ea3297351afa03ULL;
static constexpr uint64 expected_img = 0x9c8975828c9578a7ULL;

std::tuple<uint64, uint64, double> RunBenchmark(Emulator *emu,
						const vector<uint8> &start,
						const vector<uint8> &movie) {
  emu->LoadUncompressed(start);
  Timer exec_timer;

  // Only the last step needs to be full, so that we render the image.
  for (int i = 0; i < movie.size() - 1; i++)
    emu->Step(movie[i], 0);
  emu->StepFull(movie[movie.size() - 1], 0);

  const double exec_seconds = exec_timer.GetSeconds();
  return make_tuple(emu->RamChecksum(), emu->ImageChecksum(), exec_seconds);
}

int main(int argc, char **argv) {
  string romdir = "roms/";
  double startup_seconds;

  Timer startup_timer;
  // TODO: This is not really fair since it counts all the IO.
  std::unique_ptr<Emulator> emu(Emulator::Create("mario.nes"));
  CHECK(emu.get() != nullptr);
  startup_seconds = startup_timer.GetSeconds();
  vector<uint8> start = emu->SaveUncompressed();
  
  vector<uint8> movie = SimpleFM7::ReadInputs("mario-long.fm7");
  CHECK(!movie.empty());
  
  double exec_seconds = -1.0;
  
  int executions = 0;
  double total_time = 0.0;
  vector<int> last_means;
  for (int i = 0; /* exit upon convergence */; i++) {
    uint64 ram, img;
    double sec;
    std::tie(ram, img, sec) = RunBenchmark(emu.get(), start, movie);
    executions++;
    total_time += sec;
    double mean = total_time / (double)executions;
    int mtrunc = (int)(round(mean * 10.0));
    if (last_means.size() >= 5) {
      if ([&]() {
	for (int lm : last_means) {
	  if (lm != mtrunc) return false;
	}
	return true;
      }()) {
	// Convergence!
	exec_seconds = mean;
	break;
      }
      // Discard oldest to keep 5 means.
      last_means.erase(last_means.begin());
    }
    last_means.push_back(mtrunc);
    printf("Round %4d in %.4f, mean %.4f\n", executions, sec, mean);
    fflush(stdout);
  }
  
  uint64 ram_checksum = emu->RamChecksum();
  uint64 img_checksum = emu->ImageChecksum();
  fprintf(stderr,
	  "RAM checksum: %llx\n"
	  "Img checksum: %llx\n",
	  ram_checksum,
	  img_checksum);

  fprintf(stderr,
          "Startup time: %.4fs\n"
          "Exec time:    %.4fs\n",
          startup_seconds, exec_seconds);

  // mario-tom
  // static constexpr uint64 expected_ram = 0xaf57274ece679455ULL;
  // static constexpr uint64 expected_img = 0xc3e8723a5a0d4020ULL;
  // mario-long

  
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
