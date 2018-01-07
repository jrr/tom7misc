
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
#include "simplefm2.h"
#include "base/stringprintf.h"

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

int main(int argc, char **argv) {
  std::unique_ptr<Emulator> emu(Emulator::Create("roms/contra.nes"));
  vector<uint8> start = emu->SaveUncompressed();
  
  vector<pair<uint8, uint8>> movie =
    SimpleFM2::ReadInputs2P("../pftwo/contra2pwaterfall.fm2");

  for (const pair<uint8, uint8> input : movie) {
    emu->StepFull(input.first, input.second);
  }

  const FC *fc = emu->GetFC();
  for (int i = 0; i < 0x10000; i++) {
    int64 count = fc->X->pc_histo[i];
    if (count > 0) {
      printf("%04x: %lld\n", i, count);
    }
  }

  return 0;
}
