
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
#include "cart.h"

#include <mutex>
#include <thread>

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

static string ReadFileToString(const string &fn) {
  FILE *f = fopen(fn.c_str(), "rb");
  if (!f) return "";
  fseek(f, 0, SEEK_END);
  int size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char *ss = (char*)malloc(size);
  fread(ss, 1, size, f);

  fclose(f);

  string ret = string(ss, size);
  free(ss);

  return ret;
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

namespace {
struct CodeConfig {
  bool is_pal = false;
  bool has_map_irq_hook = false;
};
}

static void GenerateCode(CodeConfig config,
			 const vector<uint8> code,
			 uint32 addr_start,
			 uint32 addr_past_end,
			 const string &symbol,
			 const string &filename) {
  // We generate a routine just like X6502::Run. This routine only
  // works if the address is in the mapped range [addr_start,
  // addr_past_end). It assumes that reads within the mapped range are
  // constant. (Note: It would improve performance if we knew other
  // addresses to be unmappable (or known-mapped) ROM, because then we
  // could avoid the indirection of calling ARead, and replacing reads
  // with literals would increase optimization opportuities.)

  // Any time we get stuck, we can appeal to the real X602 interpreter
  // (RunLoop), which is of course fully general. So our goal here is
  // to find cases that are very common and optimize those.

  FILE *f = fopen(filename.c_str(), "w");

  // Prelude.
  fprintf(f,
	  "// Generated code! Do not edit.\n"
	  "// Generated from %s on [DATE].\n"
	  "\n"
	  "#include \"fc.h\"\n"
	  "#include \"x6502.h\"\n"
	  "#include \"sound.h\"\n"
	  "#include \"fceu.h\"\n",
	  filename.c_str());
  
  
  // Function header.
  fprintf(f,
	  "// From %s, $%04x--$%04x.\n"
	  "void %s_Run(FC *fc, int32 cycles) {\n",
	  filename.c_str(), addr_start, addr_past_end - 1,
	  symbol.c_str());

  // Copies of FC objects, used locally.
  fprintf(f,
	  "  X6502 *X = fc->X;\n"
	  "  const FCEU *fceu = fc->fceu;\n"  // const?
	  "  Sound *sound = fc->sound;\n"
	  );

  if (config.is_pal) fprintf(f, "  cycles *= 15;  // is pal\n");
  else fprintf(f, "  cycles *= 16;  // is ntsc\n");

  fprintf(f, "  X->count += cycles;\n");

  fprintf(f, "  switch (X->reg_PC) {\n");

  for (int32 pc_addr = addr_start; pc_addr < addr_past_end; /* in loop */) {
    fprintf(f, "    case 0x%04x: {", pc_addr);
    const int code_idx = pc_addr - addr_start;
    CHECK(code_idx >= 0 && code_idx < code.size()) << code_idx;
    const uint8 b1 = code[code_idx];

    // XXX include disassembly here
    fprintf(f, " // %2x\n", b1);

    #define I "      "
    const int cycles = CycTable[b1];
    // ADDCYC() macro.
    fprintf(f, I "X->tcount += %d; X->count -= %d; X->timestamp += %d;\n",
	    cycles, cycles * 48, cycles);

    // "temp" only used for the calls to irq and sound hooks, I guess
    // in case they try to read tcount?
    fprintf(f, I "{\n"
	    I "  int32 temp = X->tcount;\n"
	    I "  X->tcount = 0;\n");

    CHECK(!config.has_map_irq_hook) << "Not supported (yet)?";

    // PERF Can remove/simplify calls to sound hook?
    fprintf(f, I "  sound->FCEU_SoundCPUHook(temp);\n");
    fprintf(f, I "}\n");

    // was reg_PC++
    pc_addr++;
    fprintf(f, I "X->reg_PC = 0x%04x;\n", pc_addr & 0xFFFF);


    fprintf(f, "    } // ends pc=0x%4x\n", pc_addr);
  }

  fprintf(f, "    default: X->RunLoop(); return;\n"
	  "  }\n");
  
  
  // fprintf(f, "%s\n", ReadFile("aot-run-prelude.inc").c_str());

  fprintf(f, "  CHECK(false) << \"Should not be reachable.\";\n");
  
  fprintf(f, "\n}  // %s_Run", symbol.c_str());
  fclose(f);
}

int main(int argc, char **argv) {
  string romdir = "roms/";

  std::unique_ptr<Emulator> emu(Emulator::Create("mario.nes"));

  Timer compile_timer;

  FC *fc = emu->GetFC();
  
  // Grab a specific block of RAM. I know this is where the
  // code resides in mario.nes, that it never gets remapped
  // (mapper 0 cannot remap), and that it is not writable
  // (only RAM and 6000-7ffff are writable).
  //
  // Note that even if an area isn't writable, it's possible that it's
  // unmapped, in which case the read returns the value of the last
  // read (this is usually predictable statically, but definitely
  // complicates things). For mapper 0, all of 0x8000-0x7fff is mapped
  // to cart rom (CartBR).
  vector<uint8> code;
  code.reserve(0x10000 - 0x8000);
  for (uint32 addr = 0x8000; addr < 0x10000; addr++) {
    code.push_back(fc->fceu->ARead[addr](fc, addr));
  }

  CodeConfig config;
  config.is_pal = !!fc->fceu->PAL;
  config.has_map_irq_hook = fc->X->MapIRQHook != nullptr;
  
  GenerateCode(config,
	       code, 0x8000, 0x10000,
	       "mario", "mario.cc");

  double compile_seconds = compile_timer.GetSeconds();
  
  fprintf(stderr, "Finished.\n"
          "Compile time: %.4fs\n",
          compile_seconds);

  return 0;
}
