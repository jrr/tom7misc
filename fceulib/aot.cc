
// New idea:
// Treat each address as a potential entry point. Generate a function
// (or whatever) for the straight-line code beginning there. If it does
// a branch that we can't predict (i.e., a RET or computed goto) then
// we return to the driver and let it dispatch again. Otherwise, can
// do an internal goto.
//
// The idea behind this is that fewer entry points makes the code
// much easier to optimize (because for example you may know the value
// of registers, etc.)
//
// Consider establishing conditions for sound (e.g. not in the middle
// of a DMA, certain hooks are guaranteed to not modify x6502 state) that
// allow us to skip the SoundHook?

// TODO: reg_PC is only accessed through x6502; I think it's impossible
// for any other code to modify it (even the interrupt calls just set
// a flag and wait for the next cycle). Could keep PC in a local variable
// and only set it when we exit generated code.

// TODO: Should not be passing X->reg_A and so on in Exp<>. These
// should contain *values*, but an intermediate write to a register
// changes its value, duh. (This is mostly, or completely, done now.)

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
#include "base/stringprintf.h"
#include "threadutil.h"

#include "x6502.h"
#include "cart.h"

#include <mutex>
#include <thread>

// When we emit a sequence of instructions, we use local variables
// for the "registers" (registers + some other state) rather than
// reading and writing through the X object, since locals should be
// easier for the compiler (and this code generator!) to reason
// about.
//
// I seem to recall that this---curiously!---made the generated code
// slower.
#define USE_LOCALS 0

// TODO: Mapper metadata.

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
struct RomConfig {
  string file;

  // Needs to be set small enough that GCC doesn't crash.
  // Game-dependent...
  int entrypoints_per_file = 512;
  // The region of code to generate. AOT assumes that the
  // data in this region cannot change (e.g. from bank switching).
  // This should also come from mapper metadata.
  int code_addr_start = 0;
  int code_addr_after_end = 0;
  
  // This is an assertion today; should come from mapper metadata.
  bool effectless_read_8000_ffff = false;
};

// Configuration during code generation; this should be derived from
// the emulated ROM or from ROMConfig.
struct CodeConfig {
  int entrypoints_per_file = 512;
  bool is_pal = false;
  bool has_map_irq_hook = false;
  // True if reads from this region can be discarded or duplicated
  // without any effect (e.g. on registers), not counting the write
  // to DB. XXX Generalize!
  bool effectless_read_8000_ffff = false;
};

struct Code {
  Code() : known(0x10000, false), code(0x10000, 0) {}

  // Both map entire 16-bit address space.
  vector<bool> known;
  vector<uint8> code;

  uint8 Get(int addr) const {
    CHECK(addr >= 0 && addr < code.size() && known[addr]) << "Unmapped "
      "read from code: " << addr;
    return code[addr];
  }

private:
  DISALLOW_COPY_AND_ASSIGN(Code);
};
}

struct Reg {
  const char *short_name;
  const char *local_name;
  int id;
  const char *ctype;
  const char *xfield;
};

#if USE_LOCALS
#define LOCAL_PC "local_pc"
#define LOCAL_A "local_a"
#define LOCAL_X "local_x"
#define LOCAL_Y "local_y"
#define LOCAL_S "local_s"
#define LOCAL_P "local_p"
#define LOCAL_PI "local_pi"
#define LOCAL_JAMMED "local_jammed"
#define LOCAL_COUNT "local_count"
#define LOCAL_IRQLOW "local_irqlow"
#define LOCAL_DB "local_db"
#define LOCAL_TCOUNT "local_tcount"
#else
#define LOCAL_PC "X->reg_PC"
#define LOCAL_A "X->reg_A"
#define LOCAL_X "X->reg_X"
#define LOCAL_Y "X->reg_Y"
#define LOCAL_S "X->reg_S"
#define LOCAL_P "X->reg_P"
#define LOCAL_PI "X->reg_PI"
#define LOCAL_JAMMED "X->jammed"
#define LOCAL_COUNT "X->count"
#define LOCAL_IRQLOW "X->IRQlow"
#define LOCAL_DB "X->DB"
#define LOCAL_TCOUNT "X->tcount"
#endif

#define ID_PC 1
#define ID_A 2
#define ID_X 4
#define ID_Y 8
#define ID_S 16
#define ID_P 32
#define ID_PI 64
#define ID_JAMMED 128
#define ID_COUNT 256
#define ID_IRQLOW 512
#define ID_DB 1024
#define ID_TCOUNT 2048

#define NUM_REGS 12
static Reg regs[] = {
  {"pc", LOCAL_PC, ID_PC, "uint16", "X->reg_PC"},
  {"a", LOCAL_A, ID_A, "uint8", "X->reg_A"},
  {"x", LOCAL_X, ID_X, "uint8", "X->reg_X"},
  {"y", LOCAL_Y, ID_Y, "uint8", "X->reg_Y"},  
  {"s", LOCAL_S, ID_S, "uint8", "X->reg_S"},
  {"p", LOCAL_P, ID_P, "uint8", "X->reg_P"},
  {"pi", LOCAL_PI, ID_PI, "uint8", "X->reg_PI"},
  {"jam", LOCAL_JAMMED, ID_JAMMED, "uint8", "X->jammed"},
  {"cnt", LOCAL_COUNT, ID_COUNT, "int32", "X->count"},
  {"irq", LOCAL_IRQLOW, ID_IRQLOW, "uint32", "X->IRQlow"},
  {"db", LOCAL_DB, ID_DB, "uint8", "X->DB"},
  {"tcnt", LOCAL_TCOUNT, ID_TCOUNT, "int32", "X->tcount"},
};
static_assert(sizeof regs / sizeof (Reg) == NUM_REGS, "");

#define REG_PC regs[0]
#define REG_A regs[1]
#define REG_X regs[2]
#define REG_Y regs[3]
#define REG_S regs[4]
#define REG_P regs[5]
#define REG_PI regs[6]
#define REG_JAMMED regs[7]
#define REG_COUNT regs[8]
#define REG_IRQLOW regs[9]
#define REG_DB regs[10]
#define REG_TCOUNT regs[11]

// These are only read and written in 6502 itself (savestate is ok).
// That means that we never need to save or restore their values
// when making an external function call.
static constexpr uint32 PRIVATE_TO_X6502 = ID_JAMMED | ID_PC;

static bool CanGenInstruction(uint8 b1) {
  // Note that this is now equivalent to just "return true;", since
  // all 6502 instructions are implemented. But it is very useful to
  // be able to turn off instructions to binary search for bugs.
  switch (b1) {
  case 0xAA: return true;
  case 0x8A: return true;
  case 0xA8: return true;
  case 0x98: return true;
  case 0xBA: return true;
  case 0x9A: return true;

  case 0x18: return true;
  case 0xD8: return true;
  case 0x58: return true;
  case 0xB8: return true;

  case 0x38: return true;
  case 0xF8: return true;
  case 0x78: return true;

  case 0xEA: return true;

  case 0x8C: return true;
  case 0x8D: return true;
  case 0x8E: return true;
  case 0x8F: return true;

  case 0xA0: return true;
  case 0xA2: return true;

  case 0xA9: return true;

  case 0xAD: return true;
  case 0xAC: return true;
  case 0xAE: return true;

  case 0x1A: return true;
  case 0x3A: return true;
  case 0x5A: return true;
  case 0x7A: return true;
  case 0xDA: return true;
  case 0xFA: return true;

  case 0x0C: return true;
  case 0x09: return true;
  case 0x0D: return true;

  case 0x04: return true; 
  case 0x14: return true; 
  case 0x34: return true; 
  case 0x44: return true; 
  case 0x54: return true; 
  case 0x64: return true; 
  case 0x74: return true; 

  case 0x80: return true; 
  case 0x82: return true; 
  case 0x89: return true; 
  case 0xC2: return true; 
  case 0xD4: return true; 
  case 0xE2: return true; 
  case 0xF4: return true;

  case 0x90: return true;
  case 0xB0: return true;

  case 0xF0: return true;
  case 0xD0: return true;
  case 0x30: return true;

  case 0x10: return true;
  case 0x50: return true;
  case 0x70: return true;

  case 0xCA: return true;
  case 0x88: return true;
  case 0xE8: return true;
  case 0xC8: return true;    

  case 0x20: return true;

  case 0x48: return true;
  case 0x08: return true;

  case 0x68: return true;
  case 0x28: return true;
  case 0x60: return true;

  case 0x29: return true;
  case 0x4c: return true;

  case 0x85: return true;
  case 0x86: return true;
  case 0x84: return true;
  case 0x87: return true;

  case 0xC9: return true;
  case 0xE0: return true;
  case 0xC0: return true;

  case 0x2D: return true;
  case 0xCD: return true;
    
  case 0xEC: return true;
  case 0xCC: return true;

  case 0x3D: return true;
  case 0x39: return true;
  case 0xDD: return true;
  case 0xD9: return true;
  case 0xBD: return true;
  case 0xB9: return true;

  case 0xBE: return true;
  case 0xBC: return true;

  case 0x1D: return true;
  case 0x19: return true;

  case 0x1C: return true;
  case 0x3C: return true;
  case 0x5C: return true;
  case 0x7C: return true;
  case 0xDC: return true;
  case 0xFC: return true;

  case 0x69: return true;
  case 0x6D: return true;
  case 0x7d: return true;
  case 0x79: return true;

  case 0xEB: return true;
  case 0xE9: return true;

  case 0x61: return true;
  case 0x21: return true;
  case 0xC1: return true;
  case 0xA1: return true;

  case 0xA3: return true;
  case 0xAF: return true;

  case 0x6C: return true;

  case 0x65: return true;
  case 0x25: return true;
  case 0xC5: return true;
  case 0xC4: return true;
  case 0xE4: return true;
  case 0xA5: return true;
  case 0xA6: return true;
  case 0xA4: return true;
  case 0x05: return true;
  case 0xE5: return true;
  case 0xA7: return true;

  case 0x24: return true;
  case 0x2C: return true;
  case 0x49: return true;
  case 0x45: return true;
  case 0x4D: return true;
  case 0x5D: return true;
  case 0x59: return true;
  case 0x41: return true;

  case 0x71: return true;
  case 0x31: return true;
  case 0xD1: return true;
  case 0x51: return true;
  case 0xB1: return true;
  case 0x11: return true;
  case 0xF1: return true;
  case 0xB3: return true;

  case 0x40: return true;
  case 0x00: return true;

  case 0x0A: return true;
  case 0x4A: return true;

  case 0x75: return true;
  case 0x35: return true;
  case 0xD5: return true;
  case 0x55: return true;
  case 0xB5: return true;
  case 0xB6: return true;
  case 0xB4: return true;
  case 0x15: return true;
  case 0xF5: return true;

  case 0x0E: return true;
  case 0xCE: return true;
  case 0xEE: return true;
  case 0x4E: return true;
  case 0x2E: return true;
  case 0x6E: return true;
  case 0xCF: return true;
  case 0xEF: return true;
  case 0x2F: return true;
  case 0x6F: return true;

  case 0x2A: return true;
  case 0x6A: return true;

  case 0x1E: return true;
  case 0xDE: return true;
  case 0xFE: return true;
  case 0x5E: return true;
  case 0x3E: return true;
  case 0x7E: return true;
  case 0xDF: return true;
  case 0xDB: return true;
  case 0xFF: return true;
  case 0xFB: return true;
  case 0x3F: return true;
  case 0x3B: return true;
  case 0x7F: return true;
  case 0x7B: return true;

  case 0x0F: return true;
  case 0x1F: return true;
  case 0x1B: return true;
  case 0x4F: return true;
  case 0x5F: return true;
  case 0x5B: return true;

  case 0xC3: return true;
  case 0xE3: return true;
  case 0x23: return true;
  case 0x63: return true;
  case 0x03: return true;
  case 0x43: return true;

  case 0xD3: return true;
  case 0xF3: return true;
  case 0x33: return true;
  case 0x73: return true;
  case 0x13: return true;
  case 0x53: return true;

  case 0xBB: return true;
  case 0xBF: return true;

  case 0x02: return true;
  case 0x12: return true;
  case 0x22: return true;
  case 0x32: return true;
  case 0x42: return true;
  case 0x52: return true;
  case 0x62: return true;
  case 0x72: return true;
  case 0x92: return true;
  case 0xB2: return true;
  case 0xD2: return true;
  case 0xF2: return true;

  case 0xAB: return true;
  case 0xCB: return true;

  case 0xF9: return true;
  case 0xED: return true;

  case 0x9D: return true;
  case 0x99: return true;
  case 0x9F: return true;
  case 0x9C: return true;

  case 0x9E: return true;
  case 0x9B: return true;

  case 0x81: return true;
  case 0x91: return true;
  case 0x83: return true;
  case 0x93: return true;

  case 0x06: return true;
  case 0xC6: return true;
  case 0xE6: return true;
  case 0x46: return true;
  case 0x26: return true;
  case 0x66: return true;
  case 0xC7: return true;
  case 0xE7: return true;
  case 0x27: return true;
  case 0x67: return true;
  case 0x07: return true;
  case 0x47: return true;

  case 0x95: return true;
  case 0x96: return true;
  case 0x94: return true;
  case 0x97: return true;

  case 0x16: return true;
  case 0xD6: return true;
  case 0xF6: return true;
  case 0x56: return true;
  case 0x36: return true;
  case 0x76: return true;
  case 0xD7: return true;
  case 0xF7: return true;
  case 0x37: return true;
  case 0x77: return true;
  case 0x17: return true;
  case 0x57: return true;

  case 0xE1: return true;
  case 0x01: return true;
  case 0x8B: return true;
  case 0xB7: return true;
  case 0xFD: return true;
  case 0x0B: return true;
  case 0x2B: return true;

  case 0x4B: return true;
  case 0x6B: return true;
    
  default: return false;
  }
}

#define I "  "

// An expression (without side-effects) that may be a known constant
// of the type T. T must be integral. Value semantics.
template<class T>
struct Exp {
  T value = 0;
  string expr = "/* XXX uninitialized */";
  bool known = false;
  explicit Exp(T val) : value(val), known(true) {}
  explicit Exp(const string &s) : expr(s), known(false) {}

  bool Known() const { return known; }
  string StringInternal() const;
  string String() const {
    if (known) return StringInternal();
    else return expr;
  }
  T Value() const {
    CHECK(Known());
    return value;
  }
};

// Specializations for various types.
template<>
string Exp<uint8>::StringInternal() const {
  return StringPrintf("0x%02x", value);
}
template<>
string Exp<uint16>::StringInternal() const {
  return StringPrintf("0x%04x", value);
}

static Exp<uint16> Extend8to16(Exp<uint8> v) {
  if (v.Known()) {
    return Exp<uint16>((uint16)v.Value());
  } else {
    return Exp<uint16>(StringPrintf("((uint16)(%s))", v.String().c_str()));
  }
}

struct AOT {
  // At the header of a function, declare all the locals for the
  // "registers". PERF Actually reading these from X is often
  // not necessary (especially for ones that are rarely read, or
  // whose values are overwritten immediately). Hopefully the C
  // compiler handles most of this for us.
  void DeclLocals(FILE *f) {
    #if USE_LOCALS
    for (int i = 0; i < NUM_REGS; i++) {
      fprintf(f, I "%s %s = %s;\n",
	      regs[i].ctype, regs[i].local_name, regs[i].xfield);
    }
    #endif
  }

  // Write the values of locals in the set back to the X object. We do
  // this before entering code that we don't understand, or exiting
  // the AOT compiled routines (so that their values are reflected in
  // the rest of the emulator!)
  void FlushLocals(FILE *f, uint32 mask) {
    #if USE_LOCALS
    fprintf(f, I);
    for (int i = 0; i < NUM_REGS; i++) {
      if (mask & regs[i].id) {
	fprintf(f, " %s=%s;", regs[i].xfield, regs[i].local_name);
      }
    }
    fprintf(f, "\n");
    #endif
  }

  // Read the locals in the set from the X object. We do this after
  // calling code that we don't understand. (Upon startup we also
  // read the locals, but via DeclLocals.)
  void LoadLocals(FILE *f, uint32 mask) {
    #if USE_LOCALS
    fprintf(f, I);
    for (int i = 0; i < NUM_REGS; i++) {
      if (mask & regs[i].id) {
	fprintf(f, " %s=%s;", regs[i].local_name, regs[i].xfield);
      }
    }
    fprintf(f, "\n");
    #endif
  }

  // Takes the known code values.
  // Takes the instruction byte, b1, and the pc value after reading
  // that byte. For some instructions, we advance the pc further.
  //
  // Returns new value of PC after this instruction (if it does not
  // branch). Returns 0xFFFFFFFF if the PC is not known (or if we
  // otherwise don't want to continue generating code); this causes
  // the entry point generator to return to the driver loop, which
  // jumps to the code for the new PC (which may mean calling the
  // interpreter).

  Exp<uint8> Read(FILE *f, const Reg &r) {
    CHECK(0 == strcmp(r.ctype, "uint8")) << r.local_name;
    string s = GenSym(r.short_name);
    // n.b. if USE_LOCALS is false, local_name is actually X->reg_Z.
    fprintf(f, I "const uint8 %s = %s;\n", s.c_str(), r.local_name);
    return Exp<uint8>(s);
  }

  #if 0
  // I think the idea here was to remove some of the cycle-timing
  // checks in straight line code?
  int MaxCycles(const Code &code, uint8 b1, uint32 pc_addr) {
    // Maximum cycles that an instruction can take.
    // This is in whatever units ADDCYC takes, which is not the
    // same as the "count" register.

    // This puts an upper bound on the execution of the code, assuming
    // no interrupts or DMA.

    // It's easy to assume no interrupts, because we exit anyway once
    // the interrupt flag is set.
    // 
    // DMA only happens when sound does DMC (through soundhook, so we
    // have some visibility into when that might happen, but in any
    // case only 4 per call) or upon write to $4014, which does the
    // PPU DMA. Unfortunately this takes 512 cycles! We have to assume
    // that a write is to this address if it's either known to be 4014,
    // or if it's a dynamically computed address that we can't somehow
    // bound. Hmm. :/
    
  }
  #endif
  
  uint32 GenInstruction(const Code &code,
			uint8 b1, uint32 pc_addr, FILE *f) {

    auto ReadMem = [this, &code, f](Exp<uint16> addr) -> Exp<uint8> {
      Exp<uint8> res{0};

      if (addr.Known() && code.known[addr.Value()]) {
	res = Exp<uint8>(code.code[addr.Value()]);
      } else if (addr.Known()) {
	// Could fall through to the next case, but it's nice to avoid
	// generating variables we don't need.
	const string val_sym = GenSym("v");
	if (addr.Value() < 0x800) {
	  // Addresses 0-2047 are always mapped to RAM. (Jeesh, I hope!)
	  fprintf(f,
		  I "const uint8 %s = fceu->RAM[0x%04x];\n",
		  val_sym.c_str(), addr.Value());
	} else {
	  // PERF: Also, here we know the address, so we can also avoid
	  // dynamically dispatching to the read handler. Add some metadata
	  // to mappers that lets us call the appropriate handler directly.
	  //
	  // This also helps the C compiler optimize this block since
	  // external read handlers could do anything (and some do). It
	  // would also be useful if handler metadata told us that some
	  // registers are untouched.

	  // But the PC (and jammed bit) can't be modified in mappers.
	  FlushLocals(f, ~PRIVATE_TO_X6502);
	  fprintf(f, 
		  I "const uint8 %s = fceu->ARead[0x%04x](fc, 0x%04x);\n",
		  val_sym.c_str(), addr.Value(), addr.Value());
	  LoadLocals(f, ~PRIVATE_TO_X6502);
	}
	res = Exp<uint8>(val_sym);
      } else {
	// Need to serialize addr and val.
	const string addr_sym = GenSym("a");
	const string val_sym = GenSym("v");
	fprintf(f,
		I "const uint16 %s = %s;\n", addr_sym.c_str(),
		addr.String().c_str());

	// Unknown address so very little hope that we can save locals.
	// But the PC (and jammed bit) can't be modified in mappers.
	FlushLocals(f, ~PRIVATE_TO_X6502);
	fprintf(f, 
		I "const uint8 %s = fceu->ARead[%s](fc, %s);\n",
		val_sym.c_str(), addr_sym.c_str(), addr_sym.c_str());
	LoadLocals(f, ~PRIVATE_TO_X6502);
	res = Exp<uint8>(val_sym);
      }

      // res is a value now. Write to data bus.
      fprintf(f, I LOCAL_DB " = %s;\n", res.String().c_str());
      return res;
    };
  
    // PERF: Look for places where I call this with an 8 bit argument
    // (there are some); those can just access RAM directly.
    auto WriteMem = [this, f](const Exp<uint16> &addr_exp,
			      const Exp<uint8> &val_exp) {
      if (addr_exp.Known()) {
	if (addr_exp.Value() < 0x800) {
	  fprintf(f,
		  I "fceu->RAM[0x%04x] = %s;\n",
		  addr_exp.Value(), val_exp.String().c_str());
	} else {
	  // PERF! Same deal; when the address is known, avoid indirection.
	  FlushLocals(f, ~PRIVATE_TO_X6502);
	  fprintf(f, I "fceu->BWrite[0x%04x](fc, 0x%04x, %s);\n",
		  addr_exp.Value(), addr_exp.Value(),
		  val_exp.String().c_str());
	  LoadLocals(f, ~PRIVATE_TO_X6502);
	}
      } else {
	FlushLocals(f, ~PRIVATE_TO_X6502);
	fprintf(f, I "fceu->BWrite[%s](fc, %s, %s);\n",
		addr_exp.String().c_str(), addr_exp.String().c_str(),
		val_exp.String().c_str());
	LoadLocals(f, ~PRIVATE_TO_X6502);
      }
    };

    auto X_ZN = [f](const string &reg) {
      // From disassembly, GCC can do the right thing for immediates
      // (that were previously set to the register) here, getting a
      // constant value from ZNTable and avoiding setting/clearing bits.
      fprintf(f, I LOCAL_P " = (" LOCAL_P " & ~(Z_FLAG | N_FLAG)) | "
	      "ZNTable[%s];\n", reg.c_str());
    };

    auto X_ZNT = [f](const string &reg) {
      fprintf(f, I LOCAL_P " |= ZNTable[%s];\n", reg.c_str());
    };
    
    auto LD_IM = [&code, &pc_addr, f, &ReadMem](
	std::function<void(Exp<uint8>)> op) {
      Exp<uint8> x = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      op(x);
    };

    auto GetAB = [this, &code, f, &pc_addr, &ReadMem]() {
      const uint16 src_addr = pc_addr;
      Exp<uint8> value_low = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      Exp<uint8> value_high = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);

      if (value_low.Known() && value_high.Known()) {
	uint16 value = ((uint16)value_low.Value()) |
	  ((uint16)value_high.Value() << 8);
	fprintf(f,
		I "// Known GetAB from $%04x = $%04x\n",
		src_addr, value);
	return Exp<uint16>(value);
      } else {
	string sym = GenSym("ab");
	fprintf(f, I "const uint16 %s = ((uint16)%s) | "
		"((uint16)%s << 8);  // GetAB\n",
		sym.c_str(), value_low.String().c_str(),
		value_high.String().c_str());
	return Exp<uint16>(sym);
      }
    };

    // For absolute-indexed reads. Kind of weird, since if the read
    // hits (or doesn't hit?) another page, it triggers a read
    // handler (and ignores the result) in addition to the cycle penalty.
    auto GetABIRD = [this, &code, &pc_addr, f, &GetAB, &ReadMem](
	Exp<uint8> idx) {
      Exp<uint16> ab = GetAB();
      const string sym = GenSym("t");
      // idx is always a register, so not known. PERF: This is a place
      // where tracking the value of registers would be potentially
      // useful. (Not sure how often these are statically known -- why
      // wouldn't you just fix the absolute address instead in that
      // case?) Actually it does definitely happen practice, see
      // around 0x801b in mario.nes; reg_X is set to a constant 5
      // and then used to index into another known address.
      fprintf(f, I "uint16 %s = %s + %s;\n",
	      sym.c_str(), ab.String().c_str(), idx.String().c_str());
      fprintf(f, I "if ((%s ^ %s) & 0x100) {\n",
	      sym.c_str(), ab.String().c_str());
      // There was an & 0xFFFF in x6502's code, but I don't see how
      // that can do anything.
      
      Exp<uint8> unused = ReadMem(
	  Exp<uint16>(StringPrintf("(%s ^ 0x100)", sym.c_str())));
      fprintf(f, I "  (void) %s;  // Unused GetABIRD\n",
	      unused.String().c_str());
      ADDCYC(f, 1);
      fprintf(f, I "}\n");
      return Exp<uint16>(sym);
    };

    // Indexed Indirect.
    auto GetIX = [this, &code, &pc_addr, f, &ReadMem]() {
      Exp<uint8> tmp = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      // PERF another place where tracking reg_X might allow
      // simplification. Here at least we know they are RAM
      // reads.
      const string sym = GenSym("x");
      fprintf(
	  f,
	  I "const uint16 %s = \n"
	  I "  (uint16)fceu->RAM[0xFF & (%s + " LOCAL_X ")] |\n"
	  I "  ((uint16)(fceu->RAM[0xFF & (%s + 1 + " LOCAL_X ")]) << 8);\n",
	  sym.c_str(),
	  tmp.String().c_str(),
	  tmp.String().c_str());
      return Exp<uint16>(sym);
    };

    // Indirect Indexed for reads
    auto GetIYRD = [this, f, &pc_addr, &ReadMem]() {
      const string sym = GenSym("iyrd");
      const string rt = GenSym("rt");
      Exp<uint8> loc = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      fprintf(f,
	      I "const uint16 %s = (uint16)fceu->RAM[%s] |\n"
	      I "  ((uint16)fceu->RAM[0xFF & (%s + 1)] << 8);\n",
	      rt.c_str(), loc.String().c_str(), loc.String().c_str());
      fprintf(f,
	      I "const uint16 %s = %s + (uint16)" LOCAL_Y ";\n",
	      sym.c_str(), rt.c_str());
      fprintf(f,
	      I "if ((%s ^ %s) & 0x100) {\n",
	      sym.c_str(), rt.c_str());
      // Also anded with 0xFFFF, but I left it out because I think
      // sym can just be 16 bits.
      Exp<uint8> unused = ReadMem(
	  Exp<uint16>(StringPrintf("(%s ^ 0x100)", sym.c_str())));
      fprintf(f, I "(void) %s;  // Unused GetIYRD\n", unused.String().c_str());
      ADDCYC(f, 1);
      fprintf(f, "}\n");
      
      return Exp<uint16>(sym);
    };

    // Indirect Indexed for writes, rmws
    auto GetIYWR = [this, f, &pc_addr, &ReadMem]() {
      const string sym = GenSym("iywr");
      const string rt = GenSym("rt");
      Exp<uint8> loc = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      fprintf(f,
	      I "const uint16 %s = (uint16)fceu->RAM[%s] |\n"
	      I "  ((uint16)fceu->RAM[0xFF & (%s + 1)] << 8);\n",
	      rt.c_str(), loc.String().c_str(), loc.String().c_str());
      fprintf(f,
	      I "const uint16 %s = %s + (uint16)" LOCAL_Y ";\n",
	      sym.c_str(), rt.c_str());
      Exp<uint8> unused = ReadMem(
	  Exp<uint16>(StringPrintf("((%s & 0x00FF) | (%s & 0xFF00))",
				   sym.c_str(), rt.c_str())));
      fprintf(f, I "(void) %s;  // Unused GetIYWR\n", unused.String().c_str());
      return Exp<uint16>(sym);
    };
    
    // Absolute Indexed (for writes and rmws)
    auto GetABIWR = [this, f, &pc_addr, &GetAB, &ReadMem](Exp<uint8> idx) {
      const string sym = GenSym("abiwr");
      Exp<uint16> rt = GetAB();
      fprintf(f, I "const uint16 %s = %s + (uint16)(%s);\n",
	      sym.c_str(), rt.String().c_str(), idx.String().c_str());
      Exp<uint8> unused = ReadMem(
	  Exp<uint16>(StringPrintf("((%s & 0x00FF) | ((%s) & 0xFF00))",
				   sym.c_str(), rt.String().c_str())));
      fprintf(f, I "(void) %s;  // Unused GetABIWR\n",
	      unused.String().c_str());
      return Exp<uint16>(sym);
    };
    
    // Same as LD_IM?
    auto GetZP = [&ReadMem, f, &pc_addr]() {
      Exp<uint8> x = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x; // GetZP\n", pc_addr);
      return x;
    };

    // Zero Page Indexed
    auto GetZPI = [this, &ReadMem, f, &pc_addr](Exp<uint8> idx) {
      // Currently, index is always a register.
      Exp<uint8> x = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x; // GetZPI\n", pc_addr);
      string sym = GenSym("zpi");
      fprintf(f, I "const uint8 %s = (%s) + (%s);\n",
	      sym.c_str(),
	      idx.String().c_str(),
	      x.String().c_str());
      return Exp<uint8>(sym);
    };

    // RMW is presumably "read-modify-write"
    auto RMW_A = [this, f](std::function<Exp<uint8>(Exp<uint8>)> op) {
      string sym = GenSym("rmw");
      fprintf(f, I "const uint8 %s = " LOCAL_A ";\n", sym.c_str());
      Exp<uint8> x = op(Exp<uint8>(sym));
      fprintf(f, I LOCAL_A " = %s;\n", x.String().c_str());
    };

    auto RMW_AB = [f, &ReadMem, &WriteMem, &GetAB](
	std::function<Exp<uint8>(Exp<uint8>)> op) {
      Exp<uint16> aa = GetAB();
      Exp<uint8> x = ReadMem(aa);
      // x6502 does this write of the value right back, presumably
      // to trigger write handlers...
      WriteMem(aa, x);
      Exp<uint8> y = op(x);
      WriteMem(aa, y);
    };

    auto RMW_ABI = [f, &ReadMem, &WriteMem, &GetABIWR](
	Exp<uint8> reg,
	std::function<Exp<uint8>(Exp<uint8>)> op) {
      Exp<uint16> aa = GetABIWR(reg);
      Exp<uint8> x = ReadMem(aa);
      // x6502 does this write of the value right back, presumably
      // to trigger write handlers...
      WriteMem(aa, x);
      Exp<uint8> y = op(x);
      WriteMem(aa, y);
    };

    auto RMW_ABX = [this, f, &RMW_ABI](
	std::function<Exp<uint8>(Exp<uint8>)> op) {
      string sym = GenSym("rmwabx");
      fprintf(f, I "const uint8 %s = " LOCAL_X ";\n", sym.c_str());
      return RMW_ABI(Exp<uint8>(sym), op);
    };
    auto RMW_ABY = [this, f, &RMW_ABI](
	std::function<Exp<uint8>(Exp<uint8>)> op) {
      string sym = GenSym("rmwaby");
      fprintf(f, I "const uint8 %s = " LOCAL_Y ";\n", sym.c_str());
      return RMW_ABI(Exp<uint8>(sym), op);
    };

    auto RMW_IX = [&GetIX, &ReadMem, &WriteMem](
	std::function<Exp<uint8>(Exp<uint8>)> op) {
      Exp<uint16> aa = GetIX();
      Exp<uint8> x = ReadMem(aa);
      WriteMem(aa, x);
      Exp<uint8> y = op(x);
      WriteMem(aa, y);
    };

    auto RMW_IY = [&GetIYWR, &ReadMem, &WriteMem](
	std::function<Exp<uint8>(Exp<uint8>)> op) {
      Exp<uint16> aa = GetIYWR();
      Exp<uint8> x = ReadMem(aa);
      WriteMem(aa, x);
      Exp<uint8> y = op(x);
      WriteMem(aa, y);
    };

    auto RMW_ZP = [this, f, &GetZP](std::function<Exp<uint8>(Exp<uint8>)> op) {
      Exp<uint8> aa = GetZP();
      string sym = GenSym("x");
      fprintf(f, I "const uint8 %s = fceu->RAM[%s];\n",
	      sym.c_str(), aa.String().c_str());
      Exp<uint8> y = op(Exp<uint8>(sym));
      fprintf(f, I "fceu->RAM[%s] = %s;\n",
	      aa.String().c_str(), y.String().c_str());
    };

    auto RMW_ZPX = [this, f, &GetZPI](
	std::function<Exp<uint8>(Exp<uint8>)> op) {
      string regx = GenSym("rmwzpx");
      fprintf(f, I "const uint8 %s = " LOCAL_X ";\n", regx.c_str());
      Exp<uint8> aa = GetZPI(Exp<uint8>(regx));
      string sym = GenSym("x");
      fprintf(f, I "const uint8 %s = fceu->RAM[%s];\n",
	      sym.c_str(), aa.String().c_str());
      Exp<uint8> y = op(Exp<uint8>(sym));
      fprintf(f, I "fceu->RAM[%s] = %s;\n",
	      aa.String().c_str(), y.String().c_str());
    };
    
    auto LD_IX = [&GetIX, &ReadMem](std::function<void(Exp<uint8>)> op) {
      Exp<uint16> aa = GetIX();
      Exp<uint8> x = ReadMem(aa);
      op(x);
    };

    auto LD_IY = [&GetIYRD, &ReadMem](std::function<void(Exp<uint8>)> op) {
      Exp<uint16> aa = GetIYRD();
      Exp<uint8> x = ReadMem(aa);
      op(x);
    };
    
    auto LD_AB = [&code, &pc_addr, f, &ReadMem, &GetAB](
	std::function<void(Exp<uint8>)> op) {
      Exp<uint16> aa = GetAB();
      Exp<uint8> x = ReadMem(aa);
      op(x);
    };

    auto LD_ABI = [&code, f, &GetABIRD, &ReadMem](
	Exp<uint8> idx,
	std::function<void(Exp<uint8>)> op) {
      Exp<uint16> aa = GetABIRD(idx);
      Exp<uint8> x = ReadMem(aa);
      op(x);
    };

    auto LD_ABX = [this, f, &LD_ABI](std::function<void(Exp<uint8>)> op) {
      string sym = GenSym("ldabx");
      fprintf(f, I "const uint8 %s = " LOCAL_X ";\n", sym.c_str());
      LD_ABI(Exp<uint8>(sym), op);
    };
    auto LD_ABY = [this, f, &LD_ABI](std::function<void(Exp<uint8>)> op) {
      string sym = GenSym("ldaby");
      fprintf(f, I "const uint8 %s = " LOCAL_Y ";\n", sym.c_str());
      LD_ABI(Exp<uint8>(sym), op);
    };

    auto LD_ZP = [this, &code, f, &GetZP](std::function<void(Exp<uint8>)> op) {
      Exp<uint8> aa = GetZP();
      const string sym = GenSym("x");
      fprintf(f, I "const uint8 %s = fceu->RAM[%s];\n",
	      sym.c_str(), aa.String().c_str());
      op(Exp<uint8>(sym));
    };

    auto LD_ZPX = [this, &code, f, &GetZPI](
	std::function<void(Exp<uint8>)> op) {
      const string regx = GenSym("ldzpx");
      fprintf(f, I "const uint8 %s = " LOCAL_X ";\n", regx.c_str());
      Exp<uint8> aa = GetZPI(Exp<uint8>(regx));
      const string sym = GenSym("x");
      fprintf(f, I "const uint8 %s = fceu->RAM[%s];\n",
	      sym.c_str(), aa.String().c_str());
      op(Exp<uint8>(sym));
    };

    auto LD_ZPY = [this, &code, f, &GetZPI](
	std::function<void(Exp<uint8>)> op) {
      const string regy = GenSym("ldzpy");
      fprintf(f, I "const uint8 %s = " LOCAL_Y ";\n", regy.c_str());
      Exp<uint8> aa = GetZPI(Exp<uint8>(regy));
      const string sym = GenSym("x");
      fprintf(f, I "const uint8 %s = fceu->RAM[%s];\n",
	      sym.c_str(), aa.String().c_str());
      op(Exp<uint8>(sym));
    };

    auto LDA = [&code, f, &X_ZN](Exp<uint8> val) {
      // PERF: Could get constant for X_ZN in case that value is known,
      // but gcc seems able to do this optimization just fine.
      fprintf(f, I LOCAL_A " = %s;\n", val.String().c_str());
      X_ZN(LOCAL_A);
    };

    auto LDX = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I LOCAL_X " = %s;\n", val.String().c_str());
      X_ZN(LOCAL_X);
    };

    auto LDY = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I LOCAL_Y " = %s;\n", val.String().c_str());
      X_ZN(LOCAL_Y);
    };

    auto ORA = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I LOCAL_A " |= %s;\n", val.String().c_str());
      X_ZN(LOCAL_A);
    };

    auto AND = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I LOCAL_A " &= %s;\n", val.String().c_str());
      X_ZN(LOCAL_A);
    };

    auto BIT = [f](Exp<uint8> val) {
      fprintf(f,
	      I LOCAL_P " = (" LOCAL_P " & ~(Z_FLAG | V_FLAG | N_FLAG)) |\n"
	      I "  (ZNTable[%s & " LOCAL_A "] & Z_FLAG) |\n"
	      I "  (%s & (V_FLAG | N_FLAG));\n",
	      val.String().c_str(), val.String().c_str());
    };

    auto EOR = [f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I LOCAL_A " ^= %s;\n", val.String().c_str());
      X_ZN(LOCAL_A);
    };
    
    auto ADC = [this, &code, f, &X_ZNT](Exp<uint8> val) {
      const string sym = GenSym("adc");
      fprintf(f, I "const uint32 %s = "
	      "(uint32)(%s) + " LOCAL_A " + (" LOCAL_P " & 1);\n",
	      sym.c_str(), val.String().c_str());
      fprintf(f,
	      I LOCAL_P " =\n"
	      I "  (" LOCAL_P " & ~(Z_FLAG | C_FLAG | N_FLAG | V_FLAG)) |\n"
	      I "  (((((" LOCAL_A " ^ %s) & 0x80) ^ 0x80) &\n"
              I "    ((" LOCAL_A " ^ %s) & 0x80)) >> 1) |\n"
	      I "  ((%s >> 8) & C_FLAG);\n",
	      val.String().c_str(),
	      sym.c_str(),
	      sym.c_str());
      fprintf(f, I LOCAL_A " = %s;\n", sym.c_str());
      X_ZNT(LOCAL_A);
    };

    auto SBC = [this, &code, f, &X_ZNT](Exp<uint8> val) {
      const string sym = GenSym("sbc");
      fprintf(f, I "const uint32 %s = "
	      "(uint32)" LOCAL_A " - %s - ((" LOCAL_P " & 1) ^ 1);\n",
	      sym.c_str(), val.String().c_str());
      fprintf(f,
	      I LOCAL_P " =\n"
	      I "  (" LOCAL_P " & ~(Z_FLAG | C_FLAG | N_FLAG | V_FLAG)) |\n"
	      I "  (((" LOCAL_A " ^ %s) & (" LOCAL_A " ^ %s) & 0x80) >> 1) |\n"
	      I "  (((%s >> 8) & C_FLAG) ^ C_FLAG);\n",
	      sym.c_str(), val.String().c_str(), 
	      sym.c_str());
      fprintf(f, I LOCAL_A " = %s;\n", sym.c_str());
      X_ZNT(LOCAL_A);
    };
    
    auto ST_AB = [&code, f, &WriteMem, &GetAB](Exp<uint8> exp) {
      Exp<uint16> aa = GetAB();
      WriteMem(aa, exp);
    };

    // In the x6502 code, some of the "r" parameters actually depend
    // on the computed address (AA), so that is explicit here.
    auto ST_ABI = [&GetABIWR, &WriteMem](
	Exp<uint8> reg,
	std::function<Exp<uint8>(Exp<uint16>)> op) {
      Exp<uint16> aa = GetABIWR(reg);
      WriteMem(aa, op(aa));
    };

    auto ST_ABX = [this, f, &ST_ABI](
	std::function<Exp<uint8>(Exp<uint16>)> op) {
      const string sym = GenSym("stabx");
      fprintf(f, I "const uint8 %s = " LOCAL_X ";\n", sym.c_str());
      ST_ABI(Exp<uint8>(sym), op);
    };
    auto ST_ABY = [this, f, &ST_ABI](
	std::function<Exp<uint8>(Exp<uint16>)> op) {
      const string sym = GenSym("staby");
      fprintf(f, I "const uint8 %s = " LOCAL_Y ";\n", sym.c_str());
      ST_ABI(Exp<uint8>(sym), op);
    };

    auto ST_IX = [&GetIX, &WriteMem](Exp<uint8> r) {
      Exp<uint16> aa = GetIX();
      WriteMem(aa, r);
    };

    auto ST_IY = [&GetIYWR, &WriteMem](
	std::function<Exp<uint8>(Exp<uint16>)> r) {
      Exp<uint16> aa = GetIYWR();
      WriteMem(aa, r(aa));
    };
    
    auto ST_ZP = [&code, f, &WriteMem, &GetZP](Exp<uint8> exp) {
      Exp<uint8> aa = GetZP();
      WriteMem(Extend8to16(aa), exp);
    };

    auto ST_ZPX = [this, f, &GetZPI](Exp<uint8> exp) {
      const string sym = GenSym("stabx");
      fprintf(f, "const uint8 %s = " LOCAL_X ";\n", sym.c_str());
      Exp<uint8> aa = GetZPI(Exp<uint8>(sym));
      fprintf(f, I "fceu->RAM[%s] = %s;\n",
	      aa.String().c_str(), exp.String().c_str());
    };

    auto ST_ZPY = [this, f, &GetZPI](Exp<uint8> exp) {
      const string sym = GenSym("staby");
      fprintf(f, "const uint8 %s = " LOCAL_Y ";\n", sym.c_str());
      Exp<uint8> aa = GetZPI(Exp<uint8>(sym));
      fprintf(f, I "fceu->RAM[%s] = %s;\n",
	      aa.String().c_str(), exp.String().c_str());
    };
    
    auto PUSH = [&code, f](Exp<uint8> v) {
      fprintf(f, I "fceu->RAM[0x100 + " LOCAL_S "] = %s;\n",
	      v.String().c_str());
      fprintf(f, I LOCAL_S "--;\n");
    };

    auto POP = [this, &code, f](std::function<void(Exp<uint8>)> op) {
      fprintf(f, I LOCAL_S "++;\n");
      const string sym = GenSym("v");
      fprintf(f,
	      I "const uint8 %s = "
	      "(" LOCAL_DB " = fceu->RAM[0x100 + " LOCAL_S "]);\n",
	      sym.c_str());
      op(Exp<uint8>(sym));
    };
    
    auto JR = [this, &code, &pc_addr, f, LD_IM](Exp<uint8> cond) {
      // PERF pretty much no way conditions are known, right?
      fprintf(f, I "if (%s) {\n", cond.String().c_str());
      uint32 pc_save = pc_addr;
      LD_IM([&](Exp<uint8> disp) {
	// True branch.
	// Ugh, be careful here. The displacement byte is treated as a
	// signed integer. NESDEV's 6502.txt implies it uses a sign-bit
	// encoding (unlikely? this would be pretty dumb, since
	// zero is a useless displacement and you get it twice, among
	// other problems). It's possible that there were some tricks
	// going on with x6502.cc since the PC is also being simultaneously
	// incremented. Going with a signed two's complement byte, since
	// other docs imply that and it makes more sense.
	if (false && disp.Known()) {
	  // PERF implement this case. It would allow us to call the
	  // entry point for the branch directly, compute the page
	  // boundary cycle penalty, etc. Should almost always be
	  // known since it comes from immediate byte.
	} else {
	  string disp_sym = GenSym("disp");
	  fprintf(f, I "const int32 %s = (int8)(%s);\n",
		  disp_sym.c_str(),
		  disp.String().c_str());
	  // PERF any reason this can't be combined with the second
	  // ADDCYC below?
	  ADDCYC(f, 1);
	  const uint32 tmp = pc_addr;
	  // Note: Starting at this point we don't know the PC!
	  // (in the true branch..)
	  pc_addr = 0xFFFFFFFF;
	  fprintf(f, I LOCAL_PC " += %s;\n", disp_sym.c_str());
	  fprintf(f, I "if ((0x%04x ^ " LOCAL_PC ") & 0x100) {\n", tmp);
	  // Penalty for crossing page boundary.
	  ADDCYC(f, 1);
	  fprintf(f, I "}\n");
	  // Since the program counter is unknown, we have to
	  // return to the driver.
	  FlushLocals(f, ~0);
	  fprintf(f, I "return; // Unknown JR\n");
	}
      });
      // Execution in the meta language does not follow the same
      // conditional structure as the object language; we need to
      // restore changes to the pc made inside the above.
      pc_addr = pc_save;
      
      fprintf(f, I "} else {\n");
      // False branch.
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I "  " LOCAL_PC " = 0x%04x;\n", pc_addr);
      fprintf(f, I "} // JR\n");
    };

    auto CMPL = [this, &code, f, &X_ZN](string reg, Exp<uint8> a2) {
      const string sym = GenSym("c");
      fprintf(f, I "const uint32 %s = (uint32)(%s) - (uint32)(%s);\n",
	      sym.c_str(),
	      reg.c_str(), a2.String().c_str());
      X_ZN(StringPrintf("%s & 0xFF", sym.c_str()));
      fprintf(f, I LOCAL_P " = (" LOCAL_P " & ~C_FLAG) |\n");
      fprintf(f, I "  (((%s >> 8) & C_FLAG) ^ C_FLAG);\n",
	      sym.c_str());
    };

    auto CMP = [CMPL](Exp<uint8> a2) { CMPL(LOCAL_A, a2); };
    auto CPX = [CMPL](Exp<uint8> a2) { CMPL(LOCAL_X, a2); };
    auto CPY = [CMPL](Exp<uint8> a2) { CMPL(LOCAL_Y, a2); };

    // Undocumented, similar to CMP.
    auto AXS = [this, f, X_ZN](Exp<uint8> x) {
      string sym = GenSym("t");
      fprintf(f, I "const uint16 %s =\n"
	      I "  (uint16)(" LOCAL_A " & " LOCAL_X ") - (uint16)(%s);\n",
	      sym.c_str(), x.String().c_str());
      X_ZN(StringPrintf("(%s & 0xFF)", sym.c_str()));
      fprintf(f,
	      I LOCAL_P " = (" LOCAL_P " & ~C_FLAG) |\n"
	      I "  (((%s >> 8) & C_FLAG) ^ C_FLAG);\n", sym.c_str());
      fprintf(f, LOCAL_X " = %s;\n", sym.c_str());
    };
    
    // PERF many of these operations can have known results if the
    // arguments are known. But currently they are always a register
    // source.
    auto DEC = [this, f, &X_ZN](Exp<uint8> x) {
      const string sym = GenSym("dec");
      fprintf(f, I "const uint8 %s = %s - 1;\n",
	      sym.c_str(), x.String().c_str());
      X_ZN(sym);
      return Exp<uint8>(sym);
    };

    auto INC = [this, f, &X_ZN](Exp<uint8> x) {
      const string sym = GenSym("inc");
      fprintf(f, I "const uint8 %s = %s + 1;\n",
	      sym.c_str(), x.String().c_str());
      X_ZN(sym);
      return Exp<uint8>(sym);
    };

    // Probably "Arithmetic shift left"
    auto ASL = [this, f, &X_ZN](Exp<uint8> x) {
      const string sym = GenSym("asl");
      fprintf(f, I LOCAL_P " = (" LOCAL_P " & ~C_FLAG) | ((%s) >> 7);\n",
	      x.String().c_str());
      fprintf(f, I "const uint8 %s = (uint8)(%s << 1);\n",
	      sym.c_str(), x.String().c_str());
      X_ZN(sym);
      return Exp<uint8>(sym);
    };

    // Probably "Logical shift right" (but note it does modify flags?)
    auto LSR = [this, f, &X_ZNT](Exp<uint8> x) {
      const string sym = GenSym("asl");
      fprintf(f,
	      I LOCAL_P " = (" LOCAL_P " & ~(C_FLAG | N_FLAG | Z_FLAG)) |\n"
	      I "  ((%s) & 1);\n",
	      x.String().c_str());
      fprintf(f, I "const uint8 %s = ((uint8)%s) >> 1;\n",
	      sym.c_str(), x.String().c_str());
      X_ZNT(sym);
      return Exp<uint8>(sym);
    };

    // Maybe "Arithmetic shift right?" Undocumented inst 0x4b.
    auto LSRA = [this, f, X_ZNT]() {
      fprintf(f,
	      I LOCAL_P " = (" LOCAL_P " & ~(C_FLAG | N_FLAG | Z_FLAG)) |\n"
	      I "  (" LOCAL_A " & 1);\n"
	      I LOCAL_A " >>= 1;\n");
      X_ZNT(LOCAL_A);
    };
    
    auto ROL = [this, f, &X_ZNT](Exp<uint8> x) {
      const string sym = GenSym("rol");
      const string xx = GenSym("xx");
      fprintf(f,
	      I "const uint8 %s = (%s) >> 7;\n"
	      I "const uint8 %s = ((%s) << 1) | (" LOCAL_P " & C_FLAG);\n",
	      sym.c_str(), x.String().c_str(),
	      xx.c_str(), x.String().c_str());
      fprintf(
	  f,
	  I LOCAL_P " = (" LOCAL_P " & ~(Z_FLAG | N_FLAG | C_FLAG)) | %s;\n",
	  sym.c_str());
      X_ZNT(xx);
      return Exp<uint8>(xx);
    };

    auto ROR = [this, f, &X_ZNT](Exp<uint8> x) {
      const string sym = GenSym("ror");
      const string xx = GenSym("xx");
      fprintf(f,
	      I "const uint8 %s = (%s) & 1;\n"
	      I "const uint8 %s = ((uint8)(%s) >> 1) |\n"
	      I "  ((" LOCAL_P " & C_FLAG) << 7);\n",
	      sym.c_str(), x.String().c_str(),
	      xx.c_str(), x.String().c_str());
      fprintf(
	  f,
	  I LOCAL_P " = (" LOCAL_P " & ~(Z_FLAG | N_FLAG | C_FLAG)) | %s;\n",
	  sym.c_str());
      X_ZNT(xx);
      return Exp<uint8>(xx);
    };
    
    switch (b1) {
    case 0x00: { /* BRK */
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      PUSH(Exp<uint8>(pc_addr >> 8));
      PUSH(Exp<uint8>(pc_addr & 0xFF));
      const string psym = GenSym("brk");
      fprintf(f, "const uint8 %s = " LOCAL_P " | U_FLAG | B_FLAG;\n",
	      psym.c_str());
      PUSH(Exp<uint8>(psym));
      fprintf(f,
	      I LOCAL_P " |= I_FLAG;\n"
	      I LOCAL_PI " |= I_FLAG;\n");
      Exp<uint8> pc_low = ReadMem(Exp<uint16>(0xFFFE));
      Exp<uint8> pc_high = ReadMem(Exp<uint16>(0xFFFF));
      // PERF sometimes can know this address statically. But we don't
      // do anything with such information yet.
      pc_addr = 0xFFFFFFFF;
      fprintf(f,
	      I LOCAL_PC " = ((uint16)%s) | ((uint16)(%s) << 8);\n",
	      pc_low.String().c_str(),
	      pc_high.String().c_str());

      return 0xFFFFFFFF;
    }
      
    case 0x40: /* RTI */
      POP([&](Exp<uint8> flags) {
	fprintf(f, LOCAL_P " = %s;\n", flags.String().c_str());
	// In x6502, "This is probably incorrect, so it's commented out."
	// (then of course followed by the statement not commented out...)
	fprintf(f, LOCAL_PI " = " LOCAL_P ";\n");
	POP([&](Exp<uint8> pc_low) {
	  POP([&](Exp<uint8> pc_high) {
	    fprintf(f, LOCAL_PC " = ((uint16)%s) | ((uint16)(%s) << 8);\n",
		    pc_low.String().c_str(),
		    pc_high.String().c_str());
	    pc_addr = 0xFFFFFFFF;
	  });
	});
      });
      // Address from RAM.
      return pc_addr;
      
    case 0x60: /* RTS */
      POP([&](Exp<uint8> pc_low) {
	// n.b. this used to set an intermediate value for the PC
	// before doing the second pop, and the pop could conceivably
	// have triggered some read handler that inspected the PC.
	// But not really, since pops are supposed to be directly
	// from RAM.
	POP([&](Exp<uint8> pc_high) {
	  fprintf(f, I LOCAL_PC " = 1 + (((uint16)(%s) << 8) | %s);\n",
		  pc_high.String().c_str(), pc_low.String().c_str());
	});
      });
      // Address from RAM.
      return 0xFFFFFFFF;

    case 0x48: { /* PHA */
      const string sym = GenSym("pha");
      fprintf(f, "const uint8 %s = " LOCAL_A ";\n", sym.c_str());
      PUSH(Exp<uint8>(sym));
      return pc_addr;
    }
    case 0x08: { /* PHP */
      const string sym = GenSym("php");
      fprintf(f, "const uint8 %s = " LOCAL_P " | U_FLAG | B_FLAG;\n",
	      sym.c_str());
      PUSH(Exp<uint8>(sym));
      return pc_addr;
    }
      
    case 0x68: /* PLA */
      POP([&](Exp<uint8> v) {
	fprintf(f, I LOCAL_A " = %s;\n", v.String().c_str());
	X_ZN(LOCAL_A);
      });
      return pc_addr;

    case 0x28: /* PLP */
      POP([&](Exp<uint8> v) {
	fprintf(f, I LOCAL_P " = %s;\n", v.String().c_str());
      });
      return pc_addr;
      

    case 0x4C: {
      /* JMP ABSOLUTE */

      // For some reason the x6502 implementation of this instruction
      // does not modify the pc in between the address reads, which
      // would only make a difference if the second memory location
      // has a read handler.
      const uint32 start_pc = pc_addr;
      Exp<uint8> pc_low = ReadMem(Exp<uint16>(start_pc));
      Exp<uint8> pc_high = ReadMem(Exp<uint16>(start_pc + 1));
      fprintf(f, I LOCAL_PC " = ((uint16)(%s) << 8) | %s;\n",
	      pc_high.String().c_str(),
	      pc_low.String().c_str());
      pc_addr = 0xFFFFFFFF;

      // PERF We often know the actual destination address, and
      // could jump to it directly...
      return pc_addr;
    } 

    case 0x6C: {
      Exp<uint16> tmp = GetAB();
      Exp<uint8> pc_low = ReadMem(tmp);
      Exp<uint16> tmp_plus_1 =
	tmp.Known() ?
	Exp<uint16>(((tmp.Value() + 1) & 0x00FF) | (tmp.Value() & 0xFF00)) :
	Exp<uint16>(StringPrintf("((((%s) + 1) & 0x00FF) | ((%s) & 0xFF00))",
				 tmp.String().c_str(), tmp.String().c_str()));
      Exp<uint8> pc_high = ReadMem(tmp_plus_1);
      
      if (pc_low.Known() && pc_high.Known()) {
	const uint16 new_pc = (uint16)pc_low.Value() |
	  ((uint16)pc_high.Value() << 8);
	pc_addr = new_pc;
	fprintf(f, I LOCAL_PC " = 0x%04x;  // Known jmp 6c\n", pc_addr);
	
      } else {
	// n.b. this does reorder the write to reg_PC with the RdMem calls.
	// Note that nothing accesses the PC except x6502.
	fprintf(f, I LOCAL_PC " = (uint16)(%s) | ((uint16)(%s) << 8); "
		" // Unknown jmp 6c\n",
		pc_low.String().c_str(), pc_high.String().c_str());
	pc_addr = 0xFFFFFFFF;
      }

      // We would know the destination in the true branch above.
      return 0xFFFFFFFF;
    }

    case 0x20: /* JSR */
      // n.b. original code was akin to LD_IM.
      LD_IM([&](Exp<uint8> pc_low) {
	PUSH(Exp<uint8>(255 & (pc_addr >> 8)));
	PUSH(Exp<uint8>(255 & pc_addr));

	// This load increments PC, but it gets immediately overwritten
	// unconditionally here, so should be easy to optimize out.
	LD_IM([&](Exp<uint8> pc_high) {
	  fprintf(f, I LOCAL_PC " = ((uint16)(%s) << 8) | %s;\n",
		  pc_high.String().c_str(), pc_low.String().c_str());
	});
      });
      // We actually know the subroutine being called, but
      // inlining here can easily lead to madness.
      return 0xFFFFFFFF;

    case 0xAA: /* TAX */
      fprintf(f, I LOCAL_X " = " LOCAL_A ";\n");
      X_ZN(LOCAL_A);
      return pc_addr;

    case 0x8A: /* TXA */
      fprintf(f, I LOCAL_A " = " LOCAL_X ";\n");
      X_ZN(LOCAL_A);
      return pc_addr;

    case 0xA8: /* TAY */
      fprintf(f, I LOCAL_Y " = " LOCAL_A ";\n");
      X_ZN(LOCAL_A);
      return pc_addr;
    case 0x98: /* TYA */
      fprintf(f, I LOCAL_A " = " LOCAL_Y ";\n");
      X_ZN(LOCAL_A);
      return pc_addr;

    case 0xBA: /* TSX */
      fprintf(f, I LOCAL_X " = " LOCAL_S ";\n");
      X_ZN(LOCAL_X);
      return pc_addr;
    case 0x9A: /* TXS */
      fprintf(f, I LOCAL_S " = " LOCAL_X ";\n");
      // n.b. no X_ZN in original code. Looks like
      // 6502 docs corroborate. -tom7
      return pc_addr;


    case 0xCA: /* DEX */
      fprintf(f, I LOCAL_X "--;\n");
      X_ZN(LOCAL_X);
      return pc_addr;
    case 0x88: /* DEY */
      fprintf(f, I LOCAL_Y "--;\n");
      X_ZN(LOCAL_Y);
      return pc_addr;

    case 0xE8: /* INX */
      fprintf(f, I LOCAL_X "++;\n");
      X_ZN(LOCAL_X);
      return pc_addr;
    case 0xC8: /* INY */
      fprintf(f, I LOCAL_Y "++;\n");
      X_ZN(LOCAL_Y);
      return pc_addr;

    case 0x18:
      fprintf(f, I LOCAL_P " &= ~C_FLAG;\n");
      return pc_addr;
    case 0xD8:
      fprintf(f, I LOCAL_P " &= ~D_FLAG;\n");
      return pc_addr;
    case 0x58:
      fprintf(f, I LOCAL_P " &= ~I_FLAG;\n");
      return pc_addr;
    case 0xB8:
      fprintf(f, I LOCAL_P " &= ~V_FLAG;\n");
      return pc_addr;

    case 0x38:
      fprintf(f, I LOCAL_P " |= C_FLAG;\n");
      return pc_addr;
    case 0xF8:
      fprintf(f, I LOCAL_P " |= D_FLAG;\n");
      return pc_addr;
    case 0x78:
      fprintf(f, I LOCAL_P " |= I_FLAG;\n");
      return pc_addr;

    case 0xEA:
      // Nop.
      return pc_addr;

    case 0x0A:
      RMW_A(ASL);
      return pc_addr;

    case 0x06:
      RMW_ZP(ASL);
      return pc_addr;

    case 0x16:
      RMW_ZPX(ASL);
      return pc_addr;

    case 0x0E:
      RMW_AB(ASL);
      return pc_addr;

    case 0x1E:
      RMW_ABX(ASL);
      return pc_addr;

    case 0xC6:
      RMW_ZP(DEC);
      return pc_addr;

    case 0xD6:
      RMW_ZPX(DEC);
      return pc_addr;

    case 0xCE:
      RMW_AB(DEC);
      return pc_addr;

    case 0xDE:
      RMW_ABX(DEC);
      return pc_addr;
      
    case 0xE6:
      RMW_ZP(INC);
      return pc_addr;

    case 0xF6:
      RMW_ZPX(INC);
      return pc_addr;
      
    case 0xEE:
      RMW_AB(INC);
      return pc_addr;

    case 0xFE:
      RMW_ABX(INC);
      return pc_addr;

    case 0x4A:
      RMW_A(LSR);
      return pc_addr;

    case 0x46:
      RMW_ZP(LSR);
      return pc_addr;

    case 0x56:
      RMW_ZPX(LSR);
      return pc_addr;

    case 0x4E:
      RMW_AB(LSR);
      return pc_addr;

    case 0x5E:
      RMW_ABX(LSR);
      return pc_addr;

    case 0x2A:
      RMW_A(ROL);
      return pc_addr;

    case 0x26:
      RMW_ZP(ROL);
      return pc_addr;

    case 0x36:
      RMW_ZPX(ROL);
      return pc_addr;

    case 0x2E:
      RMW_AB(ROL);
      return pc_addr;

    case 0x3E:
      RMW_ABX(ROL);
      return pc_addr;

    case 0x6A:
      RMW_A(ROR);
      return pc_addr;

    case 0x66:
      RMW_ZP(ROR);
      return pc_addr;

    case 0x76:
      RMW_ZPX(ROR);
      return pc_addr;

    case 0x6E:
      RMW_AB(ROR);
      return pc_addr;

    case 0x7E:
      RMW_ABX(ROR);
      return pc_addr;

    case 0x69:
      LD_IM(ADC);
      return pc_addr;
    case 0x65:
      LD_ZP(ADC);
      return pc_addr;
      
    case 0x75:
      LD_ZPX(ADC);
      return pc_addr;

    case 0x6D:
      LD_AB(ADC);
      return pc_addr;
    case 0x7D:
      LD_ABX(ADC);
      return pc_addr;
    case 0x79:
      LD_ABY(ADC);
      return pc_addr;

    case 0x61:
      LD_IX(ADC);
      return pc_addr;

    case 0x71:
      LD_IY(ADC);
      return pc_addr;
      
    case 0x29:
      LD_IM(AND);
      return pc_addr;

    case 0x25:
      LD_ZP(AND);
      return pc_addr;

    case 0x35:
      LD_ZPX(AND);
      return pc_addr;

    case 0x2D:
      LD_AB(AND);
      return pc_addr;

    case 0x3D:
      LD_ABX(AND);
      return pc_addr;
    case 0x39:
      LD_ABY(AND);
      return pc_addr;

    case 0x21:
      LD_IX(AND);
      return pc_addr;

    case 0x31:
      LD_IY(AND);
      return pc_addr;

    case 0x24:
      LD_ZP(BIT);
      return pc_addr;
    case 0x2C:
      LD_AB(BIT);
      return pc_addr;

    case 0xC9:
      LD_IM(CMP);
      return pc_addr;

    case 0xC5:
      LD_ZP(CMP);
      return pc_addr;

    case 0xD5:
      LD_ZPX(CMP);
      return pc_addr;

    case 0xCD:
      LD_AB(CMP);
      return pc_addr;
    case 0xDD:
      LD_ABX(CMP);
      return pc_addr;
    case 0xD9:
      LD_ABY(CMP);
      return pc_addr;

    case 0xC1:
      LD_IX(CMP);
      return pc_addr;

    case 0xD1:
      LD_IY(CMP);
      return pc_addr;

    case 0xE0:
      LD_IM(CPX);
      return pc_addr;

    case 0xE4:
      LD_ZP(CPX);
      return pc_addr;

    case 0xEC:
      LD_AB(CPX);
      return pc_addr;
    case 0xC0:
      LD_IM(CPY);
      return pc_addr;

    case 0xC4:
      LD_ZP(CPY);
      return pc_addr;

    case 0xCC:
      LD_AB(CPY);
      return pc_addr;

    case 0x49:
      LD_IM(EOR);
      return pc_addr;
    case 0x45:
      LD_ZP(EOR);
      return pc_addr;

    case 0x55:
      LD_ZPX(EOR);
      return pc_addr;

    case 0x4D:
      LD_AB(EOR);
      return pc_addr;
    case 0x5D:
      LD_ABX(EOR);
      return pc_addr;
    case 0x59:
      LD_ABY(EOR);
      return pc_addr;
    case 0x41:
      LD_IX(EOR);
      return pc_addr;
    case 0x51:
      LD_IY(EOR);
      return pc_addr;

    case 0xA9:
      LD_IM(LDA);
      return pc_addr;

    case 0xA5:
      LD_ZP(LDA);
      return pc_addr;

    case 0xB5:
      LD_ZPX(LDA);
      return pc_addr;

    case 0xAD:
      LD_AB(LDA);
      return pc_addr;
    case 0xBD:
      LD_ABX(LDA);
      return pc_addr;
    case 0xB9:
      LD_ABY(LDA);
      return pc_addr;

    case 0xA1:
      LD_IX(LDA);
      return pc_addr;
    case 0xB1:
      LD_IY(LDA);
      return pc_addr;

    case 0xA2:
      LD_IM(LDX);
      return pc_addr;

    case 0xA6:
      LD_ZP(LDX);
      return pc_addr;

    case 0xB6:
      LD_ZPY(LDX);
      return pc_addr;

    case 0xAE:
      LD_AB(LDX);
      return pc_addr;

    case 0xBE:
      LD_ABY(LDX);
      return pc_addr;

    case 0xA0:
      LD_IM(LDY);
      return pc_addr;

    case 0xA4:
      LD_ZP(LDY);
      return pc_addr;

    case 0xB4:
      LD_ZPX(LDY);
      return pc_addr;

    case 0xAC:
      LD_AB(LDY);
      return pc_addr;

    case 0xBC:
      LD_ABX(LDY);
      return pc_addr;

    case 0x09:
      LD_IM(ORA);
      return pc_addr;

    case 0x05:
      LD_ZP(ORA);
      return pc_addr;

    case 0x15:
      LD_ZPX(ORA);
      return pc_addr;
      
    case 0x0D:
      LD_AB(ORA);
      return pc_addr;
    case 0x1D:
      LD_ABX(ORA);
      return pc_addr;
    case 0x19:
      LD_ABY(ORA);
      return pc_addr;

    case 0x01:
      LD_IX(ORA);
      return pc_addr;

    case 0x11:
      LD_IY(ORA);
      return pc_addr;

    case 0xEB: /* (undocumented) */
    case 0xE9:
      LD_IM(SBC);
      return pc_addr;
      
    case 0xE5:
      LD_ZP(SBC);
      return pc_addr;

    case 0xF5:
      LD_ZPX(SBC);
      return pc_addr;

    case 0xED:
      LD_AB(SBC);
      return pc_addr;
    case 0xFD:
      LD_ABX(SBC);
      return pc_addr;
    case 0xF9:
      LD_ABY(SBC);
      return pc_addr;
    case 0xE1:
      LD_IX(SBC);
      return pc_addr;
    case 0xF1:
      LD_IY(SBC);
      return pc_addr;

    case 0x85: {
      Exp<uint8> x = Read(f, REG_A);
      ST_ZP(x);
      return pc_addr;
    }
    case 0x95: {
      Exp<uint8> x = Read(f, REG_A);
      ST_ZPX(x);
      return pc_addr;
    }
    case 0x8D: {
      Exp<uint8> x = Read(f, REG_A);
      ST_AB(x);
      return pc_addr;
    }
    case 0x9D:
      ST_ABX([this, f](Exp<uint16> unused) { return Read(f, REG_A); });
      return pc_addr;
    case 0x99:
      ST_ABY([this, f](Exp<uint16> unused) { return Read(f, REG_A); });
      return pc_addr;

    case 0x81: {
      Exp<uint8> x = Read(f, REG_A);
      ST_IX(x);
      return pc_addr;
    }
    case 0x91:
      ST_IY([this, f](Exp<uint16> aa_unused) { return Read(f, REG_A); });
      return pc_addr;

    case 0x86: {
      Exp<uint8> x = Read(f, REG_X);
      ST_ZP(Exp<uint8>(x));
      return pc_addr;
    }
      
    case 0x96: {
      Exp<uint8> x = Read(f, REG_X);
      ST_ZPY(x);
      return pc_addr;
    }
      
    case 0x8E: {
      Exp<uint8> x = Read(f, REG_X);
      ST_AB(x);
      return pc_addr;
    }
    case 0x84: {
      Exp<uint8> x = Read(f, REG_Y);
      ST_ZP(Exp<uint8>(x));
      return pc_addr;
    }

    case 0x94: {
      Exp<uint8> x = Read(f, REG_Y);
      ST_ZPX(x);
      return pc_addr;
    }
    case 0x8C: {
      Exp<uint8> x = Read(f, REG_Y);
      ST_AB(x);
      return pc_addr;
    }
      /* BCC */
    case 0x90: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(!(%s & C_FLAG))", p.String().c_str())));
      return pc_addr;
    }
      /* BCS */
    case 0xB0: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(%s & C_FLAG)", p.String().c_str())));
      return pc_addr;
    }
      /* BEQ */
    case 0xF0: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(%s & Z_FLAG)", p.String().c_str())));
      return pc_addr;
    }
      /* BNE */
    case 0xD0: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(!(%s & Z_FLAG))", p.String().c_str())));
      return pc_addr;
    }
      /* BMI */
    case 0x30: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(%s & N_FLAG)", p.String().c_str())));
      return pc_addr;
    }

      /* BPL */
    case 0x10: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(!(%s & N_FLAG))", p.String().c_str())));
      return pc_addr;
    }
      /* BVC */
    case 0x50: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(!(%s & V_FLAG))", p.String().c_str())));
      return pc_addr;
    }
      /* BVS */
    case 0x70: {
      Exp<uint8> p = Read(f, REG_P);
      JR(Exp<uint8>(StringPrintf("(%s & V_FLAG)", p.String().c_str())));
      return pc_addr;
    }

      /* Here comes the undocumented instructions block.  Note that this
	 implementation may be "wrong".  If so, please tell me.
      */

      /* AAC */
    case 0x2B:
    case 0x0B:
      LD_IM(AND);
      fprintf(f, I LOCAL_P " = (" LOCAL_P " & ~C_FLAG) | "
	      "(" LOCAL_A " >> 7);\n");
      return pc_addr;
      
      /* AAX */
    case 0x87: {
      Exp<uint8> a = Read(f, REG_A);
      Exp<uint8> x = Read(f, REG_X);
      ST_ZP(Exp<uint8>(StringPrintf("(%s & %s)",
				    a.String().c_str(), x.String().c_str())));
      return pc_addr;
    }
      
    case 0x97: {
      Exp<uint8> a = Read(f, REG_A);
      Exp<uint8> x = Read(f, REG_X);
      ST_ZPY(Exp<uint8>(StringPrintf("(%s & %s)",
				    a.String().c_str(), x.String().c_str())));
      return pc_addr;
    }
      
    case 0x8F: {
      Exp<uint8> a = Read(f, REG_A);
      Exp<uint8> x = Read(f, REG_X);
      ST_AB(Exp<uint8>(StringPrintf("(%s & %s)",
				    a.String().c_str(), x.String().c_str())));
      return pc_addr;
    }
      
    case 0x83: {
      Exp<uint8> a = Read(f, REG_A);
      Exp<uint8> x = Read(f, REG_X);
      ST_IX(Exp<uint8>(StringPrintf("(%s & %s)",
				    a.String().c_str(), x.String().c_str())));
      return pc_addr;
    }
      /* ARR - ARGH, MATEY! */
    case 0x6B:
      LD_IM([&](Exp<uint8> x) {
	AND(x);
	string sym = GenSym("arr");
	fprintf(f,
		I LOCAL_P " = (" LOCAL_P " & ~V_FLAG) |\n"
		I "  ((" LOCAL_A " ^ (" LOCAL_A " >> 1)) & 0x40);\n");
	fprintf(f, I "const uint8 %s = " LOCAL_A " >> 7;\n", sym.c_str());
	fprintf(f,
		I LOCAL_A " >>= 1;\n"
		I LOCAL_A " |= (" LOCAL_P " & C_FLAG) << 7;\n"
		I LOCAL_P " = (" LOCAL_P " & ~C_FLAG) | %s;\n", sym.c_str());
	X_ZN(LOCAL_A);
      });
      return pc_addr;
      
      /* ASR */
    case 0x4B:
      LD_IM(AND);
      LSRA();
      return pc_addr;
      
      /* ATX(OAL) */
    case 0xAB:
      // Hmm, this instruction really doesn't make sense. At some
      // point, the comment indicates that A gets ORed with 0xEE, but
      // that they did some tests and it was actually 0xFF. But (A | 0xFF)
      // is of course just 0xFF, and then 0xFF & x is just x.
      // I simplified it. -tom7
      LD_IM([&](Exp<uint8> x) {
	LDA(x);
	LDX(x);
      });
      return pc_addr;

      /* AXS */
    case 0xCB:
      LD_IM(AXS);
      return pc_addr;

      /* DCP */
    case 0xC7:
      RMW_ZP([&](Exp<uint8> x) {
	Exp<uint8> y = DEC(x);
	CMP(y);
	return y;
      });
      return pc_addr;

    case 0xD7:
      RMW_ZPX([&](Exp<uint8> x) {
	Exp<uint8> y = DEC(x);
	CMP(y);
	return y;
      });
      return pc_addr;

    case 0xCF:
      RMW_AB([&](Exp<uint8> x) {
	Exp<uint8> y = DEC(x);
	CMP(y);
	return y;
      });
      return pc_addr;

    case 0xDF:
      RMW_ABX([&](Exp<uint8> x) {
	Exp<uint8> y = DEC(x);
	CMP(y);
	return y;
      });
      return pc_addr;
    case 0xDB:
      RMW_ABY([&](Exp<uint8> x) {
	Exp<uint8> y = DEC(x);
	CMP(y);
	return y;
      });
      return pc_addr;

    case 0xC3:
      RMW_IX([&](Exp<uint8> x) {
	Exp<uint8> y = DEC(x);
	CMP(y);
	return y;
      });
      return pc_addr;

    case 0xD3:
      RMW_IY([&](Exp<uint8> x) {
	Exp<uint8> y = DEC(x);
	CMP(y);
	return y;
      });
      return pc_addr;

      /* ISB */
    case 0xE7:
      RMW_ZP([&](Exp<uint8> x) {
	Exp<uint8> y = INC(x);
	SBC(y);
	return y;
      });
      return pc_addr;

    case 0xF7:
      RMW_ZPX([&](Exp<uint8> x) {
	Exp<uint8> y = INC(x);
	SBC(y);
	return y;
      });
      return pc_addr;

    case 0xEF:
      RMW_AB([&](Exp<uint8> x) {
	Exp<uint8> y = INC(x);
	SBC(y);
	return y;
      });
      return pc_addr;

    case 0xFF:
      RMW_ABX([&](Exp<uint8> x) {
	Exp<uint8> y = INC(x);
	SBC(y);
	return y;
      });
      return pc_addr;
    case 0xFB:
      RMW_ABY([&](Exp<uint8> x) {
	Exp<uint8> y = INC(x);
	SBC(y);
	return y;
      });
      return pc_addr;

    case 0xE3:
      RMW_IX([&](Exp<uint8> x) {
	Exp<uint8> y = INC(x);
	SBC(y);
	return y;
      });
      return pc_addr;

    case 0xF3:
      RMW_IY([&](Exp<uint8> x) {
	Exp<uint8> y = INC(x);
	SBC(y);
	return y;
      });
      return pc_addr;

      /* DOP */
    case 0x04: 
    case 0x14: 
    case 0x34: 
    case 0x44: 
    case 0x54: 
    case 0x64: 
    case 0x74: 

    case 0x80: 
    case 0x82: 
    case 0x89: 
    case 0xC2: 
    case 0xD4: 
    case 0xE2: 
    case 0xF4:
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      return pc_addr;

    /* KIL */
    // These are illegal instructions, I think.
    case 0x02:
    case 0x12:
    case 0x22:
    case 0x32:
    case 0x42:
    case 0x52:
    case 0x62:
    case 0x72:
    case 0x92:
    case 0xB2:
    case 0xD2:
    case 0xF2:
      // PERF These instructions can be executed MUCH faster,
      // but it's not clear if any carts are actually executing
      // them (seems to freeze the processor and only allow
      // a reset).
      ADDCYC(f, 0xFF);
      fprintf(f, I LOCAL_JAMMED " = 1;\n  // KIL = %02x\n", b1);
      pc_addr--; pc_addr &= 0xFFFF;
      fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr);
      return 0xFFFFFFFF;

      /* LAR */
    case 0xBB:
      RMW_ABY([&](Exp<uint8> x) {
	fprintf(f, I LOCAL_S " &= %s;\n", x.String().c_str());
	fprintf(f, I LOCAL_A " = " LOCAL_X " = " LOCAL_S ";\n");
	X_ZN(LOCAL_X);
	return x;
      });
      return pc_addr;

      /* LAX */
    case 0xA7:
      LD_ZP([&](Exp<uint8> x) {
	LDA(x);
	LDX(x);
      });
      return pc_addr;

    case 0xB7:
      LD_ZPY([&](Exp<uint8> x) {
	LDA(x);
	LDX(x);
      });
      return pc_addr;

    case 0xAF:
      LD_AB([&](Exp<uint8> x) {
	LDA(x);
	LDX(x);
      });
      return pc_addr;

    case 0xBF:
      LD_ABY([&](Exp<uint8> x) {
	LDA(x);
	LDX(x);
      });
      return pc_addr;

    case 0xA3:
      LD_IX([&](Exp<uint8> x) {
	LDA(x);
	LDX(x);
      });
    return pc_addr;

    case 0xB3:
      LD_IY([&](Exp<uint8> x) {
	LDA(x);
	LDX(x);
      });
    return pc_addr;

      /* NOP */
    case 0x1A:
    case 0x3A:
    case 0x5A:
    case 0x7A:
    case 0xDA:
    case 0xFA:
      fprintf(f, I "// NOP %02x\n", b1);
      return pc_addr;

      /* RLA */
    case 0x27:
      RMW_ZP([&](Exp<uint8> x) {
	Exp<uint8> y = ROL(x);
	AND(y);
	return y;
      });
      return pc_addr;

    case 0x37:
      RMW_ZPX([&](Exp<uint8> x) {
	Exp<uint8> y = ROL(x);
	AND(y);
	return y;
      });
      return pc_addr;

    case 0x2F:
      RMW_AB([&](Exp<uint8> x) {
	Exp<uint8> y = ROL(x);
	AND(y);
	return y;
      });
      return pc_addr;

    case 0x3F:
      RMW_ABX([&](Exp<uint8> x) {
	Exp<uint8> y = ROL(x);
	AND(y);
	return y;
      });
      return pc_addr;
    case 0x3B:
      RMW_ABY([&](Exp<uint8> x) {
	Exp<uint8> y = ROL(x);
	AND(y);
	return y;
      });
      return pc_addr;

    case 0x23:
      RMW_IX([&](Exp<uint8> x) {
	Exp<uint8> y = ROL(x);
	AND(y);
	return y;
      });
      return pc_addr;

    case 0x33:
      RMW_IY([&](Exp<uint8> x) {
	Exp<uint8> y = ROL(x);
	AND(y);
	return y;
      });
      return pc_addr;

      /* RRA */
    case 0x67:
      RMW_ZP([&](Exp<uint8> x) {
	Exp<uint8> y = ROR(x);
	ADC(y);
	return y;
      });
      return pc_addr;

    case 0x77:
      RMW_ZPX([&](Exp<uint8> x) {
	Exp<uint8> y = ROR(x);
	ADC(y);
	return y;
      });
      return pc_addr;

    case 0x6F:
      RMW_AB([&](Exp<uint8> x) {
	Exp<uint8> y = ROR(x);
	ADC(y);
	return y;
      });
      return pc_addr;

    case 0x7F:
      RMW_ABX([&](Exp<uint8> x) {
	Exp<uint8> y = ROR(x);
	ADC(y);
	return y;
      });
      return pc_addr;

    case 0x7B: 
      RMW_ABY([&](Exp<uint8> x) {
	Exp<uint8> y = ROR(x);
	ADC(y);
	return y;
      });
      return pc_addr;

    case 0x63: 
      RMW_IX([&](Exp<uint8> x) {
	Exp<uint8> y = ROR(x);
	ADC(y);
	return y;
      });
      return pc_addr;

    case 0x73:
      RMW_IY([&](Exp<uint8> x) {
	Exp<uint8> y = ROR(x);
	ADC(y);
	return y;
      });
      return pc_addr;

      /* SLO */
    case 0x07:
      RMW_ZP([&](Exp<uint8> x) {
	Exp<uint8> y = ASL(x);
	ORA(y);
	return y;
      });
      return pc_addr;

    case 0x17:
      RMW_ZPX([&](Exp<uint8> x) {
	Exp<uint8> y = ASL(x);
	ORA(y);
	return y;
      });
      return pc_addr;

    case 0x0F:
      RMW_AB([&](Exp<uint8> x) {
	Exp<uint8> y = ASL(x);
	ORA(y);
	return y;
      });
      return pc_addr;

    case 0x1F:
      RMW_ABX([&](Exp<uint8> x) {
	Exp<uint8> y = ASL(x);
	ORA(y);
	return y;
      });
      return pc_addr;

    case 0x1B:
      RMW_ABY([&](Exp<uint8> x) {
	Exp<uint8> y = ASL(x);
	ORA(y);
	return y;
      });
      return pc_addr;

    case 0x03:
      RMW_IX([&](Exp<uint8> x) {
	Exp<uint8> y = ASL(x);
	ORA(y);
	return y;
      });
      return pc_addr;

    case 0x13:
      RMW_IY([&](Exp<uint8> x) {
	Exp<uint8> y = ASL(x);
	ORA(y);
	return y;
      });
      return pc_addr;

      /* SRE */
    case 0x47:
      RMW_ZP([&](Exp<uint8> x) {
	Exp<uint8> y = LSR(x);
	EOR(y);
	return y;
      });

    case 0x57:
      RMW_ZPX([&](Exp<uint8> x) {
	Exp<uint8> y = LSR(x);
	EOR(y);
	return y;
      });
      return pc_addr;

    case 0x4F:
      RMW_AB([&](Exp<uint8> x) {
	Exp<uint8> y = LSR(x);
	EOR(y);
	return y;
      });
      return pc_addr;

    case 0x5F:
      RMW_ABX([&](Exp<uint8> x) {
	Exp<uint8> y = LSR(x);
	EOR(y);
	return y;
      });
      return pc_addr;

    case 0x5B:
      RMW_ABY([&](Exp<uint8> x) {
	Exp<uint8> y = LSR(x);
	EOR(y);
	return y;
      });
      return pc_addr;

    case 0x43:
      RMW_IX([&](Exp<uint8> x) {
	Exp<uint8> y = LSR(x);
	EOR(y);
	return y;
      });
      return pc_addr;

    case 0x53:
      RMW_IY([&](Exp<uint8> x) {
	Exp<uint8> y = LSR(x);
	EOR(y);
	return y;
      });
      return pc_addr;

      /* AXA - SHA */
    case 0x93:
      ST_IY([this, f](Exp<uint16> aa) {
	Exp<uint8> a = Read(f, REG_A);
	Exp<uint8> x = Read(f, REG_X);
	Exp<uint8> y = Read(f, REG_Y);
	return Exp<uint8>(
	    StringPrintf("(%s & %s & "
			 "((((uint16)(%s) - %s) >> 8) + 1))",
			 a.String().c_str(), x.String().c_str(),
			 aa.String().c_str(),
			 y.String().c_str()));
      });
      return pc_addr;
      
    case 0x9F:
      ST_ABY([this, f](Exp<uint16> aa) {
	Exp<uint8> a = Read(f, REG_A);
	Exp<uint8> x = Read(f, REG_X);
	Exp<uint8> y = Read(f, REG_Y);
	return Exp<uint8>(
	    StringPrintf(
		"(%s & %s & "
		"((((uint16)(%s) - %s) >> 8) + 1))",
		a.String().c_str(), x.String().c_str(),
		aa.String().c_str(),
		y.String().c_str()));
      });
      return pc_addr;

    /* SYA */
    case 0x9C:
      ST_ABX([this, f](Exp<uint16> aa) {
	Exp<uint8> x = Read(f, REG_X);
	Exp<uint8> y = Read(f, REG_Y);
	return Exp<uint8>(
	    StringPrintf(
		"(%s & ((((uint16)(%s) - %s) >> 8) + 1))",
		y.String().c_str(),
		aa.String().c_str(),
		x.String().c_str()));
      });
      return pc_addr;

      /* SXA */
    case 0x9E:
      ST_ABY([this, f](Exp<uint16> aa) {
	Exp<uint8> x = Read(f, REG_X);
	Exp<uint8> y = Read(f, REG_Y);
	return Exp<uint8>(
	    StringPrintf(
		"(%s & ((((uint16)(%s) - %s) >> 8) + 1))",
		x.String().c_str(), 
		aa.String().c_str(),
		y.String().c_str()));
      });
      return pc_addr;

      /* XAS */
    case 0x9B:
      fprintf(f, I LOCAL_S " = " LOCAL_A " & " LOCAL_X ";\n");
      ST_ABY([this, f](Exp<uint16> aa) {
	Exp<uint8> s = Read(f, REG_S);
	Exp<uint8> y = Read(f, REG_Y);
	return Exp<uint8>(
	    StringPrintf(
		"(%s & ((((uint16)(%s) - %s) >> 8) + 1))",
		s.String().c_str(),
		aa.String().c_str(),
		y.String().c_str()));
      });
      return pc_addr;

      /* TOP */
    case 0x0C:
      LD_AB([f](Exp<uint8> x) {
	fprintf(f, I "(void) %s;  // TOP\n", x.String().c_str());
      });
      return pc_addr;

    case 0x1C:
    case 0x3C:
    case 0x5C:
    case 0x7C:
    case 0xDC:
    case 0xFC:
      LD_ABX([f, b1](Exp<uint8> x) {
	fprintf(f, I "(void) %s;  // %02x\n", x.String().c_str(), b1);
      });
      return pc_addr;

      /* XAA - BIG QUESTION MARK HERE */
    case 0x8B: {
      fprintf(f, I LOCAL_A " = (0xEE | " LOCAL_A ") & " LOCAL_X ";\n");
      LD_IM(AND);
      return pc_addr;
    }

    default:;
    }
    LOG(FATAL) << "Unimplemented inst " << StringPrintf("0x%02x", b1) << "\n"
    "(or forgot to return pc_addr, fell through or did break).";
    return 0xFFFFFFFF;
  }

  void ADDCYC(FILE *f, int cycles) {
    // XXX local timestamp...?
    fprintf(f, I LOCAL_TCOUNT " += %d; "
	    LOCAL_COUNT " -= %d; "
	    "X->timestamp += %d;\n",
	    cycles, cycles * 48, cycles);
  }

  // XXX need to be checking count and returning when
  // out of time!
  //
  // need to be able to jump to relative addresses, I guess
  // by updating PC and returning?
  void GenerateEntry(const CodeConfig &config,
		     const Code &code,
		     uint32 entry_addr,
		     uint32 addr_past_end,
		     const string &symbol,
		     FILE *f) {
    fprintf(f,
	    "void %s_%04x(FC *fc) {\n",
	    symbol.c_str(), entry_addr);

    // Copies of FC objects, used locally.
    fprintf(f,
	    "  X6502 *X = fc->X; (void)X;\n"
	    // "  const FCEU *fceu = fc->fceu;\n"  // const?
	    "  Sound *sound = fc->sound; (void)sound;\n"
	    "  FCEU *fceu = fc->fceu; (void)fceu;\n"  // const?
	    );

    DeclLocals(f);
    
    uint32 pc_addr = entry_addr;
    for (;;) {
      // We don't want to try reading outside the mapped region, of
      // course.
      // Note that we don't really have any effective protection
      // against an infinite codegen loop if 0000-FFFF is all mapped,
      // but this should never be the case since 0-0x7ff is
      // architectually RAM.
      // XXX isn't this just like code.Known()?
      if (pc_addr < entry_addr || pc_addr >= addr_past_end) {
	fprintf(f, I "// PC $%04x exits code region.\n", pc_addr);
	FlushLocals(f, ~0);
	fprintf(f,
		I "%s_any(fc);\n"
		I "return;\n", symbol.c_str());
	break;
      }

      const uint8 b1 = code.Get(pc_addr);
      if (!CanGenInstruction(b1)) {
	fprintf(f, I "// Unimplemented instruction $%02x\n", b1);
	FlushLocals(f, ~0);
	fprintf(f,
		I "%s_any(fc);\n"
		I "return;\n", symbol.c_str());
	break;
      } else {
	// PERF: Would be nice to avoid testing this over and over,
	// like by testing the number of cycles we need for the
	// straight-line code at the top of the entry point, and just
	// running the slow interpreter if we don't have enough cycles
	// left.
	fprintf(f, I "if (" LOCAL_COUNT " <= 0) {\n");
	FlushLocals(f, ~0);
	fprintf(f, I "  return;\n"
		I "}");

	fprintf(f, I "// %04x = %02x\n", pc_addr, b1);

	// PERF: Similarly, good to avoid testing this over and over.
	// Mappers can trigger interrupt, as can sound.
	fprintf(f, I "if (" LOCAL_IRQLOW ") {\n");
	FlushLocals(f, ~0);
	fprintf (f, I "%s_any(fc); return; }\n",
		 symbol.c_str());

	fprintf(f, I LOCAL_PI " = " LOCAL_P ";\n");

	const int cycles = CycTable[b1];
	// ADDCYC() macro.
	ADDCYC(f, cycles);

	// XXX sort out whether we need to be modifying tcount now
	// that we have locals.
	// "temp" only used for the calls to irq and sound hooks, I guess
	// in case they try to read tcount? (Actually, these calls can
	// trigger DMA calls that call ADDCYC, so we need to make sure
	// that we don't set to 0 after the call.)
	fprintf(f, I "{\n"
		I "  const int32 temp = " LOCAL_TCOUNT ";\n"
		I "  " LOCAL_TCOUNT " = 0;\n");

	// Known effects of sound:
	//  - Sound can trigger an interrupt (so read/write irqlow)
	//  - Sound calls DMR, which does ADDCYC, overwrites DB,
	//    and calls read handlers.
	//    (maybe never reads from db though?)
	//    HOWEVER, the address it does DMR from are guaranteed
	//    to be >= 0x8000. So if we know something about the
	//    mapper for this region, we can limit the collateral
	//    to just DB/ADDCYC?
	//
	// FlushEmulateSound uses timestamp, but that is only called
	// externally, not from SoundCPUHook.
	//
	// Other than this, the sound code is pretty self contained.
	// So basically, if we know that DMC is not active, then
	// we just have to worry about the interrupt. If we know that
	// reads from 0x8000-0xFFFF don't have an effect, then we
	// only need to flush:
	//   - ADDCYC stuff
	//   - interrupt flag
	// and only need to read
	//   - ADDCYC stuff,
	//   - interrupt flag
	//   - DB
	// Since this is true for mario (and probably many games),
	// let's do that latter case first. 

	if (config.effectless_read_8000_ffff) {
	  FlushLocals(f, ID_TCOUNT | ID_COUNT | ID_IRQLOW);
	} else {
	  // Slow case. In this case, could build a mask of all the
	  // possibly-read regs for all the read mappers in 8000-ffff.
	  FlushLocals(f, ~PRIVATE_TO_X6502);
	}

	CHECK(!config.has_map_irq_hook) << "Not supported (yet)?";
	// PERF Can remove/simplify calls to sound hook?
	fprintf(f, I "  sound->SoundCPUHook(temp);\n");

	if (config.effectless_read_8000_ffff) {
	  // See above, except DB may be written, so restore that.
	  LoadLocals(f, ID_TCOUNT | ID_COUNT | ID_IRQLOW | ID_DB);
	} else {
	  LoadLocals(f, ~PRIVATE_TO_X6502);
	}

	fprintf(f, I "}\n");
	
	pc_addr++;
	fprintf(f, I LOCAL_PC " = 0x%04x;\n", pc_addr & 0xFFFF);

	// Instructions may introduce local variables (e.g. a temporary
	// containing the result of calling a read handler), but they
	// only need to be in scope for the instruction body.
	fprintf(f, I "{\n");
	pc_addr = GenInstruction(code, b1, pc_addr, f);
	fprintf(f, I "}\n");
	if (pc_addr == 0xFFFFFFFF) {
	  fprintf(f, I "// Branch was unconditional.\n");
	  FlushLocals(f, ~0);
	  fprintf(f, I "return;\n");
	  break;
	}
      }
    }

    fprintf(f, "}  // %s_%04x\n\n", symbol.c_str(), entry_addr);
  }

  string GenSym(const string &base) {
    next_symbol++;
    return StringPrintf("_%s_%lld", base.c_str(), next_symbol);
  }

  string GenSym() { return GenSym("sym"); }
  
  int64 next_symbol = 0;
};

// Returns the files written (without .cc extension).
static vector<string> GenerateCode(const CodeConfig &config,
				   const Code &code,
				   uint32 addr_start,
				   uint32 addr_past_end,
				   const string &symbol,
				   const string &cart_name) {
  CHECK(addr_start <= 0xFFFF);
  CHECK(addr_past_end <= 0x10000);

  // We generate a routine just like X6502::Run. This routine only
  // works if the address is in the mapped range [addr_start,
  // addr_past_end). It assumes that reads within the mapped range are
  // constant. (Note: It would improve performance if we knew other
  // addresses to be unmappable (or known-mapped) ROM, because then we
  // could avoid the indirection of calling ARead, and replacing reads
  // with literals would increase optimization opportuities.)

  // Any time we get stuck, we can appeal to the real X6502 interpreter
  // (RunLoop), which is of course fully general. So our goal here is
  // to find cases that are very common and optimize those.

  vector<int> start_addrs;
  for (int i = addr_start;
       i < addr_past_end;
       i += config.entrypoints_per_file) {
    start_addrs.push_back(i);
  }

  std::mutex ret_m;
  vector<string> ret;
  
  auto F = [&config, &code, addr_start, addr_past_end, &symbol, &cart_name,
	    &ret_m, &ret](int i) {
    AOT aot;
    string filebase = StringPrintf("%s_%d", symbol.c_str(), i);
    string filename = filebase + ".cc";
    FILE *f = fopen(filename.c_str(), "w");
    
    // Prelude.
    fprintf(f,
	    "// Generated code! Do not edit.\n"
	    "// Generated from %s on [DATE].\n"
	    "\n"
	    "#include <cstdint>\n"
	    "\n"
	    "#include \"fc.h\"\n"
	    "#include \"x6502.h\"\n"
	    "#include \"sound.h\"\n"
	    "#include \"fceu.h\"\n",
	    cart_name.c_str());

    fprintf(f, "\n/* aot-prelude.inc */\n%s\n/* aot-prelude.inc */\n",
	    ReadFile("aot-prelude.inc").c_str());

    fprintf(f, "\n\n");

    // First, a function to call when we don't have a compiled
    // version.
    fprintf(f, "static void %s_any(FC *fc) { fc->X->RunLoop(); }\n\n",
	    symbol.c_str());

    // Then, a function for each address entry point.
    for (uint32 j = i;
	 j < addr_past_end && j < i + config.entrypoints_per_file;
	 j++) {
      aot.GenerateEntry(config, code, j, addr_past_end, symbol, f);      
    }

    fclose(f);
    {
      MutexLock ml(&ret_m);
      ret.push_back(filebase);
      fprintf(stderr, "Wrote %s.\n", filename.c_str());
    }
  };
  
  ParallelApp(start_addrs, F, 16);
  return ret;
}

// One of these per compiled file. It does the per-Run setup and
// coordinates control transfer between the chunks.
static void GenerateDispatcher(const CodeConfig &config,
			       uint32 addr_start,
			       uint32 addr_past_end,
			       const string &symbol,
			       const string &filename) {
  FILE *f = fopen(filename.c_str(), "w");
  CHECK(f) << filename;

  fprintf(f,
	  "#include <cstdint>\n"
	  "#include \"x6502.h\"\n"
	  "#include \"fc.h\"\n"
	  "#include \"fceu.h\"\n\n");
  
  // Avoid needing a header file; just generate the externs here.
  for (int i = addr_start; i < addr_past_end; i++) {
    fprintf(f, "void %s_%04x(FC *);\n", symbol.c_str(), i);
  }

  // We put this in every file, but also need it here for gaps in the
  // entry table.
  fprintf(f, "\n\nstatic void %s_any(FC *fc) { fc->X->RunLoop(); }\n\n",
	  symbol.c_str());
  
  fprintf(f, "static void (*entries[0x10000])(FC *fc) = {\n");
  for (int i = 0; i < 0x10000; i++) {
    if (i < addr_start || i >= addr_past_end)
      fprintf(f, "  &%s_any,\n", symbol.c_str());
    else
      fprintf(f, "  &%s_%04x,\n", symbol.c_str(), i);
  }
  fprintf(f, "};  // entries array\n\n");

  fprintf(f, "// Dispatcher.\n"
	  "void %s_Run(FC *fc, int32 cycles) {\n",
	  symbol.c_str());

  fprintf(f, "  X6502 *X = fc->X;\n");

  if (config.is_pal) fprintf(f, "  cycles *= 15;  // is pal\n");
  else fprintf(f, "  cycles *= 16;  // is ntsc\n");

  // PERF could pass this rather than loading it, though note
  // the different calling conventions between AOT and _any code.
  fprintf(f, "  X->count += cycles;\n");

  fprintf(f, "  while (X->count > 0) {\n");
  fprintf(f, "    const uint16 pc = X->reg_PC;\n");
  fprintf(f, "    void (*const entry)(FC *) = entries[pc];\n");
  fprintf(f, "    (*entry)(fc);\n");
  fprintf(f, "  }\n");

  fprintf(f, "}  // Dispatcher.\n\n\n");

  fprintf(stderr, "Wrote %s.\n", filename.c_str());
  fclose(f);
}

// XXX this could come from text file, duh
RomConfig MarioROM() {
  RomConfig rom;
  rom.file = "mario.nes";
  rom.entrypoints_per_file = 512;
  // This is where the code resides in mario.nes. It never gets
  // remapped (mapper 0 cannot remap), and it is not writable
  // (only RAM and 6000-7fff are writable).
  rom.code_addr_start = 0x8000;
  rom.code_addr_after_end = 0x10000;
  rom.effectless_read_8000_ffff = true;
  return rom;
}

RomConfig ContraROM() {
  RomConfig rom;
  // This is mapper 2, UNROM. See boards/datalatch.cc.
  rom.file = "contra.nes";
  rom.entrypoints_per_file = 16;
  // 8000-BFFF is bank switched.
  // C000-FFFF is hardwired to last bank.
  // The PC does seem to go into 8000-BFFF (fceux debugger), so
  // the fact that we don't understand bank switching here likely
  // hurts us.
  rom.code_addr_start = 0xC000;
  // The last bank of contra ends with a bunch of FF FF FF FF,
  // which produecs bad bloated code that we don't need. So stop
  // compiling before that.
  rom.code_addr_after_end = 0xf5ac;
  // Writes definitely do something (bank switching), but I think
  // reads are effectless.
  rom.effectless_read_8000_ffff = true;
  return rom;
}

int main(int argc, char **argv) {
  string romdir = "roms/";

  if (argc != 2) {
    printf("Give the name of a game (no extension) on the command-line; "
	   "it must have already been documented inside aot.cc.\n");
    return -1;
  }

  string game = argv[1];
  RomConfig rom;
  if (game == "mario") {
    rom = MarioROM();
  } else if (game == "contra") {
    rom = ContraROM();
  } else {
    printf("Unknown game %s. You have to put some constants in aot.cc.\n",
	   game.c_str());
    return -1;
  }
  
  std::unique_ptr<Emulator> emu(Emulator::Create(romdir + "/" + rom.file));

  Timer compile_timer;

  FC *fc = emu->GetFC();

  // Note that even if an area isn't writable, it's possible that it's
  // unmapped, in which case the read returns the value of the last
  // read (this is usually predictable statically, but definitely
  // complicates things). For mapper 0, all of 0x8000-0x7fff is mapped
  // to cart rom (CartBR).
  Code code;
  for (uint32 addr = rom.code_addr_start;
       addr < rom.code_addr_after_end;
       addr++) {
    code.known[addr] = true;
    code.code[addr] = fc->fceu->ARead[addr](fc, addr);
    // Can do this to generate all instructions to make sure they
    // compile! (Though it does not exercise all cases in the code...)
    // code.code[addr] = addr & 0xFF;
  }

  CodeConfig config;
  config.entrypoints_per_file = rom.entrypoints_per_file;
  config.is_pal = !!fc->fceu->PAL;
  config.has_map_irq_hook = fc->X->MapIRQHook != nullptr;
  config.effectless_read_8000_ffff = rom.effectless_read_8000_ffff;

  vector<string> files =
    GenerateCode(config, code, rom.code_addr_start, rom.code_addr_after_end,
		 game, rom.file);
  GenerateDispatcher(config, rom.code_addr_start, rom.code_addr_after_end,
		     game, game + ".cc");
  files.push_back(game);

  {
    // This just makes it easier to manually build makefiles during
    // development. XXX do this a better way.
    FILE *mf = fopen(StringPrintf("%s.makefile", game.c_str()).c_str(),
		     "w");
    CHECK(mf != nullptr);
    fprintf(mf, "FCEULIB_GAME_OBJECTS= ");
    for (const string &f : files) {
      fprintf(mf, "%s.o ", f.c_str());
    }
    fprintf(mf, "\n");
    fclose(mf);
  }
    
  double compile_seconds = compile_timer.GetSeconds();
  
  fprintf(stderr, "Finished.\n"
          "Compile time: %.4fs\n",
          compile_seconds);
  
  return 0;
}
