
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

static bool CanGenInstruction(uint8 b1) {
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

    // Above instructions were good in mario benchmark.

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
  T Value() {
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
  // XXX: Needs to take ROM and PC so that it can read multi-byte
  // instructions.
  //
  // Takes the known code values.
  // Takes the instruction byte, b1, and the pc value after reading
  // that byte. For some instructions, we advance the pc further.
  //
  // Returns new value of PC after this instruction (if it does not
  // branch). Returns 0xFFFFFFFF if the PC is not known (or if we
  // otherwise don't want to continue generating code); this causes
  // the entry point generator ..  XXX HERE

  uint32 GenInstruction(const Code &code,
			uint8 b1, uint32 pc_addr, FILE *f) {

    auto ReadMem = [this, &code, f](Exp<uint16> addr) -> Exp<uint8> {
      Exp<uint8> res{0};

      if (addr.Known() && code.known[addr.Value()]) {
	res = Exp<uint8>(code.code[addr.Value()]);
      } else if (addr.Known()) {
	// Could fall through to the next case, but it's nice to avoid
	// generating variables we don't need.
	// PERF: Also, here we know the address, so we can also avoid
	// dynamically dispatcing to the read handler (and e.g., read
	// directly from RAM). This also helps the C compiler optimize
	// this block since external read handlers could do anything
	// (and some do).
	const string val_sym = GenSym("v");
	fprintf(f, 
		I "const uint8 %s = fceu->ARead[0x%04x](fc, 0x%04x);\n",
		val_sym.c_str(), addr.Value(), addr.Value());
	res = Exp<uint8>(val_sym);
      } else {
	// Need to serialize addr and val.
	const string addr_sym = GenSym("a");
	const string val_sym = GenSym("v");
	fprintf(f,
		I "const uint16 %s = %s;\n", addr_sym.c_str(),
		addr.String().c_str());
	fprintf(f, 
		I "const uint8 %s = fceu->ARead[%s](fc, %s);\n",
		val_sym.c_str(), addr_sym.c_str(), addr_sym.c_str());
	res = Exp<uint8>(val_sym);
      }

      // res is a value now. Write to data bus.
      fprintf(f, I "X->DB = %s;\n", res.String().c_str());
      return res;
    };
  
    auto WriteMem = [f](const Exp<uint16> &addr_exp,
			const Exp<uint8> &val_exp) {
      // PERF! Same deal; when the address is known, avoid indirection.
      fprintf(f, I "fceu->BWrite[%s](fc, %s, %s);\n",
	      addr_exp.String().c_str(), addr_exp.String().c_str(),
	      val_exp.String().c_str());
    };

    auto X_ZN = [f](const string &reg) {
      // From disassembly, GCC can do the right thing for immediates
      // (that were previously set to the register) here, getting a
      // constant value from ZNTable and avoiding setting/clearing bits.
      fprintf(f, I "X->reg_P &= ~(Z_FLAG | N_FLAG);\n"
	      I "X->reg_P |= ZNTable[%s];\n", reg.c_str());
    };

    auto X_ZNT = [f](const string &reg) {
      fprintf(f, I "X->reg_P |= ZNTable[%s];\n", reg.c_str());
    };
    
    auto LD_IM = [&code, &pc_addr, f, &ReadMem](
	std::function<void(Exp<uint8>)> op) {
      Exp<uint8> x = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I "X->reg_PC = 0x%04x;\n", pc_addr);
      op(x);
    };

    auto GetAB = [this, &code, f, &pc_addr, &ReadMem]() {
      const uint16 src_addr = pc_addr;
      Exp<uint8> value_low = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I "X->reg_PC = 0x%04x;\n", pc_addr);
      Exp<uint8> value_high = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I "X->reg_PC = 0x%04x;\n", pc_addr);

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

      Exp<uint8> unused = ReadMem(
	  Exp<uint16>(StringPrintf("(%s ^ 0x100)", sym.c_str())));
      fprintf(f, I "  (void) %s;  // Unused GetABIRD\n",
	      unused.String().c_str());
      ADDCYC(f, 1);
      fprintf(f, I "}\n");
      return Exp<uint16>(sym);
    };
    
    // As far as I can tell this is exactly identical.
    auto GetZP = [&ReadMem, f, &pc_addr]() {
      Exp<uint8> x = ReadMem(Exp<uint16>(pc_addr));
      pc_addr++; pc_addr &= 0xFFFF;
      fprintf(f, I "X->reg_PC = 0x%04x; // GetZP\n", pc_addr);
      return x;
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

    auto LD_ABX = [&LD_ABI](std::function<void(Exp<uint8>)> op) {
      LD_ABI(Exp<uint8>("X->reg_X"), op);
    };
    auto LD_ABY = [&LD_ABI](std::function<void(Exp<uint8>)> op) {
      LD_ABI(Exp<uint8>("X->reg_Y"), op);
    };
    
    auto LDA = [&code, f, &X_ZN](Exp<uint8> val) {
      // PERF: Could get constant for X_ZN in case that value is known,
      // but gcc seems able to do this optimization just fine.
      fprintf(f, I "X->reg_A = %s;\n", val.String().c_str());
      X_ZN("X->reg_A");
    };

    auto LDX = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I "X->reg_X = %s;\n", val.String().c_str());
      X_ZN("X->reg_X");
    };

    auto LDY = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I "X->reg_Y = %s;\n", val.String().c_str());
      X_ZN("X->reg_Y");
    };

    auto ORA = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I "X->reg_A |= %s;\n", val.String().c_str());
      X_ZN("X->reg_A");
    };

    auto AND = [&code, f, &X_ZN](Exp<uint8> val) {
      fprintf(f, I "X->reg_A &= %s;\n", val.String().c_str());
      X_ZN("X->reg_A");
    };

    auto ADC = [this, &code, f, &X_ZNT](Exp<uint8> val) {
      const string sym = GenSym("adc");
      fprintf(f, I "const uint32 %s = "
	      "(uint32)(%s) + X->reg_A + (X->reg_P & 1);\n",
	      sym.c_str(), val.String().c_str());
      fprintf(f,
	      I "X->reg_P =\n"
	      I "  (X->reg_P & ~(Z_FLAG | C_FLAG | N_FLAG | V_FLAG)) |\n"
	      I "  (((((X->reg_A ^ %s) & 0x80) ^ 0x80) &\n"
              I "    ((X->reg_A ^ %s) & 0x80)) >> 1) |\n"
	      I "  ((%s >> 8) & C_FLAG);\n",
	      val.String().c_str(),
	      sym.c_str(),
	      sym.c_str());
      fprintf(f, I "X->reg_A = %s;\n", sym.c_str());
      X_ZNT("X->reg_A");
    };

    auto SBC = [this, &code, f, &X_ZNT](Exp<uint8> val) {
      const string sym = GenSym("sbc");
      fprintf(f, I "const uint32 %s = "
	      "(uint32)X->reg_A - %s - ((X->reg_P & 1) ^ 1);\n",
	      sym.c_str(), val.String().c_str());
      fprintf(f,
	      I "X->reg_P =\n"
	      I "  (X->reg_P & ~(Z_FLAG | C_FLAG | N_FLAG | V_FLAG)) |\n"
	      I "  (((X->reg_A ^ %s) & (X->reg_A ^ %s) & 0x80) >> 1) |\n"
	      I "  (((%s >> 8) & C_FLAG) ^ C_FLAG);\n",
	      sym.c_str(), val.String().c_str(), 
	      sym.c_str());
      fprintf(f, I "X->reg_A = %s;\n", sym.c_str());
      X_ZNT("X->reg_A");
    };
    
    auto ST_AB = [&code, f, &WriteMem, &GetAB](Exp<uint8> exp) {
      Exp<uint16> aa = GetAB();
      WriteMem(aa, exp);
    };

    auto ST_ZP = [&code, f, &WriteMem, &GetZP](Exp<uint8> exp) {
      Exp<uint8> aa = GetZP();
      WriteMem(Extend8to16(aa), exp);
    };
    
    auto PUSH = [&code, f](Exp<uint8> v) {
      fprintf(f, I "fceu->RAM[0x100 + X->reg_S] = %s;\n", v.String().c_str());
      fprintf(f, I "X->reg_S--;\n");
    };

    auto POP = [this, &code, f](std::function<void(Exp<uint8>)> op) {
      fprintf(f, I "X->reg_S++;\n");
      const string sym = GenSym("v");
      fprintf(f,
	      I "const uint8 %s = (X->DB = fceu->RAM[0x100 + X->reg_S]);\n",
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
	  fprintf(f, I "X->reg_PC += %s;\n", disp_sym.c_str());
	  fprintf(f, I "if ((0x%04x ^ X->reg_PC) & 0x100) {\n", tmp);
	  // Penalty for crossing page boundary.
	  ADDCYC(f, 1);
	  fprintf(f, I "}\n");
	  // Since the program counter is unknown, we have to
	  // return to the driver.
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
      fprintf(f, I "  X->reg_PC = 0x%04x;\n", pc_addr);
      fprintf(f, I "} // JR\n");
    };

    auto CMPL = [this, &code, f, &X_ZN](Exp<uint8> a1, Exp<uint8> a2) {
      const string sym = GenSym("c");
      fprintf(f, I "uint32 %s = (uint32)(%s) - (uint32)(%s);\n",
	      sym.c_str(),
	      a1.String().c_str(), a2.String().c_str());
      X_ZN(StringPrintf("%s & 0xFF", sym.c_str()));
      fprintf(f, I "X->reg_P &= ~C_FLAG;\n");
      fprintf(f, I "X->reg_P |= ((%s >> 8) & C_FLAG) ^ C_FLAG;\n",
	      sym.c_str());
    };

    auto CMP = [CMPL](Exp<uint8> a2) { CMPL(Exp<uint8>("X->reg_A"), a2); };
    auto CPX = [CMPL](Exp<uint8> a2) { CMPL(Exp<uint8>("X->reg_X"), a2); };
    auto CPY = [CMPL](Exp<uint8> a2) { CMPL(Exp<uint8>("X->reg_Y"), a2); };
    
    switch (b1) {
#if 0
    case 0x00: /* BRK */
      reg_PC++;
      PUSH(reg_PC >> 8);
      PUSH(reg_PC);
      PUSH(reg_P | U_FLAG | B_FLAG);
      reg_P |= I_FLAG;
      reg_PI |= I_FLAG;
      reg_PC = RdMem(0xFFFE);
      reg_PC |= RdMem(0xFFFF) << 8;
      break;

    case 0x40: /* RTI */
      reg_P = POP();
      /* reg_PI=reg_P; This is probably incorrect, so it's commented out. */
      reg_PI = reg_P;
      reg_PC = POP();
      reg_PC |= POP() << 8;
      break;
#endif
      
    case 0x60: /* RTS */
      POP([&](Exp<uint8> pc_low) {
	// n.b. this used to set an intermediate value for the PC
	// before doing the second pop, and the pop could conceivably
	// have triggered some read handler that inspected the PC.
	// But not really, since pops are supposed to be directly
	// from RAM.
	POP([&](Exp<uint8> pc_high) {
	  fprintf(f, "X->reg_PC = 1 + (((uint16)(%s) << 8) | %s);\n",
		  pc_high.String().c_str(), pc_low.String().c_str());
	});
      });
      // Address from RAM.
      return 0xFFFFFFFF;

    case 0x48: /* PHA */
      PUSH(Exp<uint8>("X->reg_A"));
      return pc_addr;
    case 0x08: /* PHP */
      PUSH(Exp<uint8>("X->reg_P | U_FLAG | B_FLAG"));
      return pc_addr;

    case 0x68: /* PLA */
      POP([&](Exp<uint8> v) {
	fprintf(f, I "X->reg_A = %s;\n", v.String().c_str());
	X_ZN("X->reg_A");
      });
      return pc_addr;

    case 0x28: /* PLP */
      POP([&](Exp<uint8> v) {
	fprintf(f, I "X->reg_P = %s;\n", v.String().c_str());
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
      fprintf(f, I "X->reg_PC = ((uint16)(%s) << 8) | %s;\n",
	      pc_high.String().c_str(),
	      pc_low.String().c_str());
      pc_addr = 0xFFFFFFFF;

      // PERF We often know the actual destination address, and
      // could jump to it directly...
      return pc_addr;
    } 
#if 0
    case 0x6C: {
      /* JMP INDIRECT */
      uint32 tmp;
      GetAB(tmp);
      reg_PC = RdMem(tmp);
      reg_PC |= RdMem(((tmp + 1) & 0x00FF) | (tmp & 0xFF00)) << 8;
      break;
    }
#endif
    case 0x20: /* JSR */
      // n.b. original code was akin to LD_IM.
      LD_IM([&](Exp<uint8> pc_low) {
	PUSH(Exp<uint8>(255 & (pc_addr >> 8)));
	PUSH(Exp<uint8>(255 & pc_addr));

	// This load increments PC, but it gets immediately overwritten
	// unconditionally here, so should be easy to optimize out.
	LD_IM([&](Exp<uint8> pc_high) {
	  fprintf(f, I "X->reg_PC = ((uint16)(%s) << 8) | %s;\n",
		  pc_high.String().c_str(), pc_low.String().c_str());
	});
      });
      // We actually know the subroutine being called, but
      // inlining here can easily lead to madness.
      return 0xFFFFFFFF;

    case 0xAA: /* TAX */
      fprintf(f, I "X->reg_X = X->reg_A;\n");
      X_ZN("X->reg_A");
      return pc_addr;

    case 0x8A: /* TXA */
      fprintf(f, I "X->reg_A = X->reg_X;\n");
      X_ZN("X->reg_A");
      return pc_addr;

    case 0xA8: /* TAY */
      fprintf(f, I "X->reg_Y = X->reg_A;\n");
      X_ZN("X->reg_A");
      return pc_addr;
    case 0x98: /* TYA */
      fprintf(f, I "X->reg_A = X->reg_Y;\n");
      X_ZN("X->reg_A");
      return pc_addr;

    case 0xBA: /* TSX */
      fprintf(f, I "X->reg_X = X->reg_S;\n");
      X_ZN("X->reg_X");
      return pc_addr;
    case 0x9A: /* TXS */
      fprintf(f, I "X->reg_S = X->reg_X;\n");
      // n.b. no X_ZN in original code. Looks like
      // 6502 docs corroborate. -tom7
      return pc_addr;


    case 0xCA: /* DEX */
      fprintf(f, I "X->reg_X--;\n");
      X_ZN("X->reg_X");
      return pc_addr;
    case 0x88: /* DEY */
      fprintf(f, I "X->reg_Y--;\n");
      X_ZN("X->reg_Y");
      return pc_addr;

    case 0xE8: /* INX */
      fprintf(f, I "X->reg_X++;\n");
      X_ZN("X->reg_X");
      return pc_addr;
    case 0xC8: /* INY */
      fprintf(f, I "X->reg_Y++;\n");
      X_ZN("X->reg_Y");
      return pc_addr;

    case 0x18:
      fprintf(f, I "X->reg_P &= ~C_FLAG;\n");
      return pc_addr;
    case 0xD8:
      fprintf(f, I "X->reg_P &= ~D_FLAG;\n");
      return pc_addr;
    case 0x58:
      fprintf(f, I "X->reg_P &= ~I_FLAG;\n");
      return pc_addr;
    case 0xB8:
      fprintf(f, I "X->reg_P &= ~V_FLAG;\n");
      return pc_addr;

    case 0x38:
      fprintf(f, I "X->reg_P |= C_FLAG;\n");
      return pc_addr;
    case 0xF8:
      fprintf(f, I "X->reg_P |= D_FLAG;\n");
      return pc_addr;
    case 0x78:
      fprintf(f, I "X->reg_P |= I_FLAG;\n");
      return pc_addr;

    case 0xEA:
      // Nop.
      return pc_addr;

#if 0

    case 0x0A: RMW_A(ASL);
    case 0x06: RMW_ZP(ASL);
    case 0x16: RMW_ZPX(ASL);
    case 0x0E: RMW_AB(ASL);
    case 0x1E: RMW_ABX(ASL);

    case 0xC6: RMW_ZP(DEC);
    case 0xD6: RMW_ZPX(DEC);
    case 0xCE: RMW_AB(DEC);
    case 0xDE: RMW_ABX(DEC);

    case 0xE6: RMW_ZP(INC);
    case 0xF6: RMW_ZPX(INC);
    case 0xEE: RMW_AB(INC);
    case 0xFE: RMW_ABX(INC);

    case 0x4A: RMW_A(LSR);
    case 0x46: RMW_ZP(LSR);
    case 0x56: RMW_ZPX(LSR);
    case 0x4E: RMW_AB(LSR);
    case 0x5E: RMW_ABX(LSR);

    case 0x2A: RMW_A(ROL);
    case 0x26: RMW_ZP(ROL);
    case 0x36: RMW_ZPX(ROL);
    case 0x2E: RMW_AB(ROL);
    case 0x3E: RMW_ABX(ROL);

    case 0x6A: RMW_A(ROR);
    case 0x66: RMW_ZP(ROR);
    case 0x76: RMW_ZPX(ROR);
    case 0x6E: RMW_AB(ROR);
    case 0x7E: RMW_ABX(ROR);

#endif
    case 0x69:
      LD_IM(ADC);
      return pc_addr;
#if 0
    case 0x65: LD_ZP(ADC);
    case 0x75: LD_ZPX(ADC);
#endif
    case 0x6D:
      LD_AB(ADC);
      return pc_addr;
    case 0x7D:
      LD_ABX(ADC);
      return pc_addr;
    case 0x79:
      LD_ABY(ADC);
      return pc_addr;
#if 0
    case 0x61: LD_IX(ADC);
    case 0x71: LD_IY(ADC);
#endif
    case 0x29:
      LD_IM(AND);
      return pc_addr;
#if 0
    case 0x25: LD_ZP(AND);
    case 0x35: LD_ZPX(AND);
#endif
    case 0x2D:
      LD_AB(AND);
      return pc_addr;

    case 0x3D:
      LD_ABX(AND);
      return pc_addr;
    case 0x39:
      LD_ABY(AND);
      return pc_addr;
#if 0
    case 0x21: LD_IX(AND);
    case 0x31: LD_IY(AND);

    case 0x24: LD_ZP(BIT);
    case 0x2C: LD_AB(BIT);

#endif
    case 0xC9:
      LD_IM(CMP);
      return pc_addr;

#if 0
    case 0xC5:
      LD_ZP(CMP);
      return pc_addr;
    case 0xD5: LD_ZPX(CMP);
#endif
    case 0xCD:
      LD_AB(CMP);
      return pc_addr;
    case 0xDD:
      LD_ABX(CMP);
      return pc_addr;
    case 0xD9:
      LD_ABY(CMP);
      return pc_addr;
#if 0
    case 0xC1: LD_IX(CMP);
    case 0xD1: LD_IY(CMP);
#endif
    case 0xE0:
      LD_IM(CPX);
      return pc_addr;
#if 0
    case 0xE4: LD_ZP(CPX);
#endif
    case 0xEC:
      LD_AB(CPX);
      return pc_addr;
    case 0xC0:
      LD_IM(CPY);
      return pc_addr;
#if 0
    case 0xC4: LD_ZP(CPY);
#endif
    case 0xCC:
      LD_AB(CPY);
      return pc_addr;

#if 0
    case 0x49: LD_IM(EOR);
    case 0x45: LD_ZP(EOR);
    case 0x55: LD_ZPX(EOR);
    case 0x4D: LD_AB(EOR);
    case 0x5D: LD_ABX(EOR);
    case 0x59: LD_ABY(EOR);
    case 0x41: LD_IX(EOR);
    case 0x51: LD_IY(EOR);
#endif

    case 0xA9:
      LD_IM(LDA);
      return pc_addr;

#if 0
    case 0xA5: LD_ZP(LDA);
    case 0xB5: LD_ZPX(LDA);
#endif
    case 0xAD:
      LD_AB(LDA);
      return pc_addr;
    case 0xBD:
      LD_ABX(LDA);
      return pc_addr;
    case 0xB9:
      LD_ABY(LDA);
      return pc_addr;
#if 0
    case 0xA1: LD_IX(LDA);
    case 0xB1: LD_IY(LDA);
#endif

    case 0xA2:
      LD_IM(LDX);
      return pc_addr;
#if 0
    case 0xA6: LD_ZP(LDX);
    case 0xB6: LD_ZPY(LDX);
#endif
    case 0xAE:
      LD_AB(LDX);
      return pc_addr;

    case 0xBE:
      LD_ABY(LDX);
      return pc_addr;

    case 0xA0:
      LD_IM(LDY);
      return pc_addr;
#if 0
    case 0xA4: LD_ZP(LDY);
    case 0xB4: LD_ZPX(LDY);
#endif
    case 0xAC:
      LD_AB(LDY);
      return pc_addr;

    case 0xBC:
      LD_ABX(LDY);
      return pc_addr;

    case 0x09:
      LD_IM(ORA);
      return pc_addr;
#if 0
    case 0x05: LD_ZP(ORA);
    case 0x15: LD_ZPX(ORA);
#endif
    case 0x0D:
      LD_AB(ORA);
      return pc_addr;
    case 0x1D:
      LD_ABX(ORA);
      return pc_addr;
    case 0x19:
      LD_ABY(ORA);
      return pc_addr;

#if 0
    case 0x01: LD_IX(ORA);
    case 0x11: LD_IY(ORA);
#endif
    case 0xEB: /* (undocumented) */
    case 0xE9:
      LD_IM(SBC);
      return pc_addr;
      
#if 0
    case 0xE5: LD_ZP(SBC);
    case 0xF5: LD_ZPX(SBC);
#endif

    case 0xED:
      LD_AB(SBC);
      return pc_addr;
    case 0xFD:
      LD_ABX(SBC);
      return pc_addr;
    case 0xF9:
      LD_ABY(SBC);
      return pc_addr;

#if 0
    case 0xE1: LD_IX(SBC);
    case 0xF1: LD_IY(SBC);
#endif

    case 0x85:
      ST_ZP(Exp<uint8>("X->reg_A"));
      return pc_addr;
#if 0
    case 0x95: ST_ZPX(reg_A);
#endif
    case 0x8D:
      ST_AB(Exp<uint8>("X->reg_A"));
      return pc_addr;
#if 0
    case 0x9D: ST_ABX(reg_A);
    case 0x99: ST_ABY(reg_A);
    case 0x81: ST_IX(reg_A);
    case 0x91: ST_IY(reg_A);

#endif
    case 0x86:
      ST_ZP(Exp<uint8>("X->reg_X"));
      return pc_addr;
#if 0
    case 0x96: ST_ZPY(reg_X);
#endif
    case 0x8E:
      ST_AB(Exp<uint8>("X->reg_X"));
      return pc_addr;

    case 0x84:
      ST_ZP(Exp<uint8>("X->reg_Y"));
      return pc_addr;
      
#if 0
    case 0x94: ST_ZPX(reg_Y);
#endif
    case 0x8C:
      ST_AB(Exp<uint8>("X->reg_Y"));
      return pc_addr;

      /* BCC */
    case 0x90:
      JR(Exp<uint8>("!(X->reg_P & C_FLAG)"));
      return pc_addr;

      /* BCS */
    case 0xB0:
      JR(Exp<uint8>("X->reg_P & C_FLAG"));
      return pc_addr;

      /* BEQ */
    case 0xF0:
      JR(Exp<uint8>("X->reg_P & Z_FLAG"));
      return pc_addr;

      /* BNE */
    case 0xD0:
      JR(Exp<uint8>("!(X->reg_P & Z_FLAG)"));
      return pc_addr;

      /* BMI */
    case 0x30:
      JR(Exp<uint8>("(X->reg_P & N_FLAG)"));
      return pc_addr;


      /* BPL */
    case 0x10:
      JR(Exp<uint8>("!(X->reg_P & N_FLAG)"));
      return pc_addr;

      /* BVC */
    case 0x50:
      JR(Exp<uint8>("!(X->reg_P & V_FLAG)"));
      return pc_addr;

      /* BVS */
    case 0x70:
      JR(Exp<uint8>("(X->reg_P & V_FLAG)"));
      return pc_addr;

#if 0
      // default: printf("Bad %02x at $%04x\n",b1,X.PC);break;
      /* Here comes the undocumented instructions block.  Note that this
	 implementation may be "wrong".  If so, please tell me.
      */

      /* AAC */
    case 0x2B:
    case 0x0B:
      LD_IM(AND; reg_P &= ~C_FLAG; reg_P |= reg_A >> 7);

      /* AAX */
#endif
    case 0x87:
      ST_ZP(Exp<uint8>("X->reg_A & X->reg_X"));
      return pc_addr;
      
#if 0
    case 0x97: ST_ZPY(reg_A & reg_X);
#endif
    case 0x8F:
      ST_AB(Exp<uint8>("(X->reg_A & X->reg_X)"));
      return pc_addr;
#if 0
    case 0x83:
      ST_IX(reg_A & reg_X);

      /* ARR - ARGH, MATEY! */
    case 0x6B: {
      uint8 arrtmp;
      LD_IM(AND; reg_P &= ~V_FLAG; reg_P |= (reg_A ^ (reg_A >> 1)) & 0x40;
	    arrtmp = reg_A >> 7; reg_A >>= 1; reg_A |= (reg_P & C_FLAG) << 7;
	    reg_P &= ~C_FLAG; reg_P |= arrtmp; X_ZN(reg_A));
    }
      /* ASR */
    case 0x4B:
      LD_IM(AND; LSRA);

      /* ATX(OAL) Is this(OR with $EE) correct? Blargg did some test
	 and found the constant to be OR with is $FF for NES */
    case 0xAB:
      LD_IM(reg_A |= 0xFF; AND; reg_X = reg_A);

      /* AXS */
    case 0xCB:
      LD_IM(AXS);

      /* DCP */
    case 0xC7: RMW_ZP(DEC; CMP);
    case 0xD7: RMW_ZPX(DEC; CMP);
    case 0xCF: RMW_AB(DEC; CMP);
    case 0xDF: RMW_ABX(DEC; CMP);
    case 0xDB: RMW_ABY(DEC; CMP);
    case 0xC3: RMW_IX(DEC; CMP);
    case 0xD3: RMW_IY(DEC; CMP);

      /* ISB */
    case 0xE7: RMW_ZP(INC; SBC);
    case 0xF7: RMW_ZPX(INC; SBC);
    case 0xEF: RMW_AB(INC; SBC);
    case 0xFF: RMW_ABX(INC; SBC);
    case 0xFB: RMW_ABY(INC; SBC);
    case 0xE3: RMW_IX(INC; SBC);
    case 0xF3: RMW_IY(INC; SBC);

#endif

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
      fprintf(f, I "X->reg_PC = 0x%04x;\n", pc_addr);
      return pc_addr;

#if 0
      /* KIL */

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
      ADDCYC(0xFF);
      jammed = 1;
      reg_PC--;
      break;

      /* LAR */
    case 0xBB:
      RMW_ABY(reg_S &= x; reg_A = reg_X = reg_S; X_ZN(reg_X));

      /* LAX */
    case 0xA7: LD_ZP(LDA; LDX);
    case 0xB7: LD_ZPY(LDA; LDX);
    case 0xAF: LD_AB(LDA; LDX);
    case 0xBF: LD_ABY(LDA; LDX);
    case 0xA3: LD_IX(LDA; LDX);
    case 0xB3: LD_IY(LDA; LDX);
#endif

      /* NOP */
    case 0x1A:
    case 0x3A:
    case 0x5A:
    case 0x7A:
    case 0xDA:
    case 0xFA:
      fprintf(f, I "// NOP %02x\n", b1);
      return pc_addr;

#if 0
      /* RLA */
    case 0x27: RMW_ZP(ROL; AND);
    case 0x37: RMW_ZPX(ROL; AND);
    case 0x2F: RMW_AB(ROL; AND);
    case 0x3F: RMW_ABX(ROL; AND);
    case 0x3B: RMW_ABY(ROL; AND);
    case 0x23: RMW_IX(ROL; AND);
    case 0x33: RMW_IY(ROL; AND);

      /* RRA */
    case 0x67: RMW_ZP(ROR; ADC);
    case 0x77: RMW_ZPX(ROR; ADC);
    case 0x6F: RMW_AB(ROR; ADC);
    case 0x7F: RMW_ABX(ROR; ADC);
    case 0x7B: RMW_ABY(ROR; ADC);
    case 0x63: RMW_IX(ROR; ADC);
    case 0x73: RMW_IY(ROR; ADC);

      /* SLO */
    case 0x07: RMW_ZP(ASL; ORA);
    case 0x17: RMW_ZPX(ASL; ORA);
    case 0x0F: RMW_AB(ASL; ORA);
    case 0x1F: RMW_ABX(ASL; ORA);
    case 0x1B: RMW_ABY(ASL; ORA);
    case 0x03: RMW_IX(ASL; ORA);
    case 0x13: RMW_IY(ASL; ORA);

      /* SRE */
    case 0x47: RMW_ZP(LSR; EOR);
    case 0x57: RMW_ZPX(LSR; EOR);
    case 0x4F: RMW_AB(LSR; EOR);
    case 0x5F: RMW_ABX(LSR; EOR);
    case 0x5B: RMW_ABY(LSR; EOR);
    case 0x43: RMW_IX(LSR; EOR);
    case 0x53: RMW_IY(LSR; EOR);

      /* AXA - SHA */
    case 0x93: ST_IY(reg_A & reg_X & (((AA - reg_Y) >> 8) + 1));
    case 0x9F: ST_ABY(reg_A & reg_X & (((AA - reg_Y) >> 8) + 1));

      /* SYA */
    case 0x9C:
      ST_ABX(reg_Y & (((AA - reg_X) >> 8) + 1));

      /* SXA */
    case 0x9E:
      ST_ABY(reg_X & (((AA - reg_Y) >> 8) + 1));

      /* XAS */
    case 0x9B:
      reg_S = reg_A & reg_X;
      ST_ABY(reg_S & (((AA - reg_Y) >> 8) + 1));

#endif
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

#if 0
      /* XAA - BIG QUESTION MARK HERE */
    case 0x8B:
      reg_A |= 0xEE;
      reg_A &= reg_X;
      LD_IM(AND);
#endif

    default:;
    }
    LOG(FATAL) << "Unimplemented inst " << StringPrintf("0x%02x", b1) << "\n"
    "(or forgot to return pc_addr, fell through or did break).";
    return 0xFFFFFFFF;
  }

  void ADDCYC(FILE *f, int cycles) {
    fprintf(f, I "X->tcount += %d; X->count -= %d; X->timestamp += %d;\n",
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

    // XXX
    fprintf(f, "  X->entered_aot[0x%04x]++;\n", entry_addr);

    uint32 pc_addr = entry_addr;
    for (;;) {
      // We don't want to try reading outside the mapped region, of
      // course.
      // Note that we don't really have any effective protection
      // against an infinite codegen loop if 0000-FFFF is all mapped.
      // XXX isn't this just like code.Known()?
      if (pc_addr < entry_addr || pc_addr >= addr_past_end) {
	fprintf(f, I "// PC $%04x exits code region.\n"
		I "%s_any(fc);\n"
		I "return;\n", pc_addr, symbol.c_str());
	break;
      }

      const uint8 b1 = code.Get(pc_addr);
      if (!CanGenInstruction(b1)) {
	fprintf(f, I "// Unimplemented instruction $%02x\n"
		// PERF
		I "X->unimpl_inst[0x%02x]++;\n"
		I "%s_any(fc);\n"
		I "return;\n", b1, b1, symbol.c_str());
	break;
      } else {
	// PERF: Would be nice to avoid testing this over and over,
	// like by testing the number of cycles we need for the
	// straight-line code at the top of the entry point, and just
	// running the slow interpreter if we don't have enough cycles
	// left.
	fprintf(f, I "if (X->count <= 0) return;\n");

	fprintf(f, I "// %04x = %02x\n", pc_addr, b1);

	// PERF: Similarly, good to avoid testing this over and over.
	// Mappers can trigger interrupt, as can sound.
	fprintf(f, I "if (X->IRQlow) { %s_any(fc); return; }\n",
		symbol.c_str());

	fprintf(f, I "X->reg_PI = X->reg_P;\n");

	const int cycles = CycTable[b1];
	// ADDCYC() macro.
	ADDCYC(f, cycles);

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

	pc_addr = GenInstruction(code, b1, pc_addr, f);
	if (pc_addr == 0xFFFFFFFF) {
	  fprintf(f, I "// Branch was unconditional.\n"
		  I "return;\n");
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
  
static void GenerateCode(const CodeConfig &config,
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

  // PERF can even do these in parallel.
  static constexpr int ENTRYPOINTS_PER_FILE = 1024;
  for (int i = addr_start; i < addr_past_end; i += ENTRYPOINTS_PER_FILE) {
    AOT aot;
    string filename = StringPrintf("%s_%d.cc", symbol.c_str(), i);
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
	    ReadFileToString("aot-prelude.inc").c_str());

    fprintf(f, "\n\n");

    // First, a function to call when we don't have a compiled
    // version.
    fprintf(f, "static void %s_any(FC *fc) { fc->X->RunLoop(); }\n\n",
	    symbol.c_str());

    // Then, a function for each address entry point.
    for (uint32 j = i; j < addr_past_end && j < i + ENTRYPOINTS_PER_FILE; j++) {
      aot.GenerateEntry(config, code, j, addr_past_end, symbol, f);
    }

    fprintf(stderr, "Wrote %s.\n", filename.c_str());
    fclose(f);
  }
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

int main(int argc, char **argv) {
  string romdir = "roms/";

  std::unique_ptr<Emulator> emu(Emulator::Create("mario.nes"));

  Timer compile_timer;

  FC *fc = emu->GetFC();

  // Grab a specific block of RAM. I know this is where the code
  // resides in mario.nes, that it never gets remapped (mapper 0
  // cannot remap), and that it is not writable (only RAM and
  // 6000-7ffff are writable).
  //
  // Note that even if an area isn't writable, it's possible that it's
  // unmapped, in which case the read returns the value of the last
  // read (this is usually predictable statically, but definitely
  // complicates things). For mapper 0, all of 0x8000-0x7fff is mapped
  // to cart rom (CartBR).
  Code code;
  for (uint32 addr = 0x8000; addr < 0x10000; addr++) {
    code.known[addr] = true;
    code.code[addr] = fc->fceu->ARead[addr](fc, addr);
  }

  CodeConfig config;
  config.is_pal = !!fc->fceu->PAL;
  config.has_map_irq_hook = fc->X->MapIRQHook != nullptr;

  GenerateCode(config, code, 0x8000, 0x10000, "mario", "mario.nes");
  GenerateDispatcher(config, 0x8000, 0x10000, "mario", "mario.cc");
  
  double compile_seconds = compile_timer.GetSeconds();

  fprintf(stderr, "Finished.\n"
          "Compile time: %.4fs\n",
          compile_seconds);

  return 0;
}
