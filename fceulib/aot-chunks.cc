
// This version was a dead end where I tried generating chunks of code
// with switch statements. Probably can be deleted.    9 May 2016

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

// 2^10 addresses per chunk.
static constexpr int CHUNK_SIZE = 10;

// One of these per compiled file. It does the per-Run setup and
// coordinates control transfer between the chunks.
static void GenerateOuter(const CodeConfig &config,
			  uint32 addr_start,
			  uint32 addr_past_end,
			  const string &symbol,
			  FILE *f) {
  fprintf(f, "// Outer.\n"
	  "void %s_Run(FC *fc, int32 cycles) {\n",
	  symbol.c_str());

  fprintf(f,
	  "  X6502 *X = fc->X;\n"
	  // "  const FCEU *fceu = fc->fceu;\n"  // const?
	  );

  if (config.is_pal) fprintf(f, "  cycles *= 15;  // is pal\n");
  else fprintf(f, "  cycles *= 16;  // is ntsc\n");

  fprintf(f, "  X->count += cycles;\n");

  auto LT16 = [](const string &lhs, uint32 rhs) -> string {
    if (rhs == 0) return "false /* < 0 */";
    else if (rhs > 0xFFFF) return StringPrintf("true /* < %x */", rhs);
    else return StringPrintf("%s < 0x%04x", lhs.c_str(), rhs);
  };
  
  // Now, which chunk to call?
  // PERF
  fprintf(f, "  const uint16 pc = X->reg_PC;\n");
  fprintf(f, "  if (%s) { X->RunLoop(); return; }\n",
	  LT16("pc", addr_start).c_str());
  for (uint32 i = addr_start; i < addr_past_end; i += (1 << CHUNK_SIZE)) {
    fprintf(f, "  else if (%s) { %s_chunk_%04x(fc); }\n",
	    LT16("pc", i + (1 << CHUNK_SIZE)).c_str(),
	    symbol.c_str(), i);
  }
  fprintf(f, "  else { X->RunLoop(); return; }\n");
  
  fprintf(f, "}  // Outer.\n\n\n");
}

static bool CanGenInstruction(uint8 b1) {
  switch (b1) {
  case 0xAA: return true;

  default: return false;
  }
}

#define I "      "

static void GenInstruction(uint8 b1, FILE *f) {
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

  case 0x60: /* RTS */
    reg_PC = POP();
    reg_PC |= POP() << 8;
    reg_PC++;
    break;

  case 0x48: /* PHA */ PUSH(reg_A); break;
  case 0x08: /* PHP */ PUSH(reg_P | U_FLAG | B_FLAG); break;
  case 0x68: /* PLA */
    reg_A = POP();
    X_ZN(reg_A);
    break;
  case 0x28: /* PLP */ reg_P = POP(); break;
  case 0x4C: {
    /* JMP ABSOLUTE */
    uint16 ptmp = reg_PC;
    unsigned int npc;

    npc = RdMem(ptmp);
    ptmp++;
    npc |= RdMem(ptmp) << 8;
    reg_PC = npc;
  } break;
  case 0x6C: {
    /* JMP INDIRECT */
    uint32 tmp;
    GetAB(tmp);
    reg_PC = RdMem(tmp);
    reg_PC |= RdMem(((tmp + 1) & 0x00FF) | (tmp & 0xFF00)) << 8;
    break;
  }
  case 0x20: /* JSR */
    {
      uint8 npc;
      npc = RdMem(reg_PC);
      reg_PC++;
      PUSH(reg_PC >> 8);
      PUSH(reg_PC);
      reg_PC = RdMem(reg_PC) << 8;
      reg_PC |= npc;
      break;
    }
#endif
    
  case 0xAA: /* TAX */
    fprintf(f,
	    I "X->reg_X = x->reg_A;\n"
	    I "X->reg_P &= ~(Z_FLAG | N_FLAG);\n"
	    I "X->reg_P |= ZNTable[X->reg_A];\n");
    break;

#if 0
  case 0x8A: /* TXA */
    reg_A = reg_X;
    X_ZN(reg_A);
    break;

  case 0xA8: /* TAY */
    reg_Y = reg_A;
    X_ZN(reg_A);
    break;
  case 0x98: /* TYA */
    reg_A = reg_Y;
    X_ZN(reg_A);
    break;

  case 0xBA: /* TSX */
    reg_X = reg_S;
    X_ZN(reg_X);
    break;
  case 0x9A: /* TXS */
    reg_S = reg_X;
    break;

  case 0xCA: /* DEX */
    reg_X--;
    X_ZN(reg_X);
    break;
  case 0x88: /* DEY */
    reg_Y--;
    X_ZN(reg_Y);
    break;

  case 0xE8: /* INX */
    reg_X++;
    X_ZN(reg_X);
    break;
  case 0xC8: /* INY */
    reg_Y++;
    X_ZN(reg_Y);
    break;

  case 0x18: /* CLC */ reg_P &= ~C_FLAG; break;
  case 0xD8: /* CLD */ reg_P &= ~D_FLAG; break;
  case 0x58: /* CLI */ reg_P &= ~I_FLAG; break;
  case 0xB8: /* CLV */ reg_P &= ~V_FLAG; break;

  case 0x38: /* SEC */ reg_P |= C_FLAG; break;
  case 0xF8: /* SED */ reg_P |= D_FLAG; break;
  case 0x78: /* SEI */ reg_P |= I_FLAG; break;

  case 0xEA: /* NOP */ break;

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

  case 0x69: LD_IM(ADC);
  case 0x65: LD_ZP(ADC);
  case 0x75: LD_ZPX(ADC);
  case 0x6D: LD_AB(ADC);
  case 0x7D: LD_ABX(ADC);
  case 0x79: LD_ABY(ADC);
  case 0x61: LD_IX(ADC);
  case 0x71: LD_IY(ADC);

  case 0x29: LD_IM(AND);
  case 0x25: LD_ZP(AND);
  case 0x35: LD_ZPX(AND);
  case 0x2D: LD_AB(AND);
  case 0x3D: LD_ABX(AND);
  case 0x39: LD_ABY(AND);
  case 0x21: LD_IX(AND);
  case 0x31: LD_IY(AND);

  case 0x24: LD_ZP(BIT);
  case 0x2C: LD_AB(BIT);

  case 0xC9: LD_IM(CMP);
  case 0xC5: LD_ZP(CMP);
  case 0xD5: LD_ZPX(CMP);
  case 0xCD: LD_AB(CMP);
  case 0xDD: LD_ABX(CMP);
  case 0xD9: LD_ABY(CMP);
  case 0xC1: LD_IX(CMP);
  case 0xD1: LD_IY(CMP);

  case 0xE0: LD_IM(CPX);
  case 0xE4: LD_ZP(CPX);
  case 0xEC: LD_AB(CPX);

  case 0xC0: LD_IM(CPY);
  case 0xC4: LD_ZP(CPY);
  case 0xCC: LD_AB(CPY);

  case 0x49: LD_IM(EOR);
  case 0x45: LD_ZP(EOR);
  case 0x55: LD_ZPX(EOR);
  case 0x4D: LD_AB(EOR);
  case 0x5D: LD_ABX(EOR);
  case 0x59: LD_ABY(EOR);
  case 0x41: LD_IX(EOR);
  case 0x51: LD_IY(EOR);

  case 0xA9: LD_IM(LDA);
  case 0xA5: LD_ZP(LDA);
  case 0xB5: LD_ZPX(LDA);
  case 0xAD: LD_AB(LDA);
  case 0xBD: LD_ABX(LDA);
  case 0xB9: LD_ABY(LDA);
  case 0xA1: LD_IX(LDA);
  case 0xB1: LD_IY(LDA);

  case 0xA2: LD_IM(LDX);
  case 0xA6: LD_ZP(LDX);
  case 0xB6: LD_ZPY(LDX);
  case 0xAE: LD_AB(LDX);
  case 0xBE: LD_ABY(LDX);

  case 0xA0: LD_IM(LDY);
  case 0xA4: LD_ZP(LDY);
  case 0xB4: LD_ZPX(LDY);
  case 0xAC: LD_AB(LDY);
  case 0xBC: LD_ABX(LDY);

  case 0x09: LD_IM(ORA);
  case 0x05: LD_ZP(ORA);
  case 0x15: LD_ZPX(ORA);
  case 0x0D: LD_AB(ORA);
  case 0x1D: LD_ABX(ORA);
  case 0x19: LD_ABY(ORA);
  case 0x01: LD_IX(ORA);
  case 0x11: LD_IY(ORA);

  case 0xEB: /* (undocumented) */
  case 0xE9: LD_IM(SBC);
  case 0xE5: LD_ZP(SBC);
  case 0xF5: LD_ZPX(SBC);
  case 0xED: LD_AB(SBC);
  case 0xFD: LD_ABX(SBC);
  case 0xF9: LD_ABY(SBC);
  case 0xE1: LD_IX(SBC);
  case 0xF1: LD_IY(SBC);

  case 0x85: ST_ZP(reg_A);
  case 0x95: ST_ZPX(reg_A);
  case 0x8D: ST_AB(reg_A);
  case 0x9D: ST_ABX(reg_A);
  case 0x99: ST_ABY(reg_A);
  case 0x81: ST_IX(reg_A);
  case 0x91: ST_IY(reg_A);

  case 0x86: ST_ZP(reg_X);
  case 0x96: ST_ZPY(reg_X);
  case 0x8E: ST_AB(reg_X);

  case 0x84: ST_ZP(reg_Y);
  case 0x94: ST_ZPX(reg_Y);
  case 0x8C:
    ST_AB(reg_Y);

    /* BCC */
  case 0x90:
    JR(!(reg_P & C_FLAG));
    break;

    /* BCS */
  case 0xB0:
    JR(reg_P & C_FLAG);
    break;

    /* BEQ */
  case 0xF0:
    JR(reg_P & Z_FLAG);
    break;

    /* BNE */
  case 0xD0:
    JR(!(reg_P & Z_FLAG));
    break;

    /* BMI */
  case 0x30:
    JR(reg_P & N_FLAG);
    break;

    /* BPL */
  case 0x10:
    JR(!(reg_P & N_FLAG));
    break;

    /* BVC */
  case 0x50:
    JR(!(reg_P & V_FLAG));
    break;

    /* BVS */
  case 0x70:
    JR(reg_P & V_FLAG);
    break;

    // default: printf("Bad %02x at $%04x\n",b1,X.PC);break;
    /* Here comes the undocumented instructions block.  Note that this
       implementation may be "wrong".  If so, please tell me.
    */

    /* AAC */
  case 0x2B:
  case 0x0B:
    LD_IM(AND; reg_P &= ~C_FLAG; reg_P |= reg_A >> 7);

    /* AAX */
  case 0x87: ST_ZP(reg_A & reg_X);
  case 0x97: ST_ZPY(reg_A & reg_X);
  case 0x8F: ST_AB(reg_A & reg_X);
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

    /* DOP */
  case 0x04: reg_PC++; break;
  case 0x14: reg_PC++; break;
  case 0x34: reg_PC++; break;
  case 0x44: reg_PC++; break;
  case 0x54: reg_PC++; break;
  case 0x64: reg_PC++; break;
  case 0x74: reg_PC++; break;

  case 0x80: reg_PC++; break;
  case 0x82: reg_PC++; break;
  case 0x89: reg_PC++; break;
  case 0xC2: reg_PC++; break;
  case 0xD4: reg_PC++; break;
  case 0xE2: reg_PC++; break;
  case 0xF4: reg_PC++; break;

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

    /* NOP */
  case 0x1A:
  case 0x3A:
  case 0x5A:
  case 0x7A:
  case 0xDA:
  case 0xFA:
    break;

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

    /* TOP */
  case 0x0C:
    LD_AB(;);
  case 0x1C:
  case 0x3C:
  case 0x5C:
  case 0x7C:
  case 0xDC:
  case 0xFC:
    LD_ABX(;);

    /* XAA - BIG QUESTION MARK HERE */
  case 0x8B:
    reg_A |= 0xEE;
    reg_A &= reg_X;
    LD_IM(AND);
#endif

  default:
    LOG(FATAL) << "Unimplemented inst " << b1;
  }
}

static void GenerateChunk(const CodeConfig &config,
			  const vector<uint8> code,
			  uint32 code_base,
			  uint32 addr_start,
			  uint32 addr_past_end,
			  const string &symbol,
			  FILE *f) {
  
  fprintf(f,
	  "// From $%04x--$%04x. (Chunk size %d)\n"
	  "static void %s_chunk_%04x(FC *fc) {\n",
	  addr_start, addr_past_end, 1 << CHUNK_SIZE,
	  symbol.c_str(), addr_start);

  // Copies of FC objects, used locally.
  fprintf(f,
	  "  X6502 *X = fc->X;\n"
	  // "  const FCEU *fceu = fc->fceu;\n"  // const?
	  "  Sound *sound = fc->sound;\n"
	  );

  fprintf(f, "  switch (X->reg_PC) {\n");

  for (int32 pc_addr = addr_start; pc_addr < addr_past_end; /* in loop */) {
    fprintf(f, "    case 0x%04x: {", pc_addr);
    const int code_idx = pc_addr - addr_start;
    CHECK(code_idx >= 0 && code_idx < code.size()) << code_idx;
    const uint8 b1 = code[code_idx];

    if (CanGenInstruction(b1)) {
    
      // XXX include disassembly here
      fprintf(f, " // %2x\n", b1);

      fprintf(f, I "// XXX check interrupt!;\n");
      fprintf(f, I "X->reg_PI = X->reg_P;\n");

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

      GenInstruction(b1, f);

      fprintf(f, "    } // ends pc=0x%4x\n", pc_addr);
    } else {
      fprintf(f, " // %2x unimplemented\n", b1);
      // XXX doesn't make sense to call runloop from here..?
      fprintf(f, I "X->RunLoop(); return;\n"
	      "  }\n");
    }
  }

  fprintf(f, "    default: X->RunLoop(); return;\n"
	  "  }  // switch(reg_PC)\n");
  
  
  fprintf(f, "  CHECK(false) << \"Should not be reachable.\";\n");
  
  fprintf(f, "}  // %s_chunk_%04x\n\n", symbol.c_str(), addr_start);
}

static void GenerateCode(const CodeConfig &config,
			 const vector<uint8> code,
			 uint32 addr_start,
			 uint32 addr_past_end,
			 const string &symbol,
			 const string &filename,
			 const string &cart_name) {
  CHECK(0 == ((addr_past_end - addr_start) % (1 << CHUNK_SIZE))) <<
    "Chunk size must divide code block size. This can be relaxed "
    "reasonably easily...";

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
	  cart_name.c_str());

  fprintf(f, "/* aot-prelude.inc */\n%s\n/* aot-prelude.inc */\n",
	  ReadFileToString("aot-prelude.inc").c_str());
  
  for (uint32 i = addr_start; i < addr_past_end; i += (1 << CHUNK_SIZE)) {
    fprintf(f, "static void %s_chunk_%04x(FC *);\n",
	    symbol.c_str(), i);
  }
  fprintf(f, "\n\n");
  
  // First we need to create the outer function header. This function
  // is capable of executing at any PC value.
  GenerateOuter(config, addr_start, addr_past_end, symbol, f);

  // Now generate a function for each chunk.
  for (uint32 i = addr_start; i < addr_past_end; i += (1 << CHUNK_SIZE)) {
    GenerateChunk(
	config,
	code,
	addr_start,
	i, min(addr_past_end, i + (1 << CHUNK_SIZE)),
	symbol,
	f);
  }
  
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
	       "mario", "mario.cc", "mario.nes");

  double compile_seconds = compile_timer.GetSeconds();
  
  fprintf(stderr, "Finished.\n"
          "Compile time: %.4fs\n",
          compile_seconds);

  return 0;
}
