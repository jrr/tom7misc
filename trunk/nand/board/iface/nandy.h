
// The entire computer is parameterized by a dimension D.

// A word is a binary3 number, three bits.
// An instruction is a triple of words, nine bits.
// The first three bits are the opcode, the second describe the
// addressing, and the third a literal.

// See below for the canonical semantics; the old comment was out
// of date!

#ifndef __NANDY_H
#define __NANDY_H

#include <cstdint>
#include <vector>
#include <tuple>
#include <deque>
#include <functional>
#include <unistd.h>
#include <ctime>

#include "base/logging.h"
#include "base/stringprintf.h"

#define VERBOSE if (0)

using namespace std;
using uint8 = uint8_t;
using uint64 = uint64_t;

class Binary3 {
public:
  Binary3() {}
  explicit constexpr Binary3(uint8_t bits) : bits(bits & 0b111) {}
  uint8_t Bits() const { return bits; }
  string ToString() const {
    string ret = "...";
    ret[0] = (bits & 0b100) ? '1' : '0';
    ret[1] = (bits & 0b010) ? '1' : '0';
    ret[2] = (bits & 0b001) ? '1' : '0';
    return ret;
  }
  
  static constexpr Binary3 Plus(Binary3 a, Binary3 b);
  static constexpr Binary3 Minus(Binary3 a, Binary3 b);
  static constexpr Binary3 Max(Binary3 a, Binary3 b);
  bool IsFinite() const {
    switch (bits) {
    case 0b000:
    case 0b001:
    case 0b100:
    case 0b101:
      return true;
    default:
      return false;
    }
  }
  
private:
  // Representation invariant: only low 3 bits can be 1.
  uint8_t bits = 0b011;
};

static constexpr Binary3 Zero =    Binary3{0b000};
static constexpr Binary3 One =     Binary3{0b001};
static constexpr Binary3 Inf =     Binary3{0b010};
static constexpr Binary3 Nan =     Binary3{0b011};
static constexpr Binary3 NegZero = Binary3{0b100};
static constexpr Binary3 NegOne =  Binary3{0b101};
static constexpr Binary3 NegInf =  Binary3{0b110};
static constexpr Binary3 NegNan =  Binary3{0b111};

constexpr Binary3 Binary3::Plus(Binary3 a, Binary3 b) {
  switch ((a.bits << 3) | b.bits) {
  default:
    // 0 + x = x, except for 0 + -0
  case 0b0'000'000: /* Zero */ return Zero;
  case 0b0'000'001: /* One */ return One;
  case 0b0'000'010: /* Inf */ return Inf;
  case 0b0'000'011: /* Nan */ return Nan;
  case 0b0'000'100: /* NegZero */ 
    // 0 + -0
    // 754-2008 6.3: "When the sum of two operands with opposite
    // signs (or the difference of two operands with like signs) is
    // exactly zero, the sign of that sum (or difference) shall be +0
    // in [this rounding mode]."
    return Zero;
  case 0b0'000'101: /* NegOne */ return NegOne;
  case 0b0'000'110: /* NegInf */ return NegInf;
  case 0b0'000'111: /* NegNan */ return NegNan;

    // 1 + x...
  case 0b0'001'000: /* Zero */ return One;
  case 0b0'001'001: /* One */ return Inf;
  case 0b0'001'010: /* Inf */ return Inf;
  case 0b0'001'011: /* Nan */ return Nan;
  case 0b0'001'100: /* NegZero */ return One;
  case 0b0'001'101: /* NegOne */ return Zero;
  case 0b0'001'110: /* NegInf */ return NegInf;
  case 0b0'001'111: /* NegNan */ return NegNan;

    // Inf + x ...
  case 0b0'010'000: /* Zero */ return Inf;
  case 0b0'010'001: /* One */ return Inf;
  case 0b0'010'010: /* Inf */ return Inf;
  case 0b0'010'011: /* Nan */ return Nan;
  case 0b0'010'100: /* NegZero */ return Inf;
  case 0b0'010'101: /* NegOne */ return Inf;
  case 0b0'010'110: /* NegInf */ return Nan;
  case 0b0'010'111: /* NegNan */ return NegNan;

    // Nan + x ...
  case 0b0'011'000: /* Zero */ return Nan;
  case 0b0'011'001: /* One */ return Nan;
  case 0b0'011'010: /* Inf */ return Nan;
  case 0b0'011'011: /* Nan */ return Nan;
  case 0b0'011'100: /* NegZero */ return Nan;
  case 0b0'011'101: /* NegOne */ return Nan;
  case 0b0'011'110: /* NegInf */ return Nan;
  case 0b0'011'111: /* NegNan */
    // nan + -nan very much not specified by the standard,
    // and there is no obvious rule to use. 
    return Nan;

    // -0 + x ...
    // This is the true additive identity, it turns out.
  case 0b0'100'000: /* Zero */ return Zero;
  case 0b0'100'001: /* One */ return One;
  case 0b0'100'010: /* Inf */ return Inf;
  case 0b0'100'011: /* Nan */ return Nan;
  case 0b0'100'100: /* NegZero */ return NegZero;
  case 0b0'100'101: /* NegOne */ return NegOne;
  case 0b0'100'110: /* NegInf */ return NegInf;
  case 0b0'100'111: /* NegNan */ return NegNan;

    // -1 + x ...
  case 0b0'101'000: /* Zero */ return NegOne;
  case 0b0'101'001: /* One */ return Zero;
  case 0b0'101'010: /* Inf */ return Inf;
  case 0b0'101'011: /* Nan */ return Nan;
  case 0b0'101'100: /* NegZero */ return NegOne;
  case 0b0'101'101: /* NegOne */ return NegInf;
  case 0b0'101'110: /* NegInf */ return NegInf;
  case 0b0'101'111: /* NegNan */ return NegNan;

    // -inf + x ...
  case 0b0'110'000: /* Zero */ return NegInf;
  case 0b0'110'001: /* One */ return NegInf;
  case 0b0'110'010: /* Inf */ return Nan;
  case 0b0'110'011: /* Nan */ return Nan;
  case 0b0'110'100: /* NegZero */ return NegInf;
  case 0b0'110'101: /* NegOne */ return NegInf;
  case 0b0'110'110: /* NegInf */ return NegInf;
  case 0b0'110'111: /* NegNan */ return NegNan;

    // -nan + x ...
  case 0b0'111'000: /* Zero */ return NegNan;
  case 0b0'111'001: /* One */ return NegNan;
  case 0b0'111'010: /* Inf */ return NegNan;
  case 0b0'111'011: /* Nan */ return Nan;
  case 0b0'111'100: /* NegZero */ return NegNan;
  case 0b0'111'101: /* NegOne */ return NegNan;
  case 0b0'111'110: /* NegInf */ return NegNan;
  case 0b0'111'111: /* NegNan */ return NegNan;
  }
}


constexpr Binary3 Binary3::Minus(Binary3 a, Binary3 b) {
  switch ((a.bits << 3) | b.bits) {
  default:
    // 0 - x = x
  case 0b0'000'000: /* Zero */ return Zero;
  case 0b0'000'001: /* One */ return NegOne;
  case 0b0'000'010: /* Inf */ return NegInf;
  case 0b0'000'011: /* Nan */ return Nan;
  case 0b0'000'100: /* NegZero */ return Zero;
  case 0b0'000'101: /* NegOne */ return One;
  case 0b0'000'110: /* NegInf */ return Inf;
  case 0b0'000'111: /* NegNan */ return NegNan;

    // 1 - x...
  case 0b0'001'000: /* Zero */ return One;
  case 0b0'001'001: /* One */ return Zero;
  case 0b0'001'010: /* Inf */ return NegInf;
  case 0b0'001'011: /* Nan */ return Nan;
  case 0b0'001'100: /* NegZero */ return One;
  case 0b0'001'101: /* NegOne */ return Inf;
  case 0b0'001'110: /* NegInf */ return Inf;
  case 0b0'001'111: /* NegNan */ return NegNan;

    // Inf - x ...
  case 0b0'010'000: /* Zero */ return Inf;
  case 0b0'010'001: /* One */ return Inf;
  case 0b0'010'010: /* Inf */ return Nan;
  case 0b0'010'011: /* Nan */ return Nan;
  case 0b0'010'100: /* NegZero */ return Inf;
  case 0b0'010'101: /* NegOne */ return Inf;
  case 0b0'010'110: /* NegInf */ return Inf;
  case 0b0'010'111: /* NegNan */ return NegNan;

    // Nan - x ...
  case 0b0'011'000: /* Zero */ return Nan;
  case 0b0'011'001: /* One */ return Nan;
  case 0b0'011'010: /* Inf */ return Nan;
  case 0b0'011'011: /* Nan */ return Nan;
  case 0b0'011'100: /* NegZero */ return Nan;
  case 0b0'011'101: /* NegOne */ return Nan;
  case 0b0'011'110: /* NegInf */ return Nan;
  case 0b0'011'111: /* NegNan */ return Nan;

    // -0 - x ...
  case 0b0'100'000: /* Zero */ return NegZero;
  case 0b0'100'001: /* One */ return NegOne;
  case 0b0'100'010: /* Inf */ return NegInf;
  case 0b0'100'011: /* Nan */ return Nan;
  case 0b0'100'100: /* NegZero */ return Zero;
  case 0b0'100'101: /* NegOne */ return One;
  case 0b0'100'110: /* NegInf */ return Inf;
  case 0b0'100'111: /* NegNan */ return NegNan;

    // -1 - x ...
  case 0b0'101'000: /* Zero */ return NegOne;
  case 0b0'101'001: /* One */ return NegInf;
  case 0b0'101'010: /* Inf */ return NegInf;
  case 0b0'101'011: /* Nan */ return Nan;
  case 0b0'101'100: /* NegZero */ return NegOne;
  case 0b0'101'101: /* NegOne */ return Zero;
  case 0b0'101'110: /* NegInf */ return Inf;
  case 0b0'101'111: /* NegNan */ return NegNan;

    // -inf - x ...
  case 0b0'110'000: /* Zero */ return NegInf;
  case 0b0'110'001: /* One */ return NegInf;
  case 0b0'110'010: /* Inf */ return NegInf;
  case 0b0'110'011: /* Nan */ return Nan;
  case 0b0'110'100: /* NegZero */ return NegInf;
  case 0b0'110'101: /* NegOne */ return NegInf;
  case 0b0'110'110: /* NegInf */ return Nan;
  case 0b0'110'111: /* NegNan */ return NegNan;

    // -nan - x ...
  case 0b0'111'000: /* Zero */ return NegNan;
  case 0b0'111'001: /* One */ return NegNan;
  case 0b0'111'010: /* Inf */ return NegNan;
  case 0b0'111'011: /* Nan */ return Nan;
  case 0b0'111'100: /* NegZero */ return NegNan;
  case 0b0'111'101: /* NegOne */ return NegNan;
  case 0b0'111'110: /* NegInf */ return NegNan;
  case 0b0'111'111: /* NegNan */ return NegNan;
  }
}

constexpr Binary3 Binary3::Max(Binary3 a, Binary3 b) {
  switch ((a.bits << 3) | b.bits) {
  default:
    // max(0, x)
  case 0b0'000'000: /* Zero */ return Zero;
  case 0b0'000'001: /* One */ return One;
  case 0b0'000'010: /* Inf */ return Inf;
  case 0b0'000'011: /* Nan */ return Zero;
  case 0b0'000'100: /* NegZero */ return Zero;
  case 0b0'000'101: /* NegOne */ return Zero;
  case 0b0'000'110: /* NegInf */ return Zero;
  case 0b0'000'111: /* NegNan */ return Zero;

    // max(1, x)
  case 0b0'001'000: /* Zero */ return One;
  case 0b0'001'001: /* One */ return One;
  case 0b0'001'010: /* Inf */ return Inf;
  case 0b0'001'011: /* Nan */ return One;
  case 0b0'001'100: /* NegZero */ return One;
  case 0b0'001'101: /* NegOne */ return One;
  case 0b0'001'110: /* NegInf */ return One;
  case 0b0'001'111: /* NegNan */ return One;

    // max(Inf, x)
  case 0b0'010'000: /* Zero */ return Inf;
  case 0b0'010'001: /* One */ return Inf;
  case 0b0'010'010: /* Inf */ return Inf;
  case 0b0'010'011: /* Nan */ return Inf;
  case 0b0'010'100: /* NegZero */ return Inf;
  case 0b0'010'101: /* NegOne */ return Inf;
  case 0b0'010'110: /* NegInf */ return Inf;
  case 0b0'010'111: /* NegNan */ return Inf;

    // max(Nan, x)
  case 0b0'011'000: /* Zero */ return Zero;
  case 0b0'011'001: /* One */ return One;
  case 0b0'011'010: /* Inf */ return Inf;
  case 0b0'011'011: /* Nan */ return Nan;
  case 0b0'011'100: /* NegZero */ return NegZero;
  case 0b0'011'101: /* NegOne */ return NegOne;
  case 0b0'011'110: /* NegInf */ return NegInf;
  case 0b0'011'111: /* NegNan */ return Nan;

    // max(-0, x)
  case 0b0'100'000: /* Zero */ return Zero;
  case 0b0'100'001: /* One */ return One;
  case 0b0'100'010: /* Inf */ return Inf;
  case 0b0'100'011: /* Nan */ return NegZero;
  case 0b0'100'100: /* NegZero */ return NegZero;
  case 0b0'100'101: /* NegOne */ return NegZero;
  case 0b0'100'110: /* NegInf */ return NegZero;
  case 0b0'100'111: /* NegNan */ return NegZero;

    // max(-1, x)
  case 0b0'101'000: /* Zero */ return Zero;
  case 0b0'101'001: /* One */ return One;
  case 0b0'101'010: /* Inf */ return Inf;
  case 0b0'101'011: /* Nan */ return NegOne;
  case 0b0'101'100: /* NegZero */ return NegZero;
  case 0b0'101'101: /* NegOne */ return NegOne;
  case 0b0'101'110: /* NegInf */ return NegOne;
  case 0b0'101'111: /* NegNan */ return NegOne;

    // max(-inf, x)
  case 0b0'110'000: /* Zero */ return Zero;
  case 0b0'110'001: /* One */ return One;
  case 0b0'110'010: /* Inf */ return Inf;
  case 0b0'110'011: /* Nan */ return NegInf;
  case 0b0'110'100: /* NegZero */ return NegZero;
  case 0b0'110'101: /* NegOne */ return NegOne;
  case 0b0'110'110: /* NegInf */ return NegInf;
  case 0b0'110'111: /* NegNan */ return NegInf;

    // max(-nan, x)
  case 0b0'111'000: /* Zero */ return Zero;
  case 0b0'111'001: /* One */ return One;
  case 0b0'111'010: /* Inf */ return Inf;
  case 0b0'111'011: /* Nan */ return Nan;
  case 0b0'111'100: /* NegZero */ return NegZero;
  case 0b0'111'101: /* NegOne */ return NegOne;
  case 0b0'111'110: /* NegInf */ return NegInf;
  case 0b0'111'111: /* NegNan */ return NegNan;
  }
}


// Compile-time integer power, for small exponents.
static constexpr int IPow(int base, int exponent) {
  return (exponent == 0) ? 1 : base * IPow(base, exponent - 1);
}

class Nandy {
public:
  // Could be a template parameter, but for simplicity...
  static constexpr int D = 4;
  static constexpr int MEM_SIZE = IPow(8, D);
  
  using Binary3D = uint16_t;
  static constexpr Binary3D MASK_3D = IPow(8, D) - 1;

  static_assert(3 * D <= sizeof(Binary3D) * 8,
		"need an integral type that's big enough for this");

  static constexpr uint8 PLUS  = 0b000;
  static constexpr uint8 MINUS = 0b001;
  static constexpr uint8 MAX   = 0b010;
  static constexpr uint8 LOAD  = 0b011;
  static constexpr uint8 STORE = 0b100;
  static constexpr uint8 STASH = 0b101;
  static constexpr uint8 JMP   = 0b110;
  static constexpr uint8 NOP   = 0b111;
  
  
  Binary3 Z = NegZero, A = NegZero, B = NegZero, C = NegZero;
  // "Big-endian"
  Binary3D IP = 0LL;

  // For load, store, and jump, we have an address that we can shift
  // bits into.
  // "Big-endian"
  Binary3D ADDR = 0LL;
  // (Actually just need enough bits to store up to D.)
  Binary3D ADDR_COUNT = 0;
  
  std::vector<Binary3> MEM;

  std::vector<Binary3> GetState() const {
    std::vector<Binary3> state;
    state.reserve(D + D + D + 4 + MEM_SIZE);
    auto Add3D = [&](Binary3D b) {
	for (int i = D - 1; i >= 0; i--) {
	  state.push_back(Binary3((b >> (i * 3)) & 0b111));
	}
      };
    Add3D(IP);
    Add3D(ADDR);
    Add3D(ADDR_COUNT);
    state.push_back(Z);
    state.push_back(A);
    state.push_back(B);
    state.push_back(C);
    for (Binary3 b : MEM) state.push_back(b);
    return state;
  }
  
  string GetStateString() const {
    string ret;
    StringAppendF(&ret, "%d/%d/%d:%s|%s|%s|%s:(mem)",
		  IP, ADDR, ADDR_COUNT,
		  Z.ToString().c_str(),
		  A.ToString().c_str(),
		  B.ToString().c_str(),
		  C.ToString().c_str());
    return ret;
  }

  Nandy() {
    // Initialize memory to NAN.
    MEM.reserve(MEM_SIZE);
    for (int i = 0; i < MEM_SIZE; i++) MEM.push_back(Nan);
  }

  // This just gets the next binary3 word; we will fetch three
  // in a row to form the op/reg/lit tuple.
  Binary3 GetNextInstruction() {
    // Here we are just treating the IP as a D*3 bit number.
    // Short of having a separate instruction stream, there is
    // no good way I can think of to stay true to "floating point"
    // addressing here, since you need it to be auto-incremented.
    // (I guess maybe you could have the "instruction pointer" be
    // a D-dimensional vector. It would get pretty crazy..?)
    const Binary3 ins = MEM[IP];
    IP++;
    IP &= MASK_3D;
    return ins;
  }

  // Where 0 is the MSB.
  static constexpr uint8 Binary3DGetBit(Binary3D value, int i) {
    return (value >> (((3 * D) - i) - 1)) & 1;
  }
  
  struct Gate {
    Gate() {}
    Gate(int src_a, int src_b) : src_a(src_a), src_b(src_b) {}
    int src_a = 0, src_b = 0;
  };

  // bool
  using gateb = int;
  using gate3 = std::tuple<int, int, int>;
  // often contiguous, but not always. Has length 3*D.
  using gate3d = std::vector<int>;
  // no conventional length.
  using gaten = std::vector<int>; 
  
  struct Nandwork {
    vector<Gate> gates;
    // Number of "gates" that are actually input wires. ("K")
    int num_inputs;

    // Must be set to 0, 1 respectively.
    gateb zero;
    gateb one;
    
    // Inputs.
    gate3 z_in;
    gate3 a_in;
    gate3 b_in;
    gate3 c_in;
    gate3d ip_in;
    gate3d addr_in;
    // D
    gaten addr_count_in;
    // MEM_SIZE * 3
    gaten mem_in;

    // Debugging locations.
    gate3 op;
    gate3 reg;
    gate3 lit;
    gate3d ip2;
    gate3 reg_value;
    gate3 ab_value;
    gate3 addr_part;
    gate3 load_value;

    gate3 plus_value, minus_value, max_value, ab_value_out;
    
    // pure debugging output for binary ops
    gateb deb00, deb01, deb10, deb11;
    gate3 deba, debb, debc;
    
    // Output locations.
    gate3 z_out;
    gate3 a_out;
    gate3 b_out;
    gate3 c_out;
    gate3d ip_out;
    gate3d addr_out;
    // D
    gaten addr_count_out;
    // MEM_SIZE * 3
    gaten mem_out;
  };

  // Initialize the vector<bool> with work.num_inputs from the
  // current state (plus constants).
  vector<bool> InitializeBools(const Nandwork &work) const;
  static void StepBools(const Nandwork &work, vector<bool> *values);
  void CopyBoolsToState(const Nandwork &work,
			const vector<bool> &values);

  // Step, using only NAND gates.
  void StepNand(const Nandwork &work) {
    vector<bool> values = InitializeBools(work);
    StepBools(work, &values);
    CopyBoolsToState(work, values);
  }
  
  // Construct the nand-based network for the Step state transformation.
  // This is just a transformation of the entire state (no latches,
  // clock, flip-flops, etc.). The execution model is that we have
  // some N bits; the first two are set to constants 0 and 1 respectively,
  // then the next K are initialized with the current state,
  // and the remainder are defined as a nand of a pair of bits from
  // *earlier* in the bit vector. The final K bits become the state
  // for the next round.
  static Nandwork MakeNandwork();

  // Normal step, directly executed.
  void Step();  
};

#endif
