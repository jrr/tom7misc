
// The entire computer is parameterized by a dimension D.

// A word is a binary3 number, three bits.
// An instruction is a triple of words, nine bits.
// The first three bits are the opcode, the second describe the
// addressing, and the third a literal.

// See below for the canonical semantics; the old comment was out
// of date!

#include <cstdint>
#include <vector>
#include <tuple>
#include <deque>
#include <functional>

#include "arcfour.h"
#include "image.h"
#include "md5.h"
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
static constexpr IPow(int base, int exponent) {
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
  Binary3D IP = 123; // XXXXXXXXX 0LL;

  // For load, store, and jump, we have an address that we can shift
  // bits into.
  // "Big-endian"
  Binary3D ADDR = 0LL;
  // XXX Actually just need enough bits to store up to D.
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
  vector<bool> InitializeBools(const Nandwork &work) const {
    vector<bool> ret;
    ret.resize(work.num_inputs);
    ret[0] = 0;
    ret[1] = 1;

    auto AssignTo3 = [&](gate3 g, Binary3 b) {
	int x, y, z;
	std::tie(x, y, z) = g;
	const uint8 bb = b.Bits();
	ret[x] = !!(bb & 0b100);
	ret[y] = !!(bb & 0b010);
	ret[z] = !!(bb & 0b001);
      };
    AssignTo3(work.z_in, Z);
    AssignTo3(work.a_in, A);
    AssignTo3(work.b_in, B);
    AssignTo3(work.c_in, C);

    auto AssignTo3D = [&](gate3d g, Binary3D b) {
	CHECK(g.size() == 3 * D);
	for (int i = 0; i < 3 * D; i++) {
	  ret[g[i]] = !!Binary3DGetBit(b, i);
	}
      };

    AssignTo3D(work.ip_in, IP);
    AssignTo3D(work.addr_in, ADDR);

    {
      // addr_count is a shift register
      for (int i : work.addr_count_in) {
	ret[i] = 0;
      }
      CHECK(ADDR_COUNT >= 0 && ADDR_COUNT < work.addr_count_in.size());
      int ac_bit_index = (D - 1) - ADDR_COUNT;
      CHECK(ac_bit_index >= 0 && ac_bit_index < work.addr_count_in.size());
      ret[work.addr_count_in[ac_bit_index]] = 1;
    }
    
    CHECK_EQ(MEM.size() * 3, work.mem_in.size());
    for (int m = 0; m < MEM.size(); m++) {
      const uint8 bb = MEM[m].Bits();
      ret[work.mem_in[m * 3 + 0]] = !!(bb & 0b100);
      ret[work.mem_in[m * 3 + 1]] = !!(bb & 0b010);
      ret[work.mem_in[m * 3 + 2]] = !!(bb & 0b001);
    }

    return ret;
  }

  void StepBools(const Nandwork &work, vector<bool> *values) {
    CHECK(values->size() == work.num_inputs);

    /*
    for (int i = 0; i < 128; i++) {
      printf("%c", (*values)[i] ? '1' : '0');
    }
    printf("\n");
    */
    
    for (int i = work.num_inputs; i < work.gates.size(); i++) {
      const Gate &gate = work.gates[i];
      bool a = (*values)[gate.src_a];
      bool b = (*values)[gate.src_b];
      bool res = !(a && b);
      values->push_back(res);
    }

    auto Get3D = [&](gate3d g) -> Binary3D {
	CHECK(g.size() == 3 * D);
	Binary3D res = 0;
	for (int i : g) {
	  res <<= 1;
	  res |= ((*values)[i] ? 0b1 : 0b0);
	}
	return res;
      };
    
    // XXX debugging crap
    auto ToString3 = [&](gate3 g) {
	const uint8 b =
	  ((*values)[std::get<0>(g)] ? 0b100 : 0) |
	  ((*values)[std::get<1>(g)] ? 0b010 : 0) |
	  ((*values)[std::get<2>(g)] ? 0b001 : 0);
	return Binary3(b).ToString();
      };

    VERBOSE
    printf("Nand ip: %d->%d ADDR %d instruction: %s|%s|%s\n",
	   Get3D(work.ip_in),
	   Get3D(work.ip2),
	   Get3D(work.addr_in),
	   ToString3(work.op).c_str(),
	   ToString3(work.reg).c_str(),
	   ToString3(work.lit).c_str());

    VERBOSE
    printf("NAND Reg value: %s AB value: %s Addr part: %s Load val: %s\n",
	   ToString3(work.reg_value).c_str(),
	   ToString3(work.ab_value).c_str(),
	   ToString3(work.addr_part).c_str(),
	   ToString3(work.load_value).c_str());

    VERBOSE
    printf("NAND plus %s minus %s max %s ab_value_out %s\n",
	   ToString3(work.plus_value).c_str(),
	   ToString3(work.minus_value).c_str(),
	   ToString3(work.max_value).c_str(),
	   ToString3(work.ab_value_out).c_str());

    VERBOSE
    printf("Deb\n"
	   "00: %c\n"
	   "01: %c\n"
	   "10: %c\n"
	   "11: %c\n",
	   (*values)[work.deb00] ? '1' : '0',
	   (*values)[work.deb01] ? '1' : '0',
	   (*values)[work.deb10] ? '1' : '0',
	   (*values)[work.deb11] ? '1' : '0');

    VERBOSE
    printf("Deb3\n"
	   "a: %s\n"
	   "b: %s\n"
	   "c: %s\n",
	   ToString3(work.deba).c_str(),
	   ToString3(work.debb).c_str(),
	   ToString3(work.debc).c_str());
  }

  void CopyBoolsToState(const Nandwork &work,
			const vector<bool> &values) {

    auto Get3 = [&](gate3 g) -> Binary3 {
	const uint8 b =
	  (values[std::get<0>(g)] ? 0b100 : 0) |
	  (values[std::get<1>(g)] ? 0b010 : 0) |
	  (values[std::get<2>(g)] ? 0b001 : 0);
	return Binary3(b);
      };

    auto Get3D = [&](gate3d g) -> Binary3D {
	Binary3D res = 0;
	for (int i : g) {
	  res <<= 1;
	  res |= (values[i] ? 0b1 : 0b0);
	}
	return res;
      };
    
    Z = Get3(work.z_out);
    A = Get3(work.a_out);
    B = Get3(work.b_out);
    C = Get3(work.c_out);
    IP = Get3D(work.ip_out);
    ADDR = Get3D(work.addr_out);

    ADDR_COUNT = [&]() {
	CHECK(work.addr_count_out.size() == D);
	int count = 0;
	for (int i = D - 1; i >= 0; i--) {
	  if (values[work.addr_count_out[i]]) {
	    return count;
	  }
	  count++;
	}
	CHECK(false) << "No bit set in addr_count_out?!";
	return 0;
      }();

    CHECK(work.mem_out.size() == MEM_SIZE * 3);
    for (int m = 0; m < MEM_SIZE; m++) {
      // In groups of three.
      const uint8 b =
	(values[work.mem_out[m * 3 + 0]] ? 0b100 : 0) |
	(values[work.mem_out[m * 3 + 1]] ? 0b010 : 0) |
	(values[work.mem_out[m * 3 + 2]] ? 0b001 : 0);
      MEM[m] = Binary3(b);
    }
  }
  
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
  static Nandwork MakeNandwork() {
    // We never actually look at the first K+2 gates' sources, but
    // this is easiest to think about if it's aligned with the values
    // array.
    // First 2 gates are built-in 0 and 1.
    vector<Gate> gates;

    auto Place3 = [&]() {
	int idx = gates.size();
	gates.emplace_back();
	gates.emplace_back();
	gates.emplace_back();
	return gate3(idx, idx + 1, idx + 2);
      };

    auto PlaceN = [&](int n) {
	vector<int> idxes;
	for (int i = 0; i < n; i++) {
	  idxes.push_back(gates.size());
	  gates.emplace_back();
	}
	return idxes;
      };
    
    auto Place3D = [&]() { return PlaceN(D * 3); };

    // 0 and 1
    gates.emplace_back();
    gates.emplace_back();
    
    gate3 z_in = Place3();
    gate3 a_in = Place3();
    gate3 b_in = Place3();
    gate3 c_in = Place3();

    gate3d ip_in = Place3D();
    gate3d addr_in = Place3D();

    // for the address count, we use unary
    gaten addr_count_in = PlaceN(D);
    
    // And then the memory is most of the space.
    gaten mem_in = PlaceN(MEM_SIZE * 3);

    const int num_inputs = gates.size();
    
    printf("[input state = K + 2] Total gates: %d\n",
	   (int)gates.size());
    
    // PERF reserve the fixed size ahead of time.

    // The vector consists just of bits, but we have some higher-level
    // structure to them. We can allocate and name a single bit,
    // or a trio of them (e.g. representing a binary3 number); then
    // we have a trio of bit addresses. Finally, a Binary3D number,
    // which is D * 3 bits.

    // TODO: Read and increment instructions.
    
    auto Nandb = [&](gateb a, gateb b) -> gateb {
	// TODO PERF peephole
	const int idx = gates.size();
	gates.emplace_back(a, b);
	return idx;
    };

    auto Notb = [&](gateb a) -> gateb {
	// TODO PERF peephole
	const int idx = gates.size();
	gates.emplace_back(a, a);
	return idx;
      };
    
    auto Orb = [&](gateb a, gateb b) -> gateb {
	// TODO: Simple peephole optimizations if a == 1 or 0, etc.
	gateb nota = Notb(a);
	gateb notb = Notb(b);
	return Nandb(nota, notb);
      };

    auto Andb = [&](gateb a, gateb b) -> gateb {
	// TODO: Simple peephole optimizations if a == 1 or 0, etc.
	gateb g = Nandb(a, b);
	return Notb(g);
      };

    auto Xorb = [&](gateb a, gateb b) -> gateb {
	gateb g = Nandb(a, b);
	gateb ga = Nandb(a, g);
	gateb gb = Nandb(b, g);
	return Nandb(ga, gb);
      };
    
    // Same as "XNOR"
    auto Eqb = [&](gateb a, gateb b) -> gateb {
	gateb nota = Notb(a);
	gateb notb = Notb(b);
	return Nandb(Nandb(a, b), Nandb(nota, notb));
      };
    
    auto LiteralEq3D = [&](gate3d v, Binary3D literal) -> gateb {
	CHECK(v.size() == 3 * D);
	gateb alleq = 1;
	for (int i = 0; i < 3 * D; i++) {
	  alleq = Andb(alleq, Eqb(v[i], Binary3DGetBit(literal, i)));
	}
	return alleq;
      };

    // Note, this checks for equality of the two, but returns a boolean,
    // not a gate3.
    auto Eq3 = [&](gate3 a, gate3 b) -> gateb {
	return Andb(Eqb(std::get<0>(a), std::get<0>(b)),
		    Andb(Eqb(std::get<1>(a), std::get<1>(b)),
			 Eqb(std::get<2>(a), std::get<2>(b))));
      };
    
    auto And3 = [&](gate3 a, gate3 b) -> gate3 {
	return gate3(Andb(std::get<0>(a), std::get<0>(b)),
		     Andb(std::get<1>(a), std::get<1>(b)),
		     Andb(std::get<2>(a), std::get<2>(b)));
      };

    auto Or3 = [&](gate3 a, gate3 b) -> gate3 {
	return gate3(Orb(std::get<0>(a), std::get<0>(b)),
		     Orb(std::get<1>(a), std::get<1>(b)),
		     Orb(std::get<2>(a), std::get<2>(b)));
      };

    // Read 3 bits from mem, at the 3*D bit address.
    // PERF: This is a pretty intense way to do this.
    // PERF: One easy optimization would be to compute
    // the 'correct' bit just once for the sequence of
    // three loads below; then just reference it offset
    // (modulo the memory size) for the two that come
    // after.
    auto ReadAddr3d = [&](gate3d addr) {
	CHECK(addr.size() == 3 * D);
	// Generate a new memory, which contains zeroes everywhere
	// except the indicated address.

	// First, a bit mask indicating which address is correct.
	vector<gateb> correct;
	for (int i = 0; i < MEM_SIZE; i++) {
	  correct.push_back(LiteralEq3D(addr, i));
	}

	// Then, a copy of the memory masked by this.
	vector<gate3> masked;
	for (int i = 0; i < MEM_SIZE; i++) {
	  gate3 mask{correct[i], correct[i], correct[i]};
	  gate3 src{mem_in[3 * i + 0],
		    mem_in[3 * i + 1],
		    mem_in[3 * i + 2]};
	  masked.push_back(And3(src, mask));
	}

	gate3 result{0, 0, 0};
	for (int i = 0; i < MEM_SIZE; i++) {
	  result = Or3(result, masked[i]);
	}
	return result;
      };

    auto DeqVec = [](std::deque<int> d) {
	vector<int> ret;
	for (int i : d) ret.push_back(i);
	return ret;
      };

    auto Increment3d = [&](gate3d x) {
	// Incremeting works its way from lsb to msb.
	// We have a carry, which starts as 1. The
	// output bit is Xor(carry, bit), and then
	// carry is And(carry, bit).
	gateb carry = 1;

	// So we can push_front.
	deque<int> out;
	for (int i = 3 * D - 1; i >= 0; i--) {
	  gateb bit = Xorb(carry, x[i]);
	  out.push_front(bit);
	  // PERF final carry value not used.
	  carry = Andb(carry, x[i]);
	}

	return DeqVec(out);
      };
    
    gate3 op = ReadAddr3d(ip_in);
    gate3d ip2 = Increment3d(ip_in);
    gate3 reg = ReadAddr3d(ip2);
    gate3d ip3 = Increment3d(ip2);
    gate3 lit = ReadAddr3d(ip3);
    // Not ip_out, since we may jump.
    gate3d ip4 = Increment3d(ip3);

    // 1.2 million gates to read the current instruction......!

    printf("[read instruction, increment IP] Total gates: %d\n",
	   (int)gates.size());

    // This is just a bit in the instruction we can use directly.
    gateb use_reg_b = std::get<0>(reg);

    // if cond is true, then a, else b.
    auto Ifb3 = [&](gateb cond, gate3 a, gate3 b) -> gate3 {
	gateb ncond = Notb(cond);
	return Or3(And3(gate3{cond, cond, cond}, a),
		   And3(gate3{ncond, ncond, ncond}, b));
      };
    
    auto Ifb = [&](gateb cond, gateb a, gateb b) -> gateb {
	return Orb(Andb(cond, a), Andb(Notb(cond), b));
      };

    auto Ifb3d = [&](gateb cond, gate3d a, gate3d b) -> gate3d {
	CHECK(a.size() == 3 * D);
	CHECK(b.size() == 3 * D);
	vector<int> out;
	for (int i = 0; i < 3 * D; i++) {
	  out.push_back(Ifb(cond, a[i], b[i]));
	}
	return out;
      };
    
    gate3 ab_value = Ifb3(use_reg_b, b_in, a_in);
    gate3 reg_value =
      Ifb3(std::get<1>(reg),
	   // B or C
      	   Ifb3(std::get<2>(reg), c_in, b_in),
	   // Z or A,
	   Ifb3(std::get<2>(reg), a_in, z_in));

    auto Literal3 = [&](Binary3 v) {
	uint8 b = v.Bits();
	return gate3((b & 0b100) ? 1 : 0,
		     (b & 0b010) ? 1 : 0,
		     (b & 0b001) ? 1 : 0);
      };
    
    auto Tabled = [&](Binary3 (*f)(Binary3, Binary3), gate3 a, gate3 b) {
	// TODO PERF: Opportunities to optimize this significantly,
	// although maybe memoization is actually enough?
	vector<int> invec = {std::get<0>(a), std::get<1>(a), std::get<2>(a),
			     std::get<0>(b), std::get<1>(b), std::get<2>(b)};
	
	vector<gate3> result;
	for (int i = 0; i < 64; i++) {
	  result.push_back(
	      Literal3((*f)(Binary3(i >> 3), Binary3(i & 0b111))));
	}

	// Now like we did for reg_value above, test each bit
	// to split the space in half.
	std::function<gate3(int, int, int)> Rec =
	  [&](int input, int idx, int bits_left) -> gate3 {
	    if (bits_left == 0) {
	      return gate3{result[input]};
	    } else {
	      gate3 one_branch = Rec((input << 1) | 1, idx + 1, bits_left - 1);
	      gate3 zero_branch = Rec((input << 1) | 0, idx + 1, bits_left - 1);
	      return Ifb3(invec[idx], one_branch, zero_branch);
	    }
	  };

	return Rec(0, 0, 6);
      };

    // The address part we would shift into when doing a load
    // or store.
    gate3 addr_part = Tabled(Binary3::Plus, reg_value, lit);

    // Full address, with this new part shifted in. Note that we
    // don't even need to do any computation for this; it's just
    // a renumbering of bits we've already computed.
    gate3d full_addr;
    // First 3 bits get shifted off.
    for (int i = 3; i < 3 * D; i++) full_addr.push_back(addr_in[i]);
    full_addr.push_back(std::get<0>(addr_part));
    full_addr.push_back(std::get<1>(addr_part));
    full_addr.push_back(std::get<2>(addr_part));

    // True if this is a load, store, or jump, which will actually
    // accept the shifted address above.
    gateb is_addressing =
      Orb(Eq3(op, Literal3(Binary3(LOAD))),
	  Orb(Eq3(op, Literal3(Binary3(STORE))),
	      Eq3(op, Literal3(Binary3(JMP)))));
    
    // And the value loaded from this address in memory, in case
    // we do a load.

    gate3 load_value = ReadAddr3d(full_addr);
    
    // Note: Can reuse addr_part instead of computing it again.
    gate3 plus_value = Tabled(Binary3::Plus, ab_value, addr_part);
    gate3 minus_value =
      Tabled(Binary3::Minus, ab_value, Tabled(Binary3::Minus, reg_value, lit));
    gate3 max_value =
      Tabled(Binary3::Max, ab_value, Tabled(Binary3::Max, reg_value, lit));

    printf("[prep values] Total gates: %d\n", (int)gates.size());

    // addr_count is just a rotation, but only if is_addressing is true.
    gaten addr_count_out;
    for (int i = 0; i < D; i++) {
      addr_count_out.push_back(
	  Ifb(is_addressing,
	      addr_count_in[(i + 1) % D],
	      addr_count_in[i]));
    }
    // like the test of (ADDR_COUNT == 0); this is true if we
    // just overflowed after incrementing and now the LSB is 1.
    gateb addr_count_active = addr_count_out[D - 1];

    static_assert(((PLUS | MINUS | MAX | LOAD) & 0b100) == 0,
		  "assumed these have leading 0 below");
    gate3 ab_value_out =
      Ifb3(std::get<0>(op),
	   // None of the ops with leading 1 bit set ab.
	   ab_value,
	   Ifb3(std::get<1>(op),
		// 0b01x
		Ifb3(std::get<2>(op),
		     // 0b011 = LOAD
		     // (but only if addr_count_active)
		     Ifb3(addr_count_active,
			  load_value,
			  ab_value),
		     // 0b010 = MAX
		     max_value),
		// 0b00x
		Ifb3(std::get<2>(op),
		     // 0b001 = MINUS
		     minus_value,
		     // 0b000 = PLUS
		     plus_value)));

    
    // Now update memory.
    auto WriteAddr3d = [&](gate3d addr, gate3 value) {
	CHECK(addr.size() == 3 * D);
	// Generate a new memory, which contains zeroes everywhere
	// except the indicated address.
	// PERF: We probably already have this from the load before.
	
	// First, a bit mask indicating which address is correct.
	vector<gateb> correct;
	for (int i = 0; i < MEM_SIZE; i++) {
	  correct.push_back(LiteralEq3D(addr, i));
	}

	// Then, a copy that replaces only the one with the
	// correct address.
	
	vector<int> result;
	result.reserve(MEM_SIZE * 3);
	for (int i = 0; i < MEM_SIZE; i++) {
	  gate3 old{mem_in[3 * i + 0],
		    mem_in[3 * i + 1],
		    mem_in[3 * i + 2]};

	  gate3 res = Ifb3(correct[i], value, old);
	  result.push_back(std::get<0>(res));
	  result.push_back(std::get<1>(res));
	  result.push_back(std::get<2>(res));
	}

	return result;
      };


    printf("[got values] Total gates: %d\n", (int)gates.size());

    gaten mem_out =
      WriteAddr3d(full_addr,
		  // Store back the load value (from the same
		  // address!) if addr_count_active is false,
		  // or the instruction is not STORE.
		  // This is then an expensive no-op.
		  Ifb3(Andb(addr_count_active,
			    Eq3(op, Literal3(Binary3(STORE)))),
		       ab_value,
		       load_value));

    // Middle bit 1 means nan/infinite.
    gateb ab_is_finite = Notb(std::get<1>(ab_value));
    
    printf("[performed store] Total gates: %d\n", (int)gates.size());

    gate3 a_out = Ifb3(use_reg_b, a_in, ab_value_out);
    gate3 b_out = Ifb3(use_reg_b, ab_value_out, b_in);

    gate3 c_out = Ifb3(Andb(Notb(use_reg_b),
			    Eq3(op, Literal3(Binary3(STASH)))),
		       addr_part,
		       c_in);
    gate3 z_out = Ifb3(Andb(use_reg_b,
			    Eq3(op, Literal3(Binary3(STASH)))),
		       addr_part,
		       z_in);

    gate3d addr_out = Ifb3d(is_addressing, full_addr, addr_in);

    gateb is_jump = Eq3(op, Literal3(Binary3(JMP)));
    gate3d ip_out = Ifb3d(Andb(Andb(is_jump, addr_count_active),
			       ab_is_finite),
			  full_addr,
			  ip4);

    printf("[final] Total gates: %d\n", (int)gates.size());
    
    // Sanity check: no forward references.
    // Early gates will have inputs set to 0, which is fine since
    // they are not real gates and because values[0] will be false
    // in initialization.
    for (int i = 0; i < gates.size(); i++) {
      CHECK(gates[i].src_a == 0 || gates[i].src_a < i);
      CHECK(gates[i].src_b == 0 || gates[i].src_b < i);
    }

    Nandwork work;
    // This kind of debugging stuff has to happen before copying gates.
    // Tested: Nand, And, Xorb, Eqb, Orb
    // work.deb00 = Orb(0, 0);
    // work.deb01 = Orb(0, 1);
    // work.deb10 = Orb(1, 0);
    // work.deb11 = Orb(1, 1);
    work.deb00 = is_addressing;
    work.deb01 = LiteralEq3D(ip_in, 2816);
    work.deb10 = LiteralEq3D(ip_in, 321);
    work.deb11 = LiteralEq3D(ip_in, 0);
    
    work.deba = Tabled(Binary3::Plus, Literal3(Nan), Literal3(Nan));
    work.debb = Tabled(Binary3::Plus, Literal3(NegNan), Literal3(Inf));
    work.debc = Tabled(Binary3::Plus, Literal3(NegOne), Literal3(One));
    
    work.gates = gates;
    work.num_inputs = num_inputs;
    work.zero = 0;
    work.one = 1;
    
    work.z_in = z_in;
    work.a_in = a_in;
    work.b_in = b_in;
    work.c_in = c_in;
    work.ip_in = ip_in;
    work.addr_in = addr_in;
    work.addr_count_in = addr_count_in;
    work.mem_in = mem_in;

    // Debugging
    work.op = op;
    work.reg = reg;
    work.lit = lit;
    work.ip2 = ip2;

    work.reg_value = reg_value;
    work.ab_value = ab_value;
    work.addr_part = addr_part;
    work.load_value = load_value;

    work.plus_value = plus_value;
    work.minus_value = minus_value;
    work.max_value = max_value;
    work.ab_value_out = ab_value_out;
    
    // Outputs.
    work.z_out = z_out;
    work.a_out = a_out;
    work.b_out = b_out;
    work.c_out = c_out;
    work.ip_out = ip_out;
    work.addr_out = addr_out;
    work.addr_count_out = addr_count_out;
    work.mem_out = mem_out;

    return work;
  }

  #if 0
      void ExecuteNand() {
    vector<bool> values(K + 2);
    values[0] = false;
    values[1] = true;

    }
#endif
  
  void Step() {
    Binary3D start_ip = IP;
    // All instructions consist of op/reg/lit fields, each 3 bits.
    const Binary3 op = GetNextInstruction();
    const Binary3 reg = GetNextInstruction();
    const Binary3 lit = GetNextInstruction();

    VERBOSE
    printf("IP: %d. ADDR: %d, Instruction: %s|%s|%s\n",
	   start_ip,
	   ADDR,
	   op.ToString().c_str(),
	   reg.ToString().c_str(),
	   lit.ToString().c_str());
    
    // Not all instructions will use them, but we always compute
    // them for simplicity:

    const Binary3 ab_value = (reg.Bits() & 0b100) ? B : A;
    const Binary3 reg_value = [&]() {
	switch (reg.Bits() & 0b011) {
	default:
	case 0b00: return Z;
	case 0b01: return A;
	case 0b10: return B;
	case 0b11: return C;
	}
      }();
    
    // The address part we would shift into when doing a load
    // or store.
    const Binary3 addr_part = Binary3::Plus(reg_value, lit);
    const Binary3D full_addr = ((ADDR << 3) & MASK_3D) | addr_part.Bits();
    // The value at the loaded memory address.
    const Binary3 load_value = MEM[full_addr];

    VERBOSE
    printf("Reg value: %s AB value: %s addr part: %s load_value: %s\n",
	   reg_value.ToString().c_str(),
	   ab_value.ToString().c_str(),
	   addr_part.ToString().c_str(),
	   load_value.ToString().c_str());
    
    // Can reuse addr_part instead of computing it again.
    const Binary3 plus_value = Binary3::Plus(ab_value, addr_part);
    const Binary3 minus_value =
      Binary3::Minus(ab_value, Binary3::Minus(reg_value, lit));
    const Binary3 max_value =
      Binary3::Max(ab_value, Binary3::Max(reg_value, lit));

    // If load, store, or jump, advance address count.
    ADDR_COUNT = (op.Bits() == LOAD ||
		  op.Bits() == STORE ||
		  op.Bits() == JMP) ? (ADDR_COUNT + 1) % D : ADDR_COUNT;
    // .. and internal address value.
    ADDR = (op.Bits() == LOAD ||
	    op.Bits() == STORE ||
	    op.Bits() == JMP) ? full_addr : ADDR;
    
    // Value written to a|b.
    const Binary3 ab_value_out = [&]() {
	switch (op.Bits()) {
	case PLUS: // Plus
	  return plus_value;
	case MINUS: // Minus
	  return minus_value;
	case MAX: // Max
	  return max_value;
	case LOAD: // Load
	  return (ADDR_COUNT == 0) ? load_value : ab_value;
	default:
	  // No change.
	  return ab_value;
	}
      }();

    VERBOSE
    printf("plus %s minus %s max %s, ab_value_out: %s\n",
	   plus_value.ToString().c_str(),
	   minus_value.ToString().c_str(),
	   max_value.ToString().c_str(),
	   ab_value_out.ToString().c_str());

    
    // Update registers. They can get the new value
    // or persist their current one.
    A = (reg.Bits() & 0b100) ? A : ab_value_out;
    B = (reg.Bits() & 0b100) ? ab_value_out : B;
    C = (!(reg.Bits() & 0b100) && op.Bits() == STASH) ? addr_part : C;
    Z = ((reg.Bits() & 0b100) && op.Bits() == STASH) ? addr_part : Z;
    
    // Update memory. Really MEM_SIZE of these in parallel.
    for (int i = 0; i < MEM_SIZE; i++) {
      MEM[i] = (full_addr == i && ADDR_COUNT == 0 && op.Bits() == 0b100) ?
	ab_value : MEM[i];
    }

    // Update instruction pointer if necessary.
    IP = (op.Bits() == JMP && ADDR_COUNT == 0 && ab_value.IsFinite()) ?
      ADDR : IP;
  }
  
};

#define SAVE_IMAGE 0

static void TestNandy() {
  ArcFour rc{"nandy"};
  auto Rand3 = [&rc]() { return Binary3(rc.Byte() & 0b111); };
  Nandy nandy;
  for (int i = 0; i < Nandy::MEM_SIZE; i++) {
    nandy.MEM[i] = Rand3();
  }
  
  static constexpr int NUM_STEPS = 2048;
  const int width = nandy.GetState().size();
# if SAVE_IMAGE
  const int height = NUM_STEPS;
  ImageRGBA image(width, height);
# endif
  
  string trace = "start";
  trace.reserve(48 + width);
  for (int y = 0; y < NUM_STEPS; y++) {
    nandy.Step();
    vector<Binary3> row = nandy.GetState();
    for (int x = 0; x < row.size(); x++) {
      uint8 rgb = row[x].Bits();
#     if SAVE_IMAGE
      image.SetPixel(x, y,
		     rgb & 0b100 ? 255 : 0,
		     rgb & 0b010 ? 255 : 0,
		     rgb & 0b001 ? 255 : 0,
		     255);
#     endif
      trace.push_back('0' + rgb);
    }
    trace = MD5::Ascii(MD5::Hash(trace));
  }

# if SAVE_IMAGE
  image.Save("nandy-trace.png");
# endif

  printf("Trace hash: %s\n", trace.c_str());
  CHECK_EQ(trace, "6dcd0f2d4041009b704ce11d393dce24");
}

void Test2Nandy() {
  Nandy::Nandwork work = Nandy::MakeNandwork();

  // Always with the same identical state.
  auto Initialize = [](Nandy *nandy) {
      ArcFour rc{"nandy"};
      auto Rand3 = [&rc]() { return Binary3(rc.Byte() & 0b111); };
      for (int i = 0; i < Nandy::MEM_SIZE; i++) {
	nandy->MEM[i] = Rand3();
      }
    };
  
  Nandy nandy, nandy_nand;
  Initialize(&nandy);
  Initialize(&nandy_nand);

  /*
  CHECK_EQ(nandy.GetStateString(),
	   nandy_nand.GetStateString());
  */
  
  for (int i = 0; i < 10; i++) {
    string s1 = nandy.GetStateString();
    string s2 = nandy_nand.GetStateString();
    printf("%s <- step\n%s <- step_nand\n",
	   s1.c_str(), s2.c_str());
    CHECK_EQ(s1, s2);

    nandy.Step();
    nandy_nand.StepNand(work);
  }
  
#if 0
  static constexpr int NUM_STEPS = 2048;
  const int width = nandy.GetState().size();
# if SAVE_IMAGE
  const int height = NUM_STEPS;
  ImageRGBA image(width, height);
# endif
  
  string trace = "start";
  trace.reserve(48 + width);
  for (int y = 0; y < NUM_STEPS; y++) {
    nandy.StepNand(work);
    vector<Binary3> row = nandy.GetState();
    for (int x = 0; x < row.size(); x++) {
      uint8 rgb = row[x].Bits();
#     if SAVE_IMAGE
      image.SetPixel(x, y,
		     rgb & 0b100 ? 255 : 0,
		     rgb & 0b010 ? 255 : 0,
		     rgb & 0b001 ? 255 : 0,
		     255);
#     endif
      trace.push_back('0' + rgb);
    }
    trace = MD5::Ascii(MD5::Hash(trace));
  }

# if SAVE_IMAGE
  image.Save("nandy-trace.png");
# endif

  printf("Trace hash: %s\n", trace.c_str());
  CHECK_EQ(trace, "6dcd0f2d4041009b704ce11d393dce24");
#endif
}


int main(int argc, char **argv) {
  (void)TestNandy;
  (void)Test2Nandy;
  // Nandy nandy;
  // nandy.Step();

  #if 0
  Nandy::Binary3D ip = 123;
  printf("Getbit: ");
  for (int i = 0; i < 3 * Nandy::D; i++) {
    printf("%c", Nandy::Binary3DGetBit(ip, i) ? '1' : '0');
  }
  printf("\n");
  #endif
  
  // Nandy::MakeNandwork();
  
  Test2Nandy();
  
  return 0;
}
