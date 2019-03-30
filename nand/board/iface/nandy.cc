
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

using namespace std;
using uint8 = uint8_t;
using uint64 = uint64_t;

class Binary3 {
public:
  Binary3() {}
  explicit constexpr Binary3(uint8_t bits) : bits(bits & 0b111) {}
  uint8_t Bits() const { return bits; }

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
  Binary3D IP = 0LL;

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

  struct Gate {
    Gate() {}
    Gate(int src_a, int src_b) : src_a(src_a), src_b(src_b) {}
    int src_a = 0, src_b = 0;
  };

  // Same as above, using only nand ("NAN") gates.
  // This is just a transformation of the entire state (no latches,
  // clock, flip-flops, etc.). The execution model is that we have
  // some N bits; the first two are set to constants 0 and 1 respectively,
  // then the next K are initialized with the current state,
  // and the remainder are defined as a nand of a pair of bits from
  // *earlier* in the bit vector. The final K bits become the state
  // for the next round.
  static void MakeNandwork() {
    // We never actually look at the first K+2 gates' sources, but
    // this is easiest to think about if it's aligned with the values
    // array.
    // First 2 gates are built-in 0 and 1.
    vector<Gate> gates(2);

    // bool
    using gateb = int;
    using gate3 = std::tuple<int, int, int>;
    // often contiguous, but not always. Has length 3*D.
    using gate3d = std::vector<int>;
    // no conventional length.
    using gaten = std::vector<int>; 
    
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

    printf("[input state = K + 2] Total gates: %d\n",
	   (int)gates.size());
    
    // PERF reserve the fixed size ahead of time.

    // The vector consists just of bits, but we have some higher-level
    // structure to them. We can allocate and name a single bit,
    // or a trio of them (e.g. representing a binary3 number); then
    // we have a trio of bit addresses. Finally, a Binary3D number,
    // which is D * 3 bits.

    // TODO: Read and increment instructions.

    // Where 0 is the MSB.
    auto Binary3DGetBit = [](Binary3D value, int i) {
	return (value >> ((D - i) - 1)) & 1;
      };
    
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
	for (int i = 0; i < 3 * D; i++) {
	  gateb bit = Xorb(carry, bit);
	  out.push_front(bit);
	  // PERF final carry value not used.
	  carry = Andb(carry, bit);
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
    
    gate3 ab_value = Ifb3(use_reg_b, b_in, a_in);
    gate3 reg_value =
      Ifb3(std::get<1>(reg),
	   // Z or A,
	   Ifb3(std::get<2>(reg), z_in, a_in),
	   // B or C
      	   Ifb3(std::get<2>(reg), b_in, c_in));

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

    // And the value loaded from this address in memory, in case
    // we do a load.

    gate3 load_value = ReadAddr3d(full_addr);
    
    // Note: Can reuse addr_part instead of computing it again.
    gate3 plus_value = Tabled(Binary3::Plus, ab_value, addr_part);
    gate3 minus_value =
      Tabled(Binary3::Minus, ab_value, Tabled(Binary3::Minus, reg_value, lit));
    gate3 max_value =
      Tabled(Binary3::Max, ab_value, Tabled(Binary3::Max, reg_value, lit));

    printf("[prep values] Total gates: %d\n",
	   (int)gates.size());

    
  }

  #if 0
      void ExecuteNand() {
    vector<bool> values(K + 2);
    values[0] = false;
    values[1] = true;

    }
#endif
  
  void Step() {
    // All instructions consist of op/reg/lit fields, each 3 bits.
    const Binary3 op = GetNextInstruction();
    const Binary3 reg = GetNextInstruction();
    const Binary3 lit = GetNextInstruction();

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
	case 0b001: // Minus
	  return minus_value;
	case 0b010: // Max
	  return max_value;
	case 0b011: // Load
	  return (ADDR_COUNT == 0) ? load_value : ab_value;
	default:
	  // No change.
	  return ab_value;
	}
      }();
    
    // Update registers. They can get the new value
    // or persist their current one.
    A = (reg.Bits() & 0b100) ? A : ab_value_out;
    B = (reg.Bits() & 0b100) ? ab_value_out : B;
    C = ((reg.Bits() & 0b100) && op.Bits() == STASH) ? C : reg_value;
    Z = ((reg.Bits() & 0b100) && op.Bits() == STASH) ? reg_value : Z;
    
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

void TestNandy() {
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
  CHECK_EQ(trace, "19341ca6ce648ff081d127643a8d0c15");
}

int main(int argc, char **argv) {
  // Nandy nandy;
  // nandy.Step();

  Nandy::MakeNandwork();
  
  TestNandy();
  
  return 0;
}
