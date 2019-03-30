
// The entire computer is parameterized by a dimension D.

// A word is a binary3 number, three bits.
// An instruction is a triple of words, nine bits.
// The first three bits are the opcode, the second describe the
// addressing, and the third a literal.

// Addressing is: 1 bit to choose A or B
// 2 bits to choose reg = Z,A,B,C
// A|B <- op(A|B, reg, lit)

// ops are
//  000 +
//  001 -
//  010 max

//  011 load    A|B <- [reg + lit]   (or A + reg + lit?)
//  100 store   [reg + lit] <- A|B
//
//  101 stash   C|Z <- reg + lit
//  110 jump to addr:reg+lit if A|B is finite
//  111 NOP

// For load and store, you are computing a point in D-dimensional
// space, where each axis can take on the 8 values of binary3 numbers.
// So there are 8^D storage locations. (I guess instructions can
// also come from this same memory, as consecutive triples. Annoying
// that 3 doesn't divide 8, then.)
// There is an internal shift register (which starts as D*3 0s), and
// the first D-1 calls to load/store are just shifting into this
// register, but not actually performing the load. The Dth call
// shifts and then does it.

//  101 something that allows writing to C
//  110 some way to do (conditional) control flow
//  111 I/O? (for example print A + reg + lit, three bits)
//      (or just memory-mapped IO)

// or if  C can't be written, then it just always contains NaN maybe?
// or Z = +0 and C = -0 for no good reason?

#include <cstdint>
#include <vector>

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
  
  std::vector<Binary3> mem;

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
    for (Binary3 b : mem) state.push_back(b);
    return state;
  }
  
  Nandy() {
    // Initialize memory to NAN.
    mem.reserve(MEM_SIZE);
    for (int i = 0; i < MEM_SIZE; i++) mem.push_back(Nan);
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
    const Binary3 ins = mem[IP];
    IP++;
    IP &= MASK_3D;
    return ins;
  }
  
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
    const Binary3 load_value = mem[full_addr];

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
      mem[i] = (full_addr == i && ADDR_COUNT == 0 && op.Bits() == 0b100) ?
	ab_value : mem[i];
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
    nandy.mem[i] = Rand3();
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

  TestNandy();
  
  return 0;
}
