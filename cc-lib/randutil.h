#ifndef _CC_LIB_RANDUTIL_H
#define _CC_LIB_RANDUTIL_H

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <vector>
#include <utility>

#include "arcfour.h"

using uint8 = uint8_t;
using uint16 = uint16_t;
using uint64 = uint64_t;
using uint32 = uint32_t;

// Creates another random stream, seeded by (and consuming some)
// of the input. Supplying a different n yields a different stream,
// which can be used to create fan-out in parallel.
//
// Caller owns new-ly allocated pointer.
inline ArcFour *Substream(ArcFour *rc, uint32 n) {
  std::vector<uint8> buf;
  buf.resize(64);
  for (int i = 0; i < 4; i++) {
    buf[i] = n & 255;
    n >>= 8;
  }

  for (int i = 4; i < 64; i++) {
    buf[i] = rc->Byte();
  }

  ArcFour *nrc = new ArcFour(buf);
  nrc->Discard(256);
  return nrc;
}

// In [0, 1].
// Note that this approach samples uniformly from
// the interval, but loses precision. Consider using
// RandDouble and then converting to float if precision
// is important.
inline float RandFloat(ArcFour *rc) {
  uint32 uu = 0U;
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  return (float)((uu   & 0x7FFFFFFF) / 
                 (double)0x7FFFFFFF);
};

inline double RandDouble(ArcFour *rc) {
  uint64 uu = 0U;
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  // PERF: Maybe could be multipling by the inverse?
  // It's a constant.
  return ((uu &   0x3FFFFFFFFFFFFFFFULL) / 
          (double)0x3FFFFFFFFFFFFFFFULL);
};

// Sample in [0, 1).
inline double RandDoubleNot1(ArcFour *rc) {
  for (;;) {
    double d = RandDouble(rc);
    if (d < 1.0) return d;
  }
}

inline uint64 Rand64(ArcFour *rc) {
  uint64 uu = 0ULL;
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  return uu;
};

inline uint32 Rand32(ArcFour *rc) {
  uint32 uu = 0ULL;
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  return uu;
};

inline uint16 Rand16(ArcFour *rc) {
  uint16 uu = 0ULL;
  uu = rc->Byte() | (uu << 8);
  uu = rc->Byte() | (uu << 8);
  return uu;
};

// Generate uniformly distributed numbers in [0, n - 1].
// n must be greater than or equal to 2.
inline uint64 RandTo(ArcFour *rc, uint64 n) {
  // We use rejection sampling, as is standard, but with
  // a modulus that's the next largest power of two. This
  // means that we succeed half the time (worst case).
  //
  // First, compute the mask. Note that 2^k will be 100...00
  // and so 2^k-1 is 011...11. This is the mask we're looking
  // for. The input may not be a power of two, however. Make
  // sure any 1 bit is propagated to every position less
  // significant than it.
  // 
  // This ought to reduce to a constant if the argument is
  // a compile-time constant.
  uint64 mask = n - 1;
  mask |= mask >> 1;
  mask |= mask >> 2;
  mask |= mask >> 4;
  mask |= mask >> 8;
  mask |= mask >> 16;
  mask |= mask >> 32;

  // Now, repeatedly generate random numbers, modulo that
  // power of two.

  // Depending on how big n is, we may not need to generate 8 random
  // bytes! PERF: I only do one test here, but we could try to
  // distinguish all 8 if we wanted, or just use a loop. Benchmark.
  if (mask & ~0xFFFF) {
    for (;;) {
      const uint64 x = Rand64(rc) & mask;
      if (x < n) return x;
    }
  } else {
    // 16-bit
    for (;;) {
      const uint64 x = Rand16(rc) & mask;
      if (x < n) return x;
    }
  }
}

// As above, but for 32-bit ints.
inline uint32 RandTo32(ArcFour *rc, uint32 n) {
  uint32 mask = n - 1;
  mask |= mask >> 1;
  mask |= mask >> 2;
  mask |= mask >> 4;
  mask |= mask >> 8;
  mask |= mask >> 16;

  // Now, repeatedly generate random numbers, modulo that
  // power of two.

  // PERF: If the number is small, we only need Rand16, etc.
  for (;;) {
    const uint32 x = Rand32(rc) & mask;
    if (x < n) return x;
  }
}

// Permute the elements of the array uniformly at random.
template<class T>
static void Shuffle(ArcFour *rc, std::vector<T> *v) {
  if (v->size() <= 1) return;
  for (uint64 i = v->size() - 1; i >= 1; i--) {
    uint64 j = RandTo(rc, i + 1);
    if (i != j) {
      std::swap((*v)[i], (*v)[j]);
    }
  }
}

// Generates two at once, so needs some state.
struct RandomGaussian {
  bool have = false;
  double next = 0;
  ArcFour *rc = nullptr;
  explicit RandomGaussian(ArcFour *rc) : rc(rc) {}
  double Next() {
    if (have) {
      have = false;
      return next;
    } else {
      double v1, v2, sqnorm;
      // Generate a non-degenerate random point in the unit circle by
      // rejection sampling.
      do {
        v1 = 2.0 * RandDouble(rc) - 1.0;
        v2 = 2.0 * RandDouble(rc) - 1.0;
        sqnorm = v1 * v1 + v2 * v2;
      } while (sqnorm >= 1.0 || sqnorm == 0.0);
      double multiplier = sqrt(-2.0 * log(sqnorm) / sqnorm);
      next = v2 * multiplier;
      have = true;
      return v1 * multiplier;
    }
  }
};

// If you need many, RandomGaussian will be twice as fast.
inline double OneRandomGaussian(ArcFour *rc) {
  return RandomGaussian{rc}.Next();
}

// Adapted from numpy, based on Marsaglia & Tsang's method.
// Please see NUMPY.LICENSE.
struct RandomGamma {
  explicit RandomGamma(ArcFour *rc) : rc(rc), rg(rc) {}
  static constexpr double one_third = 1.0 / 3.0;
  
  double Exponential() {
    return -log(1.0 - RandDoubleNot1(rc));
  }
  
  double Next(double shape) {
    if (shape == 1.0) {
      return Exponential();
    } else if (shape < 1.0) {
      const double one_over_shape = 1.0 / shape;
      for (;;) {
        const double u = RandDoubleNot1(rc);
        const double v = Exponential();
        if (u < 1.0 - shape) {
          const double x = pow(u, one_over_shape);
          if (x <= v) {
            return x;
          }
        } else {
          const double y = -log((1.0 - u) / shape);
          const double x = pow(1.0 - shape + shape * y, one_over_shape);
          if (x <= v + y) {
            return x;
          }
        }
      }
    } else {
      const double b = shape - one_third;
      const double c = 1.0 / sqrt(9.0 * b);
      for (;;) {
        double x, v;
        do {
          x = rg.Next();
          v = 1.0 + c * x;
        } while (v <= 0.0);

        const double v_cubed = v * v * v;
        const double x_squared = x * x;
        const double u = RandDoubleNot1(rc);
        if (u < 1.0 - 0.0331 * x_squared * x_squared ||
            log(u) < 0.5 * x_squared + b * (1.0 - v_cubed - log(v_cubed))) {
          return b * v_cubed;
        }
      }
    }
  }
  
  ArcFour *rc = nullptr;
  RandomGaussian rg;
};

inline double OneRandomGamma(ArcFour *rc, double shape) {
  return RandomGamma(rc).Next(shape);
}

// Reminder: Beta(a, b) gives the probability distribution
// when we have 'a' successful trials and 'b' unsuccessful
// trials. (The expected value is a/(a + b)).
inline double RandomBeta(ArcFour *rc, double a, double b) {
  if (a <= 1.0 && b <= 1.0) {
    for (;;) {
      const double u = RandDoubleNot1(rc);
      const double v = RandDoubleNot1(rc);
      const double x = pow(u, 1.0 / a);
      const double y = pow(v, 1.0 / b);
      const double x_plus_y = x + y;
      if (x_plus_y <= 1.0) {
        if (x_plus_y > 0.0) {
          return x / x_plus_y;
        } else {
          double log_x = log(u) / a;
          double log_y = log(v) / b;
          const double log_m = log_x > log_y ? log_x : log_y;
          log_x -= log_m;
          log_y -= log_m;

          return exp(log_x - log(exp(log_x) + exp(log_y)));
        }
      }
    }
  } else {
    RandomGamma rg(rc);
    const double ga = rg.Next(a);
    const double gb = rg.Next(b);
    return ga / (ga + gb);
  }
}

#endif
