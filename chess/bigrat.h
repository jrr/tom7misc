
// Nonnegative "big rationals"

#ifndef __BIGRAT_H
#define __BIGRAT_H

#include <deque>
#include <string>
#include <cstdint>

#include <gmp.h>

#include "../../cc-lib/base/logging.h"

// Nonnegative "big" rational. Denominator is always some power of the
// given base. Numerator is a sequence of digits in that base,
// in reverse order.
struct BigRat {
  // Zero.
  BigRat() {
    mpq_init(r);
  }
  BigRat(int64_t numer, int64_t denom) {
    CHECK(denom >= 0);
    mpq_set_ui(r, numer, denom);
    mpq_canonicalize(r);
  }

  // No copy / assignment supported.
  // (This can actually be done pretty easily with
  // mpq_set. but it is not free)
  BigRat(const BigRat &other) = delete;
  BigRat &operator =(const BigRat &other) = delete;
  
  std::string ToString() const;

  ~BigRat() {
    mpq_clear(r);
  }

  void PlusEq(const BigRat &b) {
    mpq_t tmp;
    mpq_init(tmp);
    mpq_add(tmp, r, b.r);
    mpq_swap(r, tmp);
    mpq_clear(tmp);
  }

  void MulEq(const BigRat &b) {
    mpq_t tmp;
    mpq_init(tmp);
    mpq_mul(tmp, r, b.r);
    mpq_swap(r, tmp);
    mpq_clear(tmp);
  }

  void ScaleBy(int64_t s) {
    BigRat sr{s, 1LL};
    MulEq(sr);
  }

  static bool LessEq(const BigRat &a, const BigRat &b) {
    const int ord = mpq_cmp(a.r, b.r);
    return ord <= 0;
  }

  static bool Less(const BigRat &a, const BigRat &b) {
    const int ord = mpq_cmp(a.r, b.r);
    return ord < 0;
  }

private:
  // Always canonicalized.
  mpq_t r;
};

#endif
