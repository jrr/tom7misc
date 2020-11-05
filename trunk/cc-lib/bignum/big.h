
// C++ wrappers for big integers and rationals by tom7 for cc-lib.

#ifndef _CC_LIB_BIGNUM_BIG_H
#define _CC_LIB_BIGNUM_BIG_H

#include "bigz.h"
#include "bign.h"
#include "bigq.h"

#include <algorithm>
#include <cstdint>
#include <string>
#include <utility>

struct BigInt {
  BigInt() : BigInt(0LL) {}
  explicit BigInt(int64_t n);
  explicit BigInt(const std::string &digits);
  
  // Value semantics with linear-time copies (like std::vector).
  BigInt(const BigInt &other);
  BigInt &operator =(const BigInt &other);
  BigInt &operator =(BigInt &&other);

  ~BigInt();
  
  std::string ToString(int base = 10) const;

  bool IsEven() const;
  bool IsOdd() const;
  
  static BigInt Negate(const BigInt &a);
  static BigInt Abs(const BigInt &a);
  static int Compare(const BigInt &a, const BigInt &b);
  static BigInt Plus(const BigInt &a, const BigInt &b);
  static BigInt Minus(const BigInt &a, const BigInt &b);
  static BigInt Times(const BigInt &a, const BigInt &b);
  static BigInt Div(const BigInt &a, const BigInt &b);
  static BigInt Mod(const BigInt &a, const BigInt &b);
  static BigInt Pow(const BigInt &a, uint64_t exponent);
  
  void Swap(BigInt *other);
  
private:
  friend class BigRat;
  // Takes ownership.
  // nullptr token here is just used to distinguish from the version
  // that takes an int64 (would be ambiguous with BigInt(0)).
  explicit BigInt(BigZ z, nullptr_t token) : bigz(z) {}
  
  // BigZ is a pointer to a bigz struct, which is the
  // header followed by digits.
  BigZ bigz = nullptr;
};


struct BigRat {
  // Zero.
  BigRat() : BigRat(0LL, 1LL) {}
  BigRat(int64_t numer, int64_t denom);
  BigRat(const BigInt &numer, const BigInt &denom);
  
  BigRat(const BigRat &other);
  BigRat &operator =(const BigRat &other);
  BigRat &operator =(BigRat &&other);

  ~BigRat();

  // In base 10.
  std::string ToString() const;
  // Only works when the numerator and denominator are small;
  // readily returns nan!
  double ToDouble() const;

  static int Compare(const BigRat &a, const BigRat &b);
  static BigRat Abs(const BigRat &a);
  static BigRat Div(const BigRat &a, const BigRat &b);
  static BigRat Inverse(const BigRat &a);
  static BigRat Times(const BigRat &a, const BigRat &b);
  static BigRat Negate(const BigRat &a);
  static BigRat Plus(const BigRat &a, const BigRat &b);
  static BigRat Minus(const BigRat &a, const BigRat &b);

  static BigRat ApproxDouble(double num, int64_t max_denom);

  void Swap(BigRat *other);
    
private:
  // Takes ownership.
  explicit BigRat(BigQ q) : bigq(q) {}
  // TODO: This is a pointer to a struct with two BigZs (pointers),
  // so it would probably be much better to just unpack it here.
  // bigq.cc is seemingly set up to do this by redefining some
  // macros in the EXTERNAL_BIGQ_MEMORY section of the header.
  BigQ bigq = nullptr;
};


// Implementations follow. These are all light wrappers around
// bigz/bigq functions, so inline makes sense.

BigInt::BigInt(int64_t n) : bigz(BzFromInteger(n)) { }

BigInt::BigInt(const BigInt &other) : bigz(BzCopy(other.bigz)) { }
BigInt &BigInt::operator =(const BigInt &other) {
  // Self-assignment does nothing.
  if (this == &other) return *this;
  BzFree(bigz);
  bigz = BzCopy(other.bigz);
  return *this;
}
BigInt &BigInt::operator =(BigInt &&other) {
  // We don't care how we leave other, but it needs to be valid (e.g. for
  // the destructor). Swap is a good way to do this.
  Swap(&other);
  return *this;
}

BigInt::~BigInt() {
  BzFree(bigz);
  bigz = nullptr;
}

void BigInt::Swap(BigInt *other) {
  std::swap(bigz, other->bigz);
}

BigInt::BigInt(const std::string &digits) {
  bigz = BzFromStringLen(digits.c_str(), digits.size(), 10, BZ_UNTIL_END);
}

std::string BigInt::ToString(int base) const {
  // Allocates a buffer.
  // Third argument forces a + sign for positive; not used here.
  BzChar *buf = BzToString(bigz, base, 0);
  std::string ret{buf};
  BzFreeString(buf);
  return ret;
}

bool BigInt::IsEven() const { return BzIsEven(bigz); }
bool BigInt::IsOdd() const { return BzIsOdd(bigz); }

BigInt BigInt::Negate(const BigInt &a) {
  return BigInt{BzNegate(a.bigz), nullptr};
}
BigInt BigInt::Abs(const BigInt &a) {
  return BigInt{BzAbs(a.bigz), nullptr};
}
// TODO: Overload <, etc. and <=>
int BigInt::Compare(const BigInt &a, const BigInt &b) {
  switch (BzCompare(a.bigz, b.bigz)) {
  case BZ_LT: return -1;
  case BZ_EQ: return 0;
  default:
  case BZ_GT: return 1;
  }
}
BigInt BigInt::Plus(const BigInt &a, const BigInt &b) {
  return BigInt{BzAdd(a.bigz, b.bigz), nullptr};
}
BigInt BigInt::Minus(const BigInt &a, const BigInt &b) {
  return BigInt{BzSubtract(a.bigz, b.bigz), nullptr};
}
BigInt BigInt::Times(const BigInt &a, const BigInt &b) {
  return BigInt{BzMultiply(a.bigz, b.bigz), nullptr};
}
// TODO: Quotrem via BzDivide
BigInt BigInt::Div(const BigInt &a, const BigInt &b) {
  return BigInt{BzDiv(a.bigz, b.bigz), nullptr};
}
// TODO: truncate, floor, ceiling round. what are they?

// TODO: Clarify mod vs rem?
BigInt BigInt::Mod(const BigInt &a, const BigInt &b) {
  return BigInt{BzMod(a.bigz, b.bigz), nullptr};
}

BigInt BigInt::Pow(const BigInt &a, uint64_t exponent) {
  return BigInt{BzPow(a.bigz, exponent), nullptr};
}

BigRat::BigRat(int64_t numer, int64_t denom) {
  // PERF This could avoid creating intermediate BigZ with
  // a new function inside bigq.
  BigInt n{numer}, d{denom};
  bigq = BqCreate(n.bigz, d.bigz);
}
BigRat::BigRat(const BigInt &numer, const BigInt &denom)
  : bigq(BqCreate(numer.bigz, denom.bigz)) {}

// PERF: Should have BqCopy so that we don't need to re-normalize.
BigRat::BigRat(const BigRat &other) :
  bigq(BqCreate(
	   BqGetNumerator(other.bigq),
	   BqGetDenominator(other.bigq))) {
}
BigRat &BigRat::operator =(const BigRat &other) {
  // Self-assignment does nothing.
  if (this == &other) return *this;
  BqDelete(bigq);
  bigq = BqCreate(BqGetNumerator(other.bigq),
		  BqGetDenominator(other.bigq));
  return *this;
}
BigRat &BigRat::operator =(BigRat &&other) {
  Swap(&other);
  return *this;
}

void BigRat::Swap(BigRat *other) {
  std::swap(bigq, other->bigq);
}

BigRat::~BigRat() {
  BqDelete(bigq);
  bigq = nullptr;
}

int BigRat::Compare(const BigRat &a, const BigRat &b) {
  switch (BqCompare(a.bigq, b.bigq)) {
  case BQ_LT: return -1;
  case BQ_EQ: return 0;
  default:
  case BQ_GT: return 1;
  }
}

BigRat BigRat::Abs(const BigRat &a) {
  return BigRat{BqAbs(a.bigq)};
}
BigRat BigRat::Div(const BigRat &a, const BigRat &b) {
  return BigRat{BqDiv(a.bigq, b.bigq)};
}
BigRat BigRat::Inverse(const BigRat &a) {
  return BigRat{BqInverse(a.bigq)};
}
BigRat BigRat::Times(const BigRat &a, const BigRat &b) {
  return BigRat{BqMultiply(a.bigq, b.bigq)};
}
BigRat BigRat::Negate(const BigRat &a) {
  return BigRat{BqNegate(a.bigq)};
}
BigRat BigRat::Plus(const BigRat &a, const BigRat &b) {
  BigQ res = BqAdd(a.bigq, b.bigq);
  return BigRat{res};
}
BigRat BigRat::Minus(const BigRat &a, const BigRat &b) {
  return BigRat{BqSubtract(a.bigq, b.bigq)};
}

std::string BigRat::ToString() const {
  // No forced +
  BzChar *buf = BqToString(bigq, 0);
  std::string ret{buf};
  BzFreeString(buf);
  return ret;
}

BigRat BigRat::ApproxDouble(double num, int64_t max_denom) {
return BigRat{BqFromDouble(num, max_denom)};
}

double BigRat::ToDouble() const {
  return BqToDouble(bigq);
}


#endif
