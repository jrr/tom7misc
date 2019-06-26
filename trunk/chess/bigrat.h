
// Nonnegative "big rationals"

#ifndef __BIGRAT_H
#define __BIGRAT_H

#include <deque>
#include <string>

#include "../../cc-lib/base/logging.h"

// Nonnegative "big" rational. Denominator is always some power of the
// given base. Numerator is a sequence of digits in that base,
// in reverse order.
struct BigRat {
  // Zero.
  explicit BigRat(int base) : denom_exp(1), base(base) {}
  BigRat(int base, int num, int denom_exp) : denom_exp(denom_exp),
					     base(base) {
    CHECK(num >= 0);
    while (num > 0) {
      numer.push_back(num % base);
      num /= base;
    }
  }

  // Represent the same number, but multiplying the numerator and
  // denominator by b^e. So Shift(2) on 34/100 becomes 3400/10000.
  void Shift(int e) {
    denom_exp += e;
    for (int i = 0; i < e; i++) numer.push_front(0);
  }

  void Unzero() {
    while (!numer.empty() && numer.back() == 0)
      numer.pop_back();
  }

  void Validate() const;
  
  std::string ToString() const;

  std::deque<int> numer;
  int denom_exp = 1;

  // TODO: Could perhaps be template param.
  int base = 2; 
};

// Losing precision, drop digits from the numerator and powers
// from the denominator until the new denominator size is reached.
BigRat Truncate(const BigRat &a, int denom_exp);

BigRat PlusSameDenom(const BigRat &a, const BigRat &b);

// Multiply a by a small rational s/(B^y)
BigRat Scale(const BigRat &a, int s, int y);

// a - b. The result may not be negative!
BigRat MinusSameDenom(const BigRat &a, const BigRat &b);

// Args must be unzeroed.
bool LessEq(const BigRat &a, const BigRat &b);

inline bool Less(const BigRat &a, const BigRat &b) {
  // a < b iff !(a >= b) aka !(b <= a)
  return !LessEq(b, a);
}

#endif
