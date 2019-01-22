
#ifndef __ARITH_H
#define __ARITH_H

#include <vector>
#include <deque>
#include <string>

#include "../../cc-lib/base/logging.h"

inline int ipow(int base, int exponent) {
  int res = 1;
  while (exponent--)
    res *= base;
  return res;
}

// Nonnegative "big" rational. Denominator is always some power of the
// given base. Numerator is a sequence of digits in that base,
// in reverse order.
struct Big {
  // Zero.
  explicit Big(int base) : denom_exp(1), base(base) {}
  Big(int base, int num, int denom_exp) : denom_exp(denom_exp),
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
  
  string ToString() const;

  deque<int> numer;
  int denom_exp = 1;

  int base = 2; 
};

// Losing precision, drop digits from the numerator and powers
// from the denominator until the new denominator size is reached.
Big Truncate(const Big &a, int denom_exp);

Big PlusSameDenom(const Big &a, const Big &b);

// Multiply a by a small rational s/(B^y)
Big Scale(const Big &a, int s, int y);

// a - b. The result may not be negative!
Big MinusSameDenom(const Big &a, const Big &b);

// Args must be unzeroed.
bool LessEq(const Big &a, const Big &b);

inline bool Less(const Big &a, const Big &b) {
  // a < b iff !(a >= b) aka !(b <= a)
  return !LessEq(b, a);
}

// Arithmetic encoder, but needs Predict() function taking
// some history.
struct ArithEncoder {
  ArithEncoder(int H, int nsymbols, int B, int W) :
    H(H), nsymbols(nsymbols), B(B), W(W) {
    // Note: Should be possible to adapt this to use some power of B
    // here, but we don't need that for our purposes since B is large.
    CHECK(nsymbols <= B);
  }

  // If allow_zero is true, then an item with exactly 0.0 is treated
  // as impossible and given a probability of 0. It cannot be encoded.
  vector<pair<int, int>> Discretize(const vector<double> &out,
				    bool allow_zero);

  static void Norm(vector<double> *v);
  
  virtual vector<pair<int, int>> Predict(const deque<int> &hist) = 0;

  vector<int> Decode(const vector<int> &start_symbols,
		     Big z,
		     int num);
  
  Big Encode(const vector<int> &symbols);
  
  const int H, nsymbols, B, W;
};

#endif
