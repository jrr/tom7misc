
#include <string>
#include <deque>

#include "../../cc-lib/base/logging.h"
#include "../../cc-lib/base/stringprintf.h"

#include "bigrat.h"

#define VERBOSE false

using namespace std;

void BigRat::Validate() const {
  CHECK(base > 0) << ToString();
  CHECK(denom_exp > 0) << ToString();
  for (auto n : numer) {
    CHECK(n >= 0 && n < base) << n << " / " << ToString();
  }
}

string BigRat::ToString() const {
  string s;
  if (base == 10) {
    if (numer.size() > denom_exp) {
      // We allow 10^k/10^k to represent e.g. the upper bound in [0, 1).
      auto Is1 =
	[this]() {
	  if (numer.size() != denom_exp + 1)
	    return false;
	  if (numer.back() != 1)
	    return false;
	  for (int i = 0; i < numer.size() - 1; i++)
	    if (numer[i] != 0)
	      return false;
	  return true;
	};
      if (Is1()) {
	// "exp" zeroes?
	s = "1.0";
	return s;
      } else {
	s = "ILL-FORMED ";
	// Fall through to general case.
      }
    } else {
      s = "0.";
      for (int i = denom_exp - 1; i >= 0; i--) {
	int d = i < numer.size() ? numer[i] : 0;
	if (d < 0 || d > 9) {
	  StringAppendF(&s, "[%d]", d);
	} else {
	  StringAppendF(&s, "%d", d);
	}
      }
      // necessary?
      StringAppendF(&s, " / %d^%d", base, denom_exp);

      StringAppendF(&s, "  *aka*  ");
      // return s;
    }
  } 
  // TODO: also allow base 100
    
  // Note: May already have something like "ILL-FORMED " in s.

  for (int i = numer.size() - 1; i >= 0; i--)
    StringAppendF(&s, "%d,", numer[i]);
  StringAppendF(&s, " / %d^%d", base, denom_exp);
  return s;
}

BigRat PlusSameDenom(const BigRat &a, const BigRat &b) {
  CHECK(a.denom_exp == b.denom_exp) << a.denom_exp << " != " << b.denom_exp;
  CHECK(a.base == b.base);
  BigRat c(a.base);
  c.denom_exp = a.denom_exp;
  int carry = 0;
  for (int i = 0; i < std::max(a.numer.size(), b.numer.size()); i++) {
    int da = (i < a.numer.size()) ? a.numer[i] : 0;
    int db = (i < b.numer.size()) ? b.numer[i] : 0;
    int sum = da + db + carry;
    c.numer.push_back(sum % c.base);
    carry = sum / c.base;
  }
  if (carry != 0) c.numer.push_back(carry);
  return c;
}

BigRat Truncate(const BigRat &a, int denom_exp) {
  CHECK(a.denom_exp >= denom_exp);
  BigRat r = a;
  while (r.denom_exp > denom_exp) {
    if (!r.numer.empty()) r.numer.pop_front();
    r.denom_exp--;
  }
  return r;
}

// Multiply a by a small rational s/(B^y)
// PERF: Can be done more efficiently, of course!
BigRat Scale(const BigRat &a, int s, int y) {
  CHECK(s >= 0);

  // a/(B^x) * s/(B^y) = (a * s)/(B^(x + y))
  // First, just get a * s, by repeated addition(!)
  // zero, with same denominator

  BigRat r(a.base, 0, a.denom_exp);
  /*
  while (s--)
    r = PlusSameDenom(r, a);
  */
  int carry = 0;
  for (int i = 0; i < a.numer.size(); i++) {
    int val = a.numer[i] * s + carry;
    r.numer.push_back(val % r.base);
    carry = val / r.base;
  }

  while (carry != 0) {
    r.numer.push_back(carry % r.base);
    carry /= r.base;
  }
  
  // Now divide by B^y.
  r.denom_exp += y;
      
  return r;
}

// a - b. The result may not be negative!
BigRat MinusSameDenom(const BigRat &a, const BigRat &b) {
  CHECK(a.denom_exp == b.denom_exp);
  CHECK(a.base == b.base);
  BigRat c(a.base);
  c.denom_exp = a.denom_exp;

  // Here the carry is <= 0, but we have to use it up by
  // the time we get to the end of the digits!
  int carry = 0;
  for (int i = 0; i < std::max(a.numer.size(), b.numer.size()); i++) {
    int da = (i < a.numer.size()) ? a.numer[i] : 0;
    int db = (i < b.numer.size()) ? b.numer[i] : 0;
    int sub = da - db + carry;
    carry = 0;
    if (sub < 0) {
      sub += a.base;
      carry--;
    }
    CHECK(sub >= 0) << "\n" << a.ToString() << "\n" << b.ToString();
    c.numer.push_back(sub);
  }
  CHECK(carry == 0) << "result cannot be negative";
  c.Unzero();
  return c;
}

// For arbitrary values in same base.
bool LessEq(const BigRat &a, const BigRat &b) {
  CHECK(a.base == b.base);

  // Just think of the numerator digits (as normally presented; left
  // to right.)
  // If the two denominators are the same, and the numerators are
  // zero padded (so, the same length) then this is just lex
  // comparison.
  //
  // we can make the denominators the same by shifting.

  // This part is the same as shifting the one with the smaller
  // denominator to match.
  // pad is the number of zeroes added on the right.
  int right_pad_a = 0, right_pad_b = 0;
  if (a.denom_exp > b.denom_exp) right_pad_b = a.denom_exp - b.denom_exp;
  else right_pad_b = a.denom_exp - b.denom_exp;

  int alen = a.numer.size() + right_pad_a;
  int blen = b.numer.size() + right_pad_b;
  int digits = std::max(alen, blen);
  
  // Now pad zeroes on the left. This is independent of the decision
  // above. (PERF: if we're adding zero padding, then it is LESS
  // unless the other one also has zeroes there..)

  int left_pad_a = digits - alen;
  int left_pad_b = digits - blen;

  /*
  printf("LESS():\n"
	 "a: %s\n"
	 "b: %s\n"
	 "left: %d, %d; right: %d; digits: %d\n",
	 a.ToString().c_str(),
	 b.ToString().c_str(),
	 left_pad_a, left_pad_b,
	 right_pad_a, right_pad_b,
	 digits);
  */

  // Get the ith digit. 
  auto Get =
    [](const BigRat &g, int lpad, int rpad, int idx) {
      /*
      printf("Get %d,%d [%d] from:\n",
	     lpad, rpad, idx);
      for (int x : g.numer)
	printf("%d, ", x);
      printf("\n");
      */

      // Idx is given from left to right, but digits are actually
      // stored in reverse order. First, remove any left padding.
      if (idx < lpad) return 0;
      idx -= lpad;

      int ridx = g.numer.size() - 1 - idx;
      // This is any right padding.
      if (ridx < 0) return 0;
      CHECK(ridx < g.numer.size());
      return g.numer[ridx];
    };

  /*
  for (int i = 0; i < digits; i++) {
    int aa = Get(a, left_pad_a, right_pad_a, i);
    int bb = Get(b, left_pad_b, right_pad_b, i);
    printf("[%d] %d vs %d\n", i, aa, bb);
  }
  */
  
  for (int i = 0; i < digits; i++) {
    int aa = Get(a, left_pad_a, right_pad_a, i);
    int bb = Get(b, left_pad_b, right_pad_b, i);
    if (aa != bb) return aa < bb;
  }
  // Equal.
  return true;
}
