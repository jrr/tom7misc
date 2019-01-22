
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <deque>

#include "../../cc-lib/threadutil.h"
#include "../../cc-lib/randutil.h"
#include "../../cc-lib/arcfour.h"
#include "../../cc-lib/base/logging.h"
#include "../../cc-lib/base/stringprintf.h"
#include "../../cc-lib/util.h"

#include "timer.h"
#include "arith.h"

#define VERBOSE false

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

void Big::Validate() const {
  CHECK(base > 0) << ToString();
  CHECK(denom_exp > 0) << ToString();
  for (auto n : numer) {
    CHECK(n >= 0 && n < base) << n << " / " << ToString();
  }
}

string Big::ToString() const {
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

Big PlusSameDenom(const Big &a, const Big &b) {
  CHECK(a.denom_exp == b.denom_exp) << a.denom_exp << " != " << b.denom_exp;
  CHECK(a.base == b.base);
  Big c(a.base);
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

Big Truncate(const Big &a, int denom_exp) {
  CHECK(a.denom_exp >= denom_exp);
  Big r = a;
  while (r.denom_exp > denom_exp) {
    if (!r.numer.empty()) r.numer.pop_front();
    r.denom_exp--;
  }
  return r;
}

// Multiply a by a small rational s/(B^y)
// PERF: Can be done more efficiently, of course!
Big Scale(const Big &a, int s, int y) {
  CHECK(s >= 0);

  // a/(B^x) * s/(B^y) = (a * s)/(B^(x + y))
  // First, just get a * s, by repeated addition(!)
  // zero, with same denominator

  Big r(a.base, 0, a.denom_exp);
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
Big MinusSameDenom(const Big &a, const Big &b) {
  CHECK(a.denom_exp == b.denom_exp);
  CHECK(a.base == b.base);
  Big c(a.base);
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

// Args must be unzeroed.
#if 0
bool LessEq(const Big &a, const Big &b) {
  CHECK(a.denom_exp == b.denom_exp);
  CHECK(a.base == b.base);

  // These need to be unzeroed!
  CHECK(a.numer.empty() || a.numer.back() != 0);
  CHECK(b.numer.empty() || b.numer.back() != 0);

  // Fewer digits means smaller!
  if (a.numer.size() < b.numer.size()) return true;
  if (b.numer.size() < a.numer.size()) return false;

  // Otherwise, it's just lex comparison.
  for (int i = a.numer.size() - 1; i >= 0; i--) {
    if (a.numer[i] == b.numer[i]) continue;
    else return a.numer[i] < b.numer[i];
  }
  // Equal.
  return true;
}
#endif

// For arbitrary values in same base.
bool LessEq(const Big &a, const Big &b) {
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
    [](const Big &g, int lpad, int rpad, int idx) {
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


vector<int> ArithEncoder::Decode(const vector<int> &start_symbols,
				 Big z,
				 int num) {
  CHECK(H == start_symbols.size()) << H << " / " << start_symbols.size();
  deque<int> hist;
  for (int i = 0; i < H; i++) {
    hist.push_back(start_symbols[i]);
  }

  if (VERBOSE) {
    printf("\n\nDecode hist: ");
    for (int i = 0; i < H; i++) {
      printf("%c", start_symbols[i] + 'a');
    }
    printf("\n");

    printf("Decoding %d symbols!\n", num);
  }
      
  // These have to stay the same denominator, but do not
  // have to agree with z (we only use LessEq).
  Big a(B, 0, 1);
  Big b(B, B, 1);
  // a.Shift(z.denom_exp - 1);
  // b.Shift(z.denom_exp - 1);

  vector<int> output;
  output.reserve(num);
  for (int count = 0; count < num; count++) {
    vector<pair<int, int>> pmf = Predict(hist);

    CHECK(a.denom_exp == b.denom_exp);
    Big w = MinusSameDenom(b, a);
    if (VERBOSE) {
      printf("\nDecode %dth\n"
	     "a: %s\n"
	     "z: %s\n"
	     "b: %s\n"
	     "w: %s\n",
	     count,
	     a.ToString().c_str(),
	     z.ToString().c_str(),
	     b.ToString().c_str(),
	     w.ToString().c_str());
    }
	
    a.Shift(W);
    // z.Shift(W);

    Big a0 = a;
    for (const auto &p : pmf) {
      // Big wc = Scale(w, prob_sum, W); 
      // Big a0 = PlusSameDenom(a, wc);

      // Big wd = Scale(w, prob_sum, W);
      // Big b0 = PlusSameDenom(a, wd);
      Big wi = Scale(w, p.second, W);
      Big b0 = PlusSameDenom(a0, wi);
	
      // a0.Unzero();
      /*
      b0.Unzero();
      CHECK(a0.denom_exp == z.denom_exp &&
	    b0.denom_exp == z.denom_exp) <<
	a0.denom_exp << " / " << z.denom_exp << " / " <<
	b0.denom_exp;
      */
      if (VERBOSE) {
	printf("[%c,%d]:\n"
	       "a0: %s\n"
	       " z: %s\n"
	       "b0: %s\n",
	       p.first + 'a', p.second,
	       a0.ToString().c_str(),
	       z.ToString().c_str(),
	       b0.ToString().c_str());
      }
      if (LessEq(a0, z) && Less(z, b0)) {
	output.push_back(p.first);
	if (VERBOSE) {
	  printf("\n[[%c]]\n", p.first + 'a');
	  fflush(stdout);
	}
	a = std::move(a0);
	b = std::move(b0);

	if (H > 0) {
	  hist.pop_front();
	  hist.push_back(p.first);
	}

	goto next;
      }
      // Shift starting point of interval to be previous
      // ending point.
      a0 = std::move(b0);
    }
    printf("BAD a: %s\n", a.ToString().c_str());
    printf("BAD b: %s\n", b.ToString().c_str());
    CHECK(false) << "Nothing matched!!";
  next:;
  }
        
  return output;
}
  
Big ArithEncoder::Encode(const vector<int> &symbols) {
  Timer encode_timer;
  for (int x : symbols) {
    CHECK(x >= 0 && x < nsymbols) << x << " / " << nsymbols;
  }

  CHECK(H <= symbols.size()) << H << " / " << symbols.size();
    
  // Initialize.
  deque<int> hist;
  printf("Hist: ");
  for (int i = 0; i < H; i++) {
    printf("%c", symbols[i] + 'a');
    hist.push_back(symbols[i]);
  }
  printf("\n");
    
  // (We assume the decoder is also told the starting
  // configuration. Could use anything agreed-upon here, like
  // zeroes.)

  // The encoded strings a, b.
  // These are initialized to 0/B and B/B, aka [0,1).
  // We maintain the invariant that the denominator exponents
  // are always the same.
  Big a(B, 0, 1);
  Big b(B, B, 1);

  for (int idx = H; idx < symbols.size(); idx++) {
    a.Validate();
    b.Validate();
    a.Unzero();
    b.Unzero();
    CHECK_EQ(a.denom_exp, b.denom_exp);
    CHECK(Less(a, b));
    
    // PERF: Should be safe to reduce the fraction if both
    // numerators are divisible by the base? This would
    // represent the same number, and probably does happen.
    // But 
      
    int actual = symbols[idx];
    if (VERBOSE) {
      printf("Encoding [[%c]]\n", actual + 'a');
      printf("----- ");
      for (int s : hist) {
	printf("%c", (s + 'a'));
      }
      printf(" -----\n");
    }
	
    vector<pair<int, int>> pmf = Predict(hist);

    if (VERBOSE) {
      printf(" == \n");
      for (const auto &p : pmf) {
	printf("%c %d%s\n", p.first + 'a', p.second,
	       (actual == p.first) ? " <-" : "");
	if (actual == p.first) break;
      }
    }
      
    // Find c, which is the sum of mass we skip over to get to
    // the symbol we want to output. We treat them as being
    // arranged in sorted order (pmf):
    int cnum = 0, dnum = 0;
    for (const auto &p : pmf) {
      if (p.first == actual) {
	CHECK(p.second > 0) << "Must have positive probability!";
	// The interval [c, d) comprises the mass assigned to
	// the expected character.
	dnum = cnum + p.second;
	break;
      } else {
	cnum += p.second;
      }
    }

    // Here the denominator exponent is W.
    // Big c(B, cnum, W);
    // Big d(B, dnum, W);

    if (VERBOSE) {
      printf("a: %s\n", a.ToString().c_str());
      printf("b: %s\n", b.ToString().c_str());
	
      printf("c = %d/%d^%d, d = %d/%d^%d\n",
	     cnum, B, W, dnum, B, W);
    }
      
    // width of the interval.
    Big w = MinusSameDenom(b, a);
    if (VERBOSE) {
      printf("w: %s\n", w.ToString().c_str());
    }
	     
    // now we want
    // a = a + w * c
    // which rs expands to like
    //  = a.numer/(B^a.denom_exp) + w.numer/(B^w.denom_exp) * cnum/(B^W)
    //  = a.numer/(B^a.denom_exp) + (w.numer * cnum)/(B^(w.denom_exp + W))

    // These will represent the same number, but with new exponent.
    // Prepares for adding to w * c, which will be in this base.
    a.Shift(W);
    // PERF b is dead
    b.Shift(W);

    // Multiply be these small values. This will result in compatible
    // denominators for a, b.
    Big cw = Scale(w, cnum, W);
    Big dw = Scale(w, dnum, W);

    CHECK_EQ(cw.denom_exp, a.denom_exp);
    CHECK_EQ(dw.denom_exp, a.denom_exp);

    b = PlusSameDenom(a, dw);
    // Note this overwrites a, so must come after the previous line,
    // which is also based on a!
    a = PlusSameDenom(a, cw);

    if (VERBOSE) {
      printf("Yielding..\n");
      printf("cw: %s\n", cw.ToString().c_str());
      printf("dw: %s\n", dw.ToString().c_str());
      printf("a: %s\n", a.ToString().c_str());
      printf("b: %s\n", b.ToString().c_str());
    }

    if (H > 0) {
      hist.pop_front();
      hist.push_back(actual);
    }
  }

  if (a.numer.size() < 1000) {
    printf("Final a: %s\n", a.ToString().c_str());
    printf("Final b: %s\n", b.ToString().c_str());
  }
      
  if (a.numer.size() == b.numer.size()) {
    printf("Same size numerator: %d\n",
	   (int)a.numer.size());
    int drop_zeroes = 0;
    while (drop_zeroes < a.numer.size() &&
	   a.numer[drop_zeroes] == 0 &&
	   b.numer[drop_zeroes] == 0)
      drop_zeroes++;

    // Drop em!
    a.numer.erase(a.numer.begin(), a.numer.begin() + drop_zeroes);
    a.denom_exp -= drop_zeroes;
    b.numer.erase(b.numer.begin(), b.numer.begin() + drop_zeroes);
    b.denom_exp -= drop_zeroes;

    printf("Dropped %d zeroes, yielding %d\n",
	   drop_zeroes,
	   (int)a.numer.size());
    // PERF: We can safely drop trailing zeroes by just reducing the
    // denominator. Right?
    int shared_prefix_size = 0;
    for (int i = a.numer.size() - 1; i >= 0; i--) {
      if (a.numer[i] != b.numer[i]) {
	break;
      }
      shared_prefix_size++;
    }
    printf("Length of shared prefix: %d\n", shared_prefix_size);
  } else {
    printf("Not even the same size numerator! %d vs %d\n",
	   (int)a.numer.size(), (int)b.numer.size());
  }

  printf("Encoding took: %.2fs\n", encode_timer.MS() / 1000.0);
  fflush(stdout);
    
  vector<int> start_symbols;
  for (int i = 0; i < H; i++)
    start_symbols.push_back(symbols[i]);

  // This is not the best way to do it! There
  // may be a shorter rational somewhere in this interval.
  // Compute (a+b) / 2 as 5(a/10 + b/10)
  CHECK(B % 2 == 0);
  Big atenth = Scale(a, 1, B);
  Big btenth = Scale(b, 1, B);
  Big midpoint = Scale(PlusSameDenom(a, b), B / 2, 1);
  
  // Now try to find a short prefix of a that decodes correctly.
  // [lower_bound, upper_bound).
  // Invariant is that something below upper_bound works,
  // but nothing below lower_bound works.
  int lower_bound = 0, upper_bound = a.denom_exp + 1;
  while (lower_bound < upper_bound) {
    int w = upper_bound - lower_bound;
    int t = lower_bound + (w >> 1);
      
    auto DecodesCorrectly =
      [this, &symbols, &start_symbols, lower_bound, upper_bound, t](
	  const Big &z) {
	printf("[%d,%d] try %d ", lower_bound, upper_bound, t);
	fflush(stdout);
	Timer trydecode_timer;
	vector<int> decoded = Decode(start_symbols, z, symbols.size() - H);
	printf("Decoding took: %.2fs\n",
	       trydecode_timer.MS() / 1000.0);
	for (int i = 0; i < decoded.size(); i++) {
	  if (symbols[i + H] != decoded[i]) {
	    printf(" ... failed @%d/%d\n", (int)i, (int)decoded.size());
	    return false;
	  }
	}
	printf(" ... and worked!\n");
	fflush(stdout);
	return true;
      };

    Big z = Truncate(midpoint, t);
    if (DecodesCorrectly(z)) {
      upper_bound = t;
    } else {
      // Skip the point itself, which we know doesn't work.
      lower_bound = t + 1;
    }
  }

  printf("Finished with lb = ub = %d\n", lower_bound);

  {
    // And sanity check.
    Big z = Truncate(midpoint, lower_bound);
    z.Unzero();
    Timer decode_timer;
    vector<int> decoded = Decode(start_symbols, a, symbols.size() - H);
    printf("Final decoding took: %.2fs\n", decode_timer.MS() / 1000.0);
    fflush(stdout);
    for (int i = 0; i < decoded.size(); i++) {
      CHECK_EQ(symbols[i + H], decoded[i]);
    }
    return z;
  }
}

// Make the values in v all non-negative.
void ArithEncoder::Norm(vector<double> *v) {
  if (v->empty()) return;
  // Shift all values so that they are at least 0. We'll normalize
  // against the sum below.
  double minv = (*v)[0];
  for (int i = 1; i < v->size(); i++) {
    minv = std::min((*v)[i], minv);
  }
  for (int i = 0; i < v->size(); i++) {
    (*v)[i] -= minv;
  }
}

vector<pair<int, int>> ArithEncoder::Discretize(
    const vector<double> &raw_out,
    bool allow_zero) {
  vector<pair<int, double>> output;
  output.reserve(nsymbols);
  CHECK_EQ(raw_out.size(), nsymbols);
  for (int i = 0; i < nsymbols; i++) {
    output.emplace_back(i, raw_out[i]);
  }

  for (const auto &p : output)
    CHECK(!std::isnan(p.second));
    
  // In order to simplify discretizing the probabilities
  // later, we sort this in descending order. Unknown
  // whether sorting affects encoding? It might make
  // a difference?
  std::sort(output.begin(), output.end(),
	    [](const pair<int, double> &a,
	       const pair<int, double> &b) {
	      // Stable in case of ties.
	      if (b.second == a.second)
		return a.first < b.first;

	      // descending by probability
	      return b.second < a.second;
	    });
    
  // Now, discretize the probabilities into units of 1/(B^W).
  // Some experimentation here is worthwhile, including
  // setting minimum probabilities.

  double sum = 0.0;
  for (const auto &p : output) {
    CHECK(p.second >= 0.0) << p.second;
    CHECK(!std::isnan(p.second));
    sum += p.second;
  }
  CHECK(!std::isnan(sum));
  
  // Now all of the values are non-negative and sum to sum. Assign a
  // discrete probability to each, and make sure that none is zero,
  // because we need to be able to encode any symbol even if we really
  // messed up the prediction! We want an easy procedure that we can
  // replicate in javascript, so we go from lowest probability (end of
  // array) to highest, and round down (!) to an integer (but a
  // minimum of 1). The remaining mass is always assigned to the
  // largest bucket, which comes last. This means that rounding error
  // is "stolen" from this one (but also that error from rounding down
  // is donated). XXX: In some distributions, this could end up being
  // non-monotonic, in that so much is stolen from the largest bucket
  // that it is no longer the largest!

  int mass_left = ipow(B, W);
  double total_mass = mass_left;
    
  // TODO: Experiment with rounding to the closest, etc. Need to
  // keep this simple though, because it has to be replicated in JS.
  vector<pair<int, int>> discrete_out;
  discrete_out.resize(nsymbols);
  for (int i = output.size() - 1; i > 0; i--) {
    CHECK(mass_left > 0);
    int sym = output[i].first;
    double r = (output[i].second / sum) * total_mass;
    printf("%d. sym %d  (%.6f / %.6f) * %.6f = %.6f)\n",
	   i, sym, output[i].second, sum, total_mass, r);
    if (allow_zero && r == 0.0) {
      printf("Allowing zero probability for sym %d\n", sym);
      discrete_out[i] = {sym, 0};
    } else if (r < 1.0) {
      mass_left--;
      discrete_out[i] = {sym, 1};
    } else {
      int d = (int)round(r); // (int)r;  
      CHECK(d >= 1) << d;
      mass_left -= d;
      discrete_out[i] = {sym, d};
    }
  }
  // Always assign the remainder to the biggest (first) bucket.
  CHECK(mass_left > 0);
  printf("Symbol %d gets the rest, %d (= %.6f) (would have been %.6f)\n",
	 output[0].first, mass_left, mass_left / (double)total_mass,
	 (output[0].second / sum) * total_mass);
  discrete_out[0] = {output[0].first, mass_left};
  mass_left = 0;
  
  return discrete_out;
}
