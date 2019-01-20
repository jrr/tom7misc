
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
    CHECK(sub >= 0);
    c.numer.push_back(sub);
  }
  CHECK(carry == 0) << "result cannot be negative";
  c.Unzero();
  return c;
}

// Args must be unzeroed.
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
      
  z.Unzero();

  // It sucks that these numbers have to be so big.
  // We could avoid it if we just allowed LessEq on mixed
  // denominators. Shouldn't be too hard..?
  Big a(B, 0, 1);
  Big b(B, B, 1);
  a.Shift(z.denom_exp - 1);
  b.Shift(z.denom_exp - 1);

  vector<int> output;
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
    z.Shift(W);
    int prob_sum = 0;
    for (const auto &p : pmf) {
      Big wc = Scale(w, prob_sum, W); 
      Big a0 = PlusSameDenom(a, wc);
      prob_sum += p.second;
      Big wd = Scale(w, prob_sum, W);
      Big b0 = PlusSameDenom(a, wd);
      a0.Unzero();
      b0.Unzero();
      CHECK(a0.denom_exp == z.denom_exp &&
	    b0.denom_exp == z.denom_exp) <<
	a0.denom_exp << " / " << z.denom_exp << " / " <<
	b0.denom_exp;
      if (VERBOSE) {
	printf("[%c,%d] (sum %d):\n"
	       "a0: %s\n"
	       " z: %s\n"
	       "b0: %s\n",
	       p.first + 'a', p.second, prob_sum,
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
	a = a0;
	b = b0;

	if (H > 0) {
	  hist.pop_front();
	  hist.push_back(p.first);
	}

	goto next;
      }
      // a0 = b0;
    }
    printf("BAD a: %s\n", a.ToString().c_str());
    printf("BAD b: %s\n", b.ToString().c_str());
    CHECK(false) << "Nothing matched!!";
  next:;
  }
    
#if 0
  // Faster, but maybe buggy version?
  vector<int> output;
  for (int count = 0; count < num; count++) {
    vector<pair<int, int>> pmf = Predict(hist);

    CHECK(a.denom_exp == b.denom_exp);
    Big w = MinusSameDenom(b, a);
    int prob_sum = 0;
    a.Shift(W);
    Big a0 = a;
    z.Shift(W);
    for (const auto &p : pmf) {
      prob_sum += p.second;
      Big wps = Scale(w, prob_sum, W);
      Big b0 = PlusSameDenom(a, wps);
      a0.Unzero();
      b0.Unzero();
      CHECK(a0.denom_exp == z.denom_exp &&
	    b0.denom_exp == z.denom_exp) <<
	a0.denom_exp << " / " << z.denom_exp << " / " <<
	b0.denom_exp;
      if (true || VERBOSE) {
	printf("[%c,%d] (sum %d):\n"
	       "a0: %s\n"
	       " z: %s\n"
	       "b0: %s\n",
	       p.first + 'a', p.second, prob_sum,
	       a0.ToString().c_str(),
	       z.ToString().c_str(),
	       b0.ToString().c_str());
      }
      if (LessEq(a0, z) && Less(z, b0)) {
	output.push_back(p.first);
	printf("\n[[%c]]\n", p.first + 'a');
	fflush(stdout);
	a = a0;
	b = b0;

	if (H > 0) {
	  hist.pop_front();
	  hist.push_back(p.first);
	}

	goto next;
      }
      a0 = b0;
    }
    printf("BAD a: %s\n", a.ToString().c_str());
    printf("BAD b: %s\n", b.ToString().c_str());
    CHECK(false) << "Nothing matched!!";
  next:;
  }
#endif
    
  return output;
    
  /*
    a = 0
    b = 1
    z is the current rational
    while symbols remain..
    w = b - a
    prob_sum = 0
    a0 = a;
    for ((sym, prob) : pmf)
    prob_sum += prob
    // this could be b0 = a0 + w*prob, I think? should be faster?
    b0 = a + w*prob_sum
    if (a0 <= z && z < b0) {
    emit sym;
    a = a0
    b = b0
    break;
    }
    a0 = b0
    }
  */
}
  
void ArithEncoder::Encode(const vector<int> &symbols) {
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
    CHECK_EQ(a.denom_exp, b.denom_exp);

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
	    printf(" ... failed @%d/%d\n", (int)i, decoded.size());
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
  
  /*
    
	vector<int> decoded = Decode(start_symbols, a, symbols.size() - H);
    printf("Decoding took: %.2fs\n", decode_timer.MS() / 1000.0);
    fflush(stdout);
    for (int i = 0; i < decoded.size(); i++) {
      CHECK_EQ(symbols[i + H], decoded[i]);
    }
  */
}

vector<pair<int, int>> ArithEncoder::Discretize(
    const vector<double> &raw_out) {
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
    
  // Normalize all values so that they are in [0,1]. They
  // can have any value to start; negative is common...
  double minv = output[0].second, maxv = output[0].second;
  for (int i = 1; i < output.size(); i++) {
    minv = std::min(output[i].second, minv);
    maxv = std::max(output[i].second, maxv);
  }

  double norm = 1.0 / (maxv - minv);
  double sum = 1.0;
  for (int i = 0; i < output.size(); i++) {
    double d = output[i].second;
    d = (d - minv) * norm;
    sum += d;
    output[i].second = d;
  }

  // Now all of the values are in [0,1] and sum to sum. Assign a
  // discrete probability to each, and make sure that none is zero,
  // because we need to be able to encode any symbol even if we
  // really messed up the prediction! We want an easy procedure that
  // we can replicate in javascript, so we go from lowest
  // probability (end of array) to highest, and round down (!) to
  // an integer (but a minimum of 1). The remaining mass is always
  // assigned to the largest bucket, which comes last. This means
  // that rounding error is "stolen" from this one (but also that
  // error from rounding down is donated).
  // XXX: In some distributions, this could end up being non-monotonic,
  // in that so much is stolen from the largest bucket that it is no
  // longer the largest!

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
    if (r < 1.0) {
      mass_left--;
      discrete_out[i] = {sym, 1};
    } else {
      int d = (int)r;
      CHECK(d >= 1);
      mass_left -= d;
      discrete_out[i] = {sym, d};
    }
  }
  // Always assign the remainder to the biggest (first) bucket.
  CHECK(mass_left > 0);
  discrete_out[0] = {output[0].first, mass_left};
  mass_left = 0;

  return discrete_out;

}
