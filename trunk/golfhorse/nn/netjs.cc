
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

#include "network.h"

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

static constexpr bool VERBOSE = true;

int ipow(int base, int exponent) {
  int res = 1;
  while (exponent--)
    res *= base;
  return res;
}

// Nonnegative "big" rational. Denominator is always some power of an
// unspecified base. Numerator is a sequence of digits in that base,
// in reverse order.
struct Big {
  // Zero.
  Big(int base) : denom_exp(1), base(base) {}
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
  
  string ToString() const {
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

  deque<int> numer;
  int denom_exp = 1;

  int base = 2; 
};


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

bool Less(const Big &a, const Big &b) {
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

  virtual vector<pair<int, int>> Predict(const deque<int> &hist) = 0;

  vector<int> Decode(const vector<int> &start_symbols,
		     Big z,
		     int num) {
    CHECK(H == start_symbols.size()) << H << " / " << start_symbols.size();
    deque<int> hist;
    printf("\n\nDecode hist: ");
    for (int i = 0; i < H; i++) {
      printf("%c", start_symbols[i] + 'a');
      hist.push_back(start_symbols[i]);
    }
    printf("\n");

    printf("Decoding %d symbols!\n", num);
    
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

	  hist.pop_front();
	  hist.push_back(p.first);

	  goto next;
	}
	a0 = b0;
      }
      printf("BAD a: %s\n", a.ToString().c_str());
      printf("BAD b: %s\n", b.ToString().c_str());
      CHECK(false) << "Nothing matched!!";
    next:;
    }

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
  
  void Encode(const vector<int> &symbols) {
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
      printf("Encoding [[%c]]\n", actual + 'a');
      if (VERBOSE) {
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
      
      hist.pop_front();
      hist.push_back(actual);
    }

    printf("Final a: %s\n", a.ToString().c_str());
    printf("Final b: %s\n", b.ToString().c_str());

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

    vector<int> start_symbols;
    for (int i = 0; i < H; i++)
      start_symbols.push_back(symbols[i]);
    (void)Decode(start_symbols, b, symbols.size() - H);
  }
  
  const int H, nsymbols, B, W;
};

struct NNEncoder : public ArithEncoder {
  // H is the number of symbols in the history.
  // B is the base of the number system.
  // W is the width of rationalized probabilities (denominator becomes
  // B^W). W=1 does not make much sense (everything would get discretized
  // to 1/B) unless B is larger than the actual number of symbols.
  NNEncoder(const Network &net, int H,
	    int nsymbols, int B, int W) :
    ArithEncoder(H, nsymbols, B, W), net(net) {
  }

  // Returns a vector of pairs (happens to be in sorted order)
  // {symbol, mass} where each integral mass is > 0 and they
  // sum to B^W exactly.
  vector<pair<int, int>> Predict(const deque<int> &hist) override {
    CHECK(hist.size() == H);
    // Use doubles since this is what we have in JS.
    StimulationD stim{net};

    vector<double> *input = &stim.values[0];
    CHECK_EQ(input->size(), nsymbols * H);
    for (int i = 0; i < H; i++) {
      const int s = hist[i];
      CHECK((nsymbols * i) + s < input->size());
      (*input)[nsymbols * i + s] = 1.0;
    }
    
    ForwardStimulationD(net, &stim);

    vector<pair<int, double>> output;
    output.reserve(nsymbols);
    CHECK_EQ(stim.values.back().size(), nsymbols);
    for (int i = 0; i < nsymbols; i++) {
      output.emplace_back(i, stim.values.back()[i]);
    }

    for (const auto &p : output)
      CHECK(!std::isnan(p.second));

    /*
    if (VERBOSE) {
      printf(" (orig) \n");
      for (int i = 0; i < nsymbols; i++) {
	printf("%c %.6f\n", i + 'a',
	       stim.values.back()[i]);
      }
    }
    */
    
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

    /*
    if (VERBOSE) {
      printf(" (sorted) \n");
      for (const auto &p : output) {
	printf("%c %.6f\n", p.first + 'a', p.second);
      }
    }
    */
    
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
    // that rounding error is "stolen" from this one.
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
  
  const Network &net;
};

int ToSymbol(char c) {
  if (c >= 'a' && c <= 'z') return c - 'a';
  if (c == '\n') return 26;
  CHECK(false) << "Bad char " << c;
  return 0;
}

bool IsDense(int num_src_nodes,
	     int num_dst_nodes,
	     const Network::Layer &layer) {
  CHECK(layer.indices.size() ==
	num_dst_nodes * layer.indices_per_node);
  CHECK(layer.indices_per_node == num_src_nodes);

  // They should also be in sorted order. We could fix this
  // if not, the Network is expected to be constructed that
  // way anyway.
  for (int n = 0; n < num_dst_nodes; n++) {
    for (int i = 0; i < layer.indices_per_node; i++) {
      if (i != layer.indices[n * layer.indices_per_node + i]) return false;
    }
  }
  return true;
}

// For debugging, use a flat model. No history, base 10.
struct ExampleEncoder : public ArithEncoder {
  ExampleEncoder() : ArithEncoder(0, 3, 10, 2) {}

  vector<pair<int, int>> Predict(const deque<int> &hist) override {
    return {{0, 20},
	    {1, 40},
	    {2, 40}};
  }
};


int main(int argc, char **argv) {
#if 0
  // Test some math.
  printf("\n");
  Big a(10, 123, 3);
  Big b(10, 10, 1);
  Big c = PlusSameDenom(a, a);
  a.Shift(1);
  b.Shift(3);
  a.Unzero();
  b.Unzero();
  Big d(10, 199, 3);
  Big e = MinusSameDenom(c, d);
  Big f = Scale(e, 999, 3);
  e.Shift(3);
  printf("a: %s\n", a.ToString().c_str());
  printf("b: %s\n", b.ToString().c_str());
  printf("c: %s\n", c.ToString().c_str());
  printf("d: %s\n", d.ToString().c_str());
  printf("e: %s\n", e.ToString().c_str());
  printf("f: %s\n", f.ToString().c_str());
  CHECK(LessEq(a, b));
  CHECK(Less(a, b));
  CHECK(LessEq(a, a));
  
  CHECK(Less(f, e));
  CHECK(LessEq(f, e));
  CHECK(!Less(e, f));
  CHECK(!LessEq(e, f));
#endif

  ExampleEncoder example;
  example.Encode({2, 1, 0});

  return 0;

  std::unique_ptr<Network> net{Network::ReadNetworkBinary("net.val")};
  CHECK(net.get());
  
  // The network is dense, so we don't need the indices (nor inverted
  // indices.)
  for (int i = 0; i < net->layers.size(); i++) {
    const Network::Layer &layer = net->layers[i];
    CHECK(IsDense(net->num_nodes[i], net->num_nodes[i + 1], layer));
  }

  int64 num_floats = 0LL;
  
  // TODO...
  // In JavaScript, the main thing we care about is the compactness
  // of the encoding. For starts, just emit the decimal floats.
  string js = "N={n:[";
  // n=num_nodes array.
  for (int i = 0; i < net->num_nodes.size(); i++) {
    if (i != 0) js += ",";
    js += StringPrintf("%d", net->num_nodes[i]);
  }
  js += "],";
  if (VERBOSE) js += "\n";
  // Now the layers. indices_per_node is always the size of the
  // previous layer. Transfer function is always leaky_relu.
  // indices are the identity. So we have
  // num_nodes[l + 1] floats (biases), then
  // num_nodes[l + 1] * num_nodes[l] floats (weights)
  js += "l:[";
  for (int layer = 0; layer < net->num_layers; layer++) {
    js += "[";
    int ipn = net->layers[layer].indices_per_node;
    if (VERBOSE) js += StringPrintf("\n/* *** layer %d *** */\n", layer);
    if (VERBOSE) js += "\n /* biases */\n  ";
    for (const float b : net->layers[layer].biases)
      js += StringPrintf("%.9g,", b);
    num_floats += net->layers[layer].biases.size();
    if (VERBOSE) js += "\n /* weights */";
    for (int n = 0; n < net->num_nodes[layer + 1]; n++) {
      if (VERBOSE) js += StringPrintf("\n   /* node %d */\n   ", n);
      string ws;
      for (int i = 0; i < ipn; i++) {
	ws += StringPrintf("%.9g,",
			   net->layers[layer].weights[n * ipn + i]);
      }
      js += ws;
    }
    num_floats += net->num_nodes[layer + 1] * ipn;
    js += "],";
  }
  js += "]}\n";

  // printf("%s", js.c_str());

  // Obviously this shouldn't be part of the final output, but it
  // is good for playin' around.
  string dict = Util::ReadFile("../wordlist.asc");


  // Use the network to generate probability mass functions, and
  // feed those to arithmetic encoder.

  NNEncoder encoder{*net, 5, 27, 100, 1};
  auto MakeSymbols =
    [](const string &str) {
      vector<int> syms;
      for (char s : str) {
	syms.push_back(ToSymbol(s));
      }
      return syms;
    };

  dict.resize(8);
  vector<int> symbols = // MakeSymbols("antidisestablishmentarianism");
    MakeSymbols(dict);
  encoder.Encode(symbols);

  return 0;
  
  
  
  printf("Original dict size: %lld\n"
	 "Net: %lld floats. *2 = %lld, *3 = %lld, *4 = %lld.\n",
	 (int64)dict.size(),
	 num_floats, num_floats * 2LL, num_floats * 3LL,
	 num_floats * 4LL);

  dict = Util::Replace(dict, "\n", "\\n");
  
  string netcode = Util::ReadFile("netcode.js");
  // TODO: if not verbose, strip lines starting with // and
  // trailing whitespace!

  string evalcode = Util::ReadFile("evalcode.js");
  
  FILE *f = fopen("overfit.js", "wb");
  fprintf(f, "%s", js.c_str());
  fprintf(f, "let dict='%s'\n", dict.c_str());
  fprintf(f, "%s", netcode.c_str());
  fprintf(f, "%s", evalcode.c_str());
  // XXX need diffs, etc.
  fclose(f);
  
  return 0;
}

