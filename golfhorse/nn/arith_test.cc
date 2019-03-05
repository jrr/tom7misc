
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
#include "../horseutil.h"

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

// for a-z 0-9
static int ToSymbol(char c) {
  if (c >= 'a' && c <= 'z') return c - 'a';
  if (c >= '0' && c <= '9') return 26 + (c - '0');
  CHECK(false) << "Bad char " << c << " = " << (int)c;
  return 0;
}

static char ToChar(int i) {
  if (i < 26) return 'a' + i;
  else if (i < 36) return '0' + (i - 26);
  CHECK(false) << "Bad symbol " << i;
  return 0;
}

// TODO: Actually test this...
namespace {
struct FreqEncoder : public ArithEncoder {
  // No history; just use frequency table.
  // Careful: 4 is even too high for 32-bit integers.
  FreqEncoder(const string &word) : ArithEncoder(0, 26 + 10, 124, 3) {
    vector<int> counts(26 + 10, 0);
    for (char c : word) {
      int sym = ToSymbol(c);
      counts[sym]++;
    }

    vector<double> fracs;
    for (int count : counts) {
      fracs.push_back(count / (double)word.size());
    }
    
    // Prepare predictions vector.
    freqs = Discretize(fracs);
    // But sort by symbol to simplify decoding.
    std::sort(freqs.begin(), freqs.end(),
	      [](const pair<int, int> &a,
		 const pair<int, int> &b) {
		return a.first < b.first;
	      });

    for (int i = 0; i < freqs.size(); i++) {
      CHECK(freqs[i].first == i);
      printf("%c / %d = %.3f\n", ToChar(i),
	     freqs[i].second, freqs[i].second / (double)(ipow(B, W)));
    }
  }
  
  vector<pair<int, int>> Predict(const deque<int> &hist) override {
    return freqs;
  }

  vector<pair<int, int>> freqs;
};
}

static vector<int> MakeSymbols(const string &str) {
  vector<int> syms;
  for (char s : str) {
    syms.push_back(ToSymbol(s));
  }
  return syms;
}

int main(int argc, char **argv) {
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

  // Check LessEq on mixed bases.
  Big aa = a;
  aa.Shift(3);
  CHECK(LessEq(aa, aa));
  CHECK(LessEq(a, aa));
  CHECK(LessEq(aa, a));
  CHECK(!Less(a, aa));
  CHECK(!Less(aa, a));
  
  CHECK(LessEq(aa, b));
  CHECK(!LessEq(b, aa));
  CHECK(Less(aa, b));
  CHECK(!Less(b, aa));

  Big ff = Scale(f, 997, 3);
  CHECK(LessEq(ff, f));
  CHECK(LessEq(ff, e));
  CHECK(!LessEq(f, ff));
  CHECK(!LessEq(e, ff));
  CHECK(LessEq(ff, ff));
  
  return 0;
}

