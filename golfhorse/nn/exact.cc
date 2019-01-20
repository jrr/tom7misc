
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
#include "network.h"
#include "arith.h"
#include "../horseutil.h"

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

// for a-z 0-9
int ToSymbol(char c) {
  if (c >= 'a' && c <= 'z') return c - 'a';
  if (c >= '0' && c <= '9') return 26 + (c - '0');
  CHECK(false) << "Bad char " << c << " = " << (int)c;
  return 0;
}

char ToChar(int i) {
  if (i < 26) return 'a' + i;
  else if (i < 36) return '0' + (i - 26);
  CHECK(false) << "Bad symbol " << i;
  return 0;
}

struct FreqEncoder : public ArithEncoder {
  // No history; just use frequency table.
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

vector<int> MakeSymbols(const string &str) {
  vector<int> syms;
  for (char s : str) {
    syms.push_back(ToSymbol(s));
  }
  return syms;
}

int main(int argc, char **argv) {
  // Obviously this shouldn't be part of the final output, but it
  // is good for playin' around.
  vector<string> dictlines = Util::ReadFileToLines("../ten-hundred");
  string dict = PrefixEncode(dictlines, true, false);
  printf("%s\n", dict.c_str());
  printf("Prefix encoded dictionary: %d\n", dict.size());
  fflush(stdout);
  
  // Use the network to generate probability mass functions, and
  // feed those to arithmetic encoder.

  FreqEncoder encoder(dict);
  fflush(stdout);
  
  vector<int> symbols = MakeSymbols(dict);
  encoder.Encode(symbols);

  return 0;
}

