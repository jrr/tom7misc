
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
    freqs = Discretize(fracs, false);
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

struct Freq2Encoder : public ArithEncoder {
  // Use history of 1 (1-markov).
  // Careful: 4 is even too high for 32-bit integers.
  Freq2Encoder(const string &word) : ArithEncoder(1, 26 + 10, 124, 2) {
    vector<int> counts(nsymbols * nsymbols, 0);
    int prev = ToSymbol(word[0]);
    for (int i = 1; i < word.size(); i++) {
      char c = word[i];
      int sym = ToSymbol(c);
      counts[nsymbols * prev + sym]++;
      prev = sym;
    }

    freqs.resize(nsymbols);
    for (int p = 0; p < nsymbols; p++) {
      int sum = 0;
      for (int n = 0; n < nsymbols; n++) {
	sum += counts[nsymbols * p + n];
      }
      vector<double> fracs;
      fracs.reserve(nsymbols);
      
      // If the history character was never seen, we will never
      // encounter it (except perhaps on a trial decoding!). So
      // just return something valid.
      if (sum == 0) {
	printf("Never seen in history: %d='%c'\n", p, ToChar(p));
	sum = nsymbols;
	for (int i = 0; i < nsymbols; i++) {
	  fracs.push_back(1.0 / (double)sum);
	  printf("%.2f ", fracs.back());
	}
	
      } else {
	for (int n = 0; n < nsymbols; n++) {
	  fracs.push_back(counts[nsymbols * p + n] /
			  (double)sum);
	}
      }
      CHECK_EQ(fracs.size(), nsymbols);
      
      // Prepare predictions vector.
      freqs[p] = Discretize(fracs, true);
      // But sort by symbol to simplify decoding.
      std::sort(freqs[p].begin(), freqs[p].end(),
		[](const pair<int, int> &a,
		   const pair<int, int> &b) {
		  return a.first < b.first;
		});

      for (int i = 0; i < freqs[p].size(); i++) {
	CHECK(freqs[p][i].first == i);
	printf("[%c] %c is %d = %.3f\n", ToChar(p), ToChar(i),
	       freqs[p][i].second,
	       freqs[p][i].second / (double)(ipow(B, W)));
      }
    }
  }
  
  vector<pair<int, int>> Predict(const deque<int> &hist) override {
    CHECK(hist.size() == 1) << hist.size();
    // This would mean that we never saw the character (except possibly
    // as the last one), so should not happen.
    CHECK(!freqs[hist[0]].empty()) << ToChar(hist[0]);
    return freqs[hist[0]];
  }

  vector<vector<pair<int, int>>> freqs;
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

