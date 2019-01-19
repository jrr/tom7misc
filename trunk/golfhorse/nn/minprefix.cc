
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <unordered_set>

#include "../../cc-lib/threadutil.h"
#include "../../cc-lib/randutil.h"
#include "../../cc-lib/arcfour.h"
#include "../../cc-lib/base/logging.h"
#include "../../cc-lib/base/stringprintf.h"
#include "../../cc-lib/util.h"

#include "../huffman.h"
#include "network.h"

using namespace std;

using int64 = int64_t;
using uint32 = uint32_t;

template<class K, class C>
inline bool ContainsKey(const C &container, const K &key) {
  return container.find(key) != container.end();
}

int main(int argc, char **argv) {
  // XXX oneoff...
  vector<int> depths =
    {
 420179,
  145949,
  89780,
  64748,
  47820,
  34415,
  27265,
  22491,
  19042,
  16953,
  15579,
  13867,
  13283,
  11807,
  11439,
  10684,
  10149,
  9609,
  8114,
  7376,
  6767,
  6605,
  5903,
  6043,
  4943,
  5744,
  3973
    };

  Huffman huff;
  for (int i = 0; i < depths.size(); i++) {
    huff.AddSymbol(i, depths[i]);
  }

  huff.MakeTree();
  
  huff.PrintTree();
  huff.PrintCodes();

  vector<vector<bool>> codes = huff.MakeCodes();
  
  int num_bits = 0;
  for (int i = 0; i < depths.size(); i++) {
    num_bits += depths[i] * codes[i].size();
  }

  printf("With Huffman, whole thing would take %d bits = %d bytes.\n",
	 num_bits, num_bits / 8);
  
  return 0;
  
  string dict = Util::Replace(Util::ReadFile("../wordlist.asc"),
			      "\n", "|");
			      
  // Find the minimum n such that every length-n substring in the
  // dictionary is unique. For this size, a completely "over"fit
  // model taking n characters and predicting the next one would
  // successfully reproduce the dict.
  for (int hist = 1; hist < 32; hist++) {
    std::unordered_set<string> seen;
    for (int i = 0; i < dict.size() - hist; i++) {
      const string s = dict.substr(i, hist);
      if (ContainsKey(seen, s)) {
	printf("Length %d substring %s appears at least twice.\n",
	       hist, s.c_str());
	goto next;
      }
      seen.insert(s);
    }
    printf("All length %d substrings are distinct.\n", hist);
    return 0;
  next:;
  }
  
  return 0;
}

