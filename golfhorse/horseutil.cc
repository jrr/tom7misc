
#include "horseutil.h"

#include <vector>
#include <string>
#include <cstdint>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

using namespace std;
using int64 = int64_t;

static int SharedPrefix(const string &a, const string &b) {
  int i = 0;
  while (i < a.size() && i < b.size() && a[i] == b[i]) i++;
  return i;
}

vector<string> DeltaEncode(const vector<string> &words) {
  vector<string> out;
  out.reserve(words.size());
  string prev_word = "";
  for (const string &word : words) {
    string new_word = word;
    // Only the overlapping region needs to change.
    for (int i = 0; i < std::min(word.size(), prev_word.size()); i++) {
      int prev_char = prev_word[i];
      int new_char = new_word[i];
      int delta = new_char - prev_char;
      int code = (delta + 26) % 26;
      CHECK(code >= 0 && code < 26) << prev_word << " "
				    << word << " " <<  i;
      new_word[i] = ('a' + code);
    }
    out.push_back(std::move(new_word));
    prev_word = word;
  }
  return out;
}

string PrefixEncode(const vector<string> &words,
		    bool max_prefix_9,
		    bool suffix_encoding) {
  string data;

  if (max_prefix_9) {
    CHECK(!suffix_encoding) << "unsupported";
    
    int64 wasted = 0LL;
    string lastword = "";
    for (int i = 0; i < words.size(); i++) {
      const string &word = words[i];
      if (i == 0) {
	data += word;
      } else {
	int real_pfx = SharedPrefix(lastword, word);
	if (real_pfx > 9) wasted += (real_pfx - 9);
	int pfx = std::min(real_pfx, 9);
	data +=
	  StringPrintf("%d%s", pfx,
		       word.substr(pfx, string::npos).c_str());
      }
      lastword = word;
    }
    
    data += "0";
    /*
    fprintf(stderr, "Prefix encoding wasted %lld by only having 0-9\n",
	    wasted);
    */
  } else {

    // Almost the same, but here the order is
    //    [newsuffix][length of prefix shared with prev]
    string lastword = "";
    for (int i = 0; i < words.size(); i++) {
      const string &word = words[i];
      int pfx = SharedPrefix(lastword, word);

      int num = suffix_encoding ? lastword.size() - pfx : pfx;
      
      data +=
	StringPrintf("%s%d",
		     word.substr(pfx, string::npos).c_str(),
		     num);
      lastword = word;
    }
  }
  
  return data;
}
