#include <string>
#include <vector>
#include <stdio.h>

#include "util.h"
#include "../cc-lib/threadutil.h"
#include "base/stringprintf.h"

using namespace std;

static int SharedPrefix(const string &a, const string &b) {
  int i = 0;
  while (i < a.size() && i < b.size() && a[i] == b[i]) i++;
  return i;
}

static int Occurrences(const string &data, const string &subs) {
  size_t start = 0;
  int occ = 0;
  for (;;) {
    size_t next = data.find(subs, start);
    if (next == string::npos)
      return occ;
    occ++;
    start = next + 1;
  }
}

#if 0
// Returns the best substring (best = maximum value of (size *
// occurrences)) and its number of occurrences.
static constexpr int LONGEST_SUBSTRING = 12;
std::pair<string, int> BestReplacement(int offset, int stride,
				       const string &s) {
  int best_mass = 0;
  int best_idx = 0;
  int best_len = 0;
  for (int i = offset; i < s.size(); i += stride) {
    for (int l = 2; l <= LONGEST_SUBSTRING; l++) {
      

    }
  }
}
#endif


int main (int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Provide wordlist on command line!");
    return -1;
  }
  vector<string> words = Util::ReadFileToLines(argv[1]);
  if (words.empty()) {
    fprintf(stderr, "Couldn't open %s\n", argv[1]);
    return -2;
  }

  string data;
  {
    string lastword = "";
    for (int i = 0; i < words.size(); i++) {
      const string &word = words[i];
      if (i == 0) {
	data += word;
      } else {
	int pfx = std::min(SharedPrefix(lastword, word), 9);
	data +=
	  StringPrintf("%d%s", pfx,
		       word.substr(pfx, string::npos).c_str());
      }
      lastword = word;
    }
  }
  data += "0";

  vector<pair<string, string>> reps;
  vector<string> sources;
  string sourcechars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-=+[]{}|;:<>?,./~`";
  for (char c : sourcechars) {
    string ch = " ";
    ch[0] = c;
    sources.push_back(ch);
  }
  if (sources.size() != sourcechars.size())
    exit(-1);
  
  //  for (int i = 0; i < 10; i++) {

    std::mutex m;
    vector<string> results =
      { "ing", "tion", "str", "sh", "tr", "st", "nth", "tive", "ly",
	"able", "ble", "ft", "er", "io", "ct", "rt", "es", "is", "it" };

    #if 0
    static constexpr int NUM_THREADS = 50;
    ParallelComp(NUM_THREADS,
		 [&m, &results, &data](int idx) {

		   auto res =
		     BestReplacement(idx, NUM_THREADS, data);
		   
		   {
		     MutexLock ml(&m);
		     results.push_back(std::move(res));
		   }
		 },
		 NUM_THREADS);
#endif

    
    
    // Take the overall best (or all of them?)
    for (const string &dst : results) {
      if (sources.empty())
	goto nomore;
      // This should be dynamic based on the string length,
      // right?
      static constexpr int MIN_MASS = 12;
      // Recompute this because 
      int occ = Occurrences(data, dst);
      fprintf(stderr, "%s has %d real occurrences.\n", dst.c_str(), occ);
      if (occ * dst.size() >= MIN_MASS) {
	string src = sources.back();
	sources.pop_back();
	reps.push_back(std::make_pair(src, dst));
	data = Util::Replace(data, dst, src);
	fprintf(stderr, "Size now %lld. %d source(s) left\n", data.size(),
		(int)sources.size());
      }
    }
    //  }
  nomore:;

  printf("s='");
  printf("%s", data.c_str());
  printf("0'\n");
  // Need to do the replacements in backwards order.
  printf("r=[");
  for (int i = reps.size() - 1; i >= 0; i--) {
    if (i != reps.size() - 1) printf(",");
    printf("'%s','%s'", reps[i].first.c_str(), reps[i].second.c_str());
  }
  printf("]\n");
  return 0;
}
