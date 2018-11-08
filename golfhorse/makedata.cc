#include <string>
#include <vector>
#include <stdio.h>
#include <algorithm>
#include <unordered_set>

#include "util.h"
#include "../cc-lib/threadutil.h"
#include "base/stringprintf.h"

using namespace std;

static int SharedPrefix(const string &a, const string &b) {
  int i = 0;
  while (i < a.size() && i < b.size() && a[i] == b[i]) i++;
  return i;
}

// Count the number of occurrences of the substring, not allowing
// overlap.
static int Occurrences(const string &data,
		       const char *subs, size_t sublen) {
  size_t start = 0;
  int occ = 0;
  for (;;) {
    size_t next = data.find(subs, start, sublen);
    if (next == string::npos)
      return occ;
    occ++;
    start = next + sublen;
  }
}

static int Occurrences(const string &data, const string &subs) {
  if (subs.size() == 0) return 0;
  return Occurrences(data, subs.c_str(), subs.size());
}

// This assumes that we have at most two-byte utf-8 encodings
// and that the string s is all valid.
static inline bool TerminatedUnicode(const char *s, int len) {
  if (len == 0) return true;

  // First byte can't be a multibyte continuation.
  if ((s[0] & 0b11'000000) == 0b10'000000)
    return false;
  // Last byte of the substring can't be the first byte in a multibyte
  // encoding.
  if ((s[len - 1] & 0b111'00000) == 0b110'00000)
    return false;
  return true;
}

// PERF: Early on, we tend to keep finding the same high-value string
// in every thread! Could perhaps protect against this by having a
// different set of starting characters that are considered by each
// thread, instead of using strides?
// Better would be to build an index by the first byte, and then
// split that up. This also helps us find matches much more
// efficiently, because you can just iterate over all the places
// where the occurrence might start. Dealing with self-overlap is a
// little tricky, although it could just be a post-processing
// step since we could generate the actual occurrence locations.
// (Or like if they are sorted, then we can keep track of
// the "next index where a valid occurrence could start" pretty
// easily. And why wouldn't we just generate them in sorted order?)

// TODO: Ideally we would allow longest extension to grow if we
// can't find anything within the horizon. Even just two
// occurrences of a very long string would be worth a lot.
//
// ALSO: Couldn't we just invent new sources? We can safely use
// any string that doesn't appear in the input, and that wouldn't
// induce any accidental occurrences (e.g. due to overlap) when
// reverse-replaced. This might open us up to a large number of
// short codes. (OTOH we can't use the very compact representation
// of the substitutions in the decoder. Would be ok to have
// two substitution phases, though, or a special separator character
// that when absent means just use the first codepoint).

// Returns the best substring (best = maximum value of (size *
// occurrences)) and the score.
static constexpr int LONGEST_EXTENSION = 6;
static pair<string, int> BestReplacement(
    int source_length,
    int offset, int stride, const string &s) {
  int best_value = 0;
  int best_idx = 0;
  int best_len = 0;
  const int min_length = source_length + 1;
  for (int i = offset; i < s.size(); i += stride) {
    for (int len = min_length;
	 len <= (min_length + LONGEST_EXTENSION) && i + len < s.size();
	 len++) {
      if (TerminatedUnicode(s.c_str() + i, len)) {
	const int occ = Occurrences(s, s.c_str() + i, (size_t)len);
	if (occ > 1) {
	  int cost = 1 + source_length + len;
	  int savings = occ * (len - source_length);
	  int value = savings - cost;

	  if (value > best_value) {
	    best_value = value;
	    best_idx = i;
	    best_len = len;
	  }
	}
      }
    }
  }

  // If none, then the empty string.
  return make_pair(s.substr(best_idx, best_len), best_value);
}

static string Encode(const string &s) {
  const bool all_ascii =
    [&s](){
      for (char c : s)
	if (c < ' ' || c >= 127)
	  return false;
      return true;
    }();
  if (all_ascii) return s;

  string ret = "";
  bool unterminated_utf = false;
  for (uint8 c : s) {
    if (c < ' ') {
      if (unterminated_utf) {
	ret += StringPrintf("[BAD UNTERMINATED LOW %02x", (int)c);
	unterminated_utf = false;
      } else {
	ret += StringPrintf("[BAD LOW %02x]", (int)c);
      }
    } else if ((c & 0b111'00000) == 0b110'00000) {
      // First byte.
      unterminated_utf = true;
      ret += StringPrintf("[%02x]", (int)c);
    } else if ((c & 0b11'000000) == 0b10'000000) {
      if (!unterminated_utf) {
	ret += StringPrintf("[BAD UNANTICIPATED UTF CONTINUATION %02x]",
			    (int)c);
      } else {
	ret += StringPrintf("[%02x]", (int)c);
	unterminated_utf = false;
      }
    } else {
      if (unterminated_utf) {
	ret += StringPrintf("[BAD UNTERMINATED ASCII %02x=%c]", (int)c, c);
	unterminated_utf = false;
      } else {
	ret += c;
      }
    }
  }
  if (unterminated_utf) ret += "[BAD UNTERMINATED]";
  return ret;
}


int main (int argc, char **argv) {
  if (argc < 3) {
    fprintf(stderr, "makedata wordlist out-data.js\n");
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
  // There are many three-character strings, so having two-byte utf-8
  // sequences is still useful. It doesn't complicate our dense
  // encoding either, since javascript works in units of codepoints,
  // not characters.
  for (int i = 128; i < 2048; i++) {
    uint8 b1 = 0b110'00000;
    uint8 b2 = 0b10'000000;

    b1 |= (i >> 6) & 0b11111;
    b2 |= i & 0b111111;
    string s = StringPrintf("%c%c", b1, b2);
    sources.push_back(s);
  }
  
  // Single-character codes come last, since these will be used for
  // more common matches in the greedy algorithm below.

  // XXX can include the two-byte \\ and \' as well as some control
  // characters like \r \n \t. They are at least as good as utf-16,
  // though maybe tricky to handle in some ways (and we have to be
  // careful about splitting them, like with multibyte utf)
  string sourcechars =
    " \"ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-=+[]{}|;:<>?,./~`";
  for (char c : sourcechars) {
    string ch = " ";
    ch[0] = c;
    sources.push_back(ch);
  }

  
  // Forced to true 
  constexpr bool all_onecodepoint_source = true;
  /*
  for (const auto &p : reps)
    if (p.first.size() != 1)
      all_onecodepoint_source = false;
  */
  

  static constexpr int BEST_PER_ROUND = 1;
  // TODO: Periodically write file?
  static constexpr int MAX_SECONDS = 2 * 3600;
  
  int64 start_time = time(nullptr);
  for (;;) {
    int64 time_now = time(nullptr);
    if (time_now - start_time > MAX_SECONDS) {
      fprintf(stderr, "Ran out of time.\n");
      break;
    }
    
    std::mutex m;
    vector<pair<string, int>> results;

    static constexpr int NUM_THREADS = 56;
    if (sources.empty()) {
      fprintf(stderr, "No more sources (up)...\n");
      goto nomore;
    }
    int source_length = sources.back().size();

    ParallelComp(NUM_THREADS,
		 [&m, &results, &data, source_length](int idx) {

		   auto res =
		     BestReplacement(source_length, idx, NUM_THREADS, data);
		   
		   {
		     MutexLock ml(&m);
		     results.push_back(std::move(res));
		   }
		 },
		 NUM_THREADS);
    fprintf(stderr, ".\n");
    fflush(stderr);
    
    // Sort descending.
    // PERF: Should sort by savings here. Common to see cases where
    // a later string has better savings.
    std::sort(results.begin(), results.end(),
	      [](const pair<string, int> &a,
		 const pair<string, int> &b) {
		return a.second > b.second;
	      });

    for (const auto &p : results) {
      fprintf(stderr, " [%s]x%d\n", p.first.c_str(), p.second);
    }
    fflush(stderr);
    
    // Take the overall best (or all of them?)
    bool made_progress = false;
    std::unordered_set<string> already;
    int tried = 0;
    for (int rr = 0; rr < results.size(); rr++) {
      const string &dst = results[rr].first;
      // Don't reconsider a string we just substituted!
      if (already.find(dst) != already.end())
	continue;
      already.insert(dst);

      if (tried > BEST_PER_ROUND)
	break;
      
      tried++;
      
      if (sources.empty()) {
	fprintf(stderr, "No more sources...\n");
	goto nomore;
      }
	
      // Recompute this because if we've done any replacements, in this
      // loop, the number of occurrences can easily change (for one
      // simple reason, two threads may find the same best replacement!)
      int occ = Occurrences(data, dst);
      // four quote chars (source and dest), two commas, source and
      // dest strings.
      // int cost = 4 + 2 + sources.back().size() + dst.size();
      // Assumes we can find a separator.
      static_assert(all_onecodepoint_source, "precondition");
      int cost = 1 + sources.back().size() + dst.size();
      int savings = occ * (dst.size() - sources.back().size());
      fprintf(stderr, "%s<-%s has %d real occurrences cost %d savings %d.\n",
	      Encode(dst).c_str(),
	      Encode(sources.back()).c_str(), occ, cost, savings);
      if (savings > cost) {
	string src = sources.back();
	sources.pop_back();
	reps.push_back(std::make_pair(src, dst));
	data = Util::Replace(data, dst, src);
	fprintf(stderr, "Size now %d. %d source(s) left\n",
		(int)data.size(),
		(int)sources.size());
	made_progress = true;
      }
      fflush(stderr);
    }
    if (!made_progress) {
      fprintf(stderr, "Didn't make progress...");
      break;
    }
  }
  nomore:;
  fflush(stderr);
  
  FILE *out = fopen(argv[2], "w");
  if (!out) {
    fprintf(stderr, "Couldn't open %s\n", argv[2]);
    return -1;
  }
  fprintf(out, "s='");
  fprintf(out, "%s", data.c_str());
  fprintf(out, "'\n");
  // Need to do the replacements in backwards order.

  auto InReps =
    [&reps](char c) {
      for (const auto &p : reps) {
	if (p.first.find(c) != string::npos ||
	    p.second.find(c) != string::npos)
	  return true;
      }
      return false;
    };

  char sep = 0;
  for (char c :
	 // Prefer a numeric separator, since we don't
	 // need to quote these.
	 "0123456789"
	 "qwertyuiopasdfghjklzxcvbnm"
	 "QWERTYUIOPASDFGHJKLZXCVBNM"
	 "!@#$%^&*()_+[]{}|:;<>?,./ ") {
    if (!InReps(c)) { sep = c; break; }
  }

  // TODO: Even better encoding here would be like a number (digit? but
  // then the maximum length would be 9, or I guess we could assume
  // the replacement is at least 2, so 11) that gives the length of the
  // replacement, then the single source codepoint (this would preclude
  // using two-codepoint ascii sources, which does give us 9025 useful
  // sources...), then the n characters. so the separator would contain
  // information. parsing would be a little trickier, but not that bad.
  // So like 7*destina3zfoo would mean replace * with destina and
  // z with foo. Actually that is no cheaper than just using a single
  // character as the separator anyway. Better is like
  // *7destinaz3foo, with the advantage here being that we don't have
  // to assume a single codepoint for the source strings, as long as
  // they can't contain a digit (easy to arrange; we use the digits
  // for a special purpose anyway). And of course we have a maximum
  // replacement size if we do that.
  
  if (all_onecodepoint_source && sep != 0) {
    fprintf(out, "for(o of'");
    for (int i = reps.size() - 1; i >= 0; i--) {
      if (i != reps.size() - 1) fprintf(out,"%c",sep);
      fprintf(out, "%s%s",
	      reps[i].first.c_str(), reps[i].second.c_str());
    }
    string sep_js = (sep >= '0' && sep <= '9') ?
      StringPrintf("%c", sep) :
      StringPrintf("'%c'", sep);
    fprintf(out, "'.split(%s))s=s.split(o[0]).join(o.slice(1))\n",
	    sep_js.c_str());
  } else if (all_onecodepoint_source) {
    fprintf(out, "for(o of[");
    for (int i = reps.size() - 1; i >= 0; i--) {
      if (i != reps.size() - 1) fprintf(out,",");
      fprintf(out, "'%s%s'", reps[i].first.c_str(), reps[i].second.c_str());
    }
    fprintf(out, "])s=s.split(o[0]).join(o.slice(1))\n");
  } else {
    fprintf(out, "r=[");
    for (int i = reps.size() - 1; i >= 0; i--) {
      if (i != reps.size() - 1) fprintf(out,",");
      fprintf(out, "'%s','%s'", reps[i].first.c_str(), reps[i].second.c_str());
    }
    fprintf(out, "]\n");
    fprintf(out,
	    "for(i=0;i<%d;i+=2)s=s.split(r[i]).join(r[i+1])\n",
	    (int)reps.size());
  }
  fclose(out);
  return 0;
}
