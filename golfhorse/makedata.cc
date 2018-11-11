#include <string>
#include <vector>
#include <stdio.h>
#include <algorithm>
#include <unordered_set>
#include <shared_mutex>

#include "util.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"
#include "base/stringprintf.h"
#include "base/logging.h"
#include "../cc-lib/gtl/top_n.h"

using namespace std;

// This is currently assumed.
static constexpr bool all_onecodepoint_source = true;

#undef WRITE_LOG

// TODO: To threadutil, but note that this is C++17.
struct ReadMutexLock {
  explicit ReadMutexLock(std::shared_mutex *m) : m(m) { m->lock_shared(); }
  ~ReadMutexLock() { m->unlock_shared(); }
  std::shared_mutex *m;
};
// Possible to template this over shared_mutex and mutex without
// requiring an argument?
struct WriteMutexLock {
  explicit WriteMutexLock(std::shared_mutex *m) : m(m) { m->lock(); }
  ~WriteMutexLock() { m->unlock(); }
  std::shared_mutex *m;
};

struct Periodically {
  explicit Periodically(int every_seconds) :
    start(time(nullptr)),
    every_seconds(every_seconds) {
    last_done = start;
  }

  void Force() {
    last_done = 0LL;
  }
  
  bool ShouldDo(int64 now) {
    const int64 elapsed = now - last_done;
    if (elapsed > every_seconds) {
      last_done = now;
      return true;
    }
    return false;
  }
  
  const int64 start = 0LL;
  const int every_seconds = 0;
  int64 last_done = 0LL;
};

/*
static constexpr int NUM_PASSES = 1000;
static constexpr int OUTER_THREADS = 2;
static constexpr int INNER_THREADS = 30;
static constexpr int LOCAL_CANDIDATES = 10;
static constexpr int GLOBAL_CANDIDATES = 16;
static constexpr int BEST_PER_ROUND = 2;
*/
static constexpr int NUM_PASSES = 1;
static constexpr int OUTER_THREADS = 1;
static constexpr int INNER_THREADS = 58;
static constexpr int LOCAL_CANDIDATES = 10;
static constexpr int GLOBAL_CANDIDATES = 16;
static constexpr int BEST_PER_ROUND = 1;
static constexpr int DETERMINISTIC = true;

// 0 - Silent
// 1 - Print e.g. termination reason
// 3 - Print the replacements we actually make
// 4 - Print the replacements we try
// 5 - Print the strings found by each inner thread
static constexpr int VERBOSE = 3;

static ArcFour *rcpool[OUTER_THREADS];
static void InitRandom(const string &seed) {
  ArcFour rc(seed);
  rc.Discard(1024);
  for (int i = 0; i < OUTER_THREADS; i++) {
    std::vector<uint8> subseed;
    subseed.reserve(16);
    for (int x = 0; x < 16; x++) subseed.push_back(rc.Byte());
    rcpool[i] = new ArcFour(subseed);
    rcpool[i]->Discard(512);
  }
}

static int SharedPrefix(const string &a, const string &b) {
  int i = 0;
  while (i < a.size() && i < b.size() && a[i] == b[i]) i++;
  return i;
}

// Count the number of occurrences of the substring, not allowing
// overlap.
static int Occurrences(const string &data,
		       const char *subs, size_t sublen) {
  if (sublen == 0) return 0;
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
  return Occurrences(data, subs.c_str(), subs.size());
}

inline static int TabledOccurrences(const vector<vector<int>> &table,
				    const string &data,
				    const char *subs,
				    size_t sublen) {
  if (sublen == 0) return 0;
  const uint8 c = *subs;
  const vector<int> &starts = table[c];

  int occ = 0;
  int next_valid_start = 0;
  for (int start : starts) {
    if (start + sublen >= data.size())
      break;
    
    if (start >= next_valid_start) {
      for (int i = 0; i < sublen; i++) {
	if (data[start + i] != subs[i]) goto next;
      }
      // Found a match.
      occ++;
      // Don't allow overlapping matches.
      next_valid_start = start + sublen;
    }
  next:;
  }
  return occ;
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

struct Candidates {
  struct Compare {
    bool operator ()(const pair<string, int> &a,
		     const pair<string, int> &b) {
      return a.second > b.second;
    }
  };

  Candidates(int num) : topn(num, Compare()) {}
  
  inline bool Has(const string &s) {
    return already.find(s) != already.end();
  }
  
  inline void Add(const string &s, int value) {
    topn.push(make_pair(s, value));
    already.insert(s);
  }
  
  std::unordered_set<string> already;
  gtl::TopN<std::pair<string, int>, Compare> topn;
};

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
static constexpr int LONGEST_EXTENSION = 11;
static std::unique_ptr<Candidates> BestReplacement(
    const vector<vector<int>> &table,
    int source_length,
    int offset, int stride,
    const string &s) {

  auto local_candidates = std::make_unique<Candidates>(LOCAL_CANDIDATES);

  const int min_length = source_length + 1;
  for (int i = offset; i < s.size(); i += stride) {
    for (int len = min_length;
	 len <= (min_length + LONGEST_EXTENSION) && i + len < s.size();
	 len++) {
      if (TerminatedUnicode(s.c_str() + i, len)) {
	const string cand = string(s.c_str() + i, (size_t)len);
	if (!local_candidates->Has(cand)) {
	  const int occ =
	    TabledOccurrences(table, s, s.c_str() + i, (size_t)len);
	  if (occ > 1) {
	    int cost = 1 + source_length + len;
	    int savings = occ * (len - source_length);
	    int value = savings - cost;

	    local_candidates->Add(cand, value);
	  }
	}
      }
    }
  }

  // If none, then the empty string.
  return local_candidates;
  // return make_pair(s.substr(best_idx, best_len), best_value);
}

// v must be length 256. v[c] contains a vector of all the positions
// in s, in ascending sorted order, where the character c is.
static void MakeTable(const string &s, vector<vector<int>> *v) {
  for (vector<int> &vv : *v) vv.clear();
  for (int i = 0; i < s.size(); i++) {
    uint8 c = s[i];
    (*v)[c].push_back(i);
  }
}

static string Encode(const string &s) {
  const bool all_ascii =
    [&s](){
      for (uint8 c : s)
	if (c < ' ' || c >= 127)
	  return false;
      return true;
    }();
  if (all_ascii) return s;

  string ret = "";
  bool unterminated_utf = false;
  for (uint8 c : s) {
    if ((c & 0b111'00000) == 0b110'00000) {
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
      } else if (c < ' ') {
	ret += StringPrintf("[%02x]", c);
      } else if (c > 127) {
	ret += StringPrintf("[BAD HI? %02x]", c);
      } else {
	ret += c;
      }
    }
  }
  if (unterminated_utf) ret += "[BAD UNTERMINATED]";
  return ret;
}

static void WriteFile(const char *filename,
		      const string &data,
		      const vector<pair<string, string>> &reps) {
  // Note "binary" is important on windows to avoid converting
  // \n to \r\n.
  FILE *out = fopen(filename, "wb");
  CHECK(out) << "Couldn't open " << filename;

  fprintf(out, "s='");
  CHECK(1 == fwrite(data.data(), data.size(), 1, out));
  // fprintf(out, "%s", data.c_str());
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

  // Note the use of fwrite below instead of fprintf with %s, since
  // the strings may include 0.
  if (all_onecodepoint_source && sep != 0) {
    fprintf(out, "for(o of'");
    for (int i = reps.size() - 1; i >= 0; i--) {
      if (i != reps.size() - 1) fprintf(out,"%c",sep);
      CHECK(1 == fwrite(reps[i].first.data(), reps[i].first.size(), 1, out));
      CHECK(1 == fwrite(reps[i].second.data(), reps[i].second.size(), 1, out));
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

      fprintf(out, "'");
      CHECK(1 == fwrite(reps[i].first.data(), reps[i].first.size(), 1, out));
      CHECK(1 == fwrite(reps[i].second.data(), reps[i].second.size(), 1, out));
      fprintf(out, "'");
    }
    fprintf(out, "])s=s.split(o[0]).join(o.slice(1))\n");
  } else {
    fprintf(out, "r=[");
    for (int i = reps.size() - 1; i >= 0; i--) {
      if (i != reps.size() - 1) fprintf(out,",");
      fprintf(out, "'");
      CHECK(1 == fwrite(reps[i].first.data(), reps[i].first.size(), 1, out));
      fprintf(out, "','");
      CHECK(1 == fwrite(reps[i].second.data(), reps[i].second.size(), 1, out));
      fprintf(out, "'");
    }
    fprintf(out, "]\n");
    fprintf(out,
	    "for(i=0;i<%d;i+=2)s=s.split(r[i]).join(r[i+1])\n",
	    (int)reps.size());
  }

  fprintf(out,
	  "w='';for(c of s)+c+1?w=console.log(w)||w.substr(0,+c):w+=c");
  
  fclose(out);
}

static string PrefixEncode(const vector<string> &words) {
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
  return data;
}

static vector<string> GetSources() {
  vector<string> sources;
  // According to ES6, anything can appear literally except:
  // closing quote code points, U+005C (REVERSE SOLIDUS),
  // U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR),
  // U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED). 
  //
  // So good news is we can probably add some low-ascii here, but bad news
  // is that we need to filter out a couple high unicode codepoints too.

  // There are many three-character strings in long wordlists, so
  // having two-byte utf-8 sequences is quite useful. It doesn't
  // complicate our dense encoding either, since javascript works in
  // units of codepoints, not characters.
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

  // TODO PERF can include the two-byte \\ and \'. They are at least
  // as good as utf-16, though tricky to handle in some ways (and we
  // have to be careful about splitting them, like with multibyte utf)
  string sourcechars =
    " \"ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-=+[]{}|;:<>?,./~`";
  for (char c : sourcechars) {
    string ch = " ";
    ch[0] = c;
    sources.push_back(ch);
  }

  // And low ASCII.
  for (uint8 c = 0; c < 32; c++) {
    // Only carriage return and newline are disallowed here.
    if (c != 0x0A && c != 0x0D) {
      string ch = " ";
      ch[0] = c;
      sources.push_back(ch);
    }
  }
  return sources;
}


static pair<string, vector<pair<string, string>>> Optimize(
    // Exclusive access.
    ArcFour *rc,
    string data,
    vector<string> sources) {
  vector<pair<string, string>> reps;
  
  vector<vector<int>> table;
  table.resize(256);

  #ifdef WRITE_LOG
  FILE *log = fopen("log.txt", "w");
  #endif
  

  for (int round = 0; true; round++) {
    // vector<pair<string, int>> results;
    // results.reserve(INNER_THREADS);
    
    if (sources.empty()) {
      if (VERBOSE >= 1)
	fprintf(stderr, "No more sources (up)...\n");
      goto nomore;
    }
    int source_length = sources.back().size();

    MakeTable(data, &table);

    std::unique_ptr<vector<pair<string, int>>> results;
    // Often it is better to use outer parallelism, to the point that
    // we might as well be single-threaded here to avoid thread
    // overhead. So special case when inner threads is 1.
    if (INNER_THREADS == 1) {
      auto res =
	BestReplacement(table, source_length, 0, 1, data);
      results.reset(res->topn.Extract());
    } else {
      std::mutex m;
      Candidates candidates(GLOBAL_CANDIDATES);
      ParallelComp(INNER_THREADS,
		   [&m, &candidates, &data, &table, source_length](int idx) {

		     auto res =
		       BestReplacement(table, source_length, idx,
				       INNER_THREADS, data);
		   
		     {
		       MutexLock ml(&m);
		       for (auto it = res->topn.unsorted_begin();
			    it != res->topn.unsorted_end();
			    ++it) {
			 if (!candidates.Has(it->first)) {
			   candidates.Add(it->first, it->second);
			 }
		       }
		     }
		   },
		   INNER_THREADS);
      if (VERBOSE >= 5) {
	fprintf(stderr, ".\n");
	fflush(stderr);
      }
      results.reset(candidates.topn.Extract());
    }
    
    // Early on, significant chance of just doing them in random order.
    if (!DETERMINISTIC && round < 2 && rc->Byte() < 100) {
      Shuffle(rc, results.get());
    }

    if (VERBOSE >= 5) {
      for (const auto &p : *results) {
	fprintf(stderr, " [%s]x%d\n", p.first.c_str(), p.second);
      }
      fflush(stderr);
    }
    
    // Take the overall best (or all of them?)
    bool made_progress = false;
    int tried = 0;
    const bool skip_allowed = !DETERMINISTIC && (rc->Byte() < 30);
    for (int rr = 0; rr < results->size(); rr++) {
      if (!DETERMINISTIC && skip_allowed && rc->Byte() < 64) {
	// Don't want to cause this random skip to exit!
	made_progress = true;
	continue;
      }
      const string &dst = (*results)[rr].first;

      if (tried > BEST_PER_ROUND)
	break;
      
      tried++;
      
      if (sources.empty()) {
	if (VERBOSE >= 1)
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
      if (VERBOSE >= 4 || (VERBOSE >= 3 && savings > cost)) {
	fprintf(stderr, "%s<-%s has %d real occurrences cost %d savings %d.\n",
		Encode(dst).c_str(),
		Encode(sources.back()).c_str(), occ, cost, savings);
      }
      if (savings > cost) {
	string src = sources.back();
	sources.pop_back();
	reps.push_back(std::make_pair(src, dst));
	data = Util::Replace(data, dst, src);
	if (VERBOSE >= 3) {
	  int source_size = sources.empty() ? 0 : (int)sources.back().size();
	  fprintf(stderr, "Size now %d. %d source(s) left [size %d]\n",
		  (int)data.size(),
		  (int)sources.size(),
		  source_size);
	}
	made_progress = true;

	#ifdef WRITE_LOG
	fprintf(log, "'%s' <- '%s' has %d real occurrences,"
		"cost %d savings %d (now %d)\n",
		Encode(dst).c_str(),
		Encode(src).c_str(), occ, cost, savings,
		(int)data.size());
	fflush(log);
	#endif
      }
      fflush(stderr);
    }
    if (!made_progress) {
      if (VERBOSE >= 1)
	fprintf(stderr, "Didn't make progress...");
      break;
    }
  }
  nomore:;
  if (VERBOSE > 0)
    fflush(stderr);
  #ifdef WRITE_LOG
  fclose(log);
  #endif
  return {data, reps};
}

int Metric(const string &data,
	   const vector<pair<string, string>> &reps) {
  // Optimistic encoding of n reps is n separators plus the
  // length of the contents.
  int rep_data_size = 0;
  for (const auto &p : reps)
    rep_data_size += p.first.size() + p.second.size();
  return data.size() + reps.size() + rep_data_size;
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

  const string start_data = PrefixEncode(words);  
  const vector<string> start_sources = GetSources();

  const int64 time_start = time(nullptr);

  InitRandom(StringPrintf("%lld", time_start));

  std::mutex m;
  int best_metric = -1;
  string best_data;
  vector<pair<string, string>> best_reps;
  Periodically progress(5);

  bool dirty = false;
  int passes = 0;
  while (passes < NUM_PASSES) {
    ParallelComp(OUTER_THREADS,
		 [&start_data, &start_sources,
		  &m, &best_metric, &best_data, &best_reps, &dirty](int x) {

		   string data;
		   vector<pair<string, string>> reps;
		   std::tie(data, reps) =
		     Optimize(rcpool[x], start_data, start_sources);
		   const int metric = Metric(data, reps);
		   {
		     MutexLock ml(&m);
		     if (best_metric < 0 ||
			 metric < best_metric) {
		       best_metric = metric;
		       best_data = std::move(data);
		       best_reps = std::move(reps);
		       dirty = true;
		       fprintf(stderr, "New best: %d\n", metric);
		     }
		   }
		 },
		 OUTER_THREADS);

    passes += OUTER_THREADS;
    
    if (progress.ShouldDo(time(nullptr))) {
      fprintf(stderr, "... %d done best %d\n", passes, best_metric);
      if (dirty) {
	dirty = false;
	WriteFile(argv[2], best_data, best_reps);
	fprintf(stderr, "Wrote %s\n", argv[2]);
      }
      fflush(stderr);
    }
  }
		  
#if 0  
  int64 time_now = time(nullptr);
  if (time_now - start_time > MAX_SECONDS) {
    if (VERBOSE >= 1)
      fprintf(stderr, "Ran out of time.\n");
    break;
  }
#endif    

  WriteFile(argv[2], best_data, best_reps);
  return 0;
}
