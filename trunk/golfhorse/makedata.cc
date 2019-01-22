#include <string>
#include <vector>
#include <stdio.h>
#include <algorithm>
#include <unordered_set>
#include <shared_mutex>
#include <unordered_map>

#include "util.h"
#include "base/stringprintf.h"
#include "base/logging.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/gtl/top_n.h"

#include "horseutil.h"

#include "huffman.h"
#include "nn/arith.h"

using namespace std;

static constexpr bool ONLY_ONECODEPOINT_SOURCES = false;


// Don't allow digits in sources if we are using the multi-codepoint
// encoding, which needs digits for the encoding.
static constexpr bool ALLOW_DIGIT_IN_SOURCE =
  ONLY_ONECODEPOINT_SOURCES;


// The following parameters need to be tuned manually:

// Instead of encoding the length of the shared prefix, encode the
// amount of the previous word to be discarded. Idea is that common
// suffix families are therefore encoded as the same sequence,
// regardless of how long the root word is. This is indeed much
// better for wordlist.asc (173427 -> 162089 bytes).
static constexpr bool SUFFIX_ENCODING = true;

// Only use prefixes up to length 9, so that we only
// use a single digit 0-9 for the prefix encoding, allowing us to use
// a shorter decoder. Not worth it unless the input basically does not
// have long shared prefixes.
static constexpr bool MAX_PREFIX_9 = false;
// Force a separator character to be reserved for the reps,
// so it can't be used in sources. This makes the encoding
// of reps much smaller ('a','bc', -> abc0) at the cost of
// a small amount of coding efficiency. (In many cases,
// there will be some character that we can use by coincidence.)
static constexpr bool FORCE_SEP = false;

// Measured in codepoints. We need that the max codepoint size minus
// the min codepoint size is a single digit. Setting this to 9 will
// certainly ensure that, but there should also never be any reason
// to use 0- or 1-length, and 2 is pointless for some inputs as well.
static constexpr int MAX_DEST_CODEPOINTS = 11;
// static_assert(ONLY_ONECODEPOINT_SOURCES ||
//    	         MAX_DEST_CODEPOINTS <= 9);

// Reverse each word and re-sort before encoding.
// (Note, this doesn't work that well so there is no decoder!)
static constexpr bool REVERSE_WORDS = false;

// Use prefix/suffix encoding.
static constexpr bool USE_PREFIX = true;
// Can turn off this phase, mainly for debugging.
static constexpr bool USE_REPS = true;
static constexpr bool USE_DELTA = false;
static_assert(!(USE_DELTA && USE_PREFIX));
// Same; makes it easier to debug by only using ascii.
static constexpr bool ONLY_ASCII = true;
static constexpr bool ONLY_PRINTABLE = false;
static_assert(!ONLY_PRINTABLE || ONLY_ASCII);

// Turn this on and cross your fingers that we don't have phantom
// occurrences when using sources like AA (see the code below). Self
// check will just abort.
static constexpr bool DUP_TMP_SOURCE_YOLO = false;

// Do moderately expensive tests with each substitution, for tracking
// down bugs.
static constexpr bool SELF_CHECK = DUP_TMP_SOURCE_YOLO || false;
// Minimum allowed ascii character.
static constexpr int MIN_ASCII = 0;
// For debugging, stop when this many reps are reached.
// If negative, no limit.
static constexpr int MAX_REPS = -1;

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

// Verbosity:
// 0 - Silent
// 1 - Print e.g. termination reason
// 3 - Print the replacements we actually make
// 4 - Print the replacements we try
// 5 - Print the strings found by each inner thread

#if 0
static constexpr int NUM_PASSES = 1000;
static constexpr int OUTER_THREADS = 5;
static constexpr int INNER_THREADS = 12;
static constexpr int LOCAL_CANDIDATES = 10;
static constexpr int GLOBAL_CANDIDATES = 16;
static constexpr int BEST_PER_ROUND = 1;
static constexpr int DETERMINISTIC = false;
static constexpr int VERBOSE = 0;
static_assert(SELF_CHECK == false);
#else
static constexpr int NUM_PASSES = 1;
static constexpr int OUTER_THREADS = 1;
static constexpr int INNER_THREADS = 58;
static constexpr int LOCAL_CANDIDATES = 10;
static constexpr int GLOBAL_CANDIDATES = 16;
static constexpr int BEST_PER_ROUND = 1;
static constexpr int DETERMINISTIC = true;
static constexpr int VERBOSE = 3;
#endif


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

// In multibyte UTF-8 encodings, bytes after the first one all have
// this mask (and exactly six meaningful bits).
static inline bool IsUtf8ContinuationByte(uint8 c) {
  return (c & 0b11'000000) == 0b10'000000;
}

// Single ASCII characters that we allow inside our JS literals.
static inline bool IsSafeASCII(uint8 c) {
  return c < 128 && c != 0x0a && c != 0x0d && c != '\\' && c != '\'';
}

// If 0, then this is regular ASCII, a continuation byte, or invalid.
// Does not return 1.
// Returns 2, 3, or 4 for valid 2-, 3-, or 4-byte prefix.
static inline uint8 IsUtf8MultibytePrefix(uint8 c) {
  // ASCII
  if ((c & 0b1'0000000) == 0)
    return 0;
  if ((c & 0b111'00000) == 0b110'00000)
    return 2;
  if ((c & 0b1111'0000) == 0b1110'0000)
    return 3;
  if ((c & 0b11111'000) == 0b11110'000)
    return 4;
  // Invalid
  return 0;
}

// Assumes a valid utf-8 string, not pointing inside a multibyte
// encoding. Returns decoded 32-bit codepoint and length (1, 2, 3, 4).
static inline std::pair<uint32, uint8> GetUtf8(const char *s) {
  switch (IsUtf8MultibytePrefix(*s)) {
  default:
  case 0: return make_pair(*s, 1);
  case 2: {
    const uint32 c1 = s[1] & 0b00111111;
    return make_pair(uint32((*s & 0b00011111) << 6) | c1, 2);
  }
  case 3: {
    const uint32 c1 = s[1] & 0b00111111;
    const uint32 c2 = s[2] & 0b00111111;
    return make_pair((((uint32(*s & 0b00001111) << 6) | c1) << 6) | c2,
		     3);
  }
  case 4: {
    const uint32 c1 = s[1] & 0b00111111;
    const uint32 c2 = s[2] & 0b00111111;
    const uint32 c3 = s[3] & 0b00111111;    
    return make_pair((((((uint32(*s & 0b00000111) << 6) | c1) << 6) |
		       c2) << 6) | c3, 4);
  }
  }
}

static inline int Utf8Codepoints(const string &str) {
  int cps = 0;
  const char *s = str.data();
  // Allow for U+0000 in the string.
  const char *s_end = s + str.length();
  while (s != s_end) {
    auto p = GetUtf8(s);
    cps++;
    s += p.second;
  }
  return cps;
}

// This assumes that we have at most two-byte utf-8 encodings
// and that the string s is all valid.
static inline bool TerminatedUnicode(const char *s, int len) {
  if (len == 0) return true;

  // First byte can't be a multibyte continuation.
  if (IsUtf8ContinuationByte(s[0]))
    return false;
  
  if (IsUtf8MultibytePrefix(s[len - 1]))
    return false;

  // TODO: To handle 3- and 4-byte encodings, first check if the last
  // byte is a continuation byte, and if so, work backwards until
  // we find a prefix byte of the correct magnitude.
  return true;
}

struct Candidates {
  struct Compare {
    bool operator ()(const pair<string, int> &a,
		     const pair<string, int> &b) {
      // In deterministic mode, make sure this is a total order.
      if (DETERMINISTIC) {
	if (a.second == b.second) {
	  return a.first < b.first;
	} else {
	  return a.second > b.second;
	}
      } else {
	return a.second > b.second;
      }
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

// Returns the best substring (best = maximum value of (size *
// occurrences)) and the score.

// Use the table to find the best repeated strings from the given
// row.
static std::unique_ptr<Candidates> NewBestReplacement(
    const vector<vector<int>> &table,
    int source_length,
    uint8 row,
    const string &s) {

  // There can't be any candidates of value without at least two
  // occurrences of the character.
  if (table[row].size() <= 1)
    return {nullptr};

  // Also, the character cannot be in the middle of a multibyte
  // encoding.
  // PERF: Could just avoid collecting these?
  if (IsUtf8ContinuationByte(row))
    return {nullptr};

  auto local_candidates = std::make_unique<Candidates>(LOCAL_CANDIDATES);

  const int min_length = source_length + 1;

  // Each entry in this row of the table is a potential starting point
  // for a candidate. Focusing on a given index, this gives us a
  // stream of characters. As we extend the length of that stream, we
  // have some set of the indices that also match, and as we extend
  // the stream, this set tends to get smaller. At any time, we can
  // compute the value of that set and insert it as a candidate (PS:
  // need to think about overlap). When the set becomes a singleton
  // (i.e., it is just the focal index) then no more extension will
  // ever be useful, and we stop.

  // So, recursively, we take a set of indices into the string. These
  // indices are all known to indicate the same character.
  //
  //  - If the set is empty or a singleton, we are done.
  //  - Otherwise, for each index, get the character that follows it.
  //  - Invert this so that we have the set of distinct extending
  //    characters associated with the indices that continue that way.
  //  - Now, recurse over each of those continuations.

  auto Consider =
    [&](int i, int len) {
      // This should enumerate distinct strings, so this block
      // should not be necessary!

      /*
      CHECK(i + len < s.size());
      CHECK(TerminatedUnicode(s.c_str() + i, len));
      */

      /*
      CHECK(!local_candidates->Has(cand));
      */
      // PERF: In principle we could compute this as we go, although
      // overlaps are tricky.
      const int occ =
	TabledOccurrences(table, s, s.c_str() + i, (size_t)len);
      if (occ > 1) {
	int cost = 1 + source_length + len;
	int savings = occ * (len - source_length);
	int value = savings - cost;

	// PERF: Don't even insert it into hash set for local
	// candidates.
	const string cand = string(s.c_str() + i, (size_t)len);
	local_candidates->Add(cand, value);
      }
    };

  // Each index in the vector is one of the starting indices.
  // We keep the starting indices so that we can extract the
  // actual string at the end!
  std::function<void(int, int, const vector<int> &)> Rec =
      // Each index has the same string for at least len bytes.
    [&s, &local_candidates, &min_length, &Consider, &Rec](
	int len, int num_cps, const vector<int> &indices){
	// Nothing to do for singletons.
	if (indices.size() <= 1)
	  return;

	// Precondition.
	// PERF DEBUG!
	/*
	string str = s.substr(indices[0], len);
	for (int i : indices) {
	  CHECK(str == s.substr(i, len));
	}
	*/
	
	// Possible continuations, grouped by 32-bit codepoint.
	// Each continuation has the encoded length of the codepoint
	// and the vector of next indices.
	std::unordered_map<uint32, std::pair<uint8, vector<int>>> conts;

	// Try continuing each string. Each extension may produce
	// a different length due to multi-byte encodings.
	for (int idx : indices) {
	  int next = idx + len;
	  // Just ignore it if we fall outside the string.
	  if (next >= s.size())
	    continue;
	  uint32 cp; uint8 cp_len;
	  std::tie(cp, cp_len) = GetUtf8(s.data() + next);
	  auto &p = conts[cp];
	  p.first = cp_len;
	  p.second.push_back(idx);
	}

	if (len >= min_length &&
	    (conts.size() != 1 || num_cps == MAX_DEST_CODEPOINTS - 1)) {
	  // If the string has multiple different continuations,
	  // then we need to consider the current string, because
	  // this may be a global best for (length * number of
	  // occurrences). (Or put another way, if there is just
	  // one way the string continues, then this string is
	  // strictly worse than its continuation.)
	  //
	  // (XXX: This looks incorrect when one of the indices
	  // is excluded above because it ends the string...)
	  //
	  // If we've reached the max dest length, also consider
	  // the string, because we won't go any deeper.
	  
	  // We can use any index because they're all the same.
	  Consider(indices[0], len);
	}

	// In any case, recurse on all continuations.
	if (num_cps < MAX_DEST_CODEPOINTS - 1) {
	  for (const auto &p : conts) {
	    const int cont_len = len + p.second.first;
	    Rec(cont_len, num_cps + 1, p.second.second);
	  }
	}
      };
	

  // Everything shares a zero-length prefix (which is also 0
  // codepoints).
  Rec(0, 0, table[row]);

  return local_candidates;
}


// v must be length 256. v[c] contains a vector of all the positions
// in s, in ascending sorted order, where the character c is.
// Note: Doesn't bother collecting utf-8 continuation bytes, since
// these can never start a valid candidate.
static void MakeTable(const string &s, vector<vector<int>> *v) {
  for (vector<int> &vv : *v) vv.clear();
  for (int i = 0; i < s.size(); i++) {
    uint8 c = s[i];
    if (!IsUtf8ContinuationByte(c)) {
      (*v)[c].push_back(i);
    }
  }
}

// XXX need to support beyond two-byte encodings!
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

// encoded_reps output parameter is a hack for experimenting with
// alternate coding of these. Only works in one of the decoder cases...
static void WriteFile(const char *filename,
		      const string &data,
		      const vector<pair<string, string>> &reps,
		      string *encoded_reps) {
  // Self-checks
  if (ONLY_PRINTABLE) {
    auto PrintableStr =
      [&](const char *where, const string &s) {
	for (int i = 0; i < s.size(); i++) {
	  CHECK(s[i] >= ' ' && s[i] <= '~') << where << ": " << (int)s[i];
	}
      };

    PrintableStr("data", data);
    for (const pair<string, string> &p : reps) {
      PrintableStr("rep src", p.first);
      PrintableStr("rep dst", p.second);
    }
  }

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
	 (string)
	 "0123456789"
	 "qwertyuiopasdfghjklzxcvbnm"
	 "QWERTYUIOPASDFGHJKLZXCVBNM"
	 "!@#$%^&*()_+[]{}|:;<>?,./ ") {
    // PERF: Consider low ascii too. It just needs to be a single
    // byte.
    if (!InReps(c)) { sep = c; break; }
  }

  bool no_digit_in_source = true;
  int max_dest_cps = 0;
  int min_dest_cps = -1;
  int64 src_bytes = 0LL, dst_bytes = 0LL;
  for (const auto &p : reps) {
    src_bytes += p.first.size();
    dst_bytes += p.second.size();
    const int cps = Utf8Codepoints(p.second);
    max_dest_cps = std::max(max_dest_cps, cps);
    if (min_dest_cps < 0) min_dest_cps = cps;
    else min_dest_cps = std::min(min_dest_cps, cps);
    for (char c : (string)"0123456789") {
      if (p.first.find(c) != string::npos) {
	fprintf(stderr, "Source with digit: [%s] = %02x\n",
		Encode(p.first).c_str(), (int)c);
	no_digit_in_source = false;
      }
    }
  }

  // PERF should compute this, because we have a simpler and smaller
  // decoder if this is the case.
  bool all_onecodepoint_source = ONLY_ONECODEPOINT_SOURCES;
  int cp_span = max_dest_cps - min_dest_cps;

  if (VERBOSE >= 1) {
    fprintf(stderr,
	    "Sep: 0x%02x\n"
	    "all one codepoint: %s\n"
	    "no digit in source: %s\n"
	    "bytes used for sources: %lld\n"
	    "bytes used for dests: %lld\n"
	    "dest cp span: max %d - min %d = %d\n",
	    sep,
	    all_onecodepoint_source ? "true" : "false",
	    no_digit_in_source ? "true" : "false",
	    src_bytes, dst_bytes,
	    max_dest_cps,
	    min_dest_cps,
	    cp_span);
  }

  
  // For the prefix phase of decoding, need some variable that
  // is the empty string. Some reps phases create such a string, so
  // store the variable here if so. Otherwise, make a new one.
  string empty_string_var;
  
  // Note the use of fwrite below instead of fprintf with %s, since
  // the strings may include 0.
  if (reps.empty()) {
    // s already contains the desired string.
    // This mainly happens with USE_REPS is false, like for debugging.

    empty_string_var = "w";
    fprintf(out, "w='';");
    
  } else if (!all_onecodepoint_source &&
	     no_digit_in_source &&
	     cp_span < 10) {

    // PERF can save the +2 etc. if there's no need to offset!
    int base_cp_length = min_dest_cps;
    CHECK(max_dest_cps - base_cp_length < 10);
    
    // Can't assume that o[0] is the source.
    // Instead we use a digit to separate. 
    fprintf(out, "z=/(\\D+)(\\d)(.*)/;"
	    "r='");
    for (int i = reps.size() - 1; i >= 0; i--) {
      CHECK(1 == fwrite(reps[i].first.data(), reps[i].first.size(), 1, out));
      // Must be *codepoints*, not bytes.
      int d = Utf8Codepoints(reps[i].second) - base_cp_length;
      fprintf(out, "%d", d);
      CHECK(1 == fwrite(reps[i].second.data(), reps[i].second.size(), 1, out));
      if (encoded_reps != nullptr) {
	*encoded_reps += reps[i].first;
	*encoded_reps += StringPrintf("%d", d);
	*encoded_reps += reps[i].second;
      }
    }

    // expression like +2 to modify the length. Also need to explicitly
    // coerce the string to a number in this case, since '1'+2 is annoyingly
    // "12" and not 3. (But +'1'+2 is ok)
    string plus_min_pre, plus_min;
    if (base_cp_length > 0) {
      plus_min_pre = "+";
      plus_min = StringPrintf("+%d", base_cp_length);
    }
    
    fprintf(out, "';"
	    "while(a=z.exec(r))"
	    // a[1] is source
	    // a[3] is rest of string, starting with dest
	    // n is length
	    "s=s.split(a[1]).join(a[3].substr(0,n=%sa[2]%s)),"
	    "r=a[3].slice(n);", // update for next pass
	    plus_min_pre.c_str(),
	    plus_min.c_str());
    empty_string_var = "r";

  } else if (all_onecodepoint_source && sep != 0) {
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

    empty_string_var = "w";
    fprintf(out, "w='';");
    
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

    empty_string_var = "w";
    fprintf(out, "w='';");

  } else {
    // This general case is basically a fallback.
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
	    2 * (int)reps.size());

    empty_string_var = "w";
    fprintf(out, "w='';");
  }

  if (USE_PREFIX) {
    if (MAX_PREFIX_9 && !SUFFIX_ENCODING) {
      fprintf(out,
	      "for(c of s)+c+1?%s=console.log(%s)||%s.substr(0,+c):%s+=c",
	      empty_string_var.c_str(),
	      empty_string_var.c_str(),
	      empty_string_var.c_str(),
	      empty_string_var.c_str());
    } else {

      if (SUFFIX_ENCODING) {
	fprintf(out,
		"e=/(\\D+)(\\d+)/g;"
		// Unfortunately can't use negative second argument here
		// because -0 is not distinct from 0, bleh
		"while(a=e.exec(s))console.log(%s=%s.substr(0,%s.length-a[2])+a[1])",
		empty_string_var.c_str(),
		empty_string_var.c_str(),
		empty_string_var.c_str());
      } else {
	fprintf(out,
		"e=/(\\D+)(\\d+)/g;"
		"while(a=e.exec(s))console.log(%s=%s.substr(0,a[2])+a[1])",
		empty_string_var.c_str(),
		empty_string_var.c_str());
      }
    }
  } else {
    if (USE_DELTA) {
      fprintf(out,
	      // PERF this can be tightened up obviously
	      "p='';"
	      "for(o of s.split(',')){"
	      "n='';"
	      "for(x=0;x<o.length;x++)"
	      "n+=(x<p.length)?"
	      "String.fromCharCode(97+(((p.charCodeAt(x)-97)+(o.charCodeAt(x)-97))%%26))"
	      ":o[x];"
	      "console.log(p=n)"
	      "}");
    } else {
      // Otherwise, expect a comma-separated wordlist in s.
      fprintf(out, "console.log(s.replace(/,/g,'\\n'))");
    }
  }
    
  fclose(out);
}

string CharString(char c) {
  string ch = " ";
  ch[0] = c;
  return ch;
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

  // For long wordslists, three-byte utf-8 is even useful.
  if (!ONLY_ASCII) {
    // PERF: We never run out of sources even on the long wordlist,
    // so probably should just limit this for efficiency.
    for (int i = 0x800; i < 0x10000; i++) {
      if (i != 0x2028 && i != 0x2029) {
	uint8 b1 = 0b1110'0000;
	uint8 b2 = 0b10'000000;
	uint8 b3 = 0b10'000000;
	b1 |= (i >> 12) & 0b1111;
	b2 |= (i >> 6) & 0b111111;
	b3 |= i & 0b111111;
	string s = StringPrintf("%c%c%c", b1, b2, b3);
	sources.push_back(s);
      }
    }
    
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
  }
  
  // Single-character codes come last, since these will be used for
  // more common matches in the greedy algorithm below.

  // TODO PERF can include the two-byte \\ and \'. They are at least
  // as good as utf-16, though tricky to handle in some ways (and we
  // have to be careful about splitting them, like with multibyte utf)
  //
  // , and space are left out here; added below if applicable.
  string sourcechars =
    "\"ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-=+[]{}|;:<>?./~`";
  for (char c : sourcechars) {
    CHECK(c != 0);
    sources.push_back(CharString(c));
  }

  // Space is excluded from the above, so that we can reserve it as
  // a separator if forced to do so.
  if (!FORCE_SEP) sources.push_back(" ");
  
  // And low ASCII.
  if (!ONLY_PRINTABLE) {
    for (uint8 c = 0; c < 32; c++) {
      // Only carriage return and newline are disallowed here.
      if (c != 0x0A && c != 0x0D) {
	sources.push_back(CharString(c));
      }
    }
  }

  if (USE_PREFIX) {
    sources.push_back(",");
  } else {
    // XXX reps encoding is very inefficient if sources can include
    // digits.
    /*
    for (char c : (string)"0123456789") {
      CHECK(c != 0);
      sources.push_back(CharString(c));
    }
    */
  }
  
  return sources;
}

template<class T>
void Reverse(vector<T> *v) {
  vector<string> tmp;
  tmp.reserve(v->size());
  for (int i = v->size() - 1; i >= 0; i--)
    tmp.push_back(std::move((*v)[i]));
  *v = std::move(tmp);
}

static pair<string, vector<pair<string, string>>> Optimize(
    // Exclusive access.
    ArcFour *rc,
    string data,
    vector<string> sources) {

  if (!USE_REPS)
    return {data, {}};

  vector<pair<string, string>> reps;
  
  if (!DETERMINISTIC) {
    vector<string> sources3;
    vector<string> sources2;
    vector<string> sources1;
    for (int i = 0; i < sources.size(); i++) {
      switch (sources[i].size()) {
      case 3:
	sources3.push_back(std::move(sources[i]));
	break;
      case 2:
	sources2.push_back(std::move(sources[i]));
	break;
      case 1:
	sources1.push_back(std::move(sources[i]));
	break;
      default:
	LOG(FATAL) << sources[i];
      }
    }
    sources.clear();
    
    if (rc->Byte() & 1) {
      Shuffle(rc, &sources3);
      Shuffle(rc, &sources2);
      Shuffle(rc, &sources1);
    } else {
      if (rc->Byte() < 64) Reverse(&sources3);
      if (rc->Byte() < 64) Reverse(&sources2);
      if (rc->Byte() < 64) Reverse(&sources1);
    }

    // XXX code below assumes these are mostly in descending order.
    // allow some way of scheduling length-1 ones later.
    auto PushAll =
      [&sources](vector<string> *dst) {
	for (string &s : *dst) {
	  sources.push_back(std::move(s));
	}
      };
    PushAll(&sources3);
    PushAll(&sources2);
    PushAll(&sources1);
  }
  
  
  vector<vector<int>> table;
  table.resize(256);

  #ifdef WRITE_LOG
  FILE *log = fopen("log.txt", "w");
  #endif
  

  for (int round = 0; true; round++) {
    if (MAX_REPS >= 0 && reps.size() > MAX_REPS)
      break;
    
    // vector<pair<string, int>> results;
    // results.reserve(INNER_THREADS);
        
    vector<string> tmp_sources;
    if (!ONLY_ONECODEPOINT_SOURCES &&
	(sources.empty() || sources.back().size() > 2)) {
      vector<bool> exists(128 * 128, false);
      uint8 b1 = data[0];
      for (int i = 1; i < data.size(); i++) {
	uint8 b2 = data[i];
	// We only care about two consecutive safe ascii characters,
	// so this is simpler than trying to skip over multibyte
	// encodings explicitly.
	if (IsSafeASCII(b1) && IsSafeASCII(b2)) {
	  exists[b1 * 128 + b2] = true;
	}
	b1 = b2;
      }
      
      int num_codes = 0;
      string examples;
      for (uint8 b1 = 0; b1 < 128; b1++) {
	if (!IsSafeASCII(b1))
	  continue;

	// Numerals aren't allowed in these sources, since we'll
	// need them for the encoding of the replacements string.
	if (b1 >= '0' && b1 <= '9')
	  continue;

	if (ONLY_PRINTABLE && (b1 < ' ' || b1 > '~'))
	  continue;

	if (b1 < MIN_ASCII)
	  continue;
	
	for (uint8 b2 = 0; b2 < 128; b2++) {
	  if (!IsSafeASCII(b2))
	    continue;

	  if (b2 >= '0' && b2 <= '9')
	    continue;

	  if (ONLY_PRINTABLE && (b2 < ' ' || b2 > '~'))
	    continue;
	  
	  // And don't use the forced separator, if enabled.
	  if (FORCE_SEP && (b1 == ' ' || b2 == ' '))
	    continue;

	  if (b2 < MIN_ASCII)
	    continue;
	  
	  // PERF: This case is currently disallowed, because if the
	  // source is e.g. AA and we'd like to do the reverse
	  // substitution XYZ<-AA, we actually can't do it if an
	  // occurrence of XYZ is actually AXYZA, because the
	  // resulting string would be AAAA, which would decode to
	  // XYZXYZ, not AXYZA as expected. Note this is not possible
	  // if the two code characters are different. We could add
	  // some codes here by handling this case.
	  if (!DUP_TMP_SOURCE_YOLO && b1 == b2)
	    continue;
	  
	  if (!exists[b1 * 128 + b2]) {
	    const string s = StringPrintf("%c%c", b1, b2);
	    
	    auto EncodeChar =
	      [](uint8 c) {
		if (c < 32) return StringPrintf("[%02x]", (int)c);
		else return StringPrintf("%c", c);
	      };
	    
	    if (SELF_CHECK) {
	      CHECK(data.find(s) == string::npos)
		<< EncodeChar(b1) << EncodeChar(b2) << " already present! ";
	    }

	    num_codes++;
	    if (VERBOSE > 3 && num_codes < 10) {
	      examples += " " + EncodeChar(b1) + EncodeChar(b2) + ", ";
	    }
	    // Can't just add these to sources list, because
	    // they may be invalidated as soon as data changes.
	    // (Note: No good reason to store ALL of these...)
	    tmp_sources.push_back(s);
	    // We'll never use more than this many.
	    if (tmp_sources.size() > BEST_PER_ROUND + 1)
	      goto enough_sources;
	  }
	}
      }

      enough_sources:;	
      if (VERBOSE > 2 && num_codes > 0) {
	fprintf(stderr, "%d unused codes, like %s ...\n",
		num_codes, examples.c_str());
      }
    }

    if (sources.empty() && tmp_sources.empty()) {
      if (VERBOSE >= 1)
	fprintf(stderr, "No more sources / tmp sources (up)...\n");
      goto nomore;
    }

    const int next_source_length =
      tmp_sources.empty() ? 
      sources.back().size() :
      tmp_sources.back().size();
    
    MakeTable(data, &table);

    std::mutex m;
    Candidates candidates(GLOBAL_CANDIDATES);
    ParallelComp(table.size(),
		 [&m, &candidates, &data, &table, next_source_length](int row) {
		   auto res =
		     NewBestReplacement(table, next_source_length, row, data);

		   // This often returns null for trivial cases (e.g.
		   // zero or one occurrence of the character).
		   if (res.get() != nullptr) {
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
    std::unique_ptr<vector<pair<string, int>>> results;
    results.reset(candidates.topn.Extract());
    
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
      if (!DETERMINISTIC && skip_allowed && rr < results->size() - 1) {
	double v1 = (*results)[rr].second;
	double v2 = (*results)[rr + 1].second;
	// Higher chance of staying if v1 is much larger than v2.
	double p = v1 / (v1 + v2);
	if (RandDouble(rc) < p) {
	  // Don't want to cause this random skip to exit!
	  made_progress = true;
	  continue;
	}
      }
      const string &dst = (*results)[rr].first;

      if (tried >= BEST_PER_ROUND)
	break;
      
      tried++;
      
      if (tmp_sources.empty() && sources.empty())
	break;

      // Recompute this because if we've done any replacements, in this
      // loop, the number of occurrences can easily change (for one
      // simple reason, two threads may find the same best replacement!)
      int occ = Occurrences(data, dst);
      // four quote chars (source and dest), two commas, source and
      // dest strings.
      // int cost = 4 + 2 + sources.back().size() + dst.size();
      // Assumes we can find a separator and that no replacement
      // is longer than 10 chars. (The encoding has one byte of overhead
      // regardless of whether we use the all_onecodepoint_source encodings
      // or not.)
      string src;
      bool used_tmp = false;
      if (!tmp_sources.empty()) {
	src = tmp_sources.back();
	tmp_sources.pop_back();
	used_tmp = true;
      } else {
	src = sources.back();
	sources.pop_back();
	used_tmp = false;
      }
      
      int cost = 1 + src.size() + dst.size();
      int savings = occ * (dst.size() - src.size());
      
      if (VERBOSE >= 4 || (VERBOSE >= 3 && savings > cost)) {
	fprintf(stderr,
		"%s<-%s%s has %d real occurrences cost %d savings %d.\n",
		Encode(dst).c_str(),
		Encode(src).c_str(), used_tmp ? " (TMP)" : "",
		occ, cost, savings);
      }
      if (savings > cost) {
	if (SELF_CHECK) {
	  CHECK(data.find(src) == string::npos) << Encode(src)
						<< " already present!";
	}
	  
	reps.push_back(std::make_pair(src, dst));

	if (SELF_CHECK) {
	  string old_data = data;
	  data = Util::Replace(data, dst, src);
	  CHECK(Util::Replace(data, src, dst) == old_data)
	    << Encode(dst) << "<-" << Encode(dst)
	    << " failed forward check.";
	} else {
	  data = Util::Replace(data, dst, src);
	}
	if (VERBOSE >= 3) {
	  int source_size = sources.empty() ? 0 : (int)sources.back().size();
	  fprintf(stderr, "Size now %d. %d source(s) left [size %d]\n",
		  (int)data.size(),
		  (int)sources.size(),
		  source_size);
	}
	made_progress = true;

	#ifdef WRITE_LOG
	/*
	fprintf(log, "'%s' <- '%s'%s has %d real occurrences, "
		"cost %d savings %d (now %d)\n",
		Encode(dst).c_str(),
		Encode(src).c_str(),
		used_tmp ? " (TMP)" : "",
		occ, cost, savings,
		(int)data.size());
	*/
	fprintf(log, "%s %d %s\n", src.c_str(),
		Utf8Codepoints(dst) - 2, dst.c_str());
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

static int HuffmanStats(const string &start_data) {
  Huffman huff;
  int freqs[256] = {};
  for (int i = 0; i < start_data.size(); i++)
    freqs[(uint8)start_data[i]]++;
  for (int i = 0; i < 256; i++)
    if (freqs[i] > 0)
      huff.AddSymbol(i, freqs[i]);
  /*
  if (VERBOSE >= 2)
    huff.PrintTree();
  */
  huff.MakeTree();
  /*
  if (VERBOSE >= 2)
    huff.PrintCodes();
  */
  vector<vector<bool>> codes = huff.MakeCodes();
  int64 total_bits = 0LL;
  for (int i = 0; i < start_data.size(); i++) {
    uint8 c = start_data[i];
    CHECK(c < codes.size());
    const vector<bool> &code = codes[c];
    CHECK(!code.empty()) << (int)c << " = " << (char)c;
    total_bits += code.size();
  }
  if (VERBOSE >= 1)
    fprintf(stderr,
	    "Input size: %d bytes\n"
	    "Total huffman bits: %lld = %lld 8-bit bytes\n"
	    " = %.2f base 124 (ideal)\n",
	    (int)start_data.size(),
	    total_bits, (total_bits >> 3) + 1,
	    total_bits / 6.9542);

  return total_bits / 6.9542;
}

struct FreqEncoder : public ArithEncoder {
  char ToChar(int s) {
    if (s >= 0x0a) s++;
    if (s >= 0x0d) s++;
    if (s >= '\\') s++;
    if (s >= '\'') s++;
    return s;
  }
  int ToSymbol(char c) {
    CHECK(c >= 0 && c < 128) << c;
    CHECK(c != 0x0a && c != 0x0d && c != '\\' && c != '\'') << c;
    int shift = 0;
    if (c > 0x0a) shift++;
    if (c > 0x0d) shift++;
    if (c > '\\') shift++;
    if (c > '\'') shift++;
    return (int)c - shift;
  }

  vector<int> MakeSymbols(const string &s) {
    vector<int> ret;
    ret.reserve(s.size());
    for (char c : s) ret.push_back(ToSymbol(c));
    return ret;
  }
  
  // No history; just use frequency table.
  FreqEncoder(const string &word) : ArithEncoder(0, 124, 124, 2) {
    vector<int> counts(nsymbols, 0);
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
      char c = ToChar(i);
      if (i >= ' ' && i < 127) printf("%c", c);
      else printf("[%d]", c);
      printf(" x %d = %.5f\n",
	     freqs[i].second, freqs[i].second / (double)(ipow(B, W)));
    }
    fflush(stdout);
  }
  
  vector<pair<int, int>> Predict(const deque<int> &hist) override {
    return freqs;
  }

  vector<pair<int, int>> freqs;
};


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

  if (REVERSE_WORDS) {
    vector<string> reversed;
    reversed.reserve(words.size());
    for (const string &w : words) {
      string r;
      r.resize(w.size());
      for (int i = 0; i < r.size(); i++) {
	r[i] = w[w.size() - i - 1];
      }
      reversed.push_back(std::move(r));
    }
    std::sort(reversed.begin(), reversed.end());
    words = std::move(reversed);
  }
  
  const string start_data =
    USE_PREFIX ? PrefixEncode(words, MAX_PREFIX_9, SUFFIX_ENCODING) :
    USE_DELTA ? Util::Join(DeltaEncode(words), ",") :
    Util::Join(words, ",");

  if (VERBOSE >= 1)
    fprintf(stderr, "Encoded data size: %d\n", (int)start_data.size());
  if (VERBOSE >= 1) {
    HuffmanStats(start_data);
  }
  
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
	WriteFile(argv[2], best_data, best_reps, nullptr);
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
  
  string encoded_reps;
  WriteFile(argv[2], best_data, best_reps, &encoded_reps);

  int huffman_data = HuffmanStats(best_data);
  int huffman_reps = HuffmanStats(encoded_reps);

  // Use the same frequency table for both. XXX test separate!
  FreqEncoder encoder(best_data + encoded_reps);

  vector<int> syms_data = encoder.MakeSymbols(best_data);
  vector<int> syms_reps = encoder.MakeSymbols(encoded_reps);

  static constexpr int MAX_CHUNK = 1000;

  auto EncodeChunks =
    [&encoder](const vector<int> &input) {
      vector<vector<int>> chunks;

      int idx = 0;
      while (idx < input.size()) {
	vector<int> encodeme;
	for (int i = 0; i < MAX_CHUNK; i++) {
	  if (idx + i >= input.size())
	    break;
	  encodeme.push_back(input[idx + i]);
	}
	idx += encodeme.size();
	chunks.push_back(std::move(encodeme));
      }

      printf("%d input -> %d chunks.\n",
	     (int)input.size(),
	     (int)chunks.size());
      fflush(stdout);
      
      // Now, in parallel.
      vector<int> sizes =
	ParallelMapi(
	    chunks,
	    [&chunks, &encoder](int idx, const vector<int> &symbols) {
	      Big res = encoder.Encode(symbols);
	      
	      printf("\n\n"
		     "[%d/%d] Arith Encoded %d -> %d\n\n",
		     idx, (int)chunks.size(),
		     (int)symbols.size(), (int)res.numer.size());
	      fflush(stdout);
	      return (int)res.numer.size();
	    },
	    24);

      int result_bytes = 0;
      for (int s : sizes) result_bytes += s;
      return result_bytes;
    };

  /*
  Big arith_data = encoder.Encode(syms_data);
  Big arith_reps = encoder.Encode(syms_reps);
  printf("Bytes with freqencoder: %d + %d = %d\n",
	 (int)arith_data.numer.size(),
	 (int)arith_reps.numer.size(),
	 (int)arith_data.numer.size() +
	 (int)arith_reps.numer.size());
  */

  int asize_data = EncodeChunks(syms_data);
  int asize_reps = EncodeChunks(syms_reps);

  fflush(stdout);
  fflush(stderr);
  printf("-------------------------------------------------\n");
  
  printf("Original payload bytes: %d + %d = %d\n",
	 (int)best_data.size(), (int)encoded_reps.size(),
	 (int)best_data.size() + (int)encoded_reps.size());
  printf("Ideal bytes with huffman: %d + %d = %d\n",
	 huffman_data, huffman_reps, huffman_data + huffman_reps);
  printf("Bytes with freqencoder: %d + %d = %d\n",
	 asize_data, asize_reps, asize_data + asize_reps);

  return 0;
}
