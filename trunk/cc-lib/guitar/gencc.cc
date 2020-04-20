// Rather than depend on the .json file (big) and JSON libraries
// (big, painful), this generates a single .cc file with the
// data embedded. Not only is that easier to use, but the data
// only occupies 13.5kb, which is probably less than the compiled JSON
// parser code!

#include <stdio.h>
#include <unordered_map>
#include <string>
#include <vector>

#include "rapidjson/document.h"

#include "base/stringprintf.h"
#include "base/logging.h"
#include "util.h"

// Note: This depends on guitar.h for some constants that are
// shared. But it cannot link guitar.cc because it GENERATES
// guitar.cc!
#include "guitar.h"

#if !RAPIDJSON_HAS_CXX11_RANGE_FOR
#error RAPIDJSON should be configured with ranged-for support!
#endif

// Emit the declarations, but also save as a string constant
// so that we can emit to the generated .cc file as well.
#define DECL_AND_SAVE(sym, decls) \
  static constexpr char sym [] = #decls; \
  decls

// Encoding is based on a mapping from 0-65 to printable
// characters.

// For each chord we store its fingering. We want to be
// pretty economical in both the size of the generated
// code and the symbols in the executable, so one good way
// to do this is with a string literal.
// Fingering is six fingers (Eadgbe) which can
// either be x (mute) or 0 (open) through ~20 (fretted).

DECL_AND_SAVE(
    SHARED_DECLS,
    [[maybe_unused]]
    static constexpr int RADIX = 66;

    [[maybe_unused]]
    static int CharNum(char c) {
      if (c >= ',' && c <= '9') return c - ',';
      if (c >= 'A' && c <= 'Z') return (c - 'A') + 14;
      if (c >= 'a' && c <= 'z') return (c - 'a') + 14 + 26;
      return -1;
    });
	      
static char NumChar(int n) {
  CHECK(n >= 0 && n < RADIX) << n;
  return ",-./"
    "0123456789"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"[n];
}

static void TestChars() {
  for (int i = 0; i < RADIX; i++) {
    char c = NumChar(i);
    CHECK(i == CharNum(c));
  }
}

// TODO: We could save a few bytes by encoding 6 fingers
// with fewer total characters.
static char FingerChar(int f) {
  CHECK(f >= -1 && f <= 20) << f;
  return NumChar(f + 1);
}

static unordered_map<string, int> FromVec(const vector<string> &name) {
  unordered_map<string, int> num;
  for (int i = 0; i < (int)name.size(); i++) {
    CHECK(num.find(name[i]) == num.end()) << name[i];
    num[name[i]] = i;
  }
  return num;
}

template<size_t N>
static vector<string> FromArray(const std::array<const char *, N> &arr) {
  vector<string> v;
  v.reserve(N);
  for (int i = 0; i < (int)N; i++) v.emplace_back(arr[i]);
  return v;
}

// Bases in canonical order.
struct Bases {
  Bases() : name(FromArray(Guitar::BASES)), num(FromVec(name)) {}
  int Num(const string &s) const {
    auto it = num.find(s);
    CHECK(it != num.end()) << "[" << s << "]";
    return it->second;
  }
  const vector<string> name;
  const unordered_map<string, int> num;
};

// Suffixes in canonical order.
struct Suffixes {
  Suffixes() : name(FromArray(Guitar::SUFFIXES)), num(FromVec(name)) {}
  int Num(const string &s) const {
    auto it = num.find(s);
    CHECK(it != num.end()) << "[" << s << "]";
    return it->second;
  }
  const vector<string> name;
  const unordered_map<string, int> num;
};

// Input data spells out "major" and "minor" which I think is wrong.
// We use "" and "m" in this library.
static string NormalizeSuffix(string_view s) {
  if (s == "major") return "";
  if (s == "minor") return "m";
  return (string)s;
}

// XXX should be a direct way to do this with rapidjson?
template<class T>
static int NumKeys(const T &t) {
  int ret = 0;
  for ([[maybe_unused]] const auto &unused : t) ret++;
  return ret;
}

static string FixKey(const string &s) {
  // JSON keys can't use '#' I guess, so it's spelled out here.
  return Util::Replace(s, "sharp", "#");
}

static void Load() {
  printf("Start\n");
  fflush(stdout);

  Bases bases;
  Suffixes suffixes;
  
  using namespace rapidjson;
  Document document;
  string guitarfile = Util::ReadFile("guitar.json");
  CHECK(!document.Parse(guitarfile.c_str()).HasParseError());

  string out;
  
  CHECK(document.HasMember("chords"));
  Value &c = document["chords"];
  Value::Object obj = c.GetObject();
  printf("... next ...\n");

  const int num_chords = NumKeys(obj);
  out += NumChar(num_chords);
  
  for (const auto &chord_it : obj) {
    const string &base = FixKey(chord_it.name.GetString());
    out += NumChar(bases.Num(base));
    
    // Now for each base, all the suffixes.
    CHECK(chord_it.value.IsArray());
    const auto &sarray = chord_it.value.GetArray();
    const int num_suffix = NumKeys(sarray);
    printf("%s has %d suffixes\n", base.c_str(), num_suffix);
    out += NumChar(num_suffix);

    // Now, each suffix...
    for (const auto &suffix_it : sarray) {
      CHECK(suffix_it.IsObject());
      const auto &suffix_obj = suffix_it.GetObject();
      CHECK(suffix_obj.HasMember("key") &&
	    suffix_obj.HasMember("suffix") &&
	    suffix_obj.HasMember("positions"));
      CHECK_EQ(suffix_obj["key"].GetString(), base);
      const string suffix =
	NormalizeSuffix(suffix_obj["suffix"].GetString());
      const auto &positions_obj = suffix_obj["positions"];
      CHECK(positions_obj.IsArray());
      const auto &positions_arr = positions_obj.GetArray();
      int num_positions = NumKeys(positions_arr);
      CHECK(num_positions > 0);
      
      out += NumChar(suffixes.Num(suffix));
      out += NumChar(num_positions);

      // Now, each position...
      for (const auto &pos_it : positions_arr) {
	CHECK(pos_it.IsObject());
	const auto &pos_obj = pos_it.GetObject();
	// Also included here:
	//  capo: If true, then open fingering is played
	//   at the base fret (where the capo is). Only
	//   affects open fingering.
	//   (XXX: Verify this. The SVGs at e.g.
	//    https://tombatossals.github.io/react-chords/guitar/D/m11
	//    suggest this, but it seems weird to me.)
	//  barres: just informational; this is included
	//   in the fingering.
	//  fingers: which finger to use. don't tell me
	//   which finger to use!!
	//  midi: Probably the absolute midi note. Skips
	//   muted strings, so not useful for this.
	CHECK(pos_obj.HasMember("baseFret") &&
	      pos_obj["baseFret"].IsNumber() &&
	      pos_obj.HasMember("frets") &&
	      pos_obj["frets"].IsArray());

	// Note that base_fret is 1 in home position. We compute
	// a 0-based fret offset (added to any fingered fret)
	// and the effective open fret (
	const auto [fret_offset, open_fret] = [&]{
	    const bool capo = pos_obj.HasMember("capo") &&
	      pos_obj["capo"].GetBool();
	    
	    const int base_fret = pos_obj["baseFret"].GetInt();
	    CHECK(base_fret > 0);
	    return make_pair(base_fret - 1, capo ? base_fret : 0);
	  }();
	
	vector<int> fingers;
	fingers.reserve(6);
	for (const auto &f : pos_obj["frets"].GetArray()) {
	  CHECK(f.IsNumber());
	  const int fn = f.GetInt();
	  // fn of -1 means mute, 0 means open.
	  if (fn == -1) fingers.push_back(-1);
	  else if (fn == 0) fingers.push_back(open_fret);
	  else fingers.push_back(fret_offset + fn);
	}
	CHECK_EQ(fingers.size(), 6);

	for (int f : fingers) out += FingerChar(f);
      }
    }
  }

  // printf("%s\n", out.c_str());
  printf("%d bytes\n", (int)out.size());

  string source = "// Generated file! Do not edit.\n";

  source += "#line 1 \"guitar-head.cc\"\n";
  source += Util::ReadFile("guitar-head.cc");
  
  source += "\n#line 1 \"(generated)\"\n";
  source += SHARED_DECLS;

  // TODO: Break into tidy lines?
  StringAppendF(&source, "\nstatic constexpr char DATA[] = \"%s\";\n",
		out.c_str());

  source += "#line 1 \"guitar-tail.cc\"\n";
  source += Util::ReadFile("guitar-tail.cc");
  
  Util::WriteFile("guitar.cc", source);
  
  fflush(stdout);
}


int main(int argc, char **argv) {
  TestChars();
  printf("Um..\n");
  Load();
  return 0;
}
