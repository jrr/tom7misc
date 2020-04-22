
#include <string>
#include <vector>

#include "guitar.h"

#include "base/logging.h"
#include "util.h"

using namespace std;
using Fingering = Guitar::Fingering;
using Chord = Guitar::Chord;

static constexpr Fingering c_maj = make_tuple(-1, 3, 2, 0, 1, 0);
static constexpr Fingering c_add9 = make_tuple(-1, 3, 2, 0, 3, 0);

static void TestParse() {
  CHECK(Guitar::Parse("C#sus4").has_value());
  CHECK(!Guitar::Parse("asdf").has_value());
  CHECK(!Guitar::Parse("").has_value());
  CHECK(Guitar::Parse("C").has_value());
  CHECK_EQ(Guitar::Parse("Cmajor"),
	   Guitar::Parse("Cmaj"));
  CHECK(Guitar::Parse("Cmajor") !=
	Guitar::Parse("Cmaj5"));
  CHECK_EQ(Guitar::Parse("Gb9#11"),
	   Guitar::Parse("F#9#11"));
}

static bool HasFingering(vector<Fingering> fs, Fingering fing) {
  for (const auto &ff : fs) {
    auto [a, b, c, d, e, f] = ff;
    printf("%d %d %d %d %d %d\n", a, b, c, d, e, f);
    if (fing == ff) return true;
  }
  return false;
}

static void TestFingering() {
  string c_maj_s = Guitar::FingeringString(c_maj);
  CHECK_EQ(c_maj_s, "x32010") << c_maj_s;
  CHECK_EQ(Guitar::FingeringString(c_add9), "x32030");
  CHECK_EQ(Guitar::FingeringString(
	       make_tuple(10, 11, 12, -1, 0, 1)), "abcx01");
  {
    optional<Guitar::Chord> co = Guitar::Parse("C");
    CHECK(co.has_value());
    vector<Fingering> f = Guitar::GetFingerings(*co);
    CHECK(!f.empty());
    CHECK(HasFingering(f, make_tuple(-1, 3, 2, 0, 1, 0)));
  }

  {
    optional<Guitar::Chord> co = Guitar::Parse("Cadd9");
    CHECK(co.has_value());
    vector<Fingering> f = Guitar::GetFingerings(*co);
    CHECK(!f.empty());
    CHECK(HasFingering(f, make_tuple(-1, 3, 2, 0, 3, 0)));
    CHECK(HasFingering(f, make_tuple(8, 7, 0, 0, 8, 0)));
  }
}

static void TestRender() {
  for (int b = 0; b < Guitar::NUM_BASES; b++) {
    for (int s = 0; s < Guitar::NUM_SUFFIXES; s++) {
      const Chord c = Guitar::ChordOf(b, s);
      const string str = Guitar::ChordString(c);
      const optional<Chord> co = Guitar::Parse(str);
      CHECK(co.has_value()) << str;
      // Round trip should give the same result.
      CHECK(c == *co) << str;
    }
  }
}

int main(int argc, char **argv) {
  TestParse();
  TestFingering();
  TestRender();
  printf("OK\n");
  return 0;
}
