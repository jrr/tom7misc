/* PLEASE KEEP THIS LINE */
// (The previous line is used by the test, which opens this test file
// as text.)

#include "base/logging.h"
#include "util.h"
#include <stdio.h>
#include <cmath>

using namespace std;

static string SlowReadFile(const string &filename) {
  FILE *f = fopen(filename.c_str(), "rb");
  if (!f) return "";
  string ret;
  ret.reserve(128);
  int c = 0;
  while (EOF != (c = getc(f))) {
    ret += (char)c;
  }
  fclose(f);
  return ret;
}

static constexpr char NONEXISTENT_FILE[] =
  "util_test_DOESNT_EXIST.cc";


// This test uses its source code as test data, so don't
// mess with the lines that tell you to keep them, duh.
static void TestReadFiles() {
  const string reference = SlowReadFile("util_test.cc");
  // Self-check.
  CHECK(reference.find("KEEP THIS LINE TOO */") != string::npos);
  CHECK(reference.find("/* PLEASE KEEP THIS LINE */") == 0);
  const string s1 = Util::ReadFile("util_test.cc");
  CHECK_EQ(reference, s1);
  const string s2 = Util::ReadFileMagic("util_test.cc",
					"/* PLEASE KEEP THIS");
  CHECK_EQ(reference, s2);
  CHECK_EQ("", Util::ReadFileMagic("util_test.cc", "WRONG"));
  CHECK_EQ("", Util::ReadFile(NONEXISTENT_FILE));
  CHECK_EQ("", Util::ReadFileMagic(NONEXISTENT_FILE, "/"));
  CHECK(Util::HasMagic("util_test.cc", "/* PLEASE KEEP THIS"));
  CHECK(!Util::HasMagic("util_test.cc", "* PLEASE KEEP"));
  // Would be nice to test files larger than 2^31 and 2^32, since these
  // have caused problems in the past.

  std::optional<string> os3 = Util::ReadFileOpt("util_test.cc");
  CHECK(os3.has_value());
  CHECK_EQ(reference, *os3);

  std::optional<string> os4 = Util::ReadFileOpt(NONEXISTENT_FILE);
  CHECK(!os4.has_value());
}

static void TestWhitespace() {
  CHECK_EQ("", Util::LoseWhiteR(""));
  CHECK_EQ("", Util::LoseWhiteR(" \n\r \n"));
  CHECK_EQ(" p", Util::LoseWhiteR(" p \n\r \n"));
  CHECK_EQ("  p\nq", Util::LoseWhiteR("  p\nq\n\r \n"));
  CHECK_EQ("rrr", Util::LoseWhiteR("rrr"));

  CHECK_EQ("", Util::NormalizeWhitespace(""));
  CHECK_EQ("", Util::NormalizeWhitespace(" "));
  CHECK_EQ("", Util::NormalizeWhitespace(" \r\n \r \r"));
  CHECK_EQ("hello world", Util::NormalizeWhitespace("  \nhello \r\n \r "
						    "\rworld  \r\r\n"));
  CHECK_EQ("hello world", Util::NormalizeWhitespace("hello world"));
  CHECK_EQ("hello world", Util::NormalizeWhitespace("\thello\tworld\t"));
  string s;
  CHECK_EQ("", Util::NormalizeWhitespace(s));
}

static void TestPad() {
  CHECK_EQ("", Util::Pad(0, ""));
  CHECK_EQ("hello  ", Util::Pad(7, "hello"));
  CHECK_EQ("  hello", Util::Pad(-7, "hello"));
  CHECK_EQ("hello", Util::Pad(4, "hello"));
  CHECK_EQ("hello", Util::Pad(-4, "hello"));

  CHECK_EQ("", Util::PadEx(0, "", '_'));
  CHECK_EQ("hello__", Util::PadEx(7, "hello", '_'));
  CHECK_EQ("__hello", Util::PadEx(-7, "hello", '_'));
  CHECK_EQ("hello", Util::PadEx(4, "hello", '_'));
  CHECK_EQ("hello", Util::PadEx(-4, "hello", '_'));
}

static void TestJoin() {
  CHECK_EQ("", Util::Join({}, "X"));
  CHECK_EQ("aYYbYYcde", Util::Join({"a", "b", "cde"}, "YY"));
  CHECK_EQ("abcde", Util::Join({"a", "b", "cde"}, ""));
}

static void TestSplit() {
  CHECK_EQ((vector<string>{"hello", "world"}),
	   Util::Split("hello world", ' '));
  CHECK_EQ((vector<string>{"", ""}), Util::Split(" ", ' '));  
  CHECK_EQ(vector<string>{""}, Util::Split("", 'x'));
}

static void TestCdup() {
  // Note: Failures here could be because util_test.o and util.o have
  // different values of DIRSEP. Probably should make this more sane!
  CHECK_EQ("abc" DIRSEP "de", Util::cdup("abc" DIRSEP "de" DIRSEP "f"));
  CHECK_EQ(".", Util::cdup("abc"));
}

static void TestPrefixSuffix() {
  CHECK(Util::StartsWith("anything", ""));
  CHECK(!Util::StartsWith("", "nonempty"));
  CHECK(Util::EndsWith("anything", ""));
  CHECK(!Util::EndsWith("", "nonempty"));

  CHECK(Util::StartsWith("food processor", "foo"));
  CHECK(!Util::StartsWith("food processor", "doo"));
  CHECK(Util::EndsWith("food processor", "sor"));
  CHECK(!Util::EndsWith("food processor", "sdr"));

  {
    // String versions.
    string s = "food processor";
    CHECK(!Util::TryStripPrefix("foods", &s));
    CHECK(Util::TryStripPrefix("foo", &s) &&
	  s == "d processor");
    CHECK(!Util::TryStripSuffix("sord", &s));
    CHECK(Util::TryStripSuffix("sor", &s) &&
	  s == "d proces");
  }

  {
    // string_piece versions
    string_view s = "food processor"sv;
    CHECK(!Util::TryStripPrefix("foods", &s));
    CHECK(Util::TryStripPrefix("foo", &s) &&
	  s == "d processor");
    CHECK(!Util::TryStripSuffix("sord", &s));
    CHECK(Util::TryStripSuffix("sor", &s) &&
	  s == "d proces");
  }
}

static void TestWriteFiles() {
  const string f = "util_test.deleteme";
  const string ss = "the\n\0contents";
  CHECK(Util::WriteFile(f, ss));
  string ssr = Util::ReadFile(f);
  CHECK(ss == ssr);

  CHECK(Util::WriteFile(f, ""));
  string sse = Util::ReadFile(f);
  CHECK(sse.empty());

  vector<string> lines = {
    "the first line",
    "",
    "",
    " white - space ",
  };

  CHECK(Util::WriteLinesToFile(f, lines));
  vector<string> rlines = Util::ReadFileToLines(f);
  CHECK(lines == rlines);
}

static void TestParseDouble() {
  CHECK(Util::ParseDouble(" +3 ") == 3.0);
  CHECK(Util::ParseDouble("  -3.5   ") == -3.5);
  CHECK(Util::ParseDouble("-3.5") == -3.5);
  CHECK(Util::ParseDouble("000") == 0.0);
  CHECK(Util::ParseDouble("000", 1.0) == 0.0);
  CHECK(Util::ParseDouble("0", 1.0) == 0.0);
  CHECK(Util::ParseDouble("-0", 1.0) == 0.0);
  double nnn = Util::ParseDouble("nan", 1.0);
  CHECK(std::isnan(nnn)) << "expected NaN";
  CHECK(Util::ParseDouble("-2e3") == -2e3);
  CHECK(Util::ParseDouble("asdf", 3.0) == 3.0);
  CHECK(Util::ParseDouble("33asdf", 2.0) == 2.0);
  CHECK(Util::ParseDouble("asdf33", 2.0) == 2.0);
}

static void TestFactorize() {
  CHECK((vector<int>{2, 2, 2, 3}) == Util::Factorize(2 * 2 * 2 * 3));
  CHECK((vector<int>{2, 2}) == Util::Factorize(4));
  CHECK((vector<int>{2}) == Util::Factorize(2));
  CHECK((vector<int>{5, 5}) == Util::Factorize(5 * 5));
  CHECK((vector<int>{31337}) == Util::Factorize(31337));
  CHECK((vector<int>{3, 5, 11, 13}) == Util::Factorize(3 * 5 * 11 * 13));
  CHECK((vector<int>{3, 5, 5, 11, 13}) ==
	Util::Factorize(3 * 5 * 5 * 11 * 13));  
  CHECK((vector<int>{3, 5, 5, 11, 11, 11}) ==
	Util::Factorize(3 * 5 * 5 * 11 * 11 * 11));  
  CHECK((vector<int>{3, 5, 5, 11, 13, 31337}) ==
	Util::Factorize(3 * 5 * 5 * 11 * 13 * 31337));  
}

static void TestItos() {
  CHECK_EQ(Util::itos(1234), "1234");
  CHECK_EQ(Util::itos(-1234), "-1234");
}

static void TestStoi() {
  CHECK_EQ(Util::stoi("1234"), 1234);
  CHECK_EQ(Util::stoi("-1234"), -1234);
  CHECK_EQ(Util::stoi(""), 0);
  CHECK_EQ(Util::stoi("a"), 0);
}

int main(int argc, char **argv) {
  TestItos();
  TestStoi();
  TestReadFiles();
  TestWriteFiles();
  TestWhitespace();
  TestPad();
  TestJoin();
  TestSplit();
  TestCdup();
  TestPrefixSuffix();
  TestParseDouble();
  TestFactorize();
  return 0;
}


/* KEEP THIS LINE TOO */
