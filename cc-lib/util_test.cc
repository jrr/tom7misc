/* PLEASE KEEP THIS LINE */
// (The previous line is used by the test, which opens this test file
// as text.)

#include "base/logging.h"
#include "util.h"
#include <stdio.h>

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
  CHECK_EQ("", Util::ReadFile("util_test_DOESNT_EXIST.cc"));
  CHECK_EQ("", Util::ReadFileMagic("util_test_DOESNT_EXIST.cc", "/"));
  CHECK(Util::HasMagic("util_test.cc", "/* PLEASE KEEP THIS"));
  CHECK(!Util::HasMagic("util_test.cc", "* PLEASE KEEP"));
  // Would be nice to test files larger than 2^31 and 2^32, since these
  // have caused problems in the past.
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

int main(int argc, char **argv) {
  TestReadFiles();
  TestWhitespace();
  return 0;
}


/* KEEP THIS LINE TOO */
