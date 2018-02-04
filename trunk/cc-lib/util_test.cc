/* PLEASE KEEP THIS LINE */

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
}

int main(int argc, char **argv) {
  TestReadFiles();
  return 0;
}


/* KEEP THIS LINE TOO */
