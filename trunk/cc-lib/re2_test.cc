
#include "re2/re2.h"

#include <stdio.h>

#include "base/logging.h"
#include "util.h"

using namespace std;

// TODO: More tests, though of course RE2 has its own tests; here
// we are just checking that we didn't screw up in the cc-lib import.
// (Might also be helpful for me to give some syntax examples too
// because I can never remember what needs to be backslashed!)
// (Would also perhaps be nice to have some tests that remind me
// of traps like "longest match" vs "first match".)
void TestSimple() {
  CHECK(RE2::FullMatch("the quick brown fox", "[a-z ]+"));
  CHECK(!RE2::FullMatch("the quick brown fox", "[a-z]+"));

  string title, artist;
  CHECK(RE2::FullMatch("Blimps Go 90 by Guided by Voices",
                       "(.+) by (.+)", &title, &artist) &&
        title == "Blimps Go 90 by Guided" &&
        artist == "Voices") << title << " / " << artist;
}


int main(int argc, char **argv) {
  TestSimple();
  return 0;
}
