
#include "re2/re2.h"

#include <stdio.h>

#include "base/logging.h"
#include "util.h"

// TODO: More tests, though of course RE2 has its own tests; here
// we are just checking that we didn't screw up in the cc-lib import.
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
