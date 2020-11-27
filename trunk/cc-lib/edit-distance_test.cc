
#include "edit-distance.h"

#include <stdio.h>

#include "base/logging.h"
#include "util.h"

using namespace std;

template<class F>
static void TestDistance(F Distance) {
  CHECK_EQ(0,
	   Distance("if on a winter's night a traveler",
		    "if on a winter's night a traveler"));
  CHECK_EQ(1,
	   Distance("if on a wintxr's night a traveler",
		    "if on a winter's night a traveler"));
  CHECK_EQ(2,
	   Distance("f on a wintxr's night a traveler",
		    "if on a winter's night a traveler"));
  CHECK_EQ(2,
	   Distance("iff on a winter's night a traveler",
		    "if on a winter's night a travele"));
  CHECK_EQ(4, Distance("zzzz", "yyyy"));
  CHECK_EQ(3, Distance("kitten", "sitting"));
  CHECK_EQ(3, Distance("sitting", "kitten"));
}

static void TestThreshold() {
  CHECK_EQ(3, EditDistance::Ukkonen("zzzz", "yyyy", 3));
  CHECK_EQ(2, EditDistance::Ukkonen("zzzz", "yyyy", 2));
  CHECK_EQ(1, EditDistance::Ukkonen("zzzz", "yyyy", 1));  

  CHECK_EQ(2,
	   EditDistance::Ukkonen("iff on a winter's night a ta traveler",
				 "if on a winter's night a travele",
				 2));
  CHECK_EQ(2,
	   EditDistance::Ukkonen("iff on a winter's night a traveler",
				 "if on a winter's night a travele",
				 2));
  CHECK_EQ(1,
	   EditDistance::Ukkonen("iff on a winter's night a traveler",
				 "if on a winter's night a travele",
				 1));

}

int main(int argc, char **argv) {
  TestDistance(EditDistance::Distance);
  TestDistance([](const string &a, const string &b) {
      return EditDistance::Distance(b, a);
    });
  TestDistance([](const string &a, const string &b) {
      // Threshold larger than maximum causes this to have the same
      // behavior as Distance().
      return EditDistance::Ukkonen(a, b, std::max(a.size(), b.size()) + 1);
    });
  TestThreshold();
  return 0;
}
