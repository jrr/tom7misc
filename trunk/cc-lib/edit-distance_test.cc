
#include "edit-distance.h"

#include <stdio.h>

#include "base/logging.h"
#include "util.h"


static void TestDistance() {
  CHECK_EQ(0,
	   EditDistance::Distance("if on a winter's night a traveler",
				  "if on a winter's night a traveler"));
  CHECK_EQ(1,
	   EditDistance::Distance("if on a wintxr's night a traveler",
				  "if on a winter's night a traveler"));
  CHECK_EQ(2,
	   EditDistance::Distance("f on a wintxr's night a traveler",
				  "if on a winter's night a traveler"));
  CHECK_EQ(2,
	   EditDistance::Distance("iff on a winter's night a traveler",
				  "if on a winter's night a travele"));
  CHECK_EQ(4, EditDistance::Distance("zzzz", "yyyy"));
  CHECK_EQ(3, EditDistance::Distance("kitten", "sitting"));
  CHECK_EQ(3, EditDistance::Distance("sitting", "kitten"));
}

int main(int argc, char **argv) {
  TestDistance();
  return 0;
}


/* KEEP THIS LINE TOO */
