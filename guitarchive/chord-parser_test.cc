#include "chord-parser.h"

#include <string>

#include "re2/re2.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"

#include "guitarchive.h"

using namespace std;

#define CHECK_VEQ(v1, v2)				\
  do {							\
    auto vv1 = (v1);					\
    auto vv2 = (v2);					\
    CHECK_EQ(vv1, vv2) << #v1 " vs " #v2 ":\n" <<	\
      Util::Join(vv1, "|") << "\n" <<			\
      Util::Join(vv2, "|");                             \
  } while (0)

static void TestChordLines() {
  ChordParser cp;

  vector<string> ch = cp.ExtractChords(R"(
      C                    F
When you're caught on an island
      C                F
'cos someone crashed a plane
      C              F
surrounded by some strangers
      Bb
who complain, complain, complain

you could sit there forever
waiting on the ground
or you could try to find yourself...
 in the lost and found

      F            G
and around here we know
                   F            G
that Others will watch where we go
               F    
inject us with drugs and
        D7        G     G7
burn somebody's boat

C              F
LOST, when you need somebody
C               F          Am
LOST, when it's Damon Lindelof
          Bb
when you killed your dad
       F            G
 but you don't know why
                        F                 G
 and what's with this guy with the weird eyes?
                  F             D7     G    G7
 and why are they wearing this disguise?

LOST, have another flashback
LOST, and in this week's podcast  scoff
 at the mild spoilers
 you can't be sure that anyone's headed to the shore
 they think you're all dead
 and what's more
 land's  a   bore....
)");
  
  vector<string> expected =
    Util::Split("C,F,C,F,C,F,"
		"Bb,"
		"F,G,F,G,F,D7,G,G7,"
		"C,F,C,F,Am,Bb,"
		"F,G,F,G,F,D7,G,G7", ',');
  CHECK_VEQ(expected, ch);
}

int main (int argc, char **argv) {
  TestChordLines();

  return 0;
}
