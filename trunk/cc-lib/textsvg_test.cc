
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <cstdint>

#include "textsvg.h"
#include "base/logging.h"

using namespace std;

#define EXPECT_EQ(a, b) do {						\
    auto aa = (a); auto bb = (b);					\
    CHECK_EQ(aa, bb) << "\nValue of: " #a "\nWhich was: " << aa		\
		     << "\nShould equal: " #b "\nWhich was: " << bb;	\
  } while (0)

int main(int argc, char **argv) {
  EXPECT_EQ(TextSVG::Rtos(0.0), "0");
  EXPECT_EQ(TextSVG::Rtos(0.01), ".01");
  EXPECT_EQ(TextSVG::Rtos(-0.01), "-.01");
  EXPECT_EQ(TextSVG::Rtos(-0.0), "0");
  EXPECT_EQ(TextSVG::Rtos(0.0000000000001), "0");
  // Note: If we're returning zero due to roundoff, it should
  // not have a minus sign.
  EXPECT_EQ(TextSVG::Rtos(-0.0000000000001), "0");
  EXPECT_EQ(TextSVG::Rtos(1.0 / 3.0), ".33333");
  EXPECT_EQ(TextSVG::Rtos(-1.0 / 3.0), "-.33333");
  EXPECT_EQ(TextSVG::Rtos(-1.0 / 3.0), "-.33333");

  EXPECT_EQ(TextSVG::Rtos(1.5), "1.5");
  EXPECT_EQ(TextSVG::Rtos(-1.5), "-1.5");
  EXPECT_EQ(TextSVG::Rtos(10.5), "10.5");
  EXPECT_EQ(TextSVG::Rtos(-10.5), "-10.5");
  EXPECT_EQ(TextSVG::Rtos(0.5), ".5");
  EXPECT_EQ(TextSVG::Rtos(-0.5), "-.5");


  EXPECT_EQ(TextSVG::Rtos(10001.2002), "10001.2002");
  EXPECT_EQ(TextSVG::Rtos(-10001.2002), "-10001.2002");
  
  return 0;
}
