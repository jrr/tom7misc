
#include <string>
#include <vector>
#include <cstdint>
#include <deque>

#include "../../cc-lib/base/logging.h"

#include "bigrat.h"

using namespace std;

static void TestBigRat() {
  // Test some math.
  printf("\n");
  BigRat a(10, 123, 3);
  BigRat b(10, 10, 1);
  BigRat c = PlusSameDenom(a, a);
  a.Shift(1);
  b.Shift(3);
  a.Unzero();
  b.Unzero();
  BigRat d(10, 199, 3);
  BigRat e = MinusSameDenom(c, d);
  BigRat f = Scale(e, 999, 3);
  e.Shift(3);
  printf("a: %s\n", a.ToString().c_str());
  printf("b: %s\n", b.ToString().c_str());
  printf("c: %s\n", c.ToString().c_str());
  printf("d: %s\n", d.ToString().c_str());
  printf("e: %s\n", e.ToString().c_str());
  printf("f: %s\n", f.ToString().c_str());
  CHECK(LessEq(a, b));
  CHECK(Less(a, b));
  CHECK(LessEq(a, a));
  
  CHECK(Less(f, e));
  CHECK(LessEq(f, e));
  CHECK(!Less(e, f));
  CHECK(!LessEq(e, f));

  // Check LessEq on mixed bases.
  BigRat aa = a;
  aa.Shift(3);
  CHECK(LessEq(aa, aa));
  CHECK(LessEq(a, aa));
  CHECK(LessEq(aa, a));
  CHECK(!Less(a, aa));
  CHECK(!Less(aa, a));
  
  CHECK(LessEq(aa, b));
  CHECK(!LessEq(b, aa));
  CHECK(Less(aa, b));
  CHECK(!Less(b, aa));

  BigRat ff = Scale(f, 997, 3);
  CHECK(LessEq(ff, f));
  CHECK(LessEq(ff, e));
  CHECK(!LessEq(f, ff));
  CHECK(!LessEq(e, ff));
  CHECK(LessEq(ff, ff));
}

int main(int argc, char **argv) {
  TestBigRat();
  return 0;
}

