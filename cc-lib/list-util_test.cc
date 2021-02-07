#include "list-util.h"

#include <list>

#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;

namespace {
struct X {
  int y = 999;
  X(int y) : y(y) {}
};
}

static void CheckContents(const std::list<X> &l,
                          const std::vector<int> &v) {
  auto itl = l.begin();
  auto itv = v.begin();
  for (;;) {
    if (itl == l.end() && itv == v.end()) return;
    CHECK(itl != l.end());
    CHECK(itv != v.end());
    CHECK(itl->y == *itv);
    ++itl;
    ++itv;
  }
}

static void TestMoveToBack() {
  std::list<X> l;
  l.emplace_back(333);
  X *a = &l.back();
  std::list<X>::iterator ai = l.end(); --ai;
  l.emplace_back(444);
  X *b = &l.back();
  std::list<X>::iterator bi = l.end(); --bi;
  l.emplace_back(555);
  X *c = &l.back();
  std::list<X>::iterator ci = l.end(); --ci;

  auto CheckPointers =
    [&]() {
      CHECK(a != b);
      CHECK(b != c);
      CHECK(a != c);
      CHECK(a->y == 333);
      CHECK(b->y == 444);
      CHECK(c->y == 555);
      CHECK(ai->y == 333);
      CHECK(bi->y == 444);
      CHECK(ci->y == 555);
    };

  CheckPointers();
  CheckContents(l, {333, 444, 555});
  ListMoveToBack(&l, ai);
  CheckPointers();
  CheckContents(l, {444, 555, 333});
  ListMoveToBack(&l, ci);
  CheckPointers();
  CheckContents(l, {444, 333, 555});
  ListMoveToBack(&l, ci);
  CheckPointers();
  CheckContents(l, {444, 333, 555});
}


int main(int argc, char **argv) {
  TestMoveToBack();
}
