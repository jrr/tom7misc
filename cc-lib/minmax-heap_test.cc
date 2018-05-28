
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <cstdint>

#include "minmax-heap.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"

using namespace std;

using uint64 = uint64_t;

struct TestValue : public Heapable {
  // XXX not mod 100
  TestValue(uint64 i) : i(i % 100) {}
  uint64 i;
};

static uint64 CrapHash(int a) {
  uint64 ret = ~a;
  ret *= 31337;
  ret ^= 0xDEADBEEF;
  ret = (ret >> 17) | (ret << (64 - 17));
  ret -= 911911911911;
  ret *= 65537;
  ret ^= 0xCAFEBABE;
  return ret;
}

using IntHeap = MinMaxHeap<uint64, TestValue>;
void TestParentChild() {
  for (int i = 0; i < 100; i++) {
    CHECK_EQ(IntHeap::Parent(IntHeap::LeftChild(i)), i);
    CHECK_EQ(IntHeap::Parent(IntHeap::RightChild(i)), i);
  }
}

void TestIsMin() {
  CHECK(IntHeap::IsMinLayer(0));
  CHECK(!IntHeap::IsMinLayer(1));
  CHECK(!IntHeap::IsMinLayer(2));
  CHECK(IntHeap::IsMinLayer(3));
  CHECK(IntHeap::IsMinLayer(4));
  CHECK(IntHeap::IsMinLayer(5));
  CHECK(IntHeap::IsMinLayer(6));
  CHECK(!IntHeap::IsMinLayer(7));
  CHECK(!IntHeap::IsMinLayer(8));
  CHECK(!IntHeap::IsMinLayer(9));
  CHECK(!IntHeap::IsMinLayer(10));
  CHECK(!IntHeap::IsMinLayer(11));
  CHECK(!IntHeap::IsMinLayer(12));
  CHECK(!IntHeap::IsMinLayer(13));
  CHECK(!IntHeap::IsMinLayer(14));
}

string Ptos(uint64 u) {
  return StringPrintf("%llu", u);
}

static void TestTypes() {
  using StringHeap = MinMaxHeap<string, TestValue>;
  StringHeap heap;
  auto Stos = [](const string &s) { return s; };
  heap.CheckInvariants(Stos);
  TestValue a(15), b(22);
  heap.Insert("hello", &a);
  heap.Insert("world", &b);
  heap.CheckInvariants(Stos);
}

void FindCounterexample() {
  static constexpr int kNumValues = 6;
  // static constexpr bool DEBUG = false;
  #define DEBUG (seed == 4)
  for (int seed = 0; seed < 0x7FFFFFFE; seed++) {
    ArcFour rc(StringPrintf("%d", seed));
    printf("Seed: %d\n", seed);
    IntHeap heap;
    vector<TestValue> values;
    values.reserve(kNumValues);
    if (DEBUG) printf("---\n");
    for (int i = 0; i < kNumValues; i++) {
      int p = RandTo32(&rc, 25);
      values.push_back(TestValue(CrapHash(p)));
      heap.Insert(p, &values[i]);
      if (DEBUG) printf("Insert: %d\n", p);
    }
    if (DEBUG) 
      printf("Before: %s\n", heap.DebugString(Ptos).c_str());
    IntHeap::Cell c = heap.PopMinimum();
    printf("Min priority: %llu\n", c.priority);
    heap.CheckInvariants(Ptos);
    if (DEBUG) 
      printf("Pop min: %s\n", heap.DebugString(Ptos).c_str());
    while (!heap.Empty()) {
      heap.PopMinimumValue();
      heap.CheckInvariants(Ptos);
    }
  }
}

vector<TestValue> GetValues(ArcFour *rc) {
  static constexpr int kNumValues = 1000;
  vector<TestValue> values;
  for (int i = 0; i < kNumValues; i++) {
    uint64 u = Rand64(rc);
    values.push_back(TestValue(u));
  }
  return values;
}

void TestMin(ArcFour *rc) {
  IntHeap heap;
  heap.CheckInvariants(Ptos);
  vector<TestValue> values = GetValues(rc);
  
  for (int i = 0; i < values.size(); i++) {
    heap.Insert(values[i].i, &values[i]);
    heap.CheckInvariants(Ptos);
  }

  // printf("PopMinimumValue:\n");
  TestValue *last = heap.PopMinimumValue();
  heap.CheckInvariants(Ptos);
  // printf("Pop all values (min):\n");
  while (!heap.Empty()) {
    TestValue *now = heap.PopMinimumValue();
    heap.CheckInvariants(Ptos);
    // fprintf(stderr, "%llu %llu\n", last->i, now->i);
    CHECK_GE(now->i, last->i) <<
      StringPrintf("FAIL: %llu %llu\n", last->i, now->i);
    last = now;
  }

  for (int i = 0; i < values.size(); i++) {
    CHECK(values[i].location == -1) <<
      StringPrintf("FAIL! %d still in heap at %d\n", i, values[i].location);
  }
}

void TestMax(ArcFour *rc) {
  IntHeap heap;
  heap.CheckInvariants(Ptos);
  vector<TestValue> values = GetValues(rc);
  
  for (int i = 0; i < values.size(); i++) {
    heap.Insert(values[i].i, &values[i]);
    heap.CheckInvariants(Ptos);
  }

  // printf("PopMaximumValue:\n");
  TestValue *last = heap.PopMaximumValue();
  heap.CheckInvariants(Ptos);
  // printf("Pop all values (max):\n");
  while (!heap.Empty()) {
    TestValue *now = heap.PopMaximumValue();
    heap.CheckInvariants(Ptos);
    printf("%llu %llu\n", last->i, now->i);
    CHECK_LE(now->i, last->i) <<
      StringPrintf("FAIL: %llu %llu\n", last->i, now->i);
    last = now;
  }

  for (int i = 0; i < values.size(); i++) {
    CHECK(values[i].location == -1) <<
      StringPrintf("FAIL! %d still in heap at %d\n", i, values[i].location);
  }
}

void TestClear(ArcFour *rc) {
  vector<TestValue> values = GetValues(rc);
  IntHeap heap;
  for (int i = 0; i < values.size() / 2; i++) {
    heap.Insert(values[i].i, &values[i]);
  }

  heap.Clear();
  CHECK(heap.Empty()) << "FAIL: Heap not empty after clear?\n";

  for (int i = 0; i < values.size() / 2; i++) {
    CHECK_EQ(values[i].location, -1) <<
      StringPrintf("FAIL (B)! %d still in heap at %d\n", i, values[i].location);
  }
}

int main() {
  ArcFour rc("minmax-heap-test");
  TestIsMin();
  TestParentChild();
  TestTypes();

  printf("Test min...\n");
  for (int i = 0; i < 100; i++)
    TestMin(&rc);
  printf("Test max...\n");
  TestMax(&rc);
  TestClear(&rc);
  
  printf("OK\n");
  return 0;
}
