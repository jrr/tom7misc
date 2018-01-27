
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <cstdint>

#include "minmax-heap.h"
#include "base/logging.h"
#include "base/stringprintf.h"

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

int main () {
  TestIsMin();

  static constexpr int kNumValues = 1000;
  IntHeap heap;
  heap.CheckInvariants(Ptos);
  

  vector<TestValue> values;
  for (int i = 0; i < kNumValues; i++) {
    values.push_back(TestValue(CrapHash(i)));
  }

  for (int i = 0; i < values.size(); i++) {
    heap.Insert(values[i].i, &values[i]);
    heap.CheckInvariants(Ptos);
  }

#if 0
  TestValue *last = heap.PopMinimumValue();
  while (!heap.Empty()) {
    TestValue *now = heap.PopMinimumValue();
    fprintf(stderr, "%llu %llu\n", last->i, now->i);
    if (now->i < last->i) {
      printf("FAIL: %llu %llu\n", last->i, now->i);
      return -1;
    }
    last = now;
  }

  for (int i = 0; i < values.size(); i++) {
    if (values[i].location != -1) {
      printf("FAIL! %d still in heap at %d\n", i, values[i].location);
      return -1;
    }
  }
  
  for (int i = 0; i < values.size() / 2; i++) {
    heap.Insert(values[i].i, &values[i]);
  }

  heap.Clear();
  if (!heap.Empty()) {
    printf("FAIL: Heap not empty after clear?\n");
    return -1;
  }

  for (int i = 0; i < values.size() / 2; i++) {
    if (values[i].location != -1) {
      printf("FAIL (B)! %d still in heap at %d\n", i, values[i].location);
      return -1;
    }
  }
  
  printf("OK\n");
  return 0;
#endif
}
