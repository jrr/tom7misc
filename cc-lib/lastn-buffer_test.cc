#include "lastn-buffer.h"

#include "base/logging.h"
#include <stdio.h>
#include <vector>
#include <string>
#include <functional>

using namespace std;

static void CanInstantiate() {
  LastNBuffer<int> ib(100, 2);
  LastNBuffer<string> is(30, "hello");
  LastNBuffer<std::function<void(int)>> ifn(30, +[](int i){});
}

static void TestBasic() {
  LastNBuffer<int> ib(5, -1);
  CHECK_EQ(ib.size(), 5);
  for (int i = 0; i < 5; i++) {
    CHECK_EQ(ib[i], -1) << "init";
  }

  ib.push_back(7);
  CHECK_EQ(ib[0], -1);
  CHECK_EQ(ib[1], -1);
  CHECK_EQ(ib[2], -1);
  CHECK_EQ(ib[3], -1);
  CHECK_EQ(ib[4], 7);

  ib.push_front(3);
  ib.push_front(3);
  CHECK_EQ(ib[0], 3);
  CHECK_EQ(ib[1], 3);
  CHECK_EQ(ib[2], -1);
  CHECK_EQ(ib[3], -1);
  CHECK_EQ(ib[4], -1);

  // Make sure wrapping around works ok
  for (int i = 0; i <= 10000; i++)
    ib.push_back(i);

  CHECK_EQ(ib[0], 9996);
  CHECK_EQ(ib[1], 9997);
  CHECK_EQ(ib[2], 9998);
  CHECK_EQ(ib[3], 9999);
  CHECK_EQ(ib[4], 10000);

  for (int i = 1234; i >= 0; i--)
    ib.push_front(i);

  CHECK_EQ(ib[0], 0);
  CHECK_EQ(ib[1], 1);
  CHECK_EQ(ib[2], 2);
  CHECK_EQ(ib[3], 3);
  CHECK_EQ(ib[4], 4);

  int r = 0;
  ib.App([&r](int val){
           CHECK_EQ(val, r);
           r++;
         });
}

int main(int argc, char **argv) {
  CanInstantiate();
  TestBasic();
  printf("OK\n");
  return 0;
}

