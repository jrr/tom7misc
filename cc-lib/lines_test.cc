
#include "lines.h"

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "randutil.h"

static void TestBresenham() {
  for (const std::pair<int, int> point : Line<int>{0, 0, 5, 5}) {
    printf("%d,%d  ", point.first, point.second);
  }

  printf("\n--\n");
  for (const std::pair<int, int> point : Line<int>{5, 5, 0, 0}) {
    printf("%d,%d  ", point.first, point.second);
  }

  printf("\n--\n");
  for (const std::pair<int, int> point : Line<int>{5, 4, 0, 0}) {
    printf("%d,%d  ", point.first, point.second);
  }

  printf("\n--\n");
  for (auto [x, y] : Line<int>{4, 5, 0, 0}) {
    printf("%d,%d  ", x, y);
  }

  printf("\n--\n");
}

static void TestWu() {
  auto Plot = [](int x, int y, float f) {
      printf("%d,%d %.2f  ", x, y, f);
    };
  LineAA::Draw<int>(0.5f, 4.0f, 2.5f, 3.0f, Plot);
  printf("\n---\n");
}

int main() {
  // ArcFour rc{"lines_test"};

  TestBresenham();
  TestWu();
  
  return 0;
}
