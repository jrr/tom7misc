
#include "lines.h"

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "randutil.h"

int main() {
  ArcFour rc{"lines_test"};

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
  for (const std::pair<int, int> point : Line<int>{4, 5, 0, 0}) {
    printf("%d,%d  ", point.first, point.second);
  }

  printf("\n--\n");
  return 0;
}
