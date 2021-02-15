
#include "lines.h"

#include <optional>

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "randutil.h"

using namespace std;

static void TestBresenham() {
  printf("Bresenham:\n");
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
  printf("Wu:\n");
  auto Plot = [](int x, int y, float f) {
      printf("%d,%d %.2f  ", x, y, f);
    };
  LineAA::Draw<int>(0.5f, 4.0f, 2.5f, 3.0f, Plot);
  printf("\n---\n");
}

// TODO: This could certainly be more comprehensive!
static void TestIntersection() {
  // Parallel.
  CHECK(!LineIntersection(3, 1,   10, 1,
                          2, 5,   10, 5).has_value());

  // Parallel.
  CHECK(!LineIntersection(2, 1,   5, 2,
                          1, 3,   4, 4).has_value());

  // Segments not long enough to intersect
  CHECK(!LineIntersection(2, 1,   5, 2,
                          2, 3,   3, 2).has_value());
  CHECK(!LineIntersection(2.0f, 1.0f,   5.0f, 2.0f,
                          2.0f, 3.0f,   3.0f, 2.0f).has_value());

  // Trivial cross at 0.
  auto z = LineIntersection(0, -1,  0, 1,
                            -1, 0,  1, 0);
  CHECK(z.has_value());
  static constexpr float EPSILON = 1e-10;
  CHECK(fabs(z.value().first) < EPSILON &&
        fabs(z.value().second) < EPSILON);

  {
    auto li = LineIntersection(1.0f, 1.0f,  4.0f, 4.0f,
                               3.0f, 1.0f,  2.0f, 4.0f);
    CHECK(li.has_value());
    auto [x, y] = li.value();
    CHECK(fabs(x - 2.5f) < EPSILON) << x;
    CHECK(fabs(y - 2.5f) < EPSILON) << y;
  }

  // Same but with integer coordinates.
  {
    auto li = LineIntersection(1, 1,  4, 4,
                               3, 1,  2, 4);
    CHECK(li.has_value());
    auto [x, y] = li.value();
    CHECK(fabs(x - 2.5f) < EPSILON) << x;
    CHECK(fabs(y - 2.5f) < EPSILON) << y;
  }
}

// Using PointLineDistance as a reference.
static void TestVertHoriz() {
  static constexpr float EPSILON = 0.0001f;
  ArcFour rc{"lines_test"};
  for (int i = 0; i < 10000; i++) {
    float a = RandDouble(&rc) * 100.0f - 50.0f;
    float b = RandDouble(&rc) * 100.0f - 50.0f;
    float c = RandDouble(&rc) * 100.0f - 50.0f;
    float x = RandDouble(&rc) * 100.0f - 50.0f;
    float y = RandDouble(&rc) * 100.0f - 50.0f;

    float dh = PointLineDistance(a, b, c, b, x, y);
    float dht = PointHorizLineDistance(a, b, c, x, y);
    CHECK(fabsf(dh - dht) < EPSILON) << dh << " vs " << dht;
    float dv = PointLineDistance(a, b, a, c, x, y);
    float dvt = PointVertLineDistance(a, b, c, x, y);
    CHECK(fabsf(dv - dvt) < EPSILON) << dv << " vs " << dvt;
  }
}

int main() {

  TestBresenham();
  TestWu();
  TestIntersection();
  TestVertHoriz();

  // TODO: Test point-line distance stuff.

  printf("OK, but need to manually check the Bresenham and Wu results\n");

  return 0;
}
