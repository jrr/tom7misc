
#include "font-problem.h"

#include <memory>
#include <string>
#include <vector>

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "timer.h"
#include "image.h"

using namespace std;

static constexpr bool EXTRA_CHECKS = true;

using Point = FontProblem::Point;
static float SqDistance(const Point &a, const Point &b) {
  const float dx = a.first - b.first;
  const float dy = a.second - b.second;
  return dx * dx + dy * dy;
}

static void TestLoopAssignment() {
  ArcFour rc{"test"};

  using LoopAssignment = FontProblem::LoopAssignment;
  
  const std::vector<Point> expected = {
    {309,461},
    {389,318},
    {548,252},
    {694,343},
    {792,453},
    {794,502},
    {407,507},
    {395,587},
    {483,707},
    {668,730},
    {793,637},
    {845,675},
    {708,789},
    {468,785},
    {302,645},
  };

  const std::vector<Point> actual = {
    {302,456},
    {307,449},
    {311,440},
    {384,307},
    {454,271},
    {561,240},
    {702,334},
    {702,334},
    {811,455},
    {804,501},
    {782,513},
    {799,479},
    {411,516},
    {401,589},
    {490,703},
    {500,703},
    {671,722},
    {818,617},
    {831,621},
    {711,797},
    {465,796},
    {292,652},
    {311,630},
    {298,604},
    {298,479},
  };

  auto DistanceAt = [&actual, &expected](int e, int a) {
      return sqrtf(SqDistance(expected[e], actual[a]));
    };
  
  auto Score = [&actual, &expected, &DistanceAt](
      const LoopAssignment &assn) -> float {
      // here we have like
      //        0       1 2
      //        x       y z      <- expected
      //    a b c d e f g h i j  <- actual
      //    0 1 2 3 4 5 6 7 8 9 
      // This would be represented with point0 = 2,
      // and groups = {4, 1, 5}.

      if (EXTRA_CHECKS) {
        int total = 0;
        for (int g : assn.groups) total += g;
        CHECK(total == actual.size()) << total << " " << actual.size();
      }

      float err = 0.0f;
      int a = assn.point0;
      for (int e = 0; e < expected.size(); e++) {
        int num = assn.groups[e];
        for (int i = 0; i < num; i++) {
          err += DistanceAt(e, a);
          a++;
          if (a == actual.size()) a = 0;
        }
      }
      return err;
    };

  
  LoopAssignment correct(expected.size());
  correct.point0 = 24;
  correct.groups = { 4, 2, 1,
                     2, 1, 3,
                     1, 1, 2,
                     1, 1, 1,
                     1, 1, 3, };
  CHECK_EQ(correct.groups.size(), expected.size());

  const float correct_score = Score(correct);
  
  auto ToString = [](const LoopAssignment &assn) {
      string ret = StringPrintf("Point0: %d groups:", assn.point0);
      for (int d : assn.groups)
        StringAppendF(&ret, " %d", d);
      return ret;
    };

  double benchtime = 0.0;
  const int NUM_ROUNDS = 10000;
  for (int i = 0; i < NUM_ROUNDS; i++) {
    Timer bench;  
    LoopAssignment test =
      FontProblem::BestLoopAssignment(&rc, expected, actual);
    benchtime += bench.MS();

    float test_score = Score(test);
    if (test_score > 1.1 * correct_score) {
      printf("Bad score: round %d %.4f -> %4f. Assn: %s\n",
             i, correct_score, test_score, ToString(test).c_str());
    }
    /*
    CHECK(correct.point0 == test.point0 &&
          correct.groups == test.groups) << i << "\n" << ToString(test);
    */
  }

  printf("Done in %.3fms (%.5f/call)\n",
         benchtime, benchtime / (double)NUM_ROUNDS);
}

static void BenchmarkBitmapSDF() {
  std::unique_ptr<ImageRGBA> input_rgba(ImageRGBA::Load("testletter.png"));
  std::unique_ptr<ImageRGBA> expected(ImageRGBA::Load("expectedsdf.png"));
  auto CheckExpected = [&expected](const ImageA &sdf) {
      ImageRGBA bitmap_sdf = sdf.GreyscaleRGBA();
      CHECK(bitmap_sdf.Width() == expected->Width());
      CHECK(bitmap_sdf.Height() == expected->Height());
      for (int y = 0; y < bitmap_sdf.Height(); y++) {
        for (int x = 0; x < bitmap_sdf.Width(); x++) {
          CHECK(expected->GetPixel32(x, y) == bitmap_sdf.GetPixel32(x, y));
        }
      }
    };
      
  CHECK(input_rgba.get() != nullptr);
  ImageA input = input_rgba->Red();

  double total_ms = 0.0;
  static constexpr int TIMES = 10;
  for (int i = 0; i < TIMES; i++) {
    Timer onerun;
    ImageA bitmap_sdf =
      FontProblem::SDFFromBitmap(FontProblem::SDFConfig(), input);
    double onems = onerun.MS();
    total_ms += onems;
    if (i == 0) bitmap_sdf.GreyscaleRGBA().Save("test-actual.png");
    CheckExpected(bitmap_sdf);
    printf("%.2f\n", onems);
  }

  printf("Average time: %.4f sec\n", total_ms / (TIMES * 1000.0));
}
  
int main(int argc, char **argv) {
  // TestLoopAssignment();
  BenchmarkBitmapSDF();
  
  printf("OK\n");
  return 0;
}
