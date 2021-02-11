
#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>

#include "loadfonts.h"
#include "timer.h"
#include "font-problem.h"

using namespace std;

using uint8 = uint8_t;
using int64 = int64_t;

int main(int argc, char **argv) {

  using SDFConfig = FontProblem::SDFConfig;
  static constexpr int SIZE = SDFConfig().sdf_size;
  static constexpr uint8 ONEDGE = SDFConfig().onedge_value;
  SDFConfig config;

  /*
  const float base_padding = (config.pad_top + config.pad_bot) * 0.5f;

  const float falloff_min =
    (float)config.onedge_value / (sqrtf(2.0f) * config.sdf_size * 0.5f);
  const float falloff_max = (float)config.onedge_value / base_padding;
  CHECK(falloff_max > falloff_min);
  // config.falloff_per_pixel = 0.5f * (falloff_min + falloff_max);
  config.falloff_per_pixel = 0.25f * falloff_max + 0.75 * falloff_min;
  */

  SDFLoadFonts loadfonts(
      []() { return false; },
      config,
      30,
      // (Don't use a huge number... the code reserves space!)
      10000);
      // 20'000LL);



  loadfonts.Sync();

  Timer avg_timer;
  vector<double> average(SIZE * SIZE, 0.0);
  vector<int64> above_thresh(SIZE * SIZE, 0);
  // denom includes 52 letters per font!
  int64 denom = 0, done = 0;
  int64 total_above = 0, total_pixels = 0;
  for (const SDFLoadFonts::Font &font : loadfonts.fonts) {
    for (const ImageA &img : font.sdfs) {
      for (int y = 0; y < SIZE; y++) {
        for (int x = 0; x < SIZE; x++) {
          const uint8 v = img.GetPixel(x, y);
          average[y * SIZE + x] += v / 255.0f;
          if (v >= ONEDGE) {
            above_thresh[y * SIZE + x]++;
            total_above++;
          }
        }
      }
      denom++;
      total_pixels += SIZE * SIZE;
    }
    done++;
    if (done % 1000 == 0) {
      printf ("%lld/%lld done\n", done, (int64)loadfonts.fonts.size());
    }
  }
  printf("Done in %.2f\n", avg_timer.MS() / 1000.0);
  printf("%lld fonts done, %lld fonts failed\n",
         done, loadfonts.NumFailed());
  printf("%lld / %lld total pixels >= onedge\n",
         total_above, total_pixels);

  ImageRGBA avg_out{SIZE, SIZE};
  for (int y = 0; y < SIZE; y++) {
    for (int x = 0; x < SIZE; x++) {
      int idx = y * SIZE + x;
      CHECK(idx < average.size()) << idx;
      const double f = average[idx] / (double)denom;
      const uint8 v = f > 1.0 ? 255 : f < 0.0 ? 0 : (uint8)(round(f * 255.0));
      avg_out.SetPixel(x, y, v, v, v, 0xFF);
    }
  }
  avg_out.Save("sdf-info-avg.png");
  printf("Wrote sdf-info-avg.png.\n");

  // Rank counts.
  vector<int64> ranks = above_thresh;
  std::sort(ranks.begin(), ranks.end());
  CHECK(ranks.size() == SIZE * SIZE);
  // PERF could use binary search
  auto GetRank = [&ranks](int64 count) -> double {
      for (int i = 0; i < ranks.size(); i++) {
        if (count <= ranks[i]) return (double)i / (double)ranks.size();
      }
      return 1.0;
    };

  ImageRGBA thresh_out{SIZE, SIZE};
  for (int y = 0; y < SIZE; y++) {
    for (int x = 0; x < SIZE; x++) {
      const int idx = y * SIZE + x;
      const int64 count = above_thresh[idx];
      const double f = GetRank(count);
      const uint8 v = f > 1.0 ? 255 : f < 0.0 ? 0 : (uint8)(round(f * 255.0));
      uint8 r = 0, g = 0, b = 0;
      if (f <= 0.33) r = v;
      else if (f >= 0.66) g = v;
      else r = g = b = v;
      thresh_out.SetPixel(x, y, r, g, b, 0xFF);
      printf("%d ", count);
    }
    printf("\n");
  }
  thresh_out.Save("sdf-info-thresh.png");
  printf("Wrote sdf-info-thresh.png.\n");

  return 0;
}
