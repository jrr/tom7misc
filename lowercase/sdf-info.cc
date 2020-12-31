
#include <vector>
#include <string>

#include "loadfonts.h"
#include "timer.h"

using namespace std;

int main(int argc, char **argv) {
  static constexpr int SIZE = 64;
  
  using SDFConfig = SDFLoadFonts::SDFConfig;
  SDFConfig config;
  config.sdf_size = SIZE;
  config.pad_top = 4;
  config.pad_bot = 18;
  config.pad_left = 18;
  config.onedge_value = 200;

  const float base_padding = (config.pad_top + config.pad_bot) * 0.5f;
  
  const float falloff_min =
    (float)config.onedge_value / (sqrtf(2.0f) * config.sdf_size * 0.5f);
  const float falloff_max = (float)config.onedge_value / base_padding;
  CHECK(falloff_max > falloff_min);
  // config.falloff_per_pixel = 0.5f * (falloff_min + falloff_max);
  config.falloff_per_pixel = 0.25f * falloff_max + 0.75 * falloff_min;
  
  SDFLoadFonts loadfonts(
      []() { return false; },
      config,
      30,
      // (Don't use a huge number... the code reserves space!)
      20'000LL);



  loadfonts.Sync();

  Timer avg_timer;
  vector<double> average(SIZE * SIZE, 0.0);
  // denom includes 52 letters per font!
  int64 denom = 0, done = 0;
  for (const SDFLoadFonts::Font &font : loadfonts.fonts) {
    for (const ImageA &img : font.sdfs) {
      for (int y = 0; y < SIZE; y++) {
	for (int x = 0; x < SIZE; x++) {
	  average[y * SIZE + x] += img.GetPixel(x, y) / 255.0f;
	}
      }
      denom++;
    }
    done++;
    if (done % 1000 == 0) printf ("%lld/%lld done\n", done, (int64)loadfonts.fonts.size());
  }
  printf("Done in %.2f\n", avg_timer.MS() / 1000.0);
  printf("%lld fonts done, %lld fonts failed\n",
	 done, loadfonts.NumFailed());
  
  ImageRGBA out{SIZE, SIZE};
  for (int y = 0; y < SIZE; y++) {
    for (int x = 0; x < SIZE; x++) {
      int idx = y * SIZE + x;
      CHECK(idx < average.size()) << idx;
      const double f = average[idx] / (double)denom;
      const uint8 v = f > 1.0 ? 255 : f < 0.0 ? 0 : (uint8)(round(f * 255.0));
      out.SetPixel(x, y, v, v, v, 0xFF);
    }
  }

  out.Save("sdf-info.png");
  printf("Wrote sdf-info.png.\n");
  return 0;
}
