// In-memory benchmarks for stb_image.
// 2 Aug 2020 (Threadripper 2990WX): Decoded 512 times in 27 sec = 18.9630 images/sec
// same w/v2.26:                     Decoded 512 times in 19 sec = 26.9474 images/sec
// with -march=native                Decoded 512 times in 20 sec = 25.6000 images/sec
// with -march=native -O3 -flto      Decoded 512 times in 18 sec = 28.4444 images/sec


#include <vector>
#include <cstdint>
#include <ctime>

#include "image.h"
#include "stb_image.h"
#include "stb_image_write.h"
#include "arcfour.h"
#include "randutil.h"
#include "base/stringprintf.h"

using namespace std;
using uint8 = uint8_t;
using uint32 = uint32_t;

static constexpr int WIDTH = 1920;
static constexpr int HEIGHT = 1080;

static constexpr int TIMES = 512;

std::tuple<int, int, int, int> RandBox(ArcFour *rc) {
  int x0 = RandTo32(rc, WIDTH);
  int x1 = RandTo32(rc, WIDTH);
  if (x1 < x0) std::swap(x0, x1);
  int y0 = RandTo32(rc, HEIGHT);
  int y1 = RandTo32(rc, HEIGHT);
  if (y1 < y0) std::swap(y0, y1);
  return {x0, y0, x1, y1};
}

static void BenchmarkPNG() {
  // Prepare a test image.
  printf("Prep image...\n");
  ImageRGBA image(WIDTH, HEIGHT);
  ArcFour rc("benchmark");

  // bunch of random rectangles.
  for (int i = 0; i < 1000; i++) {
    uint8 r = rc.Byte();
    uint8 g = rc.Byte();
    uint8 b = rc.Byte();
    uint8 a = rc.Byte();
    const auto [x0, y0, x1, y1] = RandBox(&rc);
    if (0)
      printf("Box %d, #%02x%02x%02x%02x %d,%d -> %d,%d\n",
	     i, r, g, b, a, x0, y0, x1, y1);
    for (int y = y0; y < y1; y++) {
      for (int x = x0; x < x1; x++) {
	image.BlendPixel(x, y, r, g, b, a);
      }
    }
  }

  // Some random AA lines
  for (int i = 0; i < 100; i++) {
    uint8 r = rc.Byte();
    uint8 g = rc.Byte();
    uint8 b = rc.Byte();
    uint8 a = rc.Byte();
    const auto [x0, y0, x1, y1] = RandBox(&rc);
    image.BlendLineAA(x0, y0, x1, y1, r, g, b, a);
  }

  // Some random text
  for (int i = 0; i < 2000; i++) {
    const auto [x0, y0, x1, y1] = RandBox(&rc);
    image.BlendText32(x0, y0, 0x000000FF,
		      StringPrintf("%d,%d", x1, y1));
  }

  const std::vector<uint8> imgbytes = image.SaveToVec();
  // image.Save("deleteme.png");  
  printf("Image ready!\n");
  
  // Now decode a buncha times.
  const int64 start = time(nullptr);
  for (int i = 0; i < TIMES; i++) {
    int w = 0, h = 0;
    int comp = STBI_default;
    uint8 *rgba =
      stbi_load_from_memory(imgbytes.data(), imgbytes.size(), &w, &h, &comp, STBI_rgb_alpha);
    CHECK(w == WIDTH);
    CHECK(h == HEIGHT);
    CHECK(comp == STBI_rgb_alpha);
    stbi_image_free(rgba);
    if (i % 10 == 0) printf(".");
  }
  const int64 took = time(nullptr) - start;
  printf("Decoded %d times in %lld sec = %.4f images/sec\n",
	 TIMES, took, (double)TIMES / took);
}

int main (int argc, char **argv) {
  BenchmarkPNG();
  return 0;
}
