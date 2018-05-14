
#include <stdio.h>
#include <string>
#include <cstdint>
#include <sys/time.h>

#include "armsnes/libretro/libretro.h"
#include "../cc-lib/util.h"
#include "../cc-lib/stb_image_write.h"
#include "../cc-lib/image.h"
#include "screen.h"
#include "convert565.h"
#include "../cc-lib/arcfour.h"

using namespace std;

using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using int64 = int64_t;

static int64 utime() {
  struct timeval tv;
  gettimeofday(&tv, nullptr);
  return tv.tv_sec * 1000000LL + tv.tv_usec;
}

static ArcFour rc("test");

int dummy = 0;
int64 paltime = 0, screentime = 0;
Screen screen;
uint8 palette_cache[16] = {};
int main(int argc, char **argv) {

  InitClosestColors565();
  
  // The environment callback is required before initialization.
  retro_set_environment([](unsigned cmd, void *data) {
	printf("environ %d\n", cmd);
	return false;
      });
  
  printf("Trying to initialize...\n");
  retro_init();
  printf("Initialized.\n");

  // Set dummy callbacks.
  retro_set_video_refresh([](const void *data, unsigned width, unsigned height, size_t pitch) {
      // printf("video CB %p %dx%d @ %d\n", data, width, height, pitch);
    });

  retro_set_audio_sample_batch([](const int16_t *data, size_t frames) -> size_t {
      // printf("audio CB %p x %d\n", data, frames);
      return 0; // n.b. return value is ignored in libretro.cpp
   });

  retro_set_get_inputs([]() -> uint32 {
    return 0;
  });
  
  retro_game_info mario;
  mario.path = "super-mario-world.smc";
  string game = Util::ReadFile(mario.path);
  mario.data = game.data();
  mario.size = game.size();
  mario.meta = "";

  printf("Load game...\n");
  retro_load_game(&mario);
  printf("Loaded.\n");

  // The opening is "easy" and distorts benchmarks.
  printf("Preroll...");
  #define PREROLL 1000
  for (int i = 0; i < PREROLL; i++) {
    if (i % 100 == 0) printf("%d/%d\n", i, PREROLL);
    retro_run();
  }

  retro_set_video_refresh([](const void *data,
                             unsigned width, unsigned height, size_t pitch) {
    int64 svr = utime();
    MakePalette565(data, width, height, pitch, palette_cache, &rc,
		   &screen);
    int64 pal = utime();
    FillScreenFast565(data, width, height, pitch, &screen);
    int64 done = utime();
    paltime += (pal - svr);
    screentime += (done - pal);
    // Make sure screen isn't optimized out
    dummy += screen.attr[rc.Byte()];
    dummy += screen.color_hi[rc.Byte()];
    dummy += screen.color_lo[rc.Byte()];
  });

 
  printf("With conversion...\n");
  #define FRAMES 2000
  int64 start = utime();
  for (int i = 0; i < FRAMES; i++) {
    if (i % 100 == 0) printf("%d/%d\n", i, FRAMES);
    retro_run();
  }

  int64 elapsed = utime() - start;
  printf("%d frames in %lld usec = %.4f FPS = %.4f ms/frame\n"
	 "%.4f ms/frame palette, %.4f ms/frame screen\n",
	 FRAMES, elapsed, FRAMES/(double)(elapsed / 1000000.0),
	 ((double)(elapsed / 1000.0) / FRAMES),
	 ((double)(paltime / 1000.0) / FRAMES),
	 ((double)(screentime / 1000.0) / FRAMES));
  
  retro_set_video_refresh([](const void *data,
                             unsigned width, unsigned height, size_t pitch) {
      vector<uint8> rgbas;
      rgbas.reserve(width * height * 4);
      for (int y = 0; y < height; y++) {
	uint16 *line = (uint16*)&((uint8 *)data)[y * pitch];
	for (int x = 0; x < width; x++) {
	  uint8 a = 0xFF;
	  uint16 packed = line[x];
	  uint8 b = (packed & 31) << 3;
	  uint8 g = ((packed >> 5) & 63) << 2;
	  uint8 r = ((packed >> 11) & 31) << 3;
	  rgbas.push_back(r);
	  rgbas.push_back(g);
	  rgbas.push_back(b);
	  rgbas.push_back(a);
	}
      }

      ImageRGBA image(std::move(rgbas), width, height);
      image.Save("snestest.png");
    });
  retro_run();
  
  retro_deinit();
  return 0;
}
