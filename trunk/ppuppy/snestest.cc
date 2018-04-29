
#include <stdio.h>
#include <string>
#include <cstdint>
#include <sys/time.h>

#include "armsnes/libretro/libretro.h"
#include "../cc-lib/util.h"
#include "../cc-lib/stb_image_write.h"
#include "../cc-lib/image.h"

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

#if 0
// TODO!
// Sets callbacks. retro_set_environment() is guaranteed to be called before retro_init().
// The rest of the set_* functions are guaranteed to have been called before the first call to retro_run() is made.
typedef bool (*retro_environment_t)(unsigned cmd, void *data);
void retro_set_environment(retro_environment_t);

// Render a frame. Pixel format is 15-bit 0RGB1555 native endian unless changed (see RETRO_ENVIRONMENT_SET_PIXEL_FORMAT).
// Width and height specify dimensions of buffer.
// Pitch specifices length in bytes between two lines in buffer.
// For performance reasons, it is highly recommended to have a frame that is packed in memory, i.e. pitch == width * byte_per_pixel.
// Certain graphic APIs, such as OpenGL ES, do not like textures that are not packed in memory.
typedef void (*retro_video_refresh_t)(const void *data, unsigned width, unsigned height, size_t pitch);
void retro_set_video_refresh(retro_video_refresh_t);

// Renders multiple audio frames in one go. One frame is defined as a sample of left and right channels, interleaved.
// I.e. int16_t buf[4] = { l, r, l, r }; would be 2 frames.
// Only one of the audio callbacks must ever be used.
typedef size_t (*retro_audio_sample_batch_t)(const int16_t *data, size_t frames);
void retro_set_audio_sample_batch(retro_audio_sample_batch_t);

// Polls input.
typedef void (*retro_input_poll_t)(void);
void retro_set_input_poll(retro_input_poll_t);

// Queries for input for player 'port'. device will be masked with RETRO_DEVICE_MASK.
// Specialization of devices such as RETRO_DEVICE_JOYPAD_MULTITAP that have been set with retro_set_controller_port_device()
// will still use the higher level RETRO_DEVICE_JOYPAD to request input.
typedef int16_t (*retro_input_state_t)(unsigned port, unsigned device, unsigned index, unsigned id);
void retro_set_input_state(retro_input_state_t);
#endif

int main(int argc, char **argv) {

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

  #define FRAMES 5000
  int64 start = utime();
  for (int i = 0; i < FRAMES; i++) {
    retro_run();
  }
  int64 elapsed = utime() - start;
  printf("%d frames in %lld usec = %.4f FPS = %.4f ms/frame\n",
	 FRAMES, elapsed, FRAMES/(double)(elapsed / 1000000.0),
	 ((double)(elapsed / 1000.0) / FRAMES));
  
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
