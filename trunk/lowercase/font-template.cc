
// Make a large PNG file that can be used as a template for
// drawing fonts as pixel bitmaps, then fed back to (upcoming
// program) to generate vector format.

#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>

#include "timer.h"
#include "font-problem.h"
#include "network.h"

#include "image.h"

using namespace std;

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

static constexpr FontProblem::SDFConfig SDF_CONFIG = {};

static bool IsLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static ImageF GetSDF(const TTF &ttf, char ch) {
  std::optional<ImageA> sdfo =
    ttf.GetSDF(ch, SDF_CONFIG.sdf_size,
               SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot, SDF_CONFIG.pad_left,
               SDF_CONFIG.onedge_value, SDF_CONFIG.falloff_per_pixel);
  CHECK(sdfo.has_value());
  return ImageF(sdfo.value());
}

int main(int argc, char **argv) {
  TTF ttf("helvetica.ttf");
  
  // string chars = "abcdefghijklmnopqrstuvwxyz";
  // string chars = "1234567890!@#$%^&*()";
  string chars = "-=_+[]{}\\|;:'\"<>?,./`~";

  const int BSIZE = 800;
  ImageRGBA out(BSIZE * chars.size(), BSIZE);
  out.Clear32(0x000000FF);

  ImageA empty(BSIZE, BSIZE);
  ImageA alpha(BSIZE, BSIZE);
  alpha.Clear(0x33);
  
  for (int i = 0; i < chars.size(); i++) {
    const char chl = chars[i];
    const char chu =
      (chl >= 'a' && chl <= 'z') ?
      chars[i] - 32 :
      // Your choice! x-height?
      'x';
      
    ImageF sdfl = GetSDF(ttf, chl);
    ImageF sdfu = GetSDF(ttf, chu);
        
    ImageRGBA tpl(BSIZE, BSIZE);
    tpl.Clear32(0x000000FF);
    float pl = SDF_CONFIG.pad_left / (float)SDF_CONFIG.sdf_size;
    float pt = SDF_CONFIG.pad_top / (float)SDF_CONFIG.sdf_size;
    float pb = SDF_CONFIG.pad_bot / (float)SDF_CONFIG.sdf_size;
    tpl.BlendRect32(0, 0, pl * BSIZE, BSIZE, 0x444400FF);
    tpl.BlendRect32(0, 0, BSIZE, pt * BSIZE, 0x004400FF);
    tpl.BlendRect32(0, BSIZE - pb * BSIZE,
                    BSIZE, pb * BSIZE, 0x004400FF);

    ImageA bitmapl = FontProblem::SDFThresholdAAFloat(
        SDF_CONFIG.onedge_value / 255.0f,
        sdfl,
        BSIZE, BSIZE,
        3);

    ImageA bitmapu = FontProblem::SDFThresholdAAFloat(
        SDF_CONFIG.onedge_value / 255.0f,
        sdfu,
        BSIZE, BSIZE,
        3);

    // Don't anti-alias; it just makes it harder to work with
    // (e.g. if I accidentally draw on this layer) and larger
    // files.
    for (int y = 0; y < BSIZE; y++) {
      for (int x = 0; x < BSIZE; x++) {
        uint8 v = bitmapu.GetPixel(x, y) > 127 ? 0xFF : 0x00;
        bitmapu.SetPixel(x, y, v);
      }
    }

    // And for red, tone it down a little to match the luminance
    // of the blue channel.
    for (int y = 0; y < BSIZE; y++) {
      for (int x = 0; x < BSIZE; x++) {
        uint8 v = bitmapl.GetPixel(x, y) > 127 ? 0xA7 : 0x00;
        bitmapl.SetPixel(x, y, v);
      }
    }
    
    ImageRGBA both = ImageRGBA::FromChannels(bitmapl, empty, bitmapu, alpha);
    
    tpl.BlendImage(0, 0, both);
    tpl.Save("bitmap-template.png");
      
    out.BlendImage(i * BSIZE, 0, tpl);
  }

  out.Save("template.png");

  return 0;
}
