
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

static void Gen(const FontProblem::SDFConfig &config,
                const Network &make_lowercase,
                const Network &make_uppercase,
                const ImageA &sdf,
                const string &filename) {
  const int SCALE = 5;
  
  FontProblem::GenResult result =
    FontProblem::GenImages(config, make_lowercase, make_uppercase,
                           sdf, SCALE);
  
  // All should be the same size.
  const int TILEW = result.input.Width();
  const int IMGH = result.input.Height();
  // Room for predictors at bottom.
  const int TILEH = IMGH + 24;
  
  ImageRGBA out(TILEW * 3, 3 * TILEH);
  out.Clear32(0x000000FF);

  out.BlendImage(TILEW, TILEH, result.input);
  out.BlendImage(0, TILEH, result.low);
  out.BlendImage(0, 0, result.low_up);
  out.BlendImage(TILEW * 2, TILEH, result.up);
  out.BlendImage(TILEW * 2, TILEH * 2, result.up_low);

  auto DrawPreds = [&out](int startx, int starty,
                          const std::array<float, 26> &preds) {
      for (int i = 0; i < 26; i++) {
        const uint8 v = FontProblem::FloatByte(preds[i]);
        string s = "A";
        s[0] += i;
        out.BlendText(startx + 9 * (i % 13),
                      starty + (i / 13) * 10,
                      v, v, 0xFF, 0xFF,
                      s);
      }
    };

  
  // and the predictors  
  DrawPreds(TILEW / 2, TILEH + IMGH + 2, result.low_pred);
  DrawPreds(2 * TILEW + TILEW / 2, 2 * TILEH + IMGH + 2, result.up_pred);  
  
  out.Save(filename);
}
  

int main(int argc, char **argv) {
  FontProblem::SDFConfig config;

  ImageA vector_sdf;
  {
    TTF ttf("times.ttf");
    std::optional<ImageA> sdf =    
      ttf.GetSDF('T', config.sdf_size,
                 config.pad_top, config.pad_bot, config.pad_left,
                 config.onedge_value, config.falloff_per_pixel);
    CHECK(sdf.has_value());
    vector_sdf = sdf.value();
    const int BSIZE = 800;

    {
      ImageRGBA bitmap = FontProblem::SDFThresholdAA(
          config.onedge_value,
          sdf.value().ResizeBilinear(BSIZE, BSIZE),
          1).GreyscaleRGBA();
      // for comparing the bitmap excursion to the round trip.
      bitmap.Save("bitmapletter-example.png");
    }

    // As a template for drawing
    {
      ImageRGBA tpl(BSIZE, BSIZE);
      tpl.Clear32(0x000000FF);
      float pl = config.pad_left / (float)config.sdf_size;
      float pt = config.pad_top / (float)config.sdf_size;
      float pb = config.pad_bot / (float)config.sdf_size;
      tpl.BlendRect32(0, 0, pl * BSIZE, BSIZE, 0x330000FF);
      tpl.BlendRect32(0, 0, BSIZE, pt * BSIZE, 0x003300FF);
      tpl.BlendRect32(0, BSIZE - pb * BSIZE,
                      BSIZE, pb * BSIZE, 0x003300FF);
      
      ImageRGBA bitmap = FontProblem::SDFThresholdAA(
          config.onedge_value,
          sdf.value().ResizeBilinear(BSIZE, BSIZE),
          3).AlphaMaskRGBA(0x00, 0x00, 0x33);
      tpl.BlendImage(0, 0, bitmap);
      tpl.Save("bitmap-template.png");
    }

  }
  
  std::unique_ptr<Network> make_lowercase, make_uppercase;  
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));
  
  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);  
  
  std::unique_ptr<ImageRGBA> input_rgba(ImageRGBA::Load("bitmapletter.png"));
  // Threshold input at >127, so that template markings can
  // remain in the image.
  ImageA input(input_rgba->Width(), input_rgba->Height());
  for (int y = 0; y < input_rgba->Height(); y++) {
    for (int x = 0; x < input_rgba->Width(); x++) {
      const auto [r, g, b, a] = input_rgba->GetPixel(x, y);
      input.SetPixel(x, y, r > 0x7F ? 0xFF : 0x00);
    }
  }
  Timer sdf_timer;
  ImageA bitmap_sdf = FontProblem::SDFFromBitmap(config, input);
  printf("Took %.3f sec\n", sdf_timer.MS() / 1000.0);

  // XXXXXX
  // ImageA sdf = bitmap_sdf;
  // sdf = vector_sdf;
  
  Gen(config, *make_lowercase, *make_uppercase, vector_sdf, "vector-sdf.png");
  Gen(config, *make_lowercase, *make_uppercase, bitmap_sdf, "bitmap-sdf.png");  

  
  return 0;
}
