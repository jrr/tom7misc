
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

#if 0
static void Gen(const FontProblem::SDFConfig &config,
                const Network &make_lowercase,
                const Network &make_uppercase,
                const ImageA &sdf,
                const string &filename) {
  const int SCALE = 5;
  const int QUALITY = 4;

  ImageRGBA islands;
  (void)FontProblem::VectorizeSDF(config, sdf, &islands);
  islands = islands.ScaleBy(4);

  FontProblem::Gen5Result gen5result =
    FontProblem::Gen5(config, make_lowercase, make_uppercase, sdf);

  FontProblem::Gen5ImagesResult result =
    FontProblem::Gen5Images(gen5result, SCALE, QUALITY);

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

  out.BlendImage(TILEW + 4, 4, islands);

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
  DrawPreds(2 * TILEW + TILEW / 2, 1 * TILEH + IMGH + 2, result.up_pred);

  out.Save(filename);
}
#endif

static void Invert(ImageA *img) {
  for (int y = 0; y < img->Height(); y++) {
    for (int x = 0; x < img->Width(); x++) {
      img->SetPixel(x, y, 255 - img->GetPixel(x, y));
    }
  }
}

int main(int argc, char **argv) {
  static constexpr FontProblem::SDFConfig SDF_CONFIG = {};

  const int SDF_SIZE = SDF_CONFIG.sdf_size;
  const int SCALE = 5;
  const int QUALITY = 4;
  const int TILE = SCALE * SDF_SIZE;
  
  std::unique_ptr<ImageRGBA> input_rgba(
      ImageRGBA::Load("sdf-figure-input.png"));
  CHECK(input_rgba.get() != nullptr);
  // Threshold input at >127, so that template markings can
  // remain in the image.
  ImageA input(input_rgba->Width(), input_rgba->Height());
  for (int y = 0; y < input_rgba->Height(); y++) {
    for (int x = 0; x < input_rgba->Width(); x++) {
      const auto [r, g, b, a] = input_rgba->GetPixel(x, y);
      input.SetPixel(x, y, r > 0x7F ? 0xFF : 0x00);
    }
  }
  ImageA nearest = input.ResizeNearest(SDF_SIZE, SDF_SIZE);
  Invert(&nearest);
  
  Timer sdf_timer;
  ImageA bitmap_sdf = FontProblem::SDFFromBitmap(SDF_CONFIG, input);
  printf("Took %.3f sec\n", sdf_timer.MS() / 1000.0);

  ImageRGBA out(TILE * 4, TILE);
  
  ImageA thresh =
    FontProblem::SDFThresholdAAFloat(SDF_CONFIG.onedge_value / 255.0f,
                                     ImageF(bitmap_sdf),
                                     TILE, TILE,
                                     1);
  Invert(&thresh);

  out.BlendImage(TILE * 0, 0, nearest.GreyscaleRGBA().ScaleBy(SCALE));
  out.BlendImage(TILE * 1, 0, bitmap_sdf.GreyscaleRGBA().ScaleBy(SCALE));
  out.BlendImage(TILE * 2, 0,
                 bitmap_sdf.ResizeBilinear(TILE, TILE).GreyscaleRGBA());
  out.BlendImage(TILE * 3, 0,
                 thresh.GreyscaleRGBA());
  
  out.Save("sdf-figure.png");
  printf("Wrote sdf-figure.png\n");
  return 0;
}
