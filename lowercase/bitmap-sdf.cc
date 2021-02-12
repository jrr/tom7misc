
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
  const int sdf_size = config.sdf_size;
  const int SLOT = sdf_size * 4;
  
  ImageRGBA out(SLOT * 6, SLOT + 24);
  out.Clear32(0x000000FF);
  
  out.BlendImage(SLOT * 2, 0, sdf.GreyscaleRGBA().ScaleBy(4));
  
  ImageA thresh = FontProblem::SDFThresholdAA(config.onedge_value,
                                              sdf.ResizeBilinear(sdf_size * 4,
                                                                 sdf_size * 4),
                                              // oversampling: quality param
                                              3);
  out.BlendImage(SLOT * 3, 0, thresh.GreyscaleRGBA());

  auto RunTo = [&config, &out, sdf_size, SLOT, &sdf](
      const Network &net, int startx, int starty) {
      const auto [out_sdf, pred] =
        FontProblem::RunSDFModel(net, config, sdf);
      out.BlendImage(startx, 0, out_sdf.GreyscaleRGBA().ScaleBy(4));

      vector<pair<uint8, uint32>> layers =
        {{(uint8)(config.onedge_value * 0.95), 0x440000FF},
         {(uint8)(config.onedge_value * 0.975), 0x66229FFF},
         {(uint8)(config.onedge_value), 0xFFFFFFFF}};
      ImageRGBA out_thresh = FontProblem::ThresholdImageMulti(
          out_sdf.ResizeBilinear(sdf_size * 4, sdf_size * 4),
          layers,
          4);
      out.BlendImage(startx + SLOT, 0, out_thresh);  

      // Letter predictors.
      // TODO: Show actual values or bar chart?
      // Show negative predictions or >1 too
      const int pred_x = startx + SLOT;
      const int pred_y = starty + SLOT + 2;
      for (int i = 0; i < 26; i++) {
        const uint8 v = FontProblem::FloatByte(pred[i]);
        string s = "A";
        s[0] += i;
        out.BlendText(pred_x + 9 * (i % 13),
                      pred_y + (i / 13) * 10,
                      v, v, 0xFF, 0xFF,
                      s);
      }
    };
  
  RunTo(make_lowercase, 0, 0);
  RunTo(make_uppercase, SLOT * 4, 0);  
  
  out.Save(filename);
}
  

int main(int argc, char **argv) {
  FontProblem::SDFConfig config;

  ImageA vector_sdf;
  {
    TTF ttf("helvetica.ttf");
    std::optional<ImageA> sdf =    
      ttf.GetSDF('g', config.sdf_size,
                 config.pad_top, config.pad_bot, config.pad_left,
                 config.onedge_value, config.falloff_per_pixel);
    CHECK(sdf.has_value());
    vector_sdf = sdf.value();
    const int BSIZE = 1000;

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
