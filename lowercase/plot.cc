
#include <functional>
#include <cstdint>

#include "image.h"
#include "font-problem.h"

using namespace std;
using uint8 = uint8_t;

static constexpr int SDF_SIZE = FontProblem::SDFConfig().sdf_size;

static std::function<float(int, float)> GetRemap() {
  static constexpr float fonedge = FontProblem::SDFConfig().onedge_value / 255.0f;
  // Remap a value x (nominally) in [0,1] space to a new value, also nominally
  // in [0,1]. Here, a piecewise linear mapping
  //                    s2
  // 1 |               b-`/
  //   |               |/
  // o |              /| 
  // u |            / |  
  // t |          /  |  interesting
  // p |        /   |   slope
  // u |      /   .-a
  // t |    /  .-`
  //   |  / .-` s1
  //   |/.-`  
  // 0 +------------|--|---
  //   0   input    a  b  1
  //     input
  //
  // Here the / line would be the identity function. Instead we have a
  // more shallow slope in the less-interesting regions [0,a] and
  // [b,1]. The interesting region [a,b] thus has a steeper slope,
  // which means that smaller differences in the inputs yield bigger
  // differences in the outputs in this region.
  //
  static constexpr float ffalloff = FontProblem::SDFConfig().falloff_per_pixel / 255.0f;
  // +/- 1.5 pixels from the edge value is the critical region
  // This gets pretty close to 1.0 on the high end, btw.
  static constexpr float ax = fonedge - ffalloff * 1.5f;
  static constexpr float bx = fonedge + ffalloff * 1.5f;
  // HERE then need to also specify the slope of the interesting region.
  static constexpr float interesting_slope = 3.0f;

  static_assert(ax > 0.0f);
  static_assert(bx < 1.0f);
  static_assert(bx > ax);
  static_assert(interesting_slope > 0.0f);
  
  static constexpr float w = bx - ax;
  static constexpr float h = interesting_slope * w;

  // Now we need to figure out the y coordinate of a. There's
  // one degree of freedom in the drawing which can be resolved
  // by setting the slopes s1 and s2 equal. Just rearrange so
  // that the interesting box (w by h) is in the bottom left
  // corner. The slope of the remaining line will be the same
  // as the two uninteresting line segments.
  static constexpr float uw = 1 - w;
  static constexpr float uh = 1 - h;
  static_assert(uw > 0.0f);
  static_assert(uh > 0.0f);
  static constexpr float s = uh / uw;

  // Now we can compute the location of the interesting box.
  static constexpr float ay = ax * s;
  static constexpr float by = ay + h;
  static_assert(ay > 0.0f && ay < 1.0f);
  static_assert(by > 0.0f && by < 1.0f);  
  
  // Finally, the remapping function is just a piecewise linear
  // function. For the lowercase problem, we only apply it for
  // the SDF pixels.

  auto Remap = [](int i, float x) {
      if (i < SDF_SIZE) {
        // PERF could probably simplify this!
        if (x < ax) return x * s;
        if (x < bx) return ay + (x - ax) * interesting_slope;
        return by + (x - bx) * s;
      } else {
        return x;
      }
    };

  return Remap;
}

// Generated code; should match Remap.
#define REMAP(i, x) ((i < 36) ? ((x < 0.774509788) ? x * 0.571428418 : (x < 0.950980425) ? 0.442576915 + (x - 0.774509788) * 3.000000000 : 0.971988797 + (x - 0.950980425) * 0.571428418) : x)


using namespace std;

int main(int argc, char **argv) {
  auto Remap = GetRemap();

  ImageRGBA img(512, 512);
  img.Clear32(0x000000FF);
  
  int MARGIN = 6;
  int SQUARE = 512 - MARGIN * 2;
  img.BlendBox32(MARGIN, MARGIN, SQUARE, SQUARE, 0x800000FF, 0x400000FF);
  
  for (int x = 0; x < SQUARE; x++) {
    float fx = (x / (float)SQUARE);
    float fy = Remap(0, fx);
    float fyd = REMAP(0, fx);
    int y = SQUARE - fy * SQUARE;
    int yd = SQUARE - fyd * SQUARE;
    
    img.BlendPixel32(x + MARGIN, y + MARGIN, 0xFFFF80FF);
    img.BlendPixel32(x + MARGIN, yd + MARGIN, 0xFF00FFFF);    
  }

  img.Save("plot.png");
  return 0;
}
