
#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>

#include "timer.h"
#include "font-problem.h"

#include "image.h"
#include "lines.h"

using namespace std;

static constexpr FontProblem::SDFConfig SDF_CONFIG = {};

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

template<class DrawPixel, class DrawPoint>
static void DrawChar(const std::vector<TTF::Contour> &contours,
                     float sx, float sy, float scale,
                     const DrawPixel &draw_pixel,
                     const DrawPoint &draw_point) {

  auto ScaledLine = [&](float x1, float y1, float x2, float y2, bool cw) {
      for (const auto [x, y] : Line<int>(sx + x1 * scale,
                                         sy + y1 * scale,
                                         sx + x2 * scale,
                                         sy + y2 * scale)) {
        draw_pixel(x, y, cw);
      }
    };

  auto ScaledPoint = [&](float x, float y) {
      int xx = sx + x * scale;
      int yy = sy + y * scale;
      for (int dx : {-1, 0, 1}) {
        for (int dy : {-1, 0, 1}) {
          draw_point(xx + dx, yy + dy);
        }
      }
    };

  // One screen pixel.
  constexpr double sqerr = 1.0f;

  for (const auto &contour : contours) {
    const bool cw = TTF::IsClockwise(contour);

    float x = contour.StartX();
    float y = contour.StartY();
    for (const auto &p : contour.paths) {
      switch (p.type) {
      case TTF::PathType::LINE: {
        ScaledLine(x, y, p.x, p.y, cw);
        x = p.x;
        y = p.y;
        break;
      }
      case TTF::PathType::BEZIER: {
        for (const auto [xx, yy] :
               TesselateQuadraticBezier<double>(x, y, p.cx, p.cy, p.x, p.y,
                                                sqerr)) {
          ScaledLine(x, y, xx, yy, cw);
          x = xx;
          y = yy;
        }
        break;
      }
      }
    }
  }

  // Now draw vertices to give a hint when there are "too many"
  // control points.
  for (const auto &contour : contours) {
    // PointAt(contour.StartX(), contour.StartY());

    for (const auto &p : contour.paths) {
      switch (p.type) {
      case TTF::PathType::LINE: {
        ScaledPoint(p.x, p.y);
        break;
      }
      case TTF::PathType::BEZIER: {
        // auto [cx, cy] = ttf->Norm(p.cx, p.cy);
        ScaledPoint(p.x, p.y);
        break;
      }
      }
    }
  }

}


static void MakeTrace(const ImageA &sdf, const string &filename) {
  const int SIZE = SDF_CONFIG.sdf_size;
  const int SCALE = 16;
  const int QUALITY = 3;

  const int TILE = SIZE * SCALE;

  ImageRGBA out(TILE * 4, TILE);
  out.Clear32(0x000000FF);
  int tile = 0;

  // Input SDF.
  out.BlendImage(tile * TILE, 0, sdf.ScaleBy(SCALE).GreyscaleRGBA());
  tile++;

  // Thresholded, AA.
  ImageA thresh = FontProblem::SDFThresholdAAFloat(
      SDF_CONFIG.onedge_value / 255.0f,
      sdf,
      SIZE * SCALE,
      SIZE * SCALE,
      QUALITY,
      1.0f);
  out.BlendImage(tile * TILE, 0, thresh.GreyscaleRGBA());
  tile++;

  ImageRGBA islands;
  Timer vectorize_timer;
  std::vector<TTF::Contour> contours =
    FontProblem::VectorizeSDF(SDF_CONFIG, sdf, &islands);
  double vectorize_ms = vectorize_timer.MS();
  out.BlendImage(tile * TILE, 0, islands.ScaleBy(SCALE));
  tile++;

  DrawChar(contours,
           tile * TILE, 0, SCALE,
           [&](int x, int y, bool cw) {
             out.BlendPixel32(x, y, cw ? 0x33FF33FF : 0xFF3333FF);
           },
           [&](int x, int y) {
             out.BlendPixel32(x, y, 0x7777FF66);
           });

  out.Save(filename);
  printf("Vectorized in %.3fs\nWrote %s.\n", vectorize_ms / 1000.0,
         filename.c_str());
}


int main(int argc, char **argv) {
  std::unique_ptr<ImageRGBA> input_rgba(ImageRGBA::Load("bitmapletter.png"));
  CHECK(input_rgba.get() != nullptr);
  // Threshold input at r>127, so that template markings can
  // remain in the image.
  ImageA input(input_rgba->Width(), input_rgba->Height());
  for (int y = 0; y < input_rgba->Height(); y++) {
    for (int x = 0; x < input_rgba->Width(); x++) {
      const auto [r, g, b, a] = input_rgba->GetPixel(x, y);
      input.SetPixel(x, y, r > 0x7F ? 0xFF : 0x00);
    }
  }
  Timer sdf_timer;
  ImageA bitmap_sdf = FontProblem::SDFFromBitmap(SDF_CONFIG, input);
  printf("Took %.3f sec\n", sdf_timer.MS() / 1000.0);

  MakeTrace(bitmap_sdf, "trace.png");
  return 0;
}
