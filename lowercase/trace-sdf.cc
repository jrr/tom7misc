
#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>

#include "timer.h"
#include "font-problem.h"

#include "image.h"
#include "lines.h"
#include "base/stringprintf.h"

#include "network.h"

using namespace std;

static constexpr FontProblem::SDFConfig SDF_CONFIG = {};

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

// XXX To TTF etc?
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
  const double sqerr = 1.0f / (scale * scale);

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
        ScaledPoint(p.cx, p.cy);
        ScaledPoint(p.x, p.y);
        break;
      }
      }
    }
  }

}

static void DrawCharTo(
    const std::vector<TTF::Contour> &contours,
    float right_edge,
    int sx, int sy, float scale,
    ImageRGBA *img) {

  int ncontours = contours.size();
  int npaths = 0;
  for (const auto &c : contours) npaths += c.paths.size();

  DrawChar(contours,
           sx, sy,
           scale,
           [&](int x, int y, bool cw) {
             img->BlendPixel32(x, y, cw ? 0x33FF33FF : 0xFF3333FF);
           },
           [&](int x, int y) {
             img->BlendPixel32(x, y, 0x7777FF66);
           });

  // Top, bottom, left come from config.
  float ty = sy + SDF_CONFIG.pad_top * scale;
  img->BlendLine32(sx, ty, sx + scale * SDF_CONFIG.sdf_size, ty,
                   0x4444FF6F);
  float by = sy + (SDF_CONFIG.sdf_size - SDF_CONFIG.pad_bot) * scale;
  img->BlendLine32(sx, by, sx + scale * SDF_CONFIG.sdf_size, by,
                   0x4444FF6F);
  float lx = sx + SDF_CONFIG.pad_left * scale;
  img->BlendLine32(lx, sy + 11, lx, sy + scale * SDF_CONFIG.sdf_size,
                   0x4444FF6F);

  // Right is a parameter; determined by heuristic.
  float rx = sx + right_edge * scale;
  img->BlendLine32(rx, sy + 11, rx, sy + scale * SDF_CONFIG.sdf_size,
                   0xFF44445F);

  img->BlendText32(sx, sy + 2,
                   0xCCCCCCFF,
                   StringPrintf("%d contours, %d paths", ncontours, npaths));
}

static void MakeTraceImage(const ImageF &sdf,
                           const ImageRGBA &islands,
                           const std::vector<TTF::Contour> &unopt_contours,
                           const std::vector<TTF::Contour> &contours,
                           float right_edge,
                           const string &filename) {
  const int SIZE = SDF_CONFIG.sdf_size;
  const int SCALE = 16;
  const int QUALITY = 3;

  const int TILE = SIZE * SCALE;
  const int TILESW = 3;

  ImageRGBA out(TILE * TILESW, TILE * 2);
  out.Clear32(0x000000FF);
  int tile = 0;

  // Input SDF.
  out.BlendImage((tile % TILESW) * TILE, (tile / TILESW) * TILE,
                 sdf.Make8Bit().ScaleBy(SCALE).GreyscaleRGBA());
  tile++;

  // Thresholded, AA.
  ImageA thresh = FontProblem::SDFThresholdAAFloat(
      SDF_CONFIG.onedge_value / 255.0f,
      sdf,
      SIZE * SCALE,
      SIZE * SCALE,
      QUALITY,
      1.0f);
  out.BlendImage((tile % TILESW) * TILE, (tile / TILESW) * TILE,
                 thresh.GreyscaleRGBA());
  tile++;

  ImageRGBA crop_islands = islands.Crop32(2, 2,
                                          islands.Width() - 4,
                                          islands.Height() - 4);
  out.BlendImage((tile % TILESW) * TILE, (tile / TILESW) * TILE,
                 crop_islands.ScaleBy(SCALE));
  tile++;

  DrawCharTo(unopt_contours,
             right_edge,
             (tile % TILESW) * TILE, (tile / TILESW) * TILE,
             SCALE,
             &out);
  tile++;

  DrawCharTo(contours,
             right_edge,
             (tile % TILESW) * TILE, (tile / TILESW) * TILE,
             SCALE,
             &out);
  tile++;

  out.Save(filename);
  printf("Wrote %s.\n", filename.c_str());
}


int main(int argc, char **argv) {

#if 1
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

  FontProblem::Image8x8 pix;
  for (int i = 0; i < 64; i++) {
    char *c =
      "  ###   "
      "  ###   "
      " ## ##  "
      " ## ##  "
      "####### "
      "##   ## "
      "##   ## "
      "##   ## ";
    int y = i / 8;
    int x = i % 8;
    pix.SetPixel(x, y, c[i] == '#');
  }
  printf("Num pixels: %d\n"
         "Num edges: %d\n",
         pix.PixelsOn(),
         pix.Edges());
  ImageA pixel_sdf = FontProblem::SDF36From8x8(pix);

  printf("Took %.3f sec\n", sdf_timer.MS() / 1000.0);


  TTF ttf("helvetica.ttf");
  std::optional<ImageA> sdf =
    ttf.GetSDF('E', SDF_CONFIG.sdf_size,
               SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot, SDF_CONFIG.pad_left,
               SDF_CONFIG.onedge_value, SDF_CONFIG.falloff_per_pixel);
  CHECK(sdf.has_value());
  ImageA vector_sdf = sdf.value();

  std::unique_ptr<Network> make_lowercase, make_uppercase;
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));
  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);
  FontProblem::Gen5Result gen5result =
    FontProblem::Gen5(SDF_CONFIG, *make_lowercase, *make_uppercase, vector_sdf);

  const ImageF &trace_sdf = gen5result.low;

  ImageRGBA islands;
  Timer vectorize_timer;
  const auto [unopt_contours, contours] =
    FontProblem::VectorizeSDF(SDF_CONFIG, trace_sdf.Make8Bit(), &islands);
  const float right_edge = FontProblem::GuessRightEdge(SDF_CONFIG, trace_sdf);
  double vectorize_ms = vectorize_timer.MS();
  printf("Traced in %.2fs\n", vectorize_ms / 1000.0f);

  MakeTraceImage(trace_sdf,
                 islands,
                 unopt_contours,
                 contours,
                 right_edge,
                 "trace.png");

  TTF::Char ttf_char = FontProblem::ToChar(SDF_CONFIG, contours, right_edge);

  TTF::Font font;
  font.baseline = FontProblem::TTFBaseline(SDF_CONFIG);
  font.chars = {{'e', ttf_char}};

  Util::WriteFile("trace.sfd", font.ToSFD("Traced"));
#else

  TTF::Char ttf_char;
  ttf_char.width = 1.0f;
  ttf_char.contours = {
    TTF::Contour{.paths = {TTF::Path(0.50, 0.30),
                           TTF::Path(0.70, 0.70),
                           TTF::Path(0.30, 0.70)}}
  };

  TTF::Font font;
  font.baseline = 0.75;
  font.chars = {{'e', ttf_char}};

  Util::WriteFile("trace.sfd", font.ToSFD("Traced"));
#endif
  printf("Wrote trace.sfd\n");
  return 0;
}
