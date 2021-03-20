
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
#include "geom/marching.h"

#include "network.h"
#include "threadutil.h"

using namespace std;

static constexpr FontProblem::SDFConfig SDF_CONFIG = {};
static constexpr int SDF_SIZE = SDF_CONFIG.sdf_size;

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

vector<bool> Threshold(const ImageF &a) {
  vector<bool> ret;
  ret.reserve(a.Width() * a.Height());
  for (int y = 0; y < a.Height(); y++) {
    for (int x = 0; x < a.Width(); x++) {
      ret.push_back(a.GetPixel(x, y) > (SDF_CONFIG.onedge_value / 255.0f));
    }
  }
  return ret;
}

constexpr float SCALE = 2.0;
constexpr int CYCLE_START = 245;
constexpr int CYCLE_END = 377;
constexpr bool LOWER = false;

/*
constexpr float SCALE = 1.5;
constexpr int CYCLE_START = 202;
constexpr int CYCLE_END = 389;
constexpr bool LOWER = false;
*/

/*
constexpr float SCALE = 3.0;
constexpr int CYCLE_START = 2291;
constexpr int CYCLE_END = 2957;
constexpr bool LOWER = false;
*/

int main(int argc, char **argv) {
  Timer timer;
  TTF helvetica("helvetica.ttf");

  vector<ImageF> cycle;
  string phrase = "LMNOP";
  for (char ch : phrase) {
    std::optional<ImageA> sdfo =
      helvetica.GetSDF(ch, SDF_CONFIG.sdf_size,
                       SDF_CONFIG.pad_top, SDF_CONFIG.pad_bot,
                       SDF_CONFIG.pad_left,
                       SDF_CONFIG.onedge_value,
                       SDF_CONFIG.falloff_per_pixel);
    CHECK(sdfo.has_value());
    cycle.emplace_back(sdfo.value());
  }

  constexpr int RENDER_SCALE = 2;
  constexpr int TILE = (SDF_SIZE * RENDER_SCALE) + 1;
  constexpr int MAX_WIDTH = 1920 / TILE; // XXX slop
  
  const int num = (int)cycle.size();
  std::vector<int> factors = Util::Factorize(num);
  int width = 1;
  int height = num;
  while (!factors.empty() && width < MAX_WIDTH && width < height) {
    int x = factors.back();
    factors.pop_back();
    if (width * x < MAX_WIDTH) {
      width *= x;
      height = num / width;
    }
  }

  printf("%d width. %d / %d = %d\n", width, num, width, num / width);
  
  constexpr int QUALITY = 6;
  const int TILESW = width;
  const int TILESH =
    ((num % TILESW) == 0) ? (num / TILESW) : (num / TILESW) + 1;
  printf("tiles: %d x %d\n", TILESW, TILESH);
  ImageRGBA out(TILE * TILESW, TILE * TILESH);
  out.Clear32(0x000033FF);
  for (int i = 0; i < cycle.size(); i++) {
    CHECK(i >= 0 && i < cycle.size());
    const ImageF &sdf = cycle[i];
    // const ImageRGBA tile =
    //   sdf.Make8Bit().GreyscaleRGBA().ScaleBy(RENDER_SCALE);

    const ImageRGBA tile =
      FontProblem::SDFThresholdAAFloat(SDF_CONFIG.onedge_value/255.0f,
                                       sdf,
                                       SDF_SIZE * RENDER_SCALE,
                                       SDF_SIZE * RENDER_SCALE,
                                       QUALITY).GreyscaleRGBA();
    
    const int y = i / TILESW;
    const int x = i % TILESW;
    // printf("%d-> %d,%d\n", i, x, y);
    /*
    out.BlendRect32(x * TILE, y * TILE,
                    SDF_SIZE * (RENDER_SCALE - 1),
                    SDF_SIZE * (RENDER_SCALE - 1),
                    0x003300FF);
    */
    out.BlendImage(x * TILE, y * TILE, tile);
    /*
    out.BlendText32(x * TILE, y * TILE, 0xFF0000FF,
                    StringPrintf("%d,%d=%d", x, y, i));
    */
  }

  string filename = "phrase.png";
  out.Save(filename);
  printf("Saved %s\n", filename.c_str());
  
  // Now generate a triangulated mesh.
  // The input to the marching cubes algorithm is a 3D SDF, which we
  // make by just stacking up the SDFs we just computed. The x,y
  // dimensions are the same as the SDFs above, and z is "time". z=0
  // is earlier.

  using Pos = MarchingCubes::Pos;
  
  // Dimensions will be SDF_SIZE x SDF_SIZE x ZSIZE;
  constexpr float ZSIZE = 3.0f * SDF_SIZE;
  // Note that with linear filtering, the output mesh will
  // inherently be faceted, maybe a lot!
  constexpr float CELLSIZE = 0.5f;
  
  auto SDF3D = [&cycle](Pos pos) -> float {
      // Trilinear filter: Find the two slices that this
      // point lies between, do bilinear sample of them,
      // and linearly interpolate.
      float zidx = (pos.z / ZSIZE) * (cycle.size() - 1);
      
      const int z0 = zidx;
      const int z1 = z0 + 1;
      const float zf = zidx - z0;
      
      auto Sample = [&cycle, &pos](int z) -> float {
          if (z < 0) return 0.0f;
          if (z >= cycle.size()) return 0.0f;
          return cycle[z].SampleBilinear(pos.x, pos.y);
        };

      const float s0 = Sample(z0);
      const float s1 = Sample(z1);

      const float s = s0 * (1.0f - zf) + s1 * zf;
      
      // s is in [0,1] with onedge_value/255 being the cutoff.
      // for mc we want 0 as onedge, with positive meaning outside.

      // centered on zero
      const float scentered = s - (SDF_CONFIG.onedge_value/255.0f);
      // and negate the direction
      return -scentered;
    };

  Timer triangulate;
  MarchingCubes::Mesh mesh =
    MarchingCubes::Generate(Pos(0.0f, 0.0f, 0.0f),
                            Pos(SDF_SIZE, SDF_SIZE, ZSIZE),
                            CELLSIZE, SDF3D);
  printf("Got mesh in %.2fs\n", triangulate.MS() / 1000.0f);
  
  using Vertex = MarchingCubes::Vertex;
  string obj = "o isosurface\n";
  for (const Vertex &v : mesh.vertices) {
    StringAppendF(&obj, "v %f %f %f\n", v.pos.x, v.pos.y, v.pos.z);
  }
  for (const Vertex &v : mesh.vertices) {
    StringAppendF(&obj, "vn %f %f %f\n", v.normal.x, v.normal.y, v.normal.z);
  }
  for (const auto [a, b, c] : mesh.triangles) {
    // +1 because obj files count from 1
    const int a1 = a + 1, b1 = b + 1, c1 = c + 1;
    StringAppendF(&obj,"f %d//%d %d//%d %d//%d\n",
                  a1, a1, b1, b1, c1, c1);
  }

  Util::WriteFile("phrase.obj", obj);
  
  printf("Triangulated in %.2fs\n"
         "Mesh size %d verts, %d tris\n",
         triangulate.MS() / 1000.0f,
         (int)mesh.vertices.size(),
         (int)mesh.triangles.size());
  
  printf("Done in %.2fs\n", timer.MS() / 1000.0);
  return 0;
}
