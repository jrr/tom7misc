#include "level.h"

#include <cstdint>
#include <vector>
#include <string>

#include "../cc-lib/stb_image.h"
#include "../cc-lib/color-util.h"

using namespace std;
using uint8 = uint8_t;

#define TILEW 32
#define TILEH 32
#define SRCTILESW 16

struct RGB {
  float r;
  float g;
  float b;
};

struct LAB {
  float l;
  float a;
  float b;
};

int main(int argc, char **argv) {
  // TODO: command-line args, etc.
  int w, h, comp_unused;
  uint8 *input_rgba = stbi_load("exit.png", &w, &h, &comp_unused, 4);

  printf("Input %d x %d...\n", w, h);
  
  int tilesw, tilesh, tilescomp_unused;
  uint8 *tiles_rgba = stbi_load("tiles.png", &tilesw, &tilesh,
				&tilescomp_unused, 4);

  printf("Tiles %d x %d...\n", tilesw, tilesh);

  // Now prepare centroid values for all tiles.
  
  vector<LAB> labs;
  labs.reserve(NUM_TILES);
  for (int i = 0; i < NUM_TILES; i++) {
    int row = i / SRCTILESW;
    int col = i % SRCTILESW;
    float r = 0, g = 0, b = 0;
    for (int y = 0; y < TILEH; y++) {
      for (int x = 0; x < TILEW; x++) {
	int yy = row * TILEH + y;
	int xx = col * TILEW + x;
	int idx = (yy * (SRCTILESW * TILEW) + xx) * 4;

	r += tiles_rgba[idx + 0];
	g += tiles_rgba[idx + 1];
	b += tiles_rgba[idx + 2];
      }
    }
    LAB lab;
    constexpr float denom = 255.0f * TILEH * TILEW;
    ColorUtil::RGBToLAB(r / denom, g / denom, b / denom,
			&lab.l, &lab.a, &lab.b);
    labs.push_back(lab);
  }

  Level *level = Level::blank(w, h);
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      int idx = (y * w + x) * 4;
      float r = input_rgba[idx + 0] / 255.0;
      float g = input_rgba[idx + 1] / 255.0;
      float b = input_rgba[idx + 2] / 255.0;
      // alpha ignored for now...
      LAB lab;
      ColorUtil::RGBToLAB(r, g, b,
			  &lab.l, &lab.a, &lab.b);
      // Now compute closest-looking tile.
      float best = 99999999.0f;
      int besti = 0;
      for (int i = 0; i < labs.size(); i++) {
	float dist = ColorUtil::DeltaE(lab.l, lab.a, lab.b,
				       labs[i].l, labs[i].a, labs[i].b);
	if (dist < best) {
	  best = dist;
	  besti = i;
	}
      }
      level->settile(x, y, besti);
    }
  }

  // Now just save.
  level->title = "png";
  level->author = "png2esx";
  level->w = w;
  level->h = h;
  level->guyx = 0;
  level->guyy = 0;
  level->sanitize();
  writefile("png.esx", level->tostring());
  printf("wrote png.esx.\n");
}
