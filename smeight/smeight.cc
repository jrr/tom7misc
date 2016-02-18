#include <vector>
#include <string>
#include <set>
#include <memory>

#include <cstdio>
#include <cstdlib>
#include <cmath>

#include "smeight.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/stb_image.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/stb_image_write.h"

// XXX make part of Emulator interface
#include "../fceulib/ppu.h"
#include "../fceulib/cart.h"

#include "SDL.h"
#include <GL/gl.h>
#include <GL/glext.h>

#include "matrices.h"

#define WIDTH 1920
#define HEIGHT 1080
static constexpr double ASPECT_RATIO = WIDTH / (double)HEIGHT;

// Size of NES nametable
#define TILESW 32
#define TILESH 30
#define TILEST 32
static_assert(TILEST >= TILESW, "TILEST must hold TILESW");
static_assert(TILEST >= TILESW, "TILEST must hold TILESH");
static_assert(0 == (TILEST & (TILEST - 1)), "TILEST must be power of 2");

#define X 255,255,255
#define O 255,0,0
#define _ 32,32,32
static const GLubyte arrow_texture[8 * 8 * 3] = {
  _, _, _, O, O, _, _, _,

  _, _, O, X, X, O, _, _,

  _, O, X, X, X, X, O, _,

  O, O, O, X, X, O, O, O,

  _, _, _, X, X, _, _, _,

  _, _, _, X, X, _, O, _,

  _, _, _, X, X, _, _, _,

  _, _, _, X, X, _, _, _,
};
#undef X
#undef O
#undef _

// XXX
static GLuint texture_id[TILESW * TILESH] = {};
static GLuint bg_texture = 0;

typedef void (APIENTRY *glWindowPos2i_t)(int, int);
glWindowPos2i_t glWindowPos2i = nullptr;
static void GetExtensions() {
  #define INSTALL(s) \
    CHECK((s = (s ## _t)SDL_GL_GetProcAddress(# s))) << s;
  INSTALL(glWindowPos2i);
}

static GLubyte red[]     = { 255,   0,   0, 255 };
static GLubyte green[]   = {   0, 255,   0, 255 };
static GLubyte blue[]    = {   0,   0, 255, 255 };
static GLubyte white[]   = { 255, 255, 255, 255 };
static GLubyte cyan[]    = {   0, 255, 255, 255 };
static GLubyte black[]   = {   0,   0,   0, 255 };
static GLubyte yellow[]  = { 255, 255,   0, 255 };
static GLubyte magenta[] = { 255,   0, 255, 255 };


enum TileType {
  UNMAPPED = 0,
  FLOOR = 1,
  WALL = 2,
};

struct Tilemap {
  Tilemap() {}
  explicit Tilemap(const string &filename) {
    vector<string> lines = Util::ReadFileToLines(filename);
    for (string line : lines) {
      string key = Util::chop(line);
      if (key.empty() || key[0] == '#') continue;
      
      if (key.size() != 2 || !Util::IsHexDigit(key[0]) || !Util::IsHexDigit(key[1])) {
	printf("Bad key in tilemap. Should be exactly 2 hex digits: [%s]\n", key.c_str());
	continue;
      }

      uint8 h = Util::HexDigitValue(key[0]) * 16 + Util::HexDigitValue(key[1]);
      if (data[h] != UNMAPPED) {
	printf("Duplicate keys in tilemap: %02x\n", h);
      }

      string value = Util::chop(line);

      if (value == "wall") {
	data[h] = WALL;
      } else if (value == "floor") {
	data[h] = FLOOR;
      } else {
	printf("Unknown tilemap value: [%s] for %02x\n", value.c_str(), h);
      }
    }
  }
    
  vector<TileType> data{256, UNMAPPED};
};

//           
//           15o
//         |/  
//  270o --+--
//         |
//
// Find the distance (in degrees) to travel clockwise to reach the end
// angle from the start angle. This will always be positive, and may
// need to wrap around 0.
static int CWDistance(int start_angle, int end_angle) {
  // const int qs = Quadrant(start_angle), qe = Quadrant(end_angle);
  if (end_angle >= start_angle) {
    return end_angle - start_angle;
  } else {
    return end_angle - start_angle + 360;
  }
}

// Again, always positive.
static int CCWDistance(int start_angle, int end_angle) {
  return CWDistance(end_angle, start_angle);
}

struct SM {
  std::unique_ptr<Emulator> emu;
  vector<uint8> inputs;
  Tilemap tilemap;
  
  SM() : rc("sm") {
    map<string, string> config = Util::ReadFileToMap("config.txt");
    if (config.empty()) {
      fprintf(stderr, "Missing config.txt.\n");
      abort();
    }

    const string game = config["game"];
    const string tilesfile = config["tiles"];
    const string moviefile = config["movie"];
    CHECK(!game.empty());
    CHECK(!tilesfile.empty());
    
    tilemap = Tilemap{tilesfile};

    // screen = sdlutil::makescreen(WIDTH, HEIGHT);
    // CHECK(screen);

    emu.reset(Emulator::Create(game));
    CHECK(emu.get());

    if (!moviefile.empty()) {
      inputs = SimpleFM2::ReadInputs(moviefile);
      printf("There are %d inputs in %s.\n", (int)inputs.size(), moviefile.c_str());
      const int warmup = atoi(config["warmup"].c_str());
      CHECK(inputs.size() >= warmup);
      // If we have warmup, advance to that point and discard them from inputs.
      for (int i = 0; i < warmup; i++) {
	emu->StepFull(inputs[i], 0);
      }
      inputs.erase(inputs.begin() + 0, inputs.begin() + warmup);
    }

    printf("There are %d inputs left after warmup.\n", (int)inputs.size());
  }

  void Play() {
    /*
    font = Font::create(screen,
			"font.png",
			FONTCHARS,
			FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
    CHECK(font != nullptr) << "Couldn't load font.";

    smallfont = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(smallfont != nullptr) << "Couldn't load smallfont.";
    */

    Loop();
    printf("UI shutdown.\n");
  }

  struct Box {
    Vec3 loc;
    int texture_x, texture_y;
  };
  
  void Loop() {
    SDL_Surface *surf = sdlutil::makesurface(256, 256, true);
    (void)surf;
    int frame = 0;

    int start = SDL_GetTicks();
    
    vector<uint8> start_state = emu->SaveUncompressed();

    int current_angle = 0;
    
    for (;;) {
      printf("Start loop!\n");
      emu->LoadUncompressed(start_state);

      for (uint8 input : inputs) {
	frame++;

	SDL_Event event;
	SDL_PollEvent(&event);
	switch (event.type) {
	case SDL_QUIT:
	  return;
	case SDL_KEYDOWN:
	  switch (event.key.keysym.sym) {
	    
	  case SDLK_ESCAPE:
	    return;
	  default:;
	  }
	  break;
	default:;
	}

	// SDL_Delay(1000.0 / 60.0);

	emu->StepFull(input, 0);

	vector<uint8> image = emu->GetImageARGB();
	// CopyARGB(image, surf);

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  
	BlitNESGL(image, 0, HEIGHT);
	// GetTextures();
	
	vector<Box> boxes = GetBoxes();


	uint8 player_x, player_y;
	int player_angle;
	std::tie(player_x, player_y, player_angle) = GetLoc();
	
	// Smooth angle a bit.
	// Maximum degrees to turn per frame.
	static constexpr int MAX_SPIN = 6;
	if (current_angle != player_angle) {
	  // Find the shortest path, which may involve going
	  // through zero.

	  int cw = CWDistance(current_angle, player_angle);
	  int ccw = CCWDistance(current_angle, player_angle);

	  if (cw < ccw) {
	    // Turn clockwise.
	    if (cw < MAX_SPIN) current_angle = player_angle;
	    else current_angle += MAX_SPIN;
	  } else {
	    if (ccw < MAX_SPIN) current_angle = player_angle;
	    else current_angle -= MAX_SPIN;
	  }

	  while (current_angle < 0) current_angle += 360;
	  while (current_angle >= 360) current_angle -= 360;
	}
	DrawScene(boxes, player_x, player_y, current_angle);

	// DrawPPU();
	// DrawScene();
	// Benchmark();

	SaveImage();
	
	SDL_GL_SwapBuffers();

	if (frame % 1000 == 0) {
	  int now = SDL_GetTicks();
	  printf("%d frames in %d ms = %f fps.\n",
		 frame, (now - start),
		 frame / ((now - start) / 1000.0f));
	}
      }
    }
  }

  int imagenum = 0;
  bool saving = false;
  void SaveImage() {
    if (!saving) return;
    
    vector<uint8> pixels;
    pixels.resize(WIDTH * HEIGHT * 4);
    glReadPixels(0, 0, WIDTH, HEIGHT, GL_RGBA, GL_UNSIGNED_BYTE, pixels.data());
    stbi_write_png(StringPrintf("image_%d.png", imagenum).c_str(),
		   WIDTH, HEIGHT, 4,
		   // Start on last row.
		   pixels.data() + (WIDTH * (HEIGHT - 1)) * 4,
		   // Negative stride to flip during saving.
		   -4 * WIDTH);
    imagenum++;
  }
  
  void GetTextures() {
    // BG pattern table can be at 0 or 0x1000. For zelda, BG seems
    // to always be at 0x1000, so this is hardcoded for now...
    const uint32 addr = 0x1000;
    const uint8 *vram = &emu->GetFC()->cart->VPage[addr >> 10][addr];

    vector<uint8> patterns;
    patterns.resize(256 * 8 * 8 * 4);

    for (int ty = 0; ty < 16; ty++) {
      for (int tx = 0; tx < 16; tx++) {
	// Each tile is made of 8 bytes giving its low color bits, then
	// 8 bytes giving its high color bits.
	const int addr = ((ty * 16) + tx) * 16;

	// Decode vram[addr] + vram[addr + 1].
	for (int row = 0; row < 8; row++) {
	  uint8 row_low = vram[addr + row];
	  uint8 row_high = vram[addr + row + 8];

	  // bit from msb to lsb.
	  for (int bit = 0; bit < 8; bit++) {
	    uint8 value =
	      ((row_low >> (7 - bit)) & 1) |
	      (((row_high >> (7 - bit)) & 1) << 1);
	    // XXX: Look up in palette. This just makes
	    // greyscale:
	    static constexpr int greys[4] = {0, 85, 170, 255};
	    value = greys[value];
	    // Put pixel in pattern image:

	    int px = tx * 8 + bit;
	    int py = ty * 8 + row;
	    int pixel = (py * (16 * 8) + px) * 4;
	    patterns[pixel + 0] = value;
	    patterns[pixel + 1] = value;
	    patterns[pixel + 2] = value;
	    patterns[pixel + 3] = 0xFF;
	  }
	}
      }
    }

    glPixelZoom(2.0f, -2.0f);
    (*glWindowPos2i)(256, HEIGHT);
    glDrawPixels(16 * 8, 16 * 8, GL_BGRA, GL_UNSIGNED_BYTE, patterns.data());

    for (int ty = 0; ty < 16; ty++) {
      for (int tx = 0; tx < 16; tx++) {
	vector<uint8> chr;
	chr.resize(8 * 8 * 4);
	for (int y = 0; y < 8; y++) {
	  for (int x = 0; x < 8; x++) {
	    // Pixel coordinates in patterns image.
	    int px = tx * 8 + x;
	    int py = ty * 8 + y;
	    for (int c = 0; c < 4; c++) {
	      // 7 - y to flip vertically.
	      chr[(y * 8 + x) * 4 + c] =
		patterns[(py * (16 * 8) + px) * 4 + c];
	    }
	  }
	}
	
	glBindTexture(GL_TEXTURE_2D, texture_id[ty * 16 + tx]);
	glTexSubImage2D(GL_TEXTURE_2D, 0,
			0, 0, 8, 8,
			GL_RGBA, GL_UNSIGNED_BYTE,
			chr.data());
	#if 0
	for (int row = 0; row < 8; row++) {
	  // Pixel offset of row
	  int px = x * 8;
	  int py = y * 8 + row;
	  glTexSubImage2D(GL_TEXTURE_2D, 0,
			  // offset
			  0, row,
			  // width, height		     
			  8, 1,
			  GL_RGBA, GL_UNSIGNED_BYTE,
			  &patterns[(py * (16 * 8) + px) * 4]);
	}
	#endif
      }
    }
    
    /*
    const uint8 *nametable = emu->GetFC()->ppu->NTARAM;
    for (int y = 0; y < TILESH; y++) {
      for (int x = 0; x < TILESW; x++) {
	int t = y * TILESW + x;
	const uint8 tile = nametable[y * TILESW + x];
      }
    }
    */
  }
  
  // Draw the 256x256 NES image at the specified coordinates (0,0 is
  // bottom left).
  void BlitNESGL(const vector<uint8> &image, int x, int y) {
    glPixelZoom(1.0f, -1.0f);
    (*glWindowPos2i)(x, y);
    glDrawPixels(256, 256, GL_BGRA, GL_UNSIGNED_BYTE, image.data());
  }
  // All boxes are 1x1x1. This returns their "top-left" corners. Larger Z is "up".
  //
  //    x=0,y=0 -----> x = TILESW-1 = 31
  //    |
  //    :
  //    |
  //    v
  //   y = TILESH - 1 = 29
  //
  vector<Box> GetBoxes() {
    vector<Box> ret;
    ret.reserve(TILESW * TILESH);
    const uint8 *nametable = emu->GetFC()->ppu->NTARAM;

    // BG pattern table can be at 0 or 0x1000. For zelda, BG seems
    // to always be at 0x1000, so this is hardcoded for now...
    const uint32 addr = 0x1000;
    const uint8 *vram = &emu->GetFC()->cart->VPage[addr >> 10][addr];
    
    // The actual BG image, used as a texture for the blocks.
    vector<uint8> bg;
    // Don't bother reserving TILEST size; we'll just copy the portion
    // that might change into the larger texture.
    bg.resize(TILESW * TILESH * 8 * 8 * 4);

    for (int sy = 0; sy < TILESH; sy++) {
      for (int sx = 0; sx < TILESW; sx++) {
	int t = sy * TILESW + sx;
	const uint8 tile = nametable[sy * TILESW + sx];

	// Coordinates of tile in CHR. This can probably be simplified
	// with the next line...
	const uint8 ty = tile >> 4;
	const uint8 tx = tile & 15;
	
	// Draw to BG.
	// Each tile is made of 8 bytes giving its low color bits, then
	// 8 bytes giving its high color bits.
	const int addr = ((ty * 16) + tx) * 16;

	// Decode vram[addr] + vram[addr + 1].
	for (int row = 0; row < 8; row++) {
	  uint8 row_low = vram[addr + row];
	  uint8 row_high = vram[addr + row + 8];

	  // bit from msb to lsb.
	  for (int bit = 0; bit < 8; bit++) {
	    uint8 value =
	      ((row_low >> (7 - bit)) & 1) |
	      (((row_high >> (7 - bit)) & 1) << 1);
	    // XXX: Look up in palette. This just makes
	    // greyscale:
	    static constexpr int greys[4] = {0, 85, 170, 255};
	    value = greys[value];
	    // Put pixel in bg image:

	    const int px = sx * 8 + bit;
	    const int py = sy * 8 + row;
	    const int pixel = (py * TILESW * 8 + px) * 4;
	    bg[pixel + 0] = value;
	    bg[pixel + 1] = value;
	    bg[pixel + 2] = value;
	    bg[pixel + 3] = 0xFF;
	  }
	}

	
	const TileType type = tilemap.data[tile];
	float z = 0.0f;
	if (type == WALL) {
	  z = 1.0f;
	} else if (type == FLOOR) {
	  z = 0.0f;
	  // continue;
	} else if (type == UNMAPPED) {
	  z = 2.0f;
	}

	ret.push_back(Box{Vec3{(float)sx, (float)sy, z}, sx, sy});
      }
    }
    
    // 1x1 blocks
#if 0
    for (int y = 0; y < TILESH; y++) {
      for (int x = 0; x < TILESW; x++) {
	int t = y * TILESW + x;
	const uint8 tile = nametable[y * TILESW + x];
	const TileType type = tilemap.data[tile];
	float z = 0.0f;
	if (type == WALL) {
	  z = 1.0f;
	} else if (type == FLOOR) {
	  z = 0.0f;
	  // continue;
	} else if (type == UNMAPPED) {
	  z = 2.0f;
	}
	ret.push_back(Box{Vec3{(float)x, (float)y, z}, tile});
      }
    }
#endif

    // I think the best plan here is to just render the whole
    // screen to a single texture, and then use texture coordinates
    // here (as part of Box) to paint each one. - Tom on night of 15 Feb 2016.

    #if 0
    for (int y = 0; y < TILESH; y += 2) {
      for (int x = 0; x < TILESW; x += 2) {
	int t = y * TILESW + x;
	const uint8 tile = nametable[y * TILESW + x];
	const TileType type = tilemap.data[tile];
	float z = 0.0f;
	if (type == WALL) {
	  z = 1.0f;
	} else if (type == FLOOR) {
	  z = 0.0f;
	  // continue;
	} else if (type == UNMAPPED) {
	  z = 2.0f;
	}
	ret.push_back(Box{Vec3{(float)x, (float)y, z}, tile});
      }
    }
    #endif

    // Copy background image into bg texture.
    glBindTexture(GL_TEXTURE_2D, bg_texture);
    glTexSubImage2D(GL_TEXTURE_2D, 0,
		    0, 0, TILESW * 8, TILESH * 8,
		    GL_RGBA, GL_UNSIGNED_BYTE,
		    bg.data());
    
    return ret;
  }

  void DrawScene(const vector<Box> &orig_boxes,
		 int player_x, int player_y,
		 int player_angle) {
    // Put boxes in GL space where Y=0 is bottom-left, not top-left.
    // This also changes the origin of the box itself.
    vector<Box> boxes;
    boxes.reserve(orig_boxes.size());
    for (const Box &box : orig_boxes) {
      boxes.push_back(Box{Vec3{box.loc.x, (float)(TILESH - 1) - box.loc.y, box.loc.z},
	    box.texture_x, box.texture_y});
    }

    // printf("There are %d boxes.\n", (int)boxes.size());
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    // Put player on ground with top of screen straight ahead.
    glRotatef(-90.0f, 1.0, 0.0, 0.0);

    // Rotate according to the direction the player is facing.
    glRotatef(player_angle, 0.0, 0.0, 1.0);
    
    // Move "camera".
    glTranslatef(player_x / -8.0f, ((TILESH * 8 - 1) - player_y) / -8.0f, -2.0f);

    // XXX don't need this
    glBegin(GL_LINE_STRIP);
    glVertex3f(0.0f, 0.0f, 0.0f);
    glVertex3f((float)TILESW, 0.0f, 0.0f);
    glVertex3f((float)TILESW, (float)TILESH, 0.0f);
    glVertex3f(0.0f, (float)TILESH, 0.0f);
    glVertex3f(0.0f, 0.0f, 0.0f);
    glEnd();
    
    glEnable(GL_TEXTURE_2D);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);

    glBindTexture(GL_TEXTURE_2D, bg_texture);
    glBegin(GL_TRIANGLES);
    
    // Draw boxes as a bunch of triangles.
    for (const Box &box : boxes) {
      // printf("Bind texture %d\n", box.texture);
      // glBindTexture(GL_TEXTURE_2D, texture_id[box.texture]); // box.texture);

      
      // Here, boxes are properly oriented in GL axes, like this. The
      // input vector is the front bottom left corner, a:
      //
      //      g-----h
      //     /:    /|              ^   7
      //    d-----c |            + |  / +
      //    | :   | |            z | / y
      //    | f---|-e            - |/ -
      //    |/    |/               0------->
      //    a-----b                  -x+
      // (0,0,0)
      
      Vec3 a = box.loc;
      Vec3 b = Vec3Plus(a, Vec3{1.0f, 0.0f, 0.0f});
      Vec3 c = Vec3Plus(a, Vec3{1.0f, 0.0f, 1.0f});
      Vec3 d = Vec3Plus(a, Vec3{0.0f, 0.0f, 1.0f});
      Vec3 e = Vec3Plus(a, Vec3{1.0f, 1.0f, 0.0f});
      Vec3 f = Vec3Plus(a, Vec3{0.0f, 1.0f, 0.0f});
      Vec3 g = Vec3Plus(a, Vec3{0.0f, 1.0f, 1.0f});
      Vec3 h = Vec3Plus(a, Vec3{1.0f, 1.0f, 1.0f});

      // XXX
      static constexpr float TD = 8.0f / (8.0f * TILEST);
      const float tx = box.texture_x * TD;
      const float ty = box.texture_y * TD;
      
      // Give bottom left first, and go clockwise.
      auto CCWFace = [tx, ty](const Vec3 &a, const Vec3 &b, const Vec3 &c, const Vec3 &d) {
	//  (0,0) (1,0)
	//    d----c
	//    | 1 /|
	//    |  / |
	//    | /  |
	//    |/ 2 |
	//    a----b
	//  (0,1)  (1,1)  texture coordinates

	glTexCoord2f(tx,      ty + TD); glVertex3fv(a.Floats());
	glTexCoord2f(tx + TD, ty);      glVertex3fv(c.Floats());
	glTexCoord2f(tx,      ty);      glVertex3fv(d.Floats());

	glTexCoord2f(tx,      ty + TD); glVertex3fv(a.Floats());
	glTexCoord2f(tx + TD, ty + TD); glVertex3fv(b.Floats());
	glTexCoord2f(tx + TD, ty);      glVertex3fv(c.Floats());
      };

      // Top
      // glColor4ubv(red);
      CCWFace(d, c, h, g);

      // Front
      // glColor4ubv(magenta);
      CCWFace(a, b, c, d);

      // Back
      // glColor4ubv(magenta);
      CCWFace(e, f, g, h);

      // Right
      // glColor4ubv(green);
      CCWFace(b, e, h, c);

      // Left
      // glColor4ubv(green);
      CCWFace(f, a, d, g);

      // Bottom
      // glColor4ubv(yellow);
      CCWFace(f, e, b, a);

    }

    glEnd();
    glDisable(GL_TEXTURE_2D);
  }

  // x, y, angle (deg, where 0 is north)
  std::tuple<uint8, uint8, int> GetLoc() {
    vector<uint8> ram = emu->GetMemory();
    
    uint8 dir = ram[0x98];

    // Link's top-left corner, so add 8,8 to get center.
    uint8 lx = ram[0x70] + 8, ly = ram[0x84] + 8;

    int angle = 0;
    switch (dir) {
    case 1: angle = 90; break;
    case 2: angle = 270; break;
    case 4: angle = 180; break;
    case 8: angle = 0; break;
    default:;
    }

    return make_tuple(lx, ly, angle);
  }
  
 private:
  static constexpr int FONTWIDTH = 9;
  static constexpr int FONTHEIGHT = 16;
  static constexpr int SMALLFONTWIDTH = 6;
  static constexpr int SMALLFONTHEIGHT = 6;
  
  // Font *font = nullptr, *smallfont = nullptr;
  ArcFour rc;
  NOT_COPYABLE(SM);
};

// Same as gluPerspective, but without depending on GLU.
static void PerspectiveGL(double fovY, double aspect, double zNear, double zFar) {
  static constexpr double pi = 3.1415926535897932384626433832795;
  const double fH = tan(fovY / 360.0 * pi) * zNear;
  const double fW = fH * aspect;
  glFrustum(-fW, fW, -fH, fH, zNear, zFar);
}

static void InitGL() {
  // Pixels!!
  glShadeModel(GL_FLAT);

  // Probably no need for backface culling except perhaps if the
  // camera gets inside walls, but turn it on anyway...
  glCullFace(GL_BACK);
  glFrontFace(GL_CCW);
  glEnable(GL_CULL_FACE);

  glClearColor(0, 0, 0, 0);

  // Set up the screen. Recall that (0,0) is the bottom-left corner,
  // not the top-left.
  glViewport(0, 0, WIDTH, HEIGHT);

  glEnable(GL_DEPTH_TEST);
  
  glEnable(GL_MULTISAMPLE);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  // Allocate all cube textures and initialize with "missing" arrow.
  glGenTextures(TILESW * TILESH, texture_id);

  for (int i = 0; i < TILESW * TILESH; i++) {
    glBindTexture(GL_TEXTURE_2D, texture_id[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 8, 8, 0,
		 GL_RGB, GL_UNSIGNED_BYTE, arrow_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  
    // Maybe want something like this, but we should always be using
    // full textures!
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  }

  // This should replace the above array.
  {
    glGenTextures(1, &bg_texture);
    glBindTexture(GL_TEXTURE_2D, bg_texture);
    vector<uint8> bg;
    bg.resize(8 * 8 * TILEST * TILEST * 4);
    // PERF is RGBA a good choice? It aligns better but does it make
    // blitting much more costly because of the (unused) alpha channel?
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 256, 256, 0,
		 GL_RGBA, GL_UNSIGNED_BYTE, bg.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    // No good answer here. Just make sure to hit pixels exactly.
    // glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    // glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  }
  
  
  PerspectiveGL(60.0, ASPECT_RATIO, 1.0, 1024.0);

  GetExtensions();
}

/**
 * The main loop for the SDL.
 */
int main(int argc, char *argv[]) {
  (void)red; (void)green; (void)blue; (void)white;
  (void)cyan; (void)black; (void)yellow; (void)magenta;
  
  fprintf(stderr, "Init SDL\n");

  CHECK(SDL_Init(SDL_INIT_VIDEO) >= 0) << SDL_GetError();

  const SDL_VideoInfo *info = SDL_GetVideoInfo();
  CHECK(info != nullptr) << SDL_GetError();

  const int bpp = info->vfmt->BitsPerPixel;

  if (bpp != 32) {
    fprintf(stderr, "This probably won't work unless BPP is "
	    "32, but I got %d from SDL.\n", bpp);
  }

  // Gimme quality.
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

  // 2x MSAA -- doesn't do much though since the textures
  // are still linear interpolation. Should instead render
  // offscreen and then downsample.
  SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4);
  
  // Can use SDL_FULLSCREEN here for full screen immersive
  // 8 bit action!!
  CHECK(SDL_SetVideoMode(WIDTH, HEIGHT, bpp,
			 SDL_OPENGL)) << SDL_GetError();

  InitGL();
  
  {
    SM sm;
    sm.Play();
  }

  SDL_Quit();
  return 0;
}
