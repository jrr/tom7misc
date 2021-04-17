
#include <string>

#include "level.h"
#include "escape-util.h"
#include "graphics.h"

#include "../cc-lib/image.h"
#include "../cc-lib/util.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"

static string self;

// Maybe move to "escape lib"?

namespace {
struct Ent {
  int x, y;
  bot type;
  Ent(int x, int y, bot type) : x(x), y(y), type(type) {}
  // Direction is not needed (always initial direction: down).
  // bota also not needed (bombs always in initial state).
};
}  // namespace

static ImageRGBA MakeScreenshot(const Graphics &graphics,
                                const Level &level) {
  ImageRGBA out(level.w * TILEW, level.h * TILEH);

  auto DrawPlayer = [&graphics, &out](int gx, int gy, bool dead) {
      const ImageRGBA *img =
        dead ? &graphics.lasered2 : &graphics.walk_forward_0;
      
      out.BlendImage(gx, gy - GUY_OVERLAPY, *img);
    };

  auto DrawBot = [&graphics, &out](int bx, int by, bot type) {
      int overlapy = 0;
      const ImageRGBA *img = &graphics.error;

      switch (type) {
      case B_BROKEN:
        overlapy = BROKEN_OVERLAPY;
        img = &graphics.deadrobot;
        break;

      case B_HUGBOT:
        overlapy = HUGBOT_OVERLAPY;
        img = &graphics.hugbot_forward_0;
        break;

      case B_DALEK:
        overlapy = DALEK_OVERLAPY;
        img = &graphics.dalek_forward_0;
        break;


      case B_HUGBOT_ASLEEP:
        overlapy = HUGBOT_OVERLAPY;
        img = &graphics.hugbot_asleep;
        break;

      case B_DALEK_ASLEEP:
        overlapy = DALEK_OVERLAPY;
        img = &graphics.dalek_asleep;
        break;

        /* draw nothing, though these are unexpected in levels! */
      case B_BOMB_X: return;
      case B_DELETED: return;
        
      default:
        if (Level::isbomb(type)) {
          overlapy = BOMB_OVERLAPY;
          img = &graphics.bomb_still;
        }
        break;
      }

      out.BlendImage(bx, by - overlapy, *img);
    };
  
  for (int y = 0; y < level.h; y++) {
    for (int x = 0; x < level.w; x++) {
      int t = level.tileat(x, y);
      auto [sx, sy] = Graphics::TileXY(t);

      // TODO: open exit special case
      out.CopyImageRect(x * TILEW, y * TILEH,
                        graphics.tiles,
                        sx, sy, TILEW, TILEH);
    }
  }

  vector<Ent> ents;
  ents.emplace_back(level.guyx, level.guyy, B_PLAYER);
  for (int i = 0; i < level.nbots; i++) {
    int x, y;
    level.where(level.boti[i], x, y);
    ents.emplace_back(x, y, level.bott[i]);
  }

  // Sort by y index, since they overlap a little.
  std::sort(ents.begin(), ents.end(),
            [](const Ent &a, const Ent &b) {
              if (a.y == b.y) {
                return a.x < b.x;
              } else {
                return a.y < b.y;
              }
            });

  // Draw bots
  for (const Ent &ent : ents) {
    if (ent.type == B_PLAYER) {
      // Draw player, maybe dead.
      int dx, dy;
      dir dd;
      bool isdead = level.isdead(dx, dy, dd);
  
      DrawPlayer(ent.x * TILEW, ent.y * TILEH,
                 isdead);
      // XXX draw laser if dead with direction.
      // (but probably outside this loop...)
    } else {
      DrawBot(ent.x * TILEW, ent.y * TILEH, ent.type);
    }
  }
  
  return out;
}

static ImageRGBA MakeSizedScreenshot(const Graphics &graphics,
                                     int max_width, int max_height,
                                     const Level &level) {
  // Note: objects can exceed the bounds of a tile. If
  // the border allows for it, would be nice if e.g. the
  // player at 0,0 didn't have his head clipped off. But
  // for a one-pixel border, maybe this is what we want.
  constexpr int TOP = 1;
  constexpr int BOTTOM = 1;
  constexpr int MARGINH = TOP + BOTTOM;
  constexpr int LEFT = 1;
  constexpr int RIGHT = 1;
  constexpr int MARGINW = LEFT + RIGHT;

  // First we just draw it 1:1.
  ImageRGBA ss = MakeScreenshot(graphics, level);

  int scaledown = 1;
  while (((level.w * TILEW) / scaledown) > (max_width - MARGINW) ||
         ((level.h * TILEH) / scaledown) > (max_height - MARGINH)) {
    scaledown++;
  }
  if (scaledown > 1) ss = ss.ScaleDownBy(scaledown);
  
  ImageRGBA out(ss.Width() + MARGINW, ss.Height() + MARGINH);
  out.Clear32(0x000000FF);
  out.CopyImage(TOP, LEFT, ss);
  return out;
}

int main(int argc, char **argv) {

  /* change to location of binary, so that we can find the
     images needed. */
  if (argc > 0) {
    string wd = EscapeUtil::pathof(argv[0]);
    EscapeUtil::changedir(wd);

#   if WIN32
    /* on win32, the ".exe" may or may not
       be present. Also, the file may have
       been invoked in any CaSe. */

    self = EscapeUtil::lcase(EscapeUtil::fileof(argv[0]));
    self = EscapeUtil::ensureext(self, ".exe");

#   else

    self = EscapeUtil::fileof(argv[0]);

#   endif

  }

  if (argc != 3) {
    fprintf(stderr,
            "Usage: screenshot /path/to/level.esx /path/to/output.png\n");
    fprintf(stderr,
            "(relative paths will be interpreted from the location of the\n"
            " screenshot binary.)\n");
    return 1;
  }

  std::unique_ptr<Graphics> graphics(Graphics::Create("."));
  CHECK(graphics.get() != nullptr) << "Failed to load graphics from .";

  string inlev = argv[1];
  string outfile = argv[2];
  std::unique_ptr<Level> level =
    Level::FromString(Util::ReadFile(inlev));
  if (level.get() == nullptr) {
    fprintf(stderr, "Can't open %s\n", inlev.c_str());
    return -1;
  }
  
  ImageRGBA out = MakeSizedScreenshot(*graphics, 300, 300, *level);
  out.Save(outfile);
  
  return 0;
}
