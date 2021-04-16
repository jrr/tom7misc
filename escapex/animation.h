
#ifndef _ESCAPE_ANIMATION_H
#define _ESCAPE_ANIMATION_H

#include <SDL.h>
#include "escapex.h"
#include "level.h"
#include "draw.h"
#include "aevent.h"
#include "escape-util.h"
#include "dirt.h"
#include "sound.h"
#include "graphics.h"

/* XXX should be at least 1 */
#define FLIPS_OVERSAMPLE 1

/* maximum y value of any animation */
#define YOSCALE 65536

/* an animation frame with timing and position information. */
struct AFrame {
  SDL_Surface **pic;
  /* offsets */
  int x, y;
  /* amount of time to wait before drawing this frame */
  int wait;
};

/* these are actual 'in-progress' animation objects.

   for example, consider a yellow block flying across
   the screen. This knows where it currently is,
   where it's going, and how many ticks before the next
   frame should be displayed. It also saves some information
   about the surface it's flying over, (ie, a graphic) so
   that it can draw and erase itself as it goes.
*/

struct Animation {
  /* when to think next */
  unsigned int nexttick;

  /* small integer; lower numbers correspond to lower (earlier)
     drawing order */
  int depth;
  /* should be actual screen coordinate. determines drawing order
     for animations at the same depth. */
  virtual int YOrder() = 0;

  /* This is called right before the animation
     is activated. It will always be called
     while the active animations are erased
     from the screen.

     If this has changed the state of the screen,
     then return true, so that they dirty mirror
     can be updated. */
  virtual bool Init(unsigned int now) {
    nexttick = now;
    return false;
  }

  /* the graphic from its current position,
     by invoking dirty->setdirty */
  virtual void Erase(Dirt *dirty) { }

  /* think as a result of the timer passing
     the point set by nexttick. this can
     change the internal state, etc. return
     true if the animation has died. */
  virtual bool Think(unsigned int now) = 0;

  /* draw at current location. */
  virtual void Draw() { }

  /* next animation in the chain,
     if applicable. To replace this one
     when think() returns true. */
  Animation *next;

  Animation() : depth(0), next(0), finale(false) {}

  static int YOrderCompare(Animation *a, Animation *b) {
    int r = ((a->depth * YOSCALE) + a->YOrder()) -
            ((b->depth * YOSCALE) + b->YOrder());

    /* if they are the same, use pointer comparison
       just to be more stable */
    if (r) return r;
    else return (a - b);
  }

  /* false if fail */
  static bool ainit(const Graphics &graphics);

  /* graphics. These are generated by packpng
     from animation.pack */
  using SDL_SurfacePtr = SDL_Surface *;
  static SDL_SurfacePtr
  #include "animation_syms.h"
  ;
  // # include "animation_defs.h"

  /* there will be DRAW_NSIZES copies of these */
  static SDL_Surface **pic_guy_up;
  static SDL_Surface **pic_guy_down;
  static SDL_Surface **pic_guy_left;
  static SDL_Surface **pic_guy_right;

  static SDL_Surface **pic_deadrobot;

  /* XXX these are currently all the same */
  static SDL_Surface **pic_dalek_up;
  static SDL_Surface **pic_dalek_down;
  static SDL_Surface **pic_dalek_left;
  static SDL_Surface **pic_dalek_right;

  /* XXX these are currently all the same */
  static SDL_Surface **pic_hugbot_up;
  static SDL_Surface **pic_hugbot_down;
  static SDL_Surface **pic_hugbot_left;
  static SDL_Surface **pic_hugbot_right;

  static SDL_Surface **pic_dalek_asleep_up;
  static SDL_Surface **pic_dalek_asleep_down;
  static SDL_Surface **pic_dalek_asleep_left;
  static SDL_Surface **pic_dalek_asleep_right;

  static SDL_Surface **pic_hugbot_asleep_up;
  static SDL_Surface **pic_hugbot_asleep_down;
  static SDL_Surface **pic_hugbot_asleep_left;
  static SDL_Surface **pic_hugbot_asleep_right;

  static SDL_Surface **pic_bomb_still;
  static SDL_Surface ***pic_bomb_lit;

  /* if finale is true, then the animation should
     not be removed despite returning 'true' from
     think -- instead, it should die when every
     animation is a finale and thinks true.

     finale true implies next is null. */
  bool finale;

  /* these are generated from tiles.png
     programatically */
  /* pvt? */
  static SDL_Surface **pic_flips_out_data;
  static SDL_Surface **pic_flips_in_data;
  static AFrame **frame_flips_out;
  static AFrame **frame_flips_in;

  /* push some animations for the event ae
     onto the list anims. assume anims is 0 */
  static void start(Drawing &dr,
                    PtrList<Animation> *&anims,
                    PtrList<Animation> *&sprites,
                    AEvent *ae);

  static void clearsprites(Drawing &dr);
  static void clearent(Drawing &dr, int ex, int ey, int olap);

  static void erase_anims(PtrList<Animation> *a, Dirt *d);

  /* Think all of the animations in 'as' that are ready.
     If an animation has finished, remove it from the
     list, perhaps starting the next one in the series.
     If anything changes the background, then remirror
     is set to true (otherwise it is untouched).

     If 'done' is true, then finales are eligible to be
     removed, as long as everything running is a finale. */
  static void think_anims(PtrList<Animation> **as, unsigned int now,
                          bool &remirror, bool done = false);

  /* Call draw method for every Animation in list. */
  static void draw_anims(PtrList<Animation> *a);

  /* Initialize every anim. Return true if any init method returns
     true. */
  static bool init_anims(PtrList<Animation> *a, unsigned int now);

  /* don't forget the tail */
  virtual ~Animation() { delete next; }

 private:

  static SDL_Surface *pitched_rect(int w, int h, int ph,
                                   SDL_Surface *src,
                                   int oversample = FLIPS_OVERSAMPLE);

  static bool ainit_frames(const Graphics &graphics);
  static bool ainit_flips();
};


struct AnInPlace : public Animation {
  int xpos, ypos;
  int loopsleft;
  int cframe = 0;
  AFrame *frames;

  /* our dirty area */
  int bx, by, bw, bh;

  /* could have other versions, ie for a certain time rather
     than # of loops */

  /* f is a pointer to an array of frames, where the last element
     of the array has a pic pointer of 0. There must be at least
     one frame. */
  AnInPlace(int x, int y, int loops, AFrame *f)
    : xpos(x), ypos(y), loopsleft(loops - 1), frames(f) {}

  int YOrder() override { return ypos + frames[cframe].y; }

  bool Think(unsigned int) override;
  void Draw() override;
  bool Init(unsigned int now) override;
  void Erase(Dirt *d) override;
};

/* sort of special, because it is used as the
   transition from a static tile that is part
   of the background and an animated version of
   that tile */
struct AnPlaceTile : public Animation {
  int what, sx, sy;
  AnPlaceTile(int what_, int sx_, int sy_)
    : what(what_), sx(sx_), sy(sy_) {}

  /* here the initializer draws it. */
  bool Init(unsigned int now) override {
    Drawing::DrawTile(sx, sy, what, 0, screen, false);
    /* trigger and die immediately */
    nexttick = now;
    return true;
  }

  int YOrder() override { return sy; }

  bool Think(unsigned int) override {
    return true;
  }
};

struct AnWait : public Animation {
  int wf;
  AnWait(int frames) : wf(frames) {}
  bool Think(unsigned int now) override {
    return true;
  }

  /* never draws */
  int YOrder() override { return 0; }

  bool Init(unsigned int now) override {
    nexttick = now + wf;
    return false;
  }
};

/* not really an animation -- plays a sound
   through the sound system. */
struct AnSound : public Animation {
  sound_t s;
  AnSound(sound_t s_) : s(s_) {}
  bool Think(unsigned int now) override {
    Sound::Play(s);
    return true;
  }

  /* never draws */
  int YOrder() override { return 0; }

  bool Init(unsigned int now) override {
    /* done immediately */
    nexttick = now;
    return false;
  }
};

/* XXX make this use AFrame instead of surface */
/* (easy now with draw method) */
/* something flying horizontally or vertically */
struct AnFlying : public Animation {
  /* XXX clean up members */
  int sx, sy;

  /* current position and direction */
  int px, py;
  int d;
  /* number of pixels left to travel */
  int pleft;

  /* pixels per frame */
  int speed;
  /* wait between frames */
  int wait;

  /* alias -- do not free */
  SDL_Surface *above;

  /* pass in screen pixel starting position, distance in
     pixels */
  AnFlying(SDL_Surface *what, int sx, int sy, dir dd, int sdist,
           int sp, int w);

  int YOrder() override { return py; }

  void Draw() override;
  bool Init(unsigned int now) override;
  bool Think(unsigned int now) override;

  void Erase(Dirt *dirty) override {
    int xx, yy, ww, hh;
    size(xx, yy, ww, hh);
    dirty->setdirty(px, py, ww, hh);
  }

  /* drawing is done through these, so that they can
     be overridden */
  virtual void blit(int x, int y) {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    // printf("blitting at %d/%d\n", x, y);
    SDL_BlitSurface(above, 0, screen, &r);
  }

  virtual void size(int &minx, int &miny, int &maxw, int &maxh) {
    minx = 0;
    miny = 0;
    maxw = above->w;
    maxh = above->h;
  }
};

/* special like AnPlaceTile */
struct AnDraw : public Animation {
  /* copy; don't free! */
  SDL_Surface *s;
  int x, y;
  AnDraw(SDL_Surface *ss, int sx, int sy) :
    s(ss), x(sx), y(sy) {}

  bool Init(unsigned int now) override {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    SDL_BlitSurface(s, 0, screen, &r);
    nexttick = now;
    return true;
  }

  int YOrder() override { return y; }

  bool Think(unsigned int) override {
    /* die */
    return true;
  }
};

/* this keeps drawing it and is never done */
struct AnFinale : public Animation {
  /* alias; don't free! */
  SDL_Surface *s;
  int x, y;
  AnFinale(SDL_Surface *ss, int sx, int sy) :
    s(ss), x(sx), y(sy) { finale = true; }

  bool Init(unsigned int now) override {
    nexttick = now + 100000;
    return false;
  }

  bool Think(unsigned int now) override {
    /* don't bother thinking */
    nexttick = now + 100000;
    return true;
  }

  void Draw() override {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    SDL_BlitSurface(s, 0, screen, &r);
  }

  void Erase(Dirt *dirty) override {
    dirty->setdirty(x, y, s->w, s->h);
  }

  int YOrder() override { return y; }
};


struct AnFlyingTile : public AnFlying {
  int ti;

  void blit(int x, int y) override {
    Drawing::DrawTile(x, y, ti, 0, screen, false);
  }

  void size(int &minx, int &miny, int &maxw, int &maxh) override {
    minx = 0;
    miny = 0;
    maxw = TILEW;
    maxh = TILEH;
  }

  AnFlyingTile(int ti_,
               int sx, int sy,
               dir dd, int sdist,
               int sp, int w);
};

struct AnLaser : public Animation {
  int sx, sy;
  dir d;
  int ntiles;
  int cyclesleft;

  bool isfinal;

  /* color of laser in high cycle */
  int rh, gh, bh;
  /* and outside of beam */
  int rho, gho, bho;

  /* and low cycle... */
  int rl, gl, bl;
  int rlo, glo, blo;

  bool Init(unsigned int now) override {
    /* finale = true; */
    nexttick = now;
    return false;
  }

  void Erase(Dirt *dirty) override {
    if (!isfinal) {
      int px, py, ww, hh;
      /* PERF don't need to draw the whole
         tile here, just laser width */
      switch (d) {
      default: return;
      case DIR_RIGHT:
        px = sx;
        py = sy;
        ww = ntiles * TILEW;
        hh = TILEH;
        break;
      case DIR_LEFT:
        px = sx - ntiles * TILEW;
        py = sy;
        ww = ntiles * TILEW;
        hh = TILEH;
        break;
      case DIR_DOWN:
        px = sx;
        py = sy;
        ww = TILEW;
        hh = ntiles * TILEH;
        break;
      case DIR_UP:
        px = sx;
        py = sy - ntiles * TILEH;
        ww = TILEW;
        hh = ntiles * TILEH;
        break;
      }
      dirty->setdirty(px, py, ww, hh);
    }
  }

  bool Think(unsigned int now) override;

  void Draw() override;

  /* XXX do we really want to use the starting position of the laser? */
  int YOrder() override { return sy; }

  AnLaser(int sx_, int sy_, dir d_, int nt, int cl,
          bool isfinal_,
          int rh_, int gh_, int bh_,
          int rho_, int gho_, int bho_,
          int rl_, int gl_, int bl_,
          int rlo_, int glo_, int blo_) :
    sx(sx_), sy(sy_), d(d_), ntiles(nt), cyclesleft(cl),
       isfinal(isfinal_),
       rh(rh_), gh(gh_), bh(bh_),
       rho(rho_), gho(gho_), bho(bho_),
       rl(rl_), gl(gl_), bl(bl_),
       rlo(rlo_), glo(glo_), blo(blo_)
  { }
};

/* for now require that first and second have no 'next' field
   also, init should not return true. */
struct AnCombo : public Animation {
  /* if either is zero, then act like the other pointer. */
  /* invt: at least one is non-null */
  Animation *first;
  Animation *second;

  void settick() {
    if (first) {
      if (second) {
        nexttick = std::min(first->nexttick, second->nexttick);
      } else nexttick = first->nexttick;
    } else nexttick = second->nexttick;
  }

  AnCombo(Animation *a, Animation *b) : first(a), second(b) { }

  bool Init(unsigned int now) override {
    /* XXX ignores return of Init */
    if (first)  first ->Init(now);
    if (second) second->Init(now);
    settick();
    return false;
  }

  void Erase(Dirt *dirty) override {
    if (first)  first ->Erase(dirty);
    if (second) second->Erase(dirty);
  }

  bool Think(unsigned int now) override {
    if (first && now >= first->nexttick) {
      if (first->Think(now)) {
        delete first;
        first = nullptr;
      }
    }

    if (second && now >= second->nexttick) {
      if (second->Think(now)) {
        delete second;
        second = nullptr;
      }
    }

    if (first || second) {
      settick();
      return false;
    } else return true;
  }

  /* use minimum, I guess... */
  int YOrder() override {
    if (!first) return second->YOrder();
    if (!second) return first->YOrder();
    return std::min(first->YOrder(), second->YOrder());
  }

  /* draw at current location. */
  void Draw() override {
    if (first) first->Draw();
    if (second) second->Draw();
  }
};

#endif
