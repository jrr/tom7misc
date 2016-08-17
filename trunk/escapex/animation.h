
#ifndef __ANIMATION_H
#define __ANIMATION_H

#include <SDL.h>
#include "escapex.h"
#include "level.h"
#include "draw.h"
#include "aevent.h"
#include "util.h"
#include "dirt.h"
#include "sound.h"

/* XXX should be at least 1 */
#define FLIPS_OVERSAMPLE 1

/* maximum y value of any animation */
#define YOSCALE 65536

/* an animation frame with timing and position information. */
struct aframe {
  SDL_Surface ** pic;
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
  virtual int yorder() = 0;

  /* This is called right before the animation
     is activated. It will always be called
     while the active animations are erased
     from the screen. 
     
     If this has changed the state of the screen,
     then return true, so that they dirty mirror
     can be updated. */
  virtual bool init(unsigned int now) {
    nexttick = now;
    return false;
  }

  /* the graphic from its current position,
     by invoking dirty->setdirty */
  virtual void erase(Dirt *dirty) { }
  
  /* think as a result of the timer passing
     the point set by nexttick. this can
     change the internal state, etc. return
     true if the animation has died. */
  virtual bool think(unsigned int now) = 0;

  /* draw at current location. */
  virtual void draw() { }

  /* next animation in the chain,
     if applicable. To replace this one
     when think() returns true. */
  Animation * next;

  Animation() : depth(0), next(0), finale(false) {}

  static int yorder_compare(Animation * a, Animation * b) {
    int r = ((a->depth * YOSCALE) + a->yorder()) -
            ((b->depth * YOSCALE) + b->yorder());

    /* if they are the same, use pointer comparison
       just to be more stable */
    if (r) return r;
    else return (a - b);
  }

  /* false if fail */
  static bool ainit();
  /* used for command-line apps (screenshot.exe) 
     that don't need animation, just frames for draw */
  static bool ainit_fast();

  /* graphics. These are generated by packpng
     from animation.pack */
# include "animation_defs.h"

  /* there will be DRAW_NSIZES copies of these */
  static  SDL_Surface ** pic_guy_up;
  static  SDL_Surface ** pic_guy_down;
  static  SDL_Surface ** pic_guy_left;
  static  SDL_Surface ** pic_guy_right;

  static  SDL_Surface ** pic_deadrobot;

  /* XXX these are currently all the same */
  static  SDL_Surface ** pic_dalek_up;
  static  SDL_Surface ** pic_dalek_down;
  static  SDL_Surface ** pic_dalek_left;
  static  SDL_Surface ** pic_dalek_right;

  /* XXX these are currently all the same */
  static  SDL_Surface ** pic_hugbot_up;
  static  SDL_Surface ** pic_hugbot_down;
  static  SDL_Surface ** pic_hugbot_left;
  static  SDL_Surface ** pic_hugbot_right;

  static SDL_Surface ** pic_dalek_asleep_up;
  static SDL_Surface ** pic_dalek_asleep_down;
  static SDL_Surface ** pic_dalek_asleep_left;
  static SDL_Surface ** pic_dalek_asleep_right;
  
  static SDL_Surface ** pic_hugbot_asleep_up;
  static SDL_Surface ** pic_hugbot_asleep_down;
  static SDL_Surface ** pic_hugbot_asleep_left;
  static SDL_Surface ** pic_hugbot_asleep_right;

  static SDL_Surface ** pic_bomb_still;
  static SDL_Surface *** pic_bomb_lit;
  
  /* if finale is true, then the animation should
     not be removed despite returning 'true' from
     think -- instead, it should die when every
     animation is a finale and thinks true. 

     finale true implies next is null. */
  bool finale;

  /* these are generated from tiles.png
     programatically */
  /* pvt? */
  static SDL_Surface ** pic_flips_out_data;
  static SDL_Surface ** pic_flips_in_data;
  static aframe ** frame_flips_out;
  static aframe ** frame_flips_in;

  /* push some animations for the event ae
     onto the list anims. assume anims is 0 */
  static void start(drawing & dr, 
		    PtrList<Animation> *& anims, 
		    PtrList<Animation> *& sprites,
		    aevent * ae);

  static void clearsprites(drawing & dr);
  static void clearent(drawing & dr, int ex, int ey, int olap);

  static void erase_anims(PtrList<Animation> * a, Dirt *d);

  /* Think all of the animations in 'as' that are ready.
     If an animation has finished, remove it from the
     list, perhaps starting the next one in the series.
     If anything changes the background, then remirror
     is set to true (otherwise it is untouched). 
     
     If 'done' is true, then finales are eligible to be
     removed, as long as everything running is a finale. */
  static void think_anims(PtrList<Animation> ** as, unsigned int now,
			  bool & remirror, bool done = false);

  /* Call draw method for every Animation in list. */
  static void draw_anims(PtrList<Animation> * a);

  /* Initialize every anim. Return true if any init method returns
     true. */
  static bool init_anims(PtrList<Animation> * a, unsigned int now);

  /* don't forget the tail */
  virtual ~Animation() { delete next; }

  private:

  static SDL_Surface *pitched_rect(int w, int h, int ph, 
				    SDL_Surface *src,
				    int oversample = FLIPS_OVERSAMPLE);

  static bool init_flips();

};


struct aninplace : public Animation {
  int xpos, ypos;
  int loopsleft;
  int cframe;
  aframe * frames;

  /* our dirty area */
  int bx, by, bw, bh;

  /* could have other versions, ie for a certain time rather
     than # of loops */

  /* f is a pointer to an array of frames, where the last element
     of the array has a pic pointer of 0. There must be at least
     one frame. */
  aninplace(int x, int y, int loops, aframe * f) 
    : xpos(x), ypos(y), loopsleft(loops-1), cframe(0), frames(f) {}

  virtual int yorder() { return ypos + frames[cframe].y; }

  virtual bool think(unsigned int);
  virtual void draw();
  virtual bool init(unsigned int now);
  virtual void erase(Dirt *d);

  virtual ~aninplace() { }
};

/* sort of special, because it is used as the
   transition from a static tile that is part
   of the background and an animated version of
   that tile */
struct anplacetile : public Animation {
  int what, sx, sy;
  anplacetile(int what_, int sx_, int sy_) 
    : what(what_), sx(sx_), sy(sy_) {}

  /* here the initializer draws it. */
  virtual bool init(unsigned int now) {
    drawing::drawtile(sx, sy, what, 0, screen, false);
    /* trigger and die immediately */
    nexttick = now;
    return true;
  }
  
  virtual int yorder() { return sy; }
  
  virtual bool think(unsigned int) {
    return true;
  }
  
};

struct anwait : public Animation {
  int wf;
  anwait(int frames) : wf(frames) {}
  virtual bool think(unsigned int now) {
    return true;
  }

  /* never draws */
  virtual int yorder() { return 0; }

  virtual bool init(unsigned int now) {
    nexttick = now + wf;
    return false;
  }

};

/* not really an animation -- plays a sound
   through the sound system. */
struct ansound : public Animation {
  sound_t s;
  ansound(sound_t s_) : s(s_) {}
  virtual bool think(unsigned int now) {
    Sound::play(s);
    return true;
  }

  /* never draws */
  virtual int yorder() { return 0; }

  virtual bool init(unsigned int now) {
    /* done immediately */
    nexttick = now;
    return false;
  }

};

/* XXX make this use aframe instead of surface */
/* (easy now with draw method) */
/* something flying horizontally or vertically */
struct anflying : public Animation {

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
  anflying(SDL_Surface *what, int sx, int sy, dir dd, int sdist,
	   int sp, int w);

  virtual int yorder() { return py; }

  virtual ~anflying() {
    /* not above! */
  }

  virtual void draw();
  virtual bool init(unsigned int now);
  virtual bool think(unsigned int now);

  virtual void erase(Dirt * dirty) {
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

  virtual void size(int & minx, int & miny, int & maxw, int & maxh) {
    minx = 0;
    miny = 0;
    maxw = above->w;
    maxh = above->h;
  }
};

/* special like anplacetile */
struct andraw : public Animation {
  /* copy; don't free! */
  SDL_Surface *s;
  int x, y;
  andraw(SDL_Surface *ss, int sx, int sy) :
    s(ss), x(sx), y(sy) {}

  virtual bool init(unsigned int now) {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    SDL_BlitSurface(s, 0, screen, &r);
    nexttick = now;
    return true;
  }

  virtual int yorder() { return y; }

  virtual bool think(unsigned int) {
    /* die */
    return true;
  }

};

/* this keeps drawing it and is never done */
struct anfinale : public Animation {
  /* copy; don't free! */
  SDL_Surface *s;
  int x, y;
  anfinale(SDL_Surface *ss, int sx, int sy) :
    s(ss), x(sx), y(sy) { finale = true; }

  virtual bool init(unsigned int now) {
    nexttick = now + 100000;
    return false;
  }

  virtual bool think(unsigned int now) {
    /* don't bother thinking */
    nexttick = now + 100000;
    return true;
  }

  virtual void draw() {
    SDL_Rect r;
    r.x = x;
    r.y = y;
    SDL_BlitSurface(s, 0, screen, &r);
  }

  virtual void erase(Dirt * dirty) {
    dirty->setdirty(x, y, s->w, s->h);
  }

  virtual int yorder() { return y; }

};


struct anflyingtile : public anflying {
  int ti;

  virtual void blit(int x, int y) {
    drawing::drawtile(x, y, ti, 0, screen, false);
  }

  virtual void size(int & minx, int & miny, int & maxw, int & maxh) {
    minx = 0;
    miny = 0;
    maxw = TILEW;
    maxh = TILEH;
  }

  anflyingtile(int ti_,
	       int sx, int sy, 
	       dir dd, int sdist,
	       int sp, int w);

};

struct anlaser : public Animation {

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

  virtual bool init(unsigned int now) {
    /* finale = true; */
    nexttick = now;
    return false;
  }

  virtual void erase(Dirt * dirty) { 
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
  
  virtual bool think(unsigned int now);

  virtual void draw();

  /* XXX do we really want to use the starting position of the laser? */
  virtual int yorder() { return sy; }

  anlaser(int sx_, int sy_, dir d_, int nt, int cl,
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
struct ancombo : public Animation {

  /* if either is zero, then act like the other pointer. */
  /* invt: at least one is non-null */
  Animation * first;
  Animation * second;

  void settick() {
    if (first) {
      if (second) {
	nexttick = util::minimum(first->nexttick, second->nexttick);
      } else nexttick = first->nexttick;
    } else nexttick = second->nexttick;
  }

  ancombo(Animation * a, Animation * b) : first(a), second(b) { }

  virtual bool init(unsigned int now) {
    /* XXX ignores return of init */
    if (first)  first ->init(now);
    if (second) second->init(now);
    settick();
    return false;
  }

  virtual void erase(Dirt * dirty) {
    if (first)  first ->erase(dirty);
    if (second) second->erase(dirty);
  }
  
  virtual bool think(unsigned int now) {
    if (first && now >= first->nexttick) {
      if (first->think(now)) {
	delete first;
	first = 0;
      }
    }

    if (second && now >= second->nexttick) {
      if (second->think(now)) {
	delete second;
	second = 0;
      }
    }

    if (first || second) {
      settick();
      return false;
    } else return true;
  }

  /* use minimum, I guess... */
  virtual int yorder() {
    if (!first) return second->yorder();
    if (!second) return first->yorder();
    return util::minimum(first->yorder(), second->yorder());
  }

  /* draw at current location. */
  virtual void draw() { 
    if (first) first->draw();
    if (second) second->draw();
  }

};

#endif
