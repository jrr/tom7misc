
#include "SDL.h"
#include "sdlutil.h"
#include <string>
#include <vector>
#include <utility>

#include "../stb_image.h"
#include "../stb_image_write.h"
#include "../image.h"

using namespace std;

/* by default, use display format. but
   when in console mode (for instance)
   there is no display!! */
#ifndef USE_DISPLAY_FORMAT
# define USE_DISPLAY_FORMAT 1
#endif

/* 
   XXX uses of these are probably always wrong. In addition
   to the host byte order (though you're probably compiling
   for x86 or x86-64, so this is probably not an issue), different
   OSes use different channel order. Stamp this out.
   SDL_GetRGBA and SDL_MapRGBA are the correct way to do this,
   though they may not be as fast.

   (Note that these are used in the call to SDL_CreateRGBSurface;
   why?)

   PERF: Instead inline SDL_GetRGBA and SDL_MapRGBA here. They
   are defined in sdl/src/video/SDL_pixels.c, and use shifts
   and masks from the surfac; they won't be as fast as constexpr
   masks but should inline decently fast without the function
   call overhead! (And then benchmark!)

 */
#if 0 /* SDL_BYTEORDER == SDL_BIG_ENDIAN */
  static constexpr Uint32 rmask = 0xff000000;
  static constexpr Uint32 gmask = 0x00ff0000;
  static constexpr Uint32 bmask = 0x0000ff00;
  static constexpr Uint32 amask = 0x000000ff;
  static constexpr Uint32 rshift = 24;
  static constexpr Uint32 gshift = 16;
  static constexpr Uint32 bshift = 8;
  static constexpr Uint32 ashift = 0;
#else
  static constexpr Uint32 rmask = 0x000000ff;
  static constexpr Uint32 gmask = 0x0000ff00;
  static constexpr Uint32 bmask = 0x00ff0000;
  static constexpr Uint32 amask = 0xff000000;
  static constexpr Uint32 ashift = 24;
  static constexpr Uint32 bshift = 16;
  static constexpr Uint32 gshift = 8;
  static constexpr Uint32 rshift = 0;
#endif

// Local copy of util's "line" code, to avoid dependency on that and
// facilitate inlining in this code. (That old weird interface also
// does a pointless heap allocation. Inlined as a regular object here
// was almost twice as fast in a line-heavy benchmark on 13 Feb 2016!)
namespace {
struct Line {
  int x0, y0, x1, y1;
  int dx, dy;
  int stepx, stepy;
  int frac;

  Line(int x0, int y0, int x1, int y1) :
    x0(x0), y0(y0), x1(x1), y1(y1) {

    dy = y1 - y0;
    dx = x1 - x0;

    if (dy < 0) {
      dy = -dy;
      stepy = -1;
    } else {
      stepy = 1;
    }

    if (dx < 0) {
      dx = -dx;
      stepx = -1;
    } else {
      stepx = 1;
    }

    dy <<= 1;
    dx <<= 1;

    if (dx > dy) {
      frac = dy - (dx >> 1);
    } else {
      frac = dx - (dy >> 1);
    }
  }

  bool Next(int &cx, int &cy) {
    if (dx > dy) {
      if (x0 == x1) return false;
      else {
        if (frac >= 0) {
          y0 += stepy;
          frac -= dx;
        }
        x0 += stepx;
        frac += dy;
        cx = x0;
        cy = y0;
        return true;
      }
    } else {
      if (y0 == y1) return false;
      else {
        if (frac >= 0) {
          x0 += stepx;
          frac -= dy;
        }
        y0 += stepy;
        frac += dx;
        cx = x0;
        cy = y0;
        return true;
      }
    }
  }
};
}  // namespace

SDL_Surface *sdlutil::resize_canvas(SDL_Surface *s,
                                    int w, int h, Uint32 color) {
  SDL_Surface *m = makesurface(w, h);
  if (!m) return nullptr;

  for (int y = 0; y < h; y++)
    for (int x = 0; x < w; x++) {
      Uint32 c = color;
      if (y < s->h && x < s->w)
        c = getpixel(s, x, y);
      setpixel(m, x, y, c);
    }

  return m;
}

// static 
sdlutil::ByteOrder sdlutil::GetByteOrder(SDL_Surface *surf) {
  // PERF: Actually the R channel determines it completely.
  // But maybe nice to have the additional sanity check on A
  // until we know this works.
  switch (surf->format->Ashift) {
  case 0:
    // Could be RGBA or BGRA
    switch (surf->format->Rshift) {
    case 24: return ByteOrder::RGBA;
    case 8: return ByteOrder::BGRA;
    }
    break;
  case 24:
    // Could be ARGB or ABGR
    switch (surf->format->Rshift) {
    case 16: return ByteOrder::ARGB;
    case 0: return ByteOrder::ABGR;
    }
    break;
  }
  fprintf(stderr,
          "GetByteOrder: Surface not 32BPP or something else is wrong. %d %d",
          surf->format->Ashift, surf->format->Rshift);
  abort();
}

static void CopyRGBA(const vector<Uint8> &rgba, SDL_Surface *surface) {
  using ByteOrder = sdlutil::ByteOrder;
  // Uint8 *p = (Uint8 *)surface->pixels;
  Uint32 *p = (Uint32 *)surface->pixels;
  int width = surface->w;
  int height = surface->h;
  switch (sdlutil::GetByteOrder(surface)) {
  case ByteOrder::ARGB:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const int idx = pidx * 4;
        const Uint32 r = rgba[idx + 0], g = rgba[idx + 1],
          b = rgba[idx + 2], a = rgba[idx + 3];
        p[pidx] = (a << 24) | (r << 16) | (g << 8) | b;
      }
    }
    break;
  case ByteOrder::RGBA:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const int idx = pidx * 4;
        const Uint32 r = rgba[idx + 0], g = rgba[idx + 1],
          b = rgba[idx + 2], a = rgba[idx + 3];
        p[pidx] = (r << 24) | (g << 16) | (b << 8) | a;
      }
    }
    break;
  case ByteOrder::ABGR:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const int idx = pidx * 4;
        const Uint32 r = rgba[idx + 0], g = rgba[idx + 1],
          b = rgba[idx + 2], a = rgba[idx + 3];
        p[pidx] = (a << 24) | (b << 16) | (g << 8) | r;
      }
    }
    break;
  case ByteOrder::BGRA:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const int idx = pidx * 4;
        const Uint32 r = rgba[idx + 0], g = rgba[idx + 1],
          b = rgba[idx + 2], a = rgba[idx + 3];
        p[pidx] = (b << 24) | (g << 16) | (r << 8) | a;
      }
    }
    break;
  }
}

SDL_Surface *sdlutil::FromRGBA(const ImageRGBA &rgba) {
  SDL_Surface *surf = makesurface(rgba.Width(), rgba.Height(), true);
  if (!surf) {
    return nullptr;
  }
  using ByteOrder = sdlutil::ByteOrder;
  // Uint8 *p = (Uint8 *)surface->pixels;
  Uint32 *p = (Uint32 *)surf->pixels;
  int width = surf->w;
  int height = surf->h;
  switch (sdlutil::GetByteOrder(surf)) {
  case ByteOrder::ARGB:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const auto [r, g, b, a] = rgba.GetPixel(x, y);
        p[pidx] = (a << 24) | (r << 16) | (g << 8) | b;
      }
    }
    break;
  case ByteOrder::RGBA:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const auto [r, g, b, a] = rgba.GetPixel(x, y);        
        p[pidx] = (r << 24) | (g << 16) | (b << 8) | a;
      }
    }
    break;
  case ByteOrder::ABGR:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const auto [r, g, b, a] = rgba.GetPixel(x, y);        
        p[pidx] = (a << 24) | (b << 16) | (g << 8) | r;
      }
    }
    break;
  case ByteOrder::BGRA:
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const int pidx = (y * width + x);
        const auto [r, g, b, a] = rgba.GetPixel(x, y);
        p[pidx] = (b << 24) | (g << 16) | (r << 8) | a;
      }
    }
    break;
  }
  return surf;
}

// Note: Avoid "LoadImage" since windows.h may #define the
// symbol to LoadImageA etc. :(
SDL_Surface *sdlutil::LoadImageFile(const string &filename) {
  int width, height, bpp;
  Uint8 *stb_rgba = stbi_load(filename.c_str(),
                              &width, &height, &bpp, 4);
  if (!stb_rgba) return nullptr;
  
  vector<Uint8> rgba;
  rgba.reserve(width * height * 4);
  for (int i = 0; i < width * height * 4; i++) {
    rgba.push_back(stb_rgba[i]);
  }
  stbi_image_free(stb_rgba);

  SDL_Surface *surf = makesurface(width, height, true);
  if (!surf) {
    return nullptr;
  }
  CopyRGBA(rgba, surf);

  return surf;
}

bool sdlutil::SavePNG(const string &filename, SDL_Surface *surf) {
  // This could be implemented for other formats, of course.
  if (surf->format->BytesPerPixel != 4) return false;
  vector<Uint8> rgba;
  rgba.reserve(surf->w * surf->h * 4);
  for (int i = 0; i < surf->w * surf->h; i++) {
    Uint8 r, g, b, a;
    SDL_GetRGBA(((Uint32*)surf->pixels)[i], surf->format, &r, &g, &b, &a);
    rgba.push_back(r);
    rgba.push_back(g);
    rgba.push_back(b);
    rgba.push_back(a);
  }
  return !!stbi_write_png(filename.c_str(),
                          surf->w, surf->h, 4, rgba.data(), 4 * surf->w);
}

SDL_Surface *sdlutil::duplicate(SDL_Surface *surf) {
  return SDL_ConvertSurface(surf, surf->format, surf->flags);
}

void sdlutil::eatevents(int ticks, Uint32 mask) {
  int now = SDL_GetTicks();
  const int destiny = now + ticks;

  SDL_Event e;
  while (now < destiny) {
    SDL_PumpEvents();
    while (1 == SDL_PeepEvents(&e, 1, SDL_GETEVENT, mask)) { }

    now = SDL_GetTicks();
  }
}

/* recursive; nmips can't reasonably be very large */
bool sdlutil::make_mipmaps(SDL_Surface **s, int nmips) {
  if (nmips <= 1) return true;

  s[1] = shrink50(s[0]);
  if (!s[1]) return false;
  return make_mipmaps(&s[1], nmips - 1);
}

void sdlutil::clearsurface(SDL_Surface *s, Uint32 color) {
  SDL_FillRect(s, 0, color);
}

void sdlutil::ClearSurface(SDL_Surface *s,
                           Uint8 r, Uint8 g, Uint8 b, Uint8 a) {
  Uint32 color = SDL_MapRGBA(s->format, r, g, b, a);
  SDL_FillRect(s, 0, color);
}

Uint32 sdlutil::mix2(Uint32 ia, Uint32 ib) {
  Uint32 ar = (ia >> rshift) & 0xFF;
  Uint32 br = (ib >> rshift) & 0xFF;

  Uint32 ag = (ia >> gshift) & 0xFF;
  Uint32 bg = (ib >> gshift) & 0xFF;

  Uint32 ab = (ia >> bshift) & 0xFF;
  Uint32 bb = (ib >> bshift) & 0xFF;

  Uint32 aa = (ia >> ashift) & 0xFF;
  Uint32 ba = (ib >> ashift) & 0xFF;

  /* if these are all fractions 0..1,
     color output is
       color1 * alpha1 + color2 * alpha2
       ---------------------------------
              alpha1 + alpha2               */

  Uint32 r = 0;
  Uint32 g = 0;
  Uint32 b = 0;

  if ((aa + ba) > 0) {
    /* really want 
        (ar / 255) * (aa / 255) +
        (br / 255) * (ba / 255)
        -------------------------
        (aa / 255) + (ba / 255)

        to get r as a fraction. we then
        multiply by 255 to get the output
        as a byte.
        
        but this is:

        ar * (aa / 255) +
        br * (ba / 255)
        -----------------------
        (aa / 255) + (ba / 255)

        which can be simplified to
        
        ar * aa + br * ba
        -----------------
             aa + ba

        .. and doing the division last keeps
        us from quantization errors.
    */
    r = (ar * aa + br * ba) / (aa + ba);
    g = (ag * aa + bg * ba) / (aa + ba);
    b = (ab * aa + bb * ba) / (aa + ba);
  }

  /* alpha output is just the average */
  Uint32 a = (aa + ba) >> 1;

  return (r << rshift) | (g << gshift) | (b << bshift) | (a << ashift);
}

Uint32 sdlutil::mix4(Uint32 a, Uint32 b, Uint32 c, Uint32 d) {
  /* XXX PERF 

     would probably get better results (less quant error) and
     better performance to write this directly, using the calculation
     in mix2. */
  return mix2(mix2(a, b), mix2(c, d));
}

Uint32 sdlutil::Mix4(const SDL_Surface *surf,
                     Uint32 ai, Uint32 bi, Uint32 ci, Uint32 di) {

  Uint8 ar8, ag8, ab8, aa8;
  Uint8 br8, bg8, bb8, ba8;
  Uint8 cr8, cg8, cb8, ca8;
  Uint8 dr8, dg8, db8, da8;

  SDL_GetRGBA(ai, surf->format, &ar8, &ag8, &ab8, &aa8);
  SDL_GetRGBA(bi, surf->format, &br8, &bg8, &bb8, &ba8);
  SDL_GetRGBA(ci, surf->format, &cr8, &cg8, &cb8, &ca8);
  SDL_GetRGBA(di, surf->format, &dr8, &dg8, &db8, &da8);

  const Uint32 ar = ar8, ag = ag8, ab = ab8, aa = aa8;
  const Uint32 br = br8, bg = bg8, bb = bb8, ba = ba8;
  const Uint32 cr = cr8, cg = cg8, cb = cb8, ca = ca8;
  const Uint32 dr = dr8, dg = dg8, db = db8, da = da8;
  
  // This is the same as idea as Mix2; see that for the math.
  
  Uint32 r = 0;
  Uint32 g = 0;
  Uint32 b = 0;

  const Uint32 denom = aa + ba + ca + da;
  if (denom > 0) {
    r = (ar * aa + br * ba + cr * ca + dr * da) / denom;
    g = (ag * aa + bg * ba + cg * ca + dg * da) / denom;
    b = (ab * aa + bb * ba + cb * ca + db * da) / denom;
  }

  /* alpha output is just the average */
  const Uint32 a = denom >> 2;

  return SDL_MapRGBA(surf->format, r, g, b, a);
}

Uint32 sdlutil::Mix2(const SDL_Surface *surf,
                     Uint32 ia, Uint32 ib) {
  Uint8 ar8, ag8, ab8, aa8;
  Uint8 br8, bg8, bb8, ba8;
  SDL_GetRGBA(ia, surf->format, &ar8, &ag8, &ab8, &aa8);
  SDL_GetRGBA(ib, surf->format, &br8, &bg8, &bb8, &ba8);

  Uint32 ar = ar8, ag = ag8, ab = ab8, aa = aa8;
  Uint32 br = br8, bg = bg8, bb = bb8, ba = ba8;
  
  /* if these are all fractions 0..1,
     color output is
       color1 * alpha1 + color2 * alpha2
       ---------------------------------
              alpha1 + alpha2               */

  Uint32 r = 0;
  Uint32 g = 0;
  Uint32 b = 0;

  if ((aa + ba) > 0) {
    /* really want 
        (ar / 255) * (aa / 255) +
        (br / 255) * (ba / 255)
        -------------------------
        (aa / 255) + (ba / 255)

        to get r as a fraction. we then
        multiply by 255 to get the output
        as a byte.
        
        but this is:

        ar * (aa / 255) +
        br * (ba / 255)
        -----------------------
        (aa / 255) + (ba / 255)

        which can be simplified to
        
        ar * aa + br * ba
        -----------------
             aa + ba

        .. and doing the division last keeps
        us from quantization errors.
    */
    r = (ar * aa + br * ba) / (aa + ba);
    g = (ag * aa + bg * ba) / (aa + ba);
    b = (ab * aa + bb * ba) / (aa + ba);
  }

  /* alpha output is just the average */
  const Uint32 a = (aa + ba) >> 1;

  return SDL_MapRGBA(surf->format, r, g, b, a);
}


Uint32 sdlutil::mixfrac(Uint32 a, Uint32 b, float f) {
  Uint32 factor  = (Uint32) (f * 0x100);
  Uint32 ofactor = 0x100 - factor;

  Uint32 o24 = (((a >> 24) & 0xFF) * factor +
                ((b >> 24) & 0xFF) * ofactor) >> 8;

  Uint32 o16 = (((a >> 16) & 0xFF) * factor +
                ((b >> 16) & 0xFF) * ofactor) >> 8;

  Uint32 o8 = (((a >> 8) & 0xFF) * factor +
                ((b >> 8) & 0xFF) * ofactor) >> 8;


  Uint32 o = ((a & 0xFF) * factor +
              (b & 0xFF) * ofactor) >> 8;

  return (o24 << 24) | (o16 << 16) | (o8 << 8) | o;
}


Uint32 sdlutil::hsv(SDL_Surface *sur, float h, float s, float v, float a) {
  int r, g, b;
  if (s == 0.0f) {
    r = (int)(v * 255);
    g = (int)(v * 255);
    b = (int)(v * 255);
  } else {
    float hue = h * 6.0f;
    int fh = (int)hue;
    float var_1 = v * (1 - s);
    float var_2 = v * (1 - s * (hue - fh));
    float var_3 = v * (1 - s * (1 - (hue - fh)));

    float red, green, blue;

    switch ((int)hue) {
    case 0:  red = v     ; green = var_3 ; blue = var_1; break;
    case 1:  red = var_2 ; green = v     ; blue = var_1; break;
    case 2:  red = var_1 ; green = v     ; blue = var_3; break;
    case 3:  red = var_1 ; green = var_2 ; blue = v    ; break;
    case 4:  red = var_3 ; green = var_1 ; blue = v    ; break;
    default: red = v     ; green = var_1 ; blue = var_2; break;
    }

    r = (int)(red * 255);
    g = (int)(green * 255);
    b = (int)(blue * 255);
  }

  return SDL_MapRGBA(sur->format, r, g, b, (int)(a * 255));
}

SDL_Surface *sdlutil::alphadim(SDL_Surface *src) {
  /* must be 32 bpp */
  if (src->format->BytesPerPixel != 4) return nullptr;

  int ww = src->w, hh = src->h;
  
  SDL_Surface *ret = makesurface(ww, hh);

  if (!ret) return nullptr;

  slock(ret);
  slock(src);

  for (int y = 0; y < hh; y++) {
    for (int x = 0; x < ww; x++) {
      Uint32 color = *((Uint32 *)src->pixels + y * src->pitch / 4 + x);
      
      /* divide alpha channel by 2 */
      Uint8 r, g, b, a;
      SDL_GetRGBA(color, src->format, &r, &g, &b, &a);
      a >>= 1;

      color = SDL_MapRGBA(src->format, r, g, b, a);
  
      *((Uint32 *)ret->pixels + y * ret->pitch / 4 + x) = color;
    }
  }
    
  sulock(src);
  sulock(ret);

  return ret;
}

SDL_Surface *sdlutil::shrink50(SDL_Surface *src) {
  /* must be 32 bpp */
  if (src->format->BytesPerPixel != 4) return nullptr;

  int ww = src->w, hh = src->h;
  
  /* we need there to be at least two pixels for each destination
     pixel, so if the src dimensions are not even, then we ignore
     that last pixel. */
  if (ww & 1) ww -= 1;
  if (hh & 1) hh -= 1;

  if (ww <= 0) return nullptr;
  if (hh <= 0) return nullptr;

  SDL_Surface *ret = makesurface(ww / 2, hh / 2);

  if (!ret) return nullptr;

  slock(ret);
  slock(src);

  for (int y = 0; y < ret->h; y++) {
    for (int x = 0; x < ret->w; x++) {
      /* get and mix four src pixels for each dst pixel */
      Uint32 c1 = *((Uint32 *)src->pixels + (y*2)*src->pitch/4 + (x*2));
      Uint32 c2 = *((Uint32 *)src->pixels + (1+y*2)*src->pitch/4 + (1+x*2));
      Uint32 c3 = *((Uint32 *)src->pixels + (1+y*2)*src->pitch/4 + (x*2));
      Uint32 c4 = *((Uint32 *)src->pixels + (y*2)*src->pitch/4 + (1+x*2));

      Uint32 color = Mix4(src, c1, c2, c3, c4);
  
      *((Uint32 *)ret->pixels + y*ret->pitch/4 + x) = color;
    }
  }

  sulock(src);
  sulock(ret);

  return ret;
}

SDL_Surface *sdlutil::grow2x(SDL_Surface *src) {
  /* must be 32 bpp */
  if (src->format->BytesPerPixel != 4) return nullptr;

  int ww = src->w, hh = src->h;
  

  SDL_Surface *ret = makesurface(ww << 1, hh << 1);
  if (!ret) return nullptr;

  slock(ret);
  slock(src);

  Uint8 *p = (Uint8*)src->pixels;
  Uint8 *pdest = (Uint8*)ret->pixels;

  int ww2 = ww << 1;
  for (int y = 0; y < hh; y++) {
    for (int x = 0; x < ww; x++) {
      Uint32 rgba = *(Uint32*)(p + 4 * (y * ww + x));
      
      // Write four pixels.
      int y2 = y << 1;
      int x2 = x << 1;
      *(Uint32*)(pdest + 4 * (y2 * ww2 + x2)) = rgba;
      *(Uint32*)(pdest + 4 * (y2 * ww2 + x2 + 1)) = rgba;
      *(Uint32*)(pdest + 4 * ((y2 + 1) * ww2 + x2)) = rgba;
      *(Uint32*)(pdest + 4 * ((y2 + 1) * ww2 + x2 + 1)) = rgba;
    }
  }

  sulock(src);
  sulock(ret);

  return ret;
}

SDL_Surface *sdlutil::GrowX(SDL_Surface *src, int px) {
  /* must be 32 bpp */
  if (src->format->BytesPerPixel != 4) {
    return nullptr;
  }

  int ww = src->w, hh = src->h;
  SDL_Surface *ret = makesurface(ww * px, hh * px);
  if (!ret) {
    return nullptr;
  }
  
  slock(ret);
  slock(src);

  Uint8 *p = (Uint8*)src->pixels;
  Uint8 *pdest = (Uint8*)ret->pixels;

  int ww2 = ww * px;
  for (int y = 0; y < hh; y++) {
    for (int x = 0; x < ww; x++) {
      const Uint32 rgba = *(Uint32*)(p + 4 * (y * ww + x));
      
      // Write px * px pixels.
      for (int yu = 0; yu < px; yu++) {
        const int y2 = y * px + yu;
        for (int xu = 0; xu < px; xu++) {
          const int x2 = x * px + xu;
          *(Uint32*)(pdest + 4 * (y2 * ww2 + x2)) = rgba;
        }
      }
    }
  }
  
  sulock(src);
  sulock(ret);

  return ret;
}


/* try to make a hardware surface, and, failing that,
   make a software surface */
SDL_Surface *sdlutil::makesurface(int w, int h, bool alpha) {
  /* PERF need to investigate relative performance 
     of sw/hw surfaces */
  SDL_Surface *ss = 0;
#if 0
    SDL_CreateRGBSurface(SDL_HWSURFACE |
                         (alpha?SDL_SRCALPHA:0),
                         w, h, 32, 
                         rmask, gmask, bmask,
                         amask);
#endif

  if (!ss) ss = SDL_CreateRGBSurface(SDL_SWSURFACE |
                                     (alpha?SDL_SRCALPHA:0),
                                     w, h, 32,
                                     rmask, gmask, bmask,
                                     amask);

  if (ss && !alpha) SDL_SetAlpha(ss, 0, 255);

  /* then convert to the display format. */
# if USE_DISPLAY_FORMAT
  if (ss) {
    SDL_Surface *rr;
    if (alpha) rr = SDL_DisplayFormatAlpha(ss);
    else rr = SDL_DisplayFormat(ss);
    SDL_FreeSurface(ss);
    return rr;
  } else return nullptr;
# else
  return ss;
# endif
}

SDL_Surface *sdlutil::makealpharect(int w, int h, int r, int g, int b, int a) {
  SDL_Surface *ret = makesurface(w, h);

  if (!ret) return nullptr;

  Uint32 color = SDL_MapRGBA(ret->format, r, g, b, a);

  SDL_FillRect(ret, 0, color);
  
  return ret;
}

SDL_Surface *sdlutil::makealpharectgrad(int w, int h,
                                        int r1, int b1, int g1, int a1,
                                        int r2, int b2, int g2, int a2,
                                        float bias) {
  SDL_Surface *ret = makesurface(w, h);

  if (!ret) return nullptr;

  /* draws each line as a separate rectangle. */
  for (int i = 0; i < h; i++) {
    /* no bias yet */
    float frac = 1.0f - ((float)i / (float)h);
    int r = (int) ((r1 * frac) + (r2 * (1.0 - frac)));
    int g = (int) ((g1 * frac) + (g2 * (1.0 - frac)));
    int b = (int) ((b1 * frac) + (b2 * (1.0 - frac)));
    int a = (int) ((a1 * frac) + (a2 * (1.0 - frac)));
    Uint32 color = SDL_MapRGBA(ret->format, r, g, b, a);
    SDL_Rect rect;
    rect.x = 0;
    rect.y = i;
    rect.w = w;
    rect.h = 1;
    SDL_FillRect(ret, &rect, color);
  }    

  return ret;
}

void sdlutil::fillrect(SDL_Surface *s, Uint32 color,
                       int x, int y, int w, int h) {
  SDL_Rect dst;
  dst.x = x;
  dst.y = y;
  dst.w = w;
  dst.h = h;
  SDL_FillRect(s, &dst, color);
}

void sdlutil::FillRectRGB(SDL_Surface *s,
                          int x, int y, int w, int h,
                          Uint8 r, Uint8 g, Uint8 b) {
  SDL_Rect dst;
  dst.x = x;
  dst.y = y;
  dst.w = w;
  dst.h = h;
  SDL_FillRect(s, &dst, SDL_MapRGB(s->format, r, g, b));
}

void sdlutil::blitall(SDL_Surface *src, SDL_Surface *dst, int x, int y) {
  SDL_Rect rec;
  rec.x = x;
  rec.y = y;
  SDL_BlitSurface(src, 0, dst, &rec);
}

void sdlutil::outline(SDL_Surface *s, int n, int r, int g, int b, int a) {
  Uint32 color = SDL_MapRGBA(s->format, r, g, b, a);

  SDL_Rect dst;
  dst.x = 0;
  dst.y = 0;
  dst.h = n;
  dst.w = s->w;
  SDL_FillRect(s, &dst, color);
  dst.w = n;
  dst.h = s->h;
  SDL_FillRect(s, &dst, color);
  dst.x = s->w - n;
  SDL_FillRect(s, &dst, color);
  dst.y = s->h - n;
  dst.x = 0;
  dst.w = s->w;
  dst.h = n;
  SDL_FillRect(s, &dst, color);
}

void sdlutil::DrawCircle32(SDL_Surface *surf,
                           int x0, int y0, int radius, Uint32 color) {
  // Only 32-bit!
  if (surf->format->BytesPerPixel != 4) return;

  Uint32 *bufp = (Uint32 *)surf->pixels;
  const int stride = surf->pitch >> 2;
  const int width = surf->w, height = surf->h;
  auto SetPixel = [color, bufp, stride, width, height](int x, int y) {
      if (x < 0 || y < 0 || x >= width || y >= height)
        return;
      bufp[y * stride + x] = color;
    };

  SetPixel(x0, y0 + radius);
  SetPixel(x0, y0 - radius);
  SetPixel(x0 + radius, y0);
  SetPixel(x0 - radius, y0);

  int f = 1 - radius;
  int ddF_x = 1;
  int ddF_y = -2 * radius;
  int x = 0;
  int y = radius;

  while (x < y) {
    // fun loop (x, y, f, ddF_x, ddF_y) =
    if (f >= 0) {
      y--;
      f += 2 + ddF_y;
      ddF_y += 2;
    }

    x++;
    ddF_x += 2;
    f += ddF_x;

    SetPixel(x0 + x, y0 + y);
    SetPixel(x0 - x, y0 + y);
    SetPixel(x0 + x, y0 - y);
    SetPixel(x0 - x, y0 - y);
    SetPixel(x0 + y, y0 + x);
    SetPixel(x0 - y, y0 + x);
    SetPixel(x0 + y, y0 - x);
    SetPixel(x0 - y, y0 - x);
  }
}

SDL_Surface *sdlutil::makescreen(int w, int h) {
  /* Can't use HWSURFACE here, because not handling this SDL_BlitSurface
     case mentioned in the documentation:

     "If either of the surfaces were in video memory, and the blit returns -2, 
     the video memory was lost, so it should be reloaded with artwork 
     and re-blitted." 

     Plus, on Windows, the only time you get HWSURFACE is with FULLSCREEN.

     -- Adam
  */

  /* SDL_ANYFORMAT      
     "Normally, if a video surface of the requested bits-per-pixel (bpp) 
     is not available, SDL will emulate one with a shadow surface. 
     Passing SDL_ANYFORMAT prevents this and causes SDL to use the 
     video surface, regardless of its pixel depth."

     Probably should not pass this.

     -- Adam
  */


  /* SDL_DOUBLEBUF only valid with SDL_HWSURFACE! */
  SDL_Surface *ret = SDL_SetVideoMode(w, h, 32,
                                      SDL_SWSURFACE |
                                      SDL_RESIZABLE);
  return ret;
}

/* XXX apparently you are not supposed
   to lock the surface for blits, only
   direct pixel access. should check
   this out if mysterious crashes on some
   systems ... */

void sdlutil::slock(SDL_Surface *screen) {
  if (SDL_MUSTLOCK(screen)) {
    SDL_LockSurface(screen);
  }
}

void sdlutil::sulock(SDL_Surface *screen) {
  if (SDL_MUSTLOCK(screen)) {
    SDL_UnlockSurface(screen);
  }
}


// XXX There may be strict aliasing problems with this and the
// next function.
/* lock before calling */
/* getpixel function only:
   Copyright (c) 2004 Bill Kendrick, Tom Murphy
   from the GPL "Tux Paint" */
Uint32 sdlutil::getpixel(SDL_Surface *surface, int x, int y) {
  Uint32 pixel = 0;

  /* Check that x/y values are within the bounds of this surface... */
  if (x >= 0 && y >= 0 && x < surface -> w && y < surface -> h) {

    /* Determine bytes-per-pixel for the surface in question: */

    int bpp = surface->format->BytesPerPixel;

    /* Set a pointer to the exact location in memory of the pixel
       in question: */

    Uint8 *p =
      (Uint8 *) (((Uint8 *)surface->pixels) +  /* Start at top of RAM */
                 (y * surface->pitch) +  /* Go down Y lines */
                 (x * bpp));             /* Go in X pixels */

    /* Return the correctly-sized piece of data containing the
       pixel's value (an 8-bit palette value, or a 16-, 24- or 32-bit
       RGB value) */

    if (bpp == 1)         /* 8-bit display */
      pixel = *p;
    else if (bpp == 2)    /* 16-bit display */
      pixel = *(Uint16 *)p;
    else if (bpp == 3) {    /* 24-bit display */
      /* Depending on the byte-order, it could be stored RGB or BGR! */

      if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
        pixel = p[0] << 16 | p[1] << 8 | p[2];
      else
        pixel = p[0] | p[1] << 8 | p[2] << 16;
    } else if (bpp == 4) {    /* 32-bit display */
      pixel = *(Uint32 *)p;
    }
  }
  return pixel;
}

/* based on getpixel above */
void sdlutil::setpixel(SDL_Surface *surface, int x, int y, Uint32 px) {
  /* Check that x/y values are within the bounds of this surface... */

  if (x >= 0 && y >= 0 && x < surface -> w && y < surface -> h) {
    /* Determine bytes-per-pixel for the surface in question: */

    int bpp = surface->format->BytesPerPixel;

    /* Set a pointer to the exact location in memory of the pixel
       in question: */

    Uint8 *p =
      (Uint8 *) (((Uint8 *)surface->pixels) +  /* Start at top of RAM */
                 (y * surface->pitch) +  /* Go down Y lines */
                 (x * bpp));             /* Go in X pixels */

    /* Return the correctly-sized piece of data containing the
       pixel's value (an 8-bit palette value, or a 16-, 24- or 32-bit
       RGB value) */

    if (bpp == 1)         /* 8-bit display */
      *p = px;
    else if (bpp == 2)    /* 16-bit display */
      *(Uint16 *)p = px;
    else if (bpp == 3) {    /* 24-bit display */
      /* Depending on the byte-order, it could be stored RGB or BGR! */

      /* XX never tested */
      if (SDL_BYTEORDER == SDL_BIG_ENDIAN) {
        p += 2;
        for (int i = 0; i < 3; i++) {
          *p = px & 255;
          px >>= 8;
          p--;
        }
      }
      else {
        for (int i = 0; i < 3; i++) {
          *p = px & 255;
          px >>= 8;
          p++;
        }
      }
    } else if (bpp == 4) {    /* 32-bit display */
      *(Uint32 *)p = px;
    }
  }
}

void sdlutil::drawline(SDL_Surface *screen, int x0, int y0,
                       int x1, int y1, 
                       Uint8 R, Uint8 G, Uint8 B) {
  Line l{x0, y0, x1, y1};

  /* direct pixel access */
  slock(screen);

  if (4 == screen->format->BytesPerPixel) {
    const Uint32 color = SDL_MapRGB(screen->format, R, G, B);
    // This is the most common case, so unroll.
    const Uint32 stride = screen->pitch/4;

    Uint32 *bufp = (Uint32 *)screen->pixels;

    bufp[y0 * stride + x0] = color;
    int x, y;
    while (l.Next(x, y))
      bufp[y * stride + x] = color;

  } else {
    // General case.
    int x, y;
    drawpixel(screen, x0, y0, R, G, B);
    while (l.Next(x, y)) {
      drawpixel(screen, x, y, R, G, B);
    }
  }

  sulock(screen);
}

void sdlutil::DrawClipLine32(SDL_Surface *screen, int x0, int y0,
                             int x1, int y1, 
                             Uint32 color) {
  // Only 32-bit!
  if (screen->format->BytesPerPixel != 4) return;

  // However, if a line completely misses the screen, we can
  // just draw nothing.
  if (x0 < 0 && x1 < 0)
    return;
  if (x0 >= screen->w && x1 >= screen->w)
    return;
  if (y0 < 0 && y1 < 0)
    return;
  if (y0 >= screen->h && y1 >= screen->h)
    return;
  
  Line l{x0, y0, x1, y1};

  Uint32 *bufp = (Uint32 *)screen->pixels;
  int stride = screen->pitch >> 2;
  auto SetPixel = [color, bufp, stride](int x, int y) {
      bufp[y * stride + x] = color;
    };
  
  /* direct pixel access */
  slock(screen);
  int x, y;
  if (x0 >= 0 && y0 >= 0 && x0 < screen->w && y0 < screen->h)
    SetPixel(x0, y0);
  while (l.Next(x, y)) {
    if (x >= 0 && y >= 0 && x < screen->w && y < screen->h) {
      SetPixel(x, y);
    }
  }
  sulock(screen);
}

void sdlutil::drawclipline(SDL_Surface *screen, int x0, int y0,
                           int x1, int y1, 
                           Uint8 R, Uint8 G, Uint8 B) {
  /* PERF could maprgb once */

  /* PERF clipping can be much more efficient, but it is a
     bit tricky to do in integer coordinates, we can not often
     represent a clipped line exactly with new integer points.
  */

  // However, if a line completely misses the screen, we can
  // just draw nothing.
  if (x0 < 0 && x1 < 0)
    return;
  if (x0 >= screen->w && x1 >= screen->w)
    return;
  if (y0 < 0 && y1 < 0)
    return;
  if (y0 >= screen->h && y1 >= screen->h)
    return;
  
  Line l{x0, y0, x1, y1};

  /* direct pixel access */
  slock(screen);
  int x, y;
  if (x0 >= 0 && y0 >= 0 && x0 < screen->w && y0 < screen->h)
    drawpixel(screen, x0, y0, R, G, B);
  while (l.Next(x, y)) {
    if (x >= 0 && y >= 0 && x < screen->w && y < screen->h)
      drawpixel(screen, x, y, R, G, B);
  }
  sulock(screen);
}

bool sdlutil::clipsegment(float cx0, float cy0, float cx1, float cy1,
                          float &x0, float &y0, float &x1, float &y1) {
  // Cohen--Sutherland clipping.
  enum Code : Uint32 {
    LEFT = 1 << 0,
    RIGHT = 1 << 1,
    BOTTOM = 1 << 2,
    TOP = 1 << 3,
  };

  auto GetCode = [cx0, cy0, cx1, cy1](float x, float y) {
    Uint32 code = 0;
    if (x < cx0) code |= LEFT;
    else if (x > cx1) code |= RIGHT;
    if (y < cy0) code |= TOP;
    else if (y > cy1) code |= BOTTOM;
    return code;
  };

  // Line completely misses screen.
  Uint32 code0 = GetCode(x0, y0), code1 = GetCode(x1, y1);
  for (;;) {
    if (0 == (code0 | code1)) {
      // Both endpoints inside now.
      return true;
    }

    // Completely missing the clip rectangle. Draw nothing.
    if (code0 & code1)
      return false;

    auto Update = [cx0, cy0, cx1, cy1, x0, y0, x1, y1, &GetCode]
      (float &x, float &y, Uint32 &code) {
      // (Beware that x aliases x0 or x1, etc.
      if (code & TOP) {
        x = x0 + (x1 - x0) * (cy1 - y0) / (y1 - y0);
        y = cy1;
      } else if (code & BOTTOM) {
        x = x0 + (x1 - x0) * (cy0 - y0) / (y1 - y0);
        y = cy0;
      } else if (code & LEFT) {
        y = y0 + (y1 - y0) * (cx0 - x0) / (x1 - x0);
        x = cx0;        
      } else if (code & RIGHT) {
        y = y0 + (y1 - y0) * (cx1 - x0) / (x1 - x0);
        x = cx1;
      }
      // PERF can probably be rolled into branches above.
      code = GetCode(x, y);
    };

    if (code0) Update(x0, y0, code0);
    else Update(x1, y1, code1);
  }
}

void sdlutil::drawbox(SDL_Surface *s, int x, int y, int w, int h,
                      Uint8 r, Uint8 g, Uint8 b) {
  // PERF can unroll a lot of this. Also, straight lines can be
  // drawn much more easily than with Bresenham, and can be clipped
  // much more easily as well.
  // Top
  drawclipline(s, x, y, x + w - 1, y, r, g, b);
  // Left
  drawclipline(s, x, y, x, y + h - 1, r, g, b);
  // Right
  drawclipline(s, x + w - 1, y, x + w - 1, y + h - 1, r, g, b);
  // Bottom
  drawclipline(s, x, y + h - 1, x + w - 1, y + h - 1, r, g, b);
}

void sdlutil::DrawBox32(SDL_Surface *s, int x, int y, int w, int h,
                        Uint32 rgba) {
  // PERF: Same as above.
  // Top
  DrawClipLine32(s, x, y, x + w - 1, y, rgba);
  // Left
  DrawClipLine32(s, x, y, x, y + h - 1, rgba);
  // Right
  DrawClipLine32(s, x + w - 1, y, x + w - 1, y + h - 1, rgba);
  // Bottom
  DrawClipLine32(s, x, y + h - 1, x + w - 1, y + h - 1, rgba);
}


/* XXX change to use function pointer? */
/* lock before calling */
void sdlutil::drawpixel(SDL_Surface *screen, int x, int y,
                        Uint8 R, Uint8 G, Uint8 B) {
  Uint32 color = SDL_MapRGB(screen->format, R, G, B);
  switch (screen->format->BytesPerPixel) {
  case 1: // Assuming 8-bpp
    {
      Uint8 *bufp;
      bufp = (Uint8 *)screen->pixels + y*screen->pitch + x;
      *bufp = color;
    }
    break;
  case 2: // Probably 15-bpp or 16-bpp
    {
      Uint16 *bufp;
      bufp = (Uint16 *)screen->pixels + y*screen->pitch/2 + x;
      *bufp = color;
    }
    break;
  case 3: // Slow 24-bpp mode, usually not used
    {
      Uint8 *bufp;
      bufp = (Uint8 *)screen->pixels + y*screen->pitch + x * 3;
      if (SDL_BYTEORDER == SDL_LIL_ENDIAN) {
        bufp[0] = color;
        bufp[1] = color >> 8;
        bufp[2] = color >> 16;
      } else {
        bufp[2] = color;
        bufp[1] = color >> 8;
        bufp[0] = color >> 16;
      }
    }
    break;
  case 4: // Probably 32-bpp
    {
      Uint32 *bufp;
      bufp = (Uint32 *)screen->pixels + y*screen->pitch/4 + x;
      *bufp = color;
    }
    break;
  }
}

void sdlutil::drawclippixel(SDL_Surface *screen, int x, int y,
                            Uint8 R, Uint8 G, Uint8 B) {
  if (x < 0 || y < 0 || x >= screen->w || y >= screen->h)
    return;
  drawpixel(screen, x, y, R, G, B);
}


void sdlutil::printsurfaceinfo(SDL_Surface *surf) {
  int f = surf->flags;

#define INFO(flag, str) \
  printf(" %s " str "\n", (f & (flag))?"X":" ");

  printf("==== info for surface at %p ====\n", surf);
  INFO(SDL_HWSURFACE,   "In Video Memory");
  INFO(SDL_ASYNCBLIT,   "Asynch blits if possible");
  INFO(SDL_ANYFORMAT,   "Any pixel format");
  INFO(SDL_HWPALETTE,   "Exclusive palette");
  INFO(SDL_DOUBLEBUF,   "Double buffered");
  INFO(SDL_FULLSCREEN,  "Full screen");
  INFO(SDL_OPENGL,      "OpenGL context");
  INFO(SDL_OPENGLBLIT,  "Supports OpenGL blitting");
  INFO(SDL_RESIZABLE,   "Can resize");
  INFO(SDL_HWACCEL,     "Blit is hardware accelerated");
  INFO(SDL_SRCCOLORKEY, "Colorkey blitting");
  INFO(SDL_RLEACCEL,    "RLE accelerated blitting");
  INFO(SDL_SRCALPHA,    "Blit uses source alpha blending");
  INFO(SDL_PREALLOC,    "Uses preallocated memory");

#undef INFO
}

SDL_Surface *sdlutil::fliphoriz(SDL_Surface *src) {
  SDL_Surface *dst = makesurface(src->w, src->h);
  if (!dst) return nullptr;

  slock(src);
  slock(dst);
  
  for (int y = 0; y < src->h; y++) {
    for (int x = 0; x < src->w; x++) {
      Uint32 px = getpixel(src, x, y);
      setpixel(dst, (src->w - x) - 1, y, px);
    }
  }

  slock(dst);
  sulock(src);
  return dst;
}

// Fills a flat triangle (vertices 2 and 3 have the same y coordinate, given once as y23)
// with the given color. Works by drawing the edges from v1 to v2 and v1 to v3 using
// Bresenham's algorithm, then drawing a horizontal line between the generated x coordinates
// when they advance. ASSUMES bpp = 4.
static void BresenhamTriangle32(SDL_Surface *surf,
                                int x1, int y1, int x2, int y23, int x3, Uint32 color) {
  Uint32 *bufp = (Uint32 *)surf->pixels;
  const int height = surf->h;
  const int width = surf->w;
  const int stride = surf->pitch >> 2;
  auto DrawHorizLine = [color, bufp, height, width, stride](int x1, int y, int x2) {
      if (y < 0 || y >= height)
        return;

      if (x2 < x1) std::swap(x1, x2);

      x1 = std::max(x1, 0);
      x2 = std::min(x2, width - 1);

      Uint32 *p = &bufp[y * stride + x1];
      for (int i = 0; i < (x2 - x1); i++) {
        *p = color;
        p++;
      }
    };

  const int y2 = y23, y3 = y23;
  int tmpx1 = x1, tmpy1 = y1;
  int tmpx2 = x1, tmpy2 = y1;

  bool changed1 = false, changed2 = false;
        
  int dx1 = std::abs(x2 - x1);
  int dy1 = std::abs(y2 - y1);
        
  int dx2 = std::abs(x3 - x1);
  int dy2 = std::abs(y3 - y1);

  // Like copysign for ints. -1, 0, or 1
  auto Sign = [](int i) { return (i > 0) - (i < 0); };
  
  int signx1 = Sign(x2 - x1);
  int signx2 = Sign(x3 - x1);
        
  int signy1 = Sign(y2 - y1);
  int signy2 = Sign(y3 - y1);

  // Canonicalize the order. PERF: Maybe caller can ensure this; it's bad
  // that we branch in the loop below.
  if (dy1 > dx1) {
    std::swap(dy1, dx1);
    changed1 = true;
  }
        
  if (dy2 > dx2) {
    std::swap(dx2, dy2);
    changed2 = true;
  }
        
  int e1 = 2 * dy1 - dx1;
  int e2 = 2 * dy2 - dx2;

  // PERF: Can exit loop early if y value is offscreen.
  for (int i = 0; i <= dx1; i++) {
    DrawHorizLine(tmpx1, tmpy1, tmpx2);

    // This is like the merge step in merge sort now; generate points
    // from each line until it catches up with the other one.
    while (e1 >= 0) {
      if (changed1)
        tmpx1 += signx1;
      else
        tmpy1 += signy1;
      e1 -= 2 * dx1;
    }
            
    if (changed1)
      tmpy1 += signy1;
    else
      tmpx1 += signx1;  
    
    e1 += 2 * dy1;

    while (tmpy2 != tmpy1) {
      while (e2 >= 0) {
        if (changed2)
          tmpx2 += signx2;
        else
          tmpy2 += signy2;
        e2 -= 2 * dx2;
      }

      if (changed2)
        tmpy2 += signy2;
      else
        tmpx2 += signx2;
      
      e2 += 2 * dy2;
    }
  }
}

void sdlutil::FillTriangle32(SDL_Surface *surf,
                             int x1, int y1, int x2, int y2, int x3, int y3,
                             Uint32 color) {
  // Only 32-bit!
  if (surf->format->BytesPerPixel != 4) return;

# define SWAP(a, b) do { std::swap(x ## a, x ## b); std::swap(y ## a, y ## b); } while (0)
  // Strategy is to draw two triangles with a horizontal edge, using
  // BresenhamTriangle32. To figure out what case we're in, sort the
  // three vertices such that y1 <= y2 <= y3.

  // PERF: This is "sort all but the last, sort all but the first, and then sort all
  // but the last" which is decent here (max 3 comparisons), although in some branches
  // we can know that we don't need to execute other ones.
  if (y1 > y2) {
    SWAP(1, 2);
  }
  if (y2 > y3) {
    SWAP(2, 3);
  }
  if (y1 > y2) {
    SWAP(1, 2);
  }
# undef SWAP

  // Degenerate cases where one edge is already flat.
  if (y2 == y3) {
    BresenhamTriangle32(surf, x1, y1, x2, y2, x3, color);
  } else if (y1 == y2) {
    BresenhamTriangle32(surf, x3, y3, x1, y1, x2, color);
  } else {
    // Split into two triangles; need to calculate the projection of v2 onto the
    // line from v1 to v3.
    // Note some loss of precision here, and use of floating point. :/
    const int x = x1 + ((float)(y2 - y1) / (float)(y3 - y1)) * (x3 - x1);
    BresenhamTriangle32(surf, x1, y1, x2, y2, x, color);
    BresenhamTriangle32(surf, x3, y3, x2, y2, x, ~color);
  }
}
