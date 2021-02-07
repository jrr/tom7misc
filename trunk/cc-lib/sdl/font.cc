
#include "font.h"
#include "SDL.h"
#include "sdlutil.h"

#include <string>
#include <memory>

using namespace std;

namespace {
enum attr { COLOR, ALPHA, };

struct AttrList {
  attr what;
  int value;

  AttrList *next;

  AttrList(attr w, int h, AttrList *n) : 
    what(w), value(h), next(n) {}

  /* PERF: we often have to look at the entire stack. */
  /* sets the current color and alpha according to the stack */
  static void set(AttrList *cs, int &c, int &a) {
    c = 0;
    a = 0;
    bool gotc = false, gota = false;

    while (cs && !(gotc && gota)) {
      switch(cs->what) {
      case COLOR: 
        if (!gotc) { c = cs->value; gotc = true; }
        break;
      case ALPHA: 
        if (!gota) { a = cs->value; gota = true; }
        break;
      }
      cs = cs -> next;
    }      
  }

  static int pop(AttrList *&il) {
    if (il) {
      AttrList *tmp = il;
      int t = tmp->value;
      il = il->next;
      delete tmp;
      return t;
    } else return 0;
  }
};
}

struct FontReal final : public Font {
  /* data is an array of font images, differing in
     their alpha transparency */
  SDL_Surface **data;
  SDL_Surface *screen;
  int ndim;

  unsigned char chars[256];

  virtual int sizex(const string &);
  virtual int sizex_plain(const string &);

  virtual void draw(int x, int y, const string &s);
  virtual void drawto(SDL_Surface *, int x, int y, const string &s);

  virtual void draw_plain(int x, int y, const string &s);
  virtual void drawto_plain(SDL_Surface *, int x, int y, const string &s);

  // XXX get rid of this.
  virtual void destroy() {
    delete this;
  }

  virtual int drawlinesc(int x, int y, const string &, bool);
  virtual int drawlines(int x, int y, const string &);
  virtual int drawcenter(int x, int y, const string &);

  virtual ~FontReal() {
    if (data) {
      for (int i = 0; i < ndim; i++) {
        if (data[i]) SDL_FreeSurface(data[i]);
      }
      free(data);
    }
  }
};

Font::~Font() {}

Font *Font::create_from_surface(SDL_Surface *screen,
                                SDL_Surface *font_surface,
                                const string &charmap,
                                int width,
                                int height,
                                int styles,
                                int overlap,
                                int ndim) {
  std::unique_ptr<FontReal> f{new FontReal};
  f->screen = screen;
  f->width = width;
  f->height = height;
  f->styles = styles;
  f->overlap = overlap;
  f->data = 0;
  f->ndim = ndim;

  if (!ndim) return nullptr;

  f->data = (SDL_Surface **)malloc(sizeof (SDL_Surface *) * ndim);
  if (!f->data) return nullptr;
  for (int z = 0; z < ndim; z++) f->data[z] = 0;

  f->data[0] = font_surface;

  int last = 0;
  while (last < ndim - 1) {
    last++;
    f->data[last] = sdlutil::alphadim(f->data[last - 1]);
    if (!f->data[last]) return nullptr;
  }

  for (int j = 0; j < 256; j++) {
    f->chars[j] = 0;
  }

  for (unsigned int i = 0; i < charmap.length(); i++) {
    int idx = (unsigned char)charmap[i];
    f->chars[idx] = i;
  }

  return f.release();
}

Font *Font::create(SDL_Surface *screen,
                   const string &file,
                   const string &charmap,
                   int width,
                   int height,
                   int styles,
                   int overlap,
                   int ndim) {
  SDL_Surface *fon = sdlutil::LoadImage(file);
  if (fon == nullptr) return nullptr;
  return Font::create_from_surface(screen, fon, charmap, 
                                   width, height, styles, overlap, ndim);
}

Font *Font::CreateX(int px,
                    SDL_Surface *screen,
                    const string &file,
                    const string &charmap,
                    int width,
                    int height,
                    int styles,
                    int overlap,
                    int ndim) {
  SDL_Surface *fon_orig = sdlutil::LoadImage(file);
  if (fon_orig == nullptr) return nullptr;
  SDL_Surface *fonx = sdlutil::GrowX(fon_orig, px);
  SDL_FreeSurface(fon_orig);
  if (fonx == nullptr) return nullptr;
  return Font::create_from_surface(screen, fonx, charmap,
                                   width * px, height * px, styles, overlap * px, ndim);
}                           

void FontReal::draw(int x, int y, const string &s) {
  drawto(screen, x, y, s);
}

void FontReal::draw_plain(int x, int y, const string &s) {
  drawto_plain(screen, x, y, s);
}

void FontReal::drawto_plain(SDL_Surface *surf, int x, int y, const string &s) {
  SDL_Rect src, dest;

  dest.x = x;
  dest.y = y;

  src.w = width;
  src.h = height;

  for (unsigned int i = 0; i < s.length(); i++) {
    int idx = (unsigned char)s[i];
    src.x = chars[idx] * width;
    src.y = 0;
  
    SDL_BlitSurface(data[0], &src, surf, &dest);

    dest.x += (width - overlap);
  }
}

void FontReal::drawto(SDL_Surface *surf, int x, int y, const string &s) {
  SDL_Rect src, dest;
  /* XXX can't init these once, since blit can side-effect fields
     (try drawing off the screen) */
  dest.x = x;
  dest.y = y;

  src.w = width;
  src.h = height;

  /* keep track of our color and alpha settings */
  AttrList *cstack = 0; 
  int color, alpha;
  AttrList::set(cstack, color, alpha);

  for (unsigned int i = 0; i < s.length(); i++) {

    if ((unsigned char)s[i] == '^') {
      if (i < s.length()) {
        i++;
        switch((unsigned char)s[i]) {
        case '^': break; /* quoted... keep going */
        case '<': /* pop */
          if (cstack) AttrList::pop(cstack);
          AttrList::set(cstack, color, alpha);
          continue;
        default:
          if (s[i] >= '#' && s[i] <= '\'') {
            /* alpha */
            cstack =
              new AttrList(ALPHA,
                           abs((unsigned char)s[i] - '#') % ndim,
                           cstack);
          } else {
            /* color */
            cstack = 
              new AttrList(COLOR,
                           abs(((unsigned char)s[i] - '0')
                               % styles),
                           cstack);
          }

          AttrList::set(cstack, color, alpha);
          continue;
        }
      }
    }

    /* current color */

    int idx = (unsigned char)s[i];
    src.x = chars[idx] * width;
    src.y = color * height;
  
    SDL_BlitSurface(data[alpha], &src, surf, &dest);

    dest.x += (width - overlap);
  }

  /* empty list */
  while (cstack) AttrList::pop(cstack);

}

int FontReal::sizex_plain(const string &s) {
  return s.length() * (width - overlap);
}

int FontReal::sizex(const string &s) {
  return Font::length(s) * (width - overlap);
}

int FontReal::drawlines(int x, int y, const string &s) {
  return drawlinesc(x, y, s, false);
}

int FontReal::drawcenter(int x, int y, const string &s) {
  return drawlinesc(x, y, s, true);
}


/* XXX doesn't handle color codes that span lines. */
int FontReal::drawlinesc(int x, int y, const string &s, bool center) {
  int start = 0;
  unsigned int idx = 0;
  /* draw every non-empty string delimited by \n */
  int offset = 0;
  int wroteany = 0;
  for (;; idx++) {
  again:;
    /* reached end of string? */
    if (idx >= s.length()) {
      if (wroteany) {
        int xx;
        string sub = s.substr(start, idx - start);
        if (center) {
          xx = x - (sizex(sub) >> 1);
        } else xx = x;

        draw(xx, y + offset, sub);
        return offset + height;
      } else return offset;
    }
     
    if (s[idx] == '\n') {
      int xx;
      string sub = s.substr(start, idx - start);
      if (center) {
        xx = x - (sizex(sub) >> 1);
      } else xx = x;
      draw(xx, y + offset, sub);
      offset += height;
      start = idx + 1;
      idx = idx + 1;
      wroteany = 0;
      goto again;
    } else wroteany = 1;
  }
}

/* by example:
   "" returns 0
   "\n" returns 1
   "hello" returns 1
   "hello\n" returns 1
   "hello\nworld" returns 2
   "hello\nworld\n" returns 2
*/
int Font::lines(const string &s) {
  unsigned int idx = 0;
  int sofar = 0;

  enum Mode { M_FINDANY, M_STEADY, };
  Mode m = M_FINDANY;

  for (;; idx++) {
    if (idx >= s.length()) return sofar;
    switch (m) {
    case M_FINDANY:
      if (s[idx] == '\n') {
        sofar++;
        continue;
      } else {
        sofar++;
        m = M_STEADY;
        continue;
      }
    case M_STEADY:
      if (s[idx] == '\n') {
        m = M_FINDANY;
        continue;
      }
    }
  }
}

string Font::substr(const string &s, unsigned int start, unsigned int len) {
  /* skip 'start' chars */

  unsigned int i = 0; /* pos in fontstring */
  unsigned int j = 0; /* number of actual chars seen */

  for (; i < s.length() && j < start; i++) {

    if ((unsigned char)s[i] == '^') {
      if (i < s.length()) {
        i++;
        if ((unsigned char)s[i] == '^') j++;
      } else j++; /* ??? */
    } else j++;

  }

  j = i;
  /* substring will start at j; now count
     'len' chars to find end */

  unsigned int k = 0;
  /* XXX should also add any trailing
     control codes */
  for (; i < s.length() && k < len; i++) {

    if ((unsigned char)s[i] == '^') {
      if (i < s.length()) {
        i++;
        if ((unsigned char)s[i] == '^') j++;
      } else k++; /* ??? */
    } else k++;

  }

  return s.substr(j, i - j);
}

/* assume n <= font::length(s) */
string Font::prefix(const string &s, unsigned int n) {
  return Font::substr(s, 0, n);
}

/* assume n <= font::length (s) */
string Font::suffix(const string &s, unsigned int n) {
  return Font::substr(s, Font::length(s) - n, n);
}

unsigned int Font::length(const string &s) {
  unsigned int i, n=0;
  for (i = 0; i < s.length(); i++) {
    if ((unsigned char)s[i] == '^') {
      if (i < s.length()) {
        i++;
        if ((unsigned char)s[i] == '^') n++;
      } else n++; /* ??? */
    } else n++;
  }
  return n;
}

/* XXX should go to fontutil */
#include "chars.h"
string Font::pad(const string &s, int ns) {
  unsigned int l = Font::length(s);
    
  unsigned int n = abs(ns);

  if (l > n) {
    return truncate(s, ns);
    // return font::prefix(s, n - 3) + (string)ALPHA50 "..." POP;
  } else {
    string ou;
    /* PERF there's definitely a faster way to do this */
    for (unsigned int j = 0; j < n - l; j++) ou += " ";
    if (ns > 0) {
      return s + ou;
    } else {
      return ou + s;
    }
  }
}

string Font::truncate(const string &s, int ns) {
  unsigned int l = Font::length(s);
  unsigned int n = abs(ns);
  if (l > n) {
    if (ns > 0) {
      return Font::prefix(s, n - 3) + (string)ALPHA50 "..." POP;
    } else {
      return (string)ALPHA50 "..." POP + Font::suffix(s, n - 3);
    }
  } else return s;
}
