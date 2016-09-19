#ifndef __PATTERN_H
#define __PATTERN_H

#include "level.h"

#include <memory>
#include <utility>
#include <vector>


/* The pattern matcher is to escape levels what
   emacs's regular expression matcher is to text.

   It allows certain templates to be specified
   and then searched for inside levels. This is
   mainly used within the random level generators
   in editai.
*/

struct Match {
  /* index that matched the (relative) top left */
  int top_left() const { return topleft; }

  /* the match may be in a different orientation.
     these functions give the relative
     up/down/left/right directions. (Ie, if the
     pattern is matched upside down, then up()
     returns DIR_DOWN */
  dir up() const { return up_dir; }
  dir down() const { return dir_reverse(up_dir); }
  dir right() const { return right_dir; }
  dir left() const { return dir_reverse(right_dir); }

  /* any 'register' set in the original pattern
     can be fetched here. false if no such
     register */
  bool getindex(int reg, int &x, int &y) {
    if (reg < nregs) {
      int idx = regs[reg];
      lev->where(idx, x, y);
      return true;
    } else return false;
  }

  struct stream {
    /* returns 0 when the stream is empty */
    virtual Match *next() = 0;
    virtual ~stream() {}
  };

  ~Match() {
    lev->destroy();
  }

  /* takes ownership of register array, but not level */
  Match(int idx, int udir, int rdir, int nr, vector<int> regs, Level *l) :
    topleft(idx), up_dir(udir), right_dir(rdir), nregs(nr),
    regs(std::move(regs)) {

    lev = l->clone();
  }

 private:
  const int topleft;
  const int up_dir;
  const int right_dir;
  const int nregs;
  const vector<int> regs;
  Level *lev;
};

/* unfortunately we have to put the implementation
   in the header. such is template programming. */
template <class Info>
struct Pattern {
  static std::unique_ptr<Pattern> Create(const string &s_in) {
    std::unique_ptr<Pattern> p{new Pattern};

    for (int i = 0; i < 256; i++)
      p->tab[i].t = HType::ERROR;

    /* ignore whitespace */
    string s = util::replace(s_in, " ", "");

    p->nregs = 0;
    p->w = -1;
    p->h = 0;

    /* conservative estimate */
    p->chars = (char *) malloc(sizeof (char) * s.length());
    p->regs  = (int *)  malloc(sizeof (int)  * s.length());

    {
      int thisline = 0;
      int idx = 0;
      for (unsigned int i = 0; i < s.length(); i++) {

        /* maybe we reached eol? */
        if (s[i] == '\n') {
          if (p->w == -1) {
            p->w = thisline;
          } else {
            if (p->w != thisline) {
              printf("pattern width mismatch\n");
              return 0;
            }
          }
          thisline = 0;
          p->h++;
          continue;
        }

        int reg = -1;

        /* look at register naming prefix */
        if (s[i] == '\\') {
          reg = 0;
          i++;
          if (!(i < s.length())) return 0;
          while (s[i] >= '0' && s[i] <= '9') {
            reg *= 10;
            reg += s[i] - '0';
            i++;
            if (!(i < s.length())) return 0;
          }
          p->nregs = std::max(p->nregs, reg + 1);

        }

        p->regs[idx]  = reg;
        p->chars[idx] = s[i];
        idx++;
        thisline++;
      }
    }

    if (p->w <= 0) return nullptr;
    if (p->h <= 0) return nullptr;

    return p;
  }

 private:
  int w = 0, h = 0;
  int *regs = nullptr;
  int nregs = 0;
  char *chars = nullptr;

 public:
  /* users can define their own predicates */
  void setpredicate(char c,
                    bool (*f)(Level *, Info *, int x, int y)) {

    int idx = ((unsigned int) c) & 255;
    tab[idx].t = HType::FN;
    tab[idx].u.f = f;
  }

  void settile(char c, int t) {
    int idx = ((unsigned int) c) & 255;
    tab[idx].t = HType::MATCHTILE;
    tab[idx].u.tile = t;
  }

  /* find any arbitrary match */
  Match *find(Level *l, Info *inf) {
    Match::stream *i = findall(l, inf);
    Match *m = nullptr;
    if (i) {
      m = i->next();
    }
    delete i;
    return m;
  }

  struct mystream : public Match::stream {
    mystream(Level *l, Info *i, Pattern<Info> *p)
      /* to get a deterministic sequential generator here
         add ",0" in the initializer of g */
      : lev(l), inf(i), pat(p), g(lev->w * lev->h) {

      dirsleft = 4;
      this_dir = 1 + (util::random() & 3);
    }

    /* PERF: this should detect symmetric patterns and only look
       for them in the directions in which they are different! */
    virtual Match *next() {
      /* if there are more dirs in the current
         position, then */
      for (;;)
        if (g.anyleft() && dirsleft) {
          this_dir = 1 + (this_dir % 4);
          dirsleft--;

          /* printf("now dirsleft %d, thisdir %s\n", dirsleft, dirstring(this_dir).c_str()); */

          int x, y;
          lev->where(g.item(), x, y);

          int sd = this_dir;

          /* printf("try from %d/%d in dir %s\n",
             x, y, dirstring(sd).c_str()); */

          /* match topleft = x, y
             in direction sd */

          /* "real" dx and dy.
             read    a = rd D1 D2    as

             when moving one positive degree
             in virtual dimension D1, this
             corresponds to 'a' degrees of
             motion along real dimension D2.

             for dir_up, real and virtual
             dimensions coincide, so this
             is rdxx = 1, rdyy = 1, and
             the others 0.

             XXX it would be possible to support
             flips in addition to rotations, and
             it seems sensible to do so? */

          int rdxx = 0, rdxy = 0,
            rdyx = 0, rdyy = 0;

          switch (sd) {
          case DIR_UP:
            rdxx = rdyy = 1;
            break;
          case DIR_DOWN:
            rdxx = rdyy = -1;
            break;
          case DIR_LEFT:
            rdxy = -1;
            rdyx = 1;
            break;
          case DIR_RIGHT:
            rdyx = -1;
            rdxy = 1;
            break;
          default:
            printf("bad dir!");
            abort();
          }

          /* bounds check. we already know
             x and y are in bounds, but what about
             the opposite corner of the pattern? */
          if (x + (pat->w * rdxx) < -1 ||
              x + (pat->w * rdxx) > lev->w ||
              x + (pat->h * rdyx) < -1 ||
              x + (pat->h * rdyx) > lev->w ||

              y + (pat->h * rdyy) < -1 ||
              y + (pat->h * rdyy) > lev->h ||
              y + (pat->w * rdxy) < -1 ||
              y + (pat->w * rdxy) > lev->h) continue;

          /* now we know that we can
             potentially fit the pattern,
             and our translation is set up.

             create our register file */

          vector<int> r;
          r.resize(pat->nregs);
          for (int z = 0; z < pat->nregs; z++) r[z] = 0;

          for (int vi = 0; vi < (pat->w * pat->h); vi++) {
            int vx = vi % pat->w;
            int vy = vi / pat->w;

            int rx = x + (rdxx * vx) + (rdyx * vy);
            int ry = y + (rdyy * vy) + (rdxy * vx);

            /* now test: */

            unsigned char code = pat->chars[vi];
            switch (pat->tab[code].t) {
            case HType::MATCHTILE:
              if (pat->tab[code].u.tile != lev->tileat(rx, ry)) goto no_match;
              break;
            case HType::FN:
              if (!pat->tab[code].u.f(lev, inf, rx, ry)) goto no_match;
              break;
            case HType::ERROR: goto no_match;
            }

            /* set reg, if any */
            if (pat->regs[vi] >= 0) r[pat->regs[vi]] = lev->index(rx, ry);
          }

          /* successful match */

          {
            /* XXX this should be derived from rdxx, etc.
               (so that we can support flips) */
            const int rd = turnright(sd);

            /*
              printf("regs are:\n");
              for (int zz=0; zz < pat->nregs; zz++) {
              printf("  %d=%d ", zz, r[zz]);
              }
              printf("\n");
            */

            return new Match(lev->index(x, y), sd, rd,
                             pat->nregs, std::move(r), lev);
          }
          /* unsuccessful */
        no_match:;

        } else {

          if (g.anyleft()) {
            g.next();
            dirsleft = 4;
            this_dir = 1 + (3 & (unsigned)util::random());
            /* go again */
          } else return 0;

        } /* dirsleft */

    } /* mystream::next */

    private:

    /* this is the entire state. we own none
       of these pointers */
    Level *lev;
    Info *inf;
    int dirsleft;
    int this_dir;

    Pattern<Info> *pat;
    Generator g;
  };

  /* find all matches. the stream is only valid
     while this pattern is still around */
  Match::stream *findall(Level *lev, Info *inf) {
    return new mystream(lev, inf, this);
  }

  ~Pattern() {
    free(regs);
    free(chars);
  }

 private:
  friend struct mystream;

  enum class HType { ERROR, MATCHTILE, FN, };

  struct handler {
    HType t;
    union {
      bool (*f)(Level *, Info *, int x, int y);
      int tile;
    } u;
  };

  handler tab[256];
};


#endif
