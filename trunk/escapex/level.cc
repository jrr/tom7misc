
#include "level.h"

#include <string>
#include <memory>

#include "ptrlist.h"
#include "rle.h"

/* This code is non-SDL, so it should be portable! */

bool Level::allowbeam(int tt) {
  switch (tt) {
  case T_FLOOR:
  case T_ELECTRIC:
  case T_ROUGH:
  case T_RDOWN:
  case T_GDOWN:
  case T_BDOWN:
  case T_TRAP2:
  case T_TRAP1:
  case T_PANEL:
  case T_BPANEL:
  case T_GPANEL:
  case T_RPANEL:
  case T_BLACK:
  case T_HOLE:
    return true;
  default:
    return false;
  }
}

/* Return true if a laser can 'see' the player.
   set tilex,y to that laser, d to the direction it
   fires in. */
/* also now checks for cohabitation with bots, which
   is deadly */
bool Level::isdead(int &tilex, int &tiley, dir &d) const {
  /* are we in the same square as a bot? Then we die. */
  if (botat(guyx, guyy)) {
    tilex = guyx;
    tiley = guyy;
    d = DIR_NONE;
    return true;
  }

  /* is there an exploded bomb adjacent to us, or on us?
     then die. */
  for (dir db = FIRST_DIR_SELF; db <= LAST_DIR; db++) {
    int xx, yy;
    if (travel(guyx, guyy, db, xx, yy)) {

      /* nb, botat might not be correct, since it
         returns the lowest bot; so just loop
         manually: */
      int z = index(xx, yy);
      for (int m = 0; m < nbots; m++) {
        // printf("Bot %d type %d @ %d\n", m, bott[m], boti[m]);
        if (boti[m] == z && bott[m] == B_BOMB_X) {
          /* ?? */
          tilex = guyx;
          tiley = guyy;
          d = DIR_NONE;
          return true;
        }
      }
    }
  }

  /* no? look for lasers. the easiest way is to look
     for lasers from the current dude outward. */

  for (dir dd = FIRST_DIR; dd <= LAST_DIR; dd++) {
    int lx = guyx, ly = guyy;

    while (travel(lx, ly, dd, lx, ly)) {
      if (tileat(lx, ly) == T_LASER) {
        tilex = lx;
        tiley = ly;
        d = dir_reverse(dd);
        return true;
      }
      int tt = tileat(lx, ly);
      if (!allowbeam(tt)) break;
      /* all robots also block lasers. */
      if (botat(lx, ly)) break;
    }
  }

  return false;
}


void Level::swapo(int idx) {
  int tmp = tiles[idx];
  tiles[idx] = otiles[idx];
  otiles[idx] = tmp;

  /* swap haspanel/opanel and their refinements as well */
  flags[idx] =
    /* panel bits */
    ((flags[idx] & TF_HASPANEL) ? TF_OPANEL : TF_NONE) |
    ((flags[idx] & TF_OPANEL) ? TF_HASPANEL : TF_NONE) |

    /* refinement */
    ((flags[idx] & TF_RPANELL) ? TF_ROPANELL : TF_NONE) |
    ((flags[idx] & TF_RPANELH) ? TF_ROPANELH : TF_NONE) |

    /* orefinement */
    ((flags[idx] & TF_ROPANELL) ? TF_RPANELL : TF_NONE) |
    ((flags[idx] & TF_ROPANELH) ? TF_RPANELH : TF_NONE) |

    /* erase old */
    (flags[idx] & ~(TF_HASPANEL | TF_OPANEL |
                    TF_RPANELL | TF_RPANELH |
                    TF_ROPANELL | TF_ROPANELH));
}

tile Level::RealPanel(int f) {
  if (f & TF_RPANELH) {
    if (f & TF_RPANELL) return T_RPANEL;
    else return T_GPANEL;
  } else {
    if (f & TF_RPANELL) return T_BPANEL;
    else return T_PANEL;
  }
}

bool Level::issphere(int t) {
  return (t == T_SPHERE ||
          t == T_RSPHERE ||
          t == T_GSPHERE ||
          t == T_BSPHERE);
}

bool Level::issteel(int t) {
  return (t == T_STEEL ||
          t == T_RSTEEL ||
          t == T_GSTEEL ||
          t == T_BSTEEL);
}

bool Level::ispanel(int t) {
  return (t == T_PANEL ||
          t == T_RPANEL ||
          t == T_GPANEL ||
          t == T_BPANEL);
}

bool Level::triggers(int tile, int panel) {
  /* "anything" triggers grey panels */
  if (panel == T_PANEL) return true;
  if (panel == T_RPANEL) {
    return tile == T_RSPHERE || tile == T_RSTEEL;
  }
  if (panel == T_GPANEL) {
    return tile == T_GSPHERE || tile == T_GSTEEL;
  }
  if (panel == T_BPANEL) {
    return tile == T_BSPHERE || tile == T_BSTEEL;
  }
  /* ? */
  return false;
}

#include "move2016.h"

#define FSDEBUG if (0)

// Clears just the tiles portion of the level.
void Level::ClearMap() {
  for (int i = 0; i < w * h; i++) {
    tiles[i]  = T_FLOOR;
    otiles[i] = T_FLOOR;
    dests[i]  = 0; /* 0, 0 */
    flags[i]  = 0;
  }
}

bool Level::sanitize() {
  bool was_sane = true;

  int len = w * h;

  if (w < 1 || h < 1) {
    FSDEBUG printf("insane: size must be positive: %dx%d\n", w, h);
    was_sane = false;
    w = std::max(1, w);
    h = std::max(1, h);

    int bytes = w * h * sizeof (int);
    tiles  = (int*)realloc(tiles, bytes);
    otiles = (int*)realloc(otiles, bytes);
    dests  = (int*)realloc(dests, bytes);
    flags  = (int*)realloc(flags, bytes);

    ClearMap();
    len = w * h;
  }

  for (int i = 0; i < len; i++) {
    /* a destination outside the level */
    if (dests[i] < 0 || dests[i] >= len) {
      FSDEBUG printf("insane: dest out of range: %d\n", dests[i]);
      was_sane = false;
      dests[i] = 0;
    }

    /* an illegal tile */
    if (tiles[i] >= NUM_TILES || tiles[i] < 0) {
      FSDEBUG printf("insane: bad tile: %d\n", tiles[i]);
      was_sane = false;
      tiles[i] = T_BLACK;
    }

    if (otiles[i] >= NUM_TILES || tiles[i] < 0) {
      FSDEBUG printf("insane: bad otile: %d\n", otiles[i]);
      was_sane = false;
      otiles[i] = T_BLACK;
    }

    // Determine the flags that should be set.
    int expected_flags = TF_NONE;
    if (ispanel(tiles[i])) {
      expected_flags |= TF_HASPANEL;
      switch (tiles[i]) {
      case T_RPANEL: expected_flags |= TF_RPANELH | TF_RPANELL; break;
      case T_GPANEL: expected_flags |= TF_RPANELH; break;
      case T_BPANEL: expected_flags |= TF_RPANELL; break;
      }
    }
    if (ispanel(otiles[i])) {
      expected_flags |= TF_OPANEL;
      switch (otiles[i]) {
      case T_RPANEL: expected_flags |= TF_ROPANELH | TF_ROPANELL; break;
      case T_GPANEL: expected_flags |= TF_ROPANELH; break;
      case T_BPANEL: expected_flags |= TF_ROPANELL; break;
      }
    }

    if (flags[i] != expected_flags) {
      FSDEBUG printf("insane: expected flags at %d: %d but got %d\n",
                     i, expected_flags, flags[i]);
      was_sane = false;
      flags[i] = expected_flags;
    }
  }

  /* staring position outside the level */
  if (guyx >= w) { was_sane = false; guyx = w - 1; }
  if (guyy >= h) { was_sane = false; guyy = h - 1; }

  if (guyx < 0) { was_sane = false; guyx = 0; }
  if (guyy < 0) { was_sane = false; guyy = 0; }

  /* bots */
  {
    if ((nbots > 0) && (!boti || !bott)) {
      FSDEBUG printf("insane: non-zero bots but missing bot data\n");
      was_sane = false;
      nbots = 0;
      free(boti);
      free(bott);
      boti = 0;
      bott = 0;
    }

    /* First, make sure that all bots are within reason, not worrying
       about them overlapping. */
    for (int i = 0; i < nbots; i++) {
      int x, y;
      where(boti[i], x, y);

      /* This might put bad bots on top of one another at position 0,
         but we delete them later. */
      if (x >= w || x < 0 || y >= h || y < 0) {
        FSDEBUG printf("insane: bot out of level\n");
        was_sane = false; boti[i] = 0;
      }

      if (bott[i] >= NUM_ROBOTS || bott[i] < 0) {
        FSDEBUG printf("insane: too many bots, or bot index bad\n");
        was_sane = false; bott[i] = B_DALEK;
      }
    }

    /* Now delete any overlapping bots. */
    for (int i = 0; i < nbots; i++) {
      for (int j = i + 1; j < nbots; j++) {
        if (boti[i] == boti[j]) {
          /* So delete bot at index j. Do this
             by swapping with the end and just
             resizing. */
          was_sane = false;
          int t = nbots - 1;
          int ti = boti[t];
          boti[t] = boti[j];
          boti[j] = ti;
          int td = botd[t];
          botd[t] = botd[j];
          botd[j] = td;
          bot tt = bott[t];
          bott[t] = bott[j];
          bott[j] = tt;
          int ta = bota[t];
          bota[t] = bota[j];
          bota[j] = ta;

          nbots--;
          /* And need to check bot j again, since it's
             different. */
          j--;
        }
      }
    }

    /* Now, are the bots in noncanonical order?
       If there is a global inconsistency then there must
       be a local one, so do that fast existence check.
     */
    for (int i = 0; i < nbots - 1; i++) {
      if (isbomb(bott[i]) && !isbomb(bott[i + 1])) {
        was_sane = false;
        /* Preserves relative bot order. */
        fixup_botorder();
        break;
      }
    }
  }

  return was_sane;
}

void Level::fixup_botorder() {
  /* other fields are constant */
  struct BB {
    bot t;
    int i;
  };

  BB *bots = (BB *)malloc(sizeof (BB) * nbots);
  {
    int j = 0;
    /* first put in non-bombs */
    for (int i = 0; i < nbots; i++) {
      if (!isbomb(bott[i])) {
        bots[j].t = bott[i];
        bots[j].i = boti[i];
        j++;
      }
    }

    /* then bombs */
    for (int i = 0; i < nbots; i++) {
      if (isbomb(bott[i])) {
        bots[j].t = bott[i];
        bots[j].i = boti[i];
        j++;
      }
    }
  }

  /* now put them back. */
  for (int i = 0; i < nbots; i++) {
    bott[i] = bots[i].t;
    boti[i] = bots[i].i;
    botd[i] = DIR_DOWN;
    bota[i] = -1;
  }

  free(bots);
}

std::unique_ptr<Level> Level::FromString(string s, bool allow_corrupted) {
  int sw, sh;

  /* check magic! */
  if (s.substr(0, ((string)LEVELMAGIC).length()) != LEVELMAGIC)
    return nullptr;


  FSDEBUG printf("magic ok...");

  unsigned int idx = ((string)LEVELMAGIC).length();

  if (idx + 12 > s.length()) return 0;
  sw = shout(4, s, idx);
  sh = shout(4, s, idx);

  FSDEBUG printf("%d x %d...\n", sw, sh);

  /* too big? */
  if (sw > LEVEL_MAX_WIDTH  || sw < 0 ||
      sh > LEVEL_MAX_HEIGHT || sh < 0 ||
      (sw * sh) > LEVEL_MAX_AREA) return 0;

  int ts = shout(4, s, idx);
  if (idx + ts > s.length()) return 0;
  string title = s.substr(idx, ts);

  idx += ts;

  if (idx + 4 > s.length()) return 0;
  int as = shout(4, s, idx);

  if (idx + as > s.length()) return 0;
  string author = s.substr(idx, as);

  idx += as;

  FSDEBUG printf("\"%s\" by \"%s\"...\n", title.c_str(), author.c_str());

  if (idx + 8 > s.length()) return 0;

  int gx = shout(4, s, idx);
  int gy = shout(4, s, idx);

  FSDEBUG printf("guy: %d,%d\n", gx, gy);

  /* at this point we will be able to return some kind of
     level, even if parts are corrupted */

  std::unique_ptr<Level> l{new Level};
  l->w = sw;
  l->h = sh;

  l->corrupted = false; /* may change later */

  l->title = title;
  l->author = author;
  l->guyx = gx;
  l->guyy = gy;
  l->guyd = DIR_DOWN;

  FSDEBUG printf("tiles = EscapeRLE::Decode(s, %d, %d)\n", idx, sw * sh);

  l->tiles  = EscapeRLE::Decode(s, idx, sw * sh);

  FSDEBUG printf("result %p. now idx is %d\n", l->tiles, idx);

  l->otiles = EscapeRLE::Decode(s, idx, sw * sh);
  l->dests  = EscapeRLE::Decode(s, idx, sw * sh);
  l->flags  = EscapeRLE::Decode(s, idx, sw * sh);

  if (idx + 4 > s.length()) {
    l->nbots = 0;
    l->boti = 0;
    l->bott = 0;
    l->botd = 0;
    l->bota = 0;
  } else {
    l->nbots = shout(4, s, idx);
    if (l->nbots < 0 ||
        l->nbots > LEVEL_MAX_ROBOTS) {
      l->nbots = 0;
      // l->corrupted = true;
      /* probably the reading frame is off
         now, but what can we do when given
         a ridiculous file? */
    }

    l->boti = EscapeRLE::Decode(s, idx, l->nbots);
    l->bott = (bot*)EscapeRLE::Decode(s, idx, l->nbots);

    /* if there are any bots, then we better have
       succeeded in finding some! */
    if (l->nbots && !(l->boti && l->bott)) {
      l->nbots = 0;
      free(l->boti);
      free(l->bott);
      l->boti = 0;
      l->bott = 0;
      l->corrupted = true;
    }

    /* presentational */
    l->botd = (dir*)malloc(l->nbots * sizeof (dir));
    /* initialized */
    l->bota = (int*)malloc(l->nbots * sizeof (int));

    for (int i = 0; i < l->nbots; i++) {
      l->botd[i] = DIR_DOWN;
      l->bota[i] = -1;
    }
  }

  /* XXX support messages here. */


  if (l->tiles && l->otiles && l->dests && l->flags) {
    /* check level's sanity, one last time */
    l->corrupted = (!l->sanitize()) || l->corrupted;

    if (!l->corrupted || allow_corrupted) {
      FSDEBUG printf("success\n");
      return l;
    } else {
      return nullptr;
    }

  } else if (l->tiles && allow_corrupted) {
    /* for anything not found, replace with empty */
    if (!l->otiles) {
      l->otiles = (int*)malloc(sw * sh * sizeof (int));
      if (l->otiles) memset(l->otiles, 0, sw * sh * sizeof (int));
    }

    if (!l->dests) {
      l->dests = (int*)malloc(sw * sh * sizeof (int));
      if (l->dests) memset(l->dests, 0, sw * sh * sizeof (int));
    }

    if (!l->flags) {
      l->flags = (int*)malloc(sw * sh * sizeof (int));
      if (l->flags) memset(l->flags, 0, sw * sh * sizeof (int));
    }

    if (l->otiles && l->dests && l->flags) {
      l->sanitize();
      l->corrupted = true;
      return l;
    } else {
      /* out of memory? */
      return nullptr;
    }

  } else {
    return nullptr;
  }
}

string Level::tostring() const {
  string ou;

  /* magic */
  ou += (string)LEVELMAGIC;

  ou += sizes(w);
  ou += sizes(h);

  ou += sizes(title.length());
  ou += title;

  ou += sizes(author.length());
  ou += author;

  ou += sizes(guyx);
  ou += sizes(guyy);

  ou += EscapeRLE::Encode(w * h, tiles);
  ou += EscapeRLE::Encode(w * h, otiles);
  ou += EscapeRLE::Encode(w * h, dests);
  ou += EscapeRLE::Encode(w * h, flags);

  ou += sizes(nbots);
  ou += EscapeRLE::Encode(nbots, boti);
  ou += EscapeRLE::Encode(nbots, (int*)bott);
  return ou;
}

/* is the tile at x,y connected to anything in
   direction d? (For instance, is there a light
   in direction d?) */
bool Level::isconnected(int pulsex, int pulsey, dir pd) const {
  while (travel(pulsex, pulsey, pd, pulsex, pulsey)) {
    int targ = tileat(pulsex, pulsey);

    switch (targ) {
    case T_REMOTE: return true; /* (and the pulse continues...) */
    case T_BLIGHT: return true;
    case T_RLIGHT: return true;
    case T_GLIGHT: return true;
    case T_TRANSPONDER:
      if (!travel(pulsex, pulsey, pd, pulsex, pulsey))
        return false;
      else {
        /* keep going until we hit another transponder. */
        do {
          int ta = tileat(pulsex, pulsey);
          if (!allowbeam(ta) ||
              botat(pulsex, pulsey) ||
              playerat(pulsex, pulsey)) {
            /* hit something. is it a transponder? */
            if (ta == T_TRANSPONDER) {
              break; /* do */
            } else return false;
          }
          /* otherwise keep going... */
        } while (travel(pulsex, pulsey, pd, pulsex, pulsey));
        /* either we've hit the transponder and the pulse
           continues, or we've fallen off the edge of the
           level (while fails). In the second case, the
           pulse will fail to find a wire, so it will fail
           on the next round */
      }
      break;
    case T_NSWE:
      /* just keep going in same direction */
      continue;

    case T_NS:
      if (pd == DIR_UP || pd == DIR_DOWN) continue;
      else return false;
      break;

    case T_WE:
      if (pd == DIR_LEFT || pd == DIR_RIGHT) continue;
      else return false;
      break;

    case T_NW:
      if (pd == DIR_DOWN) pd = DIR_LEFT;
      else if (pd == DIR_RIGHT) pd = DIR_UP;
      else return false;
      break;

    case T_SW:
      if (pd == DIR_UP) pd = DIR_LEFT;
      else if (pd == DIR_RIGHT) pd = DIR_DOWN;
      else return false;
      break;

    case T_NE:
      if (pd == DIR_DOWN) pd = DIR_RIGHT;
      else if (pd == DIR_LEFT) pd = DIR_UP;
      else return false;
      break;

    case T_SE:
      if (pd == DIR_UP) pd = DIR_RIGHT;
      else if (pd == DIR_LEFT) pd = DIR_DOWN;
      else return false;
      break;

    default: return false; /* not a wire */
    }
  }
  /* pulse can't travel */
  return false;
}

Level::~Level() {
  free(tiles);
  free(otiles);
  free(dests);
  free(flags);

  free(boti);
  free(bott);
  free(botd);
  free(bota);
}

std::unique_ptr<Level> Level::Clone() const {
  std::unique_ptr<Level> n{new Level};

  n->title = title;
  n->author = author;

  n->corrupted = corrupted;

  n->h = h;
  n->w = w;
  n->guyx = guyx;
  n->guyy = guyy;
  n->guyd = guyd;

  int bytes = w * h * sizeof (int);

  n->tiles  = (int*)malloc(bytes);
  n->otiles = (int*)malloc(bytes);
  n->dests  = (int*)malloc(bytes);
  n->flags  = (int*)malloc(bytes);

  memcpy(n->tiles,  tiles,  bytes);
  memcpy(n->otiles, otiles, bytes);
  memcpy(n->dests,  dests,  bytes);
  memcpy(n->flags,  flags,  bytes);

  n->nbots = nbots;
  int bbytes = nbots * sizeof (int);
  n->boti = (int*)malloc(bbytes);
  n->bott = (bot*)malloc(bbytes);
  n->botd = (dir*)malloc(nbots * sizeof (dir));
  n->bota = (int*)malloc(bbytes);

  memcpy(n->boti, boti, bbytes);
  memcpy(n->bott, bott, bbytes);
  memcpy(n->botd, botd, nbots * sizeof (dir));
  memcpy(n->bota, bota, bbytes);

  return n;
}

std::unique_ptr<Level> Level::Blank(int w, int h) {
  std::unique_ptr<Level> n{new Level};
  n->w = w;
  n->h = h;
  n->guyx = 1;
  n->guyy = 1;
  n->guyd = DIR_DOWN;

  n->corrupted = false;

  const int bytes = w * h * sizeof (int);

  n->tiles  = (int*)malloc(bytes);
  n->otiles = (int*)malloc(bytes);
  n->dests  = (int*)malloc(bytes);
  n->flags  = (int*)malloc(bytes);

  /* no bots, no allocation */
  n->nbots  = 0;
  n->boti   = 0;
  n->bott   = 0;
  n->botd   = 0;
  n->bota   = 0;

  n->ClearMap();

  return n;
}

std::unique_ptr<Level> Level::DefBoard(int w, int h) {
  std::unique_ptr<Level> n = Blank(w, h);

  /* just draw blue around it. */

  /* top, bottom */
  for (int i = 0; i < w; i++) {
    n->tiles[i] = T_BLUE;
    n->tiles[(h - 1) * w + i] = T_BLUE;
  }

  /* left, right */
  for (int j = 1; j < h - 1; j++) {
    n->tiles[j * w] = T_BLUE;
    n->tiles[j * w + (w - 1)] = T_BLUE;
  }

  return n;
}

bool Level::VerifyPrefix(const Level *lev, const Solution &s,
                         Solution *out_ref) {
  std::unique_ptr<Level> l = lev->Clone();
  Solution out;

  for (Solution::iter i = Solution::iter(s);
       i.hasnext();
       i.next()) {

    dir d = i.item();

    if (l->Move(d)) {
      /* include it */
      out.Append(d);
      /* potentially fail *after* each move */
      int dummy; dir dumb;
      if (l->isdead(dummy, dummy, dumb)) return false;
      if (l->iswon()) {
        *out_ref = std::move(out);
        return true;
      }
    }
  }

  /* solution is over, but we didn't win or die */
  return false;
}

bool Level::Verify(const Level *lev, const Solution &s) {
  std::unique_ptr<Level> l = lev->Clone();

  int moves;
  const bool won = l->Play(s, moves);
  
  return won && moves == s.Length();
}

bool Level::PlayPrefix(const Solution &s, int &moves, int start, int len) {
  moves = 0;
  for (int z = 0; z < len; z++) {
    dir d = s.At(start + z);

    moves++;
    if (Move(d)) {
      /* potentially fail *after* each move */
      int dummy; dir dumb;
      if (isdead(dummy, dummy, dumb)) return false;
      if (iswon()) return true;
    }

    /* else perhaps a 'strict' mode where this
       solution is rejected */
  }
  /* solution is over, but we didn't win or die */
  return false;
}

bool Level::Play(const Solution &s, int &moves) {
  return PlayPrefix(s, moves, 0, s.Length());
}

/* must be called with sensible sizes (so that malloc won't fail) */
void Level::resize(int neww, int newh) {
  int bytes = neww * newh * sizeof (int);

  int * nt, * no, * nd, * nf;

  nt = (int *)malloc(bytes);
  no = (int *)malloc(bytes);
  nd = (int *)malloc(bytes);
  nf = (int *)malloc(bytes);

  for (int x = 0; x < neww; x++)
    for (int y = 0; y < newh; y++) {

      if (x < w && y < h) {
        nt[y * neww + x] = tiles[y * w + x];
        no[y * neww + x] = otiles[y * w + x];
        nf[y * neww + x] = flags[y * w + x];

        /* set dests to point to the same place in
           the new level, if possible. otherwise
           just point it to 0,0 */
        int odx = dests[y * w + x] % w;
        int ody = dests[y * w + x] / w;
        if (odx < neww && ody < newh && odx >= 0 && ody >= 0) {
          nd[y * neww + x] = odx + (ody * neww);
        } else {
          nd[y * neww + x] = 0;
        }
      } else {
        nt[y * neww + x] = T_FLOOR;
        no[y * neww + x] = T_FLOOR;
        nd[y * neww + x] = 0;
        nf[y * neww + x] = 0;
      }

    }

  /* put bots back in level  */
  {
    int bi = 0;
    for (int b = 0; b < nbots; b++) {
      int bx, by;
      where(boti[b], bx, by);
      if (bx < neww && bx >= 0 &&
          by < newh && by >= 0) {
        /* keep bot */
        boti[bi] = by * neww + bx;
        bott[bi] = bott[b];
        botd[bi] = botd[b];
        bota[bi] = bota[b];
        bi++;
      }
    }
    nbots = bi;

  }

  free(tiles);
  free(otiles);
  free(dests);
  free(flags);

  tiles = nt;
  otiles = no;
  dests = nd;
  flags = nf;

  w = neww;
  h = newh;

  /* sanitization moves the guy back on the board,
     as well as making any destinations point within
     the level */
  sanitize();
}


bool Level::Move(dir d) {
  NullDisamb ctx;
  AList *events = nullptr;
  AList **tail = &events;
  return MoveMaybeAnimate<false, NullDisamb>(d, &ctx, events, tail);
}

bool Level::MoveAnimate(dir d, Disamb *ctx, AList *&events) {
  AList **tail = &events;
  return MoveMaybeAnimate<true, Disamb>(d, ctx, events, tail);
}
