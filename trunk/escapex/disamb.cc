#include "disamb.h"
#include "ptrlist.h"
#include "aevent.h"
#include "level.h"

#include <memory>

using namespace std;

using AList = PtrList<aevent>;

/* disambiguation context implementation */
Disamb *Disamb::Create(const Level *l) {
  std::unique_ptr<Disamb> d{new Disamb};
  d->w = l->w;
  d->h = l->h;
  d->serials = (unsigned int *)malloc(d->w * d->h * sizeof(unsigned int));
  if (!d->serials) return nullptr;
  d->clear();
  return d.release();
}

Disamb::~Disamb() {
  free(serials);
}

void Disamb::clear() {
  serial = 1;
  player = 0;
  for (int i = 0; i < LEVEL_MAX_ROBOTS; i++) {
    bots[i] = 0;
  }

  for (int i = 0; i < (w * h); i++) {
    serials[i] = 0;
  }
}

bool Disamb::affect(int x, int y, Level *l, PtrList<aevent> **& etail) {
  return affecti(y * w + x, l, etail);
}

bool Disamb::affecti(int i, Level *l, PtrList<aevent> **& etail) {
  /* was this last updated in this same
     serial? if so, we need to move to
     the next serial. */

  if (serials[i] == serial) {
    serialup(l, etail);
    serials[i] = serial;
    return true;
  } else {
    serials[i] = serial;
    return false;
  }
}

#define DIS_PUSHEVENT(type, var)                \
  aevent *a ## var = new aevent;                \
  *etail = new AList(a ## var, nullptr);        \
  etail = &((*etail)->next);                    \
  a ## var->serial = serial;                    \
  a ## var->t = tag_ ## type;                   \
  type ## _t *var = & (a ## var->u. type);

/* maintain the invariant that in every serial within the list
   etail, there is at least one animation for every active
   entity */
void Disamb::serialup(Level *l, PtrList<aevent> **& etail) {
  //  printf("serialup(old serial = %d)!\n", serial);

  if (serial != player) {
    // printf("  %d (stand player @ %d/%d)\n", player, l->guyx, l->guyy);
    {
      DIS_PUSHEVENT(stand, e);
      e->x = l->guyx;
      e->y = l->guyy;
      e->d = l->guyd;
      e->data = 0;
      e->entt = B_PLAYER;
    }
    player = serial;
  }

  for (int i = 0; i < l->nbots; i++) {
    if (l->bott[i] != B_DELETED &&
        l->bott[i] != B_BOMB_X &&
        bots[i] != serial) {
      {
        DIS_PUSHEVENT(stand, e);
        l->where(l->boti[i], e->x, e->y);
        // printf("  %d stand bot %d (@ %d/%d)\n", bots[i], i, e->x, e->y);
        e->d = l->botd[i];
        e->entt = l->bott[i];
        e->data = l->bota[i];
      }

      bots[i] = serial;
    }
  }

  /* now we can increment the serial */
  serial++;
}

/* XXX should these also do the same check that affecti does? */
void Disamb::preaffectplayer(Level *l, PtrList<aevent> **& etail) {
  if (player == serial) serialup(l, etail);
}

void Disamb::postaffectplayer() {
  player = serial;
}

void Disamb::preaffectbot(int i, Level *l, PtrList<aevent> **& etail) {
  if (bots[i] == serial) serialup(l, etail);
}

void Disamb::postaffectbot(int i) {
  bots[i] = serial;
}
