/* Disambiguation context.
   Used only within animation; keeps track of where action has
   occurred on the grid -- if two actions interfere, then we serialize
   them.

   Disambiguation contexts are associated with particular levels and
   shan't be mixed up!
*/

#ifndef _ESCAPE_DISAMB_H
#define _ESCAPE_DISAMB_H

#include "ptrlist.h"
#include "aevent.h"
#include "level-base.h"

struct Level;

struct Disamb {
  // Level is just used to initialize the size; no alias is retained.
  static Disamb *Create(const Level *l);
  ~Disamb();

  /* sets everything to serial 0 */
  void clear();

  /* affect a location. This might
     cause the serial to increase. Call this
     before creating the associated animation. Returns true if
     the serial did increase. */
  bool affect(int x, int y, Level *l, PtrList<AEvent> **& etail);
  bool affecti(int idx, Level *l, PtrList<AEvent> **& etail);

  /* should be paired with calls to 'affect'
     for the squares that these things live in */
  void preaffectplayer(Level *l, PtrList<AEvent> **& etail);
  void preaffectbot(int i, Level *l, PtrList<AEvent> **& etail);

  void postaffectplayer();
  void postaffectbot(int i);

  void serialup(Level *l, PtrList<AEvent> **& etail);

  unsigned int Serial() const { return serial; }

  // For debugging
  unsigned int SerialAt(int x, int y) const { return serials[y * w + x]; }
 private:
  /* array of serial numbers. */
  int w, h;
  unsigned int *serials;

  /* last serial in which the player was updated */
  unsigned int player;
  /* same, bots */
  unsigned int bots[LEVEL_MAX_ROBOTS];

  /* keep track of current serial */
  unsigned int serial;
};

// Version of the above that does no work. Used to instantiate
// the Move template for the best performance but no animation
// support.
struct NullDisamb {
  // XXX2016, unused?
  static NullDisamb *Create(const Level *l);
  ~NullDisamb() {};

  /* sets everything to serial 0 */
  void clear() {}

  /* affect a location. This might
     cause the serial to increase. Call this
     before creating the associated animation. Returns true if
     the serial did increase. */
  bool affect(int x, int y, Level *l, PtrList<AEvent> **& etail) {
    return false;
  }
  bool affecti(int idx, Level *l, PtrList<AEvent> **& etail) {
    return false;
  }

  /* should be paired with calls to 'affect'
     for the squares that these things live in */
  void preaffectplayer(Level *l, PtrList<AEvent> **& etail) {}
  void preaffectbot(int i, Level *l, PtrList<AEvent> **& etail) {}

  void postaffectplayer() {}
  void postaffectbot(int i) {}

  void serialup(Level *l, PtrList<AEvent> **& etail) {}

  unsigned int Serial() const { return 0U; }

  // For debugging
  unsigned int SerialAt(int x, int y) const { return 0U; }
};

#endif
