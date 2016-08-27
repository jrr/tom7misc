/* Disambiguation context.
   Used only within animation; keeps track of where action has
   occurred on the grid -- if two actions interfere, then we serialize
   them.
   
   Disambiguation contexts are associated with particular levels and
   shan't be mixed up!
*/

#ifndef __DISAMB_H
#define __DISAMB_H

#include "ptrlist.h"
#include "aevent.h"
#include "level-base.h"

struct Level;

struct Disamb {
  static Disamb *Create(Level *);
  ~Disamb();

  /* sets everything to serial 0 */
  void clear();

  /* affect a location. This might
     cause the serial to increase. Call this
     before creating the associated animation. Returns true if
     the serial did increase. */
  bool affect(int x, int y, Level *l, PtrList<aevent> **& etail);
  bool affecti(int idx, Level *l, PtrList<aevent> **& etail);

  /* should be paired with calls to 'affect' 
     for the squares that these things live in */
  void preaffectplayer(Level *l, PtrList<aevent> **& etail);
  void preaffectbot(int i, Level *l, PtrList<aevent> **& etail);

  void postaffectplayer();
  void postaffectbot(int i);

  void serialup(Level *l, PtrList<aevent> **& etail);

  unsigned int Serial() const { return serial; }
  
  // For debugging
  unsigned int serialat(int x, int y) const { return map[y * w + x]; }
 private:
  /* array of serial numbers. */
  int w, h;
  unsigned int *map; // XXX rename
  
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
  static NullDisamb *Create(Level *);
  ~NullDisamb();

  /* sets everything to serial 0 */
  void clear() {}

  /* affect a location. This might
     cause the serial to increase. Call this
     before creating the associated animation. Returns true if
     the serial did increase. */
  bool affect(int x, int y, Level *l, PtrList<aevent> **& etail) {
    return false;
  }
  bool affecti(int idx, Level *l, PtrList<aevent> **& etail) {
    return false;
  }

  /* should be paired with calls to 'affect' 
     for the squares that these things live in */
  void preaffectplayer(Level *l, PtrList<aevent> **& etail) {}
  void preaffectbot(int i, Level *l, PtrList<aevent> **& etail) {}

  void postaffectplayer() {}
  void postaffectbot(int i) {}

  void serialup(Level *l, PtrList<aevent> **& etail) {}

  unsigned int Serial() const { return 0U; }
  
  // For debugging
  unsigned int serialat(int x, int y) const { return 0U; }
};

#endif
