// Base constants and enums that are needed by level and
// other files.
//
// Might make sense to move more stuff in here? Level is pretty
// monstrous.

#ifndef __LEVEL_BASE_H
#define __LEVEL_BASE_H

#define LEVELMAGIC "ESXL"

#define LEVEL_MAX_HEIGHT 100
#define LEVEL_MAX_WIDTH  100
#define LEVEL_MAX_AREA   2048
#define LEVEL_MAX_ROBOTS 15
#define LEVEL_BOMB_MAX_TIMER 10

using dir = int;

enum bot {
  B_BROKEN,
  B_DALEK,
  B_HUGBOT,

  /* no sleeping broken, since it would be
     identical to regular broken. */
  B_DALEK_ASLEEP,
  B_HUGBOT_ASLEEP,

  /* BOMB_n is a bomb with max timer n. 
     the bot data indicates the current
     timer value. */
  /* explodes immediately after being pushed */
  B_BOMB_0,
  /* ... not named ... */
  B_BOMB_MAX = B_BOMB_0 + LEVEL_BOMB_MAX_TIMER,

  NUM_ROBOTS,

  /* can't place on map, but is used to identify
     the type of an entity in general */
  B_PLAYER = -1,
  /* deleted bots arise from destruction. They
     are invisible and inert. We can't rearrange
     the bot list because their indices are used
     in an essential way as identities. */
  B_DELETED = -2,

  /* exploded bomb; becomes deleted next turn */
  B_BOMB_X = -3,
};

#endif
