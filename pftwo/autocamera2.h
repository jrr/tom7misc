// Rewrite of autocamera. Goal is to find the canonical
// memory location that represents the player's x and y
// coordinates (for 1P or 2P).
//
// Autocamera v1 had the following assumptions, among others:
//  - The player is represented by a single sprite (of course in
//    practice there is usually a cluster of sprites, but we assumed
//    that one was at a fixed offset from the memory location)
//  - This sprite has a fixed position in the OAM table
//  - The sprite is drawn every frame.
//
// Each of these is violated in practice; Contra violates all three.
// The first is because animations sometimes have frame-dependent
// offsets, or even use a different number of sprites on each
// animation frame. The second is violated due to tricks used to work
// around NES sprite limitations (8 sprites per scanline), like
// rotating the sprite table on each frame (which produces flicker
// rather than blanking). The third is violated to e.g. show
// an invincibility state by flickering the player deliberately.
//
// For v2 we make only these assumptions:
//  - There exists some pair of memory locations xloc,yloc that
//    contain the authoritative x and y position of the player.
//    Changing this position simply warps the player.
//  - Pressing LEFT and RIGHT "usually" decrement and increment the
//    x coordinate, which is an unsigned 8-bit quantity.
//  - A cluster of at least one sprite is drawn "near" the x,y
//    coordinate on "many" frames. (But the sprite slots and tiles
//    may change arbitrarily.)
//
// The approach is as follows:
//  - As before, first find x coordinates. We can just look at a sample
//    of frames, and find any location whose value is close to a sprite's
//    x coordinate. Since we don't know that the sprite is always drawn,
//    and we don't assume that the x coordinate matches exactly, we have
//    to "score" these locations and use some thresholds.
//      - (But note that the sprites actually drawn tend to lag the
//         memory location by a single frame.)
//  - Next, for these candidates, we can modify the memory location and
//    test that the sprite is drawn at a new location. 
//      - Here, we'll compare the sprites drawn at various x locations.
//        We assume that the x coordinate is not a factor in choosing
//        whether the sprite is drawn (flickering) and the animation
//        frame chosen, so we're expecting some sprite cluster to move
//        around at a fixed offset from the memory location. We can
//        include the y coordinate of the sprite in this test, because
//        we know we're not changing the y coordinate, since xloc != yloc.
//
// So actually, a simplification of this is, for many frame samples:
//    For each candidate memory location xcand:
//      For each enabled sprite at sx,sy:
//        Compute xoff = (sx - *xcand)
//        If xoff is "small",
//        For a number of random positions xpos:
//          set *xcand = xpos,
//          execute a frame,   [1]
//          read sx', sy', the new sprite position
//          Compute xoff' = (sx' - xpos)
//          Compute yoff' = (sy' - sy)
//          if |(xoff - xoff', yoff')| is "small", then this is good. [2]
//
// [1] Maybe it needs to be 2 frames, since sprite position usually
//     lags? And if more than one frame, then we need to worry about
//     how a different position could now cause different game behavior.
//
// [2] Note that there may be sprites that are initially close to xpos
//     (especially since we only consider x) but that aren't part of
//     the player cluster. So we probably should not penalize this
//     case, as long as there is one sprite that does move. Could maybe
//     just be min()?
//
// We then perform this same procedure to find y coordinates that affect
// the y position of sprite clusters.
//
// Now we have some frames on which we can move some sprites
// (clusters?) in x and y directions. Next, we want to pair the x,y
// coordinates so that we can control a single sprite cluster's full
// position.
//
// For each surviving pair xcand, ycand
// take the intersection of frames on which they were ...
//
// Actually, what about just trying to find the x,y coordinates in
// a single pass? It makes that algorithm a bit more complex (and much
// more expensive), but is probably easier than this multiphase approach.
// In particular I'm worried that the sprite clusters might not exactly
// agree, or the active frames might not agree, etc. Try again:
//
// For many frame samples:
//   For each active sprite at sx,sy:
//   (Note: skip cases where sx is too close to sy?)
//   Take xcands and ycands as the set of memory locations where *cand
//    is close to (say within 16 pixels) sx/sy.
//   (If we skip cases where sx and sy are too close, then xcands and
//    ycands would be disjoint.)
//   Now, perform the same procedure above; for some pair xcand,ycand,
//   set them to random other values, and measure the offset for this
//   sprite. If it is small and consistent with the original offset,
//   vote for that xcand,ycand pair.
//   (It may be more efficient to first filter out the individual
//    candidates if they don't move the sprite, since most(?) won't.
//    But this is just an optimization.)
//
// OK, that's a simple way to find x,y memory locations that seem to
// control a sprite cluster. But we haven't established that this is
// the player (and for example haven't done anything to distinguish
// player 1 and player 2).
//
// As a totally separate matter, try to figure out x memory locations
// that can be modified by pressing left and right. This can be the
// same algorithm as in Autocamera 1, but performed on memory locations
// rather than sprite slots.
//
// Next, we can intersect this list with the memory locations that
// control sprite clusters. We have only the x coordinate, so it's
// possible that we are left with multiple different xloc,yloc pairs
// for any given memory location that we seem to control. (This would
// happen if for example there's always a shadow sprite that follows
// the player's x coordinate, but keeps y = 220 or something.) I think
// that such an object is probably covered by the consequentiality
// filter (since presumably the game derives the shadow from the
// player's position). So, just take all x,y pairs that have an x
// that we seem to control.
//
// Next, we probably still need to filter for consequentiality as
// in autocamera v1 (the very first step checks that we can move
// the sprite by changing the memory, but it's possible that the
// value is actually derived from the "authoritative" value, stored
// elsewhere, like on a 1-frame lag.

#ifndef __AUTOCAMERA2_H
#define __AUTOCAMERA2_H

#include "pftwo.h"

#include <functional>
#include <memory>
#include <string>
#include <vector>
#include <functional>

#include "../fceulib/emulator.h"
#include "../cc-lib/arcfour.h"
#include "random-pool.h"
#include "emulator-pool.h"

// Focus is on quality and debuggability, not performance.
struct AutoCamera2 {
  // Creates some private emulator instances that it can reuse.
  explicit AutoCamera2(const string &game);
  ~AutoCamera2();

  struct Linkage {
    int xloc = 0, yloc = 0;
    // Larger is better.
    float score = 0.0f;
    Linkage(int xloc, int yloc, float score) :
      xloc(xloc), yloc(yloc), score(score) {}
  };
  
  // For one uncompressed save state sample, find linkages between
  // pair of memory locations and a nonempty cluster of sprites
  // on-screen.
  //
  // Linkages are output in order by descending score; only the
  // best linkages are returned.
  //
  // If report is non-empty (e.g. default ctor) then it is called
  // periodically with progress.
  vector<Linkage> FindLinkages(const vector<uint8> &save,
			       const std::function<void(string)> &report);

  // Take the results of several calls to FindLinkages and merge them
  // by summing the scores.
  static vector<Linkage> MergeLinkages(
      const vector<vector<Linkage>> &samples);

  // Find memory locations that may be an x coordinate controlled by
  // the player. We assume that the player moves left and right with
  // the corresponding buttons on the controller.
  // In many situations the player will not be in control (e.g. while
  // taking damage; cutscenes, etc.). This routine does not attempt
  // to explicitly detect or avoid this case; it should just be called
  // for many different save states.
  struct XLoc {
    int xloc = 0;
    float score = 0.0f;
    XLoc(int xloc, float score) : xloc(xloc), score(score) {}
  };
  vector<XLoc> FindXLocs(const vector<uint8> &save,
			 bool player_two,
			 const std::function<void(string)> &report);

  // Merge the output of several calls of FindXLocs by summing scores.
  static vector<XLoc> MergeXLocs(const vector<vector<XLoc>> &samples);

private:
  RandomPool random_pool;
  EmulatorPool emu_pool;
};
  
#endif
