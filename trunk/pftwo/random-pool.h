
#ifndef __RANDOM_POOL_H
#define __RANDOM_POOL_H

#include <string>
#include <unordered_set>
#include <vector>
#include <mutex>

#include "../cc-lib/arcfour.h"

// Thread-safe collection of random streams. Possibly overkill,
// and absolutely not ready for cryptography!
//
// Several algorithms require generation of random streams which
// are a little bit expensive to initialize. This is a pool of
// them, like EmulatorPool.
struct RandomPool {
  RandomPool(const std::string &init, int initial_size = 3);
  ArcFour *Acquire();

  // Return an ArcFour object to the pool. It must have
  // previously been returned by Acquire.
  void Release(ArcFour *rc);

  // Requires that all ArcFour objects have been released.
  ~RandomPool();
  
 private:
  ArcFour meta;
  ArcFour *CreateNew();
  std::mutex m;
  std::unordered_set<ArcFour *> claimed;
  std::vector<ArcFour *> ready;
};

#endif
