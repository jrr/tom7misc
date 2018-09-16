
#ifndef __EMULATOR_POOL_H
#define __EMULATOR_POOL_H

#include <string>
#include <unordered_set>
#include <vector>
#include <mutex>

class Emulator;

// Thread-safe collection of Emulator objects for a given game.
//
// TODO PERF: In some workloads, we could often avoid unnecessary
// calls to "LoadUncompressed" by reusing an emulator that's already
// in that state. This interface could have "AcquireWithState" which
// guarantees the emulator to be in that state. We'd need the calls
// to SaveUncompressed to happen through this interface, etc.
struct EmulatorPool {
  EmulatorPool(const std::string &romfile, int initial_size = 3);

  // Emulator in unspecified state (must LoadUncompressed, etc.).
  // Blocks until there's
  // Emulator *BlockingAcquire();

  // Acquire an emulator. Creates a new one if all are currently
  // claimed.
  Emulator *Acquire();

  // Return an emulator to the pool. The emulator must have
  // previously been returned by Acquire.
  void Release(Emulator *emu);

  // Requires that all emulators have been released.
  ~EmulatorPool();

  // TODO: scoped_emulator?
  
 private:
  const std::string romfile;
  Emulator *CreateNew() const;
  std::mutex sets_m;
  std::unordered_set<Emulator *> claimed;
  std::vector<Emulator *> ready;
};

#endif
