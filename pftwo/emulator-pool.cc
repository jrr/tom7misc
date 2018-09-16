
#include "emulator-pool.h"

#include <string>
#include <unordered_set>
#include <vector>

#include "../cc-lib/threadutil.h"
#include "../fceulib/emulator.h"
#include "../cc-lib/base/logging.h"

using namespace std;

EmulatorPool::EmulatorPool(const string &romfile, int initial_size) :
  romfile(romfile) {
  CHECK(initial_size >= 0);
  // We don't have the lock yet, but exclusive access in the ctor.
  while (initial_size--) {
    ready.push_back(CreateNew());
  }
}

Emulator *EmulatorPool::CreateNew() const {
  Emulator *emu = Emulator::Create(romfile);
  CHECK(emu != nullptr) << "EmulatorPool failed to create: " << romfile;
  return emu;
}

EmulatorPool::~EmulatorPool() {
  MutexLock ml(&sets_m);
  CHECK(claimed.empty());
  for (Emulator *e : ready) delete e;
}

Emulator *EmulatorPool::Acquire() {
  MutexLock ml(&sets_m);

  auto NextOrNew =
    [this]() {
      if (ready.empty())
	return CreateNew();
      Emulator *e = ready.back();
      ready.pop_back();
      return e;
    };

  // PERF: Perhaps release lock for expensive call to Create?
  // Or create more than one?
  Emulator *e = NextOrNew();
  CHECK(claimed.insert(e).second) << "Duplicate " << e;
  return e;
}

void EmulatorPool::Release(Emulator *e) {
  CHECK(e != nullptr);
  
  MutexLock ml(&sets_m);
  CHECK(1 == claimed.erase(e))
    << "Tried to return foreign/unclaimed emulator " << e;
  ready.push_back(e);
}
