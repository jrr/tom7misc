
#include "random-pool.h"

#include <string>
#include <unordered_set>
#include <vector>
#include <cstdint>

#include "../cc-lib/threadutil.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/base/logging.h"

using uint8 = uint8_t;

using namespace std;

RandomPool::RandomPool(const string &init, int initial_size) :
  meta(init) {
  CHECK(initial_size >= 0);
  // We don't have the lock yet, but exclusive access in the ctor.
  while (initial_size--) {
    ready.push_back(CreateNew());
  }
}

ArcFour *RandomPool::CreateNew() {
  vector<uint8> init;
  init.reserve(32);
  for (int i = 0; i < 32; i++) init.push_back(meta.Byte());
  return new ArcFour(init);
}

RandomPool::~RandomPool() {
  MutexLock ml(&m);
  CHECK(claimed.empty());
  for (ArcFour *e : ready) delete e;
}

ArcFour *RandomPool::Acquire() {
  MutexLock ml(&m);

  auto NextOrNew =
    [this]() {
      if (ready.empty())
	return CreateNew();
      ArcFour *e = ready.back();
      ready.pop_back();
      return e;
    };

  // PERF: Perhaps release lock for expensive call to Create?
  // Or create more than one?
  ArcFour *e = NextOrNew();
  CHECK(claimed.insert(e).second) << "Duplicate " << e;
  return e;
}

void RandomPool::Release(ArcFour *e) {
  CHECK(e != nullptr);
  
  MutexLock ml(&m);
  CHECK(1 == claimed.erase(e))
    << "Tried to return foreign/unclaimed ArcFour " << e;
  ready.push_back(e);
}
