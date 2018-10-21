
#ifndef __PFTWO_H
#define __PFTWO_H

#include <string>
#include <vector>
#include <map>
#include <cstdint>
#include <memory>
#include <unordered_map>
#include <type_traits>

#include <thread>
#include <mutex>
#include <shared_mutex>

// using namespace std;
using std::string;
using std::vector;
using std::map;
using std::unordered_map;

#include "../fceulib/types.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/threadutil.h"

// Status of a worker. This is basically stuff that the UI can
// display.
enum WorkerStatus : int {
  STATUS_UNKNOWN,
  STATUS_SEARCH,
  STATUS_EXPLORE,
  STATUS_DIE,
  STATUS_TREE,
  STATUS_MARATHON,
};

// TODO: To threadutil, but note that this is C++17.
struct ReadMutexLock {
  explicit ReadMutexLock(std::shared_mutex *m) : m(m) { m->lock_shared(); }
  ~ReadMutexLock() { m->unlock_shared(); }
  std::shared_mutex *m;
};
// Possible to template this over shared_mutex and mutex without
// requiring an argument?
struct WriteMutexLock {
  explicit WriteMutexLock(std::shared_mutex *m) : m(m) { m->lock(); }
  ~WriteMutexLock() { m->unlock(); }
  std::shared_mutex *m;
};

// Read with the mutex that protects it. T must be copyable,
// obviously!
template<class T>
T SharedReadWithLock(std::shared_mutex *m, const T *t) {
  ReadMutexLock ml(m);
  return *t;
}

// e.g. with C = map<int, string>. Third argument is the default.
// Returns "string", not "const string &", to avoid surprises
// about temporary lifetime in a call like
// GetDefault(m, "missing_key", "temporary").
template<class K, class C>
auto GetDefault(const C &container,
		const K &key,
		const decltype(container.find(key)->second) &def) ->
  typename std::remove_reference<
    decltype(container.find(key)->second)>::type {
  auto it = container.find(key);
  if (it == container.end()) return def;
  return it->second;
}

template<class K, class C>
inline bool ContainsKey(const C &container, const K &key) {
  return container.find(key) != container.end();
}

#define NOT_COPYABLE(classname) \
  private: \
  classname(const classname &) = delete; \
  classname &operator =(const classname &) = delete

namespace internal {
template<class... Argtypes>
struct DisjointBitsC;

template<class T, class ...Argtypes>
struct DisjointBitsC<T, Argtypes...> {
  static constexpr bool F(uint64 used, T head, Argtypes... tail) {
    return !(used & head) &&
      DisjointBitsC<Argtypes...>::F(used | head, tail...);
  }
};

template<>
struct DisjointBitsC<> {
  static constexpr bool F(uint64 used_unused) {
    return true;
  }
};
}

// Compile-time check that the arguments don't have any overlapping
// bits; used for bitmasks. Assumes the width is no greater than
// uint64. Intended for static_assert.
template<class... Argtypes>
constexpr bool DisjointBits(Argtypes... a) {
  return internal::DisjointBitsC<Argtypes...>::F(0, a...);
}


#endif
