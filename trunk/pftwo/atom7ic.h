#ifndef __ATOM7IC_H
#define __ATOM7IC_H


#include <atomic>

static_assert(ATOMIC_INT_LOCK_FREE == 2,
	      "Integers aren't atomic. :-(");

static_assert(ATOMIC_POINTER_LOCK_FREE == 2,
	      "Pointers aren't atomic. :-(");


// Lockless counter. Should only be used for reporting stats in
// the UI since it uses relaxed memory ordering.
struct Counter {
  inline void IncrementBy(int d) {
    value.fetch_add(d, std::memory_order_relaxed);
  }
  inline void Increment() {
    value.fetch_add(1, std::memory_order_relaxed);
  }
  inline int Get() {
    return value.load(std::memory_order_relaxed);
  }
  std::atomic<int> value{0};
};

#endif
