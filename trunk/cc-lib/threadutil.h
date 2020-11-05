
#ifndef _CC_LIB_THREADUTIL_H
#define _CC_LIB_THREADUTIL_H

#include <vector>
#include <thread>
#include <mutex>
#include <functional>
#include <cstdint>

#if __cplusplus >= 201703L
// shared_mutex only available in C++17 and later.
# include <shared_mutex>
#endif


struct MutexLock {
  explicit MutexLock(std::mutex *m) : m(m) { m->lock(); }
  ~MutexLock() { m->unlock(); }
  std::mutex *m;
};

// Read with the mutex that protects it. T must be copyable,
// obviously!
template<class T>
T ReadWithLock(std::mutex *m, const T *t) {
  MutexLock ml(m);
  return *t;
}

// Write with the mutex that protects it. T must be copyable.
template<class T>
void WriteWithLock(std::mutex *m, T *t, const T &val) {
  MutexLock ml(m);
  *t = val;
}

#if __cplusplus >= 201703L
// Overloads for shared_mutex, if available.
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
T ReadWithLock(std::shared_mutex *m, const T *t) {
  ReadMutexLock ml(m);
  return *t;
}

// Write with the mutex that protects it. T must be copyable.
template<class T>
void WriteWithLock(std::shared_mutex *m, T *t, const T &val) {
  WriteMutexLock ml(m);
  *t = val;
}
#endif

// TODO: A thing that comes up often is where we want to accumulate a
// sum (maybe on many variables) over an array in parallel. The
// typical way to do this involves sharing a mutex to protect some
// variable and doing +=. A nice utility in here would fold over a
// std::tuple of integral types or something like that, by keeping a
// tuple per thread and only adding them at the end. Would save a
// lot of synchronization.

// TODO: Could have an optimism parameter for most of these that
// causes each thread to grab up to N work items (using num_threads as
// a stride) before starting to synchronize for indices. If each item
// takes the same amount of time, this just has less overhead.

// Res should be a small copyable type, like a pair of ints.
// add should be commutative and associative, with zero as its zero value.
// f is applied to every int in [0, num - 1] and Res*, returning void.
// It has exclusive access to res. Typical would be to += into it.
template<class Res, class Add, class F>
Res ParallelAccumulate(int64_t num,
		       Res zero,
		       const Add &add,
		       const F &f,
		       int max_concurrency) {
  if (max_concurrency > num) max_concurrency = num;
  // Need at least one thread for correctness.
  int num_threads = std::max(max_concurrency, 1);

  std::mutex index_m;
  // First num_threads indices are assigned from the start
  int64_t next_index = num_threads;

  // Each thread gets its own accumulator so there's no need to
  // synchronize access.
  std::vector<Res> accs(num_threads, zero);
  
  auto th = [&accs,
	     &index_m, &next_index, num, &f](int thread_num) {
    // PERF consider creating the accumulator in the thread as
    // a local, for numa etc.?
    int my_idx = thread_num;
    Res *my_acc = &accs[thread_num];
    do {
      // Do work, not holding any locks.
      (void)f(my_idx, my_acc);

      // Get next index, if any.
      index_m.lock();
      if (next_index == num) {
	// All done. Don't increment counter so that other threads can
	// notice this too.
	index_m.unlock();
	return;
      }
      my_idx = next_index++;
      index_m.unlock();
    } while (true);
  };

  std::vector<std::thread> threads;
  threads.reserve(num_threads);
  for (int i = 0; i < num_threads; i++) {
    threads.emplace_back(th, i);
  }

  // Wait for each thread to finish, and accumulate the thread-local
  // values into the final one as we do.
  Res res = zero;
  for (int i = 0; i < num_threads; i++) {
    threads[i].join();
    res = add(res, accs[i]);
  }
  return res;
}

// TODO: Easy to have version of the above that applies to each
// element of a vector, etc.

// Do progress meter.
// It should be thread safe and have a way for a thread to register a sub-meter.

// Run the function f(idx, item) for each item in the vector in up to
// max_concurrency parallel threads. The caller must of course
// synchronize any accesses to shared data structures. Return value of
// function is ignored.
//
// TODO: Implement this in terms of ParallelComp
template<class T, class F>
void ParallelAppi(const std::vector<T> &vec, 
		  const F &f,
		  int max_concurrency) {
  // TODO: XXX This cast may really be unsafe, since these vectors
  // could exceed 32 bit ints in practice.
  max_concurrency = std::min((int)vec.size(), max_concurrency);
  // Need at least one thread for correctness.
  max_concurrency = std::max(max_concurrency, 1);
  std::mutex index_m;
  int next_index = 0;
  
  // Thread applies f repeatedly until there are no more indices.
  // PERF: Can just start each thread knowing its start index, and avoid
  // the synchronization overhead at startup.
  auto th = [&index_m, &next_index, &vec, &f]() {
    for (;;) {
      index_m.lock();
      if (next_index == (int64_t)vec.size()) {
	// All done. Don't increment counter so that other threads can
	// notice this too.
	index_m.unlock();
	return;
      }
      int my_index = next_index++;
      index_m.unlock();

      // Do work, not holding mutex.
      (void)f(my_index, vec[my_index]);
    }
  };

  std::vector<std::thread> threads;
  threads.reserve(max_concurrency);
  for (int i = 0; i < max_concurrency; i++) {
    threads.emplace_back(th);
  }
  // Now just wait for them all to finish.
  for (std::thread &t : threads) t.join();
}

// Same, but the typical case that the index is not needed.
template<class T, class F>
void ParallelApp(const std::vector<T> &vec, 
		 const F &f,
		 int max_concurrency) {
  auto ff = [&f](int i_unused, const T &arg) { return f(arg); };
  ParallelAppi(vec, ff, max_concurrency);
}

// Drop-in serial replacement for debugging, etc.
template<class T, class F>
void UnParallelApp(const std::vector<T> &vec, 
		   const F &f,
		   int max_concurrency) {
  for (const auto &t : vec) f(t);
}

// Parallel comprehension. Runs f on 0...(num-1).
// Actually is comprehension the right name for this given that it
// doesn't return anything? XXX
template<class F>
void ParallelComp(int num,
		  const F &f,
		  int max_concurrency) {
  max_concurrency = std::min(num, max_concurrency);
  // Need at least one thread for correctness.
  max_concurrency = std::max(max_concurrency, 1);
  std::mutex index_m;
  int next_index = 0;

  // Thread applies f repeatedly until there are no more indices.
  // PERF: Can just start each thread knowing its start index, and avoid
  // the synchronization overhead at startup.
  auto th = [&index_m, &next_index, num, &f]() {
    for (;;) {
      index_m.lock();
      if (next_index == num) {
	// All done. Don't increment counter so that other threads can
	// notice this too.
	index_m.unlock();
	return;
      }
      int my_index = next_index++;
      index_m.unlock();

      // Do work, not holding mutex.
      (void)f(my_index);
    }
  };

  std::vector<std::thread> threads;
  threads.reserve(max_concurrency);
  for (int i = 0; i < max_concurrency; i++) {
    threads.emplace_back(th);
  }
  // Now just wait for them all to finish.
  for (std::thread &t : threads) t.join();
}

// Drop-in serial replacement for debugging, etc.
template<class F>
void UnParallelComp(int num, const F &f, int max_concurrency_ignored) {
  for (int i = 0; i < num; i++) (void)f(i);
}

// With f(index, value).
// F needs to be callable (std::function or lambda) and thread safe.
// It returns R, which must have a default constructor, and this will
// only be efficient if it has move semantics as well.
template<class T, class F>
auto ParallelMapi(const std::vector<T> &vec,
		  const F &f,
		  int max_concurrency) ->
      std::vector<decltype(f(0, vec.front()))> {
  using R = decltype(f(0, vec.front()));
  std::vector<R> result;
  result.resize(vec.size());

  // Not sure if C++11 makes thread safety guarantees about
  // vector::operator[], but if we have a data pointer then we can be
  // confident of having one writer to each slot.
  R *data = result.data();
  auto run_write = [data, &f](int idx, const T &arg) {
		     data[idx] = f(idx, arg);
		   };
  ParallelAppi(vec, run_write, max_concurrency);
  return result;
}

// With f(value).
// F needs to be callable (std::function or lambda) and thread safe.
// It returns R, which must have a default constructor, and this will
// only be efficient if it has move semantics as well.
template<class T, class F>
auto ParallelMap(const std::vector<T> &vec,
		 const F &f,
		 int max_concurrency) -> std::vector<decltype(f(vec.front()))> {
  auto ff = [&f](int idx, const T &arg) { return f(arg); };
  return ParallelMapi(vec, ff, max_concurrency);
}

// Drop in replacement for testing, debugging, etc.
template<class T, class F>
auto UnParallelMap(const std::vector<T> &vec,
		   const F &f, int max_concurrency_ignored) ->
  std::vector<decltype(f(vec.front()))> {
  using R = decltype(f(vec.front()));
  std::vector<R> result;
  result.resize(vec.size());

  for (int64_t i = 0; i < (int64_t)vec.size(); i++) {
    result[i] = f(vec[i]);
  }

  return result;
}

// When going out of scope, wait for the given thread.
struct ThreadJoiner {
  explicit ThreadJoiner(std::thread *t) : t(t) {}
  ~ThreadJoiner() {
    t->join();
  }
  std::thread *t;
};

// Calls f(x, y) for each 0 <= x < num1 and 0 <= y < num2,
// in up to max_concurrency parallel threads.
template<class F>
void ParallelComp2D(int num1, int num2,
		    const F &f,
		    int max_concurrency) {
  const int total_num = num1 * num2;
  ParallelComp(total_num,
	       [&f, num2](int x) {
		 const int x2 = x % num2;
		 const int x1 = x / num2;
		 f(x1, x2);
	       },
	       max_concurrency);
}

template<class F>
void ParallelComp3D(int num1, int num2, int num3,
		    const F &f,
		    int max_concurrency) {
  const int total_num = num1 * num2 * num3;
  ParallelComp(total_num,
	       [&f, num2, num3](int x) {
		 const int x3 = x % num3;
		 const int xx = x / num3;
		 const int x2 = xx % num2;
		 const int x1 = xx / num2;
		 f(x1, x2, x3);
	       },
	       max_concurrency);
}

// Manages running up to X asynchronous tasks in separate threads. This
// is intended for use in situations like compressing and writing a
// bunch of frames of a movie out to disk. There's substantial parallelism
// opportunity, but it can bad if we eaglery generate the frames because
// they might fill the entire memory. This automatically throttles once
// the specified level of parallelism is reached, by running further calls
// synchronously.
// 
// The threads are started for each asynchronous Run (no thread pool),
// making this kinda high-overhead but easy to manage. It at least
// cleans up after itself.
struct Asynchronously {
  explicit Asynchronously(int max_threads) : threads_active(0),
					     max_threads(max_threads) {}

  // Run the function asynchronously if we haven't exceeded the maximum
  // number of threads. Otherwise, run it in this thread and don't
  // return until it's done.
  void Run(std::function<void()> f) {
    m.lock();
    if (threads_active < max_threads) {
      threads_active++;
      m.unlock();
      std::thread t{[this, f]() {
	  f();
	  MutexLock ml(&this->m);
	  threads_active--;
	}};
      t.detach();
      
    } else {
      m.unlock();
      // Run synchronously.
      f();
    }
  }

  // Wait until all threads have finished.
  ~Asynchronously() {
    for (;;) {
      MutexLock ml(&m);
      if (threads_active == 0) {
	return;
      }
    }
  }

 private:
  std::mutex m;
  int threads_active;
  const int max_threads;
};

#endif
