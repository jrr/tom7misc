// Local utilities for working with large amounts of chess data.

#ifndef _BIGCHESS_H
#define _BIGCHESS_H

#include <shared_mutex>
#include <cstdint>
#include <thread>
#include <deque>
#include <vector>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/threadutil.h"

using int64 = int64_t;
using uint64 = uint64_t;
using uint8 = uint8_t;

template<class K, class C>
inline bool ContainsKey(const C &container, const K &key) {
  return container.find(key) != container.end();
}

// TODO: This does not parallelize enough. Only one of the numa nodes
// succeeds in getting significant work scheduled. (Maybe actually
// what happens is the N threads are all scheduled onto the numa node
// that's closest to the data?) Would be pretty simple to have K
// distinct deques and round-robin assignment of work to them. When
// one is empty we could only then start work stealing.
//
// An alternate simple experiment would be to grab larger batches
// of work when lots is available. Like if there are N threads, always
// be willing to claim close to (1/N) * pending work units.
template<class W, class F, int64 max_chunk>
struct WorkQueue {
  static_assert(max_chunk > 0LL, "Must take positive chunks!");
  WorkQueue(F f, int num_workers) : f(std::move(f)),
                                    num_workers(num_workers) {
    threads.reserve(num_workers);
    auto th =
      [this]() {
        // This is to avoid having to take the mutex just to
        // increment the done counter. We instead do this when
        // we look for work again.
        int64 local_done = 0LL;
        for (;;) {
          m.lock();
          done += local_done;
          in_progress -= local_done;
          local_done = 0LL;
          if (todo.empty()) {
            if (no_more_work) {
              m.unlock();
              return;
            }

            // PERF Else better to block on a second mutex here.
            m.unlock();
          } else {
            if /* constexpr */ (max_chunk > 1LL) {
              int64 get_up_to = std::max(1LL, pending / this->num_workers);
              std::vector<W> local_work;
              local_work.reserve(get_up_to);
              while (!todo.empty() && get_up_to--) {
                local_work.emplace_back(todo.front());
                todo.pop_front();
                in_progress++;
                pending--;
              }

              m.unlock();

              // Do work without lock.
              for (W &work : local_work)
                this->f(std::move(work));

              local_done += local_work.size();
            } else {
              auto local_work = todo.front();
              todo.pop_front();
              in_progress++;
              pending--;

              m.unlock();

              this->f(std::move(local_work));
              local_done++;
            }
          }
        }
      };

    for (int i = 0; i < num_workers; i++)
      threads.emplace_back(th);
  }

  ~WorkQueue() {
    for (std::thread &t : threads)
      t.join();
  }

  void Add(W work) {
    WriteMutexLock ml(&m);
    pending++;
    todo.push_back(std::move(work));
  }

  void SetNoMoreWork() {
    WriteMutexLock ml(&m);
    no_more_work = true;
  }

  bool IsNoMoreWork() {
    ReadMutexLock ml(&m);
    return done;
  }

  bool StillRunning() {
    ReadMutexLock ml(&m);
    return !no_more_work || pending > 0 || in_progress > 0;
  }

  void Stats(int64 *done, int64 *in_progress, int64 *pending) {
    ReadMutexLock ml(&m);
    *done = this->done;
    *in_progress = this->in_progress;
    *pending = this->pending;
  }

  // Abandon pending work (if possible). Normally the destructor
  // waits for all threads to exit, and a thread only exits if
  // there is no more work for it. Also implies that no more work
  // will be added (a la SetNoMoreWork).
  void Abandon() {
    WriteMutexLock ml(&m);
    todo.clear();
    pending = 0LL;
    no_more_work = true;
  }

private:
  const F f;
  const int num_workers;
  std::shared_mutex m;
  std::vector<std::thread> threads;
  int64 done = 0LL, in_progress = 0LL, pending = 0LL;
  // Also consider list<>?
  std::deque<W> todo;
  // This is set to true when no more work will be added,
  // which signals that the workers can exit.
  bool no_more_work = false;
};


struct PGNTextStream {
  PGNTextStream(const char *filename) {
    f = fopen(filename, "r");
    CHECK(f != nullptr) << "[" << filename << "]";
  }

  bool NextPGN(std::string *pgn_text) {
    if (f == nullptr)
      return false;

    pgn_text->clear();
    pgn_text->reserve(1024);
    int blank_lines = 0;
    int lastc = 0;
    for (;;) {
      const int c = BigGetC();
      if (c == '\n' && lastc == '\n')
        blank_lines++;

      // Don't require double blank lines at end of file.
      if (blank_lines == 2 || c == EOF) {
        num_read++;

        if (c == EOF) {
          fclose(f);
          f = nullptr;
        }

        return true;

      } else {
        *pgn_text += c;
        lastc = c;
      }
    }
  }

  int64 NumRead() const { return num_read; }

  ~PGNTextStream() {
    if (f) {
      fclose(f);
      f = nullptr;
    }
  }

 private:
  int BigGetC() {
    if (data_idx == data.size()) {
      if (feof(f)) return EOF;

      data_idx = 0LL;
      static constexpr int64 MAX_READ_SIZE = 1LL << 24;
      data.resize(MAX_READ_SIZE);
      const size_t bytes_read =
        fread(&data.front(), 1, MAX_READ_SIZE, f);
      if (bytes_read != MAX_READ_SIZE)
        data.resize(bytes_read);

      if (data.empty()) {
        return EOF;
      }
    }
    // Otherwise, data is non-empty and data_idx points at the
    // next element.
    return data[data_idx++];
  }

  FILE *f = nullptr;
  std::vector<uint8> data;
  int64 data_idx = 0LL;
  int64 num_read = 0LL;
};


#endif
