
// For use in polling loops. Keeps track of a "next time to run"
// and tells the caller when it should run. Not thread-safe.

#ifndef __TEMPO_PERIODICALLY_H
#define __TEMPO_PERIODICALLY_H

#include <cstdint>
#include <time.h>

// TODO: To cc-lib?

struct Periodically {
  Periodically(int seconds) : seconds(seconds) {
    next_run = time(nullptr);
  }

  // Return true if 'seconds' has elapsed since the last run.
  // If this function returns true, we assume the caller does
  // the associated action now (and so move the next run time
  // forward).
  bool ShouldRun() {
    if (paused) return false;
    const int64_t now = time(nullptr);
    if (now >= next_run) {
      next_run = now + seconds;
      return true;
    }
    return false;
  }

  void Pause() {
    paused = true;
  }

  void Reset() {
    paused = false;
    next_run = time(nullptr) + seconds;
  }

private:
  int seconds = 0;
  int64_t next_run = 0LL;
  bool paused = false;
};

#endif
