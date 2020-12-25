#ifndef _LOWERCASE_TIMER_H
#define _LOWERCASE_TIMER_H

#include <windows.h>

// Computes time elapsed in ms. Windows only.
// TODO: POSIX version
struct Timer {
  Timer() {
    LARGE_INTEGER fq;
    QueryPerformanceFrequency(&fq);
    ticks_to_ms = 1000.0 / fq.QuadPart;
    starttime.QuadPart = 0;
    Start();
  }

  void Start() {
    QueryPerformanceCounter(&starttime);
  }

  void Stop() {
    if (starttime.QuadPart == 0) {
      // Already stopped.
    } else {
      LARGE_INTEGER stoptime;
      QueryPerformanceCounter(&stoptime);
      saved_ms += MillisecondSpan(starttime, stoptime);
      // Mark as stopped.
      starttime.QuadPart = 0;
    }
  }

  // The total milliseconds for the timer is:
  //  - saved_ms: any time saved from previous calls to Stop()
  //  - if running, the current time since we last started.
  double MS() {
    if (starttime.QuadPart == 0) {
      // not running
      return saved_ms;
    } else {
      // Running. Get the current count but keep running.
      LARGE_INTEGER nowtime;
      QueryPerformanceCounter(&nowtime);
      return saved_ms + MillisecondSpan(starttime, nowtime);
    }
  }

 private:
  double MillisecondSpan(LARGE_INTEGER begin, LARGE_INTEGER end) {
    return (end.QuadPart - begin.QuadPart) * ticks_to_ms;
  }

  double ticks_to_ms = 0.0;
  // LARGE_INTEGER is just int64 with support (union) for getting parts
  // out. Use .QuadPart for the 64-bit quantity.
  LARGE_INTEGER starttime;
  double saved_ms = 0.0;
  bool stopped = false;
};

#endif
