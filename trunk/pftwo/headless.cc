#include <algorithm>
#include <vector>
#include <string>
#include <set>
#include <memory>
#include <list>

#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/randutil.h"

#include "atom7ic.h"

#include "weighted-objectives.h"
#include "problem-twoplayer.h"

#include "treesearch.h"

// Note: This is not actually a std::thread -- it's really the main
// thread. SDL really doesn't like being called outside the main
// thread, even if it's exclusive.
struct ConsoleThread {
  explicit ConsoleThread(TreeSearch *search) : search(search) {}

  void Run() {
    const int64 start = time(nullptr);
    int64 last_wrote = 0LL;
    int64 last_minute = 0LL;
    
    for (;;) {
      frame++;
      sleep(1);
      const int64 elapsed = time(nullptr) - start;
      search->SetApproximateSeconds(elapsed);

      constexpr int64 TEN_MINUTES = 60LL; // XXX // 10LL * 60LL;
      // Every ten minutes, write FM2 file.
      // TODO: Superimpose all of the trees at once.
      if (elapsed - last_wrote > TEN_MINUTES) {
	string filename_part = StringPrintf("frame-%lld", frame);
	string filename = search->SaveBestMovie(filename_part);
	// XXX races are possible, and Util::copy does byte-by-byte.
	// Should use posix link?
	(void)Util::remove("latest.fm2");
	if (!Util::copy(filename, "latest.fm2")) {
	  printf("Couldn't copy to latest.fm2?\n");
	}
	last_wrote = elapsed;
      }

      int64 total_steps = 0LL;
      {
	MutexLock ml(&search->tree_m);
	vector<Worker *> workers = search->WorkersWithLock();
	for (const Worker *w : workers) {
	  total_steps += w->nes_frames.load(std::memory_order_relaxed);
	}
      }
	
      // Save the sum to a single value for benchmarking.
      search->SetApproximateNesFrames(total_steps);
      int64 minutes = elapsed / 60LL;
      int64 hours = minutes / 60LL;
      if (minutes != last_minute) {
	last_minute = minutes;
	const int64 sec = elapsed % 60LL;
	const int64 min = minutes % 60LL;
	printf("%02d:%02d:%02d  %lld NES Frames; %.2fKframes/sec\n",
	       (int)hours, (int)min, (int)sec,
	       total_steps,
	       (double)total_steps / ((double)elapsed * 1000.0));
      }
    }
  }

 private:
  TreeSearch *search = nullptr;
  int64 frame = 0LL;
};

int main(int argc, char *argv[]) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif
  
  {
    TreeSearch search;
    search.StartThreads();
    {
      ConsoleThread *console_thread = new ConsoleThread(&search);
      console_thread->Run();
      delete console_thread;
    }
    search.DestroyThreads();
  }
  
  return 0;
}
