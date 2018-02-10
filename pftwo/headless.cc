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

      constexpr int64 TEN_MINUTES = 10LL * 60LL;
      // Every ten minutes, write FM2 file.
      // We don't bother if running an experment, since these only
      // run for a few hours and in batch (so the checkpoint files
      // get mixed up anyway).
      if (experiment_file.empty() &&
	  elapsed - last_wrote > TEN_MINUTES) {
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

      int64 total_nes_frames = 0LL;
      {
	MutexLock ml(&search->tree_m);
	vector<Worker *> workers = search->WorkersWithLock();
	for (const Worker *w : workers) {
	  total_nes_frames += w->nes_frames.load(std::memory_order_relaxed);
	}
      }
	
      // Save the sum to a single value for benchmarking.
      search->SetApproximateNesFrames(total_nes_frames);
      int64 minutes = elapsed / 60LL;
      int64 hours = minutes / 60LL;
      if (minutes != last_minute) {
	last_minute = minutes;
	const int64 sec = elapsed % 60LL;
	const int64 min = minutes % 60LL;
	string es = experiment_file.empty() ? "" :
	  StringPrintf(" [%s]", experiment_file.c_str());
	printf("%02d:%02d:%02d  %lld NES Frames; %.2fKframes/sec%s\n",
	       (int)hours, (int)min, (int)sec,
	       total_nes_frames,
	       (double)total_nes_frames / ((double)elapsed * 1000.0),
	       es.c_str());
      }

      if (max_nes_frames > 0LL && total_nes_frames > max_nes_frames) {
	if (!experiment_file.empty()) {
	  string f = search->SaveBestMovie(experiment_file);
	  printf("Wrote final experiment results to %s\n", f.c_str());
	}
	return;
      }
    }
  }

  // If positive, then stop after about this many NES frames.
  int64 max_nes_frames = 0LL;
  // And write the final movie here.
  string experiment_file;
  
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

  TreeSearch::Options options;

  // XXX not general-purpose!
  string experiment;
  if (argc >= 2) {
    double d = std::stod((string)argv[1]);
    printf("\n***\nStarting experiment with value %.4f\n***\n\n", d);
    options.frames_stddev = d;
    options.random_seed = (int)(d * 100002.0);
    experiment = StringPrintf("expt-%s", argv[1]);
  }

  {
    TreeSearch search{options};
    search.StartThreads();
    {
      ConsoleThread *console_thread = new ConsoleThread(&search);
      if (!experiment.empty()) {
	console_thread->max_nes_frames = 3600 * 2 * 20000LL;
	console_thread->experiment_file = experiment;
      }
      console_thread->Run();
      delete console_thread;
    }
    search.DestroyThreads();
  }
  
  return 0;
}
