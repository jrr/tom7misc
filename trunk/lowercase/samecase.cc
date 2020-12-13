// Mark fonts whose characters for a-z seem to be simple
// transformations (x = ax + b, y = cx + d) of A-Z.

#include <algorithm>
#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <string_view>
#include <unordered_set>
#include <mutex>
#include <unordered_map>

#include "util.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"
#include "threadutil.h"
#include "city/city.h"

#include "ttf.h"
#include "stb_truetype.h"
#include "ttfops.h"

using namespace std;

using uint8 = uint8_t;
using int64 = int64_t;

enum Result {
  NOT_STRUCTURAL,
  NO_TRANSFORM,
  SAME_CASE,
};
  

int main(int argc, char **argv) {

  vector<string> all_filenames = Util::ReadFileToLines("all_fonts.txt");

  std::mutex print_m;

  #if 0
  vector<optional<tuple<double, double, double>>> results =
  ParallelMapi(all_filenames,
	       [&](int idx, const string &filename) {
		 {
		   MutexLock ml(&print_m);
		   printf("Doing %s\n", filename.c_str());
		   fflush(stdout);
		 }

		 TTF ttf{filename};
		 
		 optional<tuple<double, double, double>> same =
		   TTFOps::GetSameCase(ttf);

		 if (idx % 100 == 0) {
		   MutexLock ml(&print_m);
		   printf("Just did %d/%lld\n", idx, (int64)all_filenames.size());
		   fflush(stdout);
		 }
		 return same;
	      }, 20);

  int64 any = 0, lt1 = 0, lt10 = 0;
  for (const auto &p : results) {
    if (p.has_value()) {
      const auto [xscale, yscale, score] = p.value();
      any++;
      if (score < 1.0) lt1++;
      if (score < 10.0) lt10++;
    }
  }

  printf("Of %lld fonts:"
	 "%lld have any transform\n"
	 "%lld score < 10.0\n"
	 "%lld score < 1.0\n",
	 (int64)all_filenames.size(),
	 any,
	 lt10,
	 lt1);
  #endif

  vector<std::pair<double, string>> results =
  ParallelMapi(all_filenames,
	       [&](int idx, const string &filename) {

		 {
		   MutexLock ml(&print_m);
		   printf("Doing %s\n", filename.c_str());
		   fflush(stdout);
		 }

		 TTF ttf{filename};

		 double diff =
		   TTFOps::TotalAlphabetDifference(ttf,
						   200.0f,
						   1000,
						   26.0f * 0.15f);

		 if (true || idx % 100 == 0) {
		   MutexLock ml(&print_m);
		   printf("Just did %d/%lld (diff %.5f) %s\n",
			  idx, (int64)all_filenames.size(), diff,
			  filename.c_str());
		 }
		 return make_pair(diff, Util::LoseWhiteR(filename));
	      }, 20);

  std::sort(results.begin(), results.end(),
	    [](const std::pair<double, string> &a,
	       const std::pair<double, string> &b) {
	      return a.first < b.first;
	    });

  vector<string> outlines;
  outlines.reserve(results.size());
  for (const auto &p : results) {
    outlines.push_back(StringPrintf("%.5f	%s", p.first, p.second.c_str()));
  }

  CHECK(Util::WriteLinesToFile("bitmap_diffs.txt", outlines));

  printf("\nDone!\n");
  
  return 0;
}



