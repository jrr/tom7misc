// Code for cleaning and working with ASCII guitar tab files, e.g. from OLGA.

#include <algorithm>
#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <string_view>
#include <unordered_set>
#include <mutex>

#include "util.h"
#include "re2/re2.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"
#include "threadutil.h"
#include "city/city.h"

#include "ttfarchive.h"
#include "stb_truetype.h"

using namespace std;

using uint8 = uint8_t;
using int64 = int64_t;
static constexpr bool DRY_RUN = false;

int main(int argc, char **argv) {

  vector<string> all_filenames;
  for (const char *d : DIRS) {
    Ttfarchive::AddAllFilesRec(d, &all_filenames);
  }
  
  // TODO: TTC (truetype collection) may also work,
  // although we only will load the first font
  // CASE! (was 137342)
  RE2 truetype_re(".*\\.(?:[Tt][Tt][Ff]|[Oo][Tt][Ff])");
  
  printf("Num files: %lld\n", (int64)all_filenames.size());

  int64 start = time(nullptr);
  
  std::mutex out_m;
  int64 not_ttf_filename = 0, cant_read = 0, bad_offset = 0, cant_init = 0;
  std::vector<string> good, bad;
  
  std::mutex bytes_m;
  int64 total_bytes = 0;

  std::mutex contents_m;
  // contents -> filename
  std::unordered_map<uint64, vector<string>> bycontent;
  
  UnParallelApp(all_filenames,
		[&](const string &filename) {
		if (!RE2::FullMatch(filename, truetype_re)) {
		  MutexLock ml(&out_m);
		  not_ttf_filename++;
		  bad.push_back(filename);
		  return;
		}
		
		vector<uint8> ttf_bytes = Util::ReadFileBytes(filename);
		if (ttf_bytes.empty()) {
		  MutexLock ml(&out_m);
		  cant_read++;
		  printf("Can't read: %s\n", filename.c_str());
		  bad.push_back(filename);
		  return;
		}
		
		{
		  MutexLock ml(&bytes_m);
		  total_bytes += ttf_bytes.size();
		}

		const uint64 h = CityHash64((const char*)ttf_bytes.data(),
					    ttf_bytes.size());
		{
		  MutexLock ml(&contents_m);
		  bycontent[h].push_back(filename);
		}
		
		// printf("%s\n", filename.c_str());
		// fflush(stdout);

                #if 1
		stbtt_fontinfo font;
		int offset = stbtt_GetFontOffsetForIndex(ttf_bytes.data(), 0);
		if (offset == -1) {
		  MutexLock ml(&out_m);
		  bad.push_back(filename);
		  bad_offset++;
		  return;
		}

		if (!stbtt_InitFont(&font, ttf_bytes.data(), offset)) {
		  MutexLock ml(&out_m);
		  bad.push_back(filename);
		  cant_init++;
		  return;
		}
		#endif
		// More checks here...
		
		{
		  MutexLock ml(&out_m);
		  good.push_back(filename);
		}
	      }, 16);
  printf("%lld bytes\n"
	 "%lld good and %lld bad\n"
	 "Bad (%lld filename, %lld can't read, %lld offset, %lld can't init)\n"
	 "in %lld sec\n",
	 total_bytes,
	 (int64)good.size(),
	 (int64)bad.size(),
	 not_ttf_filename,
	 cant_read,
	 bad_offset,
	 cant_init,
	 time(nullptr) - start);


  vector<string> ranks = Util::ReadFileToLines("ranks.txt");
  auto Rank = [&ranks](const string &a) {
      for (int i = 0; i < ranks.size(); i++)
	if (!ranks[i].empty() && Util::StartsWith(a, ranks[i]))
	  return i;
	
      LOG(FATAL) << "Need directory in ranks.txt matching " << a;
      return 99999;
    };
  
  auto Preference = [&Rank](const string &a, const string &b) {
      int aa = Rank(a);
      int bb = Rank(b);
      if (aa < bb) return -1;
      if (aa == bb) return 0;
      return 1;
  };

  int deleted = 0;
  printf("Exact duplicates:\n");
  for (const auto &[h, v] : bycontent) {
    if (v.size() > 1) {
      vector<string> prefs;
      for (const string &f : v)
	prefs.push_back(Util::lcase(f));

      std::sort(prefs.begin(), prefs.end(), Preference);

      printf("%llx:\n", h);
      printf("  KEEP %s\n", prefs[0].c_str());
      for (int i = 1; i < prefs.size(); i++) {
	const string &f = prefs[i];
	printf("  DELETE %s\n", f.c_str());
	CHECK(0 == unlink(f.c_str())) << f;
	deleted++;
      }
    }
  }
  printf("Deleted %d duplicate files\n", deleted);
  return 0;
}



