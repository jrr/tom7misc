// Code for cleaning database of TTFs.

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

struct ShapeFreer {
  // XXX don't need n
  ShapeFreer(stbtt_fontinfo *font, int n, stbtt_vertex *v) : font(font), n(n), v(v) {}
  ShapeFreer() = delete;
  ~ShapeFreer() {
    if (v != nullptr) {
      stbtt_FreeShape(font, v);
    }
  }

private:
  stbtt_fontinfo *font;
  int n;
  stbtt_vertex *v;
};

int main(int argc, char **argv) {

  vector<string> all_filenames;
  for (const char *d : DIRS) {
    Ttfarchive::AddAllFilesRec(d, &all_filenames);
  }
  
  // TODO: TTC (truetype collection) may also work,
  // although we only will load the first font.
  RE2 truetype_re(".*\\.(?:[Tt][Tt][Ff]|[Oo][Tt][Ff])");
  
  printf("Num files: %lld\n", (int64)all_filenames.size());

  int64 start = time(nullptr);
  
  std::mutex out_m;
  std::unordered_map<string, int64> counters;
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
		  counters["filename"]++;
		  bad.push_back(filename);
		  return;
		}
		
		vector<uint8> ttf_bytes = Util::ReadFileBytes(filename);
		if (ttf_bytes.empty()) {
		  MutexLock ml(&out_m);
		  counters["cant_read"]++;
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

		stbtt_fontinfo font;
		int offset = stbtt_GetFontOffsetForIndex(ttf_bytes.data(), 0);
		if (offset == -1) {
		  MutexLock ml(&out_m);
		  bad.push_back(filename);
		  counters["bad_offset"]++;
		  return;
		}

		if (!stbtt_InitFont(&font, ttf_bytes.data(), offset)) {
		  MutexLock ml(&out_m);
		  bad.push_back(filename);
		  counters["cant_init"]++;
		  return;
		}

		// More checks here...

		int ascent, descent;
		stbtt_GetFontVMetrics(&font, &ascent, &descent, nullptr);
		if ((ascent - descent) <= 0) {
		  MutexLock ml(&out_m);
		  counters["bad_vmetrics"]++;
		  bad.push_back(filename);
		  return;
		}

		// Must have all uppercase and lowercase English
		// letters, and can't always be the same.
		// XXX check this code for mistaken uses of 'uc' instead of 'un'
		bool all_match = true;
		for (int uc = 'A'; uc <= 'Z'; uc++) {
		  const int lc = uc | 32;
		  
		  stbtt_vertex *uvertices = nullptr;
		  stbtt_vertex *lvertices = nullptr;
		  const int un = stbtt_GetCodepointShape(&font, uc, &uvertices);
		  ShapeFreer free_uv(&font, un, uvertices);
		  const int ln = stbtt_GetCodepointShape(&font, lc, &lvertices);
		  ShapeFreer free_lv(&font, ln, lvertices);
		  if (un == 0 || ln == 0) {
		    MutexLock ml(&out_m);
		    counters["missing_letters"]++;
		    bad.push_back(filename);
		    return;
		  }

		  auto HasCubic = [](int n, stbtt_vertex *vs) {
		      for (int i = 0; i < n; i++)
			if (vs[i].type == STBTT_vcubic)
			  return true;
		      return false;
		    };
		  if (HasCubic(un, uvertices) ||
		      HasCubic(ln, lvertices)) {
		    MutexLock ml(&out_m);
		    counters["has_cubic"]++;
		    bad.push_back(filename);
		    return;
		  }
		      

		  auto SameShape = [un, ln, uvertices, lvertices]() {
		      if (un != ln) return false;
		      
		      for (int i = 0; i < un; i++) {
			const stbtt_vertex &u = uvertices[i];
			const stbtt_vertex &l = lvertices[i];

			if (l.type != u.type)
			  return false;

			switch (l.type) {
			default:
			case STBTT_vcubic:
			  LOG(FATAL) << "Already checked above";
			  break;
			case STBTT_vcurve:
			  if (l.cx != u.cx ||
			      l.cy != u.cy ||
			      l.x != u.x ||
			      l.y != u.y) {
			    return false;
			  }
			  break;
			case STBTT_vmove:
			case STBTT_vline:
			  if (l.x != u.x ||
			      l.y != u.y) {
			    return false;
			  }
			  break;
			}
		      }

		      return true;
		    };
		  
		  // See if they still match...
		  if (all_match)
		    if (!SameShape())
		      all_match = false;

		}

		if (all_match) {
		  MutexLock ml(&out_m);
		  counters["all_match"]++;
		  bad.push_back(filename);
		  return;
		}
		
		{
		  MutexLock ml(&out_m);
		  good.push_back(filename);
		}
	      }, 16);

  printf("%lld bytes in %lld sec\n"
	 "%lld good and %lld bad\n"
	 "Bad:\n",
	 total_bytes, time(nullptr) - start,
	 (int64)good.size(),
	 (int64)bad.size());
  for (const auto &[name, count] : counters) {
    printf("  %s: %lld\n", name.c_str(), count);
  }

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

  Util::WriteLinesToFile("all_fonts.txt", good);
  return 0;
}



