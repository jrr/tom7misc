
#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>

#include "util.h"
#include "re2/re2.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"
#include "threadutil.h"
#include "edit-distance.h"

using namespace std;

static constexpr const char *DIRS[] = {
  "c:\\code\\electron-guitar\\tabscrape\\tabs",
  "d:\\temp\\olga",
};

static void AddAllFilesRec(const string &dir, vector<string> *all_files) {
  for (const string &f : Util::ListFiles(dir)) {
    const string filename = Util::dirplus(dir, f);
    // printf("%s + %s = %s\n", dir.c_str(), f.c_str(), filename.c_str());
    if (Util::isdir(filename)) {
      // printf("Dir: [%s]\n", filename.c_str());
      AddAllFilesRec(filename, all_files);
    } else {
      all_files->push_back(filename);
    }
  }
}

static constexpr int SIMILAR_DISTANCE = 200;

void ComputeDistances(const vector<pair<string, string>> &contents) {
  const int n = contents.size();
  vector<int> distances(contents.size() * contents.size(), 0);
  std::mutex m;
  int total_done = 0;
  int64 start = time(nullptr);
  ParallelComp2D(contents.size(), contents.size(),
		 [&contents, n, &distances, &total_done, &m, start](int x,
								    int y) {
		   if (x < y) {
		     int dist = EditDistance::Ukkonen(contents[x].second,
						      contents[y].second,
						      SIMILAR_DISTANCE);
		     /*
		     int dist = std::abs((int)(contents[x].second.size() -
					       contents[y].second.size()));
		     */
		     /*
		     int dist = EditDistance::Distance(contents[x].second,
						       contents[y].second);
		     */
		     
		     distances[y * n + x] = dist;
		     bool pr = false;
		     int td = 0;
		     {
		       MutexLock ml(&m);
		       total_done++;
		       if (total_done % 1000000 == 0) {
			 pr = true;
			 td = total_done;
		       }
		     }
		     if (pr) {
		       int64 secs = time(nullptr) - start;
		       printf("%d = (%.2f%%), %.2f/s\n",
			      td,
			      (td * 100.0) / (n * (n - 1) / 2.0),
			      td / (double)secs);
		       fflush(stdout);
		     }
		   }
		 },
		 42);
  printf("Done computing distances.\n");

  FILE *f = fopen("similar.txt", "wb");
  for (int y = 0; y < n; y++) {
    for (int x = 0; x < y; x++) {
      int dist = distances[y * n + x];
      if (dist < SIMILAR_DISTANCE) {
	fprintf(f,
		"Very similar (%d):\n"
		"  %s\n"
		"  %s\n",
		dist,
		contents[x].first.c_str(),
		contents[y].first.c_str());
      }
    }
  }
  fclose(f);
  
  fflush(stdout);
}

string StripCR(const string &contents) {
  string ret;
  ret.reserve(contents.size());
  for (const char c : contents) {
    if (c == '\r')
      continue;
    ret += c;
  }
  return ret;
}

string StripHeader(const string &contents) {
  /*
  const char *hdr =
    "#----------------------------------PLEASE NOTE-----"
    "----------------------------#\n"
    "#This file is the author's own work and represents "
    "their interpretation of the #\n"
    "#song. You may only use this file for private study, "
    "scholarship, or research. #\n"
    "#----------------------------------------------------"
    "--------------------------##\n";
  */
    const char *hdr =
    "#----------------------------------PLEASE NOTE-----"
    "----------------------------#\n"
    "#This file is the author's own work and represents "
    "their interpretation of the #\n"
    "#song. You may only use this file for private study, "
    "scholarship, or research. #\n"
    "#----------------------------------------------------"
    "--------------------------#\n";
    // Also one version with an additional # on its own.
    

  if (contents.find(hdr) == 0) {
    return contents.substr(string(hdr).size(), string::npos);
  }
  return contents;
}

string Frontslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '\\' ? '/' : c);

  if (ret.find("d:/") == 0) {
    ret[0] = '/';
    ret[1] = 'd';
  } else if (ret.find("c:/") == 0) {
    ret[0] = '/';
    ret[1] = 'c';
  }

  return ret;
}

string Backslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '/' ? '\\' : c);
  return ret;
}

bool HasNonAscii(const string &s) {
  for (const char c : s) {
    switch (c) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      // ok.
      break;
    default:
      if (c < ' ' || c > 127)
	return true;
    }
  }
  return false;
}

int main(int argc, char **argv) {
#if 0
  FILE *f = fopen("d:\\temp\\food.txt", "wb");
  CHECK(f);
  fprintf(f, "uhh\n");
  fclose(f);

  return 0;
#endif
  
  vector<string> all_filenames;
  for (const char *d : DIRS) {
    AddAllFilesRec(d, &all_filenames);
  }

  printf("Num files: %lld\n", (int64)all_filenames.size());

  printf("Read files..\n");
  fflush(stdout);
  vector<pair<string, string>> files =
    ParallelMap(all_filenames,
		[](const string &filename) {
		  return make_pair(filename, Util::ReadFile(filename));
		},
		12);
  if (false) {
    for (const auto &p : files) {
      if (p.second.empty()) {
	printf("Remove empty %s...\n", p.first.c_str());
	Util::remove(p.first);
      }
    }
  }

  int non_ascii = 0;
  for (const auto &p : files) {
    if (HasNonAscii(p.second)) {
      non_ascii++;
      // printf("Non-ascii: %s\n", p.first.c_str());
    }
  }
  printf("Non-ascii files: %d\n", non_ascii);


  std::mutex m;
  int changed = 0;
  ParallelApp(files,
	      [&m, &changed](const pair<string, string> &row) {
		string c = StripHeader(StripCR(row.second));
		if (c != row.second) {
		  string filename = Backslash(row.first);
		  CHECK(Util::WriteFile(filename, c)) << filename;
		  
		  {
		    MutexLock ml(&m);
		    changed++;
		    printf("[%d] Write [%s] (%d -> %d)\n",
			   changed,
			   filename.c_str(),
			   (int)row.second.size(), (int)c.size());
		  }
		}
	      },
	      12);

  printf("Modified %d files in place.\n", changed);
  
  printf("Done.\n");
  fflush(stdout);

  // ComputeDistances(files);
  
  return 0;
}
