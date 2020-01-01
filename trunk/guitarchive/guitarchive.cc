
// Code for cleaning and working with ASCII guitar tab files, e.g. from OLGA.

#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <string_view>

#include "util.h"
#include "re2/re2.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "randutil.h"
#include "arcfour.h"
#include "threadutil.h"
#include "edit-distance.h"

#include "headers.h"

using namespace std;
// using namespace re2;

static constexpr const char *DIRS[] = {
  "c:\\code\\electron-guitar\\tabscrape\\tabs",
  "d:\\temp\\olga",
  "d:\\temp\\tabs",
};

struct Parser {

  bool GetMetadata(const string &filename,
		   const string &contents,
		   string *title,
		   string *artist) {
    re2::StringPiece cont(contents);
    if (RE2::Consume(&cont, explicit_meta, title, artist)) {
      return true;
    }

    // XXXX
    return false;
  }

  // Preferred explicit form at the beginning of the file.
  RE2 explicit_meta{"Title: (.*)\n"
                    "Artist: (.*)\n"};
};


static void AddAllFilesRec(const string &dir, vector<string> *all_files) {
  for (const string &f : Util::ListFiles(dir)) {
    const string filename = Util::dirplus(dir, f);
    // printf("%s + %s = %s\n", dir.c_str(), f.c_str(), filename.c_str());
    if (Util::isdir(filename)) {
      // printf("Dir: [%s]\n", filename.c_str());
      AddAllFilesRec(filename, all_files);
    } else {
      if (!filename.empty() &&
	  // Should perhaps just delete emacs backups..?
	  filename[filename.size() - 1] != '~') {
	all_files->push_back(filename);
      }
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

int CountSpaces(const string &line) {
  for (int i = 0; i < (int)line.size(); i++) {
    if (line[i] != ' ') return i;
  }
  return (int)line.size();
}

// Remove trailing spaces from lines, and blank lines from bottom.
// End every line (including the last) with a newline.
string StripTrailing(const string &contents) {
  vector<string> lines = Util::SplitToLines(contents);
  int min_spaces = 999;
  for (string &line : lines) {
    while (!line.empty() && line[line.size() - 1] == ' ') {
      line.resize(line.size() - 1);
    }
    if (!line.empty()) {
      min_spaces = std::min(min_spaces, CountSpaces(line));
    }
  }

  // And remove empty lines at the end.
  while (!lines.empty() && lines[lines.size() - 1].empty()) {
    lines.resize(lines.size() - 1);
  }
  // TODO: And beginning...
  
  // Now if every non-empty line starts with space, remove it.
  if (min_spaces > 0) {
    for (string &line : lines) {
      if ((int)line.size() < min_spaces) {
	line = "";
      } else {
	line = line.substr(min_spaces);
      }
    }
  }
    
  string out;
  out.reserve(contents.size());
  for (const string &line : lines) {
    out += line;
    out += "\n";
  }
  return out;
}

bool TryStripSuffix(string_view suffix,
		    string_view *s) {
  // In c++20, can use s->ends_with(suffix).
  auto EndsWith = [](string_view big, string_view little) {
      if (big.size() < little.size()) return false;
      return big.substr(big.size() - little.size()) == little;
    };
  if (EndsWith(*s, suffix)) {
    s->remove_suffix(suffix.length());
    return true;
  }
  return false;
}

string StripFooter(const string &contents) {
  const string_view footer =
    "<br>\n"
    "Guitartabs.cc &copy; 2019</pre></pre>\n"
    "</body></html>\n"
    "<script language=\"javascript\">\n"
    "\twindow.print();\n"
    "</script>"sv;

  string_view cont(contents);
  if (TryStripSuffix(footer, &cont)) {
    return (string)cont;
  }
  return contents;
}

string RewriteHeader(const string &contents) {
  static LazyRE2 gt1 = {
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n"
    "<html><head>\n"
    "<title>[^<]*</title>\n"
    "</head><body style='font-family: Courier New; font-size:11px'>\n"
    "<pre><h2 style='color:#000000'><a style='color:#000000' "
    "href='[^']*'>([^<]*)</a> "};

  re2::StringPiece cont(contents);
  string artist;
  if (RE2::Consume(&cont, *gt1, &artist)) {
    // Skip '227'
    cont.remove_prefix(1);
    static LazyRE2 gt2 =
      {" <a style='color:#000000' href='[^']*'>([^<]*) Tab</a></h2>\n"};
    string title;
    if (RE2::Consume(&cont, *gt2, &title)) {
      return
	StringPrintf("Title: %s\n"
		     "Artist: %s\n"
		     "%s", title.c_str(), artist.c_str(),
		     cont.ToString().c_str());
    }
  }
  return contents;
}


string StripHeader(const string &contents) {
  for (const char *hdr : Headers::headers) {
    auto pos = contents.find(hdr);
    if (pos != string::npos) {
      string ret = contents.substr(0, pos);
      ret += contents.substr(pos + string(hdr).size(), string::npos);
      return ret;
    }
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
		14);
  if (false) {
    for (const auto &p : files) {
      if (p.second.empty()) {
	printf("Remove empty %s...\n", p.first.c_str());
	Util::remove(p.first);
      }
    }
  }

  {
    int64 total_bytes = 0;
    int non_ascii = 0;
    for (const auto &p : files) {
      total_bytes += p.second.size();
      if (HasNonAscii(p.second)) {
	non_ascii++;
	// printf("Non-ascii: %s\n", p.first.c_str());
      }
    }
    printf("Total bytes: %lld\n", total_bytes);
    printf("Non-ascii files: %d\n", non_ascii);
    fflush(stdout);
  }
  
  {
    std::mutex m;
    int has_marker = 0;
    ParallelApp(files,
		[&m, &has_marker](const pair<string, string> &row) {
		  if (row.second.find("-PLEASE NOTE-") != string::npos ||
		      row.second.find("scholarship, or research") !=
		      string::npos) {
		    MutexLock ml(&m);
		    if (has_marker < 30 ||
			(has_marker % 1000 == 0)) {
		      printf("Marker? %s\n", row.first.c_str());
		    }
		    has_marker++;
		  }
		},
		32);
    printf("May still have markers: %d\n", has_marker);
    fflush(stdout);
  }
  
  
  {
    Parser p;
    std::mutex m;
    int has_meta = 0;
    ParallelApp(files,
		[&p, &m, &has_meta](const pair<string, string> &row) {
		  string title, artist;
		  if (p.GetMetadata(row.first, row.second,
				    &title, &artist)) {
		    MutexLock ml(&m);
		    // printf("[%s] by [%s]\n", title.c_str(), artist.c_str());
		    has_meta++;
		  }
		},
		32);
    printf("Total with metadata: %d\n", has_meta);
    fflush(stdout);
  }

  {
    std::mutex m;
    int changed = 0;
    ParallelApp(files,
		[&m, &changed](const pair<string, string> &row) {
		  string c = StripTrailing(row.second);
		  c = StripCR(c);
		  c = StripHeader(c);
		  // c = StripFooter(c);
		  if (c != row.second) {
		    string filename = Backslash(row.first);
		    CHECK(Util::WriteFile(filename, c)) << filename;
		    /*
		    printf("Rewrite %s:\n"
			   "[%s] to [%s]\n",
			   filename.c_str(),
			   row.second.c_str(), c.c_str());
		    */
		    {
		      MutexLock ml(&m);
		      changed++;
		      if (changed < 10 ||
			  (changed % 1000 == 0)) {
			    printf("[%d] Write [%s] (%d -> %d)\n",
				   changed,
				   filename.c_str(),
				   (int)row.second.size(), (int)c.size());
		      }
		    }
		  }
		},
		30);

    printf("Modified %d files in place.\n", changed);
    fflush(stdout);
  }
  
  printf("Done.\n");
  fflush(stdout);

  // ComputeDistances(files);
  
  return 0;
}
