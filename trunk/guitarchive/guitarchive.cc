// Code for cleaning and working with ASCII guitar tab files, e.g. from OLGA.

#include "guitarchive.h"

#include <algorithm>
#include <string>
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <string_view>
#include <unordered_set>

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
  "d:\\temp\\tabs",
};


void Guitarchive::AddAllFilesRec(const string &dir, vector<string> *all_files) {
  for (const string &f : Util::ListFiles(dir)) {
    const string filename = Util::dirplus(dir, f);
    // printf("%s + %s = %s\n", dir.c_str(), f.c_str(), filename.c_str());
    if (Util::isdir(filename)) {
      // printf("Dir: [%s]\n", filename.c_str());
      AddAllFilesRec(filename, all_files);
    } else {
      if (!filename.empty() &&
	  // Should perhaps just delete emacs backups..?
	  filename[0] != '#' &&
	  filename[filename.size() - 1] != '~') {
	all_files->push_back(filename);
      }
    }
  }
}




bool Guitarchive::TryStripSuffix(string_view suffix,
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


string Guitarchive::Frontslash(const string &s) {
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

string Guitarchive::Backslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '/' ? '\\' : c);
  return ret;
}

vector<Entry> Guitarchive::Load(int threads) {

  printf("List files..\n");
  fflush(stdout);

  vector<string> all_filenames;
  for (const char *d : DIRS) {
    Guitarchive::AddAllFilesRec(d, &all_filenames);
  }

  printf("Num files: %lld\nReading..\n", (int64)all_filenames.size());
  fflush(stdout);

  // For a well-formed file, this will stop on the blank line after the
  // headers.
  RE2 normalized_header{"([A-Za-z0-9][^:]+): (.+)\n"};
  
  auto MakeEntry = [&normalized_header](const string &filename,
					const string &contents) {
      Entry entry;
      entry.filename = filename;

      re2::StringPiece cont(contents);
      string key, val;
      while (RE2::Consume(&cont, normalized_header, &key, &val)) {
	if (key == "Title") {
	  entry.title = std::move(val);
	} else if (key == "Artist") {
	  entry.artist = std::move(val);
	} else if (key == "Album") {
	  entry.album = std::move(val);
	} else {
	  entry.headers.emplace_back(std::move(key), std::move(val));
	}
      }

      entry.body = cont.as_string();
      return entry;
    };
  
  vector<Entry> entries =
    ParallelMap(all_filenames,
		[&MakeEntry](const string &filename) {
		  string f = Backslash(filename);
		  return MakeEntry(f, Util::ReadFile(f));
		},
		threads);

  return entries;
}


