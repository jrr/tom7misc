
#include "ttfarchive.h"

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

using namespace std;

void Ttfarchive::AddAllFilesRec(const string &dir, vector<string> *all_files) {
  for (const string &f : Util::ListFiles(dir)) {
    const string filename = Util::dirplus(dir, f);
    // printf("%s + %s = %s\n", dir.c_str(), f.c_str(), filename.c_str());
    if (Util::isdir(filename)) {
      // printf("Dir: [%s]\n", filename.c_str());
      AddAllFilesRec(filename, all_files);
    } else {
      if (!filename.empty() &&
	  // Should probably delete emacs backups..?
	  filename[filename.size() - 1] != '#' &&
	  filename[filename.size() - 1] != '~') {
	all_files->push_back(filename);
      }
    }
  }
}

string Ttfarchive::Frontslash(const string &s) {
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

string Ttfarchive::Backslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '/' ? '\\' : c);
  return ret;
}



