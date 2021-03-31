
// For numbered files in a directory, copy them to a new directory
// but with consecutive frame numbers (e.g. for VirtualDub).

#include <vector>
#include <string>
#include <algorithm>
#include <cstdint>
#include <memory>

#include "timer.h"

#include "threadutil.h"
#include "base/stringprintf.h"
#include "util.h"
#include "base/logging.h"

using namespace std;

using uint8 = uint8_t;
using uint32 = uint32_t;
using int64 = int64_t;

static string Frontslash(const string &s) {
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

static string Backslash(const string &s) {
  string ret;
  for (const char c : s)
    ret += (c == '/' ? '\\' : c);
  return ret;
}

static void AddAllFiles(const string &dir, vector<string> *all_files) {
  for (const string &f : Util::ListFiles(dir)) {
    const string filename = Util::dirplus(dir, f);
    // printf("%s + %s = %s\n", dir.c_str(), f.c_str(), filename.c_str());
    if (!Util::isdir(filename)) {
      if (!filename.empty() &&
          // Should probably delete emacs backups..?
          filename[filename.size() - 1] != '#' &&
          filename[filename.size() - 1] != '~') {
        all_files->push_back(Backslash(filename));
      }
    }
  }
}

static void Resequence(const string &in_dir, const string &out_dir) {
  CHECK(out_dir.find("\\") == string::npos);
  CHECK(out_dir.find("/") == string::npos);  
  vector<string> files;
  AddAllFiles(in_dir, &files);
  std::sort(files.begin(), files.end(),
            [](const string &a, const string &b) {
              return Util::natural_compare(a, b) < 0;
            });
  printf("%lld files\n", (int64)files.size());
  vector<pair<string, string>> remapped;
  remapped.reserve(files.size());
  for (int i = 0; i < files.size(); i++) {
    remapped.emplace_back(files[i],
                          StringPrintf("%s\\frame%d.png", out_dir.c_str(), i));
  }
  printf("Copying...\n");
  ParallelApp(remapped,
              [](const std::pair<string, string> &fs) {
                const auto &[infile, outfile] = fs;
                vector<uint8> bytes = Util::ReadFileBytes(infile);
                CHECK(Util::WriteFileBytes(outfile, bytes));
              }, 16);
  printf("Done.\n");
}

int main(int argc, char **argv) {
  CHECK(argc == 3) << "./resequence.exe in_dir out_dir";
  Resequence(argv[1], argv[2]);
  
  return 0;
}
