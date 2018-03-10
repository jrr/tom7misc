
#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "threadutil.h"
#include "re2/re2.h"

#include <vector>
#include <string>
#include <map>
#include <unordered_map>

using namespace std;

template<class K, class C>
inline bool ContainsKey(const C &container, const K &key) {
  return container.find(key) != container.end();
}

struct AuthorStats {
  int64 articles = 0;
  int64 citations = 0;
};

template<class F>
static void LocalForEachLine(const string &filename, F f) {
  // printf("Readfiletolines %s\n", filename.c_str());
  vector<string> lines = Util::ReadFileToLines(filename);
  // printf("%d lines\n", (int)lines.size());
  for (int i = 0; i < lines.size(); i++) {
    const string &line = lines[i];
    // if (0 == i % 1000) printf("%d\n", i);
    f(line);
  }
}

int main(int argc, char **argv) {
  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  string outfile = "authors.txt";
  
  vector<string> filenames;
  for (int i = 1; i < argc; i++) {
    string arg = (string)argv[i];
    if (arg == "-o") {
      CHECK(i < argc - 1);
      outfile = argv[i + 1];
      i++;
    } else {
      filenames.push_back(arg);
    }
  }
  
  if (filenames.empty()) {
    fprintf(stderr, "Pass JSON paper files on the command line.\n");
    return -1;
  }
  
  // Number of articles authored by each name.
  std::unordered_map<string, AuthorStats> author_stats;
  RE2 line_re{"([^\t]*)\t([\\d]+)\t([\\d]+)"};  
  auto OneFile = 
    [&author_stats, &line_re](const string &filename) {
    printf("%s\n", filename.c_str());
    LocalForEachLine(filename,
		     [&author_stats, &line_re](string line) {
      string author;
      int64 articles = 0LL, citations = 0LL;
      CHECK(RE2::FullMatch(line, line_re, &author, &articles, &citations)) << line;
      
      AuthorStats &stats = author_stats[author];
      stats.articles += articles;
      stats.citations += citations;
    });
  };    

  UnParallelApp(filenames, OneFile, 8);
  
  printf("Writing %lld merged author records to %s...\n", author_stats.size(),
	 outfile.c_str());
  FILE *out = fopen(outfile.c_str(), "wb");
  CHECK(out != nullptr) << outfile.c_str();
  for (const auto &row : author_stats) {
    fprintf(out, "%s\t%lld\t%lld\n",
	    row.first.c_str(), row.second.articles, row.second.citations);
  }
  fclose(out);

  return 0;
}
