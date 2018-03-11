
#include "util.h"

#include <string>
#include <vector>

using namespace std;

template<class K, class C>
inline bool ContainsKey(const C &container, const K &key) {
  return container.find(key) != container.end();
}

template<class F>
static void LocalForEachLine(const string &filename, F f) {
  vector<string> lines = Util::ReadFileToLines(filename);
  for (int i = 0; i < lines.size(); i++) {
    const string &line = lines[i];
    f(line);
  }
}

struct CiteStats {
  int64 articles = 0;
  int64 citations = 0;
};
