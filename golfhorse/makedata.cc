#include <string>
#include <vector>
#include <stdio.h>

#include "util.h"

using namespace std;

static int SharedPrefix(const string &a, const string &b) {
  int i = 0;
  while (i < a.size() && i < b.size() && a[i] == b[i]) i++;
  return i;
}

int main (int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Provide wordlist on command line!");
    return -1;
  }
  vector<string> words = Util::ReadFileToLines(argv[1]);
  if (words.empty()) {
    fprintf(stderr, "Couldn't open %s\n", argv[1]);
    return -2;
  }
    
  printf("s='");
  string lastword = "";
  for (int i = 0; i < words.size(); i++) {
    const string &word = words[i];
    if (i == 0) {
      printf("%s", word.c_str());
    } else {
      int pfx = std::min(SharedPrefix(lastword, word), 9);
      printf("%d%s", pfx, word.substr(pfx, string::npos).c_str());
    }
    lastword = word;
  }
  printf("0'\n");
  return 0;
}
