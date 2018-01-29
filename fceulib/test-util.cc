
#include "test-util.h"
#include <cstdlib>
#include <sys/stat.h>

static string ReadAndCloseFile(FILE *f) {
  fseek(f, 0, SEEK_END);
  const int size = ftell(f);
  fseek(f, 0, SEEK_SET);

  string ret;
  ret.resize(size);
  // Bytes are required to be contiguous from C++11;
  // use .front() instead of [0] since the former,
  // introduced in C++11, will prove we have a compatible
  // version.
  // After C++17, this can be ret.data().
  const size_t chunks_read =
    fread(&ret.front(), size, 1, f);
  fclose(f);

  if (chunks_read == 1)
    return ret;
  return "";
}

string ReadFile(const string &s) {
  if (s == "") return "";

  FILE *f = fopen(s.c_str(), "rb");
  if (!f) return "";
  return ReadAndCloseFile(f);
}

vector<string> ReadFileToLines(const string &f) {
  return SplitToLines(ReadFile(f));
}

vector<string> SplitToLines(const string &s) {
  vector<string> v;
  string line;
  // PERF don't need to do so much copying.
  for (size_t i = 0; i < s.size(); i++) {
    if (s[i] == '\r')
      continue;
    else if (s[i] == '\n') {
      v.push_back(line);
      line.clear();
    } else {
      line += s[i];
    }
  }
  return v;
}

string Chop(string &line) {
  for (int i = 0; i < line.length(); i ++) {
    if (line[i] != ' ') {
      string acc;
      for(int j = i; j < line.length(); j ++) {
        if (line[j] == ' ') {
          line = line.substr(j, line.length() - j);
          return acc;
        } else acc += line[j];
      }
      line = "";
      return acc;
    }
  }
  /* all whitespace */
  line = "";
  return "";
}

string LoseWhiteL(const string &s) {
  for (unsigned int i = 0; i < s.length(); i ++) {
    switch(s[i]) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      /* keep going ... */
      break;
    default:
      return s.substr(i, s.length() - i);
    }
  }
  /* all whitespace */
  return "";
}

bool ExistsFile(const string &s) {
  struct stat st;
  return !stat(s.c_str(), &st);
}
