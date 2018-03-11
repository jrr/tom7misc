
#include "util.h"

#include <cmath>
#include <string>
#include <vector>

using namespace std;

template<class K, class C>
inline bool ContainsKey(const C &container, const K &key) {
  return container.find(key) != container.end();
}

template<class F>
inline static void LocalForEachLine(const string &filename, F f) {
  vector<string> lines = Util::ReadFileToLines(filename);
  for (int i = 0; i < lines.size(); i++) {
    const string &line = lines[i];
    f(line);
  }
}

inline static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

template<class T>
inline static void Reverse(vector<T> *v) {
  vector<T> ret;
  ret.reserve(v->size());
  for (int i = v->size() - 1; i >= 0; i--)
    ret.push_back(std::move((*v)[i]));
  v->swap(ret);
}



struct CiteStats {
  int64 articles = 0;
  int64 citations = 0;
};

inline static string Normalize(string w) {
  // Lowercase ASCII letters.
  for (char &c : w) {
    if (c >= 'A' && c <= 'Z') c |= 32;
  }

  // Characters that shall not occur.
  {
    string other;
    for (char c : w) {
      switch (c) {
      case '\n':
      case '\r':
      case '\t':
      case '\0':
	break;
      default:
	other += c;
      }
    }
    other.swap(w);
  }
    
  
  // Nonstandard quotation marks
  w = Util::Replace(std::move(w), "”", "\"");
  w = Util::Replace(std::move(w), "“", "\"");
  w = Util::Replace(std::move(w), "‘", "'");
  w = Util::Replace(std::move(w), "’", "'");
  
  // Remove punctuation from the end of words.
  [&w]() {
    while (!w.empty()) {
      switch (w.back()) {
      case ':':
      case ',':
      case '.':
      case '?':
      case ';':
      case '!':
      case '\'':
      case '\"':
      case ')':
      case ']':
	w.resize(w.size() - 1);
	break;
      default:
	return;
      }
    }
  }();

  // And a few things from the front of words:
  w = [](const string &w) -> string {
    for (int i = 0; i < w.size(); i++) {
      switch (w[i]) {
      case '(':
      case '\'':
      case '\"':
      case '[':
	break;
      default:
	return w.substr(i, string::npos);
      }
    }
    return "";
  }(w);
  
  // U+2013 EN DASH becomes hyphen
  // w = Util::Replace(std::move(w), "\u2013", "-");
  w = Util::Replace(std::move(w), "–", "-");
  w = Util::Replace(std::move(w), "—", "-");
  return w;
}
