
#include <vector>
#include <string>
#include <unordered_map>

#include "../cc-lib/util.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"

using namespace std;

struct Point {
  int x = 0;
  int y = 0;
  constexpr Point(int x, int y) : x(x), y(y) {}
  Point() {}
};

struct Keyboard {
  Keyboard() {
    vector<string> lines = {
      "qwertyuiop",
      "asdfghjkl",
      "zxcvbnm"
    };

    for (int y = 0; y < lines.size(); y++) {
      const string &line = lines[y];
      for (int x = 0; x < line.size(); x++) {
	points[(int)line[x]] = Point{x, y};
      }
    }
  }

  Point points[128] = {};
};

// Based on more general code from TextSVG.
struct ColinearRemover {

  enum State {
    EMPTY,
    ONE_POINT,
    LINE,
  };
  
  void Push(Point pt, vector<Point> *out) {
    switch (state) {
    case EMPTY:
      // Always take a starting point.
      a = pt;
      state = ONE_POINT;
      return;

    case ONE_POINT:
      if (pt.x != a.x || pt.y != a.y) {
	b = pt;
	state = LINE;
	return;
      }
      return;

    case LINE:
      if (a.y == b.y && b.y == pt.y &&
	  ((a.x <= b.x && b.x <= pt.x) ||
	   (a.x >= b.x && b.x >= pt.x))) {
	// Drop the interior point.
	b = pt;
	return;
      } else {
	// Shift.
	out->push_back(a);
	a = b;
	b = pt;
	return;
      }
    }
  }
  
  void Flush(vector<Point> *out) {
    switch (state) {
    case EMPTY: return;
    case ONE_POINT:
      out->push_back(a);
      break;
    case LINE:
      out->push_back(a);
      out->push_back(b);
    }
  }

  State state = EMPTY;
  Point a, b;
};

vector<Point> Normalize(const vector<Point> &code) {
  // Here we'll only treat the case of colinearity along a row of
  // letters, since we don't know what grid they are layed out in.
  // The situation we are looking for is [x1, y], [x2, y], [x3, y]
  // where x1 <= x2 <= x3 or x1 >= x2 >= x3, and we remove the
  // middle point in this case.
  vector<Point> out;
  ColinearRemover remover;
  for (const Point &p : code) remover.Push(p, &out);
  remover.Flush(&out);
  return out;
}

string CodeString(const vector<Point> &code) {
  string s;
  for (Point p : code) {
    StringAppendF(&s, " %d,%d", p.x, p.y);
  }
  return s;
}

vector<Point> GetCode(const string &s) {
  static Keyboard board;
  vector<Point> points;
  for (int i = 0; i < s.size(); i++) {
    char c = s[i];
    CHECK(c <= 'z' && c >= 'a') << c;
    points.push_back(board.points[(int)c]);
  }
  return points;
}

int main(int argc, char **argv) {
  vector<string> words = Util::ReadFileToLines("../manarags/wordlist.asc");

  std::unordered_map<string, vector<string>> coded;
  
  for (const string &word : words) {
    vector<Point> code = GetCode(word);
    // printf("%s %s\n", word.c_str(), CodeString(code).c_str());
    string norm = CodeString(Normalize(GetCode(word)));
    coded[norm].push_back(word);
  }

  int ambiguous = 0;
  for (const auto &p : coded) {
    if (p.second.size() > 1) {
      // printf("%s:\n  ", p.first.c_str());
      for (const string &w : p.second) {
	printf("%s  ", w.c_str());
      }
      printf("\n");
      ambiguous++;
    }
  }

  printf("\n%d ambiguous clusters.\n", ambiguous);
  
  return 0;
}
