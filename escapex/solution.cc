
#include "solution.h"

#include <string>

#include "rle.h"
#include "util.h"

using namespace std;

string Solution::ToString() const {
  return sizes(Length()) + EscapeRLE::Encode(Length(), (int *)dirs.data());
}

// static
bool Solution::FromString(const string &s, Solution *sol) {
  unsigned int idx = 0;
  if (s.length() < 4) return false;
  int len = shout(4, s, idx);

  dir *dd = EscapeRLE::Decode(s, idx, len);

  if (!dd) return false;

  sol->Clear();
  sol->dirs.reserve(len);
  for (int i = 0; i < len; i++) sol->Append(dd[i]);
  sol->verified = false;

  return true;
}

// static
bool Solution::Equal(const Solution &l, const Solution &r) {
  if (l.dirs.size() != r.dirs.size()) return false;
  for (int i = 0; i < l.dirs.size(); i++) {
    if (l.dirs[i] != r.dirs[i]) return false;
  }
  return true;
}
