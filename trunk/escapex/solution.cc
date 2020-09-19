
#include "solution.h"

#include <string>
#include <cstdint>

#include "rle.h"

using uint32 = uint32_t;
using uint8 = uint8_t;
using namespace std;

static string BigEndian32(int i) {
  string s = "    ";
  s[0] = 255 & (i >> 24);
  s[1] = 255 & (i >> 16);
  s[2] = 255 & (i >> 8);
  s[3] = 255 &  i;
  return s;
}

static uint32 ReadBigEndian32(const string &s, int idx) {
  auto Byte32 = [&s](int idx) -> uint32 {
      uint8 b = s[idx];
      return (uint32)b;
    };
  return (Byte32(0) << 24) |
    (Byte32(1) << 16) |
    (Byte32(2) << 8) |
    Byte32(3);
}

string Solution::ToString() const {
  return BigEndian32(Length()) + EscapeRLE::Encode(Length(), (int *)dirs.data());
}

// static
bool Solution::FromString(const string &s, Solution *sol) {
  if (s.length() < 4) return false;
  int len = ReadBigEndian32(s, 0);
  if (len < 0) return false;

  unsigned int idx = 4;
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
  for (int i = 0; i < (int)l.dirs.size(); i++) {
    if (l.dirs[i] != r.dirs[i]) return false;
  }
  return true;
}
