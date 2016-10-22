
#include "httputil.h"

using namespace std;

string HTTPUtil::URLEncode(const string &s) {
  string out;
  for (unsigned int x = 0; x < s.length(); x++) {
    if (s[x] == ' ')
      out += '+';
    else if ((s[x] >= 'a' && s[x] <= 'z')
         ||  (s[x] >= 'A' && s[x] <= 'Z') || s[x] == '.'
         ||  (s[x] >= '0' && s[x] <= '9')) {
      out += s[x];
    } else {
      out += '%';
      out += "0123456789ABCDEF"[15&(s[x]>>4)];
      out += "0123456789ABCDEF"[s[x]&15];
    }
  }
  return out;
}
