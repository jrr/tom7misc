
#include "parseutil.h"

int pu::intsplit(string & s) {
  return atoi(-wordsplit(s));
}

string pu::lowercase (string s) {
  for(unsigned int i = 0; i < s.length(); i++) {
    if (s[i] >= 'A' && s[i] <= 'Z')
      s[i] = (char)(s[i] | 32);
  }
  return s;
}

string pu::wordsplit (string & in) {
  string out;

  in = loseheadwhite(in);

  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] == ' ') {
      out = in.substr(0, i);
      in = in.substr(i + 1, in.length() - ( i + 1));
      return out;
    } 
  }
  out = in;
  in = "";
  return out;
}

int pu::bracksplit (string & in, string & ret) {
  string out;
  int inquotes = 0;
  in = loseheadwhite(in);

  if (in[0] != '{') return 0;

  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] == '\"') inquotes ^= 1;
    else if (in[i] == '}' && !inquotes) {
      out = in.substr(1, i - 1);
      in = in.substr(i + 1, in.length() - ( i + 1));
      ret = out;
      return 1;
    }
  }
  /* unclosed { */
  in = "";
  return 0;
}

string pu::wordsplitchar (string & in, char c, bool stripwhite) {
  string out;

  if (stripwhite) in = loseheadwhite(in);

  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] == c) {
      out = in.substr(0, i);
      in = in.substr(i + 1, in.length() - ( i + 1));
      return out;
    } 
  }
  out = in;
  in = "";
  return out;
}

string pu::wordsplitquot (string & in) {
  string out;
  int inquot = 0;
  in = loseheadwhite(in);

  for (unsigned int i = 0; i < in.length(); i++) {
    switch (in[i]) {
    case ' ':
      if (inquot) break;

      out = in.substr(0, i);
      in = in.substr(i + 1, in.length() - ( i + 1 ));
      return out;
    case '\"':
      if (inquot) {
        out = in.substr(1, i - 1);
        in = in.substr(i + 1, in.length() - ( i + 1 ));
        return out;
      } else inquot ++;
    default:;
    }
  }
  out = in;
  in = "";
  return out;
}


string pu::loseheadwhite(string in) {
  for (unsigned int i = 0; i < in.length(); i++) {
    if (in[i] != ' ') return in.substr(i, in.length() - i);
  }
  return "";
}
