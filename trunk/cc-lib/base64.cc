
#include "base64.h"
#include <cstdio>
#include <cstdint>
#include <string>

using namespace std;

/* This code copied from the common public domain utility and then
   modified to like, pass in arguments instead of using global
   variables. */

using uint8 = uint8_t;

static string EncodePtr(const uint8 *in, unsigned int length) {
  int i, hiteof = 0;

  uint8 dtable[256] = {};

  /* PERF should not be needed with dtable = {} above? */
  for (i = 0; i < 256; i++) dtable[i] = 0;

  for (i = 0; i < 9; i++) {
    dtable[i] = 'A' + i;
    dtable[i + 9] = 'J' + i;
    
    dtable[26 + i] = 'a' + i;
    dtable[26 + i + 9] = 'j' + i;
  }

  for (i = 0; i < 8; i++) {
    dtable[i + 18] = 'S' + i;
    dtable[26 + i + 18] = 's' + i;
  }

  for (i = 0; i < 10; i++) {
    dtable[52 + i] = '0' + i;
  }

  dtable[62] = '+';
  dtable[63] = '/';


  unsigned int idx = 0;
  string ou;

  while (!hiteof) {
    uint8 igroup[3];
    uint8 ogroup[4];
    int n;

    igroup[0] = igroup[1] = igroup[2] = 0;
    for (n = 0; n < 3; n++) {
      
      if (idx >= length) { hiteof = 1; break; }
      else igroup[n] = (uint8) in[idx++];

    }

    if (n > 0) {

      ogroup[0] = dtable[igroup[0] >> 2];
      ogroup[1] = dtable[((igroup[0] & 3) << 4) | (igroup[1] >> 4)];
      ogroup[2] = dtable[((igroup[1] & 0xF) << 2) | (igroup[2] >> 6)];
      ogroup[3] = dtable[igroup[2] & 0x3F];

      if (n < 3) {
        ogroup[3] = '=';
        if (n < 2) {
          ogroup[2] = '=';
        }
      }

      for (i = 0; i < 4; i++) {
        ou += (char)ogroup[i];
      }

    }
  }
  return ou;
}

string Base64::Encode(const string &in) {
  return EncodePtr((const uint8 *)in.data(), in.size());
}

string Base64::EncodeV(const std::vector<uint8> &in) {
  return EncodePtr((const uint8 *)in.data(), in.size());
}

template<class C>
static C DecodeC(const string &in) {
  uint8 dtable[256];

  /* strange magic for EBCDIC. I'm not touching it! */
  for (int i = 0; i < 255; i++) dtable[i] = 0x80;
  for (int i = 'A'; i <= 'I'; i++) dtable[i] = 0 + (i - 'A');
  for (int i = 'J'; i <= 'R'; i++) dtable[i] = 9 + (i - 'J');
  for (int i = 'S'; i <= 'Z'; i++) dtable[i] = 18 + (i - 'S');
  for (int i = 'a'; i <= 'i'; i++) dtable[i] = 26 + (i - 'a');
  for (int i = 'j'; i <= 'r'; i++) dtable[i] = 35 + (i - 'j');
  for (int i = 's'; i <= 'z'; i++) dtable[i] = 44 + (i - 's');
  for (int i = '0'; i <= '9'; i++) dtable[i] = 52 + (i - '0');
  dtable[(unsigned char)'+'] = 62;
  dtable[(unsigned char)'/'] = 63;
  dtable[(unsigned char)'='] = 0;

  unsigned int idx = 0;
  C ou;

  for (;;) {
    uint8 a[4], b[4], o[3];

    for (int i = 0; i < 4; i++){
      int c = 0;
      while (idx < in.length()) {
        c = in[idx];
        if (c > ' ') break;
        else idx++;
      }

      if (idx >= in.length()) {
        if (i > 0) {
          fprintf(stderr, "Unexpected end of file.\n");
          return C{};
        } else return ou; /* done */
      }

      idx++;

      if (dtable[c] & 0x80){
        fprintf(stderr, "Illegal character '%c' in input file.\n", c);
        return C{};
      }

      a[i] = (uint8)c;
      b[i] = (uint8)dtable[(uint8)c];
    }

    o[0] = (b[0] << 2) | (b[1] >> 4);
    o[1] = (b[1] << 4) | (b[2] >> 2);
    o[2] = (b[2] << 6) |  b[3];
    int c = a[2] == '=' ? 1 : (a[3] == '=' ? 2 : 3);

    for (int m = 0; m < c; m++)
      ou.push_back(o[m]);

    if (c < 3) return ou;
  }
}

string Base64::Decode(const string &in) {
  return DecodeC<string>(in);
}

std::vector<uint8> Base64::DecodeV(const string &in) {
  return DecodeC<std::vector<uint8>>(in);
}

bool Base64::IsBase64Char(char c) {
  if (c >= 'a' && c <= 'z') return true;
  if (c >= 'A' && c <= 'Z') return true;
  if (c >= '0' && c <= '9') return true;
  if (c == '+' || c == '/' || c == '=') return true;
  return false;
}
