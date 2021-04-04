#ifndef _ESCAPE_CHECKFILE_H
#define _ESCAPE_CHECKFILE_H

#include "escapex.h"
#include "escape-util.h"

/* nicer interface to files. automatically closes
   on destroy, automatically checks eof. */

/* XXX also check eof when these functions are called */

struct CheckFile {
  FILE *ff = nullptr;
  bool Read(unsigned int bytes, string &s) {
    /* fread stupidly *fails* if the size is 0 */
    if (bytes == 0) return "";
    char *r = new char[bytes];
    string ss(bytes, '*');
    if (fread(r, bytes, 1, ff) == 1) {
      for (unsigned int i = 0; i < bytes; i++) ss[i] = r[i];
      s = ss;
      delete [] r;
      return 1;
    } else {
      delete [] r;
      return 0;
    }
  }

  ~CheckFile() {
    fclose(ff);
  }

  static std::unique_ptr<CheckFile> Create(const string &f) {
    FILE *ff = fopen(f.c_str(), "rb");
    if (ff == nullptr) return nullptr;
    return std::unique_ptr<CheckFile>(new CheckFile{ff});
  }

  bool ReadInt(int &i) {
    string s;
    if (!Read(4, s)) return 0;
    i =
      ((unsigned char)s[0] << 24) |
      ((unsigned char)s[1] << 16) |
      ((unsigned char)s[2] << 8) |
      ((unsigned char)s[3]);
    return 1;
  }

  bool GetLine(string &s) {
    if (feof(ff)) return false;
    s = EscapeUtil::fgetline(ff);
    return true;
  }

 private:
  explicit CheckFile(FILE *ff) : ff(ff) {}
  NOT_COPYABLE(CheckFile);
};

#endif
