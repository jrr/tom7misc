
#ifndef __TOM7_PARSEUTIL_H
#define __TOM7_PARSEUTIL_H

#include "stringhack.h"

struct pu {
  static string lowercase(string in);

  static int    bracksplit(string & in, string & ret);
  static int    intsplit(string & in);
  static string wordsplit (string & in);
  static string wordsplitchar(string & in, char c, bool stripwhite=true);
  static string wordsplitquot(string & in);
  static string loseheadwhite(string in);
};

#endif
