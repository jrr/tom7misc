
#ifndef __TOM7_CONLOG_H
#define __TOM7_CONLOG_H

#include "common/stringhack.h"

/* abstract interface for a log window/console.
 */

struct conlog {
  
  virtual void logs(string in) = 0;
  virtual void logf(const char * fmt, ...) = 0;
  
  virtual void clear() = 0;

  virtual void show() = 0;
  virtual void hide() = 0;


};

#endif
