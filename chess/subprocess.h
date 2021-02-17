
#ifndef _SUBPROCESS_H
#define _SUBPROCESS_H

#include <string>

struct Subprocess {

  // TODO: Version taking command-line, etc.
  static Subprocess *Create(const std::string &path);

  // These functions are not thread-safe and should only be called from
  // one thread. However, it is permissible to have one thread (only) writing
  // while another (only) reads.
  virtual bool Write(const std::string &data) = 0;
  virtual bool ReadLine(std::string *line) = 0;
  virtual ~Subprocess();
};

#endif
