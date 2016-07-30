#ifndef __BASE64_H
#define __BASE64_H

#include <string>

struct Base64 {
  static std::string Encode(const std::string &s);
  /* XXX good if it could do error checking */
  static std::string Decode(const std::string &s);
};

#endif
