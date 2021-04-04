// Base definitions and macros. Should not have any SDL dependencies.

#ifndef _ESCAPE_BASE_H
#define _ESCAPE_BASE_H

#include <cstdint>
#include <string>
#include <memory>

// TODO: Maybe limit this to a couple of types like string, vector,
// etc. (or remove it entirely...)
using namespace std;

using uint8 = uint8_t;
using int32 = int32_t;
using int64 = int64_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

#define NOT_COPYABLE(classname) \
  private: \
  classname(const classname &) = delete; \
  classname &operator =(const classname &) = delete

#endif
