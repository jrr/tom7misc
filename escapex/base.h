// Base definitions and macros. Should not have any SDL dependencies.

#ifndef __MACROS_H
#define __MACROS_H

#include <cstdint>
#include <string>
#include <memory>

using namespace std;

using uint8 = uint8_t;
using int32 = int32_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

#define NOT_COPYABLE(classname) \
  private: \
  classname(const classname &) = delete; \
  classname &operator =(const classname &) = delete

#endif
