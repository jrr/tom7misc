
#include <string>
#include <deque>

#include "../../cc-lib/base/logging.h"
#include "../../cc-lib/base/stringprintf.h"

#include "bigrat.h"

#define VERBOSE false

using namespace std;

string BigRat::ToString() const {
  char *p = nullptr;
  gmp_asprintf(&p, "%Q", r);
  CHECK(p != nullptr);
  string ret = p;
  free(p);
  return ret;
}

