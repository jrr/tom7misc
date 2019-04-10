#include "md5.h"

#include <string>
#include <vector>
#include <cstdint>

#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;

#define EXPECT_EQ(a, b) do { auto aa = (a); auto bb = (b); \
    CHECK(aa == bb) << \
      "Expected equality of these two expressions:\n  " #a \
      "\n  " #b "\nBut the results were:\n  " << aa << \
      "\n  " << bb; \
  } while (0)

int main(int argc, char **argv) {
  // XXX check these against a known good implementation
  EXPECT_EQ("d41d8cd98f00b204e9800998ecf8427e", MD5::Ascii(MD5::Hash("")));
  EXPECT_EQ("5eb63bbbe01eeed093cb22bb8f5acdc3", MD5::Ascii(MD5::Hash("hello world")));
  std::vector<uint8> v;
  for (int i = 0; i < 256; i++) v.push_back((uint8)i);
  EXPECT_EQ("e2c865db4162bed963bfaa9ef6ac18f0", MD5::Ascii(MD5::Hashv(v)));
}
