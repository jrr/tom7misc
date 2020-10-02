
#include "process-util.h"

#include <cstdio>
#include <optional>
#include <string>

using namespace std;

// TODO: Make it work on windows too? This is probably posix-only
// because of popen().
std::optional<string> ProcessUtil::GetOutput(const string &cmd) {
  string ret;

  FILE *f = popen(cmd.c_str(), "r");
  if (f == nullptr) return {};

  int count = 0;
  static constexpr int CHUNK_SIZE = 256;
  char buf[CHUNK_SIZE];
  while ((count = fread(buf, 1, CHUNK_SIZE, f))) {
    // printf("Count: %d. buf: %02x %02x %02x...", count, buf[0], buf[1], buf[2]);
    ret.append(buf, count);
    if (count < CHUNK_SIZE) break;
  }
  
  fclose(f);

  return {ret};
}
