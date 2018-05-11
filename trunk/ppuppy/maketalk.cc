#include <string>

#include "base/logging.h"
#include "ppuppy.h"
#include "talk.h"

using namespace std;

int main(int argc, char **argv) {
  const char *USAGE = "./maketalk.exe input.talk output.ctalk output.screens";
  CHECK(argc == 4) << USAGE;

  Talk t = Talk::Load(argv[1]);
  t.Save(argv[2], argv[3]);
  return 0;
}
