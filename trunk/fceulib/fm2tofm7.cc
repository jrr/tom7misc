/* This standalone program is part of the fceulib distribution, but shouldn't
   be compiled into it. */

#include <vector>
#include <string>

#include "simplefm2.h"
#include "simplefm7.h"
#include "base/stringprintf.h"
#include "base/logging.h"

int main(int argc, char **argv) {
  if (argc != 3) {
    fprintf(stderr,
            "Convert an fm2 file to fm7 format."
            "Usage: fm2tocc karate.fm2 karate.fm7\n\n");
    return -1;
  }

  vector<pair<uint8, uint8>> inputs = SimpleFM2::ReadInputs2P(argv[1]);
  fprintf(stderr, "Loaded %s with %lld inputs.\n", argv[1], inputs.size());

  SimpleFM7::WriteInputs2P(argv[2], inputs);

  vector<pair<uint8, uint8>> readback = SimpleFM7::ReadInputs2P(argv[2]);

  CHECK(inputs.size() == readback.size()) << inputs.size() << " vs "
					  << readback.size();
  for (int i = 0; i < inputs.size(); i++) {
    if (inputs[i] != readback[i]) {
      printf("%d. %s|%s  vs  %s|%s\n",
	     i,
	     SimpleFM2::InputToString(inputs[i].first).c_str(),
	     SimpleFM2::InputToString(inputs[i].second).c_str(),
	     SimpleFM2::InputToString(readback[i].first).c_str(),
	     SimpleFM2::InputToString(readback[i].second).c_str());
    }
  }
  
  return 0;
}
