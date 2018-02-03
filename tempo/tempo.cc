#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <cstdint>
#include <unordered_map>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/util.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;

// Util::ReadFile doesn't work, probably because
// fseek doesn't 
static string ReadFile(const string &filename) {
  FILE *f = fopen(filename.c_str(), "rb");
  if (!f) return "";
  string ret;
  ret.reserve(128);
  int c = 0;
  while (EOF != (c = getc(f))) {
    ret += (char)c;
  }
  fclose(f);
  return ret;
}

struct OneWire {
  const string dir = "/sys/bus/w1/devices";

  // This will load all the probes it can find.
  OneWire() {
    vector<string> files = Util::ListFiles(dir);
    for (const string &file : files) {
      const string fullpath = dir + "/" + file + "/w1_slave";
      printf("Trying %s...\n", fullpath.c_str());
      string testread = Util::ReadFile(fullpath);
      if (!testread.empty()) {
	CHECK(probes.find(file) == probes.end())
	  << file << " duplicate?";
	probes[file].fullpath = fullpath;
	printf("  ... OK\n");
      } else {
	printf("  ... (failed -- might be a master?)\n");
      }
    }

    printf("Found %d probe(s).\n", (int)probes.size());
  }

  struct Probe {
    // The path to the file. From /proc we only get a streaming read,
    // so we need to freshly open and stream the entire file each
    // time.
    string fullpath;
    // XXX Last reading, etc.

    bool Temperature(uint32 *microdeg_c) {
      // This is coming from the /proc filesystem, but still,
      // this is a very bizarre format. Two lines, like so:
      // 1c 01 4b 46 7f ff 04 10 e8 : crc=e8 YES
      // 1c 01 4b 46 7f ff 04 10 e8 t=17750
      // The hex dump is literally there in the file, and both
      // lines seem to always be the same (so I guess this is
      // like the raw data--but I don't see any way to get the
      // data itself; just this dump!). The encoding is not
      // obvious. Other than the last byte being the CRC, there
      // aren't two bytes there representing 17750 (0x4556).
      // But 17750 is 17.750 degrees C.
      string data = Util::ReadFile(fullpath);
      if (data.empty()) return false;
      if (data.find("YES") == string::npos) return false;
      size_t te = data.find("t=");
      if (te == string::npos) return false;
      *microdeg_c = atoi(&data[te + 2]);
      return true;
    }
  };

  std::unordered_map<string, Probe> probes;
};


int main(int argc, char **argv) {
  OneWire onewire;

  for (;;) {
    for (auto &p : onewire.probes) {
      uint32 microdegs_c = 0;
      if (p.second.Temperature(&microdegs_c)) {
	printf("%s: %u\n", p.first.c_str(), microdegs_c);
      } else {
	printf("%s: ERROR\n", p.first.c_str());
      }
    }
  }
    
  printf("SERVER OK\n");
  return 0;
}
