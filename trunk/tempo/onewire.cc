
#include "onewire.h"

#include <cstdint>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/util.h"

using uint32 = uint32_t;
using namespace std;

OneWire::OneWire() {
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

bool OneWire::Probe::Temperature(uint32 *millideg_c) {
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
  const uint32 reading = atoi(&data[te + 2]);
  last_reading = reading;
  *millideg_c = reading;
  return true;
}
