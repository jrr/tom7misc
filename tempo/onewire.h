
#ifndef _TEMPO_ONEWIRE_H
#define _TEMPO_ONEWIRE_H

#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <cstdint>
#include <unordered_map>

// For Dallas OneWire temperature probes.

struct OneWire {
  const std::string dir = "/sys/bus/w1/devices";

  // This will load all the probes it can find.
  OneWire();

  struct Probe {
    // The path to the file. From /proc we only get a streaming read,
    // so we need to freshly open and stream the entire file each
    // time.
    std::string fullpath;
    uint32_t last_reading = 0;

    bool Temperature(uint32_t *microdeg_c);
  };

  // Key here is the hexadecimal code like 28-0417c13c18ff.
  std::unordered_map<std::string, Probe> probes;
};

#endif
