
#ifndef __TEMPO_AM2315_H
#define __TEMPO_AM2315_H

#include <stdio.h>
#include <stdlib.h>
#include <cstdint>

// For AM2315 (probably also 2320) temperature/humidity sensors.

struct AM2315 {
  static constexpr uint8_t ADDRESS = 0x5C;

  // There can only be one of these, since they all have the same
  // address and we only support i2c bus 1.
  static void Initialize();

  // XXX?
  // In degrees C.
  static bool ReadTemp(float *temp, const char **err = nullptr);
  // In % RH.
  static bool ReadRH(float *rh, const char **err = nullptr);  
};

#endif
