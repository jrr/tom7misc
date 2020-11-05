
#ifndef _CC_LIB_WAVESAVE_H
#define _CC_LIB_WAVESAVE_H

#include <string>
#include <vector>
#include <utility>
#include <cstdint>

struct WaveSave {

  static bool SaveStereo(const std::string &filename,
                         const std::vector<std::pair<float, float>> &samples,
                         int samples_per_sec);

  // Unsigned 16-bit samples, mono.
  static bool SaveMono16(const std::string &filename,
			 const std::vector<uint16_t> &samples,
			 int samples_per_sec);
};

#endif
