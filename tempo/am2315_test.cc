#include "am2315.h"

#include <stdio.h>
#include <unistd.h>
#include <cstdint>

#include "base/logging.h"
#include "base/stringprintf.h"
#include "pi/bcm2835.h"

using uint32 = uint32_t;

int main(int argc, char **argv) {

  CHECK(bcm2835_init()) << "BCM Init failed!";

  AM2315::Initialize();
  int64 success = 0, failure = 0;

  AM2315::Info info;
  const char *info_err = "not set";
  CHECK(AM2315::ReadInfo(&info, &info_err)) << info_err;
  printf("AM2315 Info:\n"
         "  Model: %04x\n"
         "  Version: %02x\n"
         "  ID: %08x\n",
         (uint32)info.model, (uint32)info.version, info.id);

  for (;;) {
    string sf = StringPrintf("[%lld/%lld = %.1f%%] ",
                             success,
                             success + failure,
                             (success * 100.0) / (success + failure));

    [[maybe_unused]] float temp = -999.0f;
    const char *err = "not set";
    if (AM2315::ReadTemp(&temp, &err)) {
      success++;
      printf("%sRead temp: %.2f deg C\n", sf.c_str(), temp);
    } else {
      failure++;
      printf("%sFailed: %s\n", sf.c_str(), err);
    }

    sleep(1);

    [[maybe_unused]] float rh = -666.0f;
    if (AM2315::ReadRH(&rh, &err)) {
      success++;
      printf("%sRead Hum: %.2f%% RH\n", sf.c_str(), rh);
    } else {
      failure++;
      printf("%sFailed: %s\n", sf.c_str(), err);
    }

    sleep(1);

  }

  return 0;
}
