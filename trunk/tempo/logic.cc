
#include "logic.h"

#include <map>
#include <string>
#include <cstdint>

#include "util.h"
#include "pi/bcm2835.h"
#include "database.h"

#include "periodically.h"

using namespace std;

static constexpr const char *CONFIG_FILE = "logic-config.txt";

static map<string, string> config;

namespace {

// TODO: Some way to register in database/graph the events of turning
// on/off, and the current state.

// It would be pretty bad to flip the attic fan relay on and off every
// second, for example; even worse if it's like an AC loop. So we want
// some hysteresis. Two useful kinds:
//
// - Debouncing: Require that a condition be true for some length of
//   time before declaring that it holds.
// - Dead zone: Rather than e.g. trigger when T > 30 C, turn on
//   when T > 31 C and off when T < 29 C. (There must be some accepted
//   term for this?)

// TODO: Abstractions/configurability for this!

struct AtticFan {
  // Set "heating/cooling/neutral" behavior based on Season at least.
  // Would be even better if we could get a target temperature from
  // Nest, or some other user-facing control? Would also be good to
  // just have some manual override (run for 24h, turn off for 24h, etc.)
  //
  // All seasons: 
  //   To prevent condensation, if RH is very high, turn on the fan
  //   (unless outdoors is higher?). Note that since RH is temperature
  //   dependent, it might still make sense to exhaust 90% RH at 60 F
  //   into 100% RH at 20 F, since the replacement air is actually
  //   drier (and so will have lower RH when warmed to 60).
  //  
  // If cooling, turn on fan if it is warmer than outside.
  // If heating, turn on fan if it is colder than outside (rare, but
  //   sometimes there are those weirdly warm winter days).
  //
  // During allergy season, perhaps be more reluctant to turn on?
};

}  // namespace

void Logic::Initialize() {
  config = Util::ReadFileToMap(CONFIG_FILE);
}

void Logic::Periodic([[maybe_unused]] Database *db) {
  // ...
}
