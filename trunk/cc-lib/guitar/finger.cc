
#include <string>
#include <vector>

#include "guitar.h"

#include "base/logging.h"
#include "util.h"

using namespace std;
using Fingering = Guitar::Fingering;
using Chord = Guitar::Chord;

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Prints known fingerings for a chord.\n"
            "Usage (case sensitive):\n\n"
            "  finger C6add9\n\n");
    return -1;
  }
  
  optional<Chord> co = Guitar::Parse(argv[1]);
  if (!co.has_value()) {
    fprintf(stderr, "Can't parse: %s\n", argv[1]);
    return -1;
  }

  vector<Fingering> fv = Guitar::GetFingerings(co.value());
  if (fv.empty()) {
    fprintf(stderr, "No known fingerings for %s\n",
            Guitar::ChordString(co.value()).c_str());
    return 01;
  }
  
  for (const Fingering f : fv) {
    printf("%s\n", Guitar::FingeringString(f).c_str());
  }
  return 0;
}
