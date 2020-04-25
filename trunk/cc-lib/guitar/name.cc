
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
    fprintf(stderr, "Prints the chord name for a fingering, if known.\n"
	    "Usage:\n\n"
	    "  name x32010\n\n");
    return -1;
  }

  // Perhaps this parsing should be in guitar.h.
  auto OKChars = [](const string &s) {
    for (const char c : s)
      if (!Util::matchspec("0-9a-gx", c))
	return false;
    return true;
  };
  
  // Assume rest of strings are muted if fewer than 6 are given.
  const string fing = Util::PadEx(6, (string)argv[1], 'x');
  if (fing.size() != 6 || !OKChars(fing)) {
    fprintf(stderr, "Expected a string with up to 6 characters, from "
	    "0 (open), frets 1-9 or a-g (standing for 10+), or x for mute.\n");
    return -1;
  }

  auto F = [](char c) {
    if (c == 'x') return -1;
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'g') return 10 + (c - 'a');
    CHECK(false) << "impossible";
    return 0;
  };
  
  Fingering f = make_tuple(F(fing[0]),
			   F(fing[1]),
			   F(fing[2]),
			   F(fing[3]),
			   F(fing[4]),
			   F(fing[5]));
  
  std::optional<Chord> co = Guitar::NameFingering(f);
  if (co.has_value()) {
    printf("%s\n", Guitar::ChordString(co.value()).c_str());
    return 0;
  } else {
    fprintf(stderr, "Uknown chord.\n");
    return -1;
  }
}
