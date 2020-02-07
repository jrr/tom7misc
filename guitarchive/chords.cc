
#include <string>
#include <vector>
#include <cstdint>

#include "util.h"
#include "re2/re2.h"
#include "threadutil.h"
#include "base/logging.h"

#include "guitarchive.h"

using namespace std;
using int64 = int64_t;

namespace {
struct ChordParser {
  ChordParser() {
    vector<string> base = {
      "C",
      "C#", "Db",
      "D",
      "D#", "Eb",
      "E",
      "F",
      "F#", "Gb",
      "G",
      "G#", "Ab",
      "A",
      "A#", "Bb",
      "B",
    };

    vector<string> suffix = {
      "", "maj7", "maj9", "maj11", "maj13", "maj9#11", "maj13#11",
      "6", "add9", "6add9", "maj7b5", "maj7#5", "m", "m7", "m9", "m11",
      "m13", "m6", "madd9", "m6add9", "mmaj7", "mmaj9", "m7b5", "m7#5",
      "7", "9", "11", "13", "7sus4", "7b5", "7#5", "7b9", "7#9", "9b5",
      "9#5", "13#11", "13b9", "11b9", "aug", "dim", "dim7", "5", "sus4",
      "sus2", "sus2sus4", "-5",
      /* exotic. note the parens would have to be escaped, too
      "7(b5,b9)", "7(b5,#9)", "7(#5,b9)", "7(#5,#9)",
      */
    };
    
    vector<string> split = {
      "C/E", "C/F", "C/G", "D/F#", "D/A", "D/Bb", "D/B", "D/C", "E/B",
      "E/C#", "E/D", "E/D#", "E/F", "E/F#", "E/G", "E/G#", "Em/B", "Em/C#",
      "Em/D", "Em/D#", "Em/F", "Em/F#", "Em/G", "Em/G#", "F/C", "F/D",
      "F/D#", "F/E", "F/G", "F/A", "Fm/C", "G/B", "G/D", "G/E", "G/F",
      "G/F#", "A/C#", "A/E", "A/F", "A/F#", "A/G", "A/G#", "Am/C", "Am/E",
      "Am/F", "Am/F#", "Am/G", "Am/G#",
    };

    string reg = "(?:(?:" + Util::Join(base, "|") + ")"
      "(?:" + Util::Join(suffix, "|") + "))";
    string splits = "(?:" + Util::Join(split, "|") + ")";
    
    standard_chord = new RE2(reg + "|" + splits);

    CHECK(RE2::FullMatch("Db", *standard_chord));
    CHECK(RE2::FullMatch("Gb7sus4", *standard_chord));
    CHECK(RE2::FullMatch("Em/F#", *standard_chord));
  }
  

  RE2 *standard_chord = nullptr;

  void ExtractChords(const string &body) {
    // Really simple chord parsing to start...

  }
};
}

int main(int argc, char **argv) {
  ChordParser parser;
  vector<Entry> entries = Guitarchive::Load(16);

  printf("Process..\n");
  fflush(stdout);
  ParallelApp(entries,
	      [&parser](const Entry &entry) {
		// XXX
		parser.ExtractChords(entry.body);
	      },
	      16);
  
  printf("Number of entries: %lld\n", (int64)entries.size());
  return 0;
}
