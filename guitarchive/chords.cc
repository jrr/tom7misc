
#include <string>
#include <vector>
#include <cstdint>
#include <utility>

#include "util.h"
#include "re2/re2.h"
#include "threadutil.h"
#include "base/logging.h"

#include "guitarchive.h"

using namespace std;
using int64 = int64_t;

#define ALLOW_WS_RE "\\s*"
#define WS_RE "\\s+"

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

    string standard_chord = "(?:" + reg + "|" + splits + ")";
    
    standard_chord_re = new RE2(standard_chord);
    line_of_chords_re = new RE2(
	// Any number of chords ending with whitespace
	"(?:" ALLOW_WS_RE + standard_chord + WS_RE ")*"
	// And then one chord that need not have whitespace
	ALLOW_WS_RE + standard_chord + ALLOW_WS_RE);

    extract_chord_re = new RE2(ALLOW_WS_RE "(" + standard_chord + ")");
    
    CHECK(RE2::FullMatch("Db", *standard_chord_re));
    CHECK(RE2::FullMatch("Gb7sus4", *standard_chord_re));
    CHECK(RE2::FullMatch("Em/F#", *standard_chord_re));
    CHECK(RE2::FullMatch("G", *line_of_chords_re));
    CHECK(RE2::FullMatch(" G", *line_of_chords_re));
    CHECK(RE2::FullMatch("G ", *line_of_chords_re));
    CHECK(RE2::FullMatch("Em/F#\tGb7sus4 ", *line_of_chords_re));
    CHECK(RE2::FullMatch("\tEm/F#\tGb7sus4", *line_of_chords_re));        
  }
  

  RE2 *standard_chord_re = nullptr;
  RE2 *line_of_chords_re = nullptr;
  RE2 *extract_chord_re = nullptr;

  static bool AcceptLineChords(const vector<string> &chords) {
    if (chords.size() <= 6) return true;

    // Don't allow it if it consists only of "E".
    for (const string &s : chords) {
      if (s != "E") return true;
    }

    return false;
  }
  
  vector<string> ExtractChords(const string &body) {
    // Really simple chord parsing to start.
    vector<string> lines = Util::SplitToLines(body);

    // All chords in the song.
    vector<string> chords;

    // TODO: Some tablature formats use E for 1/8 notes, so
    // we get lines of just E E E E E E. Should reject these.
    // e.g. c:\code\electron-guitar\tabscrape\tabs\424266.txt

    // Does the line just have chords, separated by whitespace?
    int cl = 0;
    for (const string &line : lines) {
      // First, filter to see if the line is suitable.
      if (RE2::FullMatch(line, *line_of_chords_re)) {
	cl++;

	re2::StringPiece l(line);

	vector<string> line_chords;
	string chord;
	while (RE2::Consume(&l, *extract_chord_re, &chord)) {
	  line_chords.push_back(std::move(chord));
	}

	if (AcceptLineChords(line_chords)) {
	  chords.reserve(chords.size() + line_chords.size());
	  for (string &s : line_chords)
	    chords.push_back(std::move(s));
	}
      }
    }

    {
      MutexLock ml(&stats_m);
      chord_lines += cl;
      num_chords += chords.size();
      all_lines += lines.size();
    }

    return chords;
  }
  
  std::mutex stats_m;
  int64 all_lines = 0, chord_lines = 0, num_chords = 0;
};
}

int main(int argc, char **argv) {
  ChordParser parser;
  vector<Entry> entries = Guitarchive::Load(16);

  printf("Process..\n");
  fflush(stdout);

  vector<pair<string, vector<string>>> chords_debug =
  ParallelMap(entries,
	      [&parser](const Entry &entry) -> pair<string, vector<string>> {
		return {entry.filename, parser.ExtractChords(entry.body)};
	      },
	      16);
  
  printf("Number of entries: %lld\n", (int64)entries.size());
  printf("All lines: %lld\n"
	 "Chord lines: %lld (%.2f%%)\n"
	 "Total chords: %lld\n",
	 parser.all_lines,
	 parser.chord_lines,
	 (parser.chord_lines * 100.0) / parser.all_lines,
	 parser.num_chords);

  printf("Writing to file...\n");
  FILE *f = fopen("chords.lines", "wb");
  CHECK(f);
  for (const auto &[filename, chords] : chords_debug) {
    if (!chords.empty()) {
      fprintf(f, "# %s\n", filename.c_str());
      for (const string &s : chords) {
	fprintf(f, "%s ", s.c_str());
      }
      fprintf(f, "\n");
    }
  }
  fclose(f);
  
  /*
Number of entries: 445764
All lines: 49052253
Chord lines: 4222050 (8.61%)
  */
  
  printf("Done.\n");
  return 0;
}
