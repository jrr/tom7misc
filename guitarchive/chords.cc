
#include <string>
#include <vector>
#include <cstdint>
#include <utility>

#include "util.h"
#include "re2/re2.h"
#include "threadutil.h"
#include "base/logging.h"

#include "guitarchive.h"
#include "chord-parser.h"

using namespace std;
using int64 = int64_t;

#define ALLOW_WS_RE "\\s*"
#define WS_RE "\\s+"

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
	 "Total chords: %lld\n"
	 "Cycle chords removed: %lld\n",
	 parser.all_lines,
	 parser.chord_lines,
	 (parser.chord_lines * 100.0) / parser.all_lines,
	 parser.num_chords,
	 parser.chords_saved);

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
