
#include <string>
#include <vector>
#include <cstdint>
#include <utility>
#include <unordered_set>
#include <optional>

#include "util.h"
#include "re2/re2.h"
#include "threadutil.h"
#include "base/logging.h"
#include "guitar/guitar.h"

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

  vector<pair<string, ChordParser::Parsed>> chords_debug =
  ParallelMap(entries,
	      [&parser](const Entry &entry) {
		return make_pair(entry.filename,
				 parser.ExtractChords(entry.body));
	      },
	      16);

  printf("Get unknown..\n");
  std::unordered_map<string, int> unknown_chords;
  std::unordered_map<string, int> unfingered_chords;  
  int64 known = 0, unfingered = 0, unknown = 0;
  for (const auto &[filename, parsed] : chords_debug) {
    for (const string &c : parsed.chords) {
      std::optional<Guitar::Chord> cho = Guitar::Parse(c);
      if (cho.has_value()) {
	vector<Guitar::Fingering> vf = Guitar::GetFingerings(*cho);
	if (!vf.empty()) {
	  known++;
	} else {
	  unfingered++;
	  unfingered_chords[c]++;
	}
      } else {
	unknown++;
	unknown_chords[c]++;
      }
    }
  }

  fflush(stdout);
  printf("Stats..\n");
  // Tally stats:
  int64 all_lines = 0, chord_lines = 0, crd_lines = 0;
  int64 intro_lines = 0;
  int64 num_chords = 0;
  int64 chords_truncated = 0;
  int64 multiformat_files = 0, unextracted_files = 0;
  int64 multiple_intro_files = 0;
  
  for (const auto &[filename, parsed] : chords_debug) {
    all_lines += parsed.lines;
    chord_lines += parsed.chord_lines;
    crd_lines += parsed.crd_lines;
    intro_lines += parsed.intro_lines;
    num_chords += parsed.chords.size();
    chords_truncated += parsed.chords_truncated;
    if (parsed.chord_lines > 0 && parsed.crd_lines > 0)
      multiformat_files++;
    if (parsed.chord_lines == 0 && parsed.crd_lines == 0)
      unextracted_files++;
    if (parsed.intro_lines > 1)
      multiple_intro_files++;
  }
  
  printf("Number of entries: %lld\n", (int64)entries.size());
  printf("All lines: %lld\n"
	 "Chord lines: %lld (%.2f%%)\n"
	 "CRD lines: %lld (%.2f%%)\n"
	 "Intro lines: %lld (%.2f%%)\n"
	 "Total chords: %lld\n"
	 "Cycle chords removed: %lld\n"
	 "All files: %lld\n"
	 "Multiformat: %lld (%.2f%%)\n"
	 "No chords: %lld (%.2f%%)\n"
	 "Multiple intro: %lld (%.2f%%)\n"
	 "Chords known: %lld\n"
	 "Chords unfingered: %lld\n"
	 "Chords unknown: %lld\n",
	 all_lines,
	 chord_lines,
	 (chord_lines * 100.0) / all_lines,
	 crd_lines,
	 (crd_lines * 100.0) / all_lines,
	 intro_lines,
	 (intro_lines * 100.0) / all_lines,
	 num_chords,
	 chords_truncated,
	 // Files
	 (int64)entries.size(),
	 multiformat_files,
	 (multiformat_files * 100.0) / entries.size(),
	 unextracted_files,
	 (unextracted_files * 100.0) / entries.size(),
	 multiple_intro_files,
	 (multiple_intro_files * 100.0) / entries.size(),
	 known,
	 unfingered,
	 unknown);

  printf("Writing to files...\n");
  {
    FILE *f = fopen("chords.lines", "wb");
    CHECK(f);
    for (const auto &[filename, parsed] : chords_debug) {
      const auto &chords = parsed.chords;
      if (!chords.empty()) {
	fprintf(f, "\n# %s\n", filename.c_str());
	for (const string &s : chords) {
	  fprintf(f, "%s ", s.c_str());
	}
	fprintf(f, "\n");
      }
    }
    fclose(f);
  }

  {
    // Multiformat files
    FILE *f = fopen("multiformat.txt", "wb");
    for (const auto &[filename, parsed] : chords_debug) {
      if (parsed.chord_lines > 0 && parsed.crd_lines > 0) {
	fprintf(f, "%s %d %d\n",
		filename.c_str(), parsed.chord_lines, parsed.crd_lines);
      }
    }
    fclose(f);
  }

  printf("Unfingered chords:");
  for (const auto &[s, count] : unfingered_chords) {
    printf(" % 6d: %s\n", count, s.c_str());
  }
  printf("\n");

  printf("Uknown chords:");
  for (const auto &[s, count] : unknown_chords) {
    printf(" % 6d: %s\n", count, s.c_str());
  }
  printf("\n");
  
  /*
Number of entries: 445764
All lines: 49052253
Chord lines: 4222050 (8.61%)
  */
  
  printf("Done.\n");
  return 0;
}
