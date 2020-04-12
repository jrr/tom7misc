
#include <vector>
#include <string>
#include <cstdint>
#include <mutex>

#include "re2/re2.h"

using namespace std;

struct ChordParser {
 public:
  ChordParser();

  // Thread safe.
  vector<string> ExtractChords(const string &body);
  
  std::mutex stats_m;
  int64_t all_lines = 0, chord_lines = 0, num_chords = 0;
  int64_t chords_saved = 0;

 private:
  RE2 *standard_chord_re = nullptr;
  RE2 *line_of_chords_re = nullptr;
  RE2 *extract_chord_re = nullptr;
};

