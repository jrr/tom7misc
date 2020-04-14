
#include <vector>
#include <string>
#include <cstdint>
#include <mutex>

#include "re2/re2.h"

using namespace std;

struct ChordParser {
 public:
  ChordParser();
  ~ChordParser();

  struct Parsed {
    vector<string> chords;
    int lines = 0;
    int chord_lines = 0, crd_lines = 0;
    int chords_truncated = 0;
  };
  
  // Thread safe.
  Parsed ExtractChords(const string &body);
    
 private:
  RE2 *standard_chord_re = nullptr;
  RE2 *line_of_chords_re = nullptr;
  RE2 *extract_chord_line_re = nullptr;
  RE2 *line_with_crd_re = nullptr;
  RE2 *extract_bracketed_re = nullptr;
};

