
#include "chord-parser.h"

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

static bool AcceptLineChords(const vector<string> &chords) {
  if (chords.size() <= 6) return true;

  // Some tablature formats use E for 1/8 notes, so we get lines of
  // just E E E E E E. e.g.
  // c:\code\electron-guitar\tabscrape\tabs\424266.txt

  // Don't allow it if it consists only of "E".
  for (const string &s : chords) {
    if (s != "E") return true;
  }

  return false;
}

static bool AcceptCrdChords(const vector<string> &chords) {
  // TODO: Any issues here?
  return true;
}


ChordParser::ChordParser() {
  vector<string> base = {
    "C",
    "C#", "Db", // synonyms
    "D",
    "D#", "Eb", // synonyms
    "E",
    "F",
    "F#", "Gb", // synonyms
    "G",
    "G#", "Ab", // synonyms
    "A",
    "A#", "Bb", // synonyms
    "B",
  };

  vector<string> suffix = {
    "", "maj", "major", // synonyms
    "m", "min", "minor", // synonyms
    "maj7", "maj9", "maj11", "maj13", "maj9#11", "maj13#11",
    "6", "add9", "6add9", "maj7b5", "maj7#5", "m7", "m9", "m11",
    "m13", "m6", "madd9", "m6add9", "mmaj7", "mmaj9", "m7b5", "m7#5",
    "7", "9", "11", "13", "7sus4", "7b5", "7#5", "7b9", "7#9", "9b5",
    "9#5", "13#11", "13b9", "11b9", "aug", "dim", "dim7", "5", "sus4",
    "sus2", "sus2sus4",
    "-5", // XXX synonym for 5?
    // XXX "o" = "dim"?
    // XXX "o7" = "dim7"?
    // XXX "+" = "aug"
    // more to consider here... 6/9, 13sus4, 5add9, ...
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
  // Just any [bracketed] chord. It's normal for this to appear
  // without surrounding whitespace (e.g. before the second
  // syllable in a lyric).
  line_with_crd_re = new RE2(".*\\[" + standard_chord + "\\].*");
      
  // Needs to find longest match or else we just parse "D7 G" as
  // "D" and fail to continue consuming the line.
  RE2::Options longest;
  longest.set_longest_match(true);
  extract_chord_line_re = new RE2(ALLOW_WS_RE "(" + standard_chord + ")",
				  longest);

  RE2::Options first;
  first.set_longest_match(false);
  // Get the first bracketed component on the line (which may not
  // be a chord).
  extract_bracketed_re = new RE2("[^[]*\\[([^]]*)\\]", first);

  // Allow a line of chords to be like "intro: C G Am G"
  intro_line_re = new RE2(" *[Ii][Nn][Tt][Rr][Oo][ :]+(.*)");
  
  // TODO: To tests
  CHECK(RE2::FullMatch("Db", *standard_chord_re));
  CHECK(RE2::FullMatch("D7", *standard_chord_re));
  CHECK(RE2::FullMatch("G7", *standard_chord_re));  
  CHECK(RE2::FullMatch("G7#5", *standard_chord_re));  
  CHECK(RE2::FullMatch("Gb7sus4", *standard_chord_re));
  CHECK(RE2::FullMatch("Em/F#", *standard_chord_re));
  CHECK(RE2::FullMatch("G", *line_of_chords_re));
  CHECK(RE2::FullMatch(" G", *line_of_chords_re));
  CHECK(RE2::FullMatch("G ", *line_of_chords_re));
  CHECK(RE2::FullMatch("Em/F#\tGb7sus4 ", *line_of_chords_re));
  CHECK(RE2::FullMatch("\tEm/F#\tGb7sus4", *line_of_chords_re));        
  CHECK(RE2::FullMatch("Sea to shining [C]sea", *line_with_crd_re));
  CHECK(RE2::FullMatch("[Csus4]Sea to shining sea", *line_with_crd_re));
  CHECK(RE2::FullMatch("[fast] Sea to shi[G#]ning sea", *line_with_crd_re));
  CHECK(RE2::FullMatch("C] Sea to shi[G#]ning sea", *line_with_crd_re));
  CHECK(RE2::FullMatch("[C Sea to shi[G#]ning sea", *line_with_crd_re));    
  string rest;
  CHECK(RE2::FullMatch("INTRO: C G Am G", *intro_line_re, &rest) &&
	rest == "C G Am G");
}

ChordParser::~ChordParser() {
  delete standard_chord_re;
  delete line_of_chords_re;
  delete extract_chord_line_re;
  delete line_with_crd_re;
  delete extract_bracketed_re;
  delete intro_line_re;
}

ChordParser::Parsed ChordParser::ExtractChords(const string &body) {
  // Really simple chord parsing to start.
  vector<string> lines = Util::SplitToLines(body);

  // All chords in the song.
  vector<string> chords;

  int intro = 0;
  // Does the line just have chords, separated by whitespace?
  int cl = 0;
  // How about CRD notation like [G]this?
  int crdl = 0;
  for (const string &line : lines) {
    string stripped;
    if (RE2::FullMatch(line, *intro_line_re, &stripped)) {
      intro++;
    } else {
      stripped = line;
    }

    // First, filter to see if the line is suitable.
    if (RE2::FullMatch(line, *line_of_chords_re)) {
      cl++;

      re2::StringPiece l(line);

      vector<string> line_chords;
      string chord;
      while (RE2::Consume(&l, *extract_chord_line_re, &chord)) {
	line_chords.push_back(std::move(chord));
      }

      if (AcceptLineChords(line_chords)) {
	chords.reserve(chords.size() + line_chords.size());
	for (string &s : line_chords)
	  chords.push_back(std::move(s));
      }
    } else if (RE2::FullMatch(line, *line_with_crd_re)) {
      crdl++;

      re2::StringPiece l(line);

      // TODO: Reject labeled tablature like this:
      // [E]--------------------------------------------|
      // [B]--------------------------------------------|
      // [G]--------------------------------------------|
      // ...

      
      vector<string> line_chords;
      string bracketed;
      while (RE2::Consume(&l, *extract_bracketed_re, &bracketed) &&
	     RE2::FullMatch(bracketed, *standard_chord_re)) {
	line_chords.push_back(std::move(bracketed));
      }

      if (AcceptCrdChords(line_chords)) {
	chords.reserve(chords.size() + line_chords.size());
	for (string &s : line_chords)
	  chords.push_back(std::move(s));
      }
    }
  }


  // Here we declare that every song is produced from a cycle of
  // chords, A B C. This trivially includes cycles repeated only
  // once (and so any song can be expressed this way), and we also
  // allow the cycle to end early (so A B C A B is ok). Find the
  // shortest prefix of the chords that explains the song.
  //
  // (Consider if we actually want to do this hierarchically?
  // Usually the verse itself repeats, too.)

  auto PrefixExplainsSong = [&chords](int length) {
      CHECK(length <= (int)chords.size());
      for (int i = 0; i < (int)chords.size(); i++) {
	if (chords[i] != chords[i % length])
	  return false;
      }
      return true;
    };

  int truncated = 0;
  for (int len = 1; len <= (int)chords.size(); len++) {
    if (PrefixExplainsSong(len)) {
      truncated += chords.size() - len;
      chords.resize(len);
      break;
    }
  }

  Parsed ret;
  ret.chords = std::move(chords);
  ret.lines = (int)lines.size();
  ret.chord_lines = cl;
  ret.crd_lines = crdl;
  ret.intro_lines = intro;
  ret.chords_truncated = truncated;

  return ret;
}
