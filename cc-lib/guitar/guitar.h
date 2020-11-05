// Guitar chord fingering database.
// Data is compiled in; just link in guitar.cc to use.
//
// Warning: There are definitely some bugs in the database
// that this is derived from; please suggest any fixes!

#ifndef _CC_LIB_GUITAR_H
#define _CC_LIB_GUITAR_H

#include <string>
#include <cstdint>
#include <string_view>
#include <optional>
#include <array>
#include <vector>

struct Guitar {
  // An abstract chord (not fingering), which consists
  // of some base key (e.g. C#) and a suffix (e.g. sus4).
  // The chord is packed into a 16-bit integer, but the
  // representation is intended to be opaque; use the 
  // routines below.
  using Chord = uint16_t;

  // E a d g b e.
  // -1 means mute, 0 means open, n means finger then nth fret.
  using Fingering = std::tuple<int, int, int, int, int, int>;
  
  // Returns -1 if not any known base, suffix.
  static int BaseNum(std::string_view s);
  static int SuffixNum(std::string_view s);

  // Requires 0 <= base < NUM_BASES and 0 <= suffix < NUM_SUFFIXES.
  static Chord ChordOf(int base, int suffix);
  
  // Parse a full chord name, like "C#sus4".
  // Some variations are available in parsing; "C#"
  // can be written as "Db" (with any suffix),
  // "Cmajor" and "Cmaj" are synonyms for "C",
  // "Cminor" and "Cmin" are synonyms for "Cm".
  static std::optional<Chord> Parse(std::string_view s);

  // Render a chord as a string like "C#madd9". Chord must be valid,
  // but can produce nonsensical output like "D/D" for nonsensical
  // input. Parse can parse any output of this function.
  static std::string ChordString(Chord c);
  // Like "x32010" for C major. After 9, abcdef... are used.
  static std::string FingeringString(Fingering f);
  
  // Can return an empty vector for a chord that doesn't
  // make sense ("D/D") or for which no fingerings are known.
  // Aborts if the base/suffix in the chord are invalid, though.
  static std::vector<Fingering> GetFingerings(Chord c);

  // Look up a chord by its fingering.
  // There are a few fingerings that have ambiguous names (either
  // due to bugs in the database or because the chords get so fancy
  // that we get into opinion territory); this just chooses one
  // arbitrarily. Guaranteed to be consistent with GetFingerings.
  static std::optional<Chord> NameFingering(Fingering f);
  
  static constexpr int NUM_BASES = 12;
  static constexpr std::array<const char *, NUM_BASES> BASES = {
    "C","C#","D","Eb","E","F","F#","G","Ab","A","Bb","B",
  };

  static constexpr int NUM_SUFFIXES = 65;
  static constexpr std::array<const char *, NUM_SUFFIXES> SUFFIXES = {
    "", // aka. "major"
    "m", // aka. "minor"
    "dim", "dim7", "sus2", "sus4", "sus2sus4", "7sus4", "alt", "aug", "5", "6",
    "69", // aka. "6/9", "6add9"
    "7", "7b5", "aug7", "9", "9b5", "aug9", "7b9", "7#9", "11", "9#11", "13",
    "maj7", "maj7b5", "maj7#5", "maj9", "maj11", "maj13", "m6",
    "m69", // aka. "m6/9", "m6add9"
    "m7", "m7b5", "m9", "m11", "mmaj7", "mmaj7b5", "mmaj9", "mmaj11", "add9",
    "madd9", "7/G", "/E", "/F", "/F#", "/G", "/G#", "/A", "/Bb", "/B", "/C",
    "/C#", "m/B", "m/C", "m/C#", "/D", "m/D", "/D#", "m/D#", "m/E", "m/F",
    "m/F#", "m/G", "m/G#",
  };
  
};

#endif
