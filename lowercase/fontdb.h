
#ifndef _LOWERCASE_FONTDB_H
#define _LOWERCASE_FONTDB_H

#include <string>
#include <vector>
#include <cstdint>
#include <unordered_map>
#include <utility>
#include <optional>
#include <map>
#include <algorithm>

#include "base/logging.h"
#include "base/stringprintf.h"

// Keeps track of properties of fonts.
// Fonts have at most one type.
// Separately, we can record flags.
struct FontDB {
  static constexpr const char *DATABASE_FILENAME = "font-db.txt";
  using int64 = int64_t;
  using string = std::string;
  
  enum class Type {
    // good clean fonts. These should have "normal-looking"
    // letters with good construction, no effects like outlines etc.
    // different weights and obliques/italics are ok.
    SANS,
    SERIF,
    // Including cursive, blackletter, caligraphic
    FANCY,

    // Cyber-fonts, pixel outlines, etc.
    TECHNO,

    // Clean fonts that are not "fancy" or "techno" but have some other
    // decorative style that makes the letter shapes not be normal (e.g.
    // "old west" font).
    DECORATIVE,

    // Scans or distressed fonts with lots of control points. Might work
    // if rendered to bitmaps.
    MESSY,

    // Anything that's not actually letters.
    DINGBATS,

    // Font is working but doesn't fit above categories.
    // Anything with effects like outlines goes in here.
    OTHER,

    // Something is afoot, e.g. metrics look wrong, missing lowercase,
    // A-Z is some other language, something weird with the rendering, etc.
    BROKEN,

    // Default, unknown state.
    UNKNOWN,
  };

  // Each flag can be true, false, unknown.
  enum class Flag {
    SAME_CASE,
  };


  static string GetBaseFilename(const string &ff) {
    int slash = ff.rfind("\\");
    return slash == string::npos ? ff : ff.substr(slash + 1, string::npos);
  }
  
  // Returns the true state, false state.
  // All chars should be distinct!
  static const pair<char, char> FlagChar(Flag f) {
    switch (f) {
    case Flag::SAME_CASE:
      return {'C', 'c'};
    default:
      LOG(FATAL) << "Bad flag?";
      return {'X', 'x'};
    }
  }

  static const pair<Flag, bool> CharFlag(char c) {
    switch (c) {
    default:
      LOG(FATAL) << "Bad flag char";
      // Fallthrough to suppress warnings.
    case 'C': return {Flag::SAME_CASE, true};
    case 'c': return {Flag::SAME_CASE, false};
    }
  }

  static const char *TypeString(Type t) {
    switch (t) {
    case Type::SANS: return "sans";
    case Type::SERIF: return "serif";
    case Type::FANCY: return "fancy";
    case Type::TECHNO: return "techno";
    case Type::DECORATIVE: return "decorative";
    case Type::MESSY: return "messy";
    case Type::DINGBATS: return "dingbats";
    case Type::OTHER: return "other";
    case Type::BROKEN: return "broken";
    case Type::UNKNOWN: return "unknown";
    default:
      LOG(FATAL) << "Bad Type?";
      return "";
    }
  }

  struct Info {
    Type type = Type::UNKNOWN;
    std::map<Flag, bool> flags;
    // In [0,1]. Negative means unknown/incalculable.
    float bitmap_diffs = -1.0f;
  };

  FontDB();

  bool Dirty() const {
    return dirty;
  }

  // XXX can probably assume success, fail if not
  std::optional<Info> Lookup(const string &s) const {
    auto it = files.find(s);
    if (it == files.end()) return {};
    else return {it->second};
  }

  void SetBitmapDiffs(const string &s, float bitmap_diffs = -1.0f) {
    files[s].bitmap_diffs = bitmap_diffs;
    dirty = true;
  }
  
  void AssignType(const string &s, Type t) {
    if (files[s].type != Type::UNKNOWN) num_sorted--;
    files[s].type = t;
    if (files[s].type != Type::UNKNOWN) num_sorted++;
    dirty = true;
  }

  void SetFlag(const string &s, Flag flag, bool on) {
    files[s].flags[flag] = on;
    dirty = true;
  }

  int64 NumSorted() const {
    return num_sorted;
  }
  
  void Save();
  
  int64 Size() const {
    return files.size();
  }

  const std::unordered_map<string, Info> &Files() const {
    return files;
  }
  
  
 private:
  static string FlagString(const std::map<Flag, bool> &flags) {
    if (flags.empty()) return "_";
    string ret;
    for (const auto [flag, val] : flags) {
      const auto [tc, fc] = FlagChar(flag);
      ret.push_back(val ? tc : fc);
    }
    return ret;
  }

  int64 num_sorted = 0;
  std::unordered_map<string, Info> files;
  bool dirty = false;
};

#endif
