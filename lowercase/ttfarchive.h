
#ifndef LOWERCASE_TTFARCHIVE_H
#define LOWERCASE_TTFARCHIVE_H

#include <vector>
#include <string>

static constexpr const char *DIRS[] = {
  "d:\\temp\\fonts2020",
  // "d:\\temp\\fonts2020\\SummitSoftCreativeFonts",  
};

struct Ttfarchive {
  static void AddAllFilesRec(const std::string &dir, std::vector<std::string> *all_files);
  
  static std::string Frontslash(const std::string &s);
  static std::string Backslash(const std::string &s);
};

#endif
