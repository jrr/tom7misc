
#ifndef __CCLIB_TEXTSVG_H
#define __CCLIB_TEXTSVG_H

#include <string>
#include <utility>
#include <vector>

struct TextSVG {
  static std::string Header(double width, double height);
  static std::string Footer();

  // text vector is like
  // {{"#FFFFFF", "this text is "}, {"#0000FF", "blue"}}
  static std::string Text(
      double x, double y,
      const std::string &face,
      double size,
      const std::vector<std::pair<std::string, std::string>> &text);
};

#endif
