
#ifndef __CCLIB_TEXTSVG_H
#define __CCLIB_TEXTSVG_H

#include <string>
#include <utility>
#include <vector>

struct TextSVG {
  static std::string Header(double width, double height);
  static std::string Footer();

  // Returns a string containing a number suitable for SVG (with
  // up to 5 digits of precision); strips unnecessary leading and
  // trailing zeroes.
  static std::string Rtos(double d);
  
  // text vector is like
  // {{"#FFFFFF", "this text is "}, {"#0000FF", "blue"}}
  static std::string Text(
      double x, double y,
      const std::string &face,
      double size,
      const std::vector<std::pair<std::string, std::string>> &text);
};

#endif
