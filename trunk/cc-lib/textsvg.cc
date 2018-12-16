
#include "textsvg.h"

#include <string>
#include <stdio.h>
#include <utility>
#include <cmath>
#include <vector>

using namespace std;

string TextSVG::Header(double width, double height) {
  char out[512];
  sprintf(out,
	  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	  "<!-- Generator: cc-lib/textsvg.cc -->\n"
	  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" "
	  "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" [\n"
	  "<!ENTITY ns_flows \"http://ns.adobe.com/Flows/1.0/\">\n"
	  "]>\n"
	  "<svg version=\"1.1\"\n"
	  " xmlns=\"http://www.w3.org/2000/svg\""
	  " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
	  " xmlns:a=\"http://ns.adobe.com/AdobeSVGViewerExtensions/3.0/\""
	  " x=\"0px\" y=\"0px\" width=\"%fpx\" height=\"%fpx\""
	  " xml:space=\"preserve\">\n",
	  width, height);
  return (string)out;
}

string TextSVG::Footer() {
  return "</svg>\n";
}

string TextSVG::Rtos(double d) {
  // This is probably wrong for svg--but what?
  if (std::isnan(d)) return "NaN";

  printf("Called on %.9f\n", d);
  // Handle minus sign ourselves. Note subtlety about negative
  // zero; testing e.g. (d < 0) fails because c++ treats
  // -0 as == 0, but printf still prints the minus sign.
  const bool negative = std::signbit(d);
  if (negative) d = abs(d);
  
  char out[16];
  // Make sure there is always room for a minus sign.
  sprintf(out + 1, "%.5f", d);
  printf("Orig [%s]\n", out + 1);
  // Strip leading zeroes. We always have a nonzero character
  // (even the terminating \0) so this loop terminates.
  char *o = out + 1;
  while (*o == '0') o++;

  // Now strip trailing zeroes. e is where we will truncate
  // the string (place a \0) if we reach the end, or nullptr
  // if we are not looking at a run of zeroes.
  char *e = nullptr, *f = o;
  while (*f != '\0') {
    if (e != nullptr) {
      // Have a run of zeroes. Extend?
      if (*f != '0')
	e = nullptr;
    } else {
      // No run. Start?
      if (*f == '0')
	e = f;
    }
    f++;
  }

  if (e != nullptr) *e = '\0';

  // If we stripped all the leading and trailing zeroes, only
  // leaving the decimal point, then return zero. We do this
  // rather than testing for zero up front because numbers
  // very close to zero still get rounded.
  if (o[0] == '.' && o[1] == '\0')
    return "0";
  
  if (negative) {
    // Okay even if we didn't strip zeroes, since we wrote to
    // out + 1 above.
    o--;
    *o = '-';
  }
  
  return string{o};
}

static string Replace(string src, const string &findme, const string &rep) {
  auto idx = src.length() - 1;

  if (findme.length() < 1) return src;

  /* idx represents the position in src which, for all chars greater
     than it, there begins no match of findme */
  for (;;) {
    idx = src.rfind(findme, idx);
    if (idx == string::npos)
      break;
    /* do replacement */
    src.replace(idx, findme.length(), rep);
    /* don't allow any matches to extend into the string we just inserted;
       (consider replacing "abc" with "bcd" in the string "aabc") */
    if (findme.length() > idx)
      break;
    idx -= findme.length();
  }
  return src;
}

// XXX Need to escape for SVG. This is insufficient.
static string Escape(string s) {
  s = Replace(s, "<", "&lt;");
  s = Replace(s, ">", "&gt;");
  s = Replace(s, "\"", "&quot;");
  return s;
}

string TextSVG::Text(
    double x, double y,
    const string &face,
    double size,
    const std::vector<std::pair<std::string, std::string>> &text) {
  auto OneText = [](pair<string, string> p) {
    return (string)"<tspan fill=\"" + p.first + "\">" +
      Escape(p.second) + "</tspan>";
  };

  string ret =
    "<text x=\"" + Rtos(x) + "\" y=\"" + Rtos(y) + "\" font-family=\"" +
    face + "\" font-size=\"" + Rtos(size) + "\">";
  for (const auto &p : text)
    ret += OneText(p);
  ret += "</text>";
  return ret;
}
