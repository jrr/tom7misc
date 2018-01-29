
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

static string Rtos(double d) {
  // This is probably wrong for svg--but what?
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

static string Replace(string src, const string &findme, const string &rep) {
  auto idx = src.length() - 1;

  if (findme.length() < 1) return src;

  /* idx represents the position in src which, for all chars greater
     than it, there begins no match of findme */
  while (idx >= 0) {
    idx = src.rfind(findme, idx);
    if (idx == string::npos)
      break;
    /* do replacement */
    src.replace(idx, findme.length(), rep);
    /* want to ensure termination even if rep contains findmes */
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
