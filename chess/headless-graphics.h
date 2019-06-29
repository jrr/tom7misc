#ifndef __HEADLESS_GRAPHICS_H
#define __HEADLESS_GRAPHICS_H

#include <vector>
#include <string>
#include <cstdint>

// Write the pixel into the surface. Replaces alpha channel; does not blend.
void SetPixel(int w, int h, int x, int y,
	      uint8_t r, uint8_t g, uint8_t b, uint8_t a,
	      std::vector<uint8_t> *rgba);

// Fill rectangle with the given color; replaces alpha channel.
void FillRect(int w, int h,
	      int rectx, int recty, int rectw, int recth,
	      uint8_t r, uint8_t g, uint8_t b, uint8_t a,
	      std::vector<uint8_t> *rgba);

void SaveARGB(const std::vector<uint8_t> &argb, int width, int height,
	      const std::string &filename);

void SaveRGBA(const std::vector<uint8_t> &rgba, int width, int height,
	      const std::string &filename);

inline constexpr uint8_t Mix4(uint8_t v1, uint8_t v2, uint8_t v3, uint8_t v4) {
  return (uint8_t)(((uint32_t)v1 + (uint32_t)v2 +
		    (uint32_t)v3 + (uint32_t)v4) >> 2);
}

struct HeadlessFont {
  const int char_width, char_height, styles, overlap;

  // Returns nullptr if the file can't be opened, etc.
  static HeadlessFont *Create(const std::string &file,
			      const std::string &charmap,
			      int char_width,
			      int char_height,
			      int styles = 1,
			      int overlap = 0);

  /* number of drawn characters, ignoring control codes.
     length(s) * (width-overlap) gives
     the screen width. */
  static int Length(const std::string &s);

  /* return the size in pixels of the string. 
     formatting characters do not count towards width.
     sizey is always font.height.
  */
  int SizeX(const std::string &);
  /* Same, but not interpreting formatting characters as special,
     for draw_plain. */
  int SizeXPlain(const std::string &);

  /* specify the top-left pixel. */
  void Draw(int x, int y, const std::string &s,
	    std::vector<uint8_t> *rgba,
	    int width, int height);
  void DrawPlain(int x, int y, const std::string &s,
		 std::vector<uint8_t> *rgba,
		 int width, int height);
  
private:
  HeadlessFont(int char_width,
	       int char_height,
	       int styles,
	       int overlap);
  void DrawChar(uint8_t c, int style,
		int dstx, int dsty,
		std::vector<uint8_t> *rgba,
		int width, int height);


  // font pixel data in rgba
  std::vector<uint8_t> font;
  int font_width = 0, font_height = 0;
  int chars[256] = {};
};

#endif
