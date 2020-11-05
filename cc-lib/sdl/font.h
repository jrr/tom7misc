
#ifndef _CC_LIB_SDL_FONT_H
#define _CC_LIB_SDL_FONT_H

#include <string>

struct SDL_Surface;

/* XXX move some of this to fontutil */
// Everything here is thread-safe, though concurrent draws to the same
// surface are potentially dangerous.
// TODO: Label const.
struct Font {
  // TODO: make these const.
  int width, height, styles, overlap;

  static Font *create(SDL_Surface *screen,
		      const std::string &file,
		      const std::string &charmap,
		      int width,
		      int height,
		      int styles = 1,
		      int overlap = 0,
		      int dims = 2);

  // Same as create, but enlarge the pixels in the font to be size px x px.
  // All of the other parameters should be given in the original font size,
  // (as though calling create()).
  static Font *CreateX(int px,
		       SDL_Surface *screen,
		       const std::string &file,
		       const std::string &charmap,
		       int width,
		       int height,
		       int styles = 1,
		       int overlap = 0,
		       int dims = 2);
  
  // Takes ownership of the font surface.
  static Font *create_from_surface(SDL_Surface *screen,
				   SDL_Surface *font_surface,
				   const std::string &charmap,
				   int width,
				   int height,
				   int styles = 1,
				   int overlap = 0,
				   int dims = 2);

  /* number of drawn characters, ignoring control codes.
     length(s) * (width-overlap) gives
     the screen width. */
  static unsigned int length(const std::string &s);
  static std::string substr(const std::string &s,
			    unsigned int start,
			    unsigned int len);
  /* len must be <= font::length(s) */
  static std::string prefix(const std::string & s,
			    unsigned int len);
  static std::string suffix(const std::string & s,
			    unsigned int len);

  /* similarly, pad a string out to n displayable
     characters, doing the right thing for
     color codes. If n is negative, pad with spaces
     on the left side instead of right.

     precondition: |n| >= 3 */
  static std::string pad(const std::string &s, int n);

  /* truncate to n chars if too long; if n is
     negative, truncate off the left side instead of
     the right. */
  static std::string truncate(const std::string &s, int n);


  /* return the size in pixels of the string. 
     formatting characters do not count towards width.
     sizey is always font.height.
  */
  virtual int sizex(const std::string &) = 0;
  /* Same, but not interpreting formatting characters as special,
     for draw_plain. */
  virtual int sizex_plain(const std::string &) = 0;

  /* returns the number of lines in the string,
     which is always at least 1 unless the
     string is empty. */
  static int lines(const std::string &);

  /* specify the top-left pixel. */
  virtual void draw(int x, int y, const std::string &s) = 0;
  virtual void draw_plain(int x, int y, const std::string &s) = 0;
  virtual void drawto(SDL_Surface *, int x, int y, const std::string &s) = 0;
  virtual void drawto_plain(SDL_Surface *, int x, int y, const std::string &s) = 0;

  /* draw a multi-line string.
     the height in pixels of the area drawn is returned. */
  virtual int drawlines(int x, int y, const std::string &s) = 0;

  /* same, but centers each line horizontally about the x position */
  virtual int drawcenter(int x, int y, const std::string &s) = 0;

  // Deprecated. Just call destructor.
  virtual void destroy() = 0;
  
  virtual ~Font();
};

#endif
