
#ifndef _CC_LIB_UTIL_H
#define _CC_LIB_UTIL_H

#include <cstdlib>
#include <map>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <cstdint>
#include <string_view>
#include <optional>

using namespace std;

#ifdef WIN32
#   define DIRSEP  "\\"
#   define DIRSEPC '\\'
#else
#   define DIRSEP  "/"
#   define DIRSEPC '/'
#endif

#define UTIL_PI 3.141592653589f

// Move stuff like this to Util struct or remove
string itos(int i);
int stoi(const string &s);
string dtos(double d);

struct Util {
  // No error handling; it just returns "".
  static string ReadFile(const string &filename);
  // Same but returns nullopt if the file can't be read.
  static std::optional<string> ReadFileOpt(const string &filename);
  
  static bool WriteFile(const string &filename, const string &contents);
  
  // Reads the lines in the file to the vector. Ignores all
  // carriage returns, including ones not followed by newline.
  static vector<string> ReadFileToLines(const string &f);

  static vector<string> SplitToLines(const string &s);

  // Calls f on each line (without the newline), streamed from
  // the file. Ignores \r. Suitable for very large files.
  template<class F>
  static void ForEachLine(const string &filename, F f);
  
  // As above, but treat the first token on each line as a map
  // key. Ignores empty lines.
  static map<string, string> ReadFileToMap(const string &f);

  static vector<uint8_t> ReadFileBytes(const string &f);
  static bool WriteFileBytes(const string &f, const vector<uint8_t> &b);

  // Read/write a vector of uint64s in big-endian byte order.
  static vector<uint64_t> ReadUint64File(const string &filename);
  static bool WriteUint64File(const string &filename,
			      const std::vector<uint64_t> &contents);
  
  static vector<string> ListFiles(const string &dir);

  // Join the strings in the input vector with the given delimiter.
  // Join({"a", "b", "c"}, ".") = "a.b.c"
  // Join({"z"}, ".") = "z"
  // Join({}, ".") = ""
  static string Join(const std::vector<std::string> &pieces,
		     const std::string &sep);

  // Split the string on the given character. The output
  // always contains at least one element; split("", "x")
  // returns {""}.
  static std::vector<string> Split(const std::string &s, char sep);
  
  // XXX terrible names
  static int shout(int, string, unsigned int &);
  static string shint(int b, int i);
  /* converts int to byte string that represents it */
  static string sizes(int i);

  /* only read if the file begins with the magic string */
  static bool HasMagic(string filename, const string &magic);
  static string ReadFileMagic(string filename, const string &magic);


  static string ptos(void *);
  static unsigned int hash(const string &s);
  /* give /home/tom/ of /home/tom/.bashrc */
  static string pathof(const string &s);
  static string fileof(const string &s);

  static string ensureext(string f, string ext);

  // Convert ASCII string to lowercase or uppercase.
  static string lcase(const string &in);
  static string ucase(const string &in);

  static bool ExistsFile(const string &f);

  /* dirplus("/usr/local", "core") and
     dirplus("/usr/local/", core")  both give  "/usr/local/core"
     dirplus("/usr/local", "/etc/passwd")  gives "/etc/passwd"  */
  static string dirplus(const string &dir, const string &file);

  /* spec is a character spec like "A-Z0-9`,."
     XXX document */
  static bool matchspec(string spec, char c);

  /* An ordering on strings that gives a more "natural" sort:
     Tutorial 1, ..., Tutorial 9, Tutorial 10, Tutorial 11, ...
     rather than
     Tutorial 1, Tutorial 10, Tutorial 11, ..., Tutorial 2, Tutorial 20, ...
  */
  static int natural_compare(const string & l, const string & r);

  /* Same as above, but ignore 'the' at the beginning */
  static int library_compare(const string & l, const string & r);

  /* Is string s alphabetized under char k? */
  static bool library_matches(char k, const string & s);

  /* open a new file. if it exists, return null */
  static FILE *open_new(string s);
  /* 0 on failure */
  static int changedir(string s);
  static int random();
  /* random in 0.0 .. 1.0 */
  static float randfrac();
  static int getpid();
  /* anything ending with \n. ignores \r.
     modifies str. */
  static string getline(string & str);
  /* same, for open file. */
  static string fgetline(FILE * f);

  /* chop the first token (ignoring whitespace) off
     of line, modifying line. eventually returns ""
     and line becomes empty. */
  static string chop(string &line);

  /* number of entries (not . or ..) in dir d */
  static int dirsize(string d);

  /* mylevels/good_tricky   to
     mylevels               to
     . 
     (Currently only handles relative paths.) */
  static string cdup(const string &dir);

  // True iff big ends with small.
  static bool EndsWith(string_view big, string_view small);
  // True iff big starts with small.
  static bool StartsWith(string_view big, string_view small);

  // If the s ends with the suffix, then strip it (in the string_view
  // version, it continues to refer to the same underlying data) and
  // return true.
  static bool TryStripSuffix(string_view suffix, string_view *s);
  static bool TryStripSuffix(string_view suffix, string *s);
  // Same, for prefix.
  static bool TryStripPrefix(string_view prefix, string_view *s);
  static bool TryStripPrefix(string_view suffix, string *s);
  
  /* split the string up to the first
     occurrence of character c. The character
     is deleted from both the returned string and
     the line */
  static string chopto(char c, string &line);

  /* erase any whitespace up to the first
     non-whitespace char. */
  static string losewhitel(const string &s);
  // Remove trailing whitespace.
  static string LoseWhiteR(string s);

  // Pads the string to n characters by appending spaces if
  // it is shorter. (Longer strings are unmodified.) If n is
  // negative, adds space on the left instead.
  static string Pad(int n, string s);
  // Same, with the given character instead of ' '.
  static string PadEx(int n, string s, char c);
  
  // All whitespace becomes a single space. Leading and trailing
  // whitespace is dropped.
  static string NormalizeWhitespace(const string &s);
  
  /* try to remove the file. If it
     doesn't exist or is successfully
     removed, then return true. */
  static bool remove(const string &f);

  /* move a file from src to dst. Return
     true on success. */
  static bool move(const string &src, const string &dst);

  /* make a copy by reading/writing */
  static bool copy(const string &src, const string &dst);

  static string tempfile(const string &suffix);

  /* does this file exist and is it a directory? */
  static bool isdir(const string &s);

  /* same as isdir */
  static bool existsdir(const string &d);

  static bool MakeDir(const string &s);

  /* try to launch the url with the default browser;
     doesn't work on all platforms. true on success */
  static bool launchurl(const string &);

  /* creates directories for f */
  static void CreatePathFor(const string &f);

  /* open, creating directories if necessary */
  static FILE *fopenp(const string &f, const string &mode);

  /* replace all occurrences of 'findme' with 'replacewith' in 'src' */
  static string Replace(string src, const string &findme,
			const string &replacewith);
  
  /* called minimum, maximum because some includes
     define these with macros, ugh */
  static int minimum(int a, int b) {
    if (a < b) return a;
    else return b;
  }

  static int maximum(int a, int b) {
    if (a > b) return a;
    else return b;
  }

  // Returns true if c is a hex digit (0-9a-fA-F). "Digit" is of course a
  // misnomer.
  static bool IsHexDigit(char c);
  // Returns 0-15 for valid hex digits (0-9a-fA-F) and arbitrary (really,
  // it's weird) values for other chars.
  static int HexDigitValue(char c);
};

/* drawing lines with Bresenham's algorithm.
   deprecated; please use lines.h
*/
struct line {
  static line *create(int x0, int y0, int x1, int y1);
  virtual void destroy() = 0;
  virtual bool next(int &x, int &y) = 0;
  virtual ~line() {};
};

// Note: bitbuffer used to be here; moved to bitbuffer.h

// Template implementations follow.

template<class F>
void Util::ForEachLine(const string &s, F f) {
  FILE *file = fopen(s.c_str(), "rb");
  if (!file) return;
  int c;
  string line;
  while ( (c = fgetc(file), c != EOF) ) {
    if (c == '\r') continue;
    if (c == '\n') {
      f(std::move(line));
      line.clear();
    } else {
      line += c;
    }
  }
  // Don't require trailing newline.
  if (!line.empty()) f(line);
  fclose(file);
}

#endif
