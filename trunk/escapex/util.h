
// XXX: Merge this with cc-lib's Util class.

#ifndef __UTIL_H
#define __UTIL_H

#include <cstdlib>
#include <map>
#include <stdio.h>
#include <stdlib.h>
#include <string>

using namespace std;

#define ATTIC_DIR "attic"

#ifdef WIN32
#   define DIRSEP  "\\"
#   define DIRSEPC '\\'
#else
#   define DIRSEP  "/"
#   define DIRSEPC '/'
#endif

#define UTIL_PI 3.141592653589f

string readfile(string);
bool writefile(string fn, string s);

string itos(int i);

int shout(int, string, unsigned int &);
string shint(int b, int i);
/* converts int to byte string that represents it */
string sizes(int i);

template <class T>
struct vallist {
  T head;
  vallist<T> *next;
  vallist<T>(T h, vallist<T> *n) : head(h), next(n) {}
  static void push(vallist<T> *&sl, T h) {
    sl = new vallist<T>(h, sl);
  }

  static T pop(vallist<T> *&sl, T u) {
    if (sl) {
      vallist<T> *tmp = sl;
      T t = tmp->head;
      sl = sl->next;
      delete tmp;
      return t;
    } else return u;
  }

  /* ie, destroy */
  static void diminish(vallist<T> *&sl) {
    T dummy;
    while (sl != nullptr) pop(sl, dummy);
  }

  void destroy() {
    diminish(this);
  }

  int length() {
    int res = 0;
    vallist<T> *tmp = this;
    while (tmp) {
      tmp = tmp->next;
      res++;
    }
    return res;
  }

  static vallist<T> *copy(vallist<T> *sl) {
    if (sl) {
      return new vallist<T>(sl->head,
                            copy(sl->next));
    } else return 0;
  }
};

typedef vallist<string> stringlist;
inline string stringpop(stringlist *&sl) {
  return stringlist::pop(sl, "");
}

/* drawing lines with Bresenham's algorithm */
struct line {
  static line *create(int x0, int y0, int x1, int y1);
  virtual void destroy() = 0;
  virtual bool next(int &x, int &y) = 0;
  virtual ~line() {};
};

struct util {
  /* only read if the file begins with the magic string */
  static bool hasmagic(string, const string &magic);
  static string readfilemagic(string, const string &magic);

  static int stoi(string s);

  static string ptos(void *);
  static unsigned int hash(string s);
  /* give /home/tom/ of /home/tom/.bashrc */
  static string pathof(string s);
  static string fileof(string s);

  static string ensureext(string f, string ext);
  static string lcase(string in);
  static string ucase(string in);

  static bool existsfile(string);

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
  static int natural_compare(const string &l, const string &r);

  /* Same as above, but ignore 'the' at the beginning */
  static int library_compare(const string &l, const string &r);

  /* Is string s alphabetized under char k? */
  static bool library_matches(char k, const string &s);

  /* open a new file. if it exists, return 0 */
  static FILE *open_new(string s);
  /* 0 on failure */
  static int changedir(string s);
  static int random();
  /* random in 0.0 .. 1.0 */
  static float randfrac();
  static int getpid();
  /* anything ending with \n. ignores \r.
     modifies str. */
  static string getline(string &str);
  /* same, for open file. */
  static string fgetline(FILE *f);

  /* chop the first token (ignoring whitespace) off
     of line, modifying line. */
  static string chop(string &line);

  /* number of entries (not . or ..) in dir d */
  static int dirsize(string d);

  /* mylevels/good_tricky   to
     mylevels               to
     . */
  static string cdup(const string &dir);

  /* true iff big ends with small */
  static bool endswith(string big_, string small_);
  /* starts */
  static bool startswith(string big_, string small_);

  /* split the string up to the first
     occurrence of character c. The character
     is deleted from both the returned string and
     the line */
  static string chopto(char c, string &line);

  /* erase any whitespace up to the first
     non-whitespace char. */
  static string losewhitel(const string &s);

  /* try to remove the file. If it
     doesn't exist or is successfully
     removed, then return true. */
  static bool remove(string f);

  /* move a file from src to dst. Return
     true on success. */
  static bool move(string src, string dst);

  /* make a copy by reading/writing */
  static bool copy(string src, string dst);

  // Not thread safe!
  static string tempfile(string suffix);

  /* does this file exist and is it a directory? */
  static bool isdir(string s);

  /* same as isdir */
  static bool existsdir(string);

  static bool makedir(string);

  /* try to launch the url with the default browser;
     doesn't work on all platforms. true on success */
  static bool launchurl(const string &);

  /* creates directories for f */
  static void createpathfor(string f);

  /* open, creating directories if necessary */
  static FILE *fopenp(string f, string mode);

  /* move a file into an 'attic' backup dir */
  static void toattic(string f);

  /* replace all occurrences of 'findme' with 'replacewith' in 'src' */
  static string replace(string src, string findme, string replacewith);

  /* returns false if failed/unsupported */
  static bool setclipboard(string);

  /* templates follow. */

  /* Return m[key] if it already exists, or allocate a new entry,
     insert it, and return that. */
  template <class K, class V>
  static V *findorinsertnew(map<K, V*> &m, const K &key) {
    V *&pos = m[key];
    if (!pos) pos = new V;
    return pos;
  }
};

#endif
