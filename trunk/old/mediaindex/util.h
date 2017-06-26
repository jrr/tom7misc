
#ifndef __UTIL_H
#define __UTIL_H

#include <string>
using namespace std;

#ifdef WIN32
#   define DIRSEP  "\\"
#   define DIRSEPC '\\'
#else
#   define DIRSEP  "/"
#   define DIRSEPC '/'
#endif

bool existsfile(string);
string readfile(string);
/* only read if the file begins with the magic string */
string readfilemagic(string, string magic);

int writefile(string fn, string s);

string itos(int i);
int stoi(string s);

bool isdir(string s);

int shout(int, string, unsigned int &);
string shint(int b, int i);
string sizes(int i);

struct stringlist {
  string head;
  stringlist * next;
  stringlist(string h, stringlist * n) : head(h), next(n) {}
  static void push(stringlist *& sl, string h) {
    sl = new stringlist(h, sl);
  }
  static string pop(stringlist *& sl) {
    if (sl) {
      stringlist * tmp = sl;
      string t = tmp->head;
      sl = sl->next;
      delete tmp;
      return t;
    } else return "";
  }
  /* ie, destroy */
  static void diminish(stringlist *& sl) {
    while (sl) pop(sl);
  }

  int length() {
    int res = 0;
    stringlist * tmp = this;
    while (tmp) {
      tmp = tmp -> next;
      res ++;
    }
    return res;
  }

  static stringlist * copy(stringlist * sl) {
    if (sl) {
      return new stringlist(sl->head, 
			    copy(sl->next));
    } else return 0;
  }

};

template <class P>
struct ptrlist {
  P * head;
  ptrlist<P> * next;
  ptrlist<P>(P * h, ptrlist<P> * n) : head(h), next(n) {}

  static void push(ptrlist<P> *& sl, P * h) {
    sl = new ptrlist<P>(h, sl);
  }

  static P * pop(ptrlist<P> *& sl) {
    if (sl) {
      ptrlist<P> * tmp = sl;
      P * t = tmp->head;
      sl = sl->next;
      delete tmp;
      return t;
    } else return 0;
  }

  /* ie, destroy. does not destroy the
     heads! */
  static void diminish(ptrlist<P> *& pl) {
    while (pl) pop(pl);
  }

  int length() {
    int res = 0;
    ptrlist<P> * tmp = this;
    while (tmp) {
      tmp = tmp -> next;
      res ++;
    }
    return res;
  }

  static ptrlist<P> * copy(ptrlist<P> * sl) {
    if (sl) {
      return new ptrlist<P>(sl->head, 
			    copy(sl->next));
    } else return 0;
  }

};

/* drawing lines with Bresenham's algorithm */
struct line {
  static line * create(int x0, int y0, int x1, int y1);
  virtual void destroy() = 0;
  virtual bool next(int & x, int & y) = 0;
  virtual ~line() {};
};

struct util {
  static unsigned int hash(string s);
  /* give /home/tom/ of /home/tom/.bashrc */
  static string pathof(string s);
  static string fileof(string s);

  static string ensureext(string f, string ext);
  static string lcase(string in);

  /* open a new file. if it exists, return 0 */
  static FILE * open_new(string s);
  static int changedir(string s);
  static int random();
  static int getpid();
  /* anything ending with \n. ignores \r.
     modifies str. */
  static string getline(string & str);
  /* chop the first token (ignoring whitespace) off
     of line, modifying line. */
  static string chop(string & line);

  /* split the string up to the first
     occurrence of character c. The character
     is deleted from both the returned string and
     the line */
  static string chopto(char c, string & line);

  /* erase any whitespace up to the first 
     non-whitespace char. */
  static string losewhitel(const string & s);

  /* try to remove the file. If it
     doesn't exist or is successfully
     removed, then return true. */
  static bool remove(string f);
  
  /* move a file from src to dst. Return
     true on success. */
  static bool move(string src, string dst);

  /* make a copy by reading/writing */
  static bool copy(string src, string dst);

  static string tempfile(string suffix);

  /* same as isdir */
  static bool existsdir(string);

  static bool makedir(string);

  /* creates directories for f */
  static void createpathfor(string f);

  /* open, creating directories if necessary */
  static FILE * fopenp(string f, string mode);

  /* move a file into an 'attic' backup dir */
  static void toattic(string f);

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
};


#endif
