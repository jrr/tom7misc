
#ifndef _ESCAPE_HTTPUTIL_H
#define _ESCAPE_HTTPUTIL_H

#include <string>

struct HTTPUtil {
  static std::string URLEncode(const std::string &);
};

enum formtype { FT_ARG, FT_FILE, };

struct formalist {
  formtype ty;
  std::string name;
  std::string filename;
  std::string content;
  formalist *next;

  static void pusharg(formalist *&l, std::string name, std::string arg) {
    formalist *x = new formalist;
    x->next = l;
    x->ty = FT_ARG;
    x->name = name;
    /* this appears to be too many */
    x->content = arg; // HTTPUtil::URLEncode(arg);
    l = x;
  }

  static void pushfile(formalist *&l, std::string name,
                       std::string filename, std::string contents) {
    formalist *x = new formalist;
    x->next = l;
    x->ty = FT_FILE;
    x->name = name;
    x->filename = filename;
    x->content = contents;
    l = x;
  }

  /* ie, destroy */
  static void diminish(formalist *&sl) {
    while (sl) {
      formalist *tmp = sl;
      sl = sl->next;
      delete tmp;
    }
  }

};


#endif
