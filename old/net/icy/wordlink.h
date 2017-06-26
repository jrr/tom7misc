#ifndef _TM7_WORDLINK_H
#define _TM7_WORDLINK_H

#include <string>

enum linkway {HOW_NOTHING, HOW_SPACE, HOW_DOT, HOW_DOTSPACE, 
	      HOW_COMMA, HOW_BANG, HOW_SEMI, HOW_COLON,
	      HOW_DOTDOTDOT, HOW_QUESTION, HOW_AMPERSAND, };

#define WORD_START 0
#define WORD_END   1

struct wnode {
  int idx;
  int metric;
  linkway how;
  wnode * next;
};

struct wordlink {
  ~wordlink() {
    /* // don't delete, cos delete [] words in resize.
      while (links) {
    
      wnode * t = links;
      links=links->next;
      delete t;
      }
    */
  }

  wordlink () { links = NULL; }
  wnode * links;
  string word;
};

#endif
