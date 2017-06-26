#ifndef _TM7_LINKBANK_H
#define _TM7_LINKBANK_H

#include <string>
#include "wordlink.h"
#include <stdlib.h>
#include <time.h>

#define LB_STARTWORDS 8192
#define MAXITER 64

struct itable;

class linkbank {
  friend struct itable;
 public:
  linkbank ();
  ~linkbank() { delete [] words; }
  linkbank (const char * filename);
  
  int savetofile(const char * filename);
  
  void addphrase(string in);
  
  int wordidx(string in);

  int changed;

  void setwordidx(string,int);

  string info(int); /* info about the database */
  string construct(); /* construct a phrase */
#if 0
  void delete_me(int);
#endif
 public:
  void addlink(int,int,linkway,int);
  void add_helper(string,int,linkway);
  wordlink * words;
  itable * index;
  int size;
  int used;
  linkbank (const linkbank &) {;} /* prevent copying */
};


struct itable {
  int idx;
  itable * l, *r;
  itable(int i, itable * ll=0, itable * rr=0) : idx(i), l(ll), r(rr) {}

  void insert(int i, linkbank * lb) {

    int ar = strcmp(lb->words[i].word.c_str(), 
		   lb->words[idx].word.c_str());
    if (ar<0)
      if (l) l->insert(i,lb);
      else l = new itable(i);
    else 
      if (r) r->insert(i,lb);
      else r = new itable(i);

  }
};



#endif
