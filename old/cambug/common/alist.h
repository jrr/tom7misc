
#ifndef __TOM7_ALIST_H
#define __TOM7_ALIST_H

#include "common/stringhack.h"

/* association lists. Maybe binmap could be used/upgraded to
   produce this same functionality; tostring would
   be tricky. */

struct alist_entry {
  string key, val;
  alist_entry * next;
  alist_entry (string k, string v, alist_entry * n = 0) :
    key(k), val(v), next(n) {}
};

struct alist {
  alist_entry * head;

  alist() : head(0) {}

  string tostring() {
    return tostring_(head);
  }

  void set(string k, string v) {
    set_(k, v, head);
  }
  
  int get(string k, string & v);

  ~alist();

  alist(const alist&);
  alist const & operator=(alist const & rhs);

  private:

  static destroy_head(alist_entry *);

  string tostring_(alist_entry * h);

  void set_(string k, string v, alist_entry *& e);

};

#endif
