
#include "alist.h"

/* copy constructor necessary */
alist::alist(const alist& rhs) {
  head = 0;
  for(alist_entry * t = rhs.head; t; t = t -> next) {
    head = new alist_entry(t->key, t->val, head);
  }
}

/* overload assignment */
alist const & alist::operator=(alist const & rhs) {
  if (this != &rhs) {
    destroy_head(head);
    head = 0;
    for(alist_entry * t = rhs.head; t; t = t -> next) {
      head = new alist_entry(t->key, t->val, head);
    }
  }
  return *this;
}

int alist :: get(string k, string & v) {
  for(alist_entry * t = head; t; t = t -> next) {
    if (k == t->key) {
      v = t->val;
      return 1;
    }
  }
  return 0;
}


string alist::tostring_(alist_entry * h) {
  if (!h) return "";
  else return (h->key + (string)" \"" + h->val + (string)"\"" +
	       (h->next?(string)" " + tostring_(h->next):(string)""));
}

void alist::set_(string k, string v, alist_entry *& e) {
  if (!e) {
    e = new alist_entry(k, v, 0);
  } else {
    if (k == e->key) {
      e->val = v;
    } else {
      set_(k, v, e->next);
    }
  }
}

alist::destroy_head(alist_entry * h) {
  for(alist_entry * t = h; t;) {
    h = t;
    t = t->next;
    delete h;
  }
}

alist::~alist() {
  destroy_head(head);
}
