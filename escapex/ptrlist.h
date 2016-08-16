#ifndef __PTRLIST_H
#define __PTRLIST_H

#include <cstdlib>

// Simple linked-list class.
// There's nothing really wrong with this, but consider using
// std::deque<> or std::vector<> (etc.) in new code.
template<class P>
struct PtrList {
  P *head = nullptr;
  PtrList<P> *next = nullptr;
  PtrList<P>(P *h, PtrList<P> *n) : head(h), next(n) {}

  static void push(PtrList<P> *& sl, P *h) {
    sl = new PtrList<P>(h, sl);
  }

  static P *pop(PtrList<P> *&sl) {
    if (sl) {
      PtrList<P> *tmp = sl;
      P *t = tmp->head;
      sl = sl->next;
      delete tmp;
      return t;
    } else return 0;
  }

  /* ie, destroy. does not destroy the
     heads! */
  static void diminish(PtrList<P> *&pl) {
    while (pl) pop(pl);
  }

  int length() const {
    int res = 0;
    const PtrList<P> *tmp = this;
    while (tmp) {
      tmp = tmp->next;
      res++;
    }
    return res;
  }

  void toarray(P **& aout, int &numout) {
    numout = length();
    
    aout = (P**) malloc(sizeof (P*) * numout);
    PtrList<P> *tmp = this;
    for (int i = 0; i < numout; i++) {
      aout[i] = tmp->head;
      tmp = tmp->next;
    }
  }

  static PtrList<P> *copy(PtrList<P> *sl) {
    if (sl) {
      return new PtrList<P>(sl->head, 
			    copy(sl->next));
    } else return 0;
  }

  /* PERF linear stack usage */
  static void push_tail(PtrList<P> *&sl, P *h) {
    if (!sl) sl = new PtrList<P>(h, 0);
    else push_tail(sl->next, h);
  }
  
  /* merge sort the list in place. 
     PERF! not tuned for speed, but still worst case O(n lg n) */
  static void sort(int compare(P *a, P *b), PtrList<P> *&pl) {
    /* if empty or a singleton, we're done. */
    if (!pl || !pl->next) return;

    // PERF don't keep computing this recursively. Could also
    // partition in place by just pushing alternate values.
    int num = pl->length() >> 1;
    PtrList<P> *left = nullptr;
    PtrList<P> *right = copy(pl);

    /* empty out pl */
    diminish(pl);

    /* copy floor(n/2) items into left, removing them from right */
    while (num--) push(left, pop(right));

    sort(compare, left);
    sort(compare, right);

    PtrList<P> *out = nullptr;

    while (left || right) {
      if (left) {

	if (right) {
	  int ord = compare(left->head, right->head);
	  
	  if (ord < 0) { /* left < right */
	    push(out, pop(left));
	  } else if (ord > 0) { /* left > right */
	    push(out, pop(right));
	  } else {
	    /* equal */
	    push(out, pop(left));
	    push(out, pop(right));
	  }
	} else {
	  push(out, pop(left));
	}

      } else {
	push(out, pop(right));
      }
    }

    rev(out);
    pl = out;
  }

  static void rev(PtrList<P> *& pl) {
    PtrList<P> *out = 0;
    while (pl) push(out, pop(pl));
    pl = out;
  }
};

#endif
