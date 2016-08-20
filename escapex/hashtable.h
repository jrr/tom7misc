// XXX see if we can use unordered_map instead; I'm sure it's
// much better!
#ifndef __HASHTABLE_H
#define __HASHTABLE_H

#include <cstdint>

#include "util.h"
#include "ptrlist.h"

/* The hashtable template takes two classes as arguments:

   I, the items in the hash table, must have these methods:
      K key();
        - return the key for this entry

      static unsigned int hash(K k);
        - hash a key

      void destroy();
        - destroy the entry
      
   K, the key used for comparison, and the input to the hash function,
   does not require any methods other than the == operator. It is
   typically std::string.
*/

/* mediocre but fast hash function on std::string */
inline uint32_t hash_string(string s) {
  uint32_t output = 0x3710EAFF;
  uint32_t len = (uint32_t)s.length();
  for (int idx = 0; idx < len; idx++) {
    output = (output << 13) | (output >> (32 - 13));
    output ^= (uint32_t)s[idx];
  }
  return output;
}

#if 0
template <class I, class K>
struct hlist {
  I *data;
  hlist<I,K> *next;
  hlist(I *dd, hlist<I,K> *nn) : data(dd), next(nn) {}
  void destroy() { delete this; }
};
#endif

template <class I, class K>
struct hashtable {
  
  int allocated;

  int items;

  PtrList<I> **data;

  void destroy() {
    for (int i = 0; i < allocated; i++) {
      for (PtrList<I> *tmp = data[i]; tmp; ) {
        while (tmp) PtrList<I>::pop(tmp)->destroy();
      }
    }
    free(data);
    delete this;
  }

  static hashtable * create(int i) {
    hashtable * h = new hashtable();

    h->allocated = i;
    h->data = (PtrList<I>**)malloc(i * sizeof (PtrList<I>*));
    h->items = 0;
    
    if (!h->data) {
      delete h;
      return 0;
    }

    for (int m = 0; m < i; m++) {
      h->data[m] = 0;
    }

    return h;

  }

  /* does not overwrite existing entry, if any,
     but obscures it from lookup! (unless perhaps
     you later sort this list) */
  void insert(I *item) {
    unsigned int loc = I::hash(item->key()) % allocated;
    
    data[loc] = new PtrList<I>(item, data[loc]);
    items++;
  }

  I *lookup(K key) {
    unsigned int loc = I::hash(key) % allocated;

    for (PtrList<I> *tmp = data[loc]; tmp; tmp = tmp->next) {
      if (key == tmp->head->key()) return tmp->head;
    }

    return 0;
  }

  /* removes if present */
  I *remove(K key) {
    unsigned int loc = I::hash(key) % allocated;
    
    PtrList<I> **tmp = &data[loc];
    while (*tmp) {
      if (key == (*tmp)->head->key()) {
        /* Remove it. */
        PtrList<I> *node = *tmp;
        *tmp = (*tmp)->next;
        I *data = node->head;
        node->next = 0;
        delete node;
        return data;
      }
      tmp = &((*tmp)->next);
    }

    /* not found */
    return 0;
  }

};

/* must be oustide of hashtable itself, or Visual C++ gets
   horribly confused */
template<class I, class K, class T>
inline void hashtable_app(hashtable<I, K> *tab,
                          void (*f)(I *, T), 
                          T d ) {
  for (int i = 0 ; i < tab->allocated; i++ ) {
    PtrList<I> *l = tab->data[i];
    while (l) {
      f(l->head, d);
      l = l->next;
    }
  }
}


#endif
