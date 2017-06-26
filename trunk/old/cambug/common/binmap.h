
#ifndef __TOM7_BINMAP_H
#define __TOM7_BINMAP_H

#include "common/map.h"

#define tmpl <class K, class I, int (*C)(void * k1, void * k2)>

template tmpl
class binnode {
  
public:

  K key;
  I item;

  binnode<K,I,C> * left,
            * right;

  binnode(K k, I i) : key(k), item(i), left(0), right(0) {}
  
  ~binnode() {
    delete left;
    delete right;
  }

  bool find(K k, I & out) {
    if (!this) return false;
    int cmp = C(&key, &k);
    if (!cmp) {
      out = item;
      return true;
    } else if (cmp > 0) {
      return left->find(k, out);
    } else {
      return right->find(k, out);
    }
  }

  /* in-order application */
  void app(void (*f) (K k, I i, void * v), void * v) {
    if (left) left->app(f, v);
    f(key, item, v);
    if (right) right->app(f, v);
  }

 private:
  /* do not use*/
  binnode();

};

template tmpl
class binmap : public map<K,I,C> {

private:
  binnode<K,I,C> * root;

public:

  binmap () : root(0) {}

  ~binmap () {
    delete root;
  }

  binmap (K k, I v) {
    root = new binnode<K,I,C>(k, v);
  }

  virtual void insert(K k, I i);
  virtual bool find (K k, I &);
  virtual void app  ( void (*f) (K k, I i, void * v), void * v );
  virtual bool remove (K key);

 private:

  /* utility functions for recursing on nodes */
  static void ins_rec(binnode<K,I,C> *& root, K k, I i);
  static bool rem_rec(binnode<K,I,C> *& root, K k);
  static void attach_leftmost(binnode<K,I,C> *& root, 
			      binnode<K,I,C> *  tree);
};

template tmpl
void binmap<K,I,C>::attach_leftmost(binnode<K,I,C> *& root, 
				    binnode<K,I,C> *  tree) {
  if (!root) root = tree;
  else attach_leftmost(root->left, tree);
}

template tmpl
bool binmap<K,I,C>::rem_rec(binnode<K,I,C> *& root, K k) {
  if (!root) return false;
  int cmp = C(&root->key, &k);
  if (!cmp) {

    /* found the node. proceed by deleting it, sticking the right
       child in its place, and then moving the left child to the
       appropriate place on the new (right) subtree */

    binnode<K,I,C> * left, * right;

    left = root -> left;
    right = root -> right;
    
    root->left = root->right = 0;

    delete root;

    root = right;

    /* don't bother if left is null */
    if (left) attach_leftmost(right, left);
    
    return true;
  } else if (cmp > 0) {
    return rem_rec(root->left, k);
  } else {
    return rem_rec(root->right, k);
  }
}

template tmpl
bool binmap<K,I,C>::remove(K k) {
  return rem_rec(root, k);
}

template tmpl
void binmap<K,I,C>::app(void (*f) (K k, I i, void * v), void * v) {
  if (root) root->app(f, v);
}

template tmpl
void binmap<K,I,C>::ins_rec(binnode<K,I,C> *& root, K k, I i) {
  if (!root) root = new binnode<K,I,C>(k, i);
  else {
    int cmp = C(&root->key, &k);
    if (!cmp) {
      root->item = i;
    } else if (cmp > 0) {
      ins_rec(root->left, k, i);
    } else {
      ins_rec(root->right, k, i);
    }
  }
}

template tmpl
void binmap<K,I,C> :: insert(K k, I i) {
  ins_rec(root, k, i);
}

template tmpl
bool binmap<K,I,C> :: find(K k, I & out) {
  return root->find(k, out);
}

/* remove these if using a better map */
template tmpl
map<K,I,C> * newmap() {
  return new binmap<K,I,C> ();
}

template tmpl
map<K,I,C> * newmap(K k, I i) {
  return new binmap<K,I,C>(k, i);
}

#undef tmpl

#endif
