#ifndef __DUMPTREE_H
#define __DUMPTREE_H

struct TreeSearch;

// Utility for dumping the whole search tree to disk (HTML/PNG/JS).
// Not actually very useful, so it's been relegated to this separate
// file...
struct TreeDumping {
  static void DumpTree(TreeSearch *search);
};

#endif
