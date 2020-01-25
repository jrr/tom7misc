/* XXXXXXXXXXXXXXXXXXXXXX
   DO NOT USE YET --- it's still in development and does NOT WORK
   XXXXXXXXXXXXXXXXXXXXXX

   Mutable min-max heaps. A min-max heap allows constant-time
   "min" and "max" operations, although the constants are
   worse than a min-heap (see heap.h).

   The structure is similar to a min-heap, except that layers
   (i.e, depths) have alternating policies. In even layers,
   the root is smaller than all descendants. In odd layers,
   the root is larger. The complete binary tree is stored
   in a dense array, like min heaps.

   Implementation is based on this paper:
   http://cglab.ca/~morin/teaching/5408/refs/minmax.pdf
*/

#ifndef __CCLIB_MINMAX_HEAP_H
#define __CCLIB_MINMAX_HEAP_H

#include <vector>
#include "base/logging.h"
#include "base/stringprintf.h"

// XXX same struct in heap.h. Make these share the definition?
struct Heapable {
  /* The Heap uses this to store the index of this element in the heap,
     which allows you to update the Heapable value and then tell the
     heap to fix the heap invariants.

     You usually shouldn't read or write it.

     If this becomes -1, then the item has been deleted. */
  int location;
};

template<class Priority /* has comparison operators, value semantics */,
         class Value /* :> Heapable */>
class MinMaxHeap {
 public:
  struct Cell {
    Priority priority;
    Value *value;
  };

  bool Valid(const Value *v) { return v->location != -1; }

  // With no elements.
  MinMaxHeap() {}

  void Insert(Priority p, Value *v) {
    Cell c;
    c.priority = p;
    c.value = v;
    // a la SetElem.
    cells.push_back(c);
    v->location = cells.size() - 1;

    // No children, so invariant violations are always upward.
    BubbleUp(cells.size() - 1);
  }
  
  // Given a cell index, return true if it is a min layer (even)
  // and false if it is a max layer (odd).
  static inline bool IsMinLayer(int idx) {
    // idx = 0b is the root, a min layer.
    // idx = 01b and 10b are the next, a max layer
    // idx = 11b, 100b, 101b, 110b are the next, a min layer
    //
    // So the algorithm we use is to increment the index, then find
    // the parity of the index of the highest 1-bit. PERF: I think
    // Hacker's Delight or some book like that has better tricks for
    // finding the highest 1 bit--and it might be even faster to
    // simply determine its parity.
    idx++;
    bool min_policy = false;
    while (idx > 0) {
      idx >>= 1;
      min_policy = !min_policy;
    }
    return min_policy;
  }

  void Delete(Value *v) {
    int i = v->location;
    if (i == -1) {
      fprintf(stderr, "Tried to delete value more than once.\n");
      abort();
    }

    // Invalidate v because it's being deleted.
    v->location = -1;

    // Need old priority to know which direction to percolate.
    Priority pold = cells[i].priority;
    
    /* if a handle is valid, then heap size > 0 */
    Cell replacement = RemoveLast();
    if (replacement.value == v) {
      // Deleting the last element, so RemoveLast has taken
      // care of everything for us.
      return;
    }

    // Write the replacement over the deleted element.
    SetElem(i, replacement);
    CHECK_EQ(cells[i].value->location, i);

    // Percolate(pold, i);
    TrickleDown(i);
  }

  // Restore invariants for the cell index idx, given the old
  // priority of an item in this same cell (when the invariants
  // were satisfied).
  // XXX private
  void Percolate(Priority pold, int idx) {
    #if 0
    const Priority pnew = cells[idx].priority;
    if (IsMinLayer(idx)) {
      if (pnew < pold) {
        PercolateUpMin(idx);
      } else if (pold < pnew) {
        PercolateDownMin(idx);
      }
    } else {
      if (pnew < pold) {
        PercolateDownMax(idx);
      } else if (pold < pnew) {
        PercolateUpMax(idx);
      }
    }
#endif
  }

  bool Empty() const {
    return cells.empty();
  }

  Cell GetMinimum() const {
    if (cells.empty()) {
      fprintf(stderr, "Can't GetMinimum on an empty heap.\n");
      abort();
    }
    return cells[0];
  }

#if 0
  // Given some "min" root, 
  //        0     min
  //      /   \    ...
  //     1     2   max
  //    / \   / \    ...
  //   3   4 5   6   min
  int MaxOfChildren(int root) {
    DCHECK(IsMinLayer(root));
    // XXX I think this is only useful in GetMaximum, so I inlined it?
  }
#endif

  Cell GetMaximum() const {
    return cells[GetMaximumIndex()];
  }

  // Get the index of the maximum element.
  // XXX should be private
  int GetMaximumIndex() const {
    CHECK(!cells.empty());
    // As a special case, if there's just one element, it's also the
    // maximum.
    if (cells.size() == 1)
      return 0;
    
    // Maximum has to be at index 1 or 2.
    if (cells.size() == 2 ||
        cells[1].priority < cells[2].priority) {
      return 1;
    } else {
      return 2;
    }
  }
  
  // Returns and removes the (well, *a*) node with the smallest score.
  // Heap may not be empty.
  Cell PopMinimum() {
    if (cells.empty()) {
      fprintf(stderr, "Can't PopMinimum on an empty heap.\n");
      abort();
    }

    Cell c = cells[0];
    // PERF could short-circuit some of the code of Delete
    // since we know we are deleting the root.
    Delete(c.value);
    return c;
  }

  Cell PopMaximum() {
    Cell c = cells[GetMaximumIndex()];
    Delete(c.value);
    return c;
  }

  Value *PopMinimumValue() {
    return PopMinimum().value;
  }

  Value *PopMaximumValue() {
    return PopMaximum().value;
  }

  Cell GetCell(const Value *v) const {
    if (v->location == -1) {
      fprintf(stderr, "Attempt to GetCell on deleted value.\n");
      abort();
    }

    return cells[v->location];
  }

 
  void AdjustPriority(Value *v, Priority p) {
    CHECK(v->location != -1);
    const int idx = v->location;
    const Priority pold = cells[idx].priority;
    Percolate(pold, idx);
  }

  int Size() const {
    return cells.size();
  }

  void Clear() {
    for (Cell &c : cells) {
      c.value->location = -1;
    }
    cells.clear();
  }
 
  // For testing. Check that the internal invariants hold, and abort if
  // they do not.
  template<class F>
  void CheckInvariants(F Ptos) const {
    for (int i = 0; i < (int)cells.size(); i++) {
      CHECK_EQ(cells[i].value->location, i) << "Each cell in the heap should "
        "have its actual index in its location field (got " <<
        cells[i].value->location << " for index=" << i << ")\nHeap:\n" <<
        DebugString(Ptos);
      if (IsMinLayer(i)) {
        // (Maybe consider a slower but simpler alternative: Check
        // that all of the elements within its subtree have higher priority).

        // On min layers, the priority should be less (or equal) to
        // all its children (max layer) and grandchildren (which is
        // the next min layer).
        const int c1 = LeftChild(i);
        const int c2 = RightChild(i);
        const int gc1 = LeftChild(c1);
        const int gc2 = RightChild(c1);
        const int gc3 = LeftChild(c2);
        const int gc4 = RightChild(c2);
        auto CheckLe = [this, &Ptos, i](const char *which, int j) {
          if (j < (int)cells.size()) {
            CHECK(!(cells[j].priority < cells[i].priority)) <<
              "Invariant violation on min layer: "
              "\nidx " << i << " with priority " << Ptos(cells[i].priority) <<
              "\nwas greater than " << which << "child:" <<
              "\nidx " << j << " with priority " << Ptos(cells[j].priority) <<
              "\n" << DebugString(Ptos);
          }
        };
        CheckLe("left ", c1);
        CheckLe("right ", c2);
        CheckLe("left-left grand", gc1);
        CheckLe("right-left grand", gc2);
        CheckLe("left-right grand", gc3);
        CheckLe("right-right grand", gc4);
      } else {
        // For max layers, the symmetric case.
        const int c1 = LeftChild(i);
        const int c2 = RightChild(i);
        const int gc1 = LeftChild(c1);
        const int gc2 = RightChild(c1);
        const int gc3 = LeftChild(c2);
        const int gc4 = RightChild(c2);
        auto CheckGe = [this, &Ptos, i](const char *which, int j) {
          if (j < (int)cells.size()) {
            CHECK(!(cells[i].priority < cells[j].priority)) <<
              "Invariant violation on max layer: "
              "\nidx " << i << " with priority " << Ptos(cells[i].priority) <<
              "\nwas less than " << which << "child:" <<
              "\nidx " << j << " with priority " << Ptos(cells[j].priority) <<
              "\n" << DebugString(Ptos);
          }
        };
        CheckGe("left ", c1);
        CheckGe("right ", c2);
        CheckGe("left-left grand", gc1);
        CheckGe("right-left grand", gc2);
        CheckGe("left-right grand", gc3);
        CheckGe("right-right grand", gc4);
      }
    }
  }

  template<class F>
  std::string DebugString(F Ptos) const {
    std::string ret = "digraph tree {\n";

    for (int i = 0; i < (int)cells.size(); i++) {
      const char *shape = IsMinLayer(i) ? " shape=box" : "";
      ret += StringPrintf(" n%d=%d [label=\"%s\"%s]\n", i,
                          cells[i].value->location,
                          Ptos(cells[i].priority).c_str(), shape);
    }

    for (int i = 0; i < (int)cells.size(); i++) {
      int lc = LeftChild(i), rc = RightChild(i);
      if (lc < (int)cells.size())
        ret += StringPrintf("n%d -> n%d\n", i, lc);
      if (rc < (int)cells.size())
        ret += StringPrintf("n%d -> n%d\n", i, rc);
    }
    ret += "}";
    
    FILE *tmp = fopen("dump.dot", "w");
    if (tmp) {
      fprintf(tmp, "%s", ret.c_str());
      fclose(tmp);
    }
    return ret;
  }

  // XXX private
  inline static int Parent(int idx) { return (idx - 1) >> 1; }
  inline static int LeftChild(int idx) { return idx * 2 + 1; }
  inline static int RightChild(int idx) { return idx * 2 + 2; }
private:

  // Requires that the heap be nonempty.
  // Note: Doesn't update the value's location.
  Cell RemoveLast() {
    DCHECK(!cells.empty());
    Cell c = cells[cells.size() - 1];
    cells.resize(cells.size() - 1);
    return c;
  }

  /* modify the heap to hold this element at i.
     updates the handle, but doesn't worry about
     what it's overwriting. Doesn't fix invariants. */
  // assumes i is in range
  void SetElem(int i, const Cell &c) {
    DCHECK_GE(i, 0);
    DCHECK_LT(i, (int)cells.size());
    cells[i] = c;
    c.value->location = i;
  }

  void SwapElem(int i, int j) {
    Cell ci = cells[i];
    Cell cj = cells[j];
    SetElem(i, cj);
    SetElem(j, ci);
  }

  // Get a grandchild with the lowest priority. Returns -1 if there are none.
  int GetMinGrandchild(int i) {
    const int c1 = LeftChild(i);
    const int gc1 = LeftChild(c1);
    if (gc1 >= cells.size()) {
      // There are no grandchildren.
      return -1;
    }
    const int gc2 = RightChild(c1);

    const int c2 = RightChild(i);
    const int gc3 = LeftChild(c2);
    const int gc4 = RightChild(c2);

    int besti = gc1;
    Priority bestp = cells[gc1].priority;
    for (int gc : { gc2, gc3, gc4 }) {
      if (gc >= cells.size())
        break;
      if (cells[gc].priority < bestp) {
        besti = gc;
        bestp = cells[gc].priority;
      }
    }
    return besti;
  }
  
  // Get the min or max of the children and grandchildren, or return
  // -1 if there are none.
  template<bool (*cmp)(Priority a, Priority b)>
  int GetOutstandingRelativeC(int i) {
    // Comments as though cmp is <.
    const int c1 = LeftChild(i);
    if (c1 >= (int)cells.size()) {
      // No children, so no minimum
      return -1;
    }

    const int c2 = RightChild(i);
    const int gc1 = LeftChild(c1);
    const int gc2 = RightChild(c1);
    const int gc3 = LeftChild(c2);
    const int gc4 = RightChild(c2);

    int besti = c1;
    Priority bestp = cells[c1].priority;
    for (int d : { c2, gc1, gc2, gc3, gc4 }) {
      if (d >= (int)cells.size())
        break;
      if (cmp(cells[d].priority, bestp)) {
        besti = d;
        bestp = cells[d].priority;
      }
    }
    return besti;
  }

  template<bool (*cmp)(Priority a, Priority b)>
  void TrickleDownC(int i) {
    // Comments/variable names as though cmp is <.
    const int minc = GetOutstandingRelativeC<cmp>(i);
    if (minc == -1) {
      // No children; nothing to do.
      return;
    }

    if (cmp(cells[minc].priority, cells[i].priority)) {
      // Swap with this minimal relative.
      SwapElem(i, minc);

      if (minc == LeftChild(i) ||
          minc == RightChild(i)) {
        // Immediate child. Done.
        return;
      } else {
        // It's a grandchild.
        int pminc = Parent(minc);
        if (cmp(cells[pminc].priority, cells[minc].priority)) {
          // (Careful: the cell at minc changed when we swapped above.)
          SwapElem(pminc, minc);
        }
        TrickleDownC<cmp>(minc);
      }
    }
  }

  static bool Less(Priority a, Priority b) { return a < b; }
  static bool Greater(Priority a, Priority b) { return b < a; }

  void TrickleDown(int i) {
    if (IsMinLayer(i)) {
      TrickleDownC<Less>(i);
    } else {
      TrickleDownC<Greater>(i);
    }
  }

  template<bool (*cmp)(Priority a, Priority b)>
  void BubbleUpC(int i) {
    if (i == 0)
      return;
    const int p = Parent(i);
    if (p == 0)
      return;
    const int g = Parent(p);
    
    if (cmp(cells[i].priority, cells[g].priority)) {
      Cell ci = cells[i];
      Cell cg = cells[g];
      SetElem(i, cg);
      SetElem(g, ci);
      BubbleUpC<cmp>(g);
    }
  }

  // For a newly inserted element (leaf), swap upward into either min
  // or max layers until the invariant is reestablished.
  void BubbleUp(int i) {
    // If leaf and root, then the invariants are trivially satisfied.
    if (i == 0) return;
    const int p = Parent(i);

    if (IsMinLayer(i)) {
      if (cells[p].priority < cells[i].priority) {
        // Swap with parent.
        Cell ci = cells[i];
        Cell cp = cells[p];
        SetElem(p, ci);
        SetElem(i, cp);
        BubbleUpC<Greater>(p);
      } else {
        BubbleUpC<Less>(i);
      }
    } else {
      // max layer
      if (cells[i].priority < cells[p].priority) {
        // Swap with parent.
        Cell ci = cells[i];
        Cell cp = cells[p];
        SetElem(p, ci);
        SetElem(i, cp);
        BubbleUpC<Less>(p);
      } else {
        BubbleUpC<Greater>(i);
      }
    }
  }

  /* The element i, which resides on a min layer, may violate the
     order invariant by being too high. Move it downward as
     necessary. */
  void PercolateDownMin(int i) {
    // Here we have a situation like
    //          8       MIN
    //         / \        .
    //        71  ...   MAX
    //       /  \         .
    //     [31+] 10     MIN
    //     / \  / \       .
    //   46 51 31 21    MAX
    //   /\               .
    //  33 37           MIN
    // 
    // where 31+ was just replaced with something larger than was
    // there before. It needs to be smaller than everything in
    // its subtree. First thing to do is look at its grandchildren
    // (also a min layer).

    // Minimal grandchild.
    const int gc = GetMinGrandchild(i);
    if (gc == -1) {
      // At the end of the heap, so the invariant is trivially
      // satisfied and there's nothing to do.
      return;
    }

    // If the grandchild is lower than us, swap with it.
    // XXX actually it is more complex than this, huh?
    #if 0
    const Cell &me = cells[i];
    // Left and right children.
    const int li = 2 * i + 1;
    const int ri = 2 * i + 2;

    /* compare to the two children */
    const Cell &cl = cells[li];

    if (me.priority > cl.priority) {
      /* Need to swap, but with which child? */
      if (ri >= cells.size()) {
        // No right child.
        SwapPercDown(i, me, li, cl);
      } else {
        const Cell &cr = cells[ri];
        if (cl.priority < cr.priority) {
          SwapPercDown(i, me, li, cl);
        } else {
          SwapPercDown(i, me, ri, cr);
        }
      }
    } else {
      /* Consider swap with right then. */
      if (ri >= cells.size()) {
        // No right child, done.
        return;
      } else {
        const Cell &cr = cells[ri];
        if (me.priority > cr.priority) {
          SwapPercDown(i, me, ri, cr);
        }
      }
    }
    #endif
  }

  std::vector<Cell> cells;
};

#endif
