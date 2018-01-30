#ifndef __TREESEARCH_H
#define __TREESEARCH_H

#include <algorithm>
#include <vector>
#include <string>
#include <set>
#include <memory>
#include <list>

#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

// XXX trim
#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/randutil.h"

#include "atom7ic.h"

#include "weighted-objectives.h"
#include "problem-twoplayer.h"

// Base "max" nodes in heap. We start cleaning the heap when there are
// more than this number of nodes, although we often have to keep more
// than this number (because some are in use by workers, grid, etc.)
#define BASE_NODE_BUDGET 1000
// We want to always include space for the path from the root to the
// best node, so "max nodes" is really
//   BASE_NODE_BUDGET + max_depth * NODE_BUDGET_BONUS_PER_DEPTH
#define NODE_BUDGET_BONUS_PER_DEPTH 2

// When expanding a node, try this many sequences and
// choose the best one.
#define NUM_NEXTS 4
static_assert(NUM_NEXTS > 0, "allowed range");

// We consider exploring from the grid if the score is
// at least GRID_BESTSCORE_FRAC * best_score_in_heap.
#define GRID_BESTSCORE_FRAC 0.90

// "Functor"
using Problem = TwoPlayerProblem;
using Worker = Problem::Worker;

// This is a private implementation detail.
struct WorkThread;

// Tree of state exploration.
// This contains all of the non-abandoned states we've visited,
// arranged so that a path from the root to a node gives the
// sequence of inputs that gets us there.
//
// In addition to this tree of nodes, the "tree" also contains
// a queue of nodes for explicit exploration. It also contains
// several heaps for prioritizing the next nodes to expand.
struct Tree {
  using State = Problem::State;
  using Seq = vector<Problem::Input>;
  static constexpr int UPDATE_FREQUENCY = 1000;
  // Want to do a commit shortly after starting, because
  // until then, scores are frequently >1 (we don't have
  // an accurate maximum to normalize against).
  static constexpr int STEPS_TO_FIRST_UPDATE = 100;

  struct Node {
    Node(State state, Node *parent) :
      state(std::move(state)),
      parent(parent),
      depth((parent != nullptr) ? (parent->depth + 1) : 0) {}
    // Note that this can be recreated by replaying the moves from the
    // root. It once was optional and could be made that way again, at
    // the cost of many complications.
    const State state;

    // Only null for the root.
    Node *const parent = nullptr;
    const int depth = 0;
    
    // Child nodes. Currently, no guarantee that these don't
    // share prefixes, but there cannot be duplicates.
    map<Seq, Node *> children;

    // Benchmark info for this node, only used for movie output. This
    // makes it possible to compare the efficiency of progress (both
    // in movie length and in CPU time / NES frames) for a particular
    // tuning of playfun, post hoc.
    // Approximate number of NES frames that have been executed.
    int64 nes_frames = 0LL;
    // Amount of wall time that has transpired.
    int64 walltime_seconds = 0LL;
    // If not <0, the goal used to produce the node.
    int goalx = -1, goaly = -1;
    
    // Total number of times chosen for expansion. This will
    // be related to the number of children, but can be more
    // in the case that the tree is pruned or children collide.
    int chosen = 0;

    // Number of times that expansion yielded a loss on the
    // objective function. Used to compute the chance that
    // a node is hopeless. (XXX But we don't actually use
    // this to compute anything yet.)
    int was_loss = 0;

    // For heap. By invariant, all nodes are in the heap (but this
    // will be temporarily violated during insertion/deletion).
    int location = -1;

    // Reference count. This also includes outstanding explore nodes
    // sourced from this node. When zero (and the lock not held), the
    // node can be garbage collected.
    int num_workers_using = 0;

    // Number of references from the grid, which also keeps nodes
    // alive.
    int used_in_grid = 0;
    
    // Should only be used inside the tree cleanup procedure. Marks
    // nodes that should not be garbage collected because they are
    // among the best.
    bool keep = false;
  };

  // Everything in ExploreNode also protected by the tree mutex.
  struct ExploreNode {
    // Points to the tree Node that this exploration began from.
    // Sequences continue from that node, for example.
    Node *source = nullptr;

    // The exploration goal (i.e., a screen position).
    Problem::Goal goal;

    // The sequence that brings us closest to the goal.
    Seq closest_seq;
    // And the state that results from that sequence.
    State closest_state;
    // And the distance (Problem::GoalDistance(goal, closest_state)).
    double distance = 0.0;

    // We work on this goal a fixed number of times before giving up.
    int iterations_left = 0;
    // Number of workers currently active on this node. The worker that
    // sets this to zero when iterations_left is zero is responsible for
    // cleaning up the node.
    int iterations_in_progress = 0;

    // Explorations that were bad (e.g., death). Just for display.
    int bad = 0;
  };

  Tree(double score, State state) {
    root = new Node(std::move(state), nullptr);
    heap.Insert(-score, root);

    for (int i = 0; i < Problem::num_grid_cells; i++) {
      grid.emplace_back(0.0, nullptr);
    }
  }

  // Must hold mutex.
  int64 MaxNodes() const {
    return BASE_NODE_BUDGET + max_depth * NODE_BUDGET_BONUS_PER_DEPTH;
  }
  
  // Tree prioritized by negation of score at current epoch. Negation
  // is used so that the minimum node is actually the node with the
  // best score.
  Heap<double, Node> heap;

  // In addition to the main heap, we keep a grid containing the best
  // node(s) matching some criteria, called the key. A canonical use
  // of this is to keep the best-scoring node(s) (according to the
  // objective function) for a grid of screen coordinates. In
  // principle this could be generalized to include stuff like graphics
  // on the screen, the values in arbitrary memory locations, etc.
  //
  // For now this is 
  struct GridCell {
    // Actual (normalized) score, not negated.
    double score = 0.0;
    Node *node = nullptr;
    GridCell(double score, Node *node) : score(score), node(node) {}
  };

  vector<GridCell> grid;
  
  // If this has anything in it, we're in exploration mode.
  std::list<ExploreNode> explore_queue;
  // Stuckness estimate from the last reheap.
  double stuckness = 0.0;
  // The maximum depth of the tree.
  // We use this to increase the budget for the total size of
  // the tree, since we have to at least retain the path back
  // to the root for the best node.
  int max_depth = 0;
  
  Node *root = nullptr;
  // Number of steps until we update reheap and thin the tree.
  int steps_until_update = STEPS_TO_FIRST_UPDATE;
  // If this is true, a thread is updating the tree. It does not
  // necessarily hold the lock, because some calculations can be
  // done without the tree (like sorting observations). If set,
  // another thread should avoid also beginning an update.
  bool update_in_progress = false;
  int64 num_nodes = 0;
};

struct TreeSearch {
  // Should not be modified after initialization.
  std::unique_ptr<Problem> problem;

  // Options with their default values.
  struct Options {
    // When expanding a node, try this many sequences and
    // choose the best one. "2.25" means 2 (with probability
    // 0.75) or 3 (with probability 0.25).
    double num_nexts = 4.0;
    // Due to threading, the process is inherently random.
    // But this explicitly seeds it to get better randomness.
    int random_seed = 0;
  };

  TreeSearch(Options options);
  
  // Initialized by one of the workers with the post-warmup
  // state.
  Tree *tree = nullptr;
  // Coarse locking.
  std::mutex tree_m;

  // The UI thread must periodically call these for the benchmark
  // metrics to be correct in the output FM2s.
  // No lock needed to update these.
  void SetApproximateSeconds(int64 seconds_since_start);
  void SetApproximateNesFrames(int64 nes_frames);
  
  struct Stats {
    // Generated the same exact move sequence for a node
    // more than once.
    Counter same_expansion;
    Counter sequences_tried;
    Counter sequences_improved;
    Counter sequences_improved_denom;

    Counter explore_iters;
    Counter explore_deaths;
  };
  Stats stats;

  // Returns the actual file written.
  string SaveBestMovie(const string &filename_part);
  
  // Not holding the lock.
  void StartThreads();
  void DestroyThreads();

  // For UI thread; returns the current workers.
  // Should hold the lock or ensure the number of workers
  // is not changed.
  vector<Worker *> WorkersWithLock() const;
  
 private:
  friend class WorkThread;
  const Options opt;
  // Updated by the UI thread.
  std::atomic<int64> approx_sec{0LL};
  std::atomic<int64> approx_nes_frames{0LL};

  // Approximately one per logical CPU. Created at startup and lives
  // until should_die becomes true. Pointers owned by TreeSearch.
  vector<WorkThread *> workers;
  int num_workers = 0;

  // TODO(twm): use shared_mutex when available
  bool should_die = false;
  mutex should_die_m;
};

#endif
