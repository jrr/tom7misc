
// TODO: Try to deduce that a state is hopeless and stop expanding
// it. -or- try to estimate the maximum reachable score from each
// node using some Bayesian method.
//
// TODO: Super Meat Brothers-style multi future visualization of tree.
//
// TODO: Always expand nodes 100 times or whatever?
//
// TODO: Serialization of tree state and restarting
//
// TODO: Find values we don't want to go down by setting them to 1 or
// zero as they go down (or something), and testing whether that is
// really bad.
//
// TODO: Be scientific! Build an ground truth dataset (e.g.
// hand-written objectives that are trustworthy) and an offline
// pipeline that can measure performance on that set, for tuning
// hyperparameters and seeing whether ideas actually pan out.
//
// TODO: Players love moving to the right because the x coordinate
// is part of the objective function. We could perhaps eliminate
// this by detecting bytes that appear to be the player's coordinates?
// Or subtracting the x coordinate from such bytes during the
// exploration phase?
//
// TODO: min-max heap would allow us a very cheap "1000 best nodes"
// representation.
//
// TODO: Rather than choosing random goals, choose goals that aren't
// already represented in the grid, and perhaps choose ones that are
// adjacent to covered cells.
//
// TODO: Smooth the markov matrix by making any input (that was
// actually executed) possible in any state, with the minimal
// probability mass. This probably also solves the problem where
// we get out of gamut.
//
// TODO: show benchmark of Mframes before reaching milestones in the
// game, so that we can scientifically judge efficiency. Also frame
// depth, since shorter movies are also better.
// (Might also be useful to output the wall time of some operations,
// since e.g. reheaping seems to get kinda slow after it's been
// running that long.)
//
// TODO: Consider generating same moves for both players when they are
// in the same approximate position? This makes the probability of
// both making a narrow jump substantially better.
//
// TODO: Grid keys for palette entries, sprites on screen, bg values?
// This would help for bosses where there's some visual feedback on
// damage, and would be easier than trying to fuzz all of ram
// I bet the bosses that are most stuck are actually BG graphics.
//
// TODO: Just some way to ensure that the highest-scoring node
// continually gets random play in parallel with the other search?
// This might result in worse (long) movies, but sometimes that's
// the right way for it to beat some spot (like e.g. you have to
// just survive some screen, or many contra bosses where you have
// to deal a bunch of damage but it doesn't know it's making
// progress...)
//
// TODO: When on the ice pillars level, regular tree search is
// sometimes backing up all the way to the waterfall climb -- this
// seems nuts (it's like almost 200k depth away). We may be able to
// improve MFrame efficiency by limiting the score loss in
// FindGoodNode or whatever.
//
// TODO: Make my own playback tool. It's not hard, and even just
// being able to fastforward at a reasonable rate would save time.
//
// TODO: Right after a score breakthrough (i.e., plateu for a while,
// then we discover a higher score), try replaying the sequence of
// moves we just made on random states from the plateau region, with
// the idea that we can sometimes cut out a lot of dithering. Not
// clear what we do in cases where the players are farming low-value
// points (e.g. score) while waiting, though, because the resulting
// states would be actually worse than the original breakthrough
// (lower score).

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

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/randutil.h"

#include "atom7ic.h"

#include "weighted-objectives.h"

#include "SDL.h"
#include "graphics.h"

#include "problem-twoplayer.h"

// Screen dimensions.
#define WIDTH 1920
#define HEIGHT 1080

// "max" nodes in heap. We start cleaning the heap when
// there are more than this number of nodes, although
// we often have to keep more than this number (because
// some are in use by workers, grid, etc.)
#define MAX_NODES 1000

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

struct PF2;
struct WorkThread;
struct UIThread;

std::mutex print_mutex;
#define Printf(fmt, ...) do {			\
    MutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);			\
  } while (0)

// TODO(twm): use shared_mutex when available
static bool should_die = false;
static mutex should_die_m;

static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

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
    Node(State state, Node *parent) : state(std::move(state)),
				      parent(parent) {}
    // Note that this can be recreated by replaying the moves from the
    // root. It once was optional and could be made that way again, at
    // the cost of many complications.
    const State state;

    // Only null for the root.
    Node *parent = nullptr;

    // Child nodes. Currently, no guarantee that these don't
    // share prefixes, but there cannot be duplicates.
    map<Seq, Node *> children;

    // Note for the node, only used in movie output. Associated
    // with the sequence that led to this node.
    string subtitle;
    
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

  #if 0
  // old idea - delete
  
  // We keep multiple heaps, each of which could contain a different
  // subset of nodes. There is one "main" heap that can hold any node
  // (this one is typically large) plus a set of smaller keyed heaps
  // that a node can only be stored in if it meets some criteria
  // (called the "key"). A canonical use of this is to keep the
  // best-scoring (according to objective function) node(s) for
  // a grid of screen positions.
  struct KeyedHeap {

    
    Heap<double, Node> heap;
  };
  #endif

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

struct PF2 {
  // Should not be modified after initialization.
  std::unique_ptr<Problem> problem;

  // Approximately one per CPU. Created at startup and lives
  // until should_die becomes true. Pointers owned by PF2.
  vector<WorkThread *> workers;
  int num_workers = 0;

  UIThread *ui_thread = nullptr;

  // Initialized by one of the workers with the post-warmup
  // state.
  Tree *tree = nullptr;
  // Coarse locking.
  std::mutex tree_m;

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

  PF2() {
    map<string, string> config = Util::ReadFileToMap("config.txt");
    if (config.empty()) {
      fprintf(stderr, "Missing config.txt.\n");
      abort();
    }

    num_workers = atoi(config["workers"].c_str());
    if (num_workers <= 0) {
      fprintf(stderr, "Warning: In config, 'workers' should be set >0.\n");
      num_workers = 1;
    }
    // An absurd amount, but we want to not overflow 8-bit ref counts.
    if (num_workers > 250) {
      fprintf(stderr, "Warning: In config, 'workers' must be <250.\n");
      num_workers = 250;
    }

    problem.reset(new Problem(config));
    printf("Problem at %p\n", problem.get());
  }

  void StartThreads();
  void DestroyThreads();

  void Play() {
    // Doesn't return until UI thread exits.
    StartThreads();

    DestroyThreads();
  }
};

struct WorkThread {
  using Node = Tree::Node;
  WorkThread(PF2 *pftwo, int id) : pftwo(pftwo), id(id),
				   rc(StringPrintf("worker_%d", id)),
				   gauss(&rc),
				   worker(pftwo->problem->CreateWorker()),
				   th{&WorkThread::Run, this} {}

  // Get the root node of the tree and associated state (which must be
  // present). Used during initialization.
  Node *GetRoot() {
    MutexLock ml(&pftwo->tree_m);
    Node *n = pftwo->tree->root;
    n->num_workers_using++;
    return n;
  }

  // Populates the vector with eligible grid indices.
  void EligibleGridNodesWithMutex(vector<int> *eligible) {
    // XXX This stuff is a hack. Improve it!
    double bestscore = -pftwo->tree->heap.GetMinimum().priority;
    const double gminscore = GRID_BESTSCORE_FRAC * bestscore;
    
    /*
      n.b. never even tried this code!
    int gidx = RandTo(rc, pftwo->tree->grid.size());
    if (pftwo->tree->grid[gidx].node != nullptr &&
	rc->Byte() < 128) {
      // XXX Do we really need a score cutoff? How to choose it?
      if (pftwo->tree->grid[gidx].score >= 0.90 * bestscore)
	ret = pftwo->tree->grid[gidx].node;
    }
    */

    for (int idx = 0; idx < pftwo->tree->grid.size(); idx++) {
      const Tree::GridCell &gc = pftwo->tree->grid[idx];
      if (gc.node != nullptr) {
	const double p = gc.score - gminscore;
	if (p > 0.0 && RandDouble(&rc) < p) {
	  eligible->push_back(idx);
	}
      }
    }
  }
  
  // Must hold tree mutex!
  // Doesn't update any reference counts.
  Node *FindGoodNodeWithMutex() {
    const int size = pftwo->tree->heap.Size();
    CHECK(size > 0) << "Heap should always have root, at least.";
    
    vector<int> eligible_grid;
    EligibleGridNodesWithMutex(&eligible_grid);

    Node *ret = nullptr;
    // The more nodes that are eligible, the more likely we are
    // to be stuck. Take a grid node proportional to the number
    // of grid nodes.
    if (!eligible_grid.empty() &&
	RandDouble(&rc) <
	// Always a reasonable chance of picking from the grid
	// (if any is eligible).
	0.25 +
	// When the grid is full, an additional 25% chance.
	0.25 * 
	// Fraction of the grid that was eligible
	(eligible_grid.size() / (double)Problem::num_grid_cells)) {
      const int idx = eligible_grid[RandTo(&rc, eligible_grid.size())];
      ret = pftwo->tree->grid[idx].node;
    }
    
    if (ret == nullptr) {
      // The idea here is to choose a good node to start; in a heap,
      // good nodes are towards the beginning of the array.
      // Gaussian centered on 0; use 0 for anything out of bounds.
      // (n.b. this doesn't seem to help get unstuck -- evaluate it)
      const int g = (int)(gauss.Next() * (size * 0.25));
      const int idx = (g <= 0 || g >= size) ? 0 : g;

      Heap<double, Node>::Cell cell = pftwo->tree->heap.GetByIndex(idx);
      ret = cell.value;
    }
    
    // Now ascend up the tree to avoid picking nodes that have high
    // scores but have been explored at lot (this means it's likely
    // that they're dead ends).
    for (;;) {
      if (ret->parent == nullptr)
	break;

      // TODO: Here, incorporate the number of expansions that caused
      // the score to go down (a lot?)?
      double p_to_ascend = 0.1 +
	// Sigmoid with a chance of ~3.5% for cell that's never been chosen
	// before, a 50% chance around 300, and a plateau at 80%.
	(0.8 / (1.0 + exp((ret->chosen - 300.0) * -.01)));
      if (RandDouble(&rc) >= p_to_ascend)
	break;
      // printf("[A]");
      ret = ret->parent;
    }
    return ret;
  }
    
  // Pick the next node to explore. Must increment the chosen field.
  // n is the current node, which may be discarded; this function
  // also maintains correct reference counts.
  Node *FindNodeToExtend(Node *n) {
    MutexLock ml(&pftwo->tree_m);

    CHECK(n->num_workers_using > 0);

    // Simple policy: 50% chance of switching to some good node using
    // the heap/grid; 50% chance of staying with the current node.
    if (rc.Byte() < 128 + 64 /* XXX */) {
      Node *ret = FindGoodNodeWithMutex();
      
      // Giving up on n.
      n->num_workers_using--;
      ret->num_workers_using++;

      ret->chosen++;
      return ret;
    } else {
      n->chosen++;
      // Keep reference count.
      return n;
    }
  }

  // Add a new child to the node (unless it's a duplicate).
  // Returns the new node or existing child. Doesn't change
  // reference counts. Must hold the tree lock.
  Node *ExtendNodeWithLock(Node *n,
			   const Tree::Seq &seq,
			   Problem::State newstate,
			   double newscore) {
    CHECK(n != nullptr);
    Node *child = new Node(std::move(newstate), n);
    auto res = n->children.insert({seq, child});
    if (!res.second) {
      // By dumb luck (this might not be that rare if the markov
      // model is sparse), we already have a node with this
      // exact path. We don't allow replacing it.
      pftwo->stats.same_expansion.Increment();

      delete child;
      return res.first->second;
    } else {
      pftwo->tree->num_nodes++;
      pftwo->tree->heap.Insert(-newscore, child);
      CHECK(child->location != -1);

      AddToGridWithLock(child, newscore);
      
      return child;
    }
  }

  // Add to the grid if it qualifies. Called from within
  // ExtendNodeWithLock once we're sure the node is new.
  void AddToGridWithLock(Node *newnode, double newscore) {
    int cell = 0;
    if (pftwo->problem->GetGridCell(newnode->state, &cell)) {
      CHECK(cell >= 0 && cell < pftwo->tree->grid.size()) <<
	cell << " vs " << pftwo->tree->grid.size();
      Tree::GridCell *gc = &pftwo->tree->grid[cell];
      if (gc->node == nullptr || newscore > gc->score) {
	if (gc->node)
	  gc->node->used_in_grid--;
	newnode->used_in_grid++;
	gc->score = newscore;
	gc->node = newnode;
      }
    }
  }
  
  // XXX a lot of this function duplicates the above (which is used
  // directly during exploration). We should figure out how to merge
  // them.
  // 
  // Extend a node with some sequence that we already ran, and
  // that results in the newstate with newscore.
  //
  // When calling, must be holding n. Modifies it and returns the new
  // child; drops the reference to n and acquires a reference to the
  // new child.
  Node *ExtendNode(Node *n,
		   const Tree::Seq &seq,
		   Problem::State newstate,
		   double newscore) {
    CHECK(n != nullptr);
    Node *child = new Node(std::move(newstate), n);
    MutexLock ml(&pftwo->tree_m);

    // XXX This should probably be done in the caller, because
    // if NUM_NEXTS isn't 1, we have more fine-grained evidence
    // that we could collect. (Right now it's like, "the probability
    // that randomly expanding the node NUM_NEXTS times and picking
    // the best one will actually make things worse") which is maybe
    // harder to think about, and certainly converges more slowly.
    const double oldscore = pftwo->problem->Score(n->state);
    if (oldscore > newscore) {
      n->was_loss++;
    }

    auto res = n->children.insert({seq, child});

    CHECK(n->num_workers_using > 0);
    n->num_workers_using--;

    if (!res.second) {
      // By dumb luck (this might not be that rare if the markov
      // model is sparse), we already have a node with this
      // exact path. We don't allow replacing it.
      pftwo->stats.same_expansion.Increment();

      delete child;
      // Maintain the invariant that the worker is at the
      // state of the returned node.
      Node *ch = res.first->second;

      CHECK(ch != nullptr);
      worker->Restore(ch->state);
      ch->num_workers_using++;
      return ch;

    } else {
      pftwo->tree->num_nodes++;
      pftwo->tree->heap.Insert(-newscore, child);
      CHECK(child->location != -1);

      child->num_workers_using++;
      AddToGridWithLock(child, newscore);
      return child;
    }
  }

  // At startup, ensure that the tree contains at least a root node.
  // Only does it in one thread, but doesn't return until this is the
  // case.
  void InitializeTree() {
    MutexLock ml(&pftwo->tree_m);
    if (pftwo->tree == nullptr) {
      // I won the race!
      printf("Initialize tree...\n");
      Problem::State s = worker->Save();
      pftwo->tree = new Tree(pftwo->problem->Score(s), s);
    }
  }

  void MaybeUpdateTree() {
    Tree *tree = pftwo->tree;
    {
      MutexLock ml(&pftwo->tree_m);
      // No use in decrementing update counter -- when we
      // finish we reset it to the max value.
      if (tree->update_in_progress)
	return;

      // Also, we're not allowed to make steps until
      // the explore queue is empty.
      if (!tree->explore_queue.empty())
	return;
      
      if (tree->steps_until_update == 0) {
	tree->steps_until_update = Tree::UPDATE_FREQUENCY;
	tree->update_in_progress = true;
	// Enter fixup below.
      } else {
	tree->steps_until_update--;
	return;
      }
    }

    // This is run every UPDATE_FREQUENCY calls, in some
    // arbitrary worker thread. We don't hold any locks right
    // now, but only one thread will enter this section at
    // a time because of the update_in_progress flag.
    worker->SetStatus("Tree: Commit observations");

    pftwo->problem->Commit();

    #if 0
    {
      // Really we should be considering every node for a new position
      // in the grid (XXX we could do this as we look at each node
      // below) but at a minimum we need to make the grid scores
      // accurate.
      MutexLock ml(&pftwo->tree_m);
    }
    #endif
    
    // Re-build heap. We do this by recalculating the score for
    // each node in place; most of the time this won't change the
    // shape of the heap much.
    {
      worker->SetStatus("Tree: Reheap");
      const uint32 start_ms = SDL_GetTicks();
      {
	MutexLock ml(&pftwo->tree_m);

	printf("Clear grid ...");
	// First, just clear the grid since every node has a chance to
	// become the best in a cell below, including the ones that are
	// already there.
	for (Tree::GridCell &gc : pftwo->tree->grid) {
	  if (gc.node != nullptr) {
	    gc.node->used_in_grid--;
	    gc.score = 0.0;
	    gc.node = nullptr;
	  }
	}
	
	printf("Reheap ... ");

	// tree->heap.Clear();

	std::function<void(Node *)> ReHeapRec =
	  [this, tree, &ReHeapRec](Node *n) {
	  // Act on the node if it's in the heap.
	  if (n->location != -1) {
	    const double new_score = pftwo->problem->Score(n->state);
	    // Note negation of score so that bigger real scores
	    // are more minimum for the heap ordering.
	    tree->heap.AdjustPriority(n, -new_score);
	    CHECK(n->location != -1);
	    AddToGridWithLock(n, new_score);
	  }
	  for (pair<const Tree::Seq, Node *> &child : n->children) {
	    ReHeapRec(child.second);
	  }
	};
	ReHeapRec(tree->root);
      }
      const uint32 end_ms = SDL_GetTicks();
      printf("Done in %.4f sec.\n", (end_ms - start_ms) / 1000.0);
    }
    
    {
      MutexLock ml(&pftwo->tree_m);
      if (tree->num_nodes > MAX_NODES) {
	const uint32 start_ms = SDL_GetTicks();
	printf("Trim tree ...\n");
	// Here we want to delete the worst-scoring nodes in order
	// to stay under our budget. We can't delete ancestors of
	// nodes we keep, and we can't delete a node that a worker
	// is currently using.
	
	// What we'll do is pop the best MAX_NODES nodes from the
	// heap. This is better than the old way of sorting all the
	// scores to get a cutoff score, because it's like
	// MAX_NODES*lg(MAX_NODES) rather than num_nodes*lg(num_nodes)
	// and it also allows us to arbitrarily break ties. (The tie
	// situation can get very bad when we have a flat objective
	// function and are stuck--there can be tens of millions of
	// nodes with the same score).

	// We also compute the 'area under the curve' (auc) for the
	// nodes we're keeping. This is high when all the nodes have
	// almost the maximum score, which suggests that we are
	// 'stuck.'
	//
	// The objective-function-based score is normalized against
	// the best value we've ever seen, so it's typical for the
	// best score to be 1.0. But in some cases, we might Observe a
	// good-looking state but not insert it into the tree. For
	// example, we might be a level but with one of the players dead.
	// In this case, Score()s might be forever small. So here we
	// normalize against the single best score when computing AUC.
	const double best_score = -tree->heap.GetMinimum().priority;
	// Predivided normalization factor, and negated because priorities
	// are negative scores.
	const double negative_one_over_best_score =
	  best_score <= 0.0 ? 0.0 : (-1.0 / best_score);
	double auc = 0.0;
	double worst_kept_score = -1.0;
	for (int i = 0; i < MAX_NODES && !tree->heap.Empty(); i++) {
	  Heap<double, Node>::Cell best = tree->heap.PopMinimum();
	  auc += best.priority * negative_one_over_best_score;
	  best.value->keep = true;
	  worst_kept_score = -best.priority;
	}

	tree->stuckness = auc * (1.0 / MAX_NODES);
	printf("\n ... auc %.2f; stuckness %.4f\n", auc, tree->stuckness);
	printf("\n ... kept nodes range in score from %.4f to %.4f\n",
	       worst_kept_score, best_score);

	// Now the heap only contains nodes we don't want.
	tree->heap.Clear();

	uint64 deleted_nodes = 0ULL;
	uint64 kept_score = 0ULL, kept_worker = 0ULL, kept_parent = 0ULL,
	  kept_grid = 0ULL;
	std::function<bool(Node *)> CleanRec =
	  // Returns true if we should keep this node; otherwise,
	  // the node is deleted.
	  [this, tree, &deleted_nodes,
	   &kept_score, &kept_worker, &kept_parent, &kept_grid,
	   &CleanRec](Node *n) -> bool {
	  if (n->keep)
	    kept_score++;
	  if (n->num_workers_using)
	    kept_worker++;
	  if (n->used_in_grid)
	    kept_grid++;
	  
	  bool keep = n->keep ||
	      n->num_workers_using > 0 ||
	      n->used_in_grid > 0;

	  map<Tree::Seq, Node *> new_children;
	  bool child_keep = false;
	  for (auto &p : n->children) {
	    if (CleanRec(p.second)) {
	      new_children.insert({p.first, p.second});
	      child_keep = true;
	    }
	  }
	  n->children.swap(new_children);

	  if (child_keep)
	    kept_parent++;
	  keep = child_keep || keep;
	  
	  if (keep) {
	    // PERF: We have to recompute the score here; particularly
	    // for nodes we're keeping because a worker is using it,
	    // we just cleared the entire heap so we don't even have
	    // its score.
	    const double new_score = pftwo->problem->Score(n->state);
	    tree->heap.Insert(-new_score, n);
	    n->keep = false;
	    CHECK(n->location != -1);
	  } else {
	    deleted_nodes++;
	    tree->num_nodes--;
	    delete n;
	  }
	  
	  return keep;
	};
	
	// This should not happen for multiple reasons -- there should
	// always be a worker working within it, and since it contains
	// all nodes, one of the MAX_NODES best scores should be beneath
	// the root!
	CHECK(CleanRec(tree->root)) << "Uh, the root was deleted.";

	printf("... Reasons for keeping nodes:\n"
	       "    score is good: %llu\n"
	       "    worker using: %llu\n"
	       "    in grid: %llu\n"
	       "    child is kept: %llu\n",
	       kept_score, kept_worker, kept_grid, kept_parent);

	
	const uint32 end_ms = SDL_GetTicks();
	printf(" ... Deleted %llu; now the tree is size %llu.\n"
	       " ... Done in %.4f sec.\n",
	       deleted_nodes,
	       tree->num_nodes,
	       (end_ms - start_ms) / 1000.0);

	// Now, find some nodes for exploration.
	CHECK(tree->explore_queue.empty());
	if (tree->stuckness > 0.50) {
	  static constexpr int NUM_EXPLORE_NODES = 50;
	  static constexpr int NUM_EXPLORE_ITERATIONS = 500;
	  // More stuck = more exploration.
	  int num_explore = NUM_EXPLORE_NODES * tree->stuckness;
	  printf("Making %d explore nodes.\n", num_explore);
	  while (num_explore--) {
	    // XXX I think it would be better if we required the
	    // score to be close to the max score, because otherwise
	    // there's basically no chance of this helping.
	    Node *source = FindGoodNodeWithMutex();
	    source->num_workers_using++;
	    Problem::Goal goal = pftwo->problem->RandomGoal(&rc);
	    
	    Tree::ExploreNode en;
	    en.source = source;
	    en.goal = goal;
	    en.closest_state = source->state;
	    en.distance = pftwo->problem->GoalDistance(goal, source->state);
	    en.iterations_left = NUM_EXPLORE_ITERATIONS;
	    en.iterations_in_progress = 0;
	    tree->explore_queue.push_back(std::move(en));
	  }
	}
      }
    }


    {
      MutexLock ml(&pftwo->tree_m);
      tree->update_in_progress = false;
    }
  }

  // Holding the tree mutex, find any explore node in the explore
  // queue, increment its reference count, and return a pointer to
  // it. The pointer stays valid even if the lock is relinquished,
  // since the reference count is nonzero. Returns nullptr if none
  // can be found.
  Tree::ExploreNode *GetExploreNodeWithMutex() {
    // PERF We'd get better lock contention and not have to go through
    // the list as much if we had workers working on different nodes,
    // right? Could move the node we select to the back in constant time.
    std::list<Tree::ExploreNode> *explore_queue = &pftwo->tree->explore_queue;
    for (std::list<Tree::ExploreNode>::iterator it = explore_queue->begin();
	 it != explore_queue->end(); ++it) {
      Tree::ExploreNode *en = &*it;
      if (en->iterations_left > 0) {
	en->iterations_left--;
	en->iterations_in_progress++;
	return en;
      }
    }
    // Didn't find any.
    return nullptr;
  }
  
  // Returns true if we should continue processing the queue.
  bool ProcessExploreQueue() {
    Tree::ExploreNode *en = nullptr;
    Tree::Seq start_seq;
    Problem::State start_state;
    double start_dist = -1.0;
    {
      MutexLock ml(&pftwo->tree_m);
      en = GetExploreNodeWithMutex();
      if (en == nullptr) return false;
      // I should have a lock.
      CHECK(en->iterations_in_progress > 0);
      start_seq = en->closest_seq;
      start_state = en->closest_state;
      start_dist = en->distance;
    }
    
    // Load source state.
    worker->SetStatus("Exploring");
    worker->Restore(start_state);

    worker->SetStatus("Explore inputs");

    // Generate a random sequence.
    constexpr double MEAN = 180.0;
    constexpr double STDDEV = 60.0;

    int num_frames = gauss.Next() * STDDEV + MEAN;
    if (num_frames < 1) num_frames = 1;

    // Generate inputs with knowledge of the goal.
    Problem::InputGenerator gen =
      worker->Generator(&rc, &en->goal);

    // XXX maybe would be cleaner to populate the full
    // sequence starting with start_seq, but then only
    // execute the new suffix. Both places below we need
    // to concatenate these.
    Tree::Seq seq;
    seq.reserve(num_frames);
    for (int frames_left = num_frames; frames_left--;) {
      seq.push_back(gen.RandomInput(&rc));
    }

    // Now, execute it. Keep track of the closest that we got
    // to our goal, since we'll use that to update the explore
    // node.
    double best_distance = start_dist;
    Problem::State closest_state;
    // Excuting up to and including this index.
    int best_prefix = -1;
    bool bad = false;
    worker->SetStatus("Explore exec");
    for (int i = 0; i < seq.size(); i++) {
      const Problem::Input input = seq[i];
      worker->Exec(input);
      Problem::State after = worker->Save();
      double penalty = pftwo->problem->EdgePenalty(start_state, after);
      // If we die, don't use this sequence at all.
      // XXX checking exactly 1.0 here is bad. But how?
      if (penalty < 1.0) {
	bad = true;
	break;
      }

      // PERF could avoid saving every iteration if we computed this
      // from the worker state
      double dist = pftwo->problem->GoalDistance(en->goal, after);

      if (dist < best_distance) {
	best_prefix = i;
	closest_state = after;
	best_distance = dist;
      }
    }

    worker->SetStatus("Explore post");
    if (!bad && best_prefix >= 0) {
      // At some point, we got closer to the goal. Put this in
      // the ExploreNode if it is still an improvement (might have
      // lost a race).
      MutexLock ml(&pftwo->tree_m);
      if (best_distance < en->distance) {
	en->distance = best_distance;
	en->closest_state = closest_state;
	// The sequence is actually the start sequence plus
	// our prefix.
	en->closest_seq = start_seq;
	// Note that the prefix is inclusive of the endpoint; see above.
	for (int i = 0; i <= best_prefix; i++) {
	  en->closest_seq.push_back(seq[i]);
	}
      }
    }

    // Now, add the node to our tree, using that source.
    // We don't increment the expansion/failure counts because it's
    // not fair to penalize this state just because we used it for
    // exploration (e.g. our goal might be right in the lava!)

    if (!bad) {
      worker->SetStatus("Explore extend");
      MutexLock ml(&pftwo->tree_m);
      Tree::Seq full_seq = start_seq;
      for (const Problem::Input input : seq) {
	full_seq.push_back(input);
      }

      // PERF could avoid saving again here (using the last save from
      // the loop above), but better would be to fix the fact that we
      // keep saving in that loop...
      Problem::State newstate = worker->Save();
      double score = pftwo->problem->Score(newstate);
      Node *child =
	ExtendNodeWithLock(en->source, full_seq, std::move(newstate), score);
      child->subtitle = StringPrintf("Goto %d,%d (closest %.2f)",
				     // XXX TPP-specific.
				     en->goal.goalx,
				     en->goal.goaly,
				     best_distance);
    }

    // Finally, reduce the iteration count, and maybe clean up the
    // ExploreNode.
    {
      MutexLock ml(&pftwo->tree_m);
      CHECK(en->iterations_in_progress > 0);
      en->iterations_in_progress--;
      if (bad) en->bad++;
      if (en->iterations_left == 0 &&
	  en->iterations_in_progress == 0) {
	worker->SetStatus("Cleanup ExploreNode");

	CHECK(en->source->num_workers_using > 0) << "Existence in the "
	  "explore queue is supposed to imply a reference to the source "
	  "node.";
	en->source->num_workers_using--;

	// XXX cleaner if we retained this iterator, but this
	// is correct since pointers in the list are guaranteed
	// to be stable.
	std::list<Tree::ExploreNode> *explore_queue =
	  &pftwo->tree->explore_queue;
	for (std::list<Tree::ExploreNode>::iterator it = explore_queue->begin();
	     it != explore_queue->end(); ++it) {
	  if (en == &*it) {
	    explore_queue->erase(it);
	    goto done;
	  }
	}
	CHECK(false) << "Didn't find ExploreNode in queue when "
	  "trying to delete it!";
        done:;
      }
    }

    // Reflect this work in stats.
    pftwo->stats.explore_iters.Increment();
    if (bad) {
      pftwo->stats.explore_deaths.Increment();
    }

    // And keep working on queue.
    return true;
  }
    
  void Run() {
    InitializeTree();

    // Handle to the last node expanded, which will match the current
    // state of the worker. Worker has an outstanding reference count.
    Node *last = nullptr;

    // Initialize the worker so that its previous state is the root.
    // With the current setup it should already be in this state, but
    // it's inexpensive to explicitly establish the invariant.
    {
      worker->SetStatus("Load root");
      last = GetRoot();
      CHECK(last != nullptr);
      worker->Restore(last->state);
    }

    for (;;) {
      // If the exploration queue isn't empty, we work on it instead
      // of continuing our work on other nodes.
      worker->SetStatus("Explore Queue");
      while (ProcessExploreQueue()) {
	// OK to ignore the rest of this loop, but we should die if
	// requested.
	if (ReadWithLock(&should_die_m, &should_die)) {
	  worker->SetStatus("Die");
	  return;
	}
      }

      // Starting this loop, the worker is in some problem state that
      // corresponds to the node 'last' in the tree. We'll extend this
      // node or find a different one.
      worker->SetStatus("Find start node");
      Node *expand_me = FindNodeToExtend(last);

      // PERF Can sometimes skip this load if expand_me == last. But
      // we have to worry about the case that we did some intermezzo
      // exploration in this worker. Anyway, loading is pretty cheap.
      worker->SetStatus("Load");
      CHECK(expand_me != nullptr);
      worker->Restore(expand_me->state);

      worker->SetStatus("Gen inputs");
      constexpr double MEAN = 300.0;
      constexpr double STDDEV = 150.0;

      // All the expansions will have the same length; this makes it
      // more sensible to compare the objectives in order to choose
      // the best.
      int num_frames = gauss.Next() * STDDEV + MEAN;
      if (num_frames < 1) num_frames = 1;

      vector<vector<Problem::Input>> nexts;
      nexts.reserve(NUM_NEXTS);
      for (int num_left = NUM_NEXTS; num_left--;) {
	// With no explicit goal.
	Problem::InputGenerator gen =
	  worker->Generator(&rc, nullptr);
	vector<Problem::Input> step;
	step.reserve(num_frames);
	for (int frames_left = num_frames; frames_left--;) {
	  step.push_back(gen.RandomInput(&rc));
	}
	nexts.push_back(std::move(step));
      }

      // PERF: Should drop duplicates and drop sequences that
      // are already in the node. Collisions do happen!

      worker->SetDenom(nexts.size());
      double best_score = -1.0;
      std::unique_ptr<Problem::State> best;
      int best_step_idx = -1;
      for (int i = 0; i < nexts.size(); i++) {
	pftwo->stats.sequences_tried.Increment();
	worker->SetStatus("Re-restore");
	// If this is the first one, no need to restore
	// because we're already in that state.
	if (i != 0) {
	  worker->Restore(expand_me->state);
	  // Since a sequence can only "improve" if it's
	  // not the first one, we store this denominator
	  // separately from sequences_tried.
	  pftwo->stats.sequences_improved_denom.Increment();
	}

	worker->SetStatus("Execute");
	worker->SetNumer(i);
	const vector<Problem::Input> &step = nexts[i];
	for (const Problem::Input &input : step) {
	  worker->Exec(input);
	}

	worker->SetStatus("Observe");
	worker->Observe();

	Problem::State state = worker->Save();
	const double score = pftwo->problem->Score(state);
	if (best.get() == nullptr || score > best_score) {
	  if (best.get() != nullptr) {
	    pftwo->stats.sequences_improved.Increment();
	  }
	  best.reset(new Problem::State(std::move(state)));
	  best_step_idx = i;
	  best_score = score;
	}
      }

      worker->SetStatus("Extend tree");
      // TODO: Consider inserting nodes other than the best one?
      // PERF move best?
      last = ExtendNode(expand_me, nexts[best_step_idx], *best, best_score);

      MaybeUpdateTree();

      worker->SetStatus("Check for death");
      if (ReadWithLock(&should_die_m, &should_die)) {
	worker->SetStatus("Die");
	return;
      }
    }
  }

  // Expects that should_die is true or will become true.
  ~WorkThread() {
    Printf("Worker %d shutting down...\n", id);
    th.join();
    Printf("Worker %d done.\n", id);
  }

  Worker *GetWorker() const { return worker; }

private:
  PF2 *pftwo = nullptr;
  const int id = 0;
  // Distinct private stream.
  ArcFour rc;
  // Private gaussian stream, aliasing rc.
  RandomGaussian gauss;
  Problem::Worker *worker = nullptr;
  std::thread th;
};

// Note: This is really the main thread. SDL really doesn't
// like being called outside the main thread, even if it's
// exclusive.
struct UIThread {
  UIThread(PF2 *pftwo) : pftwo(pftwo) {}

  // Dump the tree to the "tree" subdirectory as some HTML/JSON/PNGs.
  // Your responsibility to clean this all up and deal with multiple
  // versions being spit into the same dir.
  void DumpTree() {
    MutexLock ml(&pftwo->tree_m);
    printf("Dumping tree.");
    Util::makedir("tree");

    std::unique_ptr<Worker> tmp{pftwo->problem->CreateWorker()};

    vector<int> expansion_cutoff;
    std::function<void(const Tree::Node *)> Count =
      [this, &expansion_cutoff, &Count](const Tree::Node *node) {
      if (node->chosen > 0) {
	expansion_cutoff.push_back(node->chosen);
      }
      for (const auto &p : node->children) {
	Count(p.second);
      }
    };
    Count(pftwo->tree->root);
    std::sort(expansion_cutoff.begin(), expansion_cutoff.end(),
	      std::greater<int>());

    static constexpr int kMaxImages = 1000;
    const int cutoff = expansion_cutoff.size() > kMaxImages ?
      expansion_cutoff[kMaxImages] : 0;
    expansion_cutoff.clear();
    printf("Nodes expanded more than %d times will have images.\n",
	   cutoff);

    int images = 0;
    int node_num = 0;
    std::function<string(const Tree::Node *)> Rec =
      [this, &tmp, cutoff, &images, &node_num, &Rec](const Tree::Node *node) {
      int id = node_num++;
      string ret = StringPrintf("{i:%d", id);
      CHECK(node->location != -1);

      double score = -pftwo->tree->heap.GetCell(node).priority;
      ret += StringPrintf(",s:%s", Rtos(score).c_str());

      if (node->chosen > 0) {
	ret += StringPrintf(",e:%d,w:%d", node->chosen, node->was_loss);
      }

      if (node->chosen > cutoff) {
	tmp->Restore(node->state);

	// UGH HACK. After restoring a state we don't have an image
	// unless we make a step. We could replay to here from the
	// parent node (accurate; slow), or be storing these in the
	// state (but they are 260kb each!)
	//
	// So the images stored are actually a single random frame
	// AFTER the one in the node.
	tmp->Exec(tmp->AnyInput());
	vector<uint8> argb256x256;
	argb256x256.resize(256 * 256 * 4);
	tmp->Visualize(nullptr, &argb256x256);

	SaveARGB(argb256x256, 256, 256, StringPrintf("tree/%d.png", id));
	images++;
	ret += ",g:1";
      }

      if (!node->children.empty()) {
	string ch;
	for (const auto &p : node->children) {
	  if (!ch.empty()) ch += ",";
	  // Note, discards sequence.
	  ch += Rec(p.second);
	}
	ret += ",c:[";
	ret += ch;
	ret += "]";
      }
      ret += "}";
      return ret;
    };

    string json = StringPrintf(
	"/* Generated by pftwo.cc. Do not edit! */\n"
	"var treedata = %s\n;",
	Rec(pftwo->tree->root).c_str());
    printf("Wrote %d images. Writing to tree/tree.js\n", images);
    Util::WriteFile("tree/tree.js", json);
  }

  void Loop() {
    SDL_Surface *surf = sdlutil::makesurface(256, 256, true);
    SDL_Surface *surfhalf = sdlutil::makesurface(128, 128, true);
    (void)frame;
    (void)surf;
    (void)surfhalf;

    const int num_workers = pftwo->workers.size();
    vector<vector<uint8>> screenshots;
    screenshots.resize(num_workers);
    for (auto &v : screenshots) v.resize(256 * 256 * 4);

    double start = SDL_GetTicks() / 1000.0;

    for (;;) {
      frame++;
      SDL_Event event;
      
      if (SDL_PollEvent(&event)) {
	switch (event.type) {
	case SDL_QUIT:
	  return;
	case SDL_KEYDOWN:
	  switch (event.key.keysym.sym) {

	  case SDLK_ESCAPE:
	    return;

	  case SDLK_t:
	    DumpTree();
	    break;
	  default:
	    break;
	  }
	  break;
	default:
	  break;
	}
      }
	
      SDL_Delay(1000.0 / 30.0);

      // Every ten thousand frames, write FM2 file.
      // TODO: Superimpose all of the trees at once.
      if (frame % 10000 == 0) {
	MutexLock ml(&pftwo->tree_m);
	printf("Saving best.\n");
	auto best = pftwo->tree->heap.GetByIndex(0);

	// Each segment of the solution, along with a subtitle for
	// the sequence.
	std::list<pair<string, Tree::Seq>> path;

	const Tree::Node *n = best.value;
	while (n->parent != nullptr) {
	  const Tree::Node *parent = n->parent;

	  auto GetKey = [n, parent]() -> const Tree::Seq * {
	    // Find among my siblings.
	    for (const auto &p : parent->children) {
	      if (p.second == n) {
		return &p.first;
	      }
	    };
	    return nullptr;
	  };

	  const Tree::Seq *seq = GetKey();
	  CHECK(seq != nullptr) << "Oops, when saving, I couldn't find "
	    "the path to this node; it must have been misparented!";

	  path.emplace_front(n->subtitle, *seq);
	  n = parent;
	}

	// Create subtitle vector.
	vector<pair<int, string>> subtitles;
	vector<Problem::Input> all_inputs;
	int node_num = 0;
	for (const pair<string, Tree::Seq> &p : path) {
	  // Could put more info from the node here...
	  subtitles.emplace_back((int)all_inputs.size(),
				 StringPrintf("%d. %s", node_num,
					      p.first.c_str()));
	  for (const Problem::Input input : p.second)
	    all_inputs.push_back(input);
	  node_num++;
	}
	string filename = StringPrintf("frame-%lld", frame);
	pftwo->problem->SaveSolution(filename,
				     all_inputs,
				     subtitles,
				     "generatd by pftwo");
      }


      sdlutil::clearsurface(screen, 0x11111111);

      int64 tree_size = ReadWithLock(&pftwo->tree_m, &pftwo->tree->num_nodes);

      const double improvement_pct =
	(pftwo->stats.sequences_improved.Get() * 100.0) /
	(double)pftwo->stats.sequences_improved_denom.Get();
      font->draw(
	  1, 0,
	  StringPrintf(
	      "Frame %d! "
	      "%lld tree nodes  "
	      "%d collisions  "
	      "%s%% improvement rate  "
	      "%d/%d explore deaths",
	      frame,
	      tree_size,
	      pftwo->stats.same_expansion.Get(),
	      Rtos(improvement_pct).c_str(),
	      pftwo->stats.explore_deaths.Get(),
	      pftwo->stats.explore_iters.Get()));

      int64 total_steps = 0ll;
      for (int i = 0; i < pftwo->workers.size(); i++) {
	const Worker *w = pftwo->workers[i]->GetWorker();
	int numer = w->numer.load(std::memory_order_relaxed);
	font->draw(256 * 6 + 10, 40 + FONTHEIGHT * i,
		   StringPrintf("[%d] %d/%d %s",
				i,
				numer,
				w->denom.load(std::memory_order_relaxed),
				w->status.load(std::memory_order_relaxed)));
	total_steps += w->nes_frames.load(std::memory_order_relaxed);
      }

      bool queue_mode = false;
      {
	MutexLock ml(&pftwo->tree_m);
	// Update all screenshots.
	if (!pftwo->tree->explore_queue.empty()) {
	  queue_mode = true;
	  static constexpr int STARTY = 128 + FONTHEIGHT + FONTHEIGHT;
	  // Draw queue!
	  // XXX Ugh, there's no good way to visualize in a queue-centric
	  // way, because we need a worker in order to compute a screenshot.
	  font->draw(30, STARTY,
		     StringPrintf("^2EXPLORE QUEUE SIZE: %d",
				  pftwo->tree->explore_queue.size()));

	  int ypos = STARTY + FONTHEIGHT + 2;;
	  for (const Tree::ExploreNode &en : pftwo->tree->explore_queue) {
	    if (ypos > HEIGHT - FONTHEIGHT - 1) break;
	    font->draw(30, ypos,
		       StringPrintf("Goal ^3%d^<,^3%d^<  "
				    "distance ^4%.2f^<  "
				    "seq ^5%d^<   ^1%d^<|^4%d^<  "
				    "bad ^2%d^<",
				    // XXX hard-coded TPP
				    en.goal.goalx,
				    en.goal.goaly,
				    en.distance,
				    (int)en.closest_seq.size(),
				    en.iterations_left,
				    en.iterations_in_progress,
				    en.bad));
	    ypos += FONTHEIGHT;
	  }

	}
      }

      {
	MutexLock ml(&pftwo->tree_m);
	static constexpr int GRIDX = 768;
	static constexpr int GRIDY = 450;
	static constexpr int CELLPX = 16;
	
	// Get best score in grid so that we can normalize
	// colors against it.
	double cutoff_bestscore =
	  -pftwo->tree->heap.GetMinimum().priority * GRID_BESTSCORE_FRAC;
	double bestscore = 0.0;
	for (const Tree::GridCell &gc : pftwo->tree->grid)
	  if (gc.score > bestscore) bestscore = gc.score;
	const double one_over_bestscore =
	  bestscore > 0.0 ? 1.0 / bestscore : 0.0;
	
	// Draw grid.
	// XXX this specific to the current TwoPlayerProblem
	// TODO! Save little pictures for each grid cell; it'd
	// look awesome!
	for (int cy = 0; cy < Problem::GRID_CELLS_Y; cy++) {
	  for (int cx = 0; cx < Problem::GRID_CELLS_X; cx++) {
	    const Tree::GridCell &gc =
	      pftwo->tree->grid[cy * Problem::GRID_CELLS_Y + cx];
	    if (gc.node == nullptr) {
	      sdlutil::FillRectRGB(screen,
				   GRIDX + cx * CELLPX,
				   GRIDY + cy * CELLPX,
				   CELLPX, CELLPX,
				   0x20, 0x0, 0x0);
	    } else {
	      uint8 rr = gc.score >= cutoff_bestscore ? 0 : 1;
	      int gg = 0x20 +
		(0xFF - 0x20) * (gc.score * one_over_bestscore);
	      if (gg > 0xFF) gg = 0xff;
	      sdlutil::FillRectRGB(screen,
				   GRIDX + cx * CELLPX,
				   GRIDY + cy * CELLPX,
				   CELLPX, CELLPX,
				   rr * gg, (uint8)gg, 0x0);
	    }
	  }
	}
	smallfont->draw(GRIDX, GRIDY,
			StringPrintf("best ^3%.4f^< / ^2%.4f",
				     bestscore,
				     cutoff_bestscore));
      }
      
      // Draw workers workin'.
      vector<vector<string>> texts;
      texts.resize(num_workers);
      for (int i = 0; i < num_workers; i++) {
	Worker *w = pftwo->workers[i]->GetWorker();
	w->Visualize(nullptr, &screenshots[i]);
	w->VizText(nullptr, &texts[i]);
      }

      for (int i = 0; i < num_workers; i++) {
	int x = i % 12;
	int y = i / 12;
	BlitARGBHalf(screenshots[i], 256, 256,
		     x * 128, y * 128 + 20, screen);

	if (!queue_mode) {
	  for (int t = 0; t < texts[i].size(); t++) {
	    font->draw(x * 128,
		       (y + 1) * 128 + 20 + t * FONTHEIGHT,
		       texts[i][t]);
	  }
	}
      }
	
	
      // Gather tree statistics.
      struct LevelStats {
	int count = 0;
	int max_chosen = 0;
	int64 chosen = 0;
	int workers = 0;
	double best_score = 0.0;
      };
      vector<LevelStats> levels;

      struct TreeStats {
	int nodes = 0;
	int leaves = 0;
	int64 statebytes = 0LL;
      };
      TreeStats treestats;

      vector<double> all_scores;
      // Note that there can be a race here, but it's just a
      // performance hint.
      all_scores.reserve(tree_size);

      std::function<void(int, const Tree::Node *)> GetLevelStats =
	[this, &levels, &treestats, &all_scores, &GetLevelStats](
	    int depth, const Tree::Node *n) {
	while (depth >= levels.size())
	  levels.push_back(LevelStats{});

	treestats.nodes++;
	treestats.statebytes += Problem::StateBytes(n->state);

	LevelStats *ls = &levels[depth];
	ls->count++;
	ls->chosen += n->chosen;
	if (n->chosen > ls->max_chosen)
	  ls->max_chosen = n->chosen;

	ls->workers += n->num_workers_using;

	CHECK(n->location != -1);

	double score = -pftwo->tree->heap.GetCell(n).priority;
	all_scores.push_back(score);
	if (score > ls->best_score) ls->best_score = score;
	// TODO save node so that we can draw picture?

	if (n->children.empty())
	  treestats.leaves++;

	for (const auto &child : n->children) {
	  GetLevelStats(depth + 1, child.second);
	}
      };

      {
	MutexLock ml(&pftwo->tree_m);
	GetLevelStats(0, pftwo->tree->root);
      }

      smallfont->draw(256 * 6 + 10, 220,
		      StringPrintf("^1Tree: ^3%d^< nodes, ^3%d^< leaves, "
				   "^3%lld^< state MB, ^4%.2f^< KB avg",
				   treestats.nodes, treestats.leaves,
				   treestats.statebytes / (1024LL * 1024LL),
				   treestats.statebytes / (1024.0 *
							   treestats.nodes)));
      for (int i = 0; i < levels.size(); i++) {
	string w;
	if (levels[i].workers) {
	  for (int j = 0; j < levels[i].workers; j++)
	    w += '*';
	}

	smallfont->draw(256 * 6 + 10, 230 + i * SMALLFONTHEIGHT,
			StringPrintf("^4%2d^<: ^3%d^< n %lld c %d mc, "
				     "%.3f ^5%s",
				     i, levels[i].count,
				     levels[i].chosen,
				     levels[i].max_chosen,
				     levels[i].best_score,
				     w.c_str()));
      }

      static constexpr int BOTTOM = HEIGHT - FONTHEIGHT - 3;

      const double now = SDL_GetTicks() / 1000.0;
      double sec = now - start;
      font->draw(10, HEIGHT - FONTHEIGHT,
		 StringPrintf("%.2f NES Mframes in %.1f sec = %.2f NES kFPS "
			      "%.2f UI FPS",
			      total_steps / 1000000.0, sec,
			      (total_steps / sec) / 1000.0,
			      frame / sec));

      // XXX maybe show the grid cells in the histo
      std::sort(all_scores.rbegin(), all_scores.rend());
      static constexpr int HISTO_HEIGHT = 128;
      sdlutil::slock(screen);
      for (int x = 0; x < all_scores.size() && x < WIDTH; x++) {
	double score = all_scores[x];
	bool over = false, under = false;
	(void)under;
	if (score > 1.0) {
	  score = 1.0;
	  over = true;
	} else if (score < 0) {
	  score = 0;
	  under = true;
	}
	double heightf = score * HISTO_HEIGHT;
	int height = heightf;
	// Fractional pixel for anti-aliasing
	uint8 leftover = 255.0 * (heightf - height);
	for (int h = 0; h < height; h++) {
	  int y = BOTTOM - h;
	  sdlutil::drawclippixel(screen, x, y,
				 0xFF, 0xFF, over ? 0xFF : 0x00);
	}
	sdlutil::drawclippixel(screen, x, BOTTOM - height,
			       leftover, leftover, 0x00);

      }
      sdlutil::sulock(screen);

      maxfont->draw(
	  10, HEIGHT - HISTO_HEIGHT / 2 - MAXFONTHEIGHT / 2,
	  StringPrintf("^2%.2f%%", pftwo->tree->stuckness));
      
      SDL_Flip(screen);
    }

  }

  void Run() {
    screen = sdlutil::makescreen(WIDTH, HEIGHT);
    CHECK(screen);

    font = Font::create(screen,
			"font.png",
			FONTCHARS,
			FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
    CHECK(font != nullptr) << "Couldn't load font.";

    smallfont = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(smallfont != nullptr) << "Couldn't load smallfont.";

    smallfont = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(smallfont != nullptr) << "Couldn't load smallfont.";

    maxfont = Font::create(screen,
			   "fontmax.png",
			   FONTCHARS,
			   MAXFONTWIDTH, MAXFONTHEIGHT, FONTSTYLES, 4, 3);
    CHECK(maxfont != nullptr) << "Couldn't load maxfont.";
    
    Loop();
    Printf("UI shutdown.\n");
    WriteWithLock(&should_die_m, &should_die, true);
  }

  ~UIThread() {
    // XXX free screen
    delete font;
  }

 private:
  static constexpr int FONTWIDTH = 9;
  static constexpr int FONTHEIGHT = 16;
  static constexpr int SMALLFONTWIDTH = 6;
  static constexpr int SMALLFONTHEIGHT = 6;
  static constexpr int MAXFONTHEIGHT = 48 * 2;
  static constexpr int MAXFONTWIDTH = 27 * 2;

  Font *font = nullptr, *smallfont = nullptr, *maxfont = nullptr;

  ArcFour rc{"ui_thread"};
  PF2 *pftwo = nullptr;
  SDL_Surface *screen = nullptr;
  int64 frame = 0LL;
};

void PF2::StartThreads() {
  CHECK(workers.empty());
  CHECK(num_workers > 0);
  workers.reserve(num_workers);
  for (int i = 0; i < num_workers; i++) {
    workers.push_back(new WorkThread(this, i));
  }

  ui_thread = new UIThread(this);
  ui_thread->Run();
}

void PF2::DestroyThreads() {
  delete ui_thread;
  ui_thread = nullptr;
  for (WorkThread *wt : workers)
    delete wt;
  workers.clear();
}


// The main loop for SDL.
int main(int argc, char *argv[]) {
  fprintf(stderr, "Init SDL\n");

  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  /* Initialize SDL. */
  CHECK(SDL_Init(SDL_INIT_VIDEO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_Surface *icon = SDL_LoadBMP("icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  {
    PF2 pftwo;
    pftwo.Play();
  }

  SDL_Quit();
  return 0;
}
