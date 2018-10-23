#include <algorithm>
#include <vector>
#include <string>
#include <set>
#include <memory>
#include <list>
#include <unordered_set>

#ifdef __MINGW32__
#include <windows.h>
#undef ARRAYSIZE
#endif

#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/list-util.h"

#include "atom7ic.h"

#include "weighted-objectives.h"
#include "treesearch.h"
#include "problem-twoplayer.h"

// Base "max" nodes in heap. We start cleaning the heap when there are
// more than this number of nodes, although we often have to keep more
// than this number (because some are in use by workers, grid, etc.)
#define BASE_NODE_BUDGET 1000
// We want to always include space for the path from the root to the
// best node, so "max nodes" is really
//   BASE_NODE_BUDGET + max_depth * NODE_BUDGET_BONUS_PER_DEPTH
#define NODE_BUDGET_BONUS_PER_DEPTH 2

// We consider exploring from the grid if the score is
// at least GRID_BESTSCORE_FRAC * best_score_in_heap.
#define GRID_BESTSCORE_FRAC 0.90

// We consider exploring from a marathon node if its
// score is at least MARATHON_BESTSCORE_FRAC * best_score_in_heap.
#define MARATHON_BESTSCORE_FRAC 0.95

// Number of loops a single thread does when expanding the explore
// queue.
#define LOOPS_PER_EXPLORE_ITER 50

static std::mutex print_mutex;
#define Printf(fmt, ...) do {			\
    MutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);			\
  } while (0)

// All events that we count.
enum PerfEvent {
  // Locks
  PE_L_COMMIT_QUEUE,
  PE_L_GET_NODES_TO_EXTEND,
  PE_L_GET_EXPLORE_NODE_E,
  PE_L_UPDATE_TREE_A,
  PE_L_UPDATE_TREE_B,
  PE_L_UPDATE_TREE_C,
  PE_L_UPDATE_TREE_D,
  PE_L_PROCESS_EXPLORE_QUEUE_A,
  PE_L_PROCESS_EXPLORE_QUEUE_ES,
  PE_L_PROCESS_EXPLORE_QUEUE_B,
  PE_L_PROCESS_EXPLORE_QUEUE_EB,
  PE_L_PROCESS_EXPLORE_QUEUE_C,
  PE_L_PROCESS_EXPLORE_QUEUE_D,
  PE_L_PROCESS_EXPLORE_QUEUE_EZ,
  PE_L_PROCESS_EXPLORE_QUEUE_ED,
  PE_L_PROCESS_EXPLORE_QUEUE_R,
  PE_L_MARATHON_START,
  PE_L_MARATHON_FAILED,
  PE_L_SHOULD_DIE_EQ,
  PE_L_SHOULD_DIE_N,
  // Work
  PE_EXEC,
  PE_OBSERVE,
  PE_COMMIT,
  PE_IN_CONTROL,
  // Meta
  NUM_PERFEVENTS,
};

static const char *PerfEventString(PerfEvent pe) {
#define CASE(c) case PE_ ## c: return # c;
  switch (pe) {
    CASE(L_COMMIT_QUEUE);
    CASE(L_GET_NODES_TO_EXTEND);
    CASE(L_GET_EXPLORE_NODE_E);
    CASE(L_UPDATE_TREE_A);
    CASE(L_UPDATE_TREE_B);
    CASE(L_UPDATE_TREE_C);
    CASE(L_UPDATE_TREE_D);
    CASE(L_PROCESS_EXPLORE_QUEUE_A);
    CASE(L_PROCESS_EXPLORE_QUEUE_ES);
    CASE(L_PROCESS_EXPLORE_QUEUE_B);
    CASE(L_PROCESS_EXPLORE_QUEUE_EB);
    CASE(L_PROCESS_EXPLORE_QUEUE_C);
    CASE(L_PROCESS_EXPLORE_QUEUE_D);
    CASE(L_PROCESS_EXPLORE_QUEUE_EZ);
    CASE(L_PROCESS_EXPLORE_QUEUE_ED);
    CASE(L_PROCESS_EXPLORE_QUEUE_R);
    CASE(L_MARATHON_START);
    CASE(L_MARATHON_FAILED);
    CASE(L_SHOULD_DIE_EQ);
    CASE(L_SHOULD_DIE_N);
    CASE(EXEC);
    CASE(OBSERVE);
    CASE(COMMIT);
    CASE(IN_CONTROL);
  default: return "?";
  }
#undef CASE
}

static inline uint64 PerfCounterNow() {
  uint64 ret;
  QueryPerformanceCounter((LARGE_INTEGER*)&ret);
  return ret;
}

namespace {
struct InternalPerfCounterScoped {
  InternalPerfCounterScoped(uint64 *dst) :
    dst(dst),
    start(PerfCounterNow()) {}
  ~InternalPerfCounterScoped() {
    const uint64 end = PerfCounterNow();
    *dst += (end - start);
  }
  uint64 *dst = nullptr;
  const uint64 start = 0ULL;
};
}

static_assert(sizeof (uint64) == sizeof (LARGE_INTEGER), "win64");
#define PERF_MUTEX_LOCK_INTERNAL(pe, mut, C)			\
  const uint64 perf_ml_start = PerfCounterNow();		\
  C perf_ml(mut);						\
  const uint64 perf_ml_end = PerfCounterNow();			\
  perf_counters[pe] += (perf_ml_end - perf_ml_start)

#define PERF_READ_MUTEX_LOCK(pe, mut)				\
  PERF_MUTEX_LOCK_INTERNAL(pe, mut, ReadMutexLock)

#define PERF_WRITE_MUTEX_LOCK(pe, mut)				\
  PERF_MUTEX_LOCK_INTERNAL(pe, mut, WriteMutexLock)

// This deliberately does not generate a distinct name to prevent
// unintentially instantiating it multiple times in the same scope.
// Use {} to delimit the scope explicitly.
#define PERF_SCOPED(pe) \
  InternalPerfCounterScoped perf_scoped(&perf_counters[pe])

struct WorkThread {
  uint64 perf_counter_start = 0LL;
  uint64 perf_counters[NUM_PERFEVENTS] = {};
  
  using Node = Tree::Node;
  WorkThread(TreeSearch *search, int id) :
    search(search), id(id),
    rc(StringPrintf("%d,worker_%d", search->opt.random_seed, id)),
    gauss(&rc),
    opt(search->opt),
    worker(search->problem->CreateWorker()),
    th{&WorkThread::Run, this} {
    perf_counter_start = PerfCounterNow();
  }

  uint64 PerfGetTotal() const {
    return PerfCounterNow() - perf_counter_start;
  }
  
  // Get the root node of the tree and associated state (which must be
  // present). Used during initialization.
  Node *GetRoot() {
    WriteMutexLock ml(&search->tree_m);
    Node *n = search->tree->root;
    n->num_workers_using++;
    return n;
  }

  // Populates the vector with eligible grid indices.
  void EligibleGridNodesWithMutex(vector<int> *eligible) {
    // XXX This stuff is a hack. Improve it!
    const double bestscore = -search->tree->heap.GetMinimum().priority;
    const double gminscore = GRID_BESTSCORE_FRAC * bestscore;
    // The interval from gminscore to bestscore is size (1.0 -
    // grid_bestscore_frac). When the cell's score falls in this
    // range we consider it, and we select it with probability
    // proportional to its distance within this interval. This
    // value is what we multiply the distance by to get a value
    // from (nominally) 0 to 1.
    constexpr double ival_norm = 1.0 / (1.0 - GRID_BESTSCORE_FRAC);

    for (int idx = 0; idx < search->tree->grid.size(); idx++) {
      const Tree::GridCell &gc = search->tree->grid[idx];
      if (gc.node != nullptr && gc.score >= gminscore) {
	const double p = (gc.score - gminscore) * ival_norm;
	if (RandDouble(&rc) < p) {
	  eligible->push_back(idx);
	}
      }
    }
  }
  
  // Must hold tree mutex!
  // Doesn't update any reference counts.
  Node *FindGoodNodeWithMutex() {
    const int size = search->tree->heap.Size();
    CHECK(size > 0) << "Heap should always have root, at least.";

    Tree *tree = search->tree;
    
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
      ret = tree->grid[idx].node;
    }

    // Consider expanding the marathon node if we have one and we are
    // stuck enough. We have to at least sometimes expand this node
    // normally, since when doing the marathon strategy we refuse to
    // expand if we lose control. That may put us in a situation where
    // we randomly chip away at a boss, but then never actually beat
    // the level (which often involves losing control).
    if (ret == nullptr &&
	tree->stuckness > 0.75 && tree->marathon.node != nullptr) {
      const double bestscore = -tree->heap.GetMinimum().priority;
      const double mminscore = MARATHON_BESTSCORE_FRAC * bestscore;

      if (tree->marathon.score >= mminscore &&
	  RandDouble(&rc) <
	  // Reach p_expand_marathon probability when stuckness is 1.0.
	  opt.p_expand_marathon * ((tree->stuckness - 0.75) * 4.0)) {
	ret = tree->marathon.node;
      }
    }
    
    if (ret == nullptr) {
      // The idea here is to choose a good node to start; in a heap,
      // good nodes are towards the beginning of the array.
      // Gaussian centered on 0; use 0 for anything out of bounds.
      // (n.b. this doesn't seem to help get unstuck -- evaluate it)
      const int g = (int)(gauss.Next() * (size * 0.05));
      const int idx = (g <= 0 || g >= size) ? 0 : g;

      Heap<double, Node>::Cell cell = tree->heap.GetByIndex(idx);
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
    
  // Get a batch of nodes for the main search procedure. Each node
  // has its workers_using counter incremented on behalf of the
  // caller (who must eventually decrement it).
  vector<Node *> GetNodesToExtend() {
    PERF_WRITE_MUTEX_LOCK(PE_L_GET_NODES_TO_EXTEND, &search->tree_m);
    vector<Node *> ret;
    ret.reserve(opt.node_batch_size);
    for (int i = 0; i < opt.node_batch_size; i++) {
      Node *n = FindGoodNodeWithMutex();
      n->num_workers_using++;
      n->chosen++;
      ret.push_back(n);
    }
    return ret;
  }

  // Construct a new node. 
  Node *NewNode(Problem::State newstate, Node *parent, int seq_length) {
    CHECK(parent != nullptr);
    // XXX constructor actually reads parent depth without lock?
    Node *child = new Node(std::move(newstate), parent,
			   parent->seqlength + seq_length);
    child->nes_frames = search->approx_total_nes_frames.load(
	std::memory_order_relaxed);
    child->walltime_seconds = search->approx_sec.load(
    	std::memory_order_relaxed);
    return child;
  }
  
  // Add to the grid if it qualifies. Called from within
  // ExtendNodeWithLock once we're sure the node is new.
  void AddToGridWithLock(Node *newnode, double newscore) {
    int cell = 0;
    if (search->problem->GetGridCell(newnode->state, &cell)) {
      CHECK(cell >= 0 && cell < search->tree->grid.size()) <<
	cell << " vs " << search->tree->grid.size();
      Tree::GridCell *gc = &search->tree->grid[cell];
      if (gc->node == nullptr || newscore > gc->score) {
	if (gc->node)
	  gc->node->used_in_grid--;
	newnode->used_in_grid++;
	gc->score = newscore;
	gc->node = newnode;
      }
    }
  }

  // Set this to the marathon node if it's a new best. Caller
  // must ensure that IsInControl for this node.
  void AddToMarathonWithLock(Node *newnode, double newscore) {
    Tree *tree = search->tree;
    // We try to maximize depth (steps) but not 
    const double bestscore = -tree->heap.GetMinimum().priority;
    const double mminscore = MARATHON_BESTSCORE_FRAC * bestscore;

    if (tree->marathon.node == nullptr ||
	(newscore >= mminscore &&
	 newnode->seqlength > tree->marathon.node->seqlength)) {
      if (tree->marathon.node != nullptr)
	tree->marathon.node->used_in_marathon--;
      newnode->used_in_marathon++;
      tree->marathon.score = newscore;
      tree->marathon.node = newnode;
    }
  }
  
  struct QueuedUpdate {
    // The starting node. Note that this may not actually yet
    // be in the main tree, because it may be part of a contiguous
    // branch that's part of the queued upate. Reference count will
    // be nonzero.
    Node *src = nullptr;
    // The sequence that brings us to the new child node.
    Tree::Seq seq;
    // The child node, which is already allocated, but not
    // yet linked into the tree.
    Node *dst = nullptr;
    double newscore = 0.0;
    // True if we thought there was a good chance that we'd update
    // the marathon node with this, so we did the expensive check
    // for IsInControl, and it was true.
    bool checked_in_control = false;
    QueuedUpdate(Node *src, Tree::Seq seq, Node *dst, double newscore,
		 bool checked_in_control) :
      src(src), seq(std::move(seq)), dst(dst), newscore(newscore),
      checked_in_control(checked_in_control) {}
  };
  vector<QueuedUpdate> local_queue;

  // Flush the local queue to global state.
  void CommitQueue() {
    if (local_queue.empty())
      return;

    std::unordered_set<Node *> blacklist;
        
    {
      PERF_WRITE_MUTEX_LOCK(PE_L_COMMIT_QUEUE, &search->tree_m);
      for (QueuedUpdate &q : local_queue) {

	if (ContainsKey(blacklist, q.src)) {
	  // We had to delete the parent node because of a collision,
	  // so this node also needs to be deleted.
	  blacklist.insert(q.dst);
	  delete q.dst;
	  continue;
	}

	// XXX This should probably be done in the caller, because
	// if NUM_NEXTS isn't 1, we have more fine-grained evidence
	// that we could collect. (Right now it's like, "the probability
	// that randomly expanding the node NUM_NEXTS times and picking
	// the best one will actually make things worse") which is maybe
	// harder to think about, and certainly converges more slowly.
	const double oldscore = search->problem->Score(q.src->state);
	if (oldscore > q.newscore) {
	  q.src->was_loss++;
	}

	auto res = q.src->children.insert({std::move(q.seq), q.dst});

	CHECK(q.src->num_workers_using > 0);
	q.src->num_workers_using--;

	if (!res.second) {
	  // By dumb luck (this might not be that rare if the markov
	  // model is sparse), we already have a node with this
	  // exact path. We don't allow replacing it.
	  search->stats.same_expansion.Increment();

	  // Now, this is fairly annoying since we may have expanded
	  // from the child already. If so, they can only appear later
	  // in the local queue. 
	  blacklist.insert(q.dst);
	  delete q.dst;

	} else {
	  search->tree->num_nodes++;
	  search->tree->heap.Insert(-q.newscore, q.dst);
	  CHECK(q.dst->location != -1);

	  AddToGridWithLock(q.dst, q.newscore);
	  if (q.checked_in_control)
	    AddToMarathonWithLock(q.dst, q.newscore);
	}
      }
    }
    local_queue.clear();
  }
  
  // Save the best expansion of the node n to be added to the local
  // queue and committed later in a batch. The source node must have a
  // corresponding refcount, which this decrements when it is
  // committed.
  Node *EnqueueExpansionResult(Node *n, Tree::Seq seq,
			       std::unique_ptr<Problem::State> newstate,
			       double newscore,
			       bool checked_in_control) {
    Node *child = NewNode(std::move(*newstate), n, seq.size());
    local_queue.emplace_back(n, std::move(seq), child, newscore,
			     checked_in_control);
    return child;
  }
  
  // At startup, ensure that the tree contains at least a root node.
  // Only does it in one thread, but doesn't return until this is the
  // case.
  void InitializeTree() {
    WriteMutexLock ml(&search->tree_m);
    if (search->tree == nullptr) {
      // I won the race!
      printf("Initialize tree...\n");
      Problem::State s = worker->Save();
      search->tree = new Tree(search->problem->Score(s), s);
    }
  }

  void MaybeUpdateTree() {
    Tree *tree = search->tree;
    {
      PERF_WRITE_MUTEX_LOCK(PE_L_UPDATE_TREE_A, &search->tree_m);

      // No use in decrementing update counter -- when we
      // finish we reset it to the max value.
      if (tree->update_in_progress)
	return;

      // Also, we're not allowed to make steps until
      // the explore queue is empty.
      if (!tree->explore_queue.empty())
	return;
      
      if (tree->steps_until_update == 0) {
	tree->steps_until_update = opt.UpdateFrequency();
	tree->update_in_progress = true;
	// Enter fixup below.
      } else {
	tree->steps_until_update--;
	return;
      }
    }

    // This is run every UpdateFrequency() calls, in some
    // arbitrary worker thread. We don't hold any locks right
    // now, but only one thread will enter this section at
    // a time because of the update_in_progress flag.
    // worker->SetStatus("Tree: Commit observations");
    worker->SetStatus(STATUS_TREE);
    
    search->problem->Commit();
    
    // Re-build heap. We do this by recalculating the score for
    // each node in place; most of the time this won't change the
    // shape of the heap much.
    {
      // worker->SetStatus("Tree: Reheap");
      {
	PERF_WRITE_MUTEX_LOCK(PE_L_UPDATE_TREE_B, &search->tree_m);

	printf("Clear grid ...");
	// First, just clear the grid since every node has a chance to
	// become the best in a cell below, including the ones that are
	// already there.
	for (Tree::GridCell &gc : search->tree->grid) {
	  if (gc.node != nullptr) {
	    gc.node->used_in_grid--;
	    gc.score = 0.0;
	    gc.node = nullptr;
	  }
	}
	
	printf("Reheap ...\n");

	// tree->heap.Clear();

	std::function<void(Node *)> ReHeapRec =
	  [this, tree, &ReHeapRec](Node *n) {
	  // Act on the node if it's in the heap.
	  if (n->location != -1) {
	    const double new_score = search->problem->Score(n->state);
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

	// We cleared the grid so its scores are vacuously accurate,
	// but we need to make sure the marathon node at least has
	// the right normalized score.
	if (tree->marathon.node != nullptr)
	  tree->marathon.score =
	    search->problem->Score(tree->marathon.node->state);
      }
    }
    
    {
      PERF_WRITE_MUTEX_LOCK(PE_L_UPDATE_TREE_C, &search->tree_m);
      const int64 MAX_NODES = search->tree->MaxNodes();
      if (tree->num_nodes > MAX_NODES) {
	printf("Trim tree (have %llu, max: %llu)...\n",
	       tree->num_nodes, MAX_NODES);

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
	// example, we might beat a level but with one of the players
	// dead. In this case, Score()s might be forever small. So
	// here we normalize against the single best score when
	// computing AUC.
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

	// We popped off the best nodes, so now the heap only contains
	// nodes we'll probably delete.
	tree->heap.Clear();

	// Now make a pass over the tree and clean out nodes where we
	// can. This loop also computes the new maximum depth.
	int max_depth = 0;
	uint64 deleted_nodes = 0ULL;
	uint64 kept_score = 0ULL, kept_worker = 0ULL, kept_parent = 0ULL,
	  kept_grid = 0ULL, kept_marathon = 0LL;
	std::function<bool(Node *)> CleanRec =
	  // Returns true if we should keep this node; otherwise,
	  // the node is deleted.
	  [this, tree, &max_depth, &deleted_nodes,
	   &kept_score, &kept_worker, &kept_parent, &kept_grid,
	   &kept_marathon,
	   &CleanRec](Node *n) -> bool {
	  if (n->keep)
	    kept_score++;
	  if (n->num_workers_using)
	    kept_worker++;
	  if (n->used_in_grid)
	    kept_grid++;
	  if (n->used_in_marathon)
	    kept_marathon++;
	  
	  bool keep = n->keep ||
	      n->num_workers_using > 0 ||
	      n->used_in_grid > 0 ||
	      n->used_in_marathon > 0;

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
	    max_depth = std::max(n->depth, max_depth);
	    // PERF: We have to recompute the score AGAIN here;
	    // particularly for nodes we're keeping because a worker
	    // is using it. We just cleared the entire heap so we
	    // don't even have its score.
	    const double new_score = search->problem->Score(n->state);
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

	tree->max_depth = max_depth;
	
	printf(" ... Reasons for keeping nodes:\n"
	       "     score is good: %llu\n"
	       "     worker using: %llu\n"
	       "     in grid: %llu\n"
	       "     in marathon: %llu\n"
	       "     child is kept: %llu\n",
	       kept_score, kept_worker, kept_grid, kept_marathon,
	       kept_parent);

	printf(" ... Deleted %llu; now the tree is size %llu.\n"
	       " ... Max depth is %d.\n",
	       deleted_nodes,
	       tree->num_nodes,
	       max_depth);

	// Now, find some nodes for exploration.
	CHECK(tree->explore_queue.empty());
	if (tree->stuckness > 0.50) {
	  static constexpr int NUM_EXPLORE_NODES = 50;

	  // Holding the lock, add the explore node, including adding
	  // to the source node's reference count.
	  auto AddExploreNode = [this, tree](
	      Node *source, Problem::Goal goal) {
  	    // Note that each "iteration" does LOOPS_PER_EXPLORE_ITER
            // loops.
	    static constexpr int NUM_EXPLORE_ITERATIONS = 10;
	    source->num_workers_using += LOOPS_PER_EXPLORE_ITER *
	      NUM_EXPLORE_ITERATIONS;
	    Tree::ExploreNode *en = new Tree::ExploreNode;
	    en->source = source;
	    en->goal = goal;
	    en->closest_state = source->state;
	    en->distance =
	      search->problem->GoalDistance(goal, source->state);
	    en->iterations_left = NUM_EXPLORE_ITERATIONS;
	    en->iterations_in_progress = 0;
	    tree->explore_queue.push_back(en);
	  };

	  // More stuck = more exploration.
	  int num_explore = NUM_EXPLORE_NODES * tree->stuckness;
	  printf("Making %d explore nodes.\n", num_explore);
	  int explore_adj = 0, explore_random = 0;
	  
	  // Prioritize using existing cells if we have them.
	  vector<int> goodcells;
	  EligibleGridNodesWithMutex(&goodcells);
	  set<int> isgood;
	  for (int c : goodcells) isgood.insert(c);

	  // Do these in a random order in case there are so many
	  // eligible expansions that we exhaust the list.
	  Shuffle(&rc, &goodcells);
	  printf("  ... with %d goodcells ...\n", (int)goodcells.size());
	  while (!goodcells.empty() && num_explore > 0) {
	    // For any eligible cell, consider its adjacent neighbors.
	    int cell = goodcells.back();
	    goodcells.pop_back();
	    vector<int> adj = Problem::AdjacentCells(cell);
	    for (int adjcell : adj) {
	      // If we don't have an eligible cell, try going there.
	      // Also some (smaller) chance to try anyway.
	      if (isgood.find(adjcell) == isgood.end() ||
		  rc.Byte() < 32) {
		Node *source = search->tree->grid[cell].node;
		Problem::Goal goal =
		  search->problem->RandomGoalInCell(&rc, adjcell);
		// printf("Explore %s -> %s (%d,%d)");
		// printf("Explore adj %d -> %d (goal %d,%d)\n",
		// cell, adjcell, goal.goalx, goal.goaly);
		
		AddExploreNode(source, goal);
		explore_adj++;
		num_explore--;
		if (num_explore == 0) break;
	      }
	    }
	  }

	  if (num_explore > 0) {
	    printf("  ... pad with %d random goals ...\n", num_explore);
	  }
	  
	  // If we have leftover quota, just do random goals.
	  // TODO: Rather than choosing random goals, choose goals that aren't
	  // already represented in the grid, and perhaps choose ones that are
	  // adjacent to covered cells.
	  while (num_explore--) {
	    // XXX I think it would be better if we required the
	    // score to be close to the max score, because otherwise
	    // there's basically no chance of this helping.
	    Node *source = FindGoodNodeWithMutex();
	    Problem::Goal goal = search->problem->RandomGoal(&rc);
	    // printf("Explore random (goal %d,%d)\n", goal.goalx, goal.goaly);
	    AddExploreNode(source, goal);
	    explore_random++;
	  }
	  printf("Made %d explore nodes (%d adj, %d rand)\n",
		 explore_adj + explore_random, explore_adj, explore_random);
	}
      }
    }


    {
      PERF_WRITE_MUTEX_LOCK(PE_L_UPDATE_TREE_D, &search->tree_m);
      tree->update_in_progress = false;
    }
  }

  // Holding the tree mutex, find any explore node in the explore
  // queue, increment its reference count, and return a pointer to
  // it. The pointer stays valid even if the lock is relinquished,
  // since the reference count is nonzero. Returns nullptr if none
  // can be found.
  Tree::ExploreNode *GetExploreNodeWithMutex() {
    std::list<Tree::ExploreNode *> *explore_queue =
      &search->tree->explore_queue;
    for (std::list<Tree::ExploreNode *>::iterator it = explore_queue->begin();
	 it != explore_queue->end(); ++it) {
      Tree::ExploreNode *en = *it;
      PERF_WRITE_MUTEX_LOCK(PE_L_GET_EXPLORE_NODE_E, &en->node_m);
      if (en->iterations_left > 0) {
	CHECK(en->source->num_workers_using > 0);
	en->iterations_left--;
	en->iterations_in_progress++;
	// Move it to the back, which may hurt with locality, but
	// should help with lock contention. This does not invalidate
	// any pointers nor iterators.
	ListMoveToBack(explore_queue, it);
	return en;
      }
    }
    // Didn't find any.
    return nullptr;
  }

  // Run marathon if applicable, and enqueue update.
  void RunMarathon() {
    Node *src = nullptr;
    double mminscore = 0.0;
    {
      PERF_WRITE_MUTEX_LOCK(PE_L_MARATHON_START, &search->tree_m);
      Tree *tree = search->tree;
      if (tree->marathon.node == nullptr)
	return;

      // Marathon node has to be good enough to consider expanding it.
      // Otherwise we should spend our time trying to do normal
      // exploration.
      const double bestscore = -tree->heap.GetMinimum().priority;
      mminscore = MARATHON_BESTSCORE_FRAC * bestscore;

      if (tree->marathon.score < mminscore)
	return;

      src = tree->marathon.node;
      src->num_workers_using++;
    }

    // Invariant: The sequence we generate always leaves us in a
    // state that IsInControl. This is satisfied for the empty
    // sequence because the marathon node has the same invariant.
    Tree::Seq seq;
    
    constexpr int MARATHON_LAPS = 100;
    constexpr int MARATHON_LAP_LENGTH = 30;
    worker->SetStatus(STATUS_MARATHON);
    worker->SetDenom(MARATHON_LAPS);
    worker->Restore(src->state);
    for (int lap = 0; lap < MARATHON_LAPS; lap++) {
      worker->SetNumer(lap);
      Problem::State undo = worker->Save();

      // With no explicit goal.
      Problem::InputGenerator gen =
	worker->Generator(&rc, nullptr);
      vector<Problem::Input> step;
      step.reserve(MARATHON_LAP_LENGTH);
      for (int frames_left = MARATHON_LAP_LENGTH; frames_left--;) {
	step.push_back(gen.RandomInput(&rc));
      }

      {
	PERF_SCOPED(PE_EXEC);
	for (const Problem::Input &input : step)
	  worker->Exec(input);
      }

      // Accept?
      bool in_control = false;
      {
	PERF_SCOPED(PE_IN_CONTROL);
	in_control = worker->IsInControl();
      }
	
      if (in_control) {
	for (const Problem::Input &input : step)
	  seq.push_back(input);
      } else {
	worker->Restore(undo);
      }
    }

    if (!seq.empty()) {
      std::unique_ptr<Problem::State> newstate =
	std::make_unique<Problem::State>(worker->Save());
      const double newscore = search->problem->Score(*newstate);

      (void)EnqueueExpansionResult(
	  src, std::move(seq),
	  std::move(newstate),
	  newscore, true);
    } else {
      // This was a disaster! After 100 tries, we failed to make
      // ANY moves without losing control. Remove the marathon
      // node if this happens.
      search->stats.failed_marathon.Increment();

      PERF_WRITE_MUTEX_LOCK(PE_L_MARATHON_FAILED, &search->tree_m);
      // Release refcount in any case.
      src->num_workers_using--;

      Tree *tree = search->tree;
      
      // Make sure nobody changed the marathon node while we were
      // working. If they did, we can just do nothing.
      if (src == tree->marathon.node) {
	tree->marathon.node->used_in_marathon--;

	// Reset it in case we fail below.
	tree->marathon.node = nullptr;
	tree->marathon.score = 0.0;

	if (src->parent == nullptr)
	  return;

	// Could just return in this case, but something is wrong
	// if the parent is not in the heap.
	CHECK(src->parent->location != -1);
	auto cell = tree->heap.GetCell(src->parent);

	const double pscore = -cell.priority;
	
	if (pscore >= mminscore) {
	  src->parent->used_in_marathon++;
	  tree->marathon.node = src->parent;
	  tree->marathon.score = pscore;
	}
      }
    }
  }
  
  // Returns true if we should continue processing the queue.
  bool ProcessExploreQueue() {
    Tree::ExploreNode *en = nullptr;
    Tree::Seq start_seq;
    Problem::State start_state;
    double start_dist = -1.0;

    // Save the marathon score/seqlength target up front. If we have
    // at least this score, then we'll do the expensive IsInControl
    // test to consider using it to replace the marathon node later.
    double marathon_minscore = 0.0;
    int64 marathon_seqlength = 0;
    {
      PERF_WRITE_MUTEX_LOCK(PE_L_PROCESS_EXPLORE_QUEUE_A, &search->tree_m);
      Tree *tree = search->tree;
      
      en = GetExploreNodeWithMutex();
      if (en == nullptr) return false;      
      // I'm going to decrement it exactly this many times.
      CHECK(en->source->num_workers_using >= LOOPS_PER_EXPLORE_ITER);
      if (tree->marathon.node != nullptr) {
	const double bestscore = -tree->heap.GetMinimum().priority;
	marathon_minscore = MARATHON_BESTSCORE_FRAC * bestscore;
	marathon_seqlength = tree->marathon.node->seqlength;
      }
    }

    {
      PERF_WRITE_MUTEX_LOCK(PE_L_PROCESS_EXPLORE_QUEUE_ES, &en->node_m);
      // I should have a lock.
      CHECK(en->iterations_in_progress > 0);
      start_seq = en->closest_seq;
      start_state = en->closest_state;
      start_dist = en->distance;
    }

    int num_bad = 0;
    // Number of times we had a bad result and so didn't enqueue
    // any node; used to batch-decrement the refcount.
    int bad_iters = 0;
    // To avoid wasting time waiting for locks, each "iteration"
    // is several loops of the same thing.
    worker->SetDenom(LOOPS_PER_EXPLORE_ITER);
    for (int loop = 0; loop < LOOPS_PER_EXPLORE_ITER; loop++) {
      worker->SetNumer(loop);
      // Load source state.
      // worker->SetStatus("Exploring");
      worker->Restore(start_state);

      // worker->SetStatus("Explore inputs");

      // Generate a random sequence.
      // XXX tune these
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
      // worker->SetStatus("Explore exec");
      for (int i = 0; i < seq.size(); i++) {
	const Problem::Input input = seq[i];
	{
	  PERF_SCOPED(PE_EXEC);
	  worker->Exec(input);
	}
	Problem::State after = worker->Save();
	const double penalty =
	  search->problem->EdgePenalty(start_state, after);
	// If we die, don't use this sequence at all.
	// XXX checking exactly 1.0 here is bad. But how?
	if (penalty < 1.0) {
	  bad = true;
	  break;
	}

	// PERF could avoid saving every iteration if we computed this
	// from the worker state
	const double dist =
	  search->problem->GoalDistance(en->goal, after);

	if (dist < best_distance) {
	  best_prefix = i;
	  closest_state = after;
	  best_distance = dist;
	}
      }

      // worker->SetStatus("Explore post");
      if (!bad && best_prefix >= 0) {
	// At some point, we got closer to the goal. Put this in
	// the ExploreNode if it is still an improvement (might have
	// lost a race). We use fine-grained locking for this, since
	// in good cases, different threads are processing different
	// explore nodes. (So DO NOT take the tree mutex after the
	// node mutex is taken below!)
	PERF_WRITE_MUTEX_LOCK(PE_L_PROCESS_EXPLORE_QUEUE_EB, &en->node_m);
	
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

      if (bad) {
	// No update on this iteration, but we need to reduce the
	// source reference count.
	bad_iters++;
	
      } else {
	// Otherwise, enqueue it to be added to the tree. This takes
	// care of decrementing the refcount.
	
	// worker->SetStatus("Explore extend");
	Tree::Seq full_seq = start_seq;
	for (const Problem::Input input : seq) {
	  full_seq.push_back(input);
	}

	// PERF could avoid saving again here (using the last save from
	// the loop above), but better would be to fix the fact that we
	// keep saving in that loop...
	std::unique_ptr<Problem::State> newstate =
	  std::make_unique<Problem::State>(worker->Save());
	const double newscore = search->problem->Score(*newstate);

	// Only perform this expensive test if we have a good chance
	// of replacing the marathon node.
	const int64 seqlength = en->source->seqlength + full_seq.size();
	bool checked_in_control = false;
	{
	  PERF_SCOPED(PE_IN_CONTROL);
	  checked_in_control = newscore >= marathon_minscore &&
	    seqlength > marathon_seqlength &&
	    worker->IsInControl();
	}
	
	// OK to read source field without node lock since we have
	// ref count.
	Node *child =
	  EnqueueExpansionResult(en->source, std::move(full_seq),
				 std::move(newstate),
				 newscore, checked_in_control);
	
	// XXX: This debugging stuff is TPP-specific.
	child->goalx = en->goal.goalx;
	child->goaly = en->goal.goaly;
      }
    }

    // We batched these up instead of decrementing the refcount in
    // the loop.
    if (bad_iters > 0) {
      // do it outside the loop...
      PERF_WRITE_MUTEX_LOCK(PE_L_PROCESS_EXPLORE_QUEUE_R, &search->tree_m);
      CHECK(en->source->num_workers_using >= bad_iters);
      en->source->num_workers_using -= bad_iters;
    }
    
    // Finally, reduce the iteration count, and maybe clean up the
    // ExploreNode.
    bool remove_node = false;
    {
      PERF_WRITE_MUTEX_LOCK(PE_L_PROCESS_EXPLORE_QUEUE_EZ, &en->node_m);
      CHECK(en->iterations_in_progress > 0);
      en->iterations_in_progress--;
      en->bad += num_bad;
      if (en->iterations_left == 0 &&
	  en->iterations_in_progress == 0) {
	// Exactly one worker will encounter this situation.
	remove_node = true;
      }
    }

    // To remove it, we'll get exclusive access to the whole explore
    // queue. Some other threads may have considered the node, but
    // they cannot take it when it has zero count.
    if (remove_node) {
      // worker->SetStatus("Cleanup ExploreNode");
      {
	PERF_WRITE_MUTEX_LOCK(PE_L_PROCESS_EXPLORE_QUEUE_D, &search->tree_m);
	{
	  PERF_WRITE_MUTEX_LOCK(PE_L_PROCESS_EXPLORE_QUEUE_ED, &en->node_m);
	  CHECK(en->iterations_left == 0);
	  CHECK(en->iterations_in_progress == 0);
	
	  // PERF would be nice to avoid linear search, e.g. by retaining
	  // the iterator.
	  std::list<Tree::ExploreNode *> *explore_queue =
	    &search->tree->explore_queue;
	  for (std::list<Tree::ExploreNode *>::iterator it =
		 explore_queue->begin();
	       it != explore_queue->end(); ++it) {
	    if (en == *it) {
	      explore_queue->erase(it);
	      goto done;
	    }
	  }
	  CHECK(false) << "Didn't find ExploreNode in queue when "
	    "trying to delete it!";
	done:;
	}
      }
      
      // It's detached and we have exclusive access.
      delete en;
      en = nullptr;
    }

    // Reflect this work in stats.
    search->stats.explore_iters.IncrementBy(LOOPS_PER_EXPLORE_ITER);
    search->stats.explore_deaths.IncrementBy(num_bad);
    
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
      // worker->SetStatus("Load root");
      last = GetRoot();
      CHECK(last != nullptr);
      worker->Restore(last->state);
    }

    for (;;) {
      // In this top-level loop, we get a batch of work (depending
      // on the current mode and globally-committed state) and
      // execute it, then commit it back to the global state.
      
      // If the exploration queue isn't empty, we work on it instead
      // of continuing our work on other nodes.
      worker->SetStatus(STATUS_EXPLORE);
      while (ProcessExploreQueue()) {
	// OK to ignore the rest of this loop, but we should die if
	// requested.
	{
	  PERF_READ_MUTEX_LOCK(PE_L_SHOULD_DIE_EQ, &search->should_die_m);
	  if (search->should_die) {
	    worker->SetStatus(STATUS_DIE);
	    return;
	  }
	}
      }
      // Explore queue enqueues commits.
      CommitQueue();

      // Always do marathon if applicable.
      RunMarathon();
      CommitQueue();
      
      
      worker->SetStatus(STATUS_UNKNOWN);
      vector<Node *> nodes_to_expand = GetNodesToExtend();

      // Now we process this batch of nodes without touching
      // global state.
      worker->SetStatus(STATUS_SEARCH);
      worker->SetDenom(nodes_to_expand.size());
      for (int node_idx = 0; node_idx < nodes_to_expand.size(); node_idx++) {
	Node *expand_me = nodes_to_expand[node_idx];
	worker->SetNumer(node_idx);
	
	// Entering the loop, expand_me must point to a node to
	// expand, with a reference count that's consumed by
	// the loop below. When we extend the node, we move
	// to the child node some of the time.
	for (;;) {
	  // worker->SetStatus("Load");
	  CHECK(expand_me != nullptr);
	  worker->Restore(expand_me->state);

	  // worker->SetStatus("Gen inputs");
	  // constexpr double MEAN = 300.0;
	  // constexpr double STDDEV = 150.0;

	  // All the expansions will have the same length; this makes it
	  // more sensible to compare the objectives in order to choose
	  // the best.
	  int num_frames = gauss.Next() * opt.frames_stddev + opt.frames_mean;
	  if (num_frames < 1) num_frames = 1;

	  // Allow for fractional num_nexts (flip a coin to move between
	  // the two adjacent integers).
	  int num_nexts = (int)opt.num_nexts;
	  {
	    const double leftover = opt.num_nexts - (double)num_nexts;
	    if (RandDouble(&rc) < leftover) num_nexts++;
	  }

	  vector<vector<Problem::Input>> nexts;
	  nexts.reserve(num_nexts);
	  for (int num_left = num_nexts; num_left--;) {
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

	  // PERF: Could drop duplicates and drop sequences that
	  // are already in the node. Collisions do happen because
	  // of sparse nmarkov!

	  double best_score = -1.0;
	  std::unique_ptr<Problem::State> best;
	  int best_step_idx = -1;
	  for (int i = 0; i < nexts.size(); i++) {
	    search->stats.sequences_tried.Increment();
	    // worker->SetStatus("Re-restore");
	    // If this is the first one, no need to restore
	    // because we're already in that state.
	    if (i != 0) {
	      worker->Restore(expand_me->state);
	      // Since a sequence can only "improve" if it's
	      // not the first one, we store this denominator
	      // separately from sequences_tried.
	      search->stats.sequences_improved_denom.Increment();
	    }

	    // worker->SetStatus("Execute");
	    const vector<Problem::Input> &step = nexts[i];
	    {
	      PERF_SCOPED(PE_EXEC);
	      for (const Problem::Input &input : step) {
		worker->Exec(input);
	      }
	    }

	    {
	      PERF_SCOPED(PE_OBSERVE);
	      // PERF: This hits a global mutex in TwoPlayerProblem.
	      // Should probably batch these too?
	      // worker->SetStatus("Observe");
	      worker->Observe();
	    }

	    Problem::State state = worker->Save();
	    const double score = search->problem->Score(state);
	    if (best.get() == nullptr || score > best_score) {
	      if (best.get() != nullptr) {
		search->stats.sequences_improved.Increment();
	      }
	      best.reset(new Problem::State(std::move(state)));
	      best_step_idx = i;
	      best_score = score;
	    }
	  }

	  // worker->SetStatus("Extend tree");

	  // TODO: Consider inserting nodes other than the best one?
	  Node *child = EnqueueExpansionResult(
	      expand_me, nexts[best_step_idx], std::move(best),
	      best_score, false /* XXX checked_in_control */);
	  best.reset();

	  // worker->SetStatus("Check for death");
	  {
	    PERF_READ_MUTEX_LOCK(PE_L_SHOULD_DIE_N, &search->should_die_m);
	    if (search->should_die) {
	      worker->SetStatus(STATUS_DIE);
	      return;
	    }
	  }

	  if (RandFloat(&rc) < opt.p_stay_on_node) {
	    // No need to lock, since this is only reachable from
	    // the current thread (it's in local_queue).
	    expand_me = child;
	    expand_me->num_workers_using++;
	    expand_me->chosen++;
	  } else {
	    goto next_node;
	  }
	}

      next_node:;
      }


      worker->SetStatus(STATUS_UNKNOWN);
      
      // Commit queue to global state.
      {
	PERF_SCOPED(PE_COMMIT);
	CommitQueue();
      }
      MaybeUpdateTree();
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
  TreeSearch *search = nullptr;
  const int id = 0;
  // Distinct private stream.
  ArcFour rc;
  // Private gaussian stream, aliasing rc.
  RandomGaussian gauss;
  const TreeSearch::Options opt;
  Problem::Worker *worker = nullptr;
  std::thread th;
};

TreeSearch::TreeSearch(Options opt) : opt(opt) {
  CHECK_GT(opt.num_nexts, 0) << "Allowed range";
  
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

  problem.reset(new Problem(config));
}

vector<Worker *> TreeSearch::WorkersWithLock() const {
  vector<Worker *> ret;
  ret.reserve(workers.size());
  for (const WorkThread *wt : workers) {
    ret.push_back(wt->GetWorker());
  }
  return ret;
}

void TreeSearch::StartThreads() {
  // Maybe this should be locked separately?
  WriteMutexLock ml(&tree_m);
  CHECK(workers.empty());
  CHECK(num_workers > 0);
  workers.reserve(num_workers);
  for (int i = 0; i < num_workers; i++) {
    workers.push_back(new WorkThread(this, i));
  }
}

void TreeSearch::DestroyThreads() {
  {
    WriteMutexLock ml(&should_die_m);
    should_die = true;
  }
  // The destructor blocks on the thread join.
  for (WorkThread *wt : workers)
    delete wt;
  workers.clear();
}

void TreeSearch::SetApproximateSeconds(int64 sec) {
  approx_sec.store(sec, std::memory_order_relaxed);
}

int64 TreeSearch::UpdateApproximateNesFrames() {
  int64 total = 0LL;
  {
    ReadMutexLock ml(&tree_m);
    for (const WorkThread *w : workers) {
      total += w->GetWorker()->nes_frames.load(std::memory_order_relaxed);
    }
  }
  approx_total_nes_frames.store(total, std::memory_order_relaxed);
    
  return total;
}

string TreeSearch::SaveBestMovie(const string &filename) {
  Printf("Saving best.\n");

  vector<pair<int, string>> subtitles;
  vector<Problem::Input> all_inputs;
  {
    ReadMutexLock ml(&tree_m);
    auto best = tree->heap.GetByIndex(0);

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

      string subtitle =
	StringPrintf("f %lld s %lld g %d,%d",
		     n->nes_frames / 1024,
		     n->walltime_seconds,
		     n->goalx, n->goaly);

      path.emplace_front(subtitle, *seq);
      n = parent;
    }

    // Create subtitle vector.
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
  }
    
  return problem->SaveSolution(filename,
			       all_inputs,
			       subtitles,
			       "generated by pftwo");
}

void TreeSearch::PrintPerfCounters() {
  vector<int64> totals;
  int64 total_denom = 0LL;
  for (int i = 0; i < NUM_PERFEVENTS; i++)
    totals.push_back(0);

  uint64 freq;
  QueryPerformanceFrequency((LARGE_INTEGER*)&freq);
  
  {
    ReadMutexLock ml(&tree_m);
    for (WorkThread *w : workers) {
      total_denom += w->PerfGetTotal();
      for (int i = 0; i < NUM_PERFEVENTS; i++) {
	totals[i] += w->perf_counters[i];
      }
    }
  }
  
  printf("Perf counters:\n");
  for (int i = 0; i < NUM_PERFEVENTS; i++)
    printf("%16llu  %.6fs\t%.6f%%\t%s\n", totals[i],
	   (double)totals[i] / (double)freq,
	   (100.0 * totals[i]) / (double)total_denom,
	   PerfEventString((PerfEvent)i));
  printf("\n");
  fflush(stdout);
}

