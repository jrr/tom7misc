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
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/randutil.h"

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

// When expanding a node, try this many sequences and
// choose the best one.
#define NUM_NEXTS 4
static_assert(NUM_NEXTS > 0, "allowed range");

// We consider exploring from the grid if the score is
// at least GRID_BESTSCORE_FRAC * best_score_in_heap.
#define GRID_BESTSCORE_FRAC 0.90

static std::mutex print_mutex;
#define Printf(fmt, ...) do {			\
    MutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);			\
  } while (0)

struct WorkThread {
  using Node = Tree::Node;
  WorkThread(TreeSearch *search, int id) :
    search(search), id(id),
    rc(StringPrintf("worker_%d", id)),
    gauss(&rc),
    worker(search->problem->CreateWorker()),
    th{&WorkThread::Run, this} {}

  // Get the root node of the tree and associated state (which must be
  // present). Used during initialization.
  Node *GetRoot() {
    MutexLock ml(&search->tree_m);
    Node *n = search->tree->root;
    n->num_workers_using++;
    return n;
  }

  // Populates the vector with eligible grid indices.
  void EligibleGridNodesWithMutex(vector<int> *eligible) {
    // XXX This stuff is a hack. Improve it!
    double bestscore = -search->tree->heap.GetMinimum().priority;
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
      ret = search->tree->grid[idx].node;
    }
    
    if (ret == nullptr) {
      // The idea here is to choose a good node to start; in a heap,
      // good nodes are towards the beginning of the array.
      // Gaussian centered on 0; use 0 for anything out of bounds.
      // (n.b. this doesn't seem to help get unstuck -- evaluate it)
      const int g = (int)(gauss.Next() * (size * 0.05));
      const int idx = (g <= 0 || g >= size) ? 0 : g;

      Heap<double, Node>::Cell cell = search->tree->heap.GetByIndex(idx);
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
    MutexLock ml(&search->tree_m);

    CHECK(n->num_workers_using > 0);

    // Simple policy: 50% chance of switching to some good node using
    // the heap/grid; 50% chance of staying with the current node.
    if (rc.Byte() < 128) {
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

  Node *NewNode(Problem::State newstate, Node *parent) {
    CHECK(parent != nullptr);
    Node *child = new Node(std::move(newstate), parent);
    child->nes_frames = search->approx_nes_frames.load(
	std::memory_order_relaxed);
    child->walltime_seconds = search->approx_sec.load(
    	std::memory_order_relaxed);
    return child;
  }

  // Add a new child to the node (unless it's a duplicate).
  // Returns the new node or existing child. Doesn't change
  // reference counts. Must hold the tree lock.
  Node *ExtendNodeWithLock(Node *n,
			   const Tree::Seq &seq,
			   Problem::State newstate,
			   double newscore) {
    CHECK(n != nullptr);
    Node *child = NewNode(std::move(newstate), n);
    auto res = n->children.insert({seq, child});
    if (!res.second) {
      // By dumb luck (this might not be that rare if the markov
      // model is sparse), we already have a node with this
      // exact path. We don't allow replacing it.
      search->stats.same_expansion.Increment();

      delete child;
      return res.first->second;
    } else {
      search->tree->num_nodes++;
      search->tree->heap.Insert(-newscore, child);
      CHECK(child->location != -1);

      AddToGridWithLock(child, newscore);
      
      return child;
    }
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
    Node *child = NewNode(std::move(newstate), n);
    MutexLock ml(&search->tree_m);

    // XXX This should probably be done in the caller, because
    // if NUM_NEXTS isn't 1, we have more fine-grained evidence
    // that we could collect. (Right now it's like, "the probability
    // that randomly expanding the node NUM_NEXTS times and picking
    // the best one will actually make things worse") which is maybe
    // harder to think about, and certainly converges more slowly.
    const double oldscore = search->problem->Score(n->state);
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
      search->stats.same_expansion.Increment();

      delete child;
      // Maintain the invariant that the worker is at the
      // state of the returned node.
      Node *ch = res.first->second;

      CHECK(ch != nullptr);
      worker->Restore(ch->state);
      ch->num_workers_using++;
      return ch;

    } else {
      search->tree->num_nodes++;
      search->tree->heap.Insert(-newscore, child);
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
    MutexLock ml(&search->tree_m);
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
      MutexLock ml(&search->tree_m);
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

    search->problem->Commit();
    
    // Re-build heap. We do this by recalculating the score for
    // each node in place; most of the time this won't change the
    // shape of the heap much.
    {
      worker->SetStatus("Tree: Reheap");
      {
	MutexLock ml(&search->tree_m);

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
	
	printf("Reheap ... ");

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
      }
    }
    
    {
      MutexLock ml(&search->tree_m);
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

	// We popped off the best nodes, so now the heap only contains
	// nodes we'll probably delete.
	tree->heap.Clear();

	// Now make a pass over the tree and clean out nodes where we
	// can. This loop also computes the new maximum depth.
	int max_depth = 0;
	uint64 deleted_nodes = 0ULL;
	uint64 kept_score = 0ULL, kept_worker = 0ULL, kept_parent = 0ULL,
	  kept_grid = 0ULL;
	std::function<bool(Node *)> CleanRec =
	  // Returns true if we should keep this node; otherwise,
	  // the node is deleted.
	  [this, tree, &max_depth, &deleted_nodes,
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
	       "     child is kept: %llu\n",
	       kept_score, kept_worker, kept_grid, kept_parent);

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
	    static constexpr int NUM_EXPLORE_ITERATIONS = 500;
	    source->num_workers_using++;
	    Tree::ExploreNode en;
	    en.source = source;
	    en.goal = goal;
	    en.closest_state = source->state;
	    en.distance =
	      search->problem->GoalDistance(goal, source->state);
	    en.iterations_left = NUM_EXPLORE_ITERATIONS;
	    en.iterations_in_progress = 0;
	    tree->explore_queue.push_back(std::move(en));
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
      MutexLock ml(&search->tree_m);
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
    std::list<Tree::ExploreNode> *explore_queue = &search->tree->explore_queue;
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
      MutexLock ml(&search->tree_m);
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
      double penalty = search->problem->EdgePenalty(start_state, after);
      // If we die, don't use this sequence at all.
      // XXX checking exactly 1.0 here is bad. But how?
      if (penalty < 1.0) {
	bad = true;
	break;
      }

      // PERF could avoid saving every iteration if we computed this
      // from the worker state
      double dist = search->problem->GoalDistance(en->goal, after);

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
      MutexLock ml(&search->tree_m);
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
      MutexLock ml(&search->tree_m);
      Tree::Seq full_seq = start_seq;
      for (const Problem::Input input : seq) {
	full_seq.push_back(input);
      }

      // PERF could avoid saving again here (using the last save from
      // the loop above), but better would be to fix the fact that we
      // keep saving in that loop...
      Problem::State newstate = worker->Save();
      double score = search->problem->Score(newstate);
      Node *child =
	ExtendNodeWithLock(en->source, full_seq, std::move(newstate), score);
      // XXX: This is TPP specific.
      child->goalx = en->goal.goalx;
      child->goaly = en->goal.goaly;
    }

    // Finally, reduce the iteration count, and maybe clean up the
    // ExploreNode.
    {
      MutexLock ml(&search->tree_m);
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
	  &search->tree->explore_queue;
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
    search->stats.explore_iters.Increment();
    if (bad) {
      search->stats.explore_deaths.Increment();
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
	if (ReadWithLock(&search->should_die_m, &search->should_die)) {
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
	search->stats.sequences_tried.Increment();
	worker->SetStatus("Re-restore");
	// If this is the first one, no need to restore
	// because we're already in that state.
	if (i != 0) {
	  worker->Restore(expand_me->state);
	  // Since a sequence can only "improve" if it's
	  // not the first one, we store this denominator
	  // separately from sequences_tried.
	  search->stats.sequences_improved_denom.Increment();
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

      worker->SetStatus("Extend tree");
      // TODO: Consider inserting nodes other than the best one?
      // PERF move best?
      last = ExtendNode(expand_me, nexts[best_step_idx], *best, best_score);

      MaybeUpdateTree();

      worker->SetStatus("Check for death");
      if (ReadWithLock(&search->should_die_m, &search->should_die)) {
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
  TreeSearch *search = nullptr;
  const int id = 0;
  // Distinct private stream.
  ArcFour rc;
  // Private gaussian stream, aliasing rc.
  RandomGaussian gauss;
  Problem::Worker *worker = nullptr;
  std::thread th;
};

TreeSearch::TreeSearch() {
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
  MutexLock ml(&tree_m);
  CHECK(workers.empty());
  CHECK(num_workers > 0);
  workers.reserve(num_workers);
  for (int i = 0; i < num_workers; i++) {
    workers.push_back(new WorkThread(this, i));
  }
}

void TreeSearch::DestroyThreads() {
  {
    MutexLock ml(&should_die_m);
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

void TreeSearch::SetApproximateNesFrames(int64 nes_frames) {
  approx_nes_frames.store(nes_frames, std::memory_order_relaxed);
}

string TreeSearch::SaveBestMovie(const string &filename) {
  Printf("Saving best.\n");
  MutexLock ml(&tree_m);
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

  return problem->SaveSolution(filename,
			       all_inputs,
			       subtitles,
			       "generated by pftwo");
}
