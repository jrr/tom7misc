// "Problem" definition for two-player games. The input
// state is an input for each controller (uint16).

#ifndef __PROBLEM_TWOPLAYER_H
#define __PROBLEM_TWOPLAYER_H

#include "pftwo.h"

#include <cmath>
#include <mutex>
#include <atomic>
#include <utility>

#include "n-markov-controller.h"
#include "../fceulib/emulator.h"
#include "weighted-objectives.h"
#include "../cc-lib/randutil.h"
#include "autotimer.h"
#include "autolives.h"
#include "options.h"

struct EmulatorPool;

struct TwoPlayerProblem {
  // Player 1 and Player 2 controllers.

  // In simple problem instances, this is just a controller input,
  // or a controller input for each of the two players. However, it's
  // also possible to include meta inputs, which change the worker's
  // state (e.g., its idea about memory locations to protect?)
  // Good to keep this small, and inputs must have an overloaded <
  // operator (see below).
  struct Input {
    uint8 p1 = 0;
    uint8 p2 = 0;
  };

  using ControllerHistory = NMarkovController::History;

  static inline uint8 Player1(Input i) { return i.p1; }
  static inline uint8 Player2(Input i) { return i.p2; }
  static inline Input ControllerInput(uint8 p1, uint8 p2) {
    Input input;
    input.p1 = p1;
    input.p2 = p2;
    return input;
  }

  struct Goal {
    uint8 goalx = 0;
    uint8 goaly = 0;
  };
  
  // Save state for a worker; the worker can save and restore these
  // at will, and they are portable betwen workers.
  struct State {
    // Emulator savestate.
    vector<uint8> save;
    // PERF This is actually part of save. But we use it to
    // compute objective functions without having to restore
    // the save. If memory becomes a big problem, we could
    // have Emulator return a more structured save which
    // provided a pointer to the 2k memory. This would preclude
    // cleverness like save state compression, though.
    // PERF We could store a TwoPlayerProblem-level remapping of
    // indices used in the objective functions to a dense sequence of
    // integers, which would usually be much fewer than 2048, and only
    // store the memory values keyed by those denser indices here.
    // (i.e., only store the bytes actually used in the objective
    // functions.)
    // Note that we use the mem for other stuff, like player position
    // and death heuristics.
    vector<uint8> mem;
    // Number of NES frames 
    int depth;
    ControllerHistory prev1, prev2;
  };

  static int64 StateBytes(const State &s) {
    return s.save.size() + s.mem.size() + sizeof (State);
  }

  // Object that can generate (pseudo)random inputs.
  //
  // Generator itself is not thread-safe, but you can create
  // many of them easily.
  struct InputGenerator {
    const TwoPlayerProblem *tpp;
    // Maybe null.
    const Goal *goal;
    // Actual locations of players.
    int p1x, p1y, p2x, p2y;
    // If true, generate the same sequence for p1 and p2.
    bool sync;
    ControllerHistory prev1, prev2;
    Input RandomInput(ArcFour *rc);
  };
  
  string SaveSolution(const string &filename_part,
		      const vector<Input> &inputs,
		      const vector<pair<int, string>> &subtitles,
		      const string &info);

  Goal RandomGoal(ArcFour *rc) const {
    Goal goal;
    goal.goalx = rc->Byte();
    goal.goaly = rc->Byte();
    return goal;
  }

  Goal RandomGoalInCell(ArcFour *rc, int cell) const {
    // Top-left corner of cell.
    const int y = cell / GRID_CELLS_W;
    const int x = cell % GRID_CELLS_W;

    Goal goal;
    goal.goalx = x + RandTo(rc, DIVI_X);
    goal.goaly = y + RandTo(rc, DIVI_Y);
    return goal;
  }
  
  // An individual instance of the emulator that can be used to
  // execute steps. We create one of these per thread.
  struct Worker {
    explicit Worker(TwoPlayerProblem *parent) :
      tpp(parent),
      autolives1(parent->game, *parent->markov1),
      autolives2(parent->game, *parent->markov2) {}
    // Every worker loads the same game.
    unique_ptr<Emulator> emu;
    int depth = 0;

    // Previous input for the two players.
    ControllerHistory previous1, previous2;
    
    // Return any input, which is used for hacks where we need
    // to adavnce the state so that we can fetch an image from
    // the emulator :(
    Input AnyInput() const;

    // Make a new input generator at the current state, which allows
    // generating multiple sequential random inputs without executing
    // them. Intended to be efficient to create.
    // Goal may be nullptr if there is no goal.
    InputGenerator Generator(ArcFour *rc, const Goal *goal);
        
    State Save() {
      MutexLock ml(&mutex);
      return State{ emu->SaveUncompressed(), emu->GetMemory(), depth,
	            previous1, previous2 };
    }

    void Restore(const State &state) {
      MutexLock ml(&mutex);
      emu->LoadUncompressed(state.save);

      depth = state.depth;
      // (Doesn't actually need the memory to restore; it's part of
      // the emulator save state.)
      previous1 = state.prev1;
      previous2 = state.prev2;
    }
    void Exec(Input input) {
      MutexLock ml(&mutex);
      const uint8 input1 = Player1(input), input2 = Player2(input);
      emu->Step(input1, input2);
      IncrementNESFrames(1);
      depth++;
      // Here it would be better if we could just call a static method,
      // like if NMarkovController were templatized on n.
      previous1 = tpp->markov1->Push(previous1, input1);
      previous2 = tpp->markov2->Push(previous2, input2);
    }

    // Return true if we are known to have control of the players
    // at the current moment. Always fails if the player locations
    // are not known...
    bool IsInControl() {
      MutexLock ml(&mutex);

      const int x1_loc = tpp->x1_loc, y1_loc = tpp->y1_loc;
      const int x2_loc = tpp->x2_loc, y2_loc = tpp->y2_loc;
      
      // XXX, we could at least test if inputs have some effect
      // on memory, etc.
      if (x1_loc < 0 || y1_loc < 0 ||
	  x2_loc < 0 || y2_loc < 0) return false;
      
      vector<uint8> save = emu->SaveUncompressed();
      // XXX this should count towards IncrementNESFrames
      // XXX how to set this threshold?
      return autolives1.IsInControl(save, x1_loc, y1_loc, false) >= 0.8 &&
	     autolives2.IsInControl(save, x2_loc, y2_loc, true) >= 0.8;
    }
    
    // After executing some inputs, observe the current state.
    // We use this to keep track of high water marks for the
    // objectives, which are used to place objectives along
    // an absolute scale. See WeightedObjectives::Observations.
    void Observe();

    // Visualize the current state, by drawing pixels into the
    // ARGB array, which has size 256x256 pixels. The goal
    // pointer will be null if there is no current goal.
    void Visualize(const Goal *goal, vector<uint8> *argb256x256);
    // Append lines to the vector that describe the current state
    // for visualization purposes. The strings should fit in a
    // column 256 pixels wide.
    void VizText(const Goal *goal, vector<string> *text);

    // Status and counters just used for the UI.
    void ClearStatus() {
      SetStatus(STATUS_UNKNOWN);
      SetNumer(0);
      SetDenom(0);
    }
    inline void SetStatus(WorkerStatus s) {
      status.store(s, std::memory_order_relaxed);
    }
    inline void SetNumer(int n) {
      numer.store(n, std::memory_order_relaxed);
    }
    inline void SetDenom(int d) {
      denom.store(d, std::memory_order_relaxed);
    }
    inline void IncrementNESFrames(int d) {
      nes_frames.fetch_add(d, std::memory_order_relaxed);
    }
    
    // Current operation. See pftwo.h.
    std::atomic<WorkerStatus> status{STATUS_UNKNOWN};
    // Fraction complete.
    std::atomic<int> numer{0}, denom{0};
    // For benchmarking, the total number of NES frames (or
    // equivalent) executed by this worker. This isn't
    // all the program does, but it is the main bottleneck,
    // so we want to make sure we aren't stalling them.
    std::atomic<int64> nes_frames{0};
    
    const TwoPlayerProblem *tpp = nullptr;

    // Coarse lock for all non-atomic members.
    std::mutex mutex;

    AutoLives autolives1, autolives2;
  };

  // Commits observations.
  void Commit() {
    CHECK(observations.get());
    observations->Commit();
  }

  // Penalty for traveling between a pair of states. [0, 1] where 0 is
  // bad (high penalty) and 1 is neutral. This is intended to measure
  // something like lives lost when going from old_state to new, so that
  // we avoid exploration play that actually kills the player.
  // Could also just be based on the objectives.
  //
  // Note that currently, exactly 1.0 is distinguished from any value
  // < 1.0 during the "explore" process (In an attempt to avoid
  // killing the player while exploring). So it is currently best to
  // treat very small penalties as 1.0.
  double EdgePenalty(const State &old_state, const State &new_state) const {
    double res = 1.0;
    for (int loc : protect_loc)
      if (new_state.mem[loc] < old_state.mem[loc])
	res *= 0.5;
    return res;
  }

  // Get the distance to the goal. (Should be a proper distance
  // metric, e.g., >= 0). Should be cheap to compute (e.g. after every
  // NES frame.)
  double GoalDistance(const Goal &goal, const State &state) const {
    CHECK(x1_loc >= 0);
    CHECK(y1_loc >= 0);
    CHECK(x2_loc >= 0);
    CHECK(y2_loc >= 0);

    // Note widening to int.
    const int gx = goal.goalx;
    const int gy = goal.goaly;

    const int p1x = state.mem[x1_loc];
    const int p1y = state.mem[y1_loc];
    const int p2x = state.mem[x2_loc];
    const int p2y = state.mem[y2_loc];

    const int dx1 = p1x - gx;
    const int dy1 = p1y - gy;
    const int dx2 = p2x - gx;
    const int dy2 = p2y - gy;

    const int sqdist1 = dx1 * dx1 + dy1 * dy1;
    const int sqdist2 = dx2 * dx2 + dy2 * dy2;

    return sqrt(sqdist1) + sqrt(sqdist2);
  }
  
  // State's score in [0, 1]. Must be stable in-between calls to Commit.
  // This should reflect the progress toward "winning the game," independent
  // of any short-term goal.
  // (0-1 range is currently nominal. When we reach new global maxima,
  // the values will be >1 until the next Commit. Could fix with a
  // sigmoid or something, although be careful that we use "lots of scores
  // are near 1" to indicate stuckness.)
  double Score(const State &state) const {
    // "Real" score, from objective functions (compared to global best).
    const double objective_score = observations->GetWeightedValue(state.mem);

    // XXX - Useful to include protect_loc here, but measured against the
    // start state? Maybe only the caller should be doing this when expanding
    // nodes? (And if this is really lives, we probably shouldn't treat it
    // as "progress towards winning the game", at least philosophically.)
    const double penalty = EdgePenalty(start_state, state);
    return penalty * objective_score;
  }

  // Number of times to subdivide the screen in both x and y
  // coordinates. 1 would yield four quadrants. Exponential!
  static constexpr int GRID_DIVISIONS = 3;
  static_assert(GRID_DIVISIONS > 0 && GRID_DIVISIONS < 8,
		"valid range");
  
  static constexpr int GRID_CELLS_W = 1 << GRID_DIVISIONS;
  static constexpr int DIVI_X = 256 >> GRID_DIVISIONS;
  static constexpr int DIVI_Y = 256 >> GRID_DIVISIONS;
  static constexpr int GRID_CELLS_H = 1 << GRID_DIVISIONS;

  // Part of the problem interface.
  static constexpr int num_grid_cells = GRID_CELLS_W * GRID_CELLS_H;
  // A state can be in 0 or 1 grid cells.
  bool GetGridCell(const State &state, int *cell) const {
    if (x1_loc == -1 || y1_loc == -1 ||
	x2_loc == -1 || y2_loc == -1) return false;

    const int p1x = state.mem[x1_loc];
    const int p1y = state.mem[y1_loc];
    const int p2x = state.mem[x2_loc];
    const int p2y = state.mem[y2_loc];

    const int c1x = p1x / DIVI_X;
    const int c1y = p1y / DIVI_Y;
    const int c2x = p2x / DIVI_X;
    const int c2y = p2y / DIVI_Y;

    // Require the players to be in the same cell, or else
    // we get another quadratic blowup.
    if (c1x != c2x || c1y != c2y) return false;
    
    *cell = c1y * GRID_CELLS_W + c1x;
    CHECK(*cell < num_grid_cells) << c1x << " " << c1y;
	
    return true;
  }

  // Get all of the cells that are "adjacent" to this one.
  static vector<int> AdjacentCells(int cell);
  
  // Must be thread safe and leave Worker in a valid state.
  Worker *CreateWorker();

  explicit TwoPlayerProblem(const map<string, string> &config,
			    const Options &opt);

 private:
  void InitTimers(const map<string, string> &config,
		  EmulatorPool *pool,
		  const vector<uint8> &start);
  void InitCameras(const map<string, string> &config,
		   EmulatorPool *pool,
		   const vector<uint8> &start);
  void InitLives(const map<string, string> &config,
		 EmulatorPool *pool,
		 const vector<uint8> &start);

  const Options opt;
  
  string game;
  int warmup_frames = -1;
  int fastforward = -1;
  int init_threads = 6;
  
  vector<AutoTimer::TimerLoc> timers;
  // (Hypothesized) memory locations corresponding to the two
  // player's screen coordinates. If -1, unknown. These are
  // determined using autocamera2.
  int x1_loc = -1, y1_loc = -1, x2_loc = -1, y2_loc = -1;
  // Locations where we want to protect the value from going
  // down. Determined using autolives.
  vector<int> protect_loc;
  
  vector<pair<uint8, uint8>> original_inputs;
  unique_ptr<NMarkovController> markov1, markov2;
  // After warmup inputs.
  State start_state;
  // For play after warmup.
  unique_ptr<WeightedObjectives> objectives;
  unique_ptr<Observations> observations;
};

// Input needs a < operator so that pftwo can use vectors of inputs
// as keys in a map.
inline bool operator <(const TwoPlayerProblem::Input &a,
		       const TwoPlayerProblem::Input &b) {
  return std::tie(a.p1, a.p2) < std::tie(b.p1, b.p2);
}

#endif
