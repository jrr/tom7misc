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

  // 
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
    ControllerHistory prev1, prev2;
    Input RandomInput(ArcFour *rc);
  };
  
  void SaveSolution(const string &filename_part,
		    const vector<Input> &inputs,
		    const State &state,
		    const string &info);

  Goal RandomGoal(ArcFour *rc) const {
    Goal goal;
    goal.goalx = rc->Byte();
    goal.goaly = rc->Byte();
    return goal;
  }
  
  // An individual instance of the emulator that can be used to
  // execute steps. We create one of these per thread.
  struct Worker {
    explicit Worker(TwoPlayerProblem *parent) : tpp(parent) {}
    // Every worker loads the same game.
    unique_ptr<Emulator> emu;
    int depth = 0;

    // Previous input for the two players.
    ControllerHistory previous1, previous2;
    
    // Sample a random input; may depend on the current state (for
    // example, in Markov models). Doesn't execute the input.
    Input RandomInput(ArcFour *rc);

    // Make a new input generator at the current state, which allows
    // generating multiple sequential random inputs without executing
    // them. Intended to be efficient to create.
    InputGenerator Generator(ArcFour *rc) {
      // TODO: Pass some notion of "stuckness", which should influence our
      // probability of generating a new goal. (A simple notion of
      // stuckness could be the number of nodes that have the maximum
      // score, or close to it? But keep in mind that if our best nodes
      // are in sticky near-death situations, we might have lots of
      // expansions of those that are dead, which means lower looking
      // scores. We are not stuck when nodes are repeatedly getting
      // better scores.)
      return InputGenerator{tpp, previous1, previous2};
    }
        
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
      SetStatus(nullptr);
      SetNumer(0);
      SetDenom(0);
    }
    inline void SetStatus(const char *s) {
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
    
    // Current operation. Should point to a string literal;
    // other code may hang on to references.
    std::atomic<const char *> status{nullptr};
    // Fraction complete.
    std::atomic<int> numer{0}, denom{0};
    // For benchmarking, the total number of NES frames (or
    // equivalent) executed by this worker. This isn't
    // all the program does, but it is the main bottleneck,
    // so we want to make sure we aren't stalling them.
    std::atomic<int> nes_frames{0};
    
    const TwoPlayerProblem *tpp = nullptr;

    // Coarse lock for all non-atomic members.
    std::mutex mutex;
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
  // XXX: Need to determine protect_loc automatically, like during training.
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

    int sqdist1 = dx1 * dx1 + dy1 * dy1;
    int sqdist2 = dx2 * dx2 + dy2 * dy2;

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
  
  // Must be thread safe and leave Worker in a valid state.
  Worker *CreateWorker();

  explicit TwoPlayerProblem(const map<string, string> &config);
 
  string game;
  int warmup_frames = -1;
  int fastforward = -1;
  
  // (Hypothesized) memory locations corresponding to the two
  // player's screen coordinates. If -1, unknown.
  // These are currently just read from the config file. In
  // a non-cheating version, the worker would deduce these
  // itself, so this is like XXX temporary. (Or maybe it would
  // be a product of training? Seems somewhat better to do it
  // online since then we can handle changes as the game
  // progresses...?)
  int x1_loc = -1, y1_loc = -1, x2_loc = -1, y2_loc = -1;
  // Locations where we want to protect the value from going
  // down. XXX determine these automatically too.
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
