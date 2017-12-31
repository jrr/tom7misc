// "Problem" definition for two-player games. The input
// state is an input for each controller (uint16).

#ifndef __PROBLEM_TWOPLAYER_H
#define __PROBLEM_TWOPLAYER_H

#include "pftwo.h"

#include <mutex>
#include <atomic>
#include <utility>

#include "n-markov-controller.h"
#include "../fceulib/emulator.h"
#include "weighted-objectives.h"

struct TwoPlayerProblem {
  // Player 1 and Player 2 controllers.
  // HERE: For the best compatibility with the pftwo, if we want to
  // do something like store goals and hypotheses in the state, we
  // probably want actions in the input that set those values, since
  // pftwo believes that it can recreate a state by replaying inputs.
  // TODO!

  // In simple problem instances, this is just a controller input,
  // or a controller input for each of the two players. However, we
  // allow the possibility of meta inputs, which change the worker's
  // state.
  struct Input {
    enum class Type : uint8 {
      CONTROLLER = 0,
      SETGOAL = 1,
      CLEARGOAL = 2,
    };

    // Should be able to make this a bitfield, but it seems G++ has
    // a bug.
    Type type = Type::CONTROLLER;
    uint8 goalx = 0;
    uint8 goaly = 0;
    
    uint8 p1 = 0;
    uint8 p2 = 0;
  };

  using ControllerHistory = NMarkovController::History;

  // Assuming the input is type CONTROLLER.
  static inline uint8 Player1(Input i) { return i.p1; }
  static inline uint8 Player2(Input i) { return i.p2; }
  static inline Input ControllerInput(uint8 p1, uint8 p2) {
    Input input;
    input.type = Input::Type::CONTROLLER;
    input.p1 = p1;
    input.p2 = p2;
    return input;
  }

  struct GoalData {
    bool has_goal = false;
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
    // Current goal, if any.
    GoalData goal;
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
  
  // An individual instance of the emulator that can be used to
  // execute steps. We create one of these per thread.
  struct Worker {
    explicit Worker(TwoPlayerProblem *parent) : tpp(parent) {}
    // Every worker loads the same game.
    unique_ptr<Emulator> emu;
    int depth = 0;
    GoalData goal;

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
	            goal, previous1, previous2 };
    }

    void Restore(const State &state) {
      MutexLock ml(&mutex);
      emu->LoadUncompressed(state.save);

      depth = state.depth;
      goal = state.goal;
      // (Doesn't actually need the memory to restore; it's part of
      // the emulator save state.)
      previous1 = state.prev1;
      previous2 = state.prev2;
    }
    void Exec(Input input) {
      MutexLock ml(&mutex);
      switch (input.type) {
      case Input::Type::CONTROLLER: {
	const uint8 input1 = Player1(input), input2 = Player2(input);
	emu->Step(input1, input2);
	IncrementNESFrames(1);
	depth++;
	// Here it would be better if we could just call a static method,
	// like if NMarkovController were templatized on n.
	previous1 = tpp->markov1->Push(previous1, input1);
	previous2 = tpp->markov2->Push(previous2, input2);
	break;
      }
      case Input::Type::SETGOAL:
	goal.has_goal = true;
	goal.goalx = input.goalx;
	goal.goaly = input.goaly;
	break;
      case Input::Type::CLEARGOAL:
	goal.has_goal = false;
	goal.goalx = 0;
	goal.goaly = 0;
	break;
      }
    }

    // After executing some inputs, observe the current state.
    // We use this to keep track of high water marks for the
    // objectives, which are used to place objectives along
    // an absolute scale. See WeightedObjectives::Observations.
    void Observe();

    // Visualize the current state, by drawing pixels into the
    // ARGB array, which has size 256x256 pixels.
    void Visualize(vector<uint8> *argb256x256);
    // Append lines to the vector that describe the current state
    // for visualization purposes. The strings should fit in a
    // column 256 pixels wide.
    void VizText(vector<string> *text);

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
  // Score in [0, 1]. Should be stable in-between calls to Commit.
  double Score(const State &state) {
    // This is tricky, and maybe needs to be improved. When the state
    // has a goal, we want to give a bonus to states that are near that
    // goal (or else, what's the point?). But we don't want the state
    // to look artificially bad or good in a global sense, because then
    // we'll either keep expanding it (despite it not necessarily being
    // any better -- although this may be ok) or avoiding it (which means
    // that we ignore goals).
    //
    // TODO: Maybe should get a global score when choosing the overall
    // best nodes, but a local score (including goal) when picking
    // which of the nexts we want to save?

    // "Real" score, from objective functions (compared to global best).
    const double objective_score = observations->GetWeightedValue(state.mem);

    // Treat a reached or nearly-reached goal as being the same as having
    // no goal. Otherwise, it looks bad when we abandon a goal.
    const double goal_score = [this, &state]() {
      if (!state.goal.has_goal) return 1.0;
      
      // Otherwise, get the coordinates from memory.
      const int gx = state.goal.goalx;
      const int gy = state.goal.goaly;
      
      const int p1x = state.mem[x1_loc];
      const int p1y = state.mem[y1_loc];
      const int p2x = state.mem[x2_loc];
      const int p2y = state.mem[y2_loc];

      const int dx1 = p1x - gx;
      const int dy1 = p1y - gy;
      const int dx2 = p2x - gx;
      const int dy2 = p2y - gy;
      // We use squared distance penalty.
      int sqdist1 = dx1 * dx1 + dy1 * dy1;
      int sqdist2 = dx2 * dx2 + dy2 * dy2;
      // But also if we're within 16 pixels, treat this as zero.
      // XXX why?
      sqdist1 -= 16 * 16;
      sqdist2 -= 16 * 16;

      static constexpr double INV_MAX_SQDIST = 1.0 / (256.0 * 256.0 + 256.0 * 256.0);
      double f1 = sqdist1 * INV_MAX_SQDIST;
      double f2 = sqdist2 * INV_MAX_SQDIST;
      // Clamp to [0, 1]. These can be negative due to -= 16 * 16 above.
      if (f1 < 0.0) f1 = 0.0;
      if (f2 < 0.0) f2 = 0.0;
      if (f1 > 1.0) f1 = 1.0;
      if (f2 > 1.0) f2 = 1.0;

      return (f1 + f2) * 0.5;
    }();

    static constexpr double OBJF = 0.999;
    
    return objective_score * OBJF + goal_score * (1.0 - OBJF);
    
    // Old idea; disabled:
    // Give a tiny penalty to longer paths to the same value, so
    // that we prefer to optimize these away.
    // This is only 4.6 hours of game time, but this is a dirty hack
    // anyway.
    // I made it one billion, because a millionth was actually causing
    // problems with lack of progress (I think).

    /*
    double depth_penalty = 1.0 - (state.depth * (1.0 / 1000000000.0));
    if (depth_penalty > 1.0) depth_penalty = 1.0;
    else if (depth_penalty < 0.0) depth_penalty = 0.0;
    return observations->GetWeightedValue(state.mem) *
      depth_penalty;
    */
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
  // itself, so this is like XXX temporary.
  int x1_loc = -1, y1_loc = -1, x2_loc = -1, y2_loc = -1;
  
  vector<pair<uint8, uint8>> original_inputs;
  unique_ptr<NMarkovController> markov1, markov2;
  // After warmup inputs.
  State start_state;
  // For play after warmup.
  unique_ptr<WeightedObjectives> objectives;
  unique_ptr<Observations> observations;
};

inline bool operator <(const TwoPlayerProblem::Input &a,
		       const TwoPlayerProblem::Input &b) {
  return std::tie(a.type, a.goalx, a.goaly, a.p1, a.p2) <
    std::tie(b.type, b.goalx, b.goaly, b.p1, b.p2);
}

#endif
