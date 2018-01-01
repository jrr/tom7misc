#include "problem-twoplayer.h"

#include <stdlib.h>
#include <map>

#include "../fceulib/simplefm2.h"
#include "n-markov-controller.h"
#include "weighted-objectives.h"
#include "learnfun.h"
#include "../cc-lib/util.h"
#include "../cc-lib/lines.h"
#include "graphics.h"

// Note NMarkovController::Stats; making this large will often
// make the matrix very sparse.
static constexpr int MARKOV_N = 3;

static_assert(MARKOV_N >= 0 && MARKOV_N <= 8, "allowed range");

// ??!?
static constexpr int OBSERVATION_SAMPLES = 32768;
// static constexpr int OBSERVATION_SAMPLES = 1024;

using TPP = TwoPlayerProblem;
using Worker = TPP::Worker;

TPP::Input Worker::RandomInput(ArcFour *rc) {
  // These are all const. No lock needed.
  // MutexLock ml(&mutex);
  const uint8 p1 = tpp->markov1->RandomNext(previous1, rc);
  const uint8 p2 = tpp->markov2->RandomNext(previous2, rc);
  return ControllerInput(p1, p2);
}

TPP::Input TPP::InputGenerator::RandomInput(ArcFour *rc) {
  /*
  if (rc->Byte() == 0) {
    if (tpp->x1_loc >= 0 &&
	tpp->y1_loc >= 0 &&
	tpp->x2_loc >= 0 &&
	tpp->y2_loc >= 0) {
      Input input;
      if (rc->Byte() & 1) {
	input.type = Input::Type::CLEARGOAL;
	input.goalx = 0;
	input.goaly = 0;
      } else {
	input.type = Input::Type::SETGOAL;
	input.goalx = rc->Byte();
	input.goaly = rc->Byte();
      }
      return input;
    }
  }
  */
  
  const uint8 p1 = tpp->markov1->RandomNext(prev1, rc);
  const uint8 p2 = tpp->markov2->RandomNext(prev2, rc);
  prev1 = tpp->markov1->Push(prev1, p1);
  prev2 = tpp->markov2->Push(prev2, p2);
  return ControllerInput(p1, p2);
}

void TPP::SaveSolution(const string &filename_part,
		       const vector<Input> &inputs,
		       const State &state,
		       const string &info) {
  // Just the controller inputs are saved.
  vector<pair<uint8, uint8>> all_inputs;
  map<int, string> subtitles;
  subtitles[0] = "warmup";
  for (int i = 0; i < warmup_frames; i++) {
    all_inputs.push_back(original_inputs[i]);
  }
  if (fastforward >= 0) {
    subtitles[all_inputs.size()] = "fastforward";
    for (int i = warmup_frames; i < fastforward; i++) {
      all_inputs.push_back(original_inputs[i]);
    }
  }
  subtitles[all_inputs.size()] = "";
  
  for (const Input &input : inputs) {
    switch (input.type) {
    case Input::Type::CONTROLLER:
      all_inputs.push_back({Player1(input), Player2(input)});
      break;
    case Input::Type::SETGOAL:
      subtitles[all_inputs.size()] = StringPrintf("GOAL %u,%u",
						  input.goalx,
						  input.goaly);
      break;
    case Input::Type::CLEARGOAL:
      subtitles[all_inputs.size()] = "";
      break;
    }
  }

  vector<pair<int, string>> subtitle_vec;
  for (const pair<const int, string> &sub : subtitles)
    subtitle_vec.emplace_back(sub.first, sub.second);
  SimpleFM2::WriteInputsWithSubtitles2P(
      StringPrintf("%s-%s.fm2", game.c_str(), filename_part.c_str()),
      game,
      // Take from inputs?
      "base64:WlwvTxyvsfVajcDVrUVQ5Q==",
      all_inputs,
      subtitle_vec);
}

// Like atoi, but allowing nonnegative hex values with 0x prefix
// WITHOUT allowing error-prone octal prefix of just 0.
static int AtoiHex(const string &s) {
  const char *c = s.c_str();
  while (isspace(*c)) c++;
  if (c[0] == '0' && (c[1] | 32) == 'x')
    return strtol(c, nullptr, 16);
  else
    return atoi(c);
}

TPP::TwoPlayerProblem(const map<string, string> &config) {
  game = GetDefault(config, "game", "");
  printf("Create TPP for %s...\n", game.c_str());
  const string movie = GetDefault(config, "movie", "");
  printf("Read inputs for %s\n", movie.c_str());
  original_inputs = SimpleFM2::ReadInputs2P(movie);

  string protect = GetDefault(config, "protect", "");
  while (!protect.empty()) {
    string tok = Util::chop(protect);
    if (!tok.empty()) {
      protect_loc.push_back(AtoiHex(tok));
      printf("[CHEATIN'] Protect %d\n", protect_loc.back());
    }
  }
  
  
  x1_loc = AtoiHex(GetDefault(config, "x1", "-1"));
  y1_loc = AtoiHex(GetDefault(config, "y1", "-1"));
  x2_loc = AtoiHex(GetDefault(config, "x2", "-1"));
  y2_loc = AtoiHex(GetDefault(config, "y2", "-1"));
  printf("[CHEATIN'] Players at %d,%d and %d,%d\n",
	   x1_loc, y1_loc, x2_loc, y2_loc);

  warmup_frames = AtoiHex(GetDefault(config, "warmup", "-1"));
  // XXX: Deduce this from input.
  CHECK(warmup_frames >= 0) << "You must currently specify the "
    "number of warmup frames as a config line 'warmup'. This is "
    "the amount copied from the training data before automatic "
    "play begins. 0 may work.";

  // If this is present, we start playing from this point
  // in the training sequence.
  fastforward = AtoiHex(GetDefault(config, "fastforward", "-1"));
  if (fastforward < 0) fastforward = warmup_frames;
  
  CHECK(original_inputs.size() > warmup_frames);
  CHECK(original_inputs.size() > fastforward);
  CHECK(fastforward >= warmup_frames);
  
  printf("Get inputs.\n");
  // Now build inputs for training markov.
  vector<uint8> player1, player2;
  for (int i = warmup_frames; i < original_inputs.size(); i++) {
    const auto &p = original_inputs[i];
    player1.push_back(p.first);
    player2.push_back(p.second);
  }
  printf("Build markov controllers.\n");
  markov1.reset(new NMarkovController(player1, MARKOV_N));
  markov2.reset(new NMarkovController(player2, MARKOV_N));

  markov1->Stats();
  markov2->Stats();

  // Throwaway emulator instance for warmup, training.
  printf("Warmup.\n");
  unique_ptr<Emulator> emu(Emulator::Create(game));
  for (int i = 0; i < warmup_frames; i++) {
    emu->Step(original_inputs[i].first, original_inputs[i].second);
  }
  
  // Save start state for each worker.
  vector<uint8> save_after_warmup = emu->SaveUncompressed();

  // See if we have cached learnfun results, since it takes some
  // time to run.

  // XXX base this on a fingerprint of ROM, FM2, and any other
  // configuration.
  string cached_objectives = StringPrintf("%s.objectives", game.c_str());
  if (Util::ExistsFile(cached_objectives)) {
    printf("Loading weighted objectives from %s.\n",
	   cached_objectives.c_str());
    objectives.reset(WeightedObjectives::LoadFromFile(cached_objectives));
    printf("There are %d objectives.\n", (int)objectives->Size());
  } else {
    // Now execute inputs to get memories for learnfun.
    printf("Get memories.\n");
    vector<vector<uint8>> memories;
    memories.reserve(original_inputs.size() - warmup_frames);
    for (int i = warmup_frames; i < original_inputs.size(); i++) {
      const auto &p = original_inputs[i];
      emu->Step(p.first, p.second);
      memories.push_back(emu->GetMemory());
    }
    
    Learnfun learnfun{memories};
    objectives.reset(learnfun.MakeWeighted());
    printf("Saved objectives to %s.\n", cached_objectives.c_str());
    objectives->SaveToFile(cached_objectives);
  }
  CHECK(objectives.get());
  observations.reset(Observations::MixedBaseObservations(*objectives));

  emu->LoadUncompressed(save_after_warmup);
  printf("Fastforward to %d\n", fastforward);
  for (int i = warmup_frames; i < fastforward; i++)
    emu->Step(original_inputs[i].first,
	      original_inputs[i].second);

  start_state = 
    {emu->SaveUncompressed(), emu->GetMemory(),
     0,
     GoalData(),
     markov1->HistoryInDomain(),
     markov2->HistoryInDomain()};
}

Worker *TPP::CreateWorker() {
  Worker *w = new Worker(this);
  w->emu.reset(Emulator::Create(game));
  CHECK(w->emu.get() != nullptr);
  w->ClearStatus();
  w->Restore(start_state);
  return w;
}

void Worker::Visualize(vector<uint8> *argb) {
  MutexLock ml(&mutex);
  CHECK(argb->size() == 4 * 256 * 256);
  emu->GetImageARGB(argb);
  vector<uint8> mem = emu->GetMemory();
  #if 0
  for (int i = 0; i < mem.size(); i++) {
    (*argb)[i * 4 + 0] = mem[i];
    (*argb)[i * 4 + 1] = mem[i];
    (*argb)[i * 4 + 2] = mem[i];
    (*argb)[i * 4 + 3] = 0xFF;
  }
  #endif

  if (goal.has_goal) {
    const int gx = goal.goalx;
    const int gy = goal.goaly;
      
    const int p1x = mem[tpp->x1_loc];
    const int p1y = mem[tpp->y1_loc];
    const int p2x = mem[tpp->x2_loc];
    const int p2y = mem[tpp->y2_loc];

    auto DrawGoal = [argb, gx, gy](int x1, int y1,
				   uint8 rr, uint8 gg, uint8 bb, uint8 aa) {
      for (const std::pair<int, int> point :
	     Line<int>{x1, y1, (int)gx, (int)gy}) {
	int x = point.first, y = point.second;
	if (x >= 0 && y >= 0 && x < 256 && y < 256) {
	  SetPixel(256, 256, x, y, rr, gg, bb, aa, argb);
	  if (x < 255)
	    SetPixel(256, 256, x + 1, y, rr, gg, bb, aa, argb);
	  if (y < 255)
	    SetPixel(256, 256, x, y + 1, rr, gg, bb, aa, argb);
	}
      }
    };
    DrawGoal(p1x, p1y, 0xFF, 127, 127, 0xFF);
    DrawGoal(p2x, p2y, 0, 0, 0xFF, 0xFF);
  }
  
  // Note: This visualization is specific to Contra!
  static constexpr int XWIDTH = 10;
  static constexpr int XTHICK = 2;
  auto DrawDeaths = [argb, &mem](int loc, int xx,
				 uint8 rr, uint8 gg, uint8 bb, uint8 aa) {
    auto DrawX = [argb, rr, gg, bb, aa](int x, int y) {
      for (int t = 0; t < XWIDTH; t++) {
	for (int w = 0; w < XTHICK; w++) {
	  SetPixel(256, 256, x + t + w, y + t,
		   rr, gg, bb, aa, argb);
	  SetPixel(256, 256, x + (XWIDTH - 1 - t) + w, y + t,
		   rr, gg, bb, aa, argb);
	}
      }
    };
    const int deaths = std::min(29 - mem[loc], 4);
    for (int x = 0; x < deaths; x++) {
      DrawX(xx + 2 + x * (XWIDTH + XTHICK + 8), 2);
    }
  };

  DrawDeaths(50, 20, 0xFF, 127, 127, 0xFF);
  DrawDeaths(51, 150, 0, 0, 0xFF, 0xFF);
  
  const double s = tpp->observations->GetWeightedValue(mem);
  // printf("%f\n", s);
  for (int y = 250; y < 256; y++) {
    int len = std::min(256, 5 + (int)(256 * s));
    for (int x = 0; x < len; x++) {
      int i = (y * 256 + x) * 4;
      (*argb)[i + 0] = 0x00;
      (*argb)[i + 1] = 0x00;
      (*argb)[i + 2] = 0xFF;
      (*argb)[i + 3] = 0xFF;
    }
  }
}

void Worker::VizText(vector<string> *text) {
  text->push_back(StringPrintf("Depth %d", depth));
  const vector<uint8> mem = emu->GetMemory();

  // XXX This is specific to contra!
  static constexpr std::initializer_list<int> kLocations = 
    { 48, 100, 101, 820, 821, 50, 51, 2019, 2018, 2021, 2020 };

  for (int i : kLocations) {
    text->push_back(StringPrintf("%d: %d", i, mem[i]));
  }

  text->push_back("--------");
  tpp->observations->VizText(mem, text);
  if (text->size() > 50) {
    text->resize(50);
    text->push_back(" (ahem!) ");
  }
}

void Worker::Observe() {
  // PERF: If we hold the lock, we could just read
  // directly from RAM.
  vector<uint8> mem;
  mem.resize(2048);
  {
    MutexLock ml(&mutex);
    emu->GetMemory(&mem);
  }
  
  tpp->observations->Accumulate(mem);
}
