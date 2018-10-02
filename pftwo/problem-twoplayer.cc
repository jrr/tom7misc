#include "problem-twoplayer.h"

#include <stdlib.h>
#include <map>
#include <unordered_set>
#include <iostream>
#include <sstream>

#include "../fceulib/simplefm2.h"
#include "../fceulib/simplefm7.h"
#include "n-markov-controller.h"
#include "weighted-objectives.h"
#include "learnfun.h"
#include "../cc-lib/util.h"
#include "../cc-lib/lines.h"
#include "headless-graphics.h"
#include "autotimer.h"
#include "autocamera2.h"
#include "autolives.h"
#include "emulator-pool.h"

// Note NMarkovController::Stats; making this large will often
// make the matrix very sparse.
static constexpr int MARKOV_N = 3;
static_assert(MARKOV_N >= 0 && MARKOV_N <= 8, "allowed range");

// Allows manual specification of protect locations, camera etc.
// in config file.
static constexpr bool ALLOW_CHEATS = false;

using TPP = TwoPlayerProblem;
using Worker = TPP::Worker;

TPP::Input Worker::AnyInput() const {
  return ControllerInput(0, 0);
}

TPP::InputGenerator Worker::Generator(ArcFour *rc, const Goal *goal) {
  MutexLock ml(&mutex);

  // PERF: Only need this if we have a goal or decide to sync.
  // Better would just be to read memory without copying all of it.
  vector<uint8> mem = emu->GetMemory();
  const int p1x = mem[tpp->x1_loc];
  const int p1y = mem[tpp->y1_loc];
  const int p2x = mem[tpp->x2_loc];
  const int p2y = mem[tpp->y2_loc];

  const bool sync = 
    std::abs(p1x - p2x) < 12 &&
    std::abs(p1y - p2y) < 12 && 
    rc->Byte() < 64;
			  
  if (goal != nullptr) {
    return InputGenerator{tpp, goal,
	p1x, p1y, p2x, p2y,
	sync,
	previous1, previous2};
  } else {
    return InputGenerator{tpp, goal, 0, 0, 0, 0,
	sync,
	previous1, previous2};
  }
}


TPP::Input TPP::InputGenerator::RandomInput(ArcFour *rc) {
  uint8 p1 = tpp->markov1->RandomNext(prev1, rc);
  uint8 p2 = tpp->markov2->RandomNext(prev2, rc);
  if (goal != nullptr) {      
    auto Mask = [this, rc](int px, int py, int buttons) {
      const int gx = goal->goalx;
      const int gy = goal->goaly;

      // If pd is less than gd, then sometimes add the input GO and
      // mask off the input NO_GO.
      auto Dir = [rc, &buttons](int pd, int gd, uint8 GO, uint8 NO_GO) {
	// TODO: Some falloff to the probabilities (proportional to
	// gd - pd)?
	if (pd < gd && rc->Byte() < 64) {
	  buttons &= ~NO_GO;
	  buttons |= GO;
	}
      };
      // Left of goal. Move right.
      Dir(px, gx, INPUT_R, INPUT_L);
      Dir(gx, px, INPUT_L, INPUT_R);
      // TODO: For top-down games these are right, but in a side-
      // view game we need to be hitting the jump button to go up.
      // But also of course jumping is not just a matter of holding
      // the button. Need to detect the view and do something
      // fancier. (This could maybe just be done locally by trying
      // to predict what button combinations yield movement in a
      // direction. This might account for the fact that we can
      // only jump when standing on a platform, or can climb some
      // ladders in e.g. megaman).
      //
      // Just as an example of the complexity: In Contra, holding
      // 'down' is pretty counterproductive because it causes you to
      // lay on the ground. And holding up can be counterproductive in
      // the '3D' corridor levels because it zaps you.
      
      // Above goal. Move down.
      Dir(py, gy, INPUT_D, INPUT_U);
      Dir(gy, py, INPUT_U, INPUT_D);
      return buttons;
    };
    p1 = Mask(p1x, p1y, p1);
    p2 = Mask(p2x, p2y, p2);
  }

  // Execute the same inputs if sync is enabled.
  if (sync) p1 = p2;
  
  prev1 = tpp->markov1->Push(prev1, p1);
  prev2 = tpp->markov2->Push(prev2, p2);
  return ControllerInput(p1, p2);
}

vector<int> TPP::AdjacentCells(int cell) {
  auto C = [](int xx, int yy) {
    return yy * GRID_CELLS_W + xx;
  };
  const int y = cell / GRID_CELLS_W;
  const int x = cell % GRID_CELLS_W;
  vector<int> ret;
  // Look at the 9-connected neighbors.
  for (int dy = -1; dy <= 1; dy++) {
    const int yy = y + dy;
    if (yy >= 0 && yy < GRID_CELLS_H) {
      for (int dx = -1; dx <= 1; dx++) {
	const int xx = x + dx;
	if (xx >= 0 && xx < GRID_CELLS_W) {
	  // But not itself.
	  if (y != yy || x != xx) {
	    ret.push_back(C(xx, yy));
	  }
	}
      }
    }
  }
  return ret;
}


string TPP::SaveSolution(const string &filename_part,
			 const vector<Input> &inputs,
			 const vector<pair<int, string>> &subtitles,
			 const string &info) {
  // Just the controller inputs are saved.
  vector<pair<uint8, uint8>> all_inputs;
  for (int i = 0; i < fastforward; i++) {
    all_inputs.push_back(original_inputs[i]);
  }

  // Have to shift the subtitles to account for warmup/fastforward.
  vector<pair<int, string>> shifted_subtitles;
  shifted_subtitles.emplace_back(0,
				 warmup_frames == fastforward ?
				 "(warmup)" :
				 "(fastforward)");
  
  for (const auto &p : subtitles)
    shifted_subtitles.emplace_back(p.first + all_inputs.size(),
				   p.second);
  
  for (const Input &input : inputs) {
    all_inputs.push_back({Player1(input), Player2(input)});
  }

  string filename =
    StringPrintf("%s-%s.fm2", game.c_str(), filename_part.c_str());
  SimpleFM2::WriteInputsWithSubtitles2P(
      filename,
      game,
      // Take from inputs?
      "base64:WlwvTxyvsfVajcDVrUVQ5Q==",
      all_inputs,
      shifted_subtitles);
  return filename;
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

// Get n sample states from the movie.
static vector<vector<uint8>> GetSamples(
    EmulatorPool *pool,
    const vector<pair<uint8, uint8>> &inputs,
    // Start state after 
    const vector<uint8> &start,
    // First input to execute. Same as warmup_frames.
    int start_idx,
    // Number of samples to get.
    int n) {
  Emulator *emu = pool->Acquire();
  emu->LoadUncompressed(start);
  vector<vector<uint8>> samples;
  samples.reserve(n);

  const int usable_length = (int)inputs.size() - start_idx;
  const int span = usable_length / n;
  CHECK(span > 1) << "Not enough frames in the movie to get "
		  << n << " state samples! start_idx "
		  << start_idx << ", length " << inputs.size();
  int until_next_span = 0;
  for (int i = start_idx; samples.size() < n && i < inputs.size(); i++) {
    if (until_next_span == 0) {
      samples.push_back(emu->SaveUncompressed());
      until_next_span = span - 1;
    } else {
      until_next_span--;
    }
    emu->Step(inputs[i].first, inputs[i].second);
  }
  CHECK(samples.size() == n) << "Maybe an off-by-one style error here";
  pool->Release(emu);
  return samples;
}

// Finds memory locations that appear to be pure count-down or
// count-up timers. These can then be excluded from objective
// functions and from autolives.
void TPP::InitTimers(const map<string, string> &config,
		     EmulatorPool *pool,
		     const vector<uint8> &start) {
  using TimerLoc = AutoTimer::TimerLoc;
  
  CHECK(!game.empty()) << "Not initialized";
  CHECK(warmup_frames >= 0) << "Not initialized";
  CHECK(this->markov1.get() != nullptr) << "Not initialized";
  
  const string cachefile = StringPrintf("%s.timers", game.c_str());

  string cache_contents = Util::ReadFile(cachefile);
  if (!cache_contents.empty()) {

    for (const string &line_orig :
	   Util::SplitToLines(cache_contents)) {
      string line = Util::losewhitel(line_orig);
      if (line.empty() || line[0] == '#') continue;
      const string loc = Util::chop(line);
      const string score = Util::chop(line);
      const string period = Util::chop(line);
      const string incrementing = Util::chop(line);
      if (!incrementing.empty()) {
	TimerLoc tl;
	tl.loc = AtoiHex(loc);
	tl.score = atof(score.c_str());
	tl.period = atof(score.c_str());
	tl.incrementing = AtoiHex(incrementing) > 0;
	timers.push_back(tl);
      }
    }

    printf("Read %d cached timer(s) from %s\n",
	   (int)timers.size(),
	   cachefile.c_str());
    
  } else {

    // Never tuned this. More is probably better!
    printf("Autotimer...\n");
    static constexpr int NUM_SAMPLES = 50;
    vector<vector<uint8>> samples = GetSamples(pool, original_inputs,
					       start, warmup_frames,
					       NUM_SAMPLES);
    AutoTimer autotimer(game, *markov1);
    vector<vector<TimerLoc>> timerlocs;
    timerlocs.resize(samples.size());
    
    ParallelComp(
	NUM_SAMPLES,
	[&autotimer, &samples, &timerlocs](int sample) {
	  const vector<uint8> &save = samples[sample];
	  timerlocs[sample] = autotimer.FindTimers(save);
	  printf(".");
	}, init_threads);

    printf("\nMerging AutoTimer results...\n");
    timers = AutoTimer::MergeTimers(timerlocs);
    {
      // Only keep stuff with a reasonably high score.
      constexpr double min_score = 0.80 * NUM_SAMPLES;
      vector<TimerLoc> filtered;
      filtered.reserve(timers.size());
      for (const TimerLoc &tl : timers)
	if (tl.score > min_score)
	  filtered.push_back(tl);
      timers = std::move(filtered);
    }
    
    string contents = "# Autogenerated\n";
    for (const TimerLoc &tl : timers)
      contents += StringPrintf("0x%04x %.6f %.6f %c\n",
			       tl.loc, tl.score, tl.period,
			       tl.incrementing ? '1' : '0');
    Util::WriteFile(cachefile, contents);
    printf("Wrote to %s\n", cachefile.c_str());
  }
}


// For now, try to determine the player locations at startup, using
// the training movie. We run autocamera periodically during the
// input movie, and let the results of that vote to determine x/y
// locations for the two players that we then use for the rest of
// time.
//
// Since this can be fairly slow, we cache the results in a file.
void TPP::InitCameras(const map<string, string> &config,
		      EmulatorPool *pool,
		      const vector<uint8> &start) {
  using XLoc = AutoCamera2::XLoc;
  using Linkage = AutoCamera2::Linkage;

  CHECK(!game.empty()) << "Not initialized";
  CHECK(warmup_frames >= 0) << "Not initialized";

  const string cachefile = StringPrintf("%s.camera", game.c_str());

  string cache_contents = Util::ReadFile(cachefile);
  if (!cache_contents.empty()) {
    stringstream ss(cache_contents, stringstream::in);
    CHECK(!ss.eof()) << "Malformed " << cachefile;
    ss >> x1_loc;
    CHECK(!ss.eof()) << "Malformed " << cachefile;
    ss >> y1_loc;
    CHECK(!ss.eof()) << "Malformed " << cachefile;
    ss >> x2_loc;
    CHECK(!ss.eof()) << "Malformed " << cachefile;
    ss >> y2_loc;

    printf("Read cached cameras from %s: %d %d %d %d\n",
	   cachefile.c_str(),
	   x1_loc, y1_loc, x2_loc, y2_loc);
  } else {

    // Never tuned this. More is probably better!
    static constexpr int NUM_SAMPLES = 10;
    vector<vector<uint8>> samples = GetSamples(pool, original_inputs,
					       start, warmup_frames,
					       NUM_SAMPLES);

    vector<vector<Linkage>> linkages;
    linkages.resize(samples.size());
    vector<vector<XLoc>> xlocs1, xlocs2;
    xlocs1.resize(samples.size());
    xlocs2.resize(samples.size());

    // PERF could create from pool...
    AutoCamera2 autocamera(game);

    printf("Running AutoCamera...\n");
    ParallelComp2D(
	samples.size(),
	// 2 players, 1 linkages
	3,
	[pool, &samples, &linkages, &xlocs1, &xlocs2, &autocamera](
	    int sample, int channel) {
	  const vector<uint8> &save = samples[sample];

	  switch (channel) {
	  case 0:
	    linkages[sample] = autocamera.FindLinkages(save, nullptr);
	    printf("L");
	    break;
	  case 1:
	    xlocs1[sample] = autocamera.FindXLocs(save, false, nullptr);
	    printf("1");
	    break;
	  case 2:
	    xlocs2[sample] = autocamera.FindXLocs(save, true, nullptr);
	    printf("2");
	    break;
	  }
	  fflush(stdout);
	}, init_threads);

    printf("\nMerging AutoCamera results...\n");
    vector<Linkage> merged = AutoCamera2::MergeLinkages(linkages);
    vector<XLoc> xmerged1 = AutoCamera2::MergeXLocs(xlocs1);
    vector<XLoc> xmerged2 = AutoCamera2::MergeXLocs(xlocs2);

    std::unordered_map<int, float> xscores1, xscores2;
    for (const XLoc &x : xmerged1) xscores1[x.xloc] = x.score;
    for (const XLoc &x : xmerged2) xscores2[x.xloc] = x.score;

    // Now rescore using xlocs for each player.
    vector<Linkage> rescored1 = merged, rescored2 = merged;
    for (Linkage &l : rescored1)
      l.score *= (1.0f + xscores1[l.xloc]);
    for (Linkage &l : rescored2)
      l.score *= (1.0f + xscores2[l.xloc]);
    // Easy way to sort them again.
    rescored1 = AutoCamera2::MergeLinkages({rescored1});
    rescored2 = AutoCamera2::MergeLinkages({rescored2});

    if (!rescored1.empty()) {
      x1_loc = rescored1.front().xloc;
      y1_loc = rescored1.front().yloc;
      printf("AutoCamera2 set p1 loc to %d,%d (score %.3f).\n",
	     x1_loc, y1_loc, rescored1.front().score);
    }

    if (!rescored2.empty()) {
      x2_loc = rescored2.front().xloc;
      y2_loc = rescored2.front().yloc;
      printf("AutoCamera2 set p2 loc to %d,%d (score %.3f).\n",
	     x2_loc, y2_loc, rescored2.front().score);
    }

    string contents = StringPrintf("%d %d %d %d\n",
				   x1_loc, y1_loc, x2_loc, y2_loc);
    Util::WriteFile(cachefile, contents);
    printf("Wrote to %s\n", cachefile.c_str());
  }

  // Override from config file if present, and allowed.
  if (ALLOW_CHEATS) {
    x1_loc = AtoiHex(GetDefault(config, "x1", "-1"));
    y1_loc = AtoiHex(GetDefault(config, "y1", "-1"));
    x2_loc = AtoiHex(GetDefault(config, "x2", "-1"));
    y2_loc = AtoiHex(GetDefault(config, "y2", "-1"));
    printf("[CHEATIN'] Players at %d,%d and %d,%d\n",
	   x1_loc, y1_loc, x2_loc, y2_loc);
  }
}

// Should follow InitCameras.
void TPP::InitLives(const map<string, string> &config,
		    EmulatorPool *pool,
		    const vector<uint8> &start) {
  using LivesLoc = AutoLives::LivesLoc;
  
  CHECK(!game.empty()) << "Not initialized";
  CHECK(warmup_frames >= 0) << "Not initialized";
  CHECK(markov1.get() != nullptr &&
	markov2.get() != nullptr) << "Not initialized";
  
  const string cachefile = StringPrintf("%s.lives", game.c_str());

  string cache_contents = Util::ReadFile(cachefile);
  if (!cache_contents.empty()) {
    while (!cache_contents.empty()) {
      const string tok = Util::chop(cache_contents);
      if (!tok.empty()) {
	protect_loc.push_back(AtoiHex(tok));
      }
    }

    printf("Read cached lives from %s:", cachefile.c_str());
    for (int loc : protect_loc) printf(" %d", loc);
    printf("\n");
    
  } else {

    // Never tuned this. More is probably better!
    static constexpr int NUM_SAMPLES = 10;
    vector<vector<uint8>> samples = GetSamples(pool, original_inputs,
					       start, warmup_frames,
					       NUM_SAMPLES);
    vector<vector<LivesLoc>> llocs1, llocs2;
    llocs1.resize(samples.size());
    llocs2.resize(samples.size());

    // PERF should share the emulator pool.
    AutoLives autolives1(game, *markov1);
    AutoLives autolives2(game, *markov2);

    printf("Running autolives on 2 players, %d samples. This will\n"
	   "likely take several minutes; the bar below must be "
	   "completed:\n",
	   (int)samples.size());
    for (int i = 0; i < NUM_SAMPLES * 2; i++) printf("-");
    printf("\n");
    ParallelComp2D(
	NUM_SAMPLES,
	// Players
	2,
	[this, &samples, &autolives1, &autolives2,
	 &llocs1, &llocs2](int sample, int player) {
	  const vector<uint8> &save = samples[sample];
	  switch (player) {
	  case 0:
	    if (x1_loc >= 0 && x2_loc >= 0)
	      llocs1[sample] =
		autolives1.FindLives(save, x1_loc, y1_loc, false);
	    printf("1");
	    break;
	  case 1:
	    if (x2_loc >= 0 && x2_loc >= 0)
	      llocs2[sample] =
		autolives2.FindLives(save, x2_loc, y2_loc, true);
	    printf("2");
	    break;
	  }
	  fflush(stdout);
	}, init_threads);

    // XXX
    printf("\nMerging AutoLives results...\n");
    vector<LivesLoc> lives1 = AutoLives::MergeLives(llocs1);
    vector<LivesLoc> lives2 = AutoLives::MergeLives(llocs2);

    if (!lives1.empty()) {
      const LivesLoc &ll = lives1.front();
      protect_loc.push_back(ll.loc);
      printf("AutoLives protects %d for p1 (score %.3f).\n",
	     ll.loc, ll.score);
    }

    if (!lives2.empty()) {
      const LivesLoc &ll = lives2.front();
      protect_loc.push_back(ll.loc);
      printf("AutoLives protects %d for p2 (score %.3f).\n",
	     ll.loc, ll.score);
    }

    // Contents should not be totally empty, or else we interpret this
    // as needing to rerun autolives. But failing to find lives is
    // fairly normal and we still want to cache that.
    string contents = " ";
    for (int loc : protect_loc) contents += StringPrintf(" %d", loc);
    Util::WriteFile(cachefile, contents);
    printf("Wrote to %s\n", cachefile.c_str());
  }

  // Override from config file if present, and allowed.
  if (ALLOW_CHEATS) {
    // These totally override any automatic ones; not appending.
    protect_loc.clear();
    string protect = GetDefault(config, "protect", "");
    while (!protect.empty()) {
      string tok = Util::chop(protect);
      if (!tok.empty()) {
	protect_loc.push_back(AtoiHex(tok));
	printf("[CHEATIN'] Protect %d\n", protect_loc.back());
      }
    }
  }
}


TPP::TwoPlayerProblem(const map<string, string> &config) {
  game = GetDefault(config, "game", "");
  printf("Create TPP for %s...\n", game.c_str());
  const string movie = GetDefault(config, "movie", "");
  printf("Read inputs for %s\n", movie.c_str());
  original_inputs = Util::endswith(movie, ".fm2") ?
    SimpleFM2::ReadInputs2P(movie) :
    SimpleFM7::ReadInputs2P(movie);

  CHECK(!original_inputs.empty()) << "No inputs in " << movie;

  if (ContainsKey(config, "init-threads")) {
    init_threads = AtoiHex(GetDefault(config, "init-threads", "6"));
  } else if (ContainsKey(config, "workers")) {
    init_threads = AtoiHex(GetDefault(config, "workers", "6"));
  }

  
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

  // Temporary pool for initialization.
  EmulatorPool pool(game, 3);
  
  // Throwaway emulator instance for warmup, training.
  printf("Warmup.\n");
  Emulator *emu = pool.Acquire();
  for (int i = 0; i < warmup_frames; i++) {
    emu->Step(original_inputs[i].first, original_inputs[i].second);
  }
  
  // Save start state for each worker.
  vector<uint8> save_after_warmup = emu->SaveUncompressed();

  InitTimers(config, &pool, save_after_warmup);
  InitCameras(config, &pool, save_after_warmup);
  InitLives(config, &pool, save_after_warmup);
  
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
     markov1->HistoryInDomain(),
     markov2->HistoryInDomain()};

  pool.Release(emu);
}

Worker *TPP::CreateWorker() {
  Worker *w = new Worker(this);
  w->emu.reset(Emulator::Create(game));
  CHECK(w->emu.get() != nullptr);
  w->ClearStatus();
  w->Restore(start_state);
  return w;
}

void Worker::Visualize(const Goal *goal, vector<uint8> *argb) {
  MutexLock ml(&mutex);
  CHECK(argb->size() == 4 * 256 * 256);
  emu->GetImageARGB(argb);
  vector<uint8> mem = emu->GetMemory();

  if (goal != nullptr) {
    const int gx = goal->goalx;
    const int gy = goal->goaly;
      
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

void Worker::VizText(const Goal *goal, vector<string> *text) {
  text->push_back(StringPrintf("Depth %d", depth));
  const vector<uint8> mem = emu->GetMemory();

  // TODO: Visualize goal? It's pretty obvious from the picture
  // whenever that's present.
  
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
