#include "problem-twoplayer.h"

#include <stdlib.h>
#include <map>
#include <unordered_set>

#include "../fceulib/simplefm2.h"
#include "n-markov-controller.h"
#include "weighted-objectives.h"
#include "learnfun.h"
#include "../cc-lib/util.h"
#include "../cc-lib/lines.h"
#include "headless-graphics.h"
#include "autocamera.h"

// Note NMarkovController::Stats; making this large will often
// make the matrix very sparse.
static constexpr int MARKOV_N = 3;

static_assert(MARKOV_N >= 0 && MARKOV_N <= 8, "allowed range");

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

// For now, try to determine the player locations at startup, using
// the training movie. We run autocamera periodically during the
// input movie, and let the results of that vote to determine x/y
// locations for the two players that we then use for the rest of
// time.

// TODO: It's probably better to run this procedure during search,
// since there might be multiple different phases in a game (a
// simple example would be an overworld map like in Super Mario 3?).
// It also gives us some hope of recovering if we mess it up.
// On the other hand, it creates a host of complexities we have to
// deal with.
static void TryAutoCameras(const string &game,
			   const vector<pair<uint8, uint8>> &inputs,
			   int *x1_loc, int *y1_loc,
			   int *x2_loc, int *y2_loc) {
  vector<vector<uint8>> saves;
  {
    // Assumed this is much smaller than the input movie, or we
    // will waste work.
    static constexpr int NUM_AUTOCAMERAS = 10;
    std::unordered_set<int> framenums;
    // Don't try anything at frame 0, which is almost certainly going
    // to be useless because it's before any initialization code!
    for (int i = 1; i <= NUM_AUTOCAMERAS; i++) {
      double f = i / (double)NUM_AUTOCAMERAS;
      int framenum = f * inputs.size();
      framenums.insert(framenum);
    }

    {
      unique_ptr<Emulator> emu(Emulator::Create(game));
      CHECK(emu.get()) << "(autocamera) " << game;
      vector<uint8> old_oam = AutoCamera::OAM(emu.get());
      for (int i = 0; i < inputs.size(); i++) {
	emu->Step(inputs[i].first, inputs[i].second);
	vector<uint8> new_oam = AutoCamera::OAM(emu.get());
	if (ContainsKey(framenums, i)) {
	  printf("Try autocamera at frame %d:\n", i);
	  saves.push_back(emu->SaveUncompressed());
	  AutoCamera::BestDisplacement(old_oam, new_oam);
	}
	old_oam = std::move(new_oam);
      }
    }
  }

  // TODO: This doesn't work for contra because it does seem to
  // use the trick of rotating the starting sprite during DMA
  // in order to get flicker instead of dropped sprites when
  // there are more than 8 on a scanline.
  //   - in GetXSprites, could also be solving for a modular
  //     offset (e.g. idx = (frame * off) % 64). It's probably
  //     a consistent rotation, but possibly this fails because
  //     of lag frames?
  //   - Maybe could fall back to something that just tries to
  //     find memory locations for which SOME sprite matches
  //     the criteria? e.g. expand XYSprite to have a wildcard?
  //   - Since the player is often multiple sprites, we could
  //     cast a more narrow net by looking for groups (indices
  //     are fixed modular distance from one another) of sprites
  //     that move together and are related to a memory address?
  //   - Hard: Could maybe be inspecting writes to DMA region
  //     directly?
  //
  // Favorite idea so far: Solve for an alignment (on each pair
  // of frames), based on minimum cost:
  //   - like do sprite[i] and sprite[(i + alignment) % 64] have
  //     - similar x,y coordinates
  //     - similar oam data, attributes, palette, etc.?
  //   - is alignment similar to the previous pair of frames?
  // I think this will probably work quite well...
  //
  // Well, on Contra, the sprites are shuffled in a pretty
  // complex pattern, it seems. There are at least a couple of
  // things going on:
  //   - On every frame it appears to rotate the starting sprite
  //     index by +45. (On the start screen, everything keeps a
  //     constant index.
  //   - Also when playing, the number-of-lives indicators have
  //     stable indices most of the time -- but when the bridge
  //     explodes, for example, everything shuffles. (lag frames?)
  //   - The player's pose affects the sprites used to draw
  //     it, and their indices (?)
  //   - When the player is shooting, sometimes bullets take
  //     slots from the player sprite. Maybe the idea is that
  //     bullets would often conflict with the player (same
  //     scanline), so this is prophylactic
  // What to do?
  //   - Maybe a much simpler version of this where we just look
  //     for memory locations that go down when we press left,
  //     up when we press right? But we do need to determine
  //     y coordinate, which we don't control so easily...
  
  using XYSprite = AutoCamera::XYSprite;
  AutoCamera autocamera{game};
  vector<XYSprite> votes;

  for (const vector<uint8> &save : saves) {
    
    #if 0
    int x_num_frames = 0;
    const vector<XYSprite> xcand =
      autocamera.GetXSprites(save, &x_num_frames);
    vector<int> player_sprites;
    const vector<XYSprite> xycand =
      autocamera.FindYCoordinates(save, x_num_frames, xcand,
				  &player_sprites);
    const vector<XYSprite> ccand =
      autocamera.FilterForConsequentiality(save, x_num_frames,
					   xycand);
    // XXX Could do DetectViewType and/or DetectCameraAngle,
    // and increase votes if it succeeds
    // (suggests that we got good sprites?)
    printf("***************************\n");
    printf("*** %d final candidates:\n", (int)ccand.size());
    for (const XYSprite &sprite : ccand) {
      printf(" Sprite %d%s: x:", sprite.sprite_idx,
	     sprite.oldmem ? " (lag)" : "");
      for (const pair<uint16, int> mem : sprite.xmems)
	printf(" %s", AutoCamera::AddrOffset(mem).c_str());
      printf("  y:");
      for (const pair<uint16, int> mem : sprite.ymems)
	printf(" %s", AutoCamera::AddrOffset(mem).c_str());
      printf("\n");
    }
    printf("***************************\n");
    #endif
  }
}

TPP::TwoPlayerProblem(const map<string, string> &config) {
  game = GetDefault(config, "game", "");
  printf("Create TPP for %s...\n", game.c_str());
  const string movie = GetDefault(config, "movie", "");
  printf("Read inputs for %s\n", movie.c_str());
  original_inputs = SimpleFM2::ReadInputs2P(movie);
  CHECK(!original_inputs.empty()) << "No inputs in " << movie;

  string protect = GetDefault(config, "protect", "");
  while (!protect.empty()) {
    string tok = Util::chop(protect);
    if (!tok.empty()) {
      protect_loc.push_back(AtoiHex(tok));
      printf("[CHEATIN'] Protect %d\n", protect_loc.back());
    }
  }

  #if 0
  // Hardcoded cheatin' version.
  x1_loc = AtoiHex(GetDefault(config, "x1", "-1"));
  y1_loc = AtoiHex(GetDefault(config, "y1", "-1"));
  x2_loc = AtoiHex(GetDefault(config, "x2", "-1"));
  y2_loc = AtoiHex(GetDefault(config, "y2", "-1"));
  printf("[CHEATIN'] Players at %d,%d and %d,%d\n",
	 x1_loc, y1_loc, x2_loc, y2_loc);
  #endif

  TryAutoCameras(game, original_inputs,
		 &x1_loc, &y1_loc, &x2_loc, &y2_loc);
  // XXX these should not be fatal! Just disable the goal-seeking stuff.
  CHECK(x1_loc >= 0 && y1_loc >= 0) << "Autocamera failed for player 1";
  CHECK(x2_loc >= 0 && y2_loc >= 0) << "Autocamera failed for player 2";
  printf("Autocamera results: P1 %d,%d   P2 %d,%d\n",
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
