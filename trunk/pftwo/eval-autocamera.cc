// Evaluates autocamera against a set of known good memory locations.

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

#include <cmath>
#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/cart.h"
#include "../fceulib/ppu.h"
#include "../cc-lib/re2/re2.h"
#include "autocamera.h"
#include "autocamera2.h"

#include "../cc-lib/threadutil.h"

#ifdef ENABLE_AOT
# error eval-autocamera can not use AOT (needs to load multiple games)
#endif

#define WARMUP_FRAMES 800

using Linkage = AutoCamera2::Linkage;

static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

struct Game {
  string romfile;
  string moviefile;
  // Pairs of x,y locations that are expected to be among
  // the best linkages. Usually length 1 or 2 (for 2P games).
  vector<pair<int, int>> expected;

  Game() {}
  Game(string rom, string movie, vector<pair<int, int>> exp)
    : romfile(rom), moviefile(movie), expected(exp) {}
  
  // Outputs.
  float fraction_found = 0.0f;
  // How deep did we have to go in the ranked list to find the
  // expected ones? Ties are interpreted pessimistically.
  int rank_loss = 0;
};

static vector<Game> Games() {
  return {
	  // Verified.
	  Game{"mario.nes", "mario.fm2", {{0x86, 0xCE}}},
	  // Verified.
	  Game{"contra.nes", "contra2p.fm2", {{0x334, 0x31A},
					      {0x335, 0x31B}}},
	  // From wiki.
	  Game{"megaman2.nes", "megaman2.fm2", {{0x460, 0x4A0}}},
	  // From wiki.
	  // This works but is VERY SLOW.
	  Game{"lolo.nes", "lolo.fm2", {{0x6D, 0x6F}}},
	  // Verified. (wiki says 0x51,0x52 which is not right?)
	  Game{"metroid.nes", "metroid.fm2", {{0x30E, 0x30D}}},
	  // From glEnd.
	  Game{"zelda.nes", "zelda.fm2", {{0x70, 0x84}}},
	  // TODO: These need expected positions
	  Game{"littlemermaid.nes", "littlemermaid.fm2", {}},
	  Game{"gyromite.nes", "gyromite.fm2", {}},
	  Game{"faxanadu.nes", "faxanadu.fm2", {}},
	  Game{"rivercity.nes", "rivercity.fm2", {}},
	  // Had trouble figuring this one out in emulator.
	  // x: 0x40c? 433? 440?
	  Game{"rocketeer.nes", "rocketeer.fm2", {}},
	  };
}

static Game EvalOne(const Game &game) {
  const int NUM_SAMPLES = 10;
  const int SAMPLE_EVERY = 500;
  const string &romfile = game.romfile;
  vector<pair<uint8, uint8>> movie =
    SimpleFM2::ReadInputs2P(game.moviefile);
  CHECK(!movie.empty()) << "Couldn't read movie: " << game.moviefile;
  CHECK(movie.size() > WARMUP_FRAMES + (SAMPLE_EVERY * NUM_SAMPLES)) <<
    game.moviefile << " not long enough!";
  
  unique_ptr<Emulator> emu;
  emu.reset(Emulator::Create(romfile));
  CHECK(emu.get() != nullptr) << romfile;
  
  // Warm up.
  int frameidx = 0;
  for (int i = WARMUP_FRAMES; i--;) {
    emu->StepFull(movie[frameidx].first, movie[frameidx].second);
    frameidx++;
  }
  printf("[%s] warmed up\n", romfile.c_str());

  vector<vector<uint8>> samples;
  samples.reserve(NUM_SAMPLES);
  while (frameidx < movie.size() &&
	 samples.size() < NUM_SAMPLES) {
    if (frameidx % SAMPLE_EVERY == 0) {
      samples.push_back(emu->SaveUncompressed());
    }
    emu->StepFull(movie[frameidx].first, movie[frameidx].second);    
    frameidx++;
  }

  AutoCamera2 ac{romfile};

  vector<vector<Linkage>> links;
  links.reserve(samples.size());
  for (const vector<uint8> &save : samples) {
    links.push_back(ac.FindLinkages(save));
    printf("[%s] %d/%d\n", romfile.c_str(),
	   (int)links.size(), (int)samples.size());
  }

  std::set<pair<int, int>> expset;
  for (const auto &p : game.expected) expset.insert(p);
  
  vector<Linkage> merged = AutoCamera2::MergeLinkages(links);
  // Now compute stats.
  printf("[%s]: Merged:\n", romfile.c_str());
  for (const Linkage &l : merged) {
    printf("  %.2f: %d/%d = 0x%04x,0x%04x%s\n",
	   l.score, l.xloc, l.yloc,
	   l.xloc, l.yloc,
	   ContainsKey(expset, make_pair(l.xloc, l.yloc)) ? " <-" : "");
  }
  Game game_copy = game;
  
  if (!game.expected.empty()) {
    int found = 0;
    int rank_loss = 0;
    for (const Linkage &l : merged) {
      if (ContainsKey(expset, make_pair(l.xloc, l.yloc))) {
	// Found one of the expected pairs.
	found++;
	const float found_score = l.score;
	// Compute its rank loss, which is the number of pairs with
	// this score or less that are NOT in our set.
	for (const Linkage &ll : merged) {
	  if (ll.score < found_score)
	    break;
	  
	  if (!ContainsKey(expset, make_pair(ll.xloc, ll.yloc)))
	    rank_loss++;
	}
      }
    }
    game_copy.fraction_found = (float)found / (float)game.expected.size();
    game_copy.rank_loss = rank_loss;
  }
  
  return game_copy;
}
  
// The main loop for SDL.
int main(int argc, char *argv[]) {
  (void)Rtos;

  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif
  
  vector<Game> games = Games();
  // PERF threaded, or make autocamera threaded!
  vector<Game> results = ParallelMap(games, EvalOne, 11);
  (void)results;
  printf(" == Summary ==\n");
  printf("game.nes\t\trecall\trank loss\n");
  for (const Game &g : results) {
    printf("%s\t\t%.2f\t%d\n",
	   g.romfile.c_str(),
	   g.fraction_found,
	   g.rank_loss);
  }
  return 0;
}
