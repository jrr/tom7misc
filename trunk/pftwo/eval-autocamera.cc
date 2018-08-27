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
#include "../fceulib/simplefm7.h"
#include "../fceulib/cart.h"
#include "../fceulib/ppu.h"
// #include "../cc-lib/re2/re2.h"
#include "../cc-lib/util.h"
#include "autocamera.h"
#include "autocamera2.h"

#include "../cc-lib/threadutil.h"

#ifdef ENABLE_AOT
# error eval-autocamera can not use AOT (needs to load multiple games)
#endif

#define WARMUP_FRAMES 800

// Cursor to beginning of previous line
#define ANSI_PREVLINE "\x1B[F"
#define ANSI_CLEARLINE "\x1B[2K"
#define ANSI_CLEARTOEOL "\x1B[0K"

#define ANSI_RED "\x1B[1;31;40m"
#define ANSI_GREY "\x1B[1;30;40m"
#define ANSI_BLUE "\x1B[1;34;40m"
#define ANSI_CYAN "\x1B[1;36;40m"
#define ANSI_YELLOW "\x1B[1;33;40m"
#define ANSI_GREEN "\x1B[1;32;40m"
#define ANSI_WHITE "\x1B[1;37;40m"
#define ANSI_PURPLE "\x1B[1;35;40m"
#define ANSI_RESET "\x1B[m"


using Linkage = AutoCamera2::Linkage;
using XLoc = AutoCamera2::XLoc;

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
  // float fraction_found = 0.0f;
  int found = 0;
  // How deep did we have to go in the ranked list to find the
  // expected ones? Ties are interpreted pessimistically.
  int rank_loss = 0;
};

static vector<Game> Games() {
  return {

	  Game{"werewolf.nes", "werewolf.fm7", {}},
	  // Verified. Warping just moves the feet, but the head will
	  // reattach when switching rooms. The head is also
	  // separately warpable (0x0707,0x0704), but doesn't seem to
	  // be the "master" location.
	  Game{"deadlytowers.nes", "deadlytowers.fm7", {{0x0703,0x0700}}},

#if 1
	  // Verified.
	  Game{"mario.nes", "mario.fm7", {{0x86, 0xCE}}},
	  // Verified.
	  Game{"contra.nes", "contra2p.fm7", {{0x334, 0x31A},
					      {0x335, 0x31B}}},
	  // From wiki.
	  Game{"megaman2.nes", "megaman2.fm7", {{0x460, 0x4A0}}},
	  // From wiki.
	  // This works but is VERY SLOW.
	  Game{"lolo.nes", "lolo.fm7", {{0x6D, 0x6F}}},
	  // Has horizontal and vertical scrolling.
	  // Verified. (wiki says 0x51,0x52 which is not right?)
	  Game{"metroid.nes", "metroid.fm7", {{0x30E, 0x30D}}},
	  // From glEnd.
	  Game{"zelda.nes", "zelda.fm7", {{0x70, 0x84}}},
	  // Verified. Warping definitely moves the sprite, though
	  // it also causes physics to get desynced.
	  Game{"rocketeer.nes", "rocketeer.fm7", {{0x40c,0x419}}},
	  // Verified. Screen coordinates. Warping works great!
	  Game{"gyromite.nes", "gyromite.fm7", {{0x609, 0x608}}},
	  // Has horizontal and vertical scrolling.
	  // Verified. Prescroll coordinates. Warping can cause
	  // glitches/locks.
	  Game{"littlemermaid.nes", "littlemermaid.fm7", {{0x330, 0x360}}},
	  // Verified. Warping works great. This game does split
	  // scrolling for a bottom menu, so it always appears to be
	  // (close to) 0,0.
	  Game{"backtothefuture.nes", "backtothefuture.fm7", {{0x3a2, 0x3a7}}},
	  // 2p. Warping works great. Lots of enemy sprite locations are
	  // detected too.
	  Game{"bubblebobble.nes", "bubblebobble2p.fm7", {{0x203,0x200},
							  {0x20b,0x208}}},

	  // 2P. There are lots of locations that track the components
	  // of the monster sprites. But these two mostly allow
	  // warping.
	  Game{"rampage.nes", "rampage2p.fm7", {{0x102, 0x103},
						{0x12e, 0x12f}}},

	  // Verified. Warping works.
	  Game{"adventureisland.nes", "adventureisland.fm7", {{0x584,0x5d3}}},
	  // Verified. Warping works well.
	  Game{"kidicarus.nes", "kidicarus.fm7", {{0x723,0x720}}},
	  // Verified. Warping works well. I think it has a sprite
	  // to do split-scrolling like mario (0x203,0x200)
	  Game{"ninjagaiden.nes", "ninjagaiden.fm7", {{0x086,0x08a}}},

	  // Found in FCEUX. Warping works.
	  Game{"baddudes.nes", "baddudes.fm7", {{0x2a6, 0x2a4}}},

	  // Found in FCEUX. Warping works.
	  Game{"jackiechan.nes", "jackiechan.fm7", {{0x610, 0x620}}},

	  // 0x370,0x330 is the cue ball. Lots of balls detected, but
	  // not the cursor?
	  Game{"lunarpool.nes", "lunarpool.fm7", {}},

	  // Doesn't work... or every sprite has its own memory loc
	  // and the right address is too deep?
	  // Looks like while there are many sprites associated with
	  // the player, the master location is stored as coarse/fine
	  // (just guessing)... 0x028 is the x game-time number.
	  Game{"bomberman.nes", "bomberman.fm7", {}},

	  // Several x addresses for sprites:
	  // 0x243, 247, 24b, 343, 347, 34b
	  //
	  // 0x617 seems to be the player's x tile coordinate
	  // This one is pretty tricky because the player's y
	  // coordinate seems pinned to the center of the screen,
	  // except like when you fall onto spikes
	  Game{"cliffhanger.nes", "cliffhanger.fm7", {}},
	  // Has split x-scrolling, but also appears to use some
	  // mapper tricks to do vertical scrolling?
	  // Despite these having weirdly distant locations, it
	  // does check out, and warping works.
	  Game{"ducktales.nes", "ducktales.fm7", {{0x720, 0x640}}},

	  // Doesn't work. has split scrolling. Difficult game.
	  Game{"gauntlet2.nes", "gauntlet2.fm7", {}},

	  // autocamera finds this now. verified with warping.
	  Game{"strider.nes", "strider.fm7", {{0x508, 0x50b}}},

	  // Verified. xlocs finds 0x09e, which is good. The y
	  // coordinate's value weirdly remaind like 24 pixels above
	  // the player, but warping works great, so I guess it's just
	  // a weird one...
	  Game{"faxanadu.nes", "faxanadu.fm7", {{0x09e, 0x0a1}}},

	  // Found in FCEUX and searching. These work with warping
	  // too. But y is the y coord negated! (Maybe it's using
	  // cartesian axes?) We don't expect autocamera to find this,
	  // but it appears to be the best answer for this game.
	  // 
	  // 1p: 0x08e, 0x09e (negated)
	  // 2p: 0x084, 0x09f (negated)
	  Game{"rivercity.nes", "rivercity2p.fm7", {{0x08e, 0x09e},
						    {0x084, 0x09f}}},
#endif
  };
}

// xxx ugh
std::mutex file_mutex;
FILE *outfile = nullptr;

static Game EvalOne(const Game &game, std::function<void(string)> report) {
  const int NUM_SAMPLES = 10;
  const int SAMPLE_EVERY = 500;
  const string &romfile = game.romfile;
  vector<pair<uint8, uint8>> movie =
    SimpleFM7::ReadInputs2P(game.moviefile);
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
  report("warmed up");

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
  vector<vector<XLoc>> xlocs1, xlocs2;
  links.reserve(samples.size());
  xlocs1.reserve(samples.size());
  xlocs2.reserve(samples.size());
  for (const vector<uint8> &save : samples) {
    auto one_report = [&report, &samples, &xlocs1, &xlocs2](const string &s) {
			report(StringPrintf("[%d/%d] %s",
					    (int)xlocs2.size(),
					    (int)samples.size(),
					    s.c_str()));
		      };
    links.push_back(ac.FindLinkages(save, one_report));
    xlocs1.push_back(ac.FindXLocs(save, false, one_report));
    xlocs2.push_back(ac.FindXLocs(save, true, one_report));
  }

  std::set<pair<int, int>> expset;
  for (const auto &p : game.expected) expset.insert(p);

  vector<Linkage> merged = AutoCamera2::MergeLinkages(links);
  vector<XLoc> xmerged1 = AutoCamera2::MergeXLocs(xlocs1);
  vector<XLoc> xmerged2 = AutoCamera2::MergeXLocs(xlocs2);

  std::unordered_map<int, float> xscores1, xscores2;
  for (const XLoc &x : xmerged1) xscores1[x.xloc] = x.score;
  for (const XLoc &x : xmerged2) xscores2[x.xloc] = x.score;

  // Now compute stats.
  {
    MutexLock ml(&file_mutex);
    if (outfile) {
      fprintf(outfile, "[%s]: Merged linkages:\n", romfile.c_str());
      for (const Linkage &l : merged) {
	string xs1 = ContainsKey(xscores1, l.xloc) ?
	  StringPrintf(" [1p: %.2f] ", xscores1[l.xloc]) : "";
	string xs2 = ContainsKey(xscores2, l.xloc) ?
	  StringPrintf(" [2p: %.2f] ", xscores2[l.xloc]) : "";

	fprintf(outfile,
		"  %.2f: %d/%d = 0x%04x,0x%04x%s%s%s\n",
		l.score, l.xloc, l.yloc,
		l.xloc, l.yloc,
		xs1.c_str(), xs2.c_str(),
		ContainsKey(expset, make_pair(l.xloc, l.yloc)) ? " <-" : "");
      }
      fprintf(outfile, "  -- Merged xlocs 1p --\n");
      for (const XLoc &x : xmerged1) {
	fprintf(outfile, "  %.2f: %d = 0x%04x\n", x.score, x.xloc, x.xloc);
      }
      fprintf(outfile, "  -- Merged xlocs 2p --\n");
      for (const XLoc &x : xmerged2) {
	fprintf(outfile, "  %.2f: %d = 0x%04x\n", x.score, x.xloc, x.xloc);
      }
      
      fflush(outfile);
    }
  }

  // Rescore linkages with xlocs for both players.
  vector<Linkage> rescored = merged;
  for (Linkage &l : rescored) {
    l.score *= (1.0f + xscores1[l.xloc] + xscores2[l.xloc]);
  }
  // Easy way to sort them again.
  rescored = AutoCamera2::MergeLinkages({rescored});
  
  Game game_copy = game;

  if (!game.expected.empty()) {
    int found = 0;
    int rank_loss = 0;
    for (const Linkage &l : rescored) {
      if (ContainsKey(expset, make_pair(l.xloc, l.yloc))) {
	// Found one of the expected pairs.
	found++;
	const float found_score = l.score;
	// Compute its rank loss, which is the number of pairs with
	// this score or less that are NOT in our set.
	for (const Linkage &ll : rescored) {
	  if (ll.score < found_score)
	    break;

	  if (!ContainsKey(expset, make_pair(ll.xloc, ll.yloc)))
	    rank_loss++;
	}
      }
    }
    // game_copy.fraction_found = (float)found / (float)game.expected.size();
    game_copy.found = found;
    game_copy.rank_loss = rank_loss;

    report(StringPrintf("Found %d/%d. Rank loss %d",
			found, (int)game_copy.expected.size(),
			rank_loss));
  }

  return game_copy;
}

static void EvalAll() {
  outfile = fopen("results.txt", "w");
  CHECK(outfile) << "results.txt";

  vector<Game> games = Games();

  enum State {
    WAITING = 0,
    RUNNING,
    DONE,
  };

  std::mutex status_m;
  vector<State> states;
  for (int i = 0; i < games.size(); i++) states.push_back(WAITING);
  vector<string> status;
  status.resize(games.size());
  // Should hold mutex.
  auto ShowTable =
    [&games, &states, &status]() {
      for (int i = 0; i < games.size() + 2; i++) {
	printf("%s", ANSI_PREVLINE);
	// #define ANSI_PREVLINE "\x1B[F"
      }
      printf("\n---------------------------------\n");
      for (int i = 0; i < games.size(); i++) {
	auto StateChar =
	  [](State s) {
	    switch(s) {
	    case WAITING: return " ";
	    case RUNNING: return ANSI_BLUE "*" ANSI_RESET;
	    case DONE: return ANSI_GREEN "-" ANSI_RESET;
	    default: return "?";
	    }
	  };

	printf("[%s] " ANSI_WHITE "%s" ANSI_RESET ": %s" ANSI_CLEARTOEOL "\n",
	       StateChar(states[i]),
	       games[i].romfile.c_str(),
	       status[i].c_str());
      }
    };

  auto EvalWithProgress =
    [&games, &status_m, &states, &status, &ShowTable](
	int idx, const Game &game) {
      {
	MutexLock ml(&status_m);
	states[idx] = RUNNING;
	status[idx] = "Start";
	// ShowTable();
      }
      auto Report = [idx, &status_m, &status, &ShowTable](const string &s) {
		      MutexLock ml(&status_m);
		      status[idx] = s;
		      ShowTable();
		    };
      Game res = EvalOne(game, Report);
      {
	MutexLock ml(&status_m);
	states[idx] = DONE;
	ShowTable();
      }
      return res;
    };

  vector<Game> results = ParallelMapi(games, EvalWithProgress, 11);
  (void)results;
  printf(" == Summary ==\n");
  string col1h = Util::Pad(24, "game.nes");
  printf("%srecall\trank loss\n", col1h.c_str());
  for (const Game &g : results) {
    printf("%s\t\t%d/%d\t%d\n",
	   Util::Pad(24, g.romfile).c_str(),
	   g.found, (int)g.expected.size(),
	   g.rank_loss);
  }

  fclose(outfile);
}

int main(int argc, char *argv[]) {
  (void)Rtos;

  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  EvalAll();
  return 0;
}
