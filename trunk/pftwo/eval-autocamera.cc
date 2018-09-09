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

#include "../cc-lib/threadutil.h"
#include "../cc-lib/util.h"
#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/simplefm7.h"
#include "../fceulib/cart.h"
#include "../fceulib/ppu.h"
// #include "autocamera.h"
#include "autocamera2.h"
#include "n-markov-controller.h"
#include "autolives.h"
#include "game-database.h"


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
using LivesLoc = AutoLives::LivesLoc;
using Player = Game::Player;

static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

namespace {
struct EvalGame {
  Game game;

  // Outputs.
  int locs_found = 0;
  int locs_known = 0;
  // How deep did we have to go in the ranked list to find the
  // expected ones? Ties are interpreted pessimistically.
  int locs_rank_loss = 0;

  // This counts both "lives" and "health" locations for each player.
  int lives_known = 0;
  int lives_found = 0;
  int lives_rank_loss = 0;
};
}


enum RunState {
  WAITING = 0,
  START,
  RUNNING_AC,
  RUNNING_AL,
  DONE,
};

static const char *StateChar(RunState s) {
  switch(s) {
  case WAITING: return " ";
  case START: return ANSI_YELLOW ">" ANSI_RESET;
  case RUNNING_AC: return ANSI_BLUE "C" ANSI_RESET;
  case RUNNING_AL: return ANSI_PURPLE "L" ANSI_RESET;
  case DONE: return ANSI_GREEN "-" ANSI_RESET;
  default: return "?";
  }
};

// xxx ugh
static std::mutex file_mutex;
static FILE *outfile = nullptr;

static EvalGame EvalOne(const Game &game,
			std::function<void(RunState)> setstate,
			std::function<void(string)> report) {
  const int NUM_SAMPLES = 10;
  const int SAMPLE_EVERY = 500;
  const string &romfile = game.romfile;
  vector<pair<uint8, uint8>> movie =
    SimpleFM7::ReadInputs2P(game.moviefile);
  CHECK(!movie.empty()) << "Couldn't read movie: " << game.moviefile;
  CHECK(movie.size() > WARMUP_FRAMES + (SAMPLE_EVERY * NUM_SAMPLES)) <<
    game.moviefile << " not long enough!";

  vector<uint8> p1inputs;
  for (const auto &p : movie) p1inputs.push_back(p.first);
  NMarkovController nmarkov(p1inputs, 3);
  
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
  report("got samples");
  
  setstate(RUNNING_AC);
  AutoCamera2 ac{romfile};

  vector<vector<Linkage>> links;
  vector<vector<XLoc>> xlocs1, xlocs2;
  links.reserve(samples.size());
  xlocs1.reserve(samples.size());
  xlocs2.reserve(samples.size());
  for (const vector<uint8> &save : samples) {
    auto one_report = [&report, &samples, &xlocs1, &xlocs2](const string &s) {
			report(StringPrintf("[%d/%d] %s",
					    (int)xlocs1.size(),
					    (int)samples.size(),
					    s.c_str()));
		      };
    links.push_back(ac.FindLinkages(save, one_report));
    xlocs1.push_back(ac.FindXLocs(save, false, one_report));

    // Should be no harm in running this when the game isn't actually
    // two-player, but it's a waste of time.
    if (game.is_two_player)
      xlocs2.push_back(ac.FindXLocs(save, true, one_report));
  }

  vector<Linkage> merged = AutoCamera2::MergeLinkages(links);
  vector<XLoc> xmerged1 = AutoCamera2::MergeXLocs(xlocs1);
  vector<XLoc> xmerged2 = AutoCamera2::MergeXLocs(xlocs2);

  std::unordered_map<int, float> xscores1, xscores2;
  for (const XLoc &x : xmerged1) xscores1[x.xloc] = x.score;
  for (const XLoc &x : xmerged2) xscores2[x.xloc] = x.score;

  // Now rescore using xlocs for each player.
  vector<Linkage> rescored1 = merged, rescored2 = merged;
  for (Linkage &l : rescored1) l.score *= (1.0f + xscores1[l.xloc]);
  for (Linkage &l : rescored2) l.score *= (1.0f + xscores2[l.xloc]);
  // Easy way to sort them again.
  rescored1 = AutoCamera2::MergeLinkages({rescored1});
  rescored2 = AutoCamera2::MergeLinkages({rescored2});

  // Rescored1 and Rescored2 are the best guesses for each player.
  
  // Stringify an address, marking it if there's something known
  // about that spot.
  auto AddrInfo =
    [&game](int loc) -> const char * {
      // Prevent this from accidentally matching an "unknown" loc.
      if (loc == -1) return "";
      if (loc == game.p1.xloc) return "p1 x";
      else if (loc == game.p1.yloc) return "p1 y";
      else if (loc == game.p1.lives) return "p1 l";
      else if (loc == game.p1.health) return "p1 h";
      else if (loc == game.p2.xloc) return "p2 x";
      else if (loc == game.p2.yloc) return "p2 y";
      else if (loc == game.p2.lives) return "p2 l";
      else if (loc == game.p2.health) return "p2 h";
      else return "";
    };

  // Now compute stats.
  {

    auto MarkAddr =
      [&AddrInfo](int loc) -> string {
	if (loc == -1) return "-1";
	const char *info = AddrInfo(loc);
	if (info[0] == '\0') {
	  return StringPrintf("%04x", loc);
	} else {
	  return StringPrintf("[%s %04x]", info, loc);
	}
      };
    
    MutexLock ml(&file_mutex);
    if (outfile) {
      fprintf(outfile, "[%s]: Rescored linkages:\n", romfile.c_str());
      
      auto PrintPlayer =
	[&xscores1, &xscores2, &MarkAddr](
	    const vector<Linkage> &rescored) {
	  for (const Linkage &l : rescored) {
	    string xs1 = ContainsKey(xscores1, l.xloc) &&
	      xscores1[l.xloc] > 0.0f ?
	      StringPrintf(" [1p: %.2f] ", xscores1[l.xloc]) : "";
	    string xs2 = ContainsKey(xscores2, l.xloc) &&
	      xscores2[l.xloc] > 0.0f ?
	      StringPrintf(" [2p: %.2f] ", xscores2[l.xloc]) : "";

	    fprintf(outfile,
		    "  %.2f: %d/%d = %s,%s%s%s\n",
		    l.score, l.xloc, l.yloc,
		    MarkAddr(l.xloc).c_str(), MarkAddr(l.yloc).c_str(),
		    xs1.c_str(), xs2.c_str());
	  }
	};

      if (game.is_two_player) {
	fprintf(outfile, " == 1P ==\n");
	PrintPlayer(rescored1);
	fprintf(outfile, " == 2P ==\n");
	PrintPlayer(rescored2);
      } else {
	PrintPlayer(rescored1);
      }
      
      fflush(outfile);
    }
  }


  // Now, autolives for each player.
  auto DoLives =
    [&report, &romfile, &nmarkov, &samples](const vector<Linkage> &rescored,
					    bool player_two) ->
    vector<LivesLoc> {
      if (rescored.empty())
	return {};
      const char *pstr = player_two ? "P2" : "P1";
      const int xloc = rescored[0].xloc, yloc = rescored[0].yloc;
      report(StringPrintf("Autolives %s with player loc %d/%d",
			  pstr, xloc, yloc));

      AutoLives autolives{romfile, nmarkov};
      vector<vector<AutoLives::LivesLoc>> lives_samples;
      lives_samples.reserve(samples.size());
      for (const vector<uint8> &save : samples) {
	lives_samples.push_back(
	    autolives.FindLives(save, xloc, yloc, player_two));
	report(StringPrintf("Autolives %s [%d/%d]", pstr,
			    (int)lives_samples.size(), (int)samples.size()));
      }
      vector<AutoLives::LivesLoc> lives_merged =
	AutoLives::MergeLives(lives_samples);

      return lives_merged;
    };

  setstate(RUNNING_AL);
  vector<LivesLoc> lives1 = DoLives(rescored1, false);
  vector<LivesLoc> lives2 = game.is_two_player ? DoLives(rescored2, true) :
    vector<LivesLoc>{};

  if (!lives1.empty() || !lives2.empty()) {
    MutexLock ml(&file_mutex);
    if (outfile) {
      fprintf(outfile, "[%s]: Merged lives:\n", romfile.c_str());

      auto PLives =
	[&AddrInfo](const Player &player,
		    const vector<LivesLoc> &lives) {
	  for (const LivesLoc &l : lives) {
	    // XXX annotate if correct for player
	    fprintf(outfile,
		    "  %.3f: %d = %04x  %s\n",
		    l.score, l.loc, l.loc, AddrInfo(l.loc));
	  }
	};

      if (game.is_two_player) {
	fprintf(outfile, "  == P1 ==\n");
	PLives(game.p1, lives1);
	fprintf(outfile, "  == P2 ==\n");
	PLives(game.p2, lives2);
      } else {
	PLives(game.p1, lives1);
      }
      
      fflush(outfile);
    }
  }

  
  EvalGame eval_game;
  eval_game.game = game;

  auto StatsPlayer =
    [&eval_game](const Player &player,
		 const vector<Linkage> &rescored,
		 const vector<LivesLoc> &lives) {
      if (player.xloc != -1 &&
	  player.yloc != -1) {
	eval_game.locs_known++;

	int found = 0, rank_loss = 0;
	for (const Linkage &l : rescored) {
	  if (l.xloc == player.xloc && l.yloc == player.yloc) {
	    // Found one of the expected pairs.
	    found++;
	    const float found_score = l.score;
	    // Compute its rank loss, which is the number of pairs with
	    // this score or less that are NOT in our set.
	    for (const Linkage &ll : rescored) {
	      if (ll.score < found_score)
		break;

	      if (ll.xloc != player.xloc || ll.yloc != player.yloc)
		rank_loss++;
	    }
	  }
	}

	eval_game.locs_found += found;
	eval_game.locs_rank_loss += rank_loss;
      }

      if (player.health != -1 ||
	  player.lives != -1) {

	if (player.health != -1)
	  eval_game.lives_known++;
	if (player.lives != -1)
	  eval_game.lives_known++;

	int found = 0, rank_loss = 0;
	for (const LivesLoc &l : lives) {
	  if (l.loc == player.health || l.loc == player.lives) {
	    found++;
	    const float found_score = l.score;
	    for (const LivesLoc &ll : lives) {
	      if (ll.score < found_score)
		break;

	      if (ll.loc != player.lives && ll.loc != player.health)
		rank_loss++;
	    }
	  }
	}

	eval_game.lives_found += found;
	eval_game.lives_rank_loss += rank_loss;
      }
    };

  StatsPlayer(eval_game.game.p1, rescored1, lives1);
  StatsPlayer(eval_game.game.p2, rescored2, lives2);
  
  report(StringPrintf("Done. Found %d/%d. Rank loss %d.",
		      eval_game.locs_found,
		      eval_game.locs_known,
		      eval_game.locs_rank_loss));

  return eval_game;
}

static void EvalAll() {
  outfile = fopen("results.txt", "w");
  CHECK(outfile) << "results.txt";

  // or getmatching...
  vector<Game> games = GameDB().GetAll();
  // vector<Game> games = GameDB().GetMatching(
  // {"mario", "contra", "rocketeer", "werewolf"});
  
  std::mutex status_m;
  vector<RunState> states;
  for (int i = 0; i < games.size(); i++) states.push_back(WAITING);
  vector<string> status;
  status.resize(games.size());
  // Should hold mutex.
  auto ShowTable =
    [&games, &states, &status]() {
      for (int i = 0; i < games.size() + 2; i++) {
	printf("%s", ANSI_PREVLINE);
      }
      printf("\n---------------------------------\n");
      for (int i = 0; i < games.size(); i++) {

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
	states[idx] = START;
	status[idx] = "Start";
	// ShowTable();
      }
      auto Report = [idx, &status_m, &status, &ShowTable](const string &s) {
		      MutexLock ml(&status_m);
		      status[idx] = s;
		      ShowTable();
		    };
      auto SetState = [idx, &status_m, &states](RunState s) {
			MutexLock ml(&status_m);
			states[idx] = s;
		      };
      
      EvalGame res = EvalOne(game, SetState, Report);
      {
	MutexLock ml(&status_m);
	states[idx] = DONE;
	ShowTable();
      }
      return res;
    };

  auto Color3 =
    [](int found, int known, int loss) -> const char * {
      if (known == 0) return "";
      else if (found == known) {
	if (loss == 0)
	  return ANSI_GREEN;
	else
	  return ANSI_YELLOW;
      } else {
	return ANSI_RED;
      }
    };
  
  vector<EvalGame> results = ParallelMapi(games, EvalWithProgress, 11);
  (void)results;
  printf(" == Summary ==\n");
  string col1h = Util::Pad(24, "game.nes");
  printf("%sp recall\tp rank loss\tl recall\t rank loss\n", col1h.c_str());
  for (const EvalGame &g : results) {
    printf("%s"
	   // positions
	   "%s%d/%d" ANSI_RESET "\t%d\t"
	   // lives
	   "%s%d/%d" ANSI_RESET "\t%d\n",
	   Util::Pad(24, g.game.romfile).c_str(),
	   Color3(g.locs_found, g.locs_known, g.locs_rank_loss),
	   g.locs_found, g.locs_known,
	   g.locs_rank_loss,

	   Color3(g.lives_found, g.lives_known, g.lives_rank_loss),
	   g.lives_found, g.lives_known,
	   g.lives_rank_loss);
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
