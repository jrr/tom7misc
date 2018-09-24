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
#include "n-markov-controller.h"
#include "autocamera2.h"
#include "autotimer.h"
#include "autolives.h"
#include "game-database.h"

// XXX get from config.txt?
static constexpr int MAX_CONCURRENCY = 11;

#ifdef ENABLE_AOT
# error eval-autocamera can not use AOT (needs to load multiple games)
#endif

// Usually pointless to run autocamera on start screens and intro.
// AutoLives is particularly useless (harmful?) because lives and
// health may not be initialized yet.
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
using TimerLoc = AutoTimer::TimerLoc;
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
struct Stats {
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


enum class RunState {
  WAITING = 0,
  START,
  RUNNING_AT,
  RUNNING_AC,
  RUNNING_AL,
  STATS,
  DONE,
};

static const char *StateChar(RunState s) {
  switch(s) {
  case RunState::WAITING: return " ";
  case RunState::START: return ANSI_YELLOW ">" ANSI_RESET;
  case RunState::RUNNING_AT: return ANSI_BLUE "T" ANSI_RESET;
  case RunState::RUNNING_AC: return ANSI_CYAN "C" ANSI_RESET;
  case RunState::RUNNING_AL: return ANSI_PURPLE "L" ANSI_RESET;
  case RunState::STATS: return ANSI_WHITE "S" ANSI_RESET;
  case RunState::DONE: return ANSI_GREEN "-" ANSI_RESET;
  default: return "?";
  }
};

static vector<uint8> Inputs1P(const vector<pair<uint8, uint8>> &inputs2p) {
  vector<uint8> ret;
  ret.reserve(inputs2p.size());
  for (const auto &p : inputs2p) ret.push_back(p.first);
  return ret;
}

static constexpr int NUM_SAMPLES = 10;
static constexpr int SAMPLE_EVERY = 500;
struct Evaluation {
  Evaluation() {
    outfile = fopen("results.txt", "w");
    CHECK(outfile) << "results.txt";
  }
  
  struct One {
    const Game game;
    // Summary of the results.
    Stats stats;
    // Sampled 
    vector<vector<uint8>> samples;
    const vector<pair<uint8, uint8>> movie;
    const NMarkovController nmarkov;
    AutoTimer autotimer;
    AutoCamera2 autocamera;
    AutoLives autolives;

    // Filled (in parallel) during phase 2.
    vector<vector<TimerLoc>> timerlocs;
    vector<TimerLoc> timers;
    
    // Filled during phase 3.
    // Same size as samples.
    vector<vector<Linkage>> links;
    vector<vector<XLoc>> xlocs1, xlocs2;
    // Merged versions of the above, filled in phase 3.
    // These are the best guesses at the player location(s).
    vector<Linkage> rescored_locs1, rescored_locs2;

    // Filled during phase 4.
    vector<vector<LivesLoc>> llocs1, llocs2;
    // Final rescored guesses for player lives/health.
    vector<LivesLoc> lives1, lives2;
    
    One(const Game &game,
	const vector<pair<uint8, uint8>> &movie) :
      game(game),
      movie(movie),
      nmarkov(Inputs1P(movie), 3),
      autotimer(game.romfile, nmarkov),
      autocamera(game.romfile),
      autolives(game.romfile, nmarkov) {

      timerlocs.resize(NUM_SAMPLES);
      
      links.resize(NUM_SAMPLES);
      xlocs1.resize(NUM_SAMPLES);
      xlocs2.resize(NUM_SAMPLES);

      llocs1.resize(NUM_SAMPLES);
      llocs2.resize(NUM_SAMPLES);
    }

    // Protects just the status stuff so that the
    // progress indicator can be rendered in another
    // thread.
    void SetStatus(RunState st, const char *msg) {
      MutexLock ml(&status_m);
      state = st;
      message = msg;
    }

    std::mutex status_m;
    // Number of players complete for each sample.
    vector<int> progress;
    const char *message = "init";
    RunState state = RunState::WAITING;
  };
  vector<One *> games;

  std::mutex table_m;
  void ShowTable() {
    MutexLock ml(&table_m);
    for (int i = 0; i < games.size() + 2; i++) {
      printf("%s", ANSI_PREVLINE);
    }
    printf("\n---------------------------------\n");
    for (int i = 0; i < games.size(); i++) {
      One *one = games[i];
      MutexLock gml(&one->status_m);
      printf("[%s] ", StateChar(one->state));
      switch (one->state) {
      case RunState::RUNNING_AT:
      case RunState::RUNNING_AC:
      case RunState::RUNNING_AL:
	printf("[");
	for (int s = 0; s < one->progress.size(); s++) {
	  char c = '?';
	  switch (one->progress[s]) {
	  default: break;
	  case -1: c = ' '; break;
	  case 0:  c = '_'; break;
	  case 1:  c = '.'; break;
	  case 2:  c = ':'; break;
	  }
	  printf("%c", c);
	}
	printf("] ");
	break;
      default:;
      }
      printf(ANSI_WHITE "%s" ANSI_RESET ": %s" ANSI_CLEARTOEOL "\n",
	     one->game.romfile.c_str(),
	     one->message);
    }
  }

  // Stringify an address, marking it if there's something known
  // about that spot.
  static const char *AddrInfo(const One *one, int loc) {
    // Prevent this from accidentally matching an "unknown" loc.
    if (loc == -1) return "";
    if (loc == one->game.p1.xloc) return "p1 x";
    else if (loc == one->game.p1.yloc) return "p1 y";
    else if (loc == one->game.p1.lives) return "p1 l";
    else if (loc == one->game.p1.health) return "p1 h";
    else if (loc == one->game.p2.xloc) return "p2 x";
    else if (loc == one->game.p2.yloc) return "p2 y";
    else if (loc == one->game.p2.lives) return "p2 l";
    else if (loc == one->game.p2.health) return "p2 h";
    else return "";
  }
  
  // Phase one: Initialize and get the samples so that
  // we can run over them in parallel.
  void StartGames(const vector<Game> &game) {
    games = ParallelMap
      (game,
       [this](const Game &game) {
	 vector<pair<uint8, uint8>> movie =
	   SimpleFM7::ReadInputs2P(game.moviefile);
	 CHECK(!movie.empty()) << game.romfile;
	 One *one = new One{game, movie};

	 CHECK(one->movie.size() >
	       WARMUP_FRAMES + (SAMPLE_EVERY * NUM_SAMPLES)) <<
	   game.moviefile << " not long enough!";

	 unique_ptr<Emulator> emu;
	 emu.reset(Emulator::Create(game.romfile));
	 CHECK(emu.get() != nullptr) << game.romfile;

	 one->SetStatus(RunState::START, "Warming up");
	 int frameidx = 0;
	 for (int i = WARMUP_FRAMES; i--;) {
	   emu->Step(one->movie[frameidx].first,
		     one->movie[frameidx].second);
	   frameidx++;
	 }

	 one->SetStatus(RunState::START, "Get samples");
	 one->samples.reserve(NUM_SAMPLES);
	 while (frameidx < one->movie.size() &&
		one->samples.size() < NUM_SAMPLES) {
	   if (frameidx % SAMPLE_EVERY == 0) {
	     one->samples.push_back(emu->SaveUncompressed());
	   }
	   emu->StepFull(one->movie[frameidx].first,
			 one->movie[frameidx].second);
	   frameidx++;
	 }

	 one->SetStatus(RunState::START, "Initialized");
	 return one;
       }, MAX_CONCURRENCY);
  }

  void DoAutoTimer() {
    for (One *one : games) {
      one->SetStatus(RunState::RUNNING_AT, "AutoTimer");
      {
	MutexLock ml(&one->status_m);
	one->progress.resize(NUM_SAMPLES);
	for (int &j : one->progress) j = -1;
      };
    }
    
    ShowTable();
    
    ParallelComp2D(
	games.size(),
	NUM_SAMPLES,
	[this](int game, int sample) {
	  One *one = games[game];

	  {
	    MutexLock ml(&one->status_m);
	    if (one->progress[sample] == -1)
	      one->progress[sample] = 0;
	  }
	  
	  const vector<uint8> &save = one->samples[sample];
	  one->timerlocs[sample] = one->autotimer.FindTimers(save);

	  {
	    MutexLock ml(&one->status_m);
	    one->progress[sample]++;
	  }
	  ShowTable();
	}, MAX_CONCURRENCY);

    // Merge and rescore.
    ParallelComp(
	games.size(),
	[this](int game) {
	  One *one = games[game];
	  one->SetStatus(RunState::RUNNING_AT, "Merge");
	  one->timers = AutoTimer::MergeTimers(one->timerlocs);

	  one->SetStatus(RunState::RUNNING_AT, "Write results");
	  
	  auto MarkAddr =
	    [one](int loc) -> string {
	      if (loc == -1) return "-1";
	      const char *info = AddrInfo(one, loc);
	      if (info[0] == '\0') {
		return StringPrintf("%04x", loc);
	      } else {
		return StringPrintf("[%s %04x]", info, loc);
	      }
	    };
	  

	  {
	    MutexLock ml(&file_mutex);
	    if (outfile) {
	      fprintf(outfile, "[%s]: Timers:\n",
		      one->game.romfile.c_str());
      
	      for (const TimerLoc &l : one->timers) {
		string xs = one->game.timer == l.loc ? " TIMER" : "";

		fprintf(outfile,
			"  %.2f: %d %s @%.2f = %s%s\n",
			l.score, l.loc, l.incrementing ? "++" : "--",
			l.period,
			MarkAddr(l.loc).c_str(),
			xs.c_str());
	      }
	    
	      fflush(outfile);
	    }
	  }

	  one->SetStatus(RunState::RUNNING_AT, "AutoTimer done");
	  
	  ShowTable();
	}, std::min(4, MAX_CONCURRENCY));
  }
  
  void DoAutoCamera() {
    for (One *one : games) {
      one->SetStatus(RunState::RUNNING_AC, "AutoCamera");
      {
	MutexLock ml(&one->status_m);
	one->progress.resize(NUM_SAMPLES);
	for (int &j : one->progress) j = -1;
      };
    }
    
    ShowTable();
    
    ParallelComp3D(
	games.size(),
	NUM_SAMPLES,
	// Players
	2,
	[this](int game, int sample, int player) {
	  One *one = games[game];
	  // Just skip if it's a one-player game.
	  if (player == 1 && !one->game.is_two_player)
	    return;

	  {
	    MutexLock ml(&one->status_m);
	    if (one->progress[sample] == -1)
	      one->progress[sample] = 0;
	  }
	  
	  const vector<uint8> &save = one->samples[sample];
	  // PERF: All three of these can be done in parallel, actually.
	  if (player == 0) {
	    one->links[sample] =
	      one->autocamera.FindLinkages(save, nullptr);
	    one->xlocs1[sample] =
	      one->autocamera.FindXLocs(save, false, nullptr);
	  } else {
	    one->xlocs2[sample] =
	      one->autocamera.FindXLocs(save, true, nullptr);
	  }

	  {
	    MutexLock ml(&one->status_m);
	    one->progress[sample]++;
	  }
	  ShowTable();
	}, MAX_CONCURRENCY);

    // Merge and rescore.
    ParallelComp(
	games.size(),
	[this](int game) {
	  One *one = games[game];
	  one->SetStatus(RunState::RUNNING_AC, "Merge");
	  vector<Linkage> merged = AutoCamera2::MergeLinkages(one->links);
	  vector<XLoc> xmerged1 = AutoCamera2::MergeXLocs(one->xlocs1);
	  vector<XLoc> xmerged2 = AutoCamera2::MergeXLocs(one->xlocs2);

	  std::unordered_map<int, float> xscores1, xscores2;
	  for (const XLoc &x : xmerged1) xscores1[x.xloc] = x.score;
	  for (const XLoc &x : xmerged2) xscores2[x.xloc] = x.score;

	  // Now rescore using xlocs for each player.
	  vector<Linkage> rescored1 = merged, rescored2 = merged;
	  for (Linkage &l : rescored1) l.score *= (1.0f + xscores1[l.xloc]);
	  for (Linkage &l : rescored2) l.score *= (1.0f + xscores2[l.xloc]);
	  // Easy way to sort them again.
	  one->rescored_locs1 = AutoCamera2::MergeLinkages({rescored1});
	  one->rescored_locs2 = AutoCamera2::MergeLinkages({rescored2});

	  one->SetStatus(RunState::RUNNING_AC, "Write results");
	  
	  auto MarkAddr =
	    [one](int loc) -> string {
	      if (loc == -1) return "-1";
	      const char *info = AddrInfo(one, loc);
	      if (info[0] == '\0') {
		return StringPrintf("%04x", loc);
	      } else {
		return StringPrintf("[%s %04x]", info, loc);
	      }
	    };
	  
	  // Now compute stats.
	  {   
	    // Perhaps pretty silly to have all the games
	    // trying to simultaneously write to the file...
	    MutexLock ml(&file_mutex);
	    if (outfile) {
	      fprintf(outfile, "[%s]: Rescored linkages:\n",
		      one->game.romfile.c_str());
      
	      auto PrintPlayer =
		[this, &xscores1, &xscores2, &MarkAddr](
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
			    MarkAddr(l.xloc).c_str(),
			    MarkAddr(l.yloc).c_str(),
			    xs1.c_str(),
			    xs2.c_str());
		  }
		};

	      if (one->game.is_two_player) {
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

	  one->SetStatus(RunState::RUNNING_AC, "AutoCamera done");
	  
	  ShowTable();
	}, std::min(4, MAX_CONCURRENCY));
  }

  void DoAutoLives() {
    for (One *one : games) {
      one->SetStatus(RunState::RUNNING_AL, "AutoLives");
      {
	MutexLock ml(&one->status_m);
	one->progress.resize(NUM_SAMPLES);
	for (int &j : one->progress) j = -1;
      };
    }
    
    ShowTable();
    
    ParallelComp3D(
	games.size(),
	NUM_SAMPLES,
	// Players
	2,
	[this](int game, int sample, int player) {
	  One *one = games[game];
	  // Just skip if it's a one-player game.
	  if (player == 1 && !one->game.is_two_player)
	    return;

	  {
	    MutexLock ml(&one->status_m);
	    if (one->progress[sample] == -1)
	      one->progress[sample] = 0;
	  }
	  
	  // Now, autolives for each player.
	  auto DoLives =
	    [one, sample](const vector<Linkage> &rescored,
			  bool player_two,
			  vector<LivesLoc> *out) {
	      if (rescored.empty())
		return;

	      const vector<uint8> &save = one->samples[sample];
	      const int xloc = rescored[0].xloc, yloc = rescored[0].yloc;

	      *out = one->autolives.FindLives(save, xloc, yloc, player_two);
	    };

	  if (player == 0) {
	    DoLives(one->rescored_locs1, false, &one->llocs1[sample]);
	  } else {
	    DoLives(one->rescored_locs2, false, &one->llocs2[sample]);
	  }

	  {
	    MutexLock ml(&one->status_m);
	    one->progress[sample]++;
	  }

	  ShowTable();
	}, MAX_CONCURRENCY);

    ShowTable();

    ParallelComp(
	games.size(),
	[this](int game) {
	  One *one = games[game];
	  const Game &g = one->game;
	  one->SetStatus(RunState::RUNNING_AL, "Merging");
	  one->lives1 = AutoLives::MergeLives(one->llocs1);
	  one->lives2 = AutoLives::MergeLives(one->llocs2);

	  if (!one->lives1.empty() || !one->lives2.empty()) {
	    one->SetStatus(RunState::RUNNING_AL, "Writing");
	    MutexLock ml(&file_mutex);
	    if (outfile) {
	      fprintf(outfile, "[%s]: Merged lives:\n",
		      g.romfile.c_str());
	      
	      auto PLives =
		[this, one](const Player &player,
				  const vector<LivesLoc> &lives) {
		  for (const LivesLoc &l : lives) {
		    // XXX annotate if correct for player
		    fprintf(outfile,
			    "  %.3f: %d = %04x  %s\n",
			    l.score, l.loc, l.loc, AddrInfo(one, l.loc));
		  }
		};
	      
	      if (g.is_two_player) {
		fprintf(outfile, "  == P1 ==\n");
		PLives(g.p1, one->lives1);
		fprintf(outfile, "  == P2 ==\n");
		PLives(g.p2, one->lives2);
	      } else {
		PLives(g.p1, one->lives1);
	      }
	      
	      fflush(outfile);
	    }
	  }

	  one->SetStatus(RunState::RUNNING_AL, "AutoLives done");
	}, std::min(4, MAX_CONCURRENCY));
  }

  void ComputeStats() {
    ParallelComp(
	games.size(),
	[this](int game) {
	  One *one = games[game];
	  one->SetStatus(RunState::STATS, "Computing stats");
    
	  auto StatsPlayer =
	  [one](const Player &player,
		const vector<Linkage> &rescored,
		const vector<LivesLoc> &lives) {
	    Stats *stats = &one->stats;
	    if (player.xloc != -1 &&
		player.yloc != -1) {
	      stats->locs_known++;

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

	      stats->locs_found += found;
	      stats->locs_rank_loss += rank_loss;
	    }

	    if (player.health != -1 ||
		player.lives != -1) {

	      if (player.health != -1)
		stats->lives_known++;
	      if (player.lives != -1)
		stats->lives_known++;

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

	      stats->lives_found += found;
	      stats->lives_rank_loss += rank_loss;
	    }
	  };

	StatsPlayer(one->game.p1, one->rescored_locs1, one->lives1);
	StatsPlayer(one->game.p2, one->rescored_locs2, one->lives2);

	}, MAX_CONCURRENCY);

    ShowTable();
  }
  
  // xxx ugh
  std::mutex file_mutex;
  FILE *outfile = nullptr;

  ~Evaluation() {
    fclose(outfile);
    for (One *one : games) delete one;
  }
};

static void EvalAll() {
  // or getmatching...
  vector<Game> games = GameDB().GetAll();
  // vector<Game> games = GameDB().GetMatching(
  // {"mario", "contra", "werewolf"});


  Evaluation evaluation;
  evaluation.StartGames(games);
  evaluation.DoAutoTimer();
  evaluation.DoAutoCamera();
  evaluation.DoAutoLives();
  evaluation.ComputeStats();

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
  
  struct EvalStats {
    int has_data = 0;
    int perfect = 0;
    int any = 0;
    int total_loss = 0;
  };
    
  auto Stats3 =
    [](int found, int known, int loss, EvalStats *stats) {
      if (known > 0) {
	stats->has_data++;
	if (found == known) stats->perfect++;
	if (found > 0) stats->any++;
	stats->total_loss += loss;
      }
    };

  EvalStats locs_stats, lives_stats;
  for (const Evaluation::One *g : evaluation.games) {
    const Stats &s = g->stats;
    Stats3(s.locs_found, s.locs_known, s.locs_rank_loss, &locs_stats);
    Stats3(s.lives_found, s.lives_known, s.lives_rank_loss, &lives_stats);
  }
  
  printf(" == Summary ==\n");
  string col1h = Util::Pad(24, "game.nes");
  printf("%sp recall\tp rank loss\tl recall\t rank loss\n", col1h.c_str());
  for (const Evaluation::One *g : evaluation.games) {
    const Stats &s = g->stats;
    printf("%s"
	   // positions
	   "%s%d/%d" ANSI_RESET "\t%d\t"
	   // lives
	   "%s%d/%d" ANSI_RESET "\t%d\n",
	   Util::Pad(24, g->game.romfile).c_str(),
	   Color3(s.locs_found, s.locs_known, s.locs_rank_loss),
	   s.locs_found, s.locs_known,
	   s.locs_rank_loss,

	   Color3(s.lives_found, s.lives_known, s.lives_rank_loss),
	   s.lives_found, s.lives_known,
	   s.lives_rank_loss);
  }

  printf("\n"
	 "  Locs: %d/%d perfect. %d any. %d total loss\n"
	 " Lives: %d/%d perfect. %d any. %d total loss\n",
	 locs_stats.perfect, locs_stats.has_data,
	 locs_stats.any, locs_stats.total_loss,
	 lives_stats.perfect, lives_stats.has_data,
	 lives_stats.any, lives_stats.total_loss);
}

int main(int argc, char *argv[]) {
  (void)Rtos;

  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }

  // Turn on ANSI support in Windows 10+. (Otherwise, use ANSICON etc.)
  // https://docs.microsoft.com/en-us/windows/console/setconsolemode
  //
  // TODO: This works but seems to subsequently break control keys
  // and stuff like that in cygwin bash?
  HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
  // mingw headers may not know about this new flag
  static constexpr int kVirtualTerminalProcessing = 0x0004;
  DWORD old_mode = 0;
  GetConsoleMode(hStdOut, &old_mode);
  // printf("%lld\n", old_mode);
  SetConsoleMode(hStdOut, old_mode | kVirtualTerminalProcessing);
  #endif

  EvalAll();
  return 0;
}
