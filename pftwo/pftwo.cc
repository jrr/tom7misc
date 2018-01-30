
// TODO: Try to deduce that a state is hopeless and stop expanding
// it. -or- try to estimate the maximum reachable score from each
// node using some Bayesian method.
//
// TODO: Super Meat Brothers-style multi future visualization of tree.
//
// TODO: Always expand nodes 100 times or whatever?
//
// TODO: Serialization of tree state and restarting
//
// TODO: Find values we don't want to go down by setting them to 1 or
// zero as they go down (or something), and testing whether that is
// really bad.
//
// TODO: Be scientific! Build an ground truth dataset (e.g.
// hand-written objectives that are trustworthy) and an offline
// pipeline that can measure performance on that set, for tuning
// hyperparameters and seeing whether ideas actually pan out.
//
// TODO: Players love moving to the right because the x coordinate
// is part of the objective function. We could perhaps eliminate
// this by detecting bytes that appear to be the player's coordinates?
// Or subtracting the x coordinate from such bytes during the
// exploration phase?
//
// TODO: min-max heap would allow us a very cheap "1000 best nodes"
// representation. (started this in cc-lib)
//
// TODO: Rather than choosing random goals, choose goals that aren't
// already represented in the grid, and perhaps choose ones that are
// adjacent to covered cells.
//
// TODO: Smooth the markov matrix by making any input (that was
// actually executed) possible in any state, with the minimal
// probability mass. This probably also solves the problem where
// we get out of gamut.
//
// TODO: Grid keys for palette entries, sprites on screen, bg values?
// This would help for bosses where there's some visual feedback on
// damage, and would be easier than trying to fuzz all of ram
// I bet the bosses that are most stuck are actually BG graphics.
//
// TODO: Just some way to ensure that the highest-scoring node
// continually gets random play in parallel with the other search?
// This might result in worse (long) movies, but sometimes that's
// the right way for it to beat some spot (like e.g. you have to
// just survive some screen, or many contra bosses where you have
// to deal a bunch of damage but it doesn't know it's making
// progress...)
//
// TODO: When on the ice pillars level, regular tree search is
// sometimes backing up all the way to the waterfall climb -- this
// seems nuts (it's like almost 200k depth away). We may be able to
// improve MFrame efficiency by limiting the score loss in
// FindGoodNode or whatever.
//
// TODO: Right after a score breakthrough (i.e., plateau for a while,
// then we discover a higher score), try replaying the sequence of
// moves we just made on random states from the plateau region, with
// the idea that we can sometimes cut out a lot of dithering. Not
// clear what we do in cases where the players are farming low-value
// points (e.g. score) while waiting, though, because the resulting
// states would be actually worse than the original breakthrough
// (lower score).
//
// TODO: Make headless version for running in CLOUDS.
//
// TODO: When stuck, I notice a lot of the grid cells have the player
// dead(?) or at least almost dead. These have very high "BAD" scores,
// and sometimes "seq 0" (meaning I guess that it couldn't make any
// moves before dying). When a grid cell is dead (all expansions
// die?), consider popping to a previous--like, store a heap in here.
//
// TODO: More generally, when stuck: Prune nodes for which we've only
// ever expanded to deaths?
//
// TODO: "Marathon" strategy where we just try to stay alive as
// long as we can?

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

#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/randutil.h"

#include "atom7ic.h"

#include "weighted-objectives.h"

#include "SDL.h"
#include "graphics.h"

#include "problem-twoplayer.h"

#include "treesearch.h"
#include "dumptree.h"

// Screen dimensions.
#define WIDTH 1920
#define HEIGHT 1080

// XXX move to library?
static std::mutex print_mutex;
#define Printf(fmt, ...) do {			\
    MutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);			\
  } while (0)

// Note: This is not actually a std::thread -- it's really the main
// thread. SDL really doesn't like being called outside the main
// thread, even if it's exclusive.
struct UIThread {
  explicit UIThread(TreeSearch *search) : search(search) {}

  void Loop() {
    SDL_Surface *surf = sdlutil::makesurface(256, 256, true);
    SDL_Surface *surfhalf = sdlutil::makesurface(128, 128, true);
    (void)frame;
    (void)surf;
    (void)surfhalf;

    const int num_workers = [&]() {
      MutexLock ml(&search->tree_m);
      return search->WorkersWithLock().size();
    }();
    vector<vector<uint8>> screenshots;
    screenshots.resize(num_workers);
    for (auto &v : screenshots) v.resize(256 * 256 * 4);

    const int64 start = time(nullptr);
    
    for (;;) {
      frame++;
      SDL_Event event;
      
      if (SDL_PollEvent(&event)) {
	switch (event.type) {
	case SDL_QUIT:
	  return;
	case SDL_KEYDOWN:
	  switch (event.key.keysym.sym) {

	  case SDLK_ESCAPE:
	    return;

	  case SDLK_t:
	    TreeDumping::DumpTree(search);
	    break;
	  default:
	    break;
	  }
	  break;
	default:
	  break;
	}
      }
	
      SDL_Delay(1000.0 / 30.0);

      const int64 now = time(nullptr);
      search->SetApproximateSeconds(now - start);
      
      // Every ten thousand frames, write FM2 file.
      // TODO: Superimpose all of the trees at once.
      if (frame % 10000 == 0) {
	string filename_part = StringPrintf("frame-%lld", frame);
	string filename = search->SaveBestMovie(filename_part);
	(void)Util::remove("latest.fm2");
	if (!Util::copy(filename, "latest.fm2")) {
	  printf("Couldn't copy to latest.fm2?\n");
	}
      }

      sdlutil::clearsurface(screen, 0x11111111);

      const int64 tree_size =
	ReadWithLock(&search->tree_m, &search->tree->num_nodes);

      const double improvement_pct =
	(search->stats.sequences_improved.Get() * 100.0) /
	(double)search->stats.sequences_improved_denom.Get();
      font->draw(
	  1, 0,
	  StringPrintf(
	      "Frame %d! "
	      "%lld tree nodes  "
	      "%d collisions  "
	      "%.3f%% improvement rate  "
	      "%d/%d explore deaths",
	      frame,
	      tree_size,
	      search->stats.same_expansion.Get(),
	      improvement_pct,
	      search->stats.explore_deaths.Get(),
	      search->stats.explore_iters.Get()));

      int64 total_steps = 0LL;
      {
	MutexLock ml(&search->tree_m);
	vector<Worker *> workers = search->WorkersWithLock();
	for (int i = 0; i < workers.size(); i++) {
	  const Worker *w = workers[i];
	  int numer = w->numer.load(std::memory_order_relaxed);
	  font->draw(256 * 6 + 10, 40 + FONTHEIGHT * i,
		     StringPrintf("[%d] %d/%d %s",
				  i,
				  numer,
				  w->denom.load(std::memory_order_relaxed),
				  w->status.load(std::memory_order_relaxed)));
	  total_steps += w->nes_frames.load(std::memory_order_relaxed);
	}
      }
	
      // Save the sum to a single value for benchmarking.
      search->SetApproximateNesFrames(total_steps);
      
      bool queue_mode = false;
      {
	MutexLock ml(&search->tree_m);
	// Update all screenshots.
	if (!search->tree->explore_queue.empty()) {
	  queue_mode = true;
	  static constexpr int STARTY = 128 + FONTHEIGHT + FONTHEIGHT;
	  // Draw queue!
	  // XXX Ugh, there's no good way to visualize in a queue-centric
	  // way, because we need a worker in order to compute a screenshot.
	  font->draw(30, STARTY,
		     StringPrintf("^2EXPLORE QUEUE SIZE: %d",
				  search->tree->explore_queue.size()));

	  int ypos = STARTY + FONTHEIGHT + 2;;
	  for (const Tree::ExploreNode &en : search->tree->explore_queue) {
	    if (ypos > HEIGHT - FONTHEIGHT - 1) break;
	    font->draw(30, ypos,
		       StringPrintf("Goal ^3%d^<,^3%d^<  "
				    "distance ^4%.2f^<  "
				    "seq ^5%d^<   ^1%d^<|^4%d^<  "
				    "bad ^2%d^<",
				    // XXX hard-coded TPP
				    en.goal.goalx,
				    en.goal.goaly,
				    en.distance,
				    (int)en.closest_seq.size(),
				    en.iterations_left,
				    en.iterations_in_progress,
				    en.bad));
	    ypos += FONTHEIGHT;
	  }

	}
      }

      {
	MutexLock ml(&search->tree_m);
	static constexpr int GRIDX = 768;
	static constexpr int GRIDY = 450;
	static constexpr int CELLPX = 16;
	
	// Get best score in grid so that we can normalize
	// colors against it.
	double cutoff_bestscore =
	  -search->tree->heap.GetMinimum().priority * GRID_BESTSCORE_FRAC;
	double bestscore = 0.0;
	for (const Tree::GridCell &gc : search->tree->grid)
	  if (gc.score > bestscore) bestscore = gc.score;
	const double one_over_bestscore =
	  bestscore > 0.0 ? 1.0 / bestscore : 0.0;
	
	// Draw grid.
	// XXX this specific to the current TwoPlayerProblem
	// TODO! Save little pictures for each grid cell; it'd
	// look awesome!
	for (int cy = 0; cy < Problem::GRID_CELLS_H; cy++) {
	  for (int cx = 0; cx < Problem::GRID_CELLS_W; cx++) {
	    const Tree::GridCell &gc =
	      search->tree->grid[cy * Problem::GRID_CELLS_W + cx];
	    if (gc.node == nullptr) {
	      sdlutil::FillRectRGB(screen,
				   GRIDX + cx * CELLPX,
				   GRIDY + cy * CELLPX,
				   CELLPX, CELLPX,
				   0x20, 0x0, 0x0);
	    } else {
	      uint8 rr = gc.score >= cutoff_bestscore ? 0 : 1;
	      int gg = 0x20 +
		(0xFF - 0x20) * (gc.score * one_over_bestscore);
	      if (gg > 0xFF) gg = 0xff;
	      sdlutil::FillRectRGB(screen,
				   GRIDX + cx * CELLPX,
				   GRIDY + cy * CELLPX,
				   CELLPX, CELLPX,
				   rr * gg, (uint8)gg, 0x0);
	    }
	  }
	}
	smallfont->draw(GRIDX, GRIDY,
			StringPrintf("best ^3%.4f^< / ^2%.4f",
				     bestscore,
				     cutoff_bestscore));
      }
      
      // Draw workers workin'.
      vector<vector<string>> texts;
      {
	MutexLock ml(&search->tree_m);
	vector<Worker *> workers = search->WorkersWithLock();
	texts.resize(workers.size());
	for (int i = 0; i < workers.size(); i++) {
	  Worker *w = workers[i];
	  // XXX if workers change size, then we won't
	  // necessarily have room for a screenshot
	  w->Visualize(nullptr, &screenshots[i]);
	  w->VizText(nullptr, &texts[i]);
	}
      }

      CHECK_EQ(texts.size(), screenshots.size());
      for (int i = 0; i < texts.size(); i++) {
	int x = i % 12;
	int y = i / 12;
	BlitARGBHalf(screenshots[i], 256, 256,
		     x * 128, y * 128 + 20, screen);

	if (!queue_mode) {
	  for (int t = 0; t < texts[i].size(); t++) {
	    font->draw(x * 128,
		       (y + 1) * 128 + 20 + t * FONTHEIGHT,
		       texts[i][t]);
	  }
	}
      }
	
	
      // Gather tree statistics.
      struct LevelStats {
	int count = 0;
	int max_chosen = 0;
	int64 chosen = 0;
	int workers = 0;
	double best_score = 0.0;
      };
      vector<LevelStats> levels;

      struct TreeStats {
	int nodes = 0;
	int leaves = 0;
	int64 nodebytes = 0LL;
      };
      TreeStats treestats;

      vector<double> all_scores;
      // Note that there can be a race here, but it's just a
      // performance hint.
      all_scores.reserve(tree_size);

      std::function<void(int, const Tree::Node *)> GetLevelStats =
	[this, &levels, &treestats, &all_scores, &GetLevelStats](
	    int depth, const Tree::Node *n) {
	while (depth >= levels.size())
	  levels.push_back(LevelStats{});

	treestats.nodes++;
	treestats.nodebytes += Problem::StateBytes(n->state);
	treestats.nodebytes += sizeof (Tree::Node) - sizeof (Problem::State);

	LevelStats *ls = &levels[depth];
	ls->count++;
	ls->chosen += n->chosen;
	if (n->chosen > ls->max_chosen)
	  ls->max_chosen = n->chosen;

	ls->workers += n->num_workers_using;

	CHECK(n->location != -1);

	double score = -search->tree->heap.GetCell(n).priority;
	all_scores.push_back(score);
	if (score > ls->best_score) ls->best_score = score;
	// TODO save node so that we can draw picture?

	if (n->children.empty())
	  treestats.leaves++;

	for (const auto &child : n->children) {
	  GetLevelStats(depth + 1, child.second);
	}
      };

      int max_nodes = 0, max_depth = 0;
      {
	MutexLock ml(&search->tree_m);
	GetLevelStats(0, search->tree->root);

	max_depth = search->tree->max_depth;
	max_nodes = search->tree->MaxNodes();
      }

      // Average state size:
      // treestats.statebytes / (1024.0 * treestats.nodes)
      smallfont->draw(256 * 6 + 10, 220,
		      StringPrintf("^3%d^</^2%d^< nodes, ^3%d^< leaves, "
				   "^3%d^< depth, "
				   "^3%lld^< MB ",
				   treestats.nodes,
				   max_nodes,
				   treestats.leaves,
				   max_depth,
				   treestats.nodebytes / (1024LL * 1024LL)));

      {
	// Could compute this from font height; depends on number of workers
	// too...
	const int MAX_TREE_LINES = 100;
	// Before this, all lines are drawn.
	int prefix = levels.size();
	// After this, all lines are drawn.
	int suffix = 0;
	// Index at which we show "..."
	int dots_index = -1;
	if (levels.size() > MAX_TREE_LINES) {
	  prefix = MAX_TREE_LINES / 2;
	  dots_index = prefix + 1;
	  suffix = (levels.size() - 1) - (MAX_TREE_LINES - MAX_TREE_LINES / 2);
	}
	int yy = 230;
	for (int i = 0; i < levels.size(); i++) {
	  if (i < prefix || i >= suffix) {
	    string w;
	    if (levels[i].workers) {
	      for (int j = 0; j < levels[i].workers; j++)
		w += '*';
	    }

	    smallfont->draw(256 * 6 + 10, yy,
			    StringPrintf("^4%2d^<: ^3%d^< n %lld c %d mc, "
					 "%.3f ^5%s",
					 i, levels[i].count,
					 levels[i].chosen,
					 levels[i].max_chosen,
					 levels[i].best_score,
					 w.c_str()));
	    yy += SMALLFONTHEIGHT;
	  } else if (i == dots_index) {
	    // XXX could show workers within this region?
	    smallfont->draw(256 * 6 + 10, yy, "    ... ");
	    yy += SMALLFONTHEIGHT;
	  }
	}
      }
      
      static constexpr int BOTTOM = HEIGHT - FONTHEIGHT - 3;

      {
	int64 total_sec = now - start;
	int64 sec = total_sec;
	int64 min = sec / 60;
	sec %= 60;
	int64 hrs = min / 60;
	min %= 60;

	font->draw(10, HEIGHT - FONTHEIGHT,
		   StringPrintf("%.2f NES Mframes in %d^2:^<%02d^2:^<%02d "
				"= %.2f NES kFPS "
				"%.2f UI FPS",
				total_steps / 1000000.0, hrs, min, sec,
				(total_steps / (double)total_sec) / 1000.0,
				frame / (double)total_sec));
      }
      
      // XXX maybe show the grid cells in the histo
      std::sort(all_scores.rbegin(), all_scores.rend());
      static constexpr int HISTO_HEIGHT = 128;
      sdlutil::slock(screen);
      for (int x = 0; x < all_scores.size() && x < WIDTH; x++) {
	double score = all_scores[x];
	bool over = false, under = false;
	(void)under;
	if (score > 1.0) {
	  score = 1.0;
	  over = true;
	} else if (score < 0) {
	  score = 0;
	  under = true;
	}
	double heightf = score * HISTO_HEIGHT;
	int height = heightf;
	// Fractional pixel for anti-aliasing
	uint8 leftover = 255.0 * (heightf - height);
	for (int h = 0; h < height; h++) {
	  int y = BOTTOM - h;
	  sdlutil::drawclippixel(screen, x, y,
				 0xFF, 0xFF, over ? 0xFF : 0x00);
	}
	sdlutil::drawclippixel(screen, x, BOTTOM - height,
			       leftover, leftover, 0x00);

      }
      sdlutil::sulock(screen);

      maxfont->draw(
	  10, HEIGHT - HISTO_HEIGHT / 2 - MAXFONTHEIGHT / 2,
	  StringPrintf("^2%.2f%%", search->tree->stuckness));
      
      SDL_Flip(screen);
    }

  }

  void Run() {
    screen = sdlutil::makescreen(WIDTH, HEIGHT);
    CHECK(screen);

    font = Font::create(screen,
			"font.png",
			FONTCHARS,
			FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
    CHECK(font != nullptr) << "Couldn't load font.";

    smallfont = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(smallfont != nullptr) << "Couldn't load smallfont.";

    smallfont = Font::create(screen,
			     "fontsmall.png",
			     FONTCHARS,
			     SMALLFONTWIDTH, SMALLFONTHEIGHT,
			     FONTSTYLES, 0, 3);
    CHECK(smallfont != nullptr) << "Couldn't load smallfont.";

    maxfont = Font::create(screen,
			   "fontmax.png",
			   FONTCHARS,
			   MAXFONTWIDTH, MAXFONTHEIGHT, FONTSTYLES, 4, 3);
    CHECK(maxfont != nullptr) << "Couldn't load maxfont.";
    
    Loop();
    Printf("UI shutdown.\n");
  }

  ~UIThread() {
    // XXX free screen
    delete font;
    delete smallfont;
    delete maxfont;
  }

 private:
  static constexpr int FONTWIDTH = 9;
  static constexpr int FONTHEIGHT = 16;
  static constexpr int SMALLFONTWIDTH = 6;
  static constexpr int SMALLFONTHEIGHT = 6;
  static constexpr int MAXFONTHEIGHT = 48 * 2;
  static constexpr int MAXFONTWIDTH = 27 * 2;

  Font *font = nullptr, *smallfont = nullptr, *maxfont = nullptr;

  ArcFour rc{"ui_thread"};
  TreeSearch *search = nullptr;
  SDL_Surface *screen = nullptr;
  int64 frame = 0LL;
};

// The main loop for SDL.
int main(int argc, char *argv[]) {
  fprintf(stderr, "Init SDL\n");

  #ifdef __MINGW32__
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }
  #endif

  /* Initialize SDL. */
  CHECK(SDL_Init(SDL_INIT_VIDEO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_Surface *icon = SDL_LoadBMP("pftwo-icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }
  
  {
    TreeSearch::Options options;
    TreeSearch search{options};
    search.StartThreads();
    {
      UIThread *ui_thread = new UIThread(&search);
      ui_thread->Run();
      delete ui_thread;
    }
    search.DestroyThreads();
  }
  
  SDL_Quit();
  return 0;
}
