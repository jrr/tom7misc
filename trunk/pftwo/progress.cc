
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
#include "../cc-lib/textsvg.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"
#include "../cc-lib/bounds.h"
#include "../cc-lib/re2/re2.h"

// Save state periodically so that rewind is fast.
#define SNAPSHOT_EVERY 1000

std::mutex print_mutex;
#define Printf(fmt, ...) do {			\
    MutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);			\
  } while (0)

static string Rtos(double d) {
  if (std::isnan(d)) return "NaN";
  char out[16];
  sprintf(out, "%.5f", d);
  char *o = out;
  while (*o == '0') o++;
  return string{o};
}

struct Series {
  // As pairs of x,y; should be in nondecreasing x order.
  vector<pair<double, double>> points;
};

struct Plots {
  string moviename;
  Series depth_pos;
  Series mframes_pos;
  Series walltime_pos;
};

// TODO HERE:
static Plots OneMovie(const string &game,
		      const string &moviename) {
  vector<pair<int, string>> subs;
  vector<pair<uint8, uint8>> movie =
    SimpleFM2::ReadInputsEx(moviename, &subs);
  CHECK(!movie.empty()) << "Couldn't read movie: " << moviename; 
  unique_ptr<Emulator> emu{Emulator::Create(game)};
  CHECK(emu.get() != nullptr) << game;

  Plots ret;
  ret.moviename = moviename;

  RE2 subtitle_re{"f +([0-9]+) +s +([0-9]+)"};
  
  int subidx = 0;
  for (int frame = 0; frame < movie.size(); frame++) {
    uint8 p1, p2;
    std::tie(p1, p2) = movie[frame];
    emu->Step(p1, p2);
    vector<uint8> mem = emu->GetMemory();
    
    // Note: Contra-specific. Could come from .objectives file?
    // XXX using full 256 bytes for these makes really big jumps...
    uint8 stage = mem[48];
    uint8 room = mem[100];
    uint8 scroll = mem[101];
    double pos = (int)scroll + 256 * (int)room + 65536 * (int)stage;
    ret.depth_pos.points.push_back({(double)frame, pos});

    if (subidx < subs.size() && subs[subidx].first == frame) {
      int64 nes_frames, secs;
      if (RE2::PartialMatch(subs[subidx].second, subtitle_re,
			    &nes_frames, &secs)) {
	ret.mframes_pos.points.push_back({(double)nes_frames, pos});
	ret.walltime_pos.points.push_back({(double)secs, pos});
      }
      subidx++;
    }
    
    if (frame % 10000 == 0) {
      Printf("%s %d/%d\n", moviename.c_str(), frame, (int)movie.size());
    }
  }
  return ret;
}

static vector<string> &Colors() {
  static vector<string> &c = *new vector<string>{
    "#000",
    "#700",
    "#070",
    "#007",
    "#770",
    "#707",
    "#077",
    "#777",
  };
  return c;
}

template<class Get>
static void DrawPlots(const vector<Plots> &plotses,
		      double width, double height,
		      const string &filename,
		      const Get &get) {
  FILE *f = fopen(filename.c_str(), "w");
  fprintf(f, "%s", TextSVG::Header(width, height).c_str());

  // First get bounds for everything.
  Bounds bounds;
  for (const Plots &plots : plotses) {
    const Series &series = get(plots);
    for (const std::pair<double, double> pt : series.points) {
      bounds.Bound(pt);
    }
  }

  bounds.AddMarginFrac(0.05);
  Bounds::Scaler scaler = bounds.Stretch(width, height); /* .FlipY(); */
  // Now output the lines
  int coloridx = 0;
  for (const Plots &plots : plotses) {
    const Series &series = get(plots);
    if (!series.points.empty()) {
      const string &color = Colors()[coloridx];
      fprintf(f, "<polyline stroke-linejoin=\"round\" "
	      "fill=\"none\" stroke=\"%s\" stroke-opacity=\"0.75\" "
	      "stroke-width=\"0.75\" points=\"",
	      color.c_str());
      for (const std::pair<double, double> pt : series.points) {
	double x, y;
	std::tie(x, y) = scaler.Scale(pt);
	fprintf(f, "%s,%s ", Rtos(x).c_str(), Rtos(y).c_str());
      }
      fprintf(f, "\"/>\n");
    }
    coloridx ++;
    coloridx %= Colors().size();
  }

  // Captions on top.
  coloridx = 0;
  for (const Plots &plots : plotses) {
    const Series &series = get(plots);
    if (!series.points.empty()) {
      const string &color = Colors()[coloridx];
      double x, y;
      std::tie(x, y) = scaler.Scale(series.points[series.points.size() - 1]);
      fprintf(f, TextSVG::Text(x, y, "sans-serif", 12.0,
			       {{color, plots.moviename}}).c_str());
    }
    coloridx ++;
    coloridx %= Colors().size();
  }
  
  fprintf(f, "%s", TextSVG::Footer().c_str());
}

#define GAME "contra.nes"

// The main loop for SDL.
int main(int argc, char *argv[]) {
  (void)Rtos;
  
  if (argc < 3) {
    fprintf(stderr, "playback.exe game.nes movie1.fm2 movie2.fmt ...\n");
    return -1;
  }
  string game = argv[1];

  vector<string> movies;
  for (int i = 2; i < argc; i++) {
    movies.push_back(argv[i]);
  }
  
  if (game != GAME) {
    fprintf(stderr, "Sorry, " GAME " is hardcoded because of AOT.\n");
    return -1;
  }

  vector<Plots> plotses = ParallelMap(movies,
				      [&game](const string &moviename) {
					return OneMovie(game, moviename);
				      }, 10);

  // Now collate and write plots.
  DrawPlots(plotses, 1024, 1024, "depth-pos.svg",
	    [](const Plots &p) { return p.depth_pos; });

  DrawPlots(plotses, 1024, 1024, "mframes-pos.svg",
	    [](const Plots &p) { return p.mframes_pos; });

  DrawPlots(plotses, 1024, 1024, "walltime-pos.svg",
	    [](const Plots &p) { return p.walltime_pos; });

  return 0;
}
