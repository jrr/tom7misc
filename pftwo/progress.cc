
#include <algorithm>
#include <vector>
#include <string>
#include <set>
#include <memory>
#include <list>

#include <cstdio>
#include <cstdlib>

#include "pftwo.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"
#include "../cc-lib/util.h"
#include "../cc-lib/textsvg.h"
#include "../cc-lib/bounds.h"
#include "../cc-lib/re2/re2.h"

static std::mutex print_mutex;
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
  string caption;
  Series depth_pos;
  Series mframes_pos;
  Series walltime_pos;
};

static Plots OneMovie(const string &game,
		      const string &moviename,
		      const string &shared_prefix) {
  vector<pair<int, string>> subs;
  vector<pair<uint8, uint8>> movie =
    SimpleFM2::ReadInputsEx(moviename, &subs);
  CHECK(!movie.empty()) << "Couldn't read movie: " << moviename; 
  unique_ptr<Emulator> emu{Emulator::Create(game)};
  CHECK(emu.get() != nullptr) << game;

  Plots ret;
  ret.caption =
    moviename.substr(shared_prefix.size(), string::npos);
  // XXX more principled?
  if (Util::endswith(ret.caption, ".fm2"))
    ret.caption = ret.caption.substr(0, ret.caption.size() - 4);
  
  RE2 subtitle_re{"f +([0-9]+) +s +([0-9]+)"};

  uint8 max_stage = 0, max_room = 0, max_scroll = 0;
  set<uint8> rooms;

  // The room parameter only ranges from 0-21, or 0xFF. Map this
  // to use more of the dynamic range.
  auto MapRoom = [](uint8 r) -> int {
    if (r == 255) return 255;
    else return (int)((r * 255.0) / 21.0);
  };
  
  int subidx = 0;
  for (int frame = 0; frame < movie.size(); frame++) {
    uint8 p1, p2;
    std::tie(p1, p2) = movie[frame];
    emu->Step(p1, p2);
    vector<uint8> mem = emu->GetMemory();
    
    // Note: Contra-specific. Could come from .objectives file?
    // But note also the manual mapping with MapRoom.
    uint8 stage = mem[48];
    uint8 room = mem[100];
    uint8 scroll = mem[101];
    max_stage = std::max(stage, max_stage);
    max_room = std::max(room, max_room);
    max_scroll = std::max(scroll, max_scroll);
    rooms.insert(room);
    double pos = (int)scroll + 256 * MapRoom(room) + 65536 * (int)stage;
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

  // printf("Maxima: Stage: %d Room: %d Scroll: %d\n",
  // (int)max_stage, (int)max_room, (int)max_scroll);
  // for (uint8 r : rooms)
  // printf(" %d\n", r);
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
	      "stroke-width=\"1.5\" points=\"",
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
			       {{color, plots.caption}}).c_str());
    }
    coloridx ++;
    coloridx %= Colors().size();
  }
  
  fprintf(f, "%s", TextSVG::Footer().c_str());
}

#define GAME "contra.nes"

// TODO: Strip suffix too
string SharedPrefix(const vector<string> &v) {
  if (v.empty()) return "";
  const string &first = v[0];
  auto AllShared = [&v, &first](int len) {
    for (int x = 1; x < v.size(); x++) {
      const string &s = v[x];
      if (s.size() < len) return false;
      for (int i = 0; i < len; i++) {
	if (s[i] != first[i]) return false;
      }
    }
    return true;
  };

  int best = 0;
  for (int i = 0; i < first.size(); i++) {
    if (first[i] == '.' ||
	first[i] == '-') {
      printf("first[%d] = '%c'\n", i, first[i]);
      if (AllShared(i + 1)) {
	printf("Allshared.\n");
	best = i + 1;
      } else {
	break;
      }
    }
  }
  return first.substr(0, best);
}

static void WriteTSV(vector<Plots> plotses,
		     const string &filename) {

  RE2 numeric_re{"([0-9]+.?[0-9]*)"};

  const bool all_numeric = [&]{
    for (const Plots &a : plotses)
      if (!RE2::FullMatch(a.caption, numeric_re))
	return false;
    return true;
  }();

  if (all_numeric) {
    std::sort(plotses.begin(), plotses.end(),
	      [&](const Plots &a, const Plots &b) {
		double af = 0.0, bf = 0.0;
		// PERF obviously, parsing over and over is wasteful
		CHECK(RE2::FullMatch(a.caption, numeric_re, &af));
		CHECK(RE2::FullMatch(a.caption, numeric_re, &bf));
		return af < bf;
	      });
  }
    
  FILE *f = fopen(filename.c_str(), "w");

  // This normalization is problematic, because if we get stuck before
  // a hard part and then don't ever improve upon that best node, the
  // slope can look good despite being worse than other nodes. See
  // discussion/example in posterity/frames-experiment.
  auto LastSlope = [](const Series &s) -> double {
    if (s.points.empty()) return 0.0;
    return s.points.back().second / s.points.back().first;
  };
  (void)LastSlope;

  // Instead just use the last value; assume each experiment executed
  // the same number of frames.
  auto LastValue = [](const Series &s) -> double {
    if (s.points.empty()) return 0.0;
    return s.points.back().second;
  };
  (void)LastValue;
  
  fprintf(f, "expt\tmframes\tdepth\twalltime\n");
  for (const Plots &plots : plotses) {
    fprintf(f, "%s\t%.3f\t%.3f\t%.3f\n",
	    plots.caption.c_str(),
	    LastValue(plots.mframes_pos),
	    LastValue(plots.depth_pos),
	    LastValue(plots.walltime_pos));
  }
  fclose(f);
}

int main(int argc, char *argv[]) {
  (void)Rtos;
  
  if (argc < 3) {
    fprintf(stderr, "progress.exe game.nes movie1.fm2 movie2.fm2 ...\n");
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
  
  const string shared_prefix = SharedPrefix(movies);
  printf("Prefix shared by all movies (stripped): %s\n",
	 shared_prefix.c_str());
  
  vector<Plots> plotses =
    ParallelMap(movies,
		[&game, &shared_prefix](const string &moviename) {
		  return OneMovie(game, moviename, shared_prefix);
		}, 10);

  // Now collate and write plots.
  DrawPlots(plotses, 1024, 1024, "depth-pos.svg",
	    [](const Plots &p) { return p.depth_pos; });

  DrawPlots(plotses, 1024, 1024, "mframes-pos.svg",
	    [](const Plots &p) { return p.mframes_pos; });

  DrawPlots(plotses, 1024, 1024, "walltime-pos.svg",
	    [](const Plots &p) { return p.walltime_pos; });

  WriteTSV(plotses, "plots.tsv");
  
  return 0;
}
