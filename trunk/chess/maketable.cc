
#include "chess.h"

#include <cmath>
#include <stdio.h>
#include <unistd.h>

#include <iostream>
#include <fstream>
#include <string>
#include <deque>
#include <shared_mutex>
#include <thread>
#include <vector>
#include <utility>

#include "base/stringprintf.h"
#include "base/logging.h"
#include "pgn.h"
#include "util.h"
#include "city.h"
#include "textsvg.h"

#include "gamestats.h"

#include <type_traits>

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

static constexpr const char *const PIECE_NAME[32] = {
  "a8 rook",
  "b8 knight",
  "c8 bishop",
  "d8 queen",
  "e8 king",
  "f8 bishop",
  "g8 knight",
  "h8 rook",
  "a7 pawn", "b7 pawn", "c7 pawn", "d7 pawn",
  "e7 pawn", "f7 pawn", "g7 pawn", "h7 pawn",
  // white
  "a2 pawn", "b2 pawn", "c2 pawn", "d2 pawn",
  "e2 pawn", "f2 pawn", "g2 pawn", "h2 pawn",
  "a1 rook",
  "b1 knight",
  "c1 bishop",
  "d1 queen",
  "e1 king",
  "f1 bishop",
  "g1 knight",
  "h1 rook", };

// For makeperms.cc
static constexpr const char *const CONSTANT_NAME[32] = {
  "BLACK_ROOK_A",
  "BLACK_KNIGHT_B",
  "BLACK_BISHOP_C",
  "BLACK_QUEEN",
  "BLACK_KING",
  "BLACK_BISHOP_F",
  "BLACK_KNIGHT_G",
  "BLACK_ROOK_H",
  "BLACK_PAWN_A",
  "BLACK_PAWN_B",
  "BLACK_PAWN_C",
  "BLACK_PAWN_D",
  "BLACK_PAWN_E",
  "BLACK_PAWN_F",
  "BLACK_PAWN_G",
  "BLACK_PAWN_H",
  "WHITE_PAWN_A",
  "WHITE_PAWN_B",
  "WHITE_PAWN_C",
  "WHITE_PAWN_D",
  "WHITE_PAWN_E",
  "WHITE_PAWN_F",
  "WHITE_PAWN_G",
  "WHITE_PAWN_H",
  "WHITE_ROOK_A",
  "WHITE_KNIGHT_B",
  "WHITE_BISHOP_C",
  "WHITE_QUEEN",
  "WHITE_KING",
  "WHITE_BISHOP_F",
  "WHITE_KNIGHT_G",
  "WHITE_ROOK_H", };

// Take a piece number 0-31 and return its Position-style piece.
static uint8 PiecePiece(int p) {
  static const Position startpos;
  const bool black = p < 16;
  const int pidx = black ? p : 48 + (p - 16);
  const int prow = pidx / 8;
  const int pcol = pidx % 8;
  return startpos.PieceAt(prow, pcol);
}

static string PieceCol(int p) {
  return StringPrintf("%c", 'a' + (p % 8));
}

void ReadStats(const string &filename, Stats *stat_buckets) {
  ifstream s;
  s.open(filename);
  for (int b = 0; b < NUM_BUCKETS; b++) {
    Stats *stats = &stat_buckets[b];
    int64 num_games;
    CHECK(s >> num_games) << filename;
    stats->num_games += num_games;
    for (int p = 0; p < 32; p++) {
      int64 on;
      for (int j = 0; j < 64; j++) {
	CHECK(s >> on);
	stats->pieces[p].died_on[j] += on;
      }
      for (int j = 0; j < 64; j++) {
	CHECK(s >> on);
	stats->pieces[p].survived_on[j] += on;
      }
    }
  }
}

struct Sum {
  int64 values[NUM_BUCKETS] = {};
  int64 Total() const {
    int64 ret = 0LL;
    for (int i = 0; i < NUM_BUCKETS; i++) ret += values[i];
    return ret;
  }

  int64 Min() const {
    int64 ret = values[0];
    for (int i = 1; i < NUM_BUCKETS; i++) ret = std::min(ret, values[i]);
    return ret;
  }

  int64 Max() const {
    int64 ret = values[0];
    for (int i = 1; i < NUM_BUCKETS; i++) ret = std::max(ret, values[i]);
    return ret;
  }
};

struct Ratio {
  int64 numer[NUM_BUCKETS] = {};
  int64 denom[NUM_BUCKETS] = {};

  int64 Numer() const {
    int64 n = 0LL;
    for (int i = 0; i < NUM_BUCKETS; i++) {
      n += numer[i];
    }
    return n;
  }

  int64 Denom() const {
    int64 d = 0LL;
    for (int i = 0; i < NUM_BUCKETS; i++) {
      d += denom[i];
    }
    return d;
  }

  double Mean() const {
    int64 n = 0LL;
    int64 d = 0LL;
    for (int i = 0; i < NUM_BUCKETS; i++) {
      n += numer[i];
      d += denom[i];
    }
    return n / (double)d;
  }

  double Min() const {
    // Could throw out non-finite values...
    double r = numer[0] / (double)denom[0];
    for (int i = 0; i < NUM_BUCKETS; i++) {
      double t = numer[i] / (double)denom[i];
      r = std::min(r, t);
    }
    return r;
  }

  double Max() const {
    // Could throw out non-finite values...
    double r = numer[0] / (double)denom[0];
    for (int i = 0; i < NUM_BUCKETS; i++) {
      double t = numer[i] / (double)denom[i];
      r = std::max(r, t);
    }
    return r;
  }

};

// Collection of rectangles.
struct RectSet {
  struct Rect {
    int x0 = 0, y0 = 0;
    // Note: This code is not careful about whether the right/bottom
    // edge is included in the rectangle.
    int x1 = 0, y1 = 0;
  };

  void Add(const Rect &a) {
    rects.push_back(a);
  }

  static bool Overlap(const Rect &a, const Rect &b) {
    if (a.x0 > b.x1 || a.x1 < b.x0)
      return false;
    if (a.y0 > b.y1 || a.y1 < b.y0)
      return false;
    return true;
  }

  // Find an x position for the rectangle that doesn't
  // overlap anything in the set.
  Rect PlaceHoriz(int y, int w, int h) const {
    // PERF could of course do some sort of search.
    Rect r;
    r.y0 = y;
    r.y1 = y + h;
    for (int x = 0; /* returns in loop */; x++) {
      r.x0 = x;
      r.x1 = x + w;
      // fprintf(stderr, "Try %d,%d-%d,%d\n", r.x0, r.y0, r.x1, r.y1);
      if (!AnyOverlap(r))
	return r;
    }
  }

  bool AnyOverlap(const Rect &r) const {
    for (const Rect &other : rects)
      if (Overlap(r, other))
	return true;
    return false;
  }

  // PERF: Could of course use a sorted data structure! But for
  // uses here we're talking about like 32-64 rectangles.
  vector<Rect> rects;
};


// SVG version. This one shows the total rate of a piece ending
// on each square, as well as its survival odds (given that it's
// there).
void GenFateMap(Stats *stat_buckets) {
  for (int p = 0; p < 32; p++) {
    FILE *f = fopen(StringPrintf("piece%d.svg", p).c_str(), "wb");
    Ratio ended_on[64] = {}, died_on[64] = {}, survived_on[64] = {};

    // For each square index, its rank in terms of the
    // overall probability of the piece's fate ending there.
    vector<double> ranks, sranks;
    for (int i = 0; i < 64; i++) {
      ranks.push_back(0.0);
      sranks.push_back(0.0);
    }
    
    for (int b = 0; b < NUM_BUCKETS; b++) {
      const Stats &stats = stat_buckets[b];
      for (int i = 0; i < 64; i++) {
	died_on[i].denom[b] = stats.num_games;
	survived_on[i].denom[b] = stats.num_games;
	ended_on[i].denom[b] = stats.num_games;
	
	died_on[i].numer[b] = stats.pieces[p].died_on[i];
	survived_on[i].numer[b] = stats.pieces[p].survived_on[i];
	ended_on[i].numer[b] = stats.pieces[p].died_on[i] +
	  stats.pieces[p].survived_on[i];
      }
    }

    for (int i = 0; i < 64; i++) {
      ranks[i] = ended_on[i].Numer();
      if (ended_on[i].Numer() > 0) {
	sranks[i] = survived_on[i].Numer() /
	  (double)ended_on[i].Numer();
      }
    }
    
    std::sort(ranks.begin(), ranks.end());
    std::sort(sranks.begin(), sranks.end());
    printf("Num sranks: %d\n", (int)sranks.size());
    
    // PERF: These can be done with binary search, obviously
    auto GetRankRevIdx = 
      [](const vector<double> &ranks, double value) {
	int rank = 0;
	for (int i = ranks.size() - 1; i >= 0; i--) {
	  if (value >= ranks[i]) return rank;
	  rank++;
	}
	return rank;
      };
    
    auto GetRank =
      [](const vector<double> &ranks, double value) {
	for (int i = 0; i < ranks.size(); i++)
	  if (value <= ranks[i]) return i / (double)ranks.size();
	return 1.0;
      };

    constexpr double WIDTH = 800.0, HEIGHT = 800.0;
    constexpr double MARGIN = 10.0;
    constexpr double SQUARE = (WIDTH - (MARGIN * 2.0)) / 8.0;
    fprintf(f, "%s", TextSVG::Header(WIDTH, HEIGHT).c_str());
    
    // fprintf(f, "<h2>%s (%s)</h2>\n", ent, PIECE_NAME[p]);

    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {
	const int idx = r * 8 + c;
	const Ratio &fate = ended_on[idx];
	// sqrt(sqrt(fate.Mean())) looks okay, but I think rank is most
	// interesting-looking.
	const double rank = GetRank(ranks, fate.Numer());

	const string fill = StringPrintf("#%02x%02x%02x",
					 (int)((1.0 - rank) * 255),
					 (int)((1.0 - rank) * 255),
					 (int)((1.0 - rank) * 255));
	
	fprintf(f, "<rect x=\"%s\" y=\"%s\" "
		"width=\"%s\" height=\"%s\" fill=\"%s\" />\n",
		TextSVG::Rtos(MARGIN + c * SQUARE).c_str(),
		TextSVG::Rtos(MARGIN + r * SQUARE).c_str(),
		TextSVG::Rtos(SQUARE).c_str(), TextSVG::Rtos(SQUARE).c_str(),
		fill.c_str());

	if (fate.Numer() > 0) {
	  // Survival rate is number of survivals over number of
	  // times it ended there.
	  const double survival_rate = survived_on[idx].Numer() /
	    (double)fate.Numer();
	  const string color = rank > 0.5 ? "#fff" : "#000";

	  // const bool near_best = survival_rate > 0.5;
	  int survival_rank = GetRankRevIdx(sranks, survival_rate);
	  // printf("Survival rank: %d\n", survival_rank);
	  const bool near_best = survival_rank < 4;
	  
	  const int permil = (int)(survival_rate * 1000.0);
	  const int fpart = permil % 10;
	  const int ipart = permil / 10;

	  const double single_fudge = ipart < 10 ? 24 : 0;
	  
	  fprintf(f, "%s%s\n",
		  TextSVG::Text(
		      MARGIN + c * SQUARE + (SQUARE * 0.15) + single_fudge,
		      MARGIN + r * SQUARE + (SQUARE * 0.65),
		      "sans-serif",
		      // Would be nice to center a single digit
		      44.0, {{color, StringPrintf("%d", ipart)}}).c_str(),
		  TextSVG::Text(
		      MARGIN + c * SQUARE + (SQUARE * 0.15) + 48,
		      MARGIN + r * SQUARE + (SQUARE * 0.65) - 18,
		      "sans-serif",
		      20.0, {{color, StringPrintf(".%d", fpart)}}).c_str());

	  if (near_best) {
	    fprintf(
		f, "<line x1=\"%s\" y1=\"%s\" "
		"x2=\"%s\" y2=\"%s\" stroke-width=\"3\" stroke=\"%s\" />\n",
		TextSVG::Rtos(MARGIN + c * SQUARE + (SQUARE * 0.15)).c_str(),
		TextSVG::Rtos(MARGIN + r * SQUARE + (SQUARE * 0.85)).c_str(),
		TextSVG::Rtos(MARGIN + c * SQUARE + (SQUARE * 0.85)).c_str(),
		TextSVG::Rtos(MARGIN + r * SQUARE + (SQUARE * 0.85)).c_str(),
		color.c_str());
	  }
	}
      }
    }
    fprintf(f, "%s", TextSVG::Footer().c_str());
    fclose(f);
  }
  fprintf(stderr, "Wrote 32 SVG files.\n");
}

// Generate fate-data.cc, which contains the aggregate probabilities
// for each piece/square/liveness combination. This allows us to
// compute the probability for a final state.
void GenCC(Stats *stat_buckets) {
  FILE *f = fopen("fate-data.cc", "wb");
  fprintf(f, "// Generated by maketable.cc. Do not edit!\n\n");

  fprintf(f, "#include \"fate-data.h\"\n\n");
  
  fprintf(f,
	  "LivedDied fate_data[32][64] = {\n");
	  
  for (int p = 0; p < 32; p++) {
    fprintf(f, "  // %s\n"
	    "  {", PIECE_NAME[p]);

    for (int i = 0; i < 64; i++) {
      int64 died_on = 0LL, survived_on = 0LL, denom = 0LL;
      for (int b = 0; b < NUM_BUCKETS; b++) {
	const Stats &stats = stat_buckets[b];
	denom += stats.num_games;
	died_on += stats.pieces[p].died_on[i];
	survived_on += stats.pieces[p].survived_on[i];
      }
      fprintf(f, "{%.17g,%.17g}, ",
	      (double)survived_on / (double)denom,
	      (double)died_on / (double)denom);
    }
    fprintf(f, "},\n");
  }
  fprintf(f, "};\n");
  fclose(f);
  fprintf(stderr, "Wrote fate-data.cc\n");
}


void GenReport(Stats *stat_buckets) {
#if 0
  for (int bucket = 0; bucket < NUM_BUCKETS; bucket++) {
    const Stats &s = stat_buckets[bucket];
    printf("%lld\n", s.num_games);
    for (int i = 0; i < 32; i++) {
      const PieceStats &p = s.pieces[i];
      for (int d = 0; d < 64; d++)
	printf(" %lld", p.died_on[d]);
      printf("\n ");
      for (int d = 0; d < 64; d++)
	printf(" %lld", p.survived_on[d]);
      printf("\n");
    }
  }
#endif

  const Position startpos;

  Sum games;
  // For the 32 pieces.
  Ratio survived[32] = {};

  for (int b = 0; b < NUM_BUCKETS; b++) {
    const Stats &stats = stat_buckets[b];
    games.values[b] += stats.num_games;
    for (int p = 0; p < 32; p++) {
      int64 total_survived = 0LL;
      for (int j = 0; j < 64; j++)
	total_survived += stats.pieces[p].survived_on[j];
      survived[p].denom[b] += stats.num_games;
      survived[p].numer[b] += total_survived;
    }
  }

  std::vector<int> pieces;
  for (int i = 0; i < 32; i++) pieces.push_back(i);
  std::sort(pieces.begin(), pieces.end(),
	    [&survived](int a, int b) {
	      return survived[a].Mean() < survived[b].Mean();
	    });

  for (int p : pieces) {
    // TODO: Compute "confidence interval" here.
    printf("%d (%s). %lld/%lld %.3f (%.3f--%.3f)\n",
	   p, PIECE_NAME[p],
	   survived[p].Numer(), survived[p].Denom(),
	   survived[p].Mean(),
	   survived[p].Min(),
	   survived[p].Max());
  }


  fprintf(stderr, "piece-survival.svg...\n");
  fflush(stderr);
  {
    constexpr double WIDTH = 800.0, HEIGHT = 1280.0;
    constexpr double MARGIN = 10.0;
    FILE *f = fopen("piece-survival.svg", "wb");
    fprintf(f, "%s", TextSVG::Header(WIDTH, HEIGHT).c_str());


    // Just keep a set of used rectangles. Place the piece at
    // "the first" x position such that it does not overlap any
    // existing rectangle. This has bad asymptotic complexity,
    // but there are only 32 pieces!
    RectSet used;

    for (int i = 0; i < pieces.size(); i++) {
      auto UseCol =
	[](int x) {
	  const int p = PiecePiece(x);
	  switch (p & Position::TYPE_MASK) {
	  case Position::QUEEN:
	  case Position::KING:
	    return false;
	  default:
	    return true;
	  }
	};
      auto PieceName =
	[&UseCol](int x) {
	  const int p = PiecePiece(x);
	  const string ent = Position::HTMLEntity(p);
	  const string col = PieceCol(x);
	  if (UseCol(x)) {
	    return ent + col;
	  } else {
	    return ent;
	  }
	};

      const int p = pieces[i];

      auto PtoY = [](double p) { return (1.0 - p) * HEIGHT; };

      // XXX should take into account the Min/Max!
      const int h = 18;
      const int pct_width = 50;
      const int label_width = UseCol(p) ? 35 : 25;
      const int w = pct_width + label_width;
      const int y = PtoY(survived[p].Min()) - 6;

      RectSet::Rect pos = used.PlaceHoriz(y, w, h);
      used.Add(pos);
      double x = MARGIN + pos.x0 + 3.0;
      /*
      fprintf(f, "<rect fill=\"none\" stroke=\"#000\" x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" />\n",
	      pos.x0 + MARGIN, pos.y0, w, h);
      */

      fprintf(f, "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" "
	      "stroke=\"black\" />\n",
	      x, PtoY(survived[p].Min()),
	      x, PtoY(survived[p].Max()));
      double pmean = survived[p].Mean();
      double cy = PtoY(pmean);
      fprintf(f, "<circle cx=\"%.2f\" cy=\"%.2f\" r=\"1.5\" />\n",
	      x, cy);

      fprintf(f, "%s\n",
	      TextSVG::Text(x + 5.0, cy + 3.2, "sans-serif",
			    18.0, {{"#000", PieceName(p)}}).c_str());

      fprintf(f, "%s\n",
	      TextSVG::Text(x + label_width, cy + 3.2, "sans-serif",
			    9.0, {{"#777",
				   StringPrintf("%.1f%%", pmean * 100.0)
				   }}).c_str());

      #if 0
      // TODO: Compute "confidence interval" here.
      printf("%d (%s). %lld/%lld %.3f (%.3f--%.3f)\n",
	     p, PIECE_NAME[p],
	     survived[p].Numer(), survived[p].Denom(),
	     survived[p].Mean(),
	     survived[p].Min(),
	     survived[p].Max());
      #endif
    }

    fprintf(f, "%s", TextSVG::Footer().c_str());
    fclose(f);
  }

  GenFateMap(stat_buckets);
  GenCC(stat_buckets);
  
  // One idea for drawing chess pieces in HTML is to use
  // a "black" king drawn in white behind a white king (in black or
  // grey) to provide the outline. It looks better than the hollow
  // "white" pieces on a colored background.
  // <span style="position:absolute; color:#fff" class="white piece">&#9818;</span>
  // <span style="position:absolute; color:#333" class="white piece">&#9812;</span>


  {
    FILE *f = fopen("report-probs.cc", "wb");
    
    vector<pair<int, float>> ranks;
    for (int p : pieces) {
      ranks.emplace_back(p, survived[p].Mean());
    }
    std::sort(ranks.begin(), ranks.end(),
	      [](const pair<int, float> &a,
		 const pair<int, float> &b) {
		return a.second > b.second;
	      });

    fprintf(f, "vector<pair<int, float>> probs = {\n");
    for (const auto &row : ranks) {
      fprintf(f, "  {%s, %.9g},\n",
	      CONSTANT_NAME[row.first],
	      row.second);
    }
    fprintf(f, "};\n");
    
    fclose(f);
  }
  
  {
    FILE *f = fopen("report.html", "wb");

    fprintf(f, "<!doctype html>\n"
	    "<link href=\"report.css\" rel=\"stylesheet\" "
	    "type=\"text/css\">\n");

    fprintf(f, "<h1>Overall chances survival (%lld games)</h1>\n"
	    "<table>"
	    "<tr><td>rank</td><td>piece</td> <td>survived</td> "
	    "<td>p(survived)</td> <td>lb</td><td>ub</td></tr>\n",
	    games.Total());
    for (int p : pieces) {
      fprintf(f,
	      "<td>%d</td> <td>%s</td> <td>%lld</td> "
	      "<td>%.4f</td> <td>%.4f</td> <td>%.4f</td></tr>\n",
	      p, PIECE_NAME[p],
	      survived[p].Numer(),
	      survived[p].Mean(),
	      survived[p].Min(),
	      survived[p].Max());
    }
    fprintf(f, "</table>\n");

    fprintf(f, "<h1>Survival board</h1>\n");

    fprintf(f, "<table class=board>\n");
    for (int r = 0; r < 8; r++) {
      fprintf(f, "<tr>");
      for (int c = 0; c < 8; c++) {
	const int idx = r * 8 + c;
	const bool light = ((r + c) & 1) == 0;
	fprintf(f, " <td class=%s>", light ? "lt" : "dk");

	if (idx < 16 || idx >= 48) {
	  const bool black = idx < 16;
	  const int pidx = black ? idx : (16 + (idx - 48));
	  // const char piecechar =
	  // Position::HumanPieceChar(startpos.PieceAt(r, c));
	  const char *ent = Position::HTMLEntity(startpos.PieceAt(r, c) |
						 Position::BLACK);
	  const Ratio &surv = survived[pidx];
	  fprintf(f,
		  "<span class=\"%s piece\">%s</span><br>"
	          "<span class=bigp>%.2f%%</span><br>"
		  "<span class=smallp>%.2f&ndash;%.2f%%</span>",
		  (black ? "black" : "white"), ent,
		  surv.Mean() * 100.0,
		  surv.Min() * 100.0,
		  surv.Max() * 100.0);
	} else {
	  fprintf(f, "&nbsp;");
	}

	fprintf(f, "</td>\n");
      }
      fprintf(f, "</tr>\n");
    }
    fprintf(f, "</table>");


    int64 min_all = 9999999999;
    int64 min_not_pawn = 999999999;
    fprintf(f, "<h1>Fate per piece</h1>\n");
    for (int p = 0; p < 32; p++) {
      Ratio died_on[64] = {}, survived_on[64] = {};

      vector<double> dranks, sranks;

      for (int b = 0; b < NUM_BUCKETS; b++) {
	const Stats &stats = stat_buckets[b];
	for (int i = 0; i < 64; i++) {
	  died_on[i].denom[b] = stats.num_games;
	  survived_on[i].denom[b] = stats.num_games;

	  died_on[i].numer[b] = stats.pieces[p].died_on[i];
	  survived_on[i].numer[b] = stats.pieces[p].survived_on[i];

	  dranks.push_back(died_on[i].Mean());
	  sranks.push_back(survived_on[i].Mean());
	}
      }

      std::sort(dranks.begin(), dranks.end());
      std::sort(sranks.begin(), sranks.end());
      auto GetRank =
	[](const vector<double> &ranks, double value) {
	  // PERF can be done with binary search, obv...
	  for (int i = 0; i < ranks.size(); i++) {
	    if (value <= ranks[i]) return i / (double)ranks.size();
	  }
	  return 1.0;
	};

      // XXX PiecePiece?
      const bool black = p < 16;
      const int pidx = black ? p : 48 + (p - 16);
      const int prow = pidx / 8;
      const int pcol = pidx % 8;
      const Position startpos;
      const uint8 piece = startpos.PieceAt(prow, pcol);
      const char *ent = Position::HTMLEntity(piece);

      fprintf(f, "<h2>%s (%s)</h2>\n", ent, PIECE_NAME[p]);

      fprintf(f, "<table class=board>\n");
      for (int r = 0; r < 8; r++) {

	auto HalfRow =
	  [f, &min_all, &min_not_pawn, piece, &GetRank, r](
	      const Ratio *fate_on, const char *fateclass,
	      const char *borderno,
	      const vector<double> &ranks,
	      std::function<string(double)> MakeBG) {
	    fprintf(f, "<tr>");
	    for (int c = 0; c < 8; c++) {
	      const int idx = r * 8 + c;
	      const bool light = ((r + c) & 1) == 0;
	      const Ratio &fate = fate_on[idx];
	      const double rank = GetRank(ranks, fate.Mean());
	      const string bg = MakeBG(rank);
	      const bool zero = fate.Mean() < 0.0001;

	      if (fate.Numer() != 0) {
		min_all = std::min(min_all, fate.Numer());
		if ((piece & Position::TYPE_MASK) != Position::PAWN) {
		  min_not_pawn = std::min(min_not_pawn, fate.Numer());
		}
	      }
	      
	      string big = zero ? StringPrintf("%lld%s", fate.Numer(),
					       fate.Numer() < 2000 ?
					       "###" : ""
					       ) :
		StringPrintf("%.2f%%", fate.Mean() * 100.0);
					      
	      fprintf(f, " <td style=\"border-%s:0;background:%s\" class=%s>",
		      borderno,
		      bg.c_str(),
		      light ? "blt" : "bdk");
	      fprintf(f,
		      "<span class=\"%s bigp\">%s</span><br>"
		      "<span class=\"%s smallp\">%.2f&ndash;%.2f%%</span>",
		      fateclass,
		      big.c_str(),
		      fateclass,
		      fate.Min() * 100.0,
		      fate.Max() * 100.0);
	      fprintf(f, "</td>\n");
	    }
	    fprintf(f, "</tr>\n");
	  };

	HalfRow(died_on, "died", "bottom", dranks,
		[](double r) {
		  return StringPrintf("rgb(255.0,%.2f,%.2f)",
				      255.0 * (1.0 - r),
				      255.0 * (1.0 - r));
		});
	HalfRow(survived_on, "surv", "top", sranks,
		[](double r) {
		  return StringPrintf("rgb(%.2f,255.0,%.2f)",
				      255.0 * (1.0 - r),
				      255.0 * (1.0 - r));
		});
      }
      fprintf(f, "</table>\n");
    }
    fprintf(stderr, "Rarest nonzero: %lld\nNot pawn: %lld", min_all,
	    min_not_pawn);
    fclose(f);
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "rungames.exe stats1.txt stats2.txt ...\n");
    return -1;
  }

  Stats stats[NUM_BUCKETS] = {};

  for (int i = 1; i < argc; i++) {
    fprintf(stderr, "Reading %s...\n", argv[i]);
    ReadStats(argv[i], stats);
  }

  GenReport(stats);
  return 0;
}
