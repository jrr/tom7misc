
#include "chess.h"

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

#include "gamestats.h"

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


void ReadStats(const string &filename, Stats *stat_buckets) {
  ifstream s;
  s.open(filename);
  for (int b = 0; b < NUM_BUCKETS; b++) {
    Stats *stats = &stat_buckets[b];
    int64 num_games;
    CHECK(s >> num_games);
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

  // One idea for drawing chess pieces in HTML is to use
  // a "black" king drawn in white behind a white king (in black or
  // grey) to provide the outline. It looks better than the hollow
  // "white" pieces on a colored background.
  // <span style="position:absolute; color:#fff" class="white piece">&#9818;</span>
  // <span style="position:absolute; color:#333" class="white piece">&#9812;</span>
  
  
  {
    FILE *f = fopen("report.html", "w");

    fprintf(f, "<!doctype html>\n"
	    "<link href=\"report.css\" rel=\"stylesheet\" type=\"text/css\">\n");

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
    const Position startpos;
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

      const bool black = p < 16;
      const int pidx = black ? p : 48 + (p - 16);
      const int prow = pidx / 8;
      const int pcol = pidx % 8;
      const Position startpos;
      const char *ent = Position::HTMLEntity(startpos.PieceAt(prow, pcol));
      
      fprintf(f, "<h2>%s (%s)</h2>\n", ent, PIECE_NAME[p]);

      fprintf(f, "<table class=board>\n");
      for (int r = 0; r < 8; r++) {

	auto HalfRow =
	  [f, &GetRank, r](const Ratio *fate_on, const char *fateclass,
			   const char *borderno,
			   const vector<double> &ranks,
			   std::function<string(double)> MakeBG) {
	    fprintf(f, "<tr>");
	    for (int c = 0; c < 8; c++) {
	      const int idx = r * 8 + c;
	      const bool light = ((r + c) & 1) == 0;
	      const Ratio &fate = fate_on[idx];
	      const double rank = GetRank(ranks, fate.Mean());
	      string bg = MakeBG(rank);
	      fprintf(f, " <td style=\"border-%s:0;background:%s\" class=%s>",
		      borderno,
		      bg.c_str(),
		      light ? "blt" : "bdk");
	      fprintf(f,
		      "<span class=\"%s bigp\">%.2f%%</span><br>"
		      "<span class=\"%s smallp\">%.2f&ndash;%.2f%%</span>",
		      fateclass, 
		      fate.Mean() * 100.0,
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
