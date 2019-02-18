
// Generates SVG tables comparing several rankings of pieces.
// This one does not try to spatially represent the survival
// probabilities (which are optional), rather, it just ranks them
// and shows the difference between consecutive rankings with
// lines.

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
#include "../cc-lib/randutil.h"
#include "chess.h"
#include "pgn.h"
#include "util.h"
#include "textsvg.h"

#include <type_traits>

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

enum {
      BLACK_ROOK_A,
      BLACK_KNIGHT_B,
      BLACK_BISHOP_C,
      BLACK_QUEEN,
      BLACK_KING,
      BLACK_BISHOP_F,
      BLACK_KNIGHT_G,
      BLACK_ROOK_H,
      BLACK_PAWN_A,
      BLACK_PAWN_B,
      BLACK_PAWN_C,
      BLACK_PAWN_D,
      BLACK_PAWN_E,
      BLACK_PAWN_F,
      BLACK_PAWN_G,
      BLACK_PAWN_H,
      // white
      WHITE_PAWN_A,
      WHITE_PAWN_B,
      WHITE_PAWN_C,
      WHITE_PAWN_D,
      WHITE_PAWN_E,
      WHITE_PAWN_F,
      WHITE_PAWN_G,
      WHITE_PAWN_H,
      WHITE_ROOK_A,
      WHITE_KNIGHT_B,
      WHITE_BISHOP_C,
      WHITE_QUEEN,
      WHITE_KING,
      WHITE_BISHOP_F,
      WHITE_KNIGHT_G,
      WHITE_ROOK_H,
};
static_assert(WHITE_ROOK_H == 31, "setup");

// Two kinds of rankings. One just gives equal rankings for black and
// white (no probabilities); the other gives a full ranking with
// probabilities.

std::vector<int> rank_tom = {
  WHITE_PAWN_F,
  BLACK_PAWN_F,
  WHITE_PAWN_C,
  BLACK_PAWN_C,
  WHITE_PAWN_G,
  BLACK_PAWN_G,
  WHITE_PAWN_A,
  BLACK_PAWN_A,
  WHITE_PAWN_H,
  BLACK_PAWN_H,
  WHITE_PAWN_B,
  BLACK_PAWN_B,
  WHITE_ROOK_H,
  BLACK_ROOK_H,
  WHITE_ROOK_A,
  BLACK_ROOK_A,
  WHITE_KING,
  BLACK_KING,
  WHITE_QUEEN,
  BLACK_QUEEN,
  WHITE_BISHOP_F,
  BLACK_BISHOP_F,
  WHITE_BISHOP_C,
  BLACK_BISHOP_C,
  WHITE_KNIGHT_G,
  BLACK_KNIGHT_G,
  WHITE_KNIGHT_B,
  BLACK_KNIGHT_B,
  WHITE_PAWN_E,
  BLACK_PAWN_E,
  WHITE_PAWN_D,
  BLACK_PAWN_D,
};

std::vector<int> rank_william = {
  WHITE_ROOK_A,
  BLACK_ROOK_A,
  WHITE_PAWN_B,
  BLACK_PAWN_B,
  WHITE_PAWN_A,
  BLACK_PAWN_A,
  WHITE_BISHOP_F,
  BLACK_BISHOP_F,
  WHITE_PAWN_C,
  BLACK_PAWN_C,
  WHITE_QUEEN,
  BLACK_QUEEN,
  WHITE_KING,
  BLACK_KING,
  WHITE_PAWN_H,
  BLACK_PAWN_H,
  WHITE_BISHOP_C,
  BLACK_BISHOP_C,
  WHITE_PAWN_F,
  BLACK_PAWN_F,
  WHITE_PAWN_G,
  BLACK_PAWN_G,
  WHITE_KNIGHT_B,
  BLACK_KNIGHT_B,
  WHITE_ROOK_H,
  BLACK_ROOK_H,
  WHITE_KNIGHT_G,
  BLACK_KNIGHT_G,
  WHITE_PAWN_D,
  BLACK_PAWN_D,
  WHITE_PAWN_E,
  BLACK_PAWN_E,
};

std::vector<int> rank_jim = {
  WHITE_KING,
  BLACK_KING,
  WHITE_ROOK_A,
  BLACK_ROOK_A,
  WHITE_ROOK_H,
  BLACK_ROOK_H,
  WHITE_PAWN_A,
  BLACK_PAWN_A,
  WHITE_PAWN_B,
  BLACK_PAWN_B,
  WHITE_PAWN_G,
  BLACK_PAWN_G,
  WHITE_PAWN_H,
  BLACK_PAWN_H,
  WHITE_KNIGHT_B,
  BLACK_KNIGHT_B,
  WHITE_KNIGHT_G,
  BLACK_KNIGHT_G,
  WHITE_PAWN_C,
  BLACK_PAWN_C,
  WHITE_PAWN_D,
  BLACK_PAWN_D,
  WHITE_PAWN_E,
  BLACK_PAWN_E,
  WHITE_PAWN_F,
  BLACK_PAWN_F,
  WHITE_BISHOP_C,
  BLACK_BISHOP_C,
  WHITE_BISHOP_F,
  BLACK_BISHOP_F,
  WHITE_QUEEN,
  BLACK_QUEEN,
};

std::vector<std::pair<int, float>> prob_david = {
  {WHITE_PAWN_G, 0.72},
  {BLACK_PAWN_G, 0.72},
  {WHITE_PAWN_B, 0.69},
  {BLACK_PAWN_A, 0.66},
  {BLACK_PAWN_B, 0.65},
  {WHITE_KING, 0.65},
  {WHITE_PAWN_A, 0.64},
  {BLACK_KING, 0.55},
  {BLACK_PAWN_F, 0.54},
  {WHITE_PAWN_F, 0.54},
  {BLACK_PAWN_H, 0.53},
  {WHITE_PAWN_H, 0.53},
  {WHITE_ROOK_H, 0.52},
  {BLACK_ROOK_H, 0.52},
  {WHITE_ROOK_A, 0.51},
  {BLACK_ROOK_A, 0.51},
  {WHITE_PAWN_C, 0.49},
  {BLACK_PAWN_C, 0.44},
  {WHITE_BISHOP_F, 0.33},
  {BLACK_BISHOP_C, 0.33},
  {WHITE_BISHOP_C, 0.32},
  {BLACK_BISHOP_F, 0.32},
  {WHITE_QUEEN, 0.31},
  {BLACK_QUEEN, 0.30},
  {WHITE_KNIGHT_G, 0.29},
  {BLACK_KNIGHT_G, 0.29},
  {WHITE_KNIGHT_B, 0.28},
  {BLACK_KNIGHT_B, 0.28},
  {WHITE_PAWN_E, 0.19},
  {BLACK_PAWN_D, 0.18},
  {WHITE_PAWN_D, 0.17},
  {BLACK_PAWN_E, 0.16},
};


std::vector<int> rank_ben = {
  WHITE_PAWN_H,
  BLACK_PAWN_H,
  WHITE_PAWN_A,
  BLACK_PAWN_A,
  WHITE_PAWN_F,
  BLACK_PAWN_F,
  WHITE_PAWN_G,
  BLACK_PAWN_G,
  WHITE_PAWN_B,
  BLACK_PAWN_B,
  WHITE_ROOK_H,
  BLACK_ROOK_H,
  WHITE_ROOK_A,
  BLACK_ROOK_A,
  WHITE_PAWN_E,
  BLACK_PAWN_E,
  WHITE_PAWN_D,
  BLACK_PAWN_D,
  WHITE_QUEEN,
  BLACK_QUEEN,
  WHITE_KING,
  BLACK_KING,
  WHITE_PAWN_C,
  BLACK_PAWN_C,
  WHITE_BISHOP_C,
  BLACK_BISHOP_C,
  WHITE_BISHOP_F,
  BLACK_BISHOP_F,
  WHITE_KNIGHT_B,
  BLACK_KNIGHT_B,
  WHITE_KNIGHT_G,
  BLACK_KNIGHT_G,
};

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

static vector<int> Strip(const vector<pair<int, float>> &probs) {
  vector<int> ret;
  ret.reserve(probs.size());
  for (const auto &p : probs) ret.push_back(p.first);
  return ret;
}

static vector<pair<int, float>> Degenerate(const vector<int> &ranks) {
  vector<pair<int, float>> ret;
  for (int p : ranks) ret.emplace_back(p, 0.0f);
  return ret;
}

static bool UseCol(int x) {
  const int p = PiecePiece(x);
  switch (p & Position::TYPE_MASK) {
  case Position::QUEEN:
  case Position::KING:
    return false;
  default:
    return true;
  }
}

static string PieceName(int x) {
  const int p = PiecePiece(x);
  const string ent = Position::HTMLEntity(p);
  const string col = PieceCol(x);
  if (UseCol(x)) {
    return ent + col;
  } else {
    return ent;
  }
}

// Actual results over all games (500m)
vector<pair<int, float>> actual_probs = {
  {WHITE_PAWN_H, 0.715432286},
  {BLACK_PAWN_A, 0.705200613},
  {WHITE_PAWN_A, 0.704906046},
  {BLACK_PAWN_H, 0.703575015},
  {WHITE_PAWN_G, 0.665572643},
  {BLACK_PAWN_G, 0.651024163},
  {WHITE_PAWN_B, 0.604643881},
  {BLACK_PAWN_B, 0.599056244},
  {WHITE_PAWN_F, 0.572712302},
  {WHITE_ROOK_A, 0.564669073},
  {BLACK_ROOK_A, 0.559042692},
  {BLACK_PAWN_F, 0.55310905},
  {WHITE_ROOK_H, 0.544626832},
  {BLACK_ROOK_H, 0.544186294},
  {WHITE_KING, 0.535234511},
  {BLACK_KING, 0.502660513},
  {WHITE_PAWN_C, 0.479240984},
  {WHITE_QUEEN, 0.456248462},
  {BLACK_PAWN_C, 0.45194149},
  {BLACK_QUEEN, 0.447802454},
  {BLACK_PAWN_E, 0.372184902},
  {BLACK_BISHOP_C, 0.323898226},
  {BLACK_PAWN_D, 0.323760539},
  {BLACK_BISHOP_F, 0.321573973},
  {WHITE_PAWN_E, 0.312881321},
  {WHITE_BISHOP_C, 0.312769711},
  {WHITE_BISHOP_F, 0.308608145},
  {WHITE_PAWN_D, 0.308215857},
  {WHITE_KNIGHT_B, 0.291056752},
  {BLACK_KNIGHT_B, 0.266897261},
  {BLACK_KNIGHT_G, 0.243342414},
  {WHITE_KNIGHT_G, 0.233440086},
};

// Sum of absolute difference in rank across all elements.
int RankError(const vector<int> &a,
	      const vector<int> &b) {
  int delta = 0;
  for (int i = 0; i < a.size(); i++) {
    int target = a[i];
    for (int j = 0; j < b.size(); j++) {
      if (b[j] == target) {
	delta += abs(i - j);
	goto next;
      }
    }
    LOG(FATAL) << "Did not find " << target << " in b?";
  next:;
  }
  return delta;
}


vector<pair<int, float>> titled_probs = {
  {WHITE_PAWN_H, 0.669196844},
  {BLACK_PAWN_A, 0.667333484},
  {WHITE_PAWN_A, 0.662262022},
  {BLACK_PAWN_H, 0.656060696},
  {WHITE_PAWN_G, 0.624552369},
  {BLACK_PAWN_G, 0.605713427},
  {WHITE_PAWN_B, 0.568870246},
  {BLACK_PAWN_B, 0.564766288},
  {WHITE_KING, 0.538510501},
  {WHITE_PAWN_F, 0.536402941},
  {BLACK_PAWN_F, 0.528088152},
  {BLACK_KING, 0.510553479},
  {BLACK_ROOK_A, 0.49197644},
  {WHITE_ROOK_A, 0.489860713},
  {WHITE_ROOK_H, 0.476608098},
  {BLACK_ROOK_H, 0.474159986},
  {WHITE_PAWN_C, 0.423101097},
  {WHITE_QUEEN, 0.395854831},
  {BLACK_QUEEN, 0.388097733},
  {BLACK_PAWN_C, 0.373647988},
  {BLACK_PAWN_E, 0.369938105},
  {BLACK_BISHOP_F, 0.312111437},
  {WHITE_BISHOP_F, 0.308377415},
  {WHITE_PAWN_E, 0.303896815},
  {BLACK_BISHOP_C, 0.287457943},
  {BLACK_PAWN_D, 0.287427694},
  {WHITE_BISHOP_C, 0.277486771},
  {WHITE_PAWN_D, 0.257768214},
  {WHITE_KNIGHT_B, 0.231363401},
  {BLACK_KNIGHT_B, 0.230935827},
  {BLACK_KNIGHT_G, 0.200624391},
  {WHITE_KNIGHT_G, 0.200406671},
};

vector<pair<int, float>> bullet_probs = {
  {BLACK_PAWN_A, 0.726668358},
  {WHITE_PAWN_H, 0.722974241},
  {WHITE_PAWN_A, 0.721098602},
  {BLACK_PAWN_H, 0.708957493},
  {WHITE_PAWN_G, 0.668866754},
  {BLACK_PAWN_G, 0.653226197},
  {WHITE_PAWN_B, 0.61848557},
  {BLACK_PAWN_B, 0.617226899},
  {WHITE_PAWN_F, 0.566843629},
  {BLACK_PAWN_F, 0.554071546},
  {WHITE_ROOK_A, 0.543329},
  {BLACK_ROOK_A, 0.540029883},
  {WHITE_KING, 0.528453827},
  {BLACK_ROOK_H, 0.524942517},
  {WHITE_ROOK_H, 0.523875415},
  {BLACK_KING, 0.501409173},
  {WHITE_PAWN_C, 0.470931053},
  {BLACK_PAWN_C, 0.443932712},
  {WHITE_QUEEN, 0.426350027},
  {BLACK_QUEEN, 0.417651385},
  {BLACK_PAWN_E, 0.38113597},
  {WHITE_PAWN_E, 0.308711886},
  {BLACK_BISHOP_F, 0.307721794},
  {BLACK_BISHOP_C, 0.30463022},
  {WHITE_BISHOP_F, 0.302136093},
  {BLACK_PAWN_D, 0.30203107},
  {WHITE_PAWN_D, 0.298784137},
  {WHITE_BISHOP_C, 0.288737357},
  {WHITE_KNIGHT_B, 0.261240125},
  {BLACK_KNIGHT_B, 0.248214781},
  {BLACK_KNIGHT_G, 0.217949584},
  {WHITE_KNIGHT_G, 0.21258761},
};

vector<pair<int, float>> blitz_probs = {
  {WHITE_PAWN_H, 0.705185533},
  {BLACK_PAWN_H, 0.694000185},
  {WHITE_PAWN_A, 0.6876176},
  {BLACK_PAWN_A, 0.686811268},
  {WHITE_PAWN_G, 0.659280062},
  {BLACK_PAWN_G, 0.644540489},
  {WHITE_PAWN_B, 0.590121329},
  {BLACK_PAWN_B, 0.58341682},
  {WHITE_PAWN_F, 0.566926539},
  {WHITE_ROOK_A, 0.560023189},
  {BLACK_ROOK_A, 0.554639757},
  {BLACK_PAWN_F, 0.550531864},
  {WHITE_ROOK_H, 0.541934133},
  {BLACK_ROOK_H, 0.541487694},
  {WHITE_KING, 0.537497282},
  {BLACK_KING, 0.504129291},
  {WHITE_PAWN_C, 0.469914317},
  {WHITE_QUEEN, 0.463243574},
  {BLACK_QUEEN, 0.454853535},
  {BLACK_PAWN_C, 0.439012706},
  {BLACK_PAWN_E, 0.370540828},
  {BLACK_PAWN_D, 0.323645025},
  {BLACK_BISHOP_C, 0.319640994},
  {BLACK_BISHOP_F, 0.317568094},
  {WHITE_PAWN_E, 0.31203258},
  {WHITE_BISHOP_C, 0.308808476},
  {WHITE_BISHOP_F, 0.304748744},
  {WHITE_PAWN_D, 0.301345259},
  {WHITE_KNIGHT_B, 0.286712468},
  {BLACK_KNIGHT_B, 0.260796309},
  {BLACK_KNIGHT_G, 0.241091579},
  {WHITE_KNIGHT_G, 0.229988366},
};

vector<pair<int, float>> rapid_probs = {
  {WHITE_PAWN_H, 0.717799306},
  {BLACK_PAWN_H, 0.707557678},
  {WHITE_PAWN_A, 0.704338729},
  {BLACK_PAWN_A, 0.699469626},
  {WHITE_PAWN_G, 0.667853653},
  {BLACK_PAWN_G, 0.655147254},
  {WHITE_PAWN_B, 0.602427602},
  {BLACK_PAWN_B, 0.59267354},
  {WHITE_ROOK_A, 0.592169166},
  {WHITE_PAWN_F, 0.587919891},
  {BLACK_ROOK_A, 0.582526207},
  {WHITE_ROOK_H, 0.571105301},
  {BLACK_ROOK_H, 0.568388283},
  {BLACK_PAWN_F, 0.554620206},
  {WHITE_KING, 0.540421069},
  {BLACK_KING, 0.501556337},
  {WHITE_PAWN_C, 0.499544144},
  {WHITE_QUEEN, 0.4821527},
  {BLACK_PAWN_C, 0.477736592},
  {BLACK_QUEEN, 0.473388523},
  {BLACK_PAWN_E, 0.359915793},
  {BLACK_BISHOP_C, 0.348341286},
  {BLACK_PAWN_D, 0.347047299},
  {WHITE_BISHOP_C, 0.342712104},
  {BLACK_BISHOP_F, 0.339081913},
  {WHITE_KNIGHT_B, 0.328480661},
  {WHITE_PAWN_D, 0.327255487},
  {WHITE_BISHOP_F, 0.3167575},
  {WHITE_PAWN_E, 0.315772921},
  {BLACK_KNIGHT_B, 0.291517437},
  {BLACK_KNIGHT_G, 0.274224102},
  {WHITE_KNIGHT_G, 0.260634691},
};

vector<pair<int, float>> classical_probs = {
  {WHITE_PAWN_H, 0.75596571},
  {BLACK_PAWN_H, 0.74674046},
  {WHITE_PAWN_A, 0.745404184},
  {BLACK_PAWN_A, 0.739825904},
  {WHITE_PAWN_G, 0.700627923},
  {BLACK_PAWN_G, 0.690689087},
  {WHITE_PAWN_B, 0.643349826},
  {WHITE_ROOK_A, 0.636144102},
  {WHITE_PAWN_F, 0.634782553},
  {BLACK_PAWN_B, 0.632014394},
  {BLACK_ROOK_A, 0.62692374},
  {WHITE_ROOK_H, 0.619760692},
  {BLACK_ROOK_H, 0.614498079},
  {BLACK_PAWN_F, 0.59248966},
  {WHITE_KING, 0.542440295},
  {WHITE_PAWN_C, 0.542292297},
  {BLACK_PAWN_C, 0.52329278},
  {WHITE_QUEEN, 0.520475864},
  {BLACK_QUEEN, 0.511975706},
  {BLACK_KING, 0.502516985},
  {BLACK_BISHOP_C, 0.411349565},
  {WHITE_BISHOP_C, 0.407337934},
  {BLACK_BISHOP_F, 0.397870809},
  {BLACK_PAWN_E, 0.392559052},
  {WHITE_KNIGHT_B, 0.390915632},
  {BLACK_PAWN_D, 0.388223618},
  {WHITE_BISHOP_F, 0.37211135},
  {WHITE_PAWN_D, 0.371815294},
  {BLACK_KNIGHT_B, 0.354173809},
  {WHITE_PAWN_E, 0.352591604},
  {BLACK_KNIGHT_G, 0.336583346},
  {WHITE_KNIGHT_G, 0.325550437},
};

void GenPerm() {
  (void)Degenerate;
  /*
  vector<pair<string, vector<pair<int, float>>>> tableau = {
    {"Actual", actual_probs},
    {"William", Degenerate(rank_william)},
    {"Jim", Degenerate(rank_jim)},
    {"Actual", actual_probs},
    {"David", prob_david},
    {"Tom", Degenerate(rank_tom)},
    {"Actual", actual_probs},    
    {"Ben", Degenerate(rank_ben)},
  };
  */
  
  vector<pair<string, vector<pair<int, float>>>> tableau = {
    {"All", actual_probs},
    {"Bullet", bullet_probs},
    {"Blitz", blitz_probs},
    {"Rapid", rapid_probs},
    {"Classical", classical_probs},
    {"Titled", titled_probs},
  };
  
  for (int i = 0; i < tableau.size(); i++) {
    for (int j = i + 1; j < tableau.size(); j++) {
      int e = RankError(Strip(tableau[i].second),
			Strip(tableau[j].second));
      printf("%s vs. %s: %d err\n",
	     tableau[i].first.c_str(),
	     tableau[j].first.c_str(), e);
    }
  }

  // Looks like slightly less than 341.
  if (false) {
    ArcFour rc(StringPrintf("%lld!", time(nullptr)));
    int64 total_error = 0LL;
    static constexpr int TRIALS = 1000000;
    for (int t = 0; t < TRIALS; t++) {
      vector<int> p1, p2;
      p1.reserve(32);
      p2.reserve(32);
      for (int i = 0; i < 32; i++) {
	p1.push_back(i);
	p2.push_back(i);
      }
      Shuffle(&rc, &p1);
      Shuffle(&rc, &p2);
      total_error += RankError(p1, p2);
    }
    double avg_error = (double)total_error / (double)TRIALS;
    printf("Average for random perms: %.6f\n", avg_error);
  }
  
  const float WIDTH = 800.0;
  const float HEIGHT = 600.0;
  FILE *f = fopen("perms.svg", "wb");
  fprintf(f, "%s", TextSVG::Header(WIDTH, HEIGHT).c_str());
  
  const float MARGIN_LEFT = 10.0;
  const float ROWHEIGHT = 16.0;
  const float HEADER_HEIGHT = 24.0;
  const float MARGIN_TOP = 10.0 + HEADER_HEIGHT;
  float xcol = MARGIN_LEFT;
  float COL_MARGIN = 35.0f;
  for (int i = 0; i < tableau.size(); i++) {
    float yy = MARGIN_TOP;
    const string &header = tableau[i].first;
    const vector<pair<int, float>> &col = tableau[i].second;

    fprintf(f, "%s\n",
	    TextSVG::Text(xcol, yy, "sans-serif",
			  20.0, {{"#000", header}}).c_str());
    yy += HEADER_HEIGHT;
    
    const bool has_probs = col[0].second > 0.0f;
    fprintf(stderr, "%s %s\n", header.c_str(),
	    has_probs ? " has probs " : " hasn't probs");
    const float LABEL_WIDTH = 35.0;
    const float PROB_WIDTH = 25.0;
    const float colwidth =
      has_probs ? (LABEL_WIDTH + PROB_WIDTH) : LABEL_WIDTH;
    for (int row = 0; row < col.size(); row++) {
      int p = col[row].first;
      fprintf(f, "%s\n",
	      TextSVG::Text(xcol, yy, "sans-serif",
			    18.0, {{"#000", PieceName(p)}}).c_str());

      if (has_probs) {
	const float prob = col[row].second;
	fprintf(f, "%s\n",
		TextSVG::Text(xcol + LABEL_WIDTH, yy, "sans-serif",
			      9.0, {{"#777",
				    StringPrintf("%d%%", (int)(prob * 100.0f))
				     }}).c_str());
      }

      // Draw connector.
      if (i != tableau.size() - 1) {
	const vector<pair<int, float>> &ncol = tableau[i + 1].second;
	// Get the index of the piece from this row.
	// if (p == WHITE_KING)
	for (int nrow = 0; nrow < ncol.size(); nrow++) {
	  if (ncol[nrow].first == p) {
	    // Maximum distance would be 31.
	    const int idist = abs(nrow - row);
	    const float actual_dist = idist / 31.0f;
	    float dist = 0.25 + (0.75 * actual_dist);
	    
	    float sx = xcol + colwidth;
	    float sy = yy - ROWHEIGHT / 2.0;
	    float dx = xcol + colwidth + COL_MARGIN;
	    float dy = (MARGIN_TOP + HEADER_HEIGHT) +
	      (ROWHEIGHT * nrow) - (ROWHEIGHT / 2.0);

	    if (idist > 0)
	    fprintf(f, "<path fill=\"none\" "
		    "stroke=\"#000\" stroke-opacity=\"%.2f\" "
		    "stroke-width=\"1px\" "
		    "d=\"M%s %s "
		    "C%s %s, %s %s, %s %s"
		    
		    // "L%s %s"
		    "\" />\n",
		    dist,
		    
		    TextSVG::Rtos(sx).c_str(),
		    TextSVG::Rtos(sy).c_str(),

		    // Curve control points.
		    TextSVG::Rtos(sx * 0.5 + dx * 0.5).c_str(),
		    TextSVG::Rtos(sy).c_str(),

		    TextSVG::Rtos(sx * 0.5 + dx * 0.5).c_str(),
		    TextSVG::Rtos(dy).c_str(),
		    
		    // end
		    TextSVG::Rtos(dx).c_str(),
		    TextSVG::Rtos(dy).c_str());
		    
	    break;
	  }
	}
      }
      
      yy += ROWHEIGHT;
    }
    xcol += colwidth + COL_MARGIN;
  }

  fprintf(f, "%s", TextSVG::Footer().c_str());
  fclose(f);
  
#if 0
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
#endif
}

int main(int argc, char **argv) {
  GenPerm();
  return 0;
}
