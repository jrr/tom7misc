
// Reads the tournament database and produces the html table with elo ratings.

#include <mutex>
#include <memory>
#include <unordered_map>
#include <cstdint>
#include <vector>
#include <string>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/util.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"

#include "headless-graphics.h"
#include "chess.h"
#include "tournament-db.h"

using namespace std;

using Move = Position::Move;
using int64 = int64_t;

static constexpr double ELO_START = 1000.0;

// 9 and 20 were decent
// static constexpr int NUM_ELO_ROUNDS = 19;
// static constexpr int ELO_PASSES = 20;

#if 1 // XXX
// Just run fast to look at output.
static constexpr int NUM_ELO_ROUNDS = 3;
static constexpr int ELO_PASSES = 2;
static constexpr int MIN_STATIONARY_ITERS = 100;
#else
// Get good, stable results
static constexpr int NUM_ELO_ROUNDS = 19;
static constexpr int ELO_PASSES = 20;
static constexpr int MIN_STATIONARY_ITERS = 100000;
#endif

// To protect against the effects of imbalanced number of games
// in the elo calculation, sample from each cell instead of using
// them all. The size of the sample is the size of the minimum
// cell.
static constexpr bool SAMPLE = true;

struct Elo {
  double elo = ELO_START;
  // Used to determine k-factor.
  int games = 0;
  // Totals.
  int wins = 0, losses = 0, draws = 0;
};

static void ReportGaps(int num_entrants,
		       const std::vector<string> &names,
		       const std::vector<Cell> &outcomes) {
  for (int white = 0; white < num_entrants; white++) {
    for (int black = 0; black < num_entrants; black++) {
      if (white != black) {
	const Cell &cell = outcomes[white * num_entrants + black];
	int64 total = cell.white_wins + cell.white_losses + cell.draws;
	if (total == 0) {
	  fprintf(stderr, "No games for white = %s, black = %s\n",
		  names[white].c_str(), names[black].c_str());
	}
      }
    }
  }
  fflush(stderr);
}

static vector<double> ComputeStationary(int num_entrants,
					const std::vector<string> &names,
					const std::vector<Cell> &outcomes) {
  // Size num_entrants * num_entants.

  // Return (win probability, loss probability) for white (playing as white)
  // vs. black. The remaining probability accounts for draws.
  auto GetProbs =
    [&outcomes, &names, num_entrants](int white, int black) {
      const Cell &cell = outcomes[white * num_entrants + black];
      int64 total = cell.white_wins + cell.white_losses + cell.draws;
      CHECK(total > 0) << "No games in cell! white = " << names[white]
		       << " black = " << names[black];
      return make_pair(cell.white_wins / (double)total,
		       cell.white_losses / (double)total);
    };
  
  // A Markov chain. The entry at mc[num_entrants * row + col] is the
  // probability of a transition from state 'row' to state 'col'. Imagine
  // a champion trophy held by the 'row' player. This player plays in
  // a tournament against all other players. Who would get the trophy?
  //   - Pick a random opponent (not including myself) and assignment
  //     of colors. If the opponent wins, they get the trophy.
  //   - If I win, I keep the trophy.
  //   - If a draw, then we split the trophy 50/50 (= same as randomly
  //     assigning it).
  // We can do this by assigning (P(loss) + 0.5 * P(draw)) / (num_entrants - 1)
  // to each cell but the diagonal, and the remainder to that cell (which
  // corresponding to a win or half a draw).
  vector<double> mc(num_entrants * num_entrants, 0.0);
  double entrants_not_me = num_entrants - 1.0;
  for (int row = 0; row < num_entrants; row++) {
    // This is the leftover probability mass from wins and half draws.
    double sum = 0.0;
    for (int col = 0; col < num_entrants; col++) {
      if (col != row) {
	// We have separate statistics for games played as white and
	// as black. Because of the way the tournament is run, we should
	// generally have a symmetric count, but just in case, we
	// average the separately-computed probabilities.
	double white_win, white_loss, black_win, black_loss;
	std::tie(white_win, white_loss) = GetProbs(row, col);
	std::tie(black_win, black_loss) = GetProbs(col, row);
	double white_draw = 1.0 - (white_win + white_loss);
	double black_draw = 1.0 - (black_win + black_loss);
      
	// Play randomly as black or white, so just mix the two
	// expectations evenly.
	double expected_losses = (white_loss + black_win) * 0.5;
	double expected_draws = (white_draw + black_draw) * 0.5;

	double e = (expected_losses + 0.5 * expected_draws) / entrants_not_me;
	mc[row * num_entrants + col] = e;
	sum += e;
      }
    }
    mc[row * num_entrants + row] = 1.0 - sum;
  }

  // OK, so now we have a Markov transition matrix. Compute its stationary
  // distribution. Begin with uniform distribution:
  vector<double> dist(num_entrants, 1.0 / num_entrants);

  // We want dist such that dist * mc = dist.
  // There are definitely faster ways to compute this, but the
  // easiest is to just iteratively perform the multiplication
  // until it has converged to our satisfaction. (Also note that
  // there exist matrices where this will not converge because
  // no such distribution exists. I think this won't happen
  // for our data set... (?))
  for (int iters = 0; true; iters++) {
    vector<double> next(num_entrants, 0.0);
    for (int row = 0; row < dist.size(); row++) {
      for (int col = 0; col < dist.size(); col++) {
	next[col] += dist[row] * mc[row * num_entrants + col];
      }
    }
    
    double diff = 0.0;
    for (int i = 0; i < num_entrants; i++)
      diff += std::fabs(next[i] - dist[i]);

    dist.swap(next);
    if (iters % 10000 == 0) {
      fprintf(stderr, "%d iters, err %.8f\n", iters, diff);
    }
    if (iters > MIN_STATIONARY_ITERS && diff < 0.000001) {
      fprintf(stderr, "Satisfactory error in %d iters\n", iters);
      break;
    }
  }
  return dist;
}

// With the win/loss/draw matrix, compute elo ratings for each player.
static vector<Elo> ComputeElo(ArcFour *rc,
			      int num_entrants,
			      const std::vector<double> &start_elos,
			      double k,
			      const std::vector<Cell> &orig_outcomes) {
  CHECK(orig_outcomes.size() == num_entrants * num_entrants);
  // We don't need the examples, but it's easiest to just copy
  // everything. We will modify wins/losses/draws to replay the games.
  std::vector<Cell> outcomes = orig_outcomes;
  
  vector<Elo> elos;
  elos.resize(num_entrants);
  CHECK(start_elos.size() == num_entrants);
  for (int i = 0; i < num_entrants; i++)
    elos[i].elo = start_elos[i];
  
  // Perform iterative updates in random order.
  vector<pair<int, int>> nonempty_matchups;
  // Initialize, ignoring self-play.
  for (int white = 0; white < num_entrants; white++) {
    for (int black = 0; black < num_entrants; black++) {
      if (white != black) {
	Cell *cell = &outcomes[white * num_entrants + black];
	if (cell->white_wins != 0 ||
	    cell->white_losses != 0 ||
	    cell->draws != 0) {
	  nonempty_matchups.emplace_back(white, black);
	}
      }
    }
  }

  auto ClaimResult =
    [rc](Cell *cell) {
      // Pick uniformly in proportion to the number of games
      // played, not the three (non-empty) categories.
      int total_mass = cell->white_wins + cell->white_losses + cell->draws;
      CHECK(total_mass > 0) << "Invariant";
      int idx = RandTo32(rc, total_mass);
      if (idx < cell->white_wins) {
	cell->white_wins--;
	return 1;
      }
      idx -= cell->white_wins;
      if (idx < cell->white_losses) {
	cell->white_losses--;
	return -1;
      }
      idx -= cell->white_losses;
      CHECK(idx < cell->draws) << "Bug? " << idx << " " << cell->draws;
      cell->draws--;
      return 0;
    };
  
  while (!nonempty_matchups.empty()) {
    Shuffle(rc, &nonempty_matchups);
    vector<pair<int, int>> next_matchups;
    for (pair<int, int> p : nonempty_matchups) {
      const int white = p.first, black = p.second;
      Cell *cell = &outcomes[white * num_entrants + black];
      // Randomly pick one of the wins, losses or draws. 1 means
      // 1 white wins.
      const int result = ClaimResult(cell);
      
      const double q_white = pow(10.0, elos[white].elo / 400.0);
      const double q_black = pow(10.0, elos[black].elo / 400.0);
      // Expected score.
      const double e_white = q_white / (q_white + q_black);
      const double e_black = 1.0 - e_white;
      // Actual score.
      double s_white = 0.0, s_black = 0.0;
      switch (result) {
      case -1:
	s_black = 1.0;
	elos[white].losses++;
	elos[black].wins++;
	break;
      case 1:
	s_white = 1.0;
	elos[white].wins++;
	elos[black].losses++;
	break;
      default:
      case 0:
	s_white = 0.5;
	s_black = 0.5;
	elos[white].draws++;
	elos[black].draws++;
      }

      // TODO: Perhaps should use diminishing k as games go on.
      // Modulating this based on the rating does not make that much
      // sense to me; I think this is intended to capture the fact
      // that true human skill changes over time. Our computer players
      // are not like that.
      const double k_white = k;
      const double k_black = k;

      elos[white].elo += k_white * (s_white - e_white);
      elos[black].elo += k_black * (s_black - e_black);
      
      elos[white].games++;
      elos[black].games++;

      // Only keep it around if there are more games to simulate.
      if (cell->white_wins > 0 ||
	  cell->white_losses > 0 ||
	  cell->draws > 0) {
	next_matchups.push_back(p);
      }
    }
    nonempty_matchups = std::move(next_matchups);
  }
  return elos;
}

static int64 GetMinMatchups(int num_entrants,
			    const vector<Cell> &outcomes,
			    int *min_white, int *min_black) {
  int64 min_total = -1;
  for (int white = 0; white < num_entrants; white++) {
    for (int black = 0; black < num_entrants; black++) {
      if (white != black) {
	const Cell &cell = outcomes[white * num_entrants + black];
	int64 total = cell.white_wins + cell.white_losses + cell.draws;
	if (min_total < 0 || total < min_total) {
	  min_total = total;
	  *min_white = white;
	  *min_black = black;
	}
      }
    }
  }
  return min_total;
}


static void MakeImages(int num_entrants,
		       const vector<string> &names,
		       const vector<int> &by_elo,
		       const vector<Cell> &outcomes) {
  std::vector<uint8> rgba;
  static constexpr int CELL = 16;
  const int MARGIN_LEFT = 160;
  const int WIDTH = CELL * num_entrants + MARGIN_LEFT;
  const int HEIGHT = CELL * num_entrants;
  rgba.resize(WIDTH * HEIGHT * 4);

  auto BlendQuarter = [&rgba, WIDTH, HEIGHT](int x, int y,
					     uint8 r, uint8 g, uint8 b) {
      const int i = (WIDTH * y + x) * 4;

      const uint8 oldr = rgba[i + 0];
      const uint8 oldg = rgba[i + 1];
      const uint8 oldb = rgba[i + 2];
      const uint8 olda = rgba[i + 3];
      uint8 rr = Mix4(oldr, oldr, oldr, r);
      uint8 gg = Mix4(oldg, oldg, oldg, g);
      uint8 bb = Mix4(oldb, oldb, oldb, b);
      SetPixel(WIDTH, HEIGHT, x, y, rr, gg, bb, olda, &rgba);
    };

  auto BlendHalf = [&rgba, WIDTH, HEIGHT](int x, int y,
					  uint8 r, uint8 g, uint8 b) {
      const int i = (WIDTH * y + x) * 4;

      const uint8 oldr = rgba[i + 0];
      const uint8 oldg = rgba[i + 1];
      const uint8 oldb = rgba[i + 2];
      const uint8 olda = rgba[i + 3];
      uint8 rr = Mix4(oldr, oldr, r, r);
      uint8 gg = Mix4(oldg, oldg, g, g);
      uint8 bb = Mix4(oldb, oldb, b, b);
      SetPixel(WIDTH, HEIGHT, x, y, rr, gg, bb, olda, &rgba);
    };

  for (int erow = 0; erow < num_entrants; erow++) {
    const int row = by_elo[erow];
    for (int ecol = 0; ecol < num_entrants; ecol++) {
      const int col = by_elo[ecol];
      const int idx = row * num_entrants + col;
      const Cell &cell = outcomes[idx];

      const int64 total = cell.white_wins + cell.white_losses + cell.draws;

      int xx = MARGIN_LEFT + ecol * CELL, yy = erow * CELL;
	
      if (total > 0) {
	double fr = cell.white_losses / (double)total;
	double fg = cell.white_wins / (double)total;
	double fb = cell.draws / (double)total;
	// We want black text to be visible on the color, so use
	// the space from 0.5 - 1.
	uint8 ir = (int)round((0.5 + (fr * 0.5)) * 255.0);
	uint8 ig = (int)round((0.5 + (fg * 0.5)) * 255.0);
	uint8 ib = (int)round((0.5 + (fb * 0.5)) * 255.0);
	FillRect(WIDTH, HEIGHT,
		 xx, yy, CELL, CELL,
		 ir, ig, ib, 255, &rgba);
      }

      if (cell.white_wins == 0 && cell.draws == 0) {
	for (int i = 3; i < CELL - 2; i++) {
	  BlendHalf(xx + i, yy + i, 0, 0, 0);
	}

	for (int i = 3; i < CELL - 2; i++) {
	  BlendHalf(xx + (CELL - i), yy + i, 0, 0, 0);
	}
      }
	
      if (cell.white_losses == 0 && cell.draws == 0) {
	for (int i = 3; i < CELL - 2; i++) {
	  BlendHalf(xx + i, yy + i, 0, 0, 0);
	}

	for (int i = 3; i < CELL - 2; i++) {
	  BlendHalf(xx + (CELL - i), yy + i, 0, 0, 0);
	}
      }

    }
  }
    
  // Draw grid over it.
  for (int y = 1; y < num_entrants; y++) {
    for (int x = 0; x < WIDTH; x++) {
      BlendQuarter(MARGIN_LEFT + x, y * CELL, 0, 0, 0);
    }
  }
  for (int x = 1; x < num_entrants; x++) {
    for (int y = 0; y < HEIGHT; y++) {
      BlendQuarter(MARGIN_LEFT + x * CELL, y, 0, 0, 0);
    }
  }

  static constexpr const char FONTCHARS[] =
    " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    "0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?";

  // Draw tiger stripes for player names.
  for (int row = 0; row < num_entrants; row++) {
    int mm = (row & 1) ? 0x2F : 0x22;
    FillRect(WIDTH, HEIGHT,
	     0, row * CELL, MARGIN_LEFT - 1, CELL - 1,
	     mm, mm, mm, 255, &rgba);
  }
  
  std::unique_ptr<HeadlessFont> font(
      HeadlessFont::Create("blind/font.png", FONTCHARS, 9, 16, 7, 1));
  CHECK(font.get() != nullptr);

  for (int erow = 0; erow < num_entrants; erow++) {
    const int row = by_elo[erow];
    // const string &name = names[row];
    // XXX -19 here is hard-coded as the longest name...
    string name = Util::Pad(-19, names[row]);
    font->DrawPlain(2, erow * CELL,
		    name,
		    &rgba, WIDTH, HEIGHT);
  }
  
  SaveRGBA(rgba, WIDTH, HEIGHT, "elo.png");
}

int main(int argc, char **argv) {
  printf("Reading outcomes db:\n");
  Outcomes sparse_outcomes = TournamentDB::LoadFromFile("tournament.db");
  printf("Densifying outcomes:\n");
  // Assign some arbitrary indices.
  int next_id = 0;
  std::map<string, int> ids;
  vector<string> names;
  auto GetId = [&next_id, &ids, &names](const string &s) {
      if (ids.find(s) == ids.end()) {
	int id = next_id++;
	ids[s] = id;
	names.push_back(s);
	return id;
      } else {
	return ids[s];
      }
    };
  for (const auto &p : sparse_outcomes) {
    (void)GetId(p.first.first);
    (void)GetId(p.first.second);
  }
  const int num_entrants = next_id;

  vector<Cell> outcomes;
  outcomes.resize(num_entrants * num_entrants);
  for (const auto &p : sparse_outcomes) {
    int row = GetId(p.first.first);
    int col = GetId(p.first.second);
    outcomes[row * num_entrants + col] = p.second;
  }

  ReportGaps(num_entrants, names, outcomes);
  
  // PERF could be in parallel...
  const vector<double> stationary =
    ComputeStationary(num_entrants, names, outcomes);

  vector<Cell> sampled_outcomes = outcomes;
  if (SAMPLE) {
    int min_white = 0, min_black = 0;
    const int sample_n =
      GetMinMatchups(num_entrants, outcomes, &min_white, &min_black);
    ArcFour rc(StringPrintf("sample.%lld", (int64)time(nullptr)));
    printf("Fewest matchups in %s vs %s.\n",
	   names[min_white].c_str(), names[min_black].c_str());
    printf("Sample %d from each cell...\n", sample_n);
    for (int white = 0; white < num_entrants; white++) {
      for (int black = 0; black < num_entrants; black++) {
	if (white != black) {
	  Cell *cell = &sampled_outcomes[white * num_entrants + black];
	  // Sampled.
	  int wins = 0, losses = 0, draws = 0;
	  int64 total = cell->white_wins + cell->white_losses + cell->draws;
	  CHECK(total >= sample_n) << names[white] << " vs " << names[black]
				   << " total: " << total;
	  for (int i = 0; i < sample_n; i++) {
	    int64 idx = RandTo(&rc, total);
	    idx -= cell->white_wins;
	    if (idx < 0) {
	      cell->white_wins--;
	      wins++;
	    } else {
	      idx -= cell->white_losses;
	      if (idx < 0) {
		cell->white_losses--;
		losses++;
	      } else {
		cell->draws--;
		draws++;
	      }
	    }
	    total--;
	  }

	  // And replace it in place.
	  cell->white_wins = wins;
	  cell->white_losses = losses;
	  cell->draws = draws;
	}
      }
    }
  }
  
  printf("Running elo:\n");
  fflush(stdout);
  const int64 start_elo = time(nullptr);
  ArcFour elo_rc(StringPrintf("elo.%lld", start_elo));
  vector<vector<Elo>> all_elos;
  all_elos.reserve(NUM_ELO_ROUNDS);
  for (int i = 0; i < NUM_ELO_ROUNDS; i++) {
    vector<double> start_elos(num_entrants, ELO_START);
    vector<Elo> elos;
    for (int j = 0; j < ELO_PASSES; j++) {
      elos = ComputeElo(&elo_rc,
			num_entrants,
			start_elos,
			// let k shrink from 10 to 10/PASSES.
			10.0 * ((ELO_PASSES - j) / (double)ELO_PASSES),
			sampled_outcomes);
      for (int e = 0; e < elos.size(); e++)
	start_elos[e] = elos[e].elo;
    }
    
    all_elos.push_back(std::move(elos));
    if (i % 100 == 0) { printf("%d ", i); fflush(stdout); }
  }
  printf("Done in %lld sec.\n", time(nullptr) - start_elo);
  fflush(stdout);

  // Print the matrix!
  string prelude = Util::ReadFile("tournament-prelude.html");
  FILE *f = fopen("tournament.html", "wb");
  CHECK(f);
  
  fprintf(f, "<!doctype html>\n"
	  "<meta charset=\"utf-8\" />\n"
	  "%s", prelude.c_str());

  fprintf(f, "<script>\n"
	  "const cells = [\n");
  for (int i = 0; i < outcomes.size(); i++) {
    fprintf(f, " {");

    const Cell &cell = outcomes[i];
    fprintf(f, "ew: '%s', ", cell.example_win.c_str());
    fprintf(f, "el: '%s', ", cell.example_loss.c_str());
    fprintf(f, "ed: '%s', ", cell.example_draw.c_str());
    fprintf(f, "}");
    if (i != outcomes.size() - 1) fprintf(f, ",\n");
  }
  fprintf(f, "];\n</script>\n\n");

  struct EloSummary {
    double median = 0.0;
    double p25 = 0.0, p75 = 0.0;
    // For the median one.
    int wins = 0, losses = 0, draws = 0;
  };

  vector<EloSummary> elos;
  for (int i = 0; i < num_entrants; i++) {
    vector<Elo> player_elos;
    player_elos.reserve(all_elos.size());
    for (int r = 0; r < all_elos.size(); r++) {
      player_elos.push_back(all_elos[r][i]);
    }
    std::sort(player_elos.begin(), player_elos.end(),
	      [](const Elo &a, const Elo &b) {
		return a.elo < b.elo;
	      });
    EloSummary es;
    const Elo &median = player_elos[NUM_ELO_ROUNDS >> 1];
    es.median = median.elo;
    es.wins = median.wins;
    es.losses = median.losses;
    es.draws = median.draws;
    es.p25 = player_elos[NUM_ELO_ROUNDS >> 2].elo;
    es.p75 = player_elos[(NUM_ELO_ROUNDS >> 2) * 3].elo;
    elos.push_back(es);
  }
  
  vector<int> by_elo;
  for (int i = 0; i < num_entrants; i++) by_elo.push_back(i);
  // Note: Assumes no nans.
  std::sort(by_elo.begin(), by_elo.end(),
	    [&elos](int a, int b) {
	      if (elos[a].median != elos[b].median)
		return elos[a].median < elos[b].median;
	      return a < b;
	    });
  
  // U+FF3C fullwidth reverse solidus
  fprintf(f, "<table>");

  auto PrintColumns = [&names, &by_elo, num_entrants, f]() {
      fprintf(f, "<tr><td>white \uFF3C black</td>\n");
      for (int ecol = 0; ecol < num_entrants; ecol++) {
	const int col = by_elo[ecol];
	fprintf(f, " <td>");
	// fprintf(f, "%s", names[col].c_str());
	string s = names[col];
	for (int i = 0; i < s.size(); i++) {
	  fprintf(f, "<br>%c", s[i]);
	}
	fprintf(f, "</td>\n");
      }
      fprintf(f, "</tr>\n");
    };

  PrintColumns();
  for (int erow = 0; erow < num_entrants; erow++) {
    const int row = by_elo[erow];
    fprintf(f, "<tr>");
    fprintf(f, "<td>%s</td>\n", names[row].c_str());
    for (int ecol = 0; ecol < num_entrants; ecol++) {
      const int col = by_elo[ecol];
      const int idx = row * num_entrants + col;
      const Cell &cell = outcomes[idx];

      const int64 total = cell.white_wins + cell.white_losses + cell.draws;
      string color = "#fff";
      if (total > 0) {
	double fr = cell.white_losses / (double)total;
	double fg = cell.white_wins / (double)total;
	double fb = cell.draws / (double)total;
	// We want black text to be visible on the color, so use
	// the space from 0.5 - 1.
	int ir = round((0.5 + (fr * 0.5)) * 255.0);
	int ig = round((0.5 + (fg * 0.5)) * 255.0);
	int ib = round((0.5 + (fb * 0.5)) * 255.0);
	color = StringPrintf("rgb(%d,%d,%d)", ir, ig, ib);
      }
      const bool insufficient = (row != col) && total < 100;
      fprintf(f, "  <td style=\"background-color:%s\" id=\"c%d\">"
	      "<span class=\"c\" onclick=\"show(%d)\"%s>"
	      "%lld w<br>%lld l<br>%lld d</span></td>\n",
	      color.c_str(),
	      idx, idx,
	      (insufficient ? " style=\"font-weight: bold\" " : ""),
	      cell.white_wins, cell.white_losses, cell.draws);
    }
    fprintf(f, "<td>%s</td>\n", names[row].c_str());
    fprintf(f, "</tr>\n");
  }
  PrintColumns();

  fprintf(f, "</table>\n");  

  MakeImages(num_entrants, names, by_elo, outcomes);
  
  fprintf(f, "<table><tr><td>player</td><td>25</td><td>elo</td><td>75</td><td>w/l/d</td>"
	  "<td>p | norm(p)</td></tr>\n");
  for (int i : by_elo) {
    fprintf(f,
	    " <tr><td>%s</td><td>%.2f</td><td>%.2f</td><td>%.2f</td><td>%d/%d/%d</td>"
	    "<td>%.8f | %.8f</td></tr>\n",
	    names[i].c_str(),
	    elos[i].p25,
	    elos[i].median,
	    elos[i].p75,
	    elos[i].wins,
	    elos[i].losses,
	    elos[i].draws,
	    stationary[i],
	    stationary[i] * num_entrants);
  }
  fprintf(f, "</table>\n");

  fprintf(f, "<div id=\"detail\"></div>\n");
  fclose(f);

  return 0;
}
