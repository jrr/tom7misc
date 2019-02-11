
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

#include "chess.h"
#include "tournament-db.h"

using namespace std;

using Move = Position::Move;
using int64 = int64_t;

static constexpr double ELO_START = 1000.0;

struct Elo {
  double elo = ELO_START;
  // Used to determine k-factor.
  int games = 0;
  // Totals.
  int wins = 0, losses = 0, draws = 0;
};

// With the win/loss/draw matrix, compute elo ratings for each player.
static vector<Elo> ComputeElo(int num_entrants, const std::vector<Cell> &orig_outcomes) {
  CHECK(orig_outcomes.size() == num_entrants * num_entrants);
  // We don't need the examples, but it's easiest to just copy
  // everything. We will modify wins/losses/draws to replay the games.
  std::vector<Cell> outcomes = orig_outcomes;
  
  vector<Elo> elos;
  elos.resize(num_entrants);
  
  ArcFour rc(StringPrintf("elo.%lld", (int64)time(nullptr)));
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
    [&rc](Cell *cell) {
      // Pick uniformly in proportion to the number of games
      // played, not the three (non-empty) categories.
      int total_mass = cell->white_wins + cell->white_losses + cell->draws;
      CHECK(total_mass > 0) << "Invariant";
      int idx = RandTo32(&rc, total_mass);
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
    Shuffle(&rc, &nonempty_matchups);
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
      // Modulating this based on the rating does not make that much sense to me;
      // I think this is intended to capture the fact that true human skill changes
      // over time. Our computer players are not like that.
      const double k_white = 10.0;
      const double k_black = 10.0;

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
  
  
  printf("Running elo:\n");
  fflush(stdout);
  const int64 start_elo = time(nullptr);
  const vector<Elo> elos = ComputeElo(num_entrants, outcomes);
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

  // U+FF3C fullwidth reverse solidus
  fprintf(f, "<table><tr><td>white \uFF3C black</td>\n");
  for (const string &name : names)
    fprintf(f, " <td>%s</td>\n", name.c_str());
  fprintf(f, "</tr>\n");
  for (int row = 0; row < num_entrants; row++) {
    fprintf(f, "<tr><td>%s</td>\n", names[row].c_str());
    for (int col = 0; col < num_entrants; col++) {
      int idx = row * num_entrants + col;
      const Cell &cell = outcomes[idx];
      fprintf(f, "  <td id=\"c%d\"><span class=\"c\" onclick=\"show(%d)\">"
	      "%lld w, %lld l, %lld d</span></td>\n",
	      idx, idx,
	      cell.white_wins, cell.white_losses, cell.draws);
    }
    fprintf(f, "</tr>\n");
  }
  fprintf(f, "</table>\n");

  vector<int> by_elo;
  for (int i = 0; i < num_entrants; i++) by_elo.push_back(i);
  // Note: Assumes no nans.
  std::sort(by_elo.begin(), by_elo.end(),
	    [&elos](int a, int b) {
	      if (elos[a].elo != elos[b].elo)
		return elos[a].elo < elos[b].elo;
	      return a < b;
	    });
  
  fprintf(f, "<table><tr><td>player</td><td>elo</td><td>w/l/d</tr>\n");
  for (int i : by_elo) {
    fprintf(f, " <tr><td>%s</td><td>%.2f</td><td>%d/%d/%d</td></tr>\n",
	    names[i].c_str(),
	    elos[i].elo,
	    elos[i].wins,
	    elos[i].losses,
	    elos[i].draws);
  }
  fprintf(f, "</table>\n");

  fprintf(f, "<div id=\"detail\"></div>\n");
  fclose(f);

  return 0;
}
