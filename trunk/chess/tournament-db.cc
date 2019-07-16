
#include "tournament-db.h"

#include <unordered_map>
#include <utility>
#include <vector>
#include <string>
#include <cstdint>

#include "../cc-lib/util.h"
#include "../cc-lib/base/logging.h"

using int64 = int64_t;
using namespace std;

void TournamentDB::SaveToFile(const Outcomes &outcomes, const std::string &filename) {
  // One per line as
  // whiteplayer|blackplayer|white_wins|white_losses|draws|example_win|example_loss|example_draw
  FILE *f = fopen(filename.c_str(), "wb");
  CHECK(f != nullptr) << filename;
  for (const auto &row : outcomes) {
    const auto &key = row.first;
    const Cell &cell = row.second;
    fprintf(f, "%s|%s|%lld|%lld|%lld|%s|%s|%s\n",
	    key.first.c_str(),
	    key.second.c_str(),
	    cell.white_wins,
	    cell.white_losses,
	    cell.draws,
	    cell.example_win.c_str(),
	    cell.example_loss.c_str(),
	    cell.example_draw.c_str());
  }
  fclose(f);
}

Outcomes TournamentDB::LoadFromFile(const string &filename,
				    const std::unordered_set<string> &ignore) {
  vector<string> lines = Util::ReadFileToLines(filename);
  Outcomes outcomes;
  for (string &line : lines) {
    if (line.empty()) continue;

    Cell cell;
    string white = Util::chopto('|', line);
    string black = Util::chopto('|', line);
    string wins_s = Util::chopto('|', line);
    string losses_s = Util::chopto('|', line);
    string draws_s = Util::chopto('|', line);
    cell.example_win = Util::chopto('|', line);
    cell.example_loss = Util::chopto('|', line);
    cell.example_draw = line;

    cell.white_wins = strtoll(wins_s.c_str(), nullptr, 10);
    cell.white_losses = strtoll(losses_s.c_str(), nullptr, 10);
    cell.draws = strtoll(draws_s.c_str(), nullptr, 10);

    if (ignore.find(white) != ignore.end() ||
	ignore.find(black) != ignore.end())
      continue;
    outcomes[make_pair(white, black)] = cell;
  }
  return outcomes;
}

void TournamentDB::MergeInto(const Outcomes &source, Outcomes *dest) {
  for (const auto &row : source) {
    const Cell &src = row.second;
    Cell *dst = &(*dest)[row.first];
    
    dst->white_wins += src.white_wins;
    dst->white_losses += src.white_losses;
    dst->draws += src.draws;
    
#   define MELD(field)						\
    do { if (dst-> field .empty()) dst-> field = src. field ; }	\
    while(0)
    MELD(example_win);
    MELD(example_loss);
    MELD(example_draw);
#   undef MELD
  }
}
