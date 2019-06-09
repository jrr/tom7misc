#include "chess.h"

#include <string>
#include <deque>
#include <shared_mutex>
#include <thread>
#include <vector>
#include <utility>
#include <unistd.h>

#include "base/stringprintf.h"
#include "gtl/top_n.h"
#include "base/logging.h"
#include "util.h"
#include "city.h"

#include "pgn.h"
#include "gamestats.h"
#include "bigchess.h"
#include "fate-data.h"
#include "packedgame.h"

constexpr int MAX_PARALLELISM = 8;

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

// static constexpr int64 MAX_GAMES = 1000000;
static constexpr int64 MAX_GAMES = 0LL;

static void ValidateOne(
    const std::pair<uint64, PackedGame> &one) {
  const uint64 hc = one.first;
  const PackedGame &pg = one.second;
  const uint64 actual_hc = pg.HashCode();

  // All moves should be legal.
  Position pos;
  for (int i = 0; i < pg.NumMoves(); i++) {
    Position::Move move = PackedGame::UnpackMove(pg.GetMove(i));
    CHECK(pos.IsLegal(move));
    pos.ApplyMove(move);
  }

  // XXX Check termination!

  CHECK(hc == actual_hc) << hc << " vs " << actual_hc;
}

static void ValidatePack(const char *filename) {
  const int64 start = time(nullptr);
  fprintf(stderr, "Validate %s...\n", filename);
  fflush(stderr);
  vector<uint8> bytes = Util::ReadFileBytes(filename);
  CHECK(!bytes.empty()) << filename;
  vector<pair<uint64_t, PackedGame>> packed_games =
    PackedGame::SplitFile(bytes);

  // In parallel?
  fprintf(stderr, "Read %lld games from %s. Validating...\n",
	  (int64)packed_games.size(), filename);
  fflush(stderr);

  UnParallelApp(packed_games, ValidateOne, 4);

  int64 secs = time(nullptr) - start;
  fprintf(stderr, "Validated %s in %lld sec.\n",
	  filename, secs);
  fflush(stderr);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "validatepack.exe file1.pack ...\n");
    return -1;
  }

  ParallelComp(argc - 1,
	       [argv](int i) { ValidatePack(argv[i + 1]); },
	       MAX_PARALLELISM);

  return 0;
}
