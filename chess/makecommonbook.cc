// For "common" positions in a whitelist of hashes, count the moves
// made in that position.

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

#include "bigchess.h"
#include "packedgame.h"
#include "pack.h"
#include "common.h"

#include <windows.h>
#include <psapi.h>

constexpr int MAX_PARALLELISM = 30;
constexpr int MIN_COMMON = 2;

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

std::shared_mutex merged_mutex;
static CommonMap *merged_map = nullptr;
static int total_files = 0;
static int done_files = 0;

static double MemUsageG() {
  PROCESS_MEMORY_COUNTERS pmc;
  CHECK(GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof pmc));
  return (double)pmc.WorkingSetSize / 1000000000.0;
}

// Shared. Read-only.
static const CommonSet *common_set = nullptr;

static void AddPackFile(const string &filename) {
  const int64 start = time(nullptr);
  fprintf(stderr, "Load %s...\n", filename.c_str());
  fflush(stderr);
  vector<uint8> bytes = Util::ReadFileBytes(filename);
  CHECK(!bytes.empty()) << filename;
  vector<pair<uint64_t, PackedGame>> packed_games =
    PackedGame::SplitFile(bytes);

  const int64 num_games = packed_games.size();
  fprintf(stderr, "Read %lld games from %s. Running...\n",
	  num_games, filename.c_str());
  fflush(stderr);

  // Local to this pack file.
  CommonMap common_map;
  
  for (const auto &p : packed_games) {
    const PackedGame &pg = p.second;

    // All moves should be legal.
    Position pos;
    for (int i = 0; i < pg.NumMoves(); i++) {
      uint16 packed_move = pg.GetMove(i);
      Position::Move move = PackedGame::UnpackMove(packed_move);

      uint64 ph = PositionHash{}(pos);
      if (common_set->Contains(ph)) {
	common_map.positions[ph][packed_move]++;
      }
      // Just assume it's legal.
      // CHECK(pos.IsLegal(move));
      pos.ApplyMove(move);
    }
  }

  const int64 num_pos = common_map.positions.size();
  fprintf(stderr, "Got %lld distinct positions in %lld games from %s.\n",
	  num_pos, num_games, filename.c_str());
  fflush(stderr);
  packed_games.clear();
  
  {
    const int64 secs = time(nullptr) - start;
    WriteMutexLock ml(&merged_mutex);

    const int64 merge_start = time(nullptr);
    merged_map->MergeFrom(common_map);
    const int64 merge_time = time(nullptr) - merge_start;
    
    done_files++;
    fprintf(stderr, "[%d/%d] %lld games from %s. "
	    "(%lld+%lld sec) [%.2f G mem]\n",
	    done_files, total_files,
	    num_games, filename.c_str(),
	    secs, merge_time, MemUsageG());
    fflush(stderr);
  }
}

static void AddPackFiles(const std::vector<string> &filenames) {
  int64 start = time(nullptr);
  fprintf(stderr, "Loading common set....\n");
  fflush(stderr);
  common_set = new CommonSet("common.u64");
  CHECK(!common_set->positions.empty());
  merged_map = new CommonMap;
  fprintf(stderr, "[Elapsed %lld]. Loaded common set.\n",
	  time(nullptr) - start);
  fflush(stderr);
  total_files = filenames.size();
  
  ParallelApp(filenames,
	      AddPackFile,
	      MAX_PARALLELISM);
  delete common_set;

  fprintf(stderr, "[Elapsed %lld]. Writing...\n",
	  time(nullptr) - start);
  fflush(stderr);
  
  merged_map->WriteFile("common_map.bin");

  fprintf(stderr, "[Elapsed %lld]. Done; total memory usage is %.4f G\n",
	  time(nullptr) - start,
	  MemUsageG());
  fflush(stderr);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "makecommonbook.exe file1.pack ...\n");
    return -1;
  }

  vector<string> filenames;
  for (int i = 1; i < argc; i++)
    filenames.emplace_back(argv[i]);

  AddPackFiles(filenames);
  
  return 0;
}
