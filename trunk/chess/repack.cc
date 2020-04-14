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

#define byte win_byte_override
#include <windows.h>
#include <psapi.h>
#undef byte

constexpr int MAX_PARALLELISM = 60;

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

struct MemPack {
  // Just try this most trivial approach first.
  std::shared_mutex m;
  std::unordered_map<uint64, PackedGame> games;
  int64 AddGames(vector<pair<uint64_t, PackedGame>> *g) {
    WriteMutexLock ml(&m);
    total_games += g->size();
    const int64 start = time(nullptr);
    for (int i = 0; i < g->size(); i++) {
      pair<uint64, PackedGame> &p = (*g)[i];
      total_positions += p.second.NumMoves();
      games[p.first] = std::move(p.second);
    }
    g->clear();
    return time(nullptr) - start;
  }
  int64 total_games = 0LL;
  int64 total_positions = 0LL;
};

static MemPack *mempack = nullptr;

static void AddPackFile(const string &filename) {
  const int64 start = time(nullptr);
  fprintf(stderr, "Load %s...\n", filename.c_str());
  fflush(stderr);
  vector<uint8> bytes = Util::ReadFileBytes(filename);
  CHECK(!bytes.empty()) << filename;
  vector<pair<uint64_t, PackedGame>> packed_games =
    PackedGame::SplitFile(bytes);

  const int64 num_games = packed_games.size();
  fprintf(stderr, "Read %lld games from %s. Adding...\n",
	  num_games, filename.c_str());
  fflush(stderr);

  int64 secs = time(nullptr) - start;
  int64 insert_time = mempack->AddGames(&packed_games);
  fprintf(stderr, "Inserted %lld games from %s. (%llds total, %llds insert)\n",
	  num_games, filename.c_str(),
	  secs, insert_time);
  fflush(stderr);
}

static void PrintMemoryUsage() {
  PROCESS_MEMORY_COUNTERS pmc;
  CHECK(GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof pmc));
  printf("PageFaultCount: %lld\n", (int64)pmc.PageFaultCount);
  printf("PeakWorkingSetSize: %lld\n", 
	 (int64)pmc.PeakWorkingSetSize);
  printf("WorkingSetSize: %lld\n", (int64)pmc.WorkingSetSize);
  printf("QuotaPeakPagedPoolUsage: %lld\n",
	 (int64)pmc.QuotaPeakPagedPoolUsage);
  printf("QuotaPagedPoolUsage: %lld\n", 
	 (int64)pmc.QuotaPagedPoolUsage);
  printf("QuotaPeakNonPagedPoolUsage: %lld\n", 
	 (int64)pmc.QuotaPeakNonPagedPoolUsage);
  printf("QuotaNonPagedPoolUsage: %lld\n", 
	 (int64)pmc.QuotaNonPagedPoolUsage);
  printf("PagefileUsage: %lld\n", (int64)pmc.PagefileUsage); 
  printf("PeakPagefileUsage: %lld\n", 
	 (int64)pmc.PeakPagefileUsage);
  fflush(stdout);
}

static void AddPackFiles(const std::vector<string> &filenames) {
  mempack = new MemPack;

  PrintMemoryUsage();
  
  int64 start = time(nullptr);
  ParallelApp(filenames,
	      AddPackFile,
	      MAX_PARALLELISM);

  int64 secs = time(nullptr) - start;
  
  fprintf(stderr, "Total %lld games (%lld positions) in %llds.\n",
	  mempack->total_games,
	  mempack->total_positions,
	  secs);
  fflush(stderr);

  // XXX here, pause so that we can measure memory footprint?
  PrintMemoryUsage();
  
  
  delete mempack;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "validatepack.exe file1.pack ...\n");
    return -1;
  }

  vector<string> filenames;
  for (int i = 1; i < argc; i++)
    filenames.emplace_back(argv[i]);

  AddPackFiles(filenames);
  
  return 0;
}
