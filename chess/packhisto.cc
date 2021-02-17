// Experiment with loading all games into RAM at once.
// They (336 million games) fit in 61 GB even in an unordered_map,
// which probably has lots of fragmentation.
// These games comprise 21'553'382'902 positions (not unique ones,
// but instances), 21 billion, an average of 64 positions per game.
//
// Simplest way to index these would be to store
//  64 bit position hash -> vector of game hashes in which they occur
//
// Naively, this would be 8 bytes * 21B games just for the vector of
// game hashes, which is 168 GB; this blows our budget. Even worse if
// most positions occur only once, since we then have to store the
// key, the vector overhead, and the one value, which could easily be
// 64+ bytes.
//
// But positions reachable after one move occur in basically every
// game, and there are probably a handful of common positions that
// account for a large percentage of the total positions reached. Let's
// make a histogram to study this.

// How many distinct games are there?
// Can we get away with 32-bit keys with an acceptable number of
// collisions? If we are indexing into games, we can always filter
// these out after the fact...
//
// Well, it turns out that the distribution is so long-tailed (90%+ of
// distinct positions are singleton; 76% of instances are singletons)
// that we don't get any help there.
//
// So here we split the problem in two. For commonly occurring
// positions, we tally a histogram of the moves taken and just store
// that in memory. there are ~millions of these. We define a "common"
// position as one that occurred more than 10 times in the single
// month of January 2018. This program outputs the hashes
// (PositionHash) of such "common" positions.

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
constexpr int MIN_COMMON = 2;

using namespace std;
using int64 = int64_t;
using uint64 = uint64_t;

using Move = Position::Move;

struct PosHisto {
  // Just try this most trivial approach first.
  std::shared_mutex m;
  // Maps position hash to the total occurrences seen.
  std::unordered_map<uint64, int32> position_count;

  int64 MergeHisto(const std::unordered_map<uint64, int32> &lpc) {
    WriteMutexLock ml(&m);
    const int64 start = time(nullptr);
    total_positions += lpc.size();
    for (const auto &p : lpc) {
      position_count[p.first] += p.second;
    }
    return time(nullptr) - start;
  }
  int64 total_positions = 0LL;
};

static PosHisto *pos_histo = nullptr;

static int64 MemUsage() {
  PROCESS_MEMORY_COUNTERS pmc;
  CHECK(GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof pmc));
  return (int64)pmc.WorkingSetSize;
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

static void AddPackFile(const string &filename) {
  const int64 start = time(nullptr);
  fprintf(stderr, "Load %s...\n", filename.c_str());
  fflush(stderr);
  vector<uint8> bytes = Util::ReadFileBytes(filename);
  CHECK(!bytes.empty()) << filename;
  vector<pair<uint64_t, PackedGame>> packed_games =
    PackedGame::SplitFile(bytes);

  std::unordered_map<uint64, int32> lpc;

  const int64 num_games = packed_games.size();
  fprintf(stderr, "Read %lld games from %s. Running...\n",
          num_games, filename.c_str());
  fflush(stderr);

  for (const auto &p : packed_games) {
    const PackedGame &pg = p.second;

    // All moves should be legal.
    Position pos;
    for (int i = 0; i < pg.NumMoves(); i++) {
      Position::Move move = PackedGame::UnpackMove(pg.GetMove(i));
      // Just assume it's legal.
      // CHECK(pos.IsLegal(move));
      pos.ApplyMove(move);
      uint64 pos_hash = PositionHash{}(pos);
      lpc[pos_hash]++;
    }
  }

  const int64 num_pos = lpc.size();
  fprintf(stderr, "Got %lld distinct positions in %lld games from %s.\n",
          num_pos, num_games, filename.c_str());
  fflush(stderr);

  int64 secs = time(nullptr) - start;
  int64 insert_time = pos_histo->MergeHisto(lpc);
  double mem = MemUsage();
  fprintf(stderr, "Inserted %lld games from %s. "
          "(%llds total, %llds insert) [%.2f G mem]\n",
          num_games, filename.c_str(),
          secs, insert_time,
          (double)mem / 1000000000.0);
  fflush(stderr);
}

static void AddPackFiles(const std::vector<string> &filenames) {
  pos_histo = new PosHisto;

  int64 start = time(nullptr);
  ParallelApp(filenames,
              AddPackFile,
              MAX_PARALLELISM);

  int64 secs = time(nullptr) - start;

  fprintf(stderr, "Done loading (%lld positions) in %llds.\n",
          pos_histo->total_positions,
          secs);
  fflush(stderr);

  PrintMemoryUsage();

  // Get common positions.
  vector<uint64> common;
  fprintf(stderr, "Get common...\n");
  fflush(stderr);
  for (const auto &p : pos_histo->position_count) {
    if (p.second >= MIN_COMMON) {
      common.push_back(p.first);
    }
  }

  fprintf(stderr, "[Elapsed %lld] There are %lld 'common' positions. Sort...\n",
          time(nullptr) - start,
          common.size());
  fflush(stderr);

  std::sort(common.begin(), common.end());
  fprintf(stderr, "[Elapsed %lld] Write %lld positions...\n",
          time(nullptr) - start,
          common.size());
  fflush(stderr);
  CHECK(Util::WriteUint64File("common.u64", common));

  // Now, invert the histo.
  std::unordered_map<int32, int64> inverted;
  fprintf(stderr, "[Elapsed %lld] Invert...\n", time(nullptr) - start);
  fflush(stderr);
  for (const auto &p : pos_histo->position_count) {
    // How many positions have been seen e.g. exactly twice?
    // Might be good to also store an example, although we don't
    // have any good way to look these up today.
    inverted[p.second]++;
  }

  // Now output this histo.
  std::map<int64, int64> sorted;
  fprintf(stderr, "[Done in %lld sec.] Sort...\n",
          time(nullptr) - start);
  fflush(stderr);
  for (const auto &p : inverted) {
    sorted[p.first] = p.second;
  }

  fprintf(stderr, "[Done in %lld sec.] Output...\n",
          time(nullptr) - start);
  fflush(stderr);

  string csv;
  for (const auto &p : sorted) {
    // printf("%lld position(s) appeared %lld time(s)\n",
    // p.second, p.first);
    csv += StringPrintf("%lld,%lld\n", p.second, p.first);
  }
  fflush(stdout);
  Util::WriteFile("packhisto.csv", csv);


  sorted.clear();

  delete pos_histo;
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
