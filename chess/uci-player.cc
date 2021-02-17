
#include <string>
#include <memory>
#include <mutex>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/util.h"

#include "player.h"
#include "subprocess.h"
#include "chess.h"
#include "player-util.h"

using Move = Position::Move;
using namespace std;

static constexpr bool VALIDATE = true;

namespace {
// This is kept file-local to avoid requiring the windows-only
// Subprocess in the header.
struct UciPlayer : public StatelessPlayer {
  UciPlayer(const string &exe,
            // Series of setoption commands (or whatever); each
            // terminated with \n.
            const string &options,
            // Options for the "go" command, like "depth 10".
            // Driver will wait for the subprocess to respond
            // with "bestmove".
            const string &go_settings,
            const string &name,
            const string &desc);

  Move MakeMove(const Position &pos, Explainer *explainer) override;

  string Name() const override { return name; }
  string Desc() const override { return desc; }

private:
  // Initialize the subprocess if it hasn't happened yet.
  // We do this lazily so that we can reduce the peak
  // number of processes open during the tournament.
  // Must hold the mutex.
  void InitEngine();

  const string exe, options, go_settings, name, desc;

  std::mutex subprocess_m;
  std::unique_ptr<Subprocess> subprocess;
};

UciPlayer::UciPlayer(const string &exe,
                     const string &options,
                     const string &go_settings,
                     const string &name,
                     const string &desc)
  : exe(exe), options(options), go_settings(go_settings),
    name(name), desc(desc) { }

void UciPlayer::InitEngine() {
  if (subprocess.get())
    return;
  subprocess.reset(Subprocess::Create(exe));
  CHECK(subprocess.get()) << exe;

  subprocess->Write("uci\n");

  string line;
  while (subprocess->ReadLine(&line)) {
    if (line == "uciok") break;
  }

  if (!options.empty())
    subprocess->Write(options);
}


Move UciPlayer::MakeMove(const Position &orig_pos, Explainer *explainer) {
  // XXX: In endgames, the move clock matters. Should
  // perhaps be providing this.
  const string fen = orig_pos.ToFEN(10, 10);
  string move_s;
  {
    MutexLock ml(&subprocess_m);
    InitEngine();
    CHECK(subprocess.get());
    subprocess->Write(StringPrintf("position fen %s\ngo %s\n",
                                   fen.c_str(), go_settings.c_str()));

    string line;
    // string info;
    do {
      CHECK(subprocess->ReadLine(&line));
      // if (line.find("info") == 0) info = line;
    } while (line.find("bestmove") != 0);

    CHECK("bestmove" == Util::chop(line));
    move_s = Util::chop(line);
  }

  Move m;
  CHECK(PlayerUtil::ParseLongMove(move_s, orig_pos.BlackMove(), &m))
    << orig_pos.BoardString()
    << "\n" << fen
    << "\n[" << move_s << "]";
  if (VALIDATE) {
    Position pos = orig_pos;
    CHECK(pos.IsLegal(m)) << exe;
  }

  return m;
}

}  // namespace

Player *Topple1M() {
  return new MakeStateless<
    UciPlayer, string, string, string,
    string, string>("engines\\topple_v0.3.5_znver1.exe",
                    "",
                    "nodes 1000000",
                    "topple1m",
                    "Topple 0.3.5, 1M nodes.");
}

Player *Topple10K() {
  return new MakeStateless<
    UciPlayer, string, string, string,
    string, string>("engines\\topple_v0.3.5_znver1.exe",
                    "",
                    "nodes 10000",
                    "topple10k",
                    "Topple 0.3.5, 10k nodes.");
}
