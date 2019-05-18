
#include "stockfish.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include <string>
#include <mutex>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/util.h"

#include "subprocess.h"

using namespace std;

// I compiled stockfish on mingw with:
// make -j build ARCH=x86-64 COMPCXX=x86_64-w64-mingw32-c++ COMP=mingw
// note that x86-64-modern just segfaults. Might be a performance
// win here if I could figure out why.

Stockfish::Stockfish(int level, int64 nodes) : level(level), nodes(nodes) { }

void Stockfish::InitEngine() {
  if (subprocess.get() != nullptr)
    return;
  subprocess.reset(
      Subprocess::Create("stockfish.exe"));
	  // "..\\..\\stockfish\\src\\stockfish.exe"));
  CHECK(subprocess.get());

  subprocess->Write("uci\n");

  string line;
  while (subprocess->ReadLine(&line)) {
    // printf("[%s]\n", line.c_str());
    // fflush(stdout);
    if (line == "uciok") break;
  }

  subprocess->Write(StringPrintf("setoption name Skill Level value %d\n", level));

  // XXX: Set time limit (better to do this in go call I think?).
}

void Stockfish::GetMove(const string &fen, string *move, Score *score) {
  // XXX clear hash?
  MutexLock ml(&subprocess_m);
  InitEngine();
  if (nodes > 0) {
    subprocess->Write(StringPrintf("position fen %s\ngo nodes %lld\n", fen.c_str(), nodes));
  } else {
    subprocess->Write(StringPrintf("position fen %s\ngo\n", fen.c_str()));
  }
    
  string line;
  string info;
  do {
    CHECK(subprocess->ReadLine(&line));
    if (line.find("info") == 0) info = line;
  } while (line.find("bestmove") != 0);

  CHECK("bestmove" == Util::chop(line));
  *move = Util::chop(line);

  // printf("%s -> [%s]\n", fen.c_str(), move->c_str());
  // fflush(stdout);
  
  // On an info line, score has one of these two forms (from uci.cpp):
  /// cp <x>    The score from the engine's point of view in centipawns.
  /// mate <y>  Mate in y moves, not plies. If the engine is getting mated
  ///           use negative values for y.

  if (score != nullptr) {
    string tok;
    while (!(tok = Util::chop(info)).empty()) {
      if (tok == "score") {
	string typ = Util::chop(info);
	string val = Util::chop(info);
	if (typ == "cp") {
	  score->is_mate = false;
	} else if (typ == "mate") {
	  score->is_mate = true;
	} else {
	  LOG(FATAL) << "Unknown score type " << typ;
	}

	score->value = atoi(val.c_str());
	break;
      }
    }
  }
}
