
#include "stockfish.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

// #define pipe(fds) _pipe(fds,4096, _O_BINARY) 

// no pseudoterminal support on windows
// #include <pty.h>
  // CHECK(0 == forkpty(&amaster, nullptr, nullptr, nullptr));

#include <string>

#include "../cc-lib/base/logging.h"

#include "subprocess.h"

using namespace std;

// I compiled stockfish on mingw with:
// make -j build ARCH=x86-64 COMPCXX=x86_64-w64-mingw32-c++ COMP=mingw
// note that x86-64-modern just segfaults. Might be a performance
// win here if I could figure out why.

Stockfish::Stockfish(int ) {
  subprocess.reset(
      Subprocess::Create(
	  "..\\..\\stockfish\\src\\stockfish.exe"));
  CHECK(subprocess.get());

  subprocess->Write("uci\n");

  string line;
  while (subprocess->ReadLine(&line)) {
    // printf("[%s]\n", line.c_str());
    // fflush(stdout);
    if (line == "uciok") break;
  }
  
#if 0
  printf("Try open stockfish...\n");
  fflush(stdout);

  int amaster = 0;


  const char *argv[2] = {"stockfish.exe", nullptr};
  const char *env[1] = {nullptr};
  PipeChild("..\\..\\stockfish\\src\\stockfish.exe",
	    argv, env, "uci\n");
#endif
  
  #if 0
  // alas, no bidirectional popen in mingw.
  FILE *f = popen("..\\..\\stockfish\\src\\stockfish.exe", "r+");
  CHECK(f);

  fprintf(f, "uci\n");
  for (;;) {
    string line = freadline(f);
    printf("line [%s]\n", line.c_str());
    if (line == "uciok") {
      break;
    }
  }

  // XXX
  pclose(f);
#endif
}
