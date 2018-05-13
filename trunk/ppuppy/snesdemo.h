#ifndef __SNESDEMO_H
#define __SNESDEMO_H

#include "ppuppy.h"
#include "screen.h"
#include <vector>
#include <string>
#include <thread>
#include <memory>
#include "arcfour.h"
#include "talk.h"

struct SNES {
  SNES(const string &cart);

  void Update(uint8 joy1, uint8 joy2);
  Screen *GetScreen();
  Screen *GetNextScreen();
  Screen screen;
 private:
  void Run();
  std::unique_ptr<std::thread> th;
};

#endif
