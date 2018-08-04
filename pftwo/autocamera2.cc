#include "autocamera2.h"

#include <vector>


AutoCamera2::AutoCamera2(const string &game) {
  emu.reset(Emulator::Create(game));
  CHECK(emu.get() != nullptr) << game;
}

