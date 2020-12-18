
#ifndef _LOWERCASE_LOADFONTS_H
#define _LOWERCASE_LOADFONTS_H

#include <functional>
#include <string>
#include <shared_mutex>
#include <thread>
#include <cstdint>
#include <memory>

#include "fontdb.h"
#include "ttf.h"

// Load fonts in the background.
struct LoadFonts {
  using int64 = int64_t;

  // Starts loading fonts into memory immediately, in separate
  // threads.
  LoadFonts(
      // If this returns true (e.g. because the startup process is
      // aborted), just stop loading.
      std::function<bool()> ExitEarly,
      int max_parallelism,
      int64 max_fonts);

  // Call once. Waits for font vector to be complete. After this
  // returns, safe to access the vector (from a single thread) without
  // taking the mutex.
  void Sync();

  std::shared_mutex fonts_m;
  // Protected by fonts_m.
  std::vector<TTF *> fonts;

private:
  void Init();

  const int max_parallelism;
  const int64 max_fonts;
  const std::function<bool()> ExitEarly;

  std::unique_ptr<FontDB> font_db;
  std::unique_ptr<std::thread> init_thread;
};


#endif
