// Implementation of custom logic: Turning on relays via
// GPIO, alerts, etc.
//
// It would be kind of "nicer" if the logic were part of
// some configuration, but since tempo is likely to only
// be used by me, this is the simplest way to do it.

#ifndef __TEMPO_LOGIC_H
#define __TEMPO_LOGIC_H

struct Database;

struct Logic {

  // All static to simplify interfacing with hardware.
  // Can assume bcm_2835 is initialized.
  static void Initialize();

  // Runs every second or so in the main thread; should do its own
  // throttling.
  static void Periodic(Database *db);

};

#endif
