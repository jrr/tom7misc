// Snapshot of fate data from all games.

#ifndef _ALL_FATE_DATA_H
#define _ALL_FATE_DATA_H

struct LivedDied {
  double lived, died;
};

extern LivedDied all_fate_data[32][64];

#endif
