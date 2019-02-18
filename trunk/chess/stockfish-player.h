
#ifndef __STOCKFISH_PLAYER_H
#define __STOCKFISH_PLAYER_H

struct StatelessPlayer;

StatelessPlayer *CreateStockfish0();
StatelessPlayer *CreateStockfish5();
StatelessPlayer *CreateStockfish10();
StatelessPlayer *CreateStockfish15();
StatelessPlayer *CreateStockfish20();
StatelessPlayer *CreateStockfish1M();

StatelessPlayer *CreateWorstfish();

#endif
