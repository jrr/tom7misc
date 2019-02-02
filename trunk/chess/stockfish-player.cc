
#include "player.h"

struct StockfishPlayer : public Player {

  StockfishPlayer() {
    
  }
  
  Move MakeMove(const Position &orig_pos) override {
    Position pos = orig_pos;

  }
  
  const char *Name() const override { return "stockfish"; }
  const char *Desc() const override {
    return "Stockfish engine.";
  }
};
