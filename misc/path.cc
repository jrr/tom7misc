
#include <cstdint>
#include <unordered_map>
#include <vector>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/base/stringprintf.h"

using namespace std;
using uint8 = uint8_t;

enum Move : uint8 {
  N, S, E, W, BARK,
};

char MoveChar(Move m) {
  switch (m) {
  case N: return 'N';
  case S: return 'S';
  case E: return 'E';
  case W: return 'W';
  case BARK: return '!';
  default: return '?';
  }
}

struct State {
  static constexpr int WIDTH = 5, HEIGHT = 5;
  // TODO: Would be a good place to use a packed fixed-length
  // vector kinda thing, since we only need 3 bits per.
  enum Tile : uint8 {
    EMPTY,
    // Dog is always on 'empty' tile.
    // DOG,
    LEASH,
    SMALL,
    // Turtles. Direction of the main 'head'
    TN,
    TS,
    TE,
    TW,
  };
  
  Tile tiles[WIDTH * HEIGHT];
  uint8 x, y, streak, xp, damage;

  Tile TileAt(int xx, int yy) const {
    return tiles[WIDTH * yy + xx];
  }
  void SetTile(int xx, int yy, Tile t) {
    tiles[WIDTH * yy + xx] = t;
  }
  
  bool MakeMove(Move m) {
    if (damage > 2)
      return false;
    
    int dx = 0, dy = 0;
    switch (m) {
    case BARK: {
      auto Visit = [this](int ax, int ay, Tile turtle) {
	  if (ax >= 0 && ax < WIDTH &&
	      ay >= 0 && ay < HEIGHT) {
	    switch (TileAt(ax, ay)) {
	    case SMALL:
	      SetTile(ax, ay, EMPTY);
	      break;
	    case TN:
	    case TS:
	    case TE:
	    case TW:
	      SetTile(ax, ay, turtle);
	      break;
	    default:;
	    }
	  }
	};
      Visit((int)x - 1, y, TE);
      Visit((int)x + 1, y, TW);
      Visit(x, (int)y - 1, TS);
      Visit(x, (int)y + 1, TN);
      return true;
      break;
    }
    case N:
      if (y == 0) return false;
      dy = -1;
      break;
    case S:
      if (y == HEIGHT - 1) return false;
      dy = 1;
      break;
    case E:
      if (x == WIDTH - 1) return false;
      dx = 1;
      break;
    case W:
      if (x == 0) return false;
      dx = -1;
      break;
    }

    const int tx = x + dx, ty = y + dy;
    const int t = TileAt(tx, ty);
    switch (t) {
    case LEASH:
      return false;
    case EMPTY:
      streak = 0;
      break;
    case SMALL:
      streak++;
      break;
    case TN:
    case TS:
    case TE:
    case TW:
      if ((m == N && t == TN) ||
	  (m == S && t == TS) ||
	  (m == W && t == TW) ||
	  (m == E && t == TE)) {
	// Clean kill.
	xp += 3;
	streak++;
      } else {
	// Takes damage.
	damage += 2;
	streak = 0;
      }
    }

    // Triple!
    if (streak == 3) {
      xp += 3;
      streak = 0;
    }
    
    SetTile(x, y, LEASH);
    SetTile(tx, ty, EMPTY);
    x = tx;
    y = ty;
    return true;
  }

  string ToString() const {
    string ret = StringPrintf("%d,%d  %d xp %d streak %d dmg\n",
			      x, y, xp, streak, damage);
    for (int y = 0; y < HEIGHT; y++) {
      for (int x = 0; x < WIDTH; x++) {
	switch (TileAt(x, y)) {
	case EMPTY: ret += "."; break;
	case LEASH: ret += "="; break;
	case SMALL: ret += "o"; break;
	case TN: ret += "^"; break;
	case TS: ret += "v"; break;
	case TE: ret += ">"; break;
	case TW: ret += "<"; break;
	}
      }
      ret += "\n";
    }
    return ret;
  }
  
  bool IsGoal() const {
    // if (xp < 18) return false;
    if (xp < 18) return false;
    if (damage > 2) return false;
    // if (x != 4 || y != 2) return false;
    return true;
  }

  // TODO: IsDeadEnd, etc.

  static State StartState() {
    Tile init[] = {
      EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
      EMPTY, TN,    SMALL, TN,    EMPTY,
      EMPTY, SMALL, TW,    SMALL, EMPTY,
      EMPTY, TW,    SMALL, TE,    EMPTY,
      EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
    };

    State start;
    for (int i = 0; i < WIDTH * HEIGHT; i++) {
      start.tiles[i] = init[i];
    }

    start.x = 0;
    start.y = 2;
    start.xp = 0;
    start.damage = 0;
    start.streak = 0;
    return start;
  }
};

inline bool operator==(const State &a, const State &b) {
  if (a.x != b.x || a.y != b.y || a.streak != b.streak ||
      a.damage != b.damage || a.xp != b.xp) return false;
  for (int i = 0; i < State::WIDTH * State::HEIGHT; i++)
    if (a.tiles[i] != b.tiles[i])
      return false;
  return true;
}

struct StateHash {
  size_t operator()(const State& k) const {
    uint64 values = ((uint64)k.x << 30) ^
      ((uint64)k.y << 26) ^
      ((uint64)k.xp << 20) ^
      ((uint64)k.damage << 16) ^
      ((uint64)k.streak << 14);
    for (int y = 0; y < State::HEIGHT; y++) {
      uint64 row = 0LL;
      for (int x = 0; x < State::WIDTH; x++) {
	values <<= 3;
	values ^= k.TileAt(x, y);
      }
      values ^= row;
      values *= 31337;
    }
    return (size_t)values;
  }
};

static void Tests() {
  {
  State start = State::StartState();
  const State start1 = State::StartState();

  CHECK(!start.MakeMove(W));
  CHECK(start == start);
  CHECK(start == start1);
  CHECK(start.MakeMove(E));
  CHECK(!(start == start1));
  CHECK(start.streak == 1);
  CHECK(!start.MakeMove(W));

  printf("%s\n", start.ToString().c_str());
  }

  {
  State s = State::StartState();
  CHECK(s.MakeMove(S));
  CHECK(s.MakeMove(S));
  CHECK(s.MakeMove(E));
  CHECK(s.MakeMove(E));
  CHECK(s.MakeMove(N));
  CHECK(s.streak == 1);
  CHECK(s.MakeMove(E));
  CHECK(s.xp == 3);
  CHECK(s.streak == 2);
  CHECK(s.MakeMove(N));
  CHECK(s.xp == 6);
  CHECK(s.streak == 0);
  printf("%s\n", s.ToString().c_str());
  }

  {
    State s = State::StartState();
    for (Move m : { S, S, E, E,
	  N, E, N, W,
	  W, N, E, BARK,
	  N, E, E, S, W}) {
      CHECK(s.MakeMove(m));
    }
    printf("%s\n", s.ToString().c_str());
  }
}

struct Item : public Heapable {
  State state;
  vector<Move> moves;
  Item(State state, vector<Move> moves) :
    state(std::move(state)), moves(std::move(moves)) {}
};

using StateHeap = Heap<int, Item>;
using StateTable = unordered_map<State, Item *, StateHash>;

int main(int argc, char **argv) {
  (void)Tests;
  Tests();
  // return 0;
  // Tests();
  StateHeap heap;
  StateTable table;

  State start = State::StartState();
  Item *start_item = new Item(start, {});
  table[start] = start_item;
  heap.Insert(0, start_item);

  int64 num_states = 1, num_iters = 0;
  while (!heap.Empty()) {
    StateHeap::Cell cell = heap.PopMinimum();
    Item *item = cell.value;

    for (Move m : {BARK, N, S, E, W}) {
      State s = item->state;
      if (s.MakeMove(m)) {
	// Have we already been here?
	if (table.find(s) != table.end()) {
	  // TODO: Replace with shorter path if
	  // possible. For now we don't even care.
	  continue;
	}

	vector<Move> moves = item->moves;
	moves.push_back(m);
	
	// Did we win?
	if (s.IsGoal()) {
	  printf("Done!\n");
	  printf("%s\n", s.ToString().c_str());
	  for (Move mm : moves) {
	    printf("%c, ", MoveChar(mm));
	  }
	  printf("\n");
	  return 0;
	}

	// Otherwise, push and continue...
	int num_moves = (int)moves.size();
	Item *new_item = new Item(s, std::move(moves));
	table[s] = new_item;
	heap.Insert(num_moves, new_item);
	num_states++;
      }
    }
    num_iters++;
    if (num_iters % 10000 == 0) {
      printf("%lld iters, %lld states\n",
	     num_iters, num_states);
      fflush(stdout);
    }
  }

  printf("No solution!\n");
  return 1;
}
