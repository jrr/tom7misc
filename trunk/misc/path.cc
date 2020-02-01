
#include <cstdint>
#include <unordered_map>
#include <vector>
#include <ctime>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/heap.h"
#include "../cc-lib/base/stringprintf.h"

using namespace std;
using uint8 = uint8_t;

enum class Rules {
  // Triple and perfect kills reset health
  ZOMBIE,
  // Health resets health
  NORMAL,
  // Same as normal except dracula is always asleep,
  // and poison/snake eggs don't work (N/A here)
  ROBOT,
};

static constexpr Rules rules = Rules::ROBOT;

static constexpr int MAX_DAMAGE = 3;
static constexpr int EXIT_X = 4;
static constexpr int EXIT_Y = 0;
static constexpr int TARGET_XP = 50;

static constexpr bool PERFECT_KILLS = true; // true; // false;

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
  static constexpr int WIDTH = 9, HEIGHT = 7;
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

    SPIDER,
    SOLDIER,
    SKING,
    HEAL,
    WALL,
    PETAL,
    FLOWER,

    DRACULA,
    // MUSHROOM, // TODO
    // TODO: Green turtles
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
    if (damage > MAX_DAMAGE)
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
    case WALL:
      return false;
    case EMPTY:
      streak = 0;
      break;
    case SMALL:
    case PETAL:
      streak++;
      break;

    case HEAL:
      // XXX doesn't work for zombie, right?
      damage = 0;
      streak = 0;
      break;

    case SPIDER:
    case SOLDIER:
      xp++;
      streak++;
      damage++;
      break;

    case DRACULA:
      if (rules == Rules::ROBOT || damage == MAX_DAMAGE) {
	// is this right?
	if (rules == Rules::ZOMBIE) damage = 0;
	xp += 5;
	streak++;
      } else {
	if (PERFECT_KILLS)
	  return false;
	streak = 0;
	damage = MAX_DAMAGE;
      }
      break;
      
    case SKING: {
      int num_soldiers = 0;
      for (int i = 0; i < WIDTH * HEIGHT; i++) {
	if (tiles[i] == SOLDIER) {
	  if (!PERFECT_KILLS) tiles[i] = EMPTY;
	  num_soldiers++;
	}
      }
      if (num_soldiers > 0) {
	if (PERFECT_KILLS) return false;
	damage += 2;
	streak = 0;
      } else {
	if (rules == Rules::ZOMBIE) damage = 0;
	xp += 15;
	streak++;
      }
      break;
    }

    case FLOWER: {
      int num_petals = 0;
      auto Visit = [this, &num_petals](int ax, int ay) {
	  if (ax >= 0 && ax < WIDTH &&
	      ay >= 0 && ay < HEIGHT &&
	      TileAt(ax, ay) == PETAL) {
	    if (!PERFECT_KILLS)
	      SetTile(ax, ay, EMPTY);
	    num_petals++;
	  }
	};
      Visit((int)tx - 1, ty);
      Visit((int)tx + 1, ty);
      Visit(tx, (int)ty - 1);
      Visit(tx, (int)ty + 1);
      if (num_petals > 0) {
	if (PERFECT_KILLS) return false;
	damage += 2;
	streak = 0;
      } else {
	if (rules == Rules::ZOMBIE) damage = 0;
	xp += 5;
	streak++;
      }
      break;
    }
      
    case TN:
    case TS:
    case TE:
    case TW:
      if ((m == N && t == TN) ||
	  (m == S && t == TS) ||
	  (m == W && t == TW) ||
	  (m == E && t == TE)) {
	// Clean kill.
	if (rules == Rules::ZOMBIE) damage = 0;
	xp += 3;
	streak++;
      } else {
	if (PERFECT_KILLS) return false;
	// Takes damage.
	damage += 2;
	streak = 0;
      }
    }

    // Triple!
    if (streak == 3) {
      if (damage <= MAX_DAMAGE &&
	  rules == Rules::ZOMBIE) damage = 0;
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
    for (int yy = 0; yy < HEIGHT; yy++) {
      for (int xx = 0; xx < WIDTH; xx++) {
	if (xx == x && yy == y) {
	  ret += "@";
	} else {
	  switch (TileAt(xx, yy)) {
	  case EMPTY: ret += "."; break;
	  case LEASH: ret += "="; break;
	  case SMALL: ret += "o"; break;
	  case PETAL: ret += "*"; break;
	  case FLOWER: ret += "F"; break;
	  case DRACULA: ret += "D"; break;
	  case WALL: ret += "#"; break;
	  case SKING: ret += "$"; break;
	  case SOLDIER: ret += "S"; break;
	  case SPIDER: ret += "x"; break;
	  case HEAL: ret += "+"; break;
	  case TN: ret += "^"; break;
	  case TS: ret += "v"; break;
	  case TE: ret += ">"; break;
	  case TW: ret += "<"; break;
	  default: ret += "?"; break;
	  }
	}
      }
      ret += "\n";
    }
    return ret;
  }
  
  bool Prune() const {
    // n.b. this is covered by the below
    if (TileAt(EXIT_X, EXIT_Y) != EMPTY)
      return true;

    // Check to make sure the exit is still reachable, or else
    // prune.
    // We also insist that there is enough XP still reachable,
    // assuming perfect kills (and assuming we can streak
    // every entity regardless of where they are).
    
    // is v<b> actually good here?
    vector<int> reachable(WIDTH * HEIGHT, false);
    auto Test = [&reachable](int x, int y) {
	return !!reachable[y * WIDTH + x];
      };
    auto Set = [&reachable](int x, int y, bool b) {
	reachable[y * WIDTH + x] = b;
      };

    // When we mark a square as reachable, we increment
    // the entities count if the contents could contribute
    // to a streak.
    int reachable_ents = 0;
    // ... and the reachable_xp with the best XP that can
    // be gotten from killing the entity, modulo 
    int reachable_xp = 0;
    
    // Anything in q should already be marked reachable.
    Set(x, y, true);
    vector<pair<int, int>> q = {{x, y}};

    while (!q.empty()) {
      int xx, yy;
      std::tie(xx, yy) = q.back();
      q.pop_back();

      // XXX should avoid expanding through the exit; this
      // is not actually possible!
      
      // n.b. can exit early if we've met both the xp
      // and exit conditions...
      
      auto Try = [this, &Test, &Set, &reachable_ents, &reachable_xp, &q](
	  int xx, int yy) {
	  // Already marked reachable.
	  if (Test(xx, yy))
	    return;

	  // Entity/xp count.
	  switch (TileAt(xx, yy)) {
	  case LEASH:
	  case WALL:
	  case EMPTY:
	  case HEAL:
	    break;
	  case SMALL:
	  case PETAL:
	    reachable_ents++;
	    break;

	  case SPIDER:
	  case SOLDIER:
	    reachable_ents++;
	    reachable_xp++;
	    break;

	  case DRACULA:
	    reachable_xp += 5;
	    reachable_ents++;
	    break;
	  case SKING:
	    reachable_xp += 15;
	    reachable_ents++;
	    break;
	  case FLOWER:
	    reachable_xp += 5;
	    reachable_ents++;
	    break;
	  case TN:
	  case TS:
	  case TE:
	  case TW:
	    reachable_xp += 3;
	    reachable_ents++;
	    break;
	  default:
	    // Need to cover this case for correctness.
	    CHECK(false) << "unimplemented!";
	    break;
	  }

	  // Now reachability...
	  
	  switch (TileAt(xx, yy)) {
	  case WALL:
	  case LEASH:
	    // Impassable.
	    return;
	  default:
	    // Reachable.
	    Set(xx, yy, true);
	    q.emplace_back(xx, yy);
	    return;
	  }
	};
      if (xx > 0) Try(xx - 1, yy);
      if (xx < WIDTH - 1) Try(xx + 1, yy);
      if (yy > 0) Try(xx, yy - 1);
      if (yy < HEIGHT - 1) Try(xx, yy + 1);
    }

    int max_streak = streak + reachable_ents;
    int streak_xp = 3 * (max_streak / 3);

    int max_xp = xp + streak_xp + reachable_xp;
    if (max_xp < TARGET_XP)
      return true;

    if (!Test(EXIT_X, EXIT_Y)) {
      // Exit unreachable.
      return true;
    }

    return false;
  }

  bool IsGoal() const {
    /*
    if (xp < 18) return false;
    if (damage > 2) return false;
    if (x != 4 || y != 2) return false;
    return true;
    */

    return xp >= TARGET_XP &&
      damage <= 3 &&
      x == EXIT_X &&
      y == EXIT_Y;
  }

  double Heuristic() const {
    return
      // Negative = better
      -10000.0 * xp +
      -1000.0 * (MAX_DAMAGE - damage) +
      -100.0 * streak +
      // Manhattan distance
      std::abs((int)x - EXIT_X) +
      std::abs((int)y - EXIT_Y);
  }
  
  static State StartState() {
    /*
    Tile init[] = {
      EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
      EMPTY, TN,    SMALL, TN,    EMPTY,
      EMPTY, SMALL, TW,    SMALL, EMPTY,
      EMPTY, TW,    SMALL, TE,    EMPTY,
      EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
    };
    */

    /*
    Tile init[] = {
      SPIDER, SPIDER, HEAL, EMPTY, EMPTY, SPIDER, SPIDER, SPIDER, HEAL,
      SPIDER, EMPTY, SOLDIER, EMPTY, EMPTY, PETAL, FLOWER, EMPTY, EMPTY,
      EMPTY, WALL, PETAL, SOLDIER, EMPTY, EMPTY, PETAL, WALL, EMPTY,
      EMPTY, HEAL, FLOWER, EMPTY, WALL, SPIDER, SOLDIER, EMPTY, SKING,
      SPIDER, SOLDIER, PETAL, EMPTY, WALL, SPIDER, PETAL, FLOWER, PETAL,
      SPIDER, EMPTY, EMPTY, EMPTY, EMPTY, SPIDER, EMPTY, HEAL, EMPTY,
      SPIDER, HEAL, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, SPIDER, SPIDER,
    };
    */
    /*

    Tile init[] = {
      SPIDER, SPIDER, HEAL, EMPTY, EMPTY, DRACULA, EMPTY, HEAL, EMPTY,
      SPIDER, EMPTY, EMPTY, SPIDER, EMPTY, PETAL, EMPTY, EMPTY, SPIDER,
      EMPTY, WALL, DRACULA, SPIDER, SPIDER, FLOWER, EMPTY, WALL, SPIDER,
      HEAL, SPIDER, SPIDER, EMPTY, WALL, PETAL, EMPTY, EMPTY, SPIDER,
      DRACULA, EMPTY, SPIDER, EMPTY, WALL, EMPTY, EMPTY, EMPTY, HEAL,
      SPIDER, PETAL, FLOWER, EMPTY, EMPTY, HEAL, EMPTY, EMPTY, EMPTY,
      SPIDER, SPIDER, PETAL, EMPTY, EMPTY, EMPTY, SPIDER, SPIDER, SPIDER,
    };
    */    

    /*
    Tile init[] = {
      EMPTY, EMPTY, SPIDER, EMPTY, EMPTY, EMPTY, SPIDER, SPIDER, EMPTY,
      SMALL, SPIDER, FLOWER, PETAL, SPIDER, EMPTY, EMPTY, SPIDER, EMPTY,
      SPIDER, WALL, PETAL, SMALL, SPIDER, EMPTY, EMPTY, WALL, EMPTY,
      EMPTY, SPIDER, SPIDER, EMPTY, WALL, EMPTY, EMPTY, EMPTY, EMPTY,
      EMPTY, EMPTY, SPIDER, EMPTY, WALL, SPIDER, EMPTY, PETAL, SMALL,
      SMALL, PETAL, FLOWER, EMPTY, SPIDER, SPIDER, EMPTY, FLOWER, SPIDER,
      SPIDER, SPIDER, PETAL, EMPTY, EMPTY, EMPTY, EMPTY, PETAL, SPIDER,
    };
    */

    auto _ = EMPTY;
    auto S = SPIDER;
    auto H = HEAL;
    auto W = WALL;
    auto O = SMALL;
    auto D = DRACULA;
    auto R = SOLDIER;
    auto K = SKING;
    (void)O;
    (void)H;
    (void)S;
    (void)D;
    (void)R;
    (void)K;
    
    /*
    Tile init[] = {
      S, S, H, _, _, O, S, _, S,
      S, _, _, S, _, D, S, _, H,
      H, W, _, S, S, _, _, W, D,
      D, _, _, _, W, _, _, S, O,
      S, _, _, _, W, _, _, _, S,
      S, _, _, O, O, S, _, H, _,
      S, _, H, _, _, _, S, S, S,
    };
    */

    /*
    Tile init[] = {
      S, S, D, _, _, _, _, _, _,
      S, _,TN, _, S, S, S, _, _,
      _, W, S, D, _, _, _, W, _,
      _, S, S, _, W, _,TW, S, S,
      _, _, _, _, W, _, _, D, S,
      _, _, _, _, _, S, S, _, _,
      S, S, S, _, _, _, S, _, _,
    };
    */

    Tile init[] = {
      S, S, H, _, _, S, S, _, H,
      S, _, _, R, K, R, S, _, O,
      _, W, _, _, _, _, _, W, O,
      _, _, _, S, W, _, _, _, S,
      _, _, S, O, W, H, _, _, _,
      O, H, R, _, _, _, R, S, _,
      S, S, _, _, _, _, S, S, H,
    };

    static_assert(sizeof(init) == WIDTH * HEIGHT);
    
    State start;
    for (int i = 0; i < WIDTH * HEIGHT; i++) {
      start.tiles[i] = init[i];
    }

    // start.x = 0;
    // start.y = 2;
    start.x = 4;
    start.y = 6;
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

static void Tests2() {
  {
    State s = State::StartState();
    for (Move m : { W, N, N, W, W, N, W, N, N, E, E, S, S }) {
      printf("%s\n", s.ToString().c_str());
      CHECK(s.MakeMove(m));
    }
    printf("%s\n", s.ToString().c_str());
  }
}

static void Tests3() {
  State s = State::StartState();
  for (Move m : {
      N, E, N, N, N, W, W, W, S, E, S, S, S, W, W, W, N, E, E, N,
	W, N, W, N, N, E
    }) {
    printf("%s\n", s.ToString().c_str());
    CHECK(s.MakeMove(m));
  }
  printf("%s\n", s.ToString().c_str());
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
  printf("Hi?\n");
  fflush(stdout);
  
  (void)Tests;
  (void)Tests2;
  (void)Tests3;
  // Tests2();
  // return 0;
  // Tests();
  // return 0;
  // Tests();
  StateHeap heap;
  StateTable table;

  State start = State::StartState();
  printf("Start:\n%s\n", start.ToString().c_str());
  fflush(stdout);
  
  Item *start_item = new Item(start, {});
  table[start] = start_item;
  heap.Insert(0, start_item);

  const int64 time_start = time(nullptr);
  int64 num_states = 1, num_iters = 0, pruned = 0;
  while (!heap.Empty()) {
    StateHeap::Cell cell = heap.PopMinimum();
    Item *item = cell.value;

    if (item->state.Prune()) {
      pruned++;
      continue;
    }
    
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
	// int num_moves = (int)moves.size();
	double h = s.Heuristic();
	Item *new_item = new Item(s, std::move(moves));
	table[s] = new_item;
	heap.Insert(h, new_item);
	num_states++;
      }
    }
    
    num_iters++;
    if (num_iters % 10000 == 0) {
      int64 elapsed = time(nullptr) - time_start;
      double ssec = num_states / (double)elapsed;
      printf("%lld iters, %lld states, %lld pruned, %.1f st/sec\n",
	     num_iters, num_states, pruned, ssec);
      printf("%s\n", item->state.ToString().c_str());
      fflush(stdout);
    }
  }

  printf("No solution!\n");
  return 1;
}
