
/* This "extended" ricochet robot solver allows for some extra stuff.
   None of this is in pure ricochet robot, so I don't want to pollute
   that code. However, it is useful for, say, solving Escape levels.

   I've extended that extended solver even more. Now it's almost
   special-purposed to solving a specific Escape level that is
   tormenting me.

   (you can see the earlier revisions for some history)

   This version doesn't look for shortest solutions; instead it just
   tries to enumerate all reachable states. This is feasible for
   small levels.

*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

/* max number of moves */
#define MAX_DEPTH 4096

/* you'll need to adjust hash_code along with this, since it
   assumes 5 */
#define NUM_ROBOTS 5

const char version[] = 
   "$Id: ricochetex2.cpp,v 1.3 2005/06/03 15:38:47 tom7 Exp $";

/* Ricochet Robot is played on a 16x16 grid. This program represents
   it as an 18x18 array, so that the edges don't have to be handled as
   a special case; the cells on the boundary are always solid walls.
   Well, actually it's a 1-dimensional array with 324 (18*18)
   elements, but it acts like an 18x18 array in row-major order.

   XXX/PERF: If I used the bits to denote permission to leave, rather
   than enter, then I could use a 16x16 grid instead.

   Each element of the array is a char. The char has a bit for each
   direction; if the bit is 1, there is a wall that prevents entry by
   robots moving in that direction. See definitions of DOWN, UP, RIGHT
   and LEFT below. In addition, bit 4 (the "16" bit) is set if there
   is a robot in that location. So, for example, if a robot is moving
   down, it can enter a cell on the grid as long as bit 0 and bit 4
   are both 0; otherwise it can't.
*/

#define BOARD_WIDTH 16
#define BOARD_HEIGHT 16

#define BOARD_REP_WIDTH (BOARD_WIDTH+2)
#define BOARD_REP_HEIGHT (BOARD_HEIGHT+2)

#define DOWN 1
#define UP 2
#define RIGHT 4
#define LEFT 8
#define BOT 16

char *direction_names[] = {
  NULL,
  "down",
  "up",
  NULL,
  "right",
  NULL,
  NULL,
  NULL,
  "left"
};

/* This is the number you have to add to an array index to get the
   index of the cell in that direction. */
int direction_deltas[] = { 0, BOARD_REP_WIDTH, -BOARD_REP_WIDTH, 0, 1,
                           0, 0, 0, -1 };

int opposite_dirs[] = { 0, UP, DOWN, 0, LEFT,
                           0, 0, 0, RIGHT, };

/* XXX what works best here? char? int?
   don't forget to change the memcpys...
*/
char board[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
char initial_board[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];

/* prevent robots from ever entering this territory. these are "dead
   end spots" (easy for a human to mark with = on the board diagram),
   which can speed up search */
char avoid[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];

void search_all();

/* Index of the goal (target). */
int goal_location;
/* Index of each robot's location. */

int robot_location[NUM_ROBOTS];

/* this stores our solution so far */
int current_depth = 0;
int move_src[MAX_DEPTH];
int move_dest[MAX_DEPTH];

/* Put robot n on the board at column x, row y (for initialization only). */
void place_robot( int n, int x, int y ) {
  robot_location[n] = y*BOARD_REP_WIDTH+x;
  board[ y*BOARD_REP_WIDTH+x ] |= 0x10;
}

void board_error( int linenum, char *message ) {
  printf("Error in board at line %d: %s\n", linenum, message);
  exit(-1);
}

/* read_board expects the file to look something like this:

+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|       |               |       |
+ + + + + + + + + + + + + +-+ + +
|                         |     |
+ + + + + + + + + + + + + + + + +
|           |       |           |
+ + + + + +-+ + + +-+ + + + + + +
|                               |
+ + +-+ + + + + + + + + + + + +-+
|    3|                         |
+-+ + + + + + + + + + + + + + + +
|             |             |   |
+ +-+ + + + + +-+ + + +-+ + +-+ +
| |2                    |       |
+ + + + + + + +-+-+ + + + + + + +
|             |   |             |
+ + + + + + + +   + + + + + + + +
|             |   |             |
+ + + + + + + +-+-+ + + +-+ + + +
|       |                0|     |
+ + + + +-+ +-+ + + + + + + + +-+
|           |                   |
+-+ + + + + + + + + + + + + + + +
|                               |
+ + + + + + + +-+ +-+ + + + + + +
|               | |             |
+ +-+ + + + + + + + + + + + + + +
|   |                       |   |
+ + + + + + + + + + + + + + +-+ +
|      @|               |       |
+ + + +-+ + + + + + + +-+ + + + +
|         |                1|   |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

 The +s are optional but highly recommended. The border must be filled
 (no leaks!). The robot '0' is the goal robot; he must reach the @
 sign. We now also support:

 < and > (one-way vertical walls) XXX support ^ and v for horiz
 = for spaces that should be avoided (they are useless)
*/

/* read the board according to the format above. */

void read_board( char *filename ) {

  int x, y;

  int target_found = 0;
  int robotx[NUM_ROBOTS], roboty[NUM_ROBOTS], robotfound[NUM_ROBOTS];
  int targetx, targety;
  char c;

  FILE *f = fopen( filename, "r" );

  if (f==NULL) {
    printf("Can't open file %s\n", filename);
    exit(-1);
  }

  /* clear it all out */
  for (y=0; y<BOARD_REP_HEIGHT; y++) {
    for (x=0; x<BOARD_REP_WIDTH; x++) {
      board[ y*BOARD_REP_WIDTH+x ] = 0;
    }
  }
  
  for (x=0; x<NUM_ROBOTS; x++) robotfound[x] = 0;

  for (y=1; ; y++) {
    do {
      c = fgetc( f );
      if (feof(f))
        board_error( y*2-1, "Unexpected end of file" );
    } while (c!='+');

    for (x=1; x<=BOARD_WIDTH; x++) {
      c =  fgetc( f );

      /* XXX support v and ^ for one-way ud floors */
      if (c=='-') {
        board[ (y-1)*BOARD_REP_WIDTH + x ] |= UP;
        board[ y*BOARD_REP_WIDTH + x ] |= DOWN;
      }
      else if (c!=' ')
        board_error( y*2-1, "space or - expected" );
      c = fgetc( f );
      if (c!='+' && c!=' ') board_error( y*2-1, "space or + expected" );
    }

    if (y>BOARD_HEIGHT) break;

    /* force each line to start with | */
    do {
      c = fgetc( f );
      if (feof(f))
        board_error( y*2, "Unexpected end of file" );
    } while (c!='|');

    board[ y*BOARD_REP_WIDTH ] |= LEFT;
    board[ y*BOARD_REP_WIDTH + 1 ] |= RIGHT;

    for (x=1; x<=BOARD_WIDTH; x++) {

      /* clear avoid to start */
      avoid[y * BOARD_REP_WIDTH + x] = 0;

      c =  fgetc( f );
      switch (c) {
	/* 'avoid' spot */
      case '=':
	avoid[y * BOARD_REP_WIDTH + x] = 1;
	break;

      case '@':
        if (target_found) board_error( y*2, "duplicate target" );
        target_found = 1;
        targetx = x;
        targety = y;
        break;

      case ' ':
        break;
      default:
	if (c >= '0' && c <= '9') {

	  int cc = c - '0';
	  if (cc >= NUM_ROBOTS) board_error ( y * 2, 
					      "robot exceeds NUM_ROBOTS");
	  if (robotfound[cc]) board_error (y*2, "duplicate robot");
	  robotfound[cc] = 1;
	  robotx[cc] = x; roboty[cc] = y;
	  break;

	} else {
	  board_error( y*2, "0-9, =, @, or space expected" );
	}
      }

      c = fgetc( f );
      if (c=='|') {
        board[ y*BOARD_REP_WIDTH + x ] |= LEFT;
        board[ y*BOARD_REP_WIDTH + x + 1 ] |= RIGHT;
      } else if (c == '>') {
	board[ y*BOARD_REP_WIDTH + x ] |= LEFT;
      } else if (c == '<') {
	board[ y*BOARD_REP_WIDTH + x + 1 ] |= RIGHT;
      } else if (c!=' ')
        board_error( y*2, "|, <, >, or space expected" );
    }
  }

  fclose( f );

  if (!target_found)
    board_error( 34, "No target found" );

  for (x=0; x < NUM_ROBOTS; x++) {
    if (!robotfound[x]) {
      printf("Robot %d missing: considered complete\n", x);
      robot_location[x] = -1;
    } else {
      place_robot( x, robotx[ x ], roboty[ x ] );
    }
  }

  goal_location = targety*BOARD_REP_WIDTH+targetx;

  memcpy(initial_board, board, 
	 sizeof(char) * BOARD_REP_WIDTH * BOARD_REP_HEIGHT);
  //  memcpy(initial_bots, robot_location,
  // sizeof(int) * NUM_ROBOTS);
}

/* print the board to stdout. The array markspots
   contains a list of numspots spots to mark with 
   an X. */
void print_board(int * markspots, int numspots) {
  int x, y, loc, n, check_goal, s;
  for (y=1; ; y++) {
    for (x=1; ; x++) {
      putc( '+', stdout );

      if (x == BOARD_REP_WIDTH-1) break;

      if ( board[ y*BOARD_REP_WIDTH+x ] & DOWN )
        putc( '-', stdout );
      else
        putc( ' ', stdout );
    }
    putc( '\n', stdout );

    if (y == BOARD_REP_HEIGHT-1) break;

    for (x=1; ; x++) {
      int loc = y*BOARD_REP_WIDTH+x;

      /* XXX support < and > */
      if ( board[ loc ] & RIGHT )
        putc( '|', stdout );
      else
        putc( ' ', stdout );

      if (x == BOARD_REP_WIDTH-1) break;

      check_goal = 1;

#if 1
      for (s = 0; s < numspots; s ++) {
        if (markspots[s] == loc) {
          putc('X', stdout);
          goto next;
        }
      } 
#endif
      for (n=0; n<NUM_ROBOTS; n++) {
        if (loc == robot_location[n]) {
          if (board[loc] & BOT) putc('0' + n, stdout);
          else putc('a' + n, stdout); /* inconsistent! */
          check_goal = 0;
          break;
        }
      }
      if (check_goal) {
        if (board[loc] & BOT) putc('!', stdout);
        else if (loc == goal_location)
          putc('@', stdout);
        else if (avoid[loc])
	  putc('=', stdout);
	else
          putc(' ', stdout );
      }
    next:;
    }
    putc( '\n', stdout );
  }
  for(x=0; x < NUM_ROBOTS; x++) {
    printf("(#%d at %d/%d) ",
           x, robot_location[x] % BOARD_REP_WIDTH,
           robot_location[x] / BOARD_REP_WIDTH);
  }
  printf("\n");

}

/* remove a robot at old_loc */
void remove_robot( int old_loc ) {
  board[ old_loc ] &= 0xEF;
}

/* Modify the board to place robot n at new_loc. He isn't removed from
   his current location, if any! */
void put_robot( int n, int new_loc ) {
  robot_location[n] = new_loc;
  board[ new_loc ] |= 0x10;
}

/* hash code for the board. this is complicated by
   the fact that we need to consider boards equal
   up to permutation of the robots. */
inline void hash_code(unsigned int & h, unsigned int & l) {
  int sorted[NUM_ROBOTS];
  
  for(int i = 0; i < NUM_ROBOTS; i ++) {
    sorted[i] = robot_location[i];
  }

  /* bubble sort. for an array of size 5, this is not a bad choice
     (best would be to generate comparison code specific for 5
     bots) */
  for(int z = 0; z < NUM_ROBOTS; z ++) {
    for(int u = 0; u < (NUM_ROBOTS - 1); u ++) {
      if (sorted[u] > sorted[u + 1]) {
	/* Swap. */
	int t = sorted[u];
	sorted[u] = sorted[u + 1];
	sorted[u + 1] = t;
      }
    }
  }

  /* write hash code. this part assumes NUM_ROBOTS=5 */
  h = 0;
  l = 0;
  l |= ((unsigned int)sorted[0] & 255);
  l |= ((unsigned int)sorted[1] & 255) << 9;
  l |= ((unsigned int)sorted[2] & 255) << 18;
  h |= ((unsigned int)sorted[3] & 255);
  h |= ((unsigned int)sorted[4] & 255) << 9;
}

/* tell where a robot at 'loc' would wind up if slid in direction d.
   returns 0 if motion is impossible. Does not modify anything. */ 
int slide_where(int loc, int direction ) {
  /* can't slide? */
  int od = opposite_dirs[direction];
  int pushing_square = loc + direction_deltas [ od ];

  int delta = direction_deltas[ direction ];
  int block_mask = 0x10 | direction;
  int new_loc = loc + delta;

  /* is there a wall that would block us from entering the
     source square in the direction we want to push? */
  if (board[ loc ] & direction) return 0;

  /* is there a bot in the pushing square? */
  if (board[ pushing_square ] & 0x10) return 0;

  if (board[ new_loc ] & block_mask) return 0;
  do {
    loc = new_loc;
    new_loc += delta;
  } while ( (board[ new_loc ] & block_mask) == 0 );
  return loc;
}

void print_sol() {
  printf("Solution:\n");
  memcpy(board, initial_board, sizeof(char) * BOARD_REP_WIDTH *
	 BOARD_REP_HEIGHT);
  
  //  memcpy(robot_location, initial_robots, sizeof(int) * NUM_ROBOTS);
  

  for(int i = 0; i < current_depth; i ++) {
    /* for slide mode */
    printf("--------------------------------------------------\n");
    printf("   %d/%d to %d/%d:\n",
	   move_src[i] % BOARD_REP_WIDTH,
	   move_src[i] / BOARD_REP_WIDTH,
	   
	   move_dest[i] % BOARD_REP_WIDTH,
	   move_dest[i] / BOARD_REP_WIDTH);

    board[move_src[i]] &= ~0x10;
    board[move_dest[i]] |= 0x10;

    print_board(0, 0);
  }
  exit(-1);
}

/* Search using the selected bot. Modifies the hash table to
   record states we've seen. */
inline void search(int bot) {
  /* try moving in each direction */

  int orig_spot = robot_location[bot];
  /* don't try to move bots that are solved */
  if (orig_spot < 0) return ;

  remove_robot( orig_spot ); /* no self-collisions */
  /* save source */
  move_src[current_depth] = orig_spot;

  for(int dir = 1; dir < 9; dir <<= 1) {
    int dest;
    if ( (dest = slide_where(orig_spot, dir)) /* &&
						 !avoid[dest] */ ) {
      /* can make a move, so do it. */
      move_dest[current_depth] = dest;
      current_depth ++;

      /* did we reach goal? if so, remove self from board */
      if (dest == goal_location) {
	robot_location[bot] = -1;

	for(int b = 0; b < NUM_ROBOTS; b++) {
	  if (robot_location[b] >= 0) goto not_finished;
	}
	
	/* finished !! */

	print_sol();
	exit(0);

      not_finished:;

      } else {
	put_robot(bot, dest);
      }

      search_all ();

      remove_robot(dest);

      current_depth --;
    }
  }

  /* undo what we did */
  put_robot (bot, orig_spot);
}

bool visit(unsigned int, unsigned int);

/* for a given board,
   check if we've been here before.
   
   if not, try all successor states. */
void search_all() {
  /* check if this state is in hash */
  
  if (current_depth >= MAX_DEPTH) {
    printf("max depth exceeded\n");
    exit(-1);
  }

  /* read hash code */
  unsigned int hh, hl;
  hash_code(hh, hl);

  if (visit(hh, hl)) {
    /* haven't been here yet. */

    /* try moving each bot */
    for(int i = 0; i < NUM_ROBOTS; i ++) {
      search(i);
    }
  }
}

void usage() {
  printf("Usage: ricochet [options]\n"
         "Options:\n"
         "  -b <filename>      File to read board from."
         " Default is \"board.txt\".\n");

  exit(-1);
}


/* hash stuff.... */

struct hashentry {
  unsigned int hh;
  unsigned int hl;
  hashentry * next;

  hashentry(unsigned int h, unsigned int l, hashentry * n)
    : hh(h), hl(l), next(n) {}
};

#define HASH_SIZE 800011
hashentry ** table;


/* have we visited this board before?
   if so, return false. otherwise, mark it
   as visited and return true. */
static int seen = 0;
bool visit(unsigned int hh, unsigned int hl) {
  unsigned int bin = (hl ^ (hh << 4)) % HASH_SIZE;
  
  hashentry * that = table[bin];
  while (that) {
    if (that->hh == hh && that->hl == hl) return false;
    that = that -> next;
  }
  /* new; add it */
  table[bin] = new hashentry(hh, hl, table[bin]);
  
  // printf("board %d, depth %d\n", seen, current_depth);
  // print_board(0, 0);

  seen ++;
  if (!(seen & 65535)) {
    printf("%d\n", seen);
  }
  return true;
}

void init_hash() {
  table = (hashentry**) malloc(HASH_SIZE * sizeof (hashentry*));
  if (!table) {
    printf("Can't allocate hash table\n");
    exit(-1);
  }
  for(int i = 0; i < HASH_SIZE; i ++) {
    table[i] = 0;
  }
}


int main( int argc, char **argv ) {
  int depth;
  char *filename = "board.txt";
  int n, r;

  int start_time = time(0);

  init_hash();
  
  for (n=1; n<argc; n++) {
    if (strcmp( argv[n], "-b" )==0) {
      n++;
      if (n>=argc) {usage(); return -1;}
      filename = argv[n];
    } else if (strcmp( argv[n], "-version" ) == 0) {
      printf("%s\n", version);
      return 0;
    } else { 
      usage(); 
      return -1; 
    }
  }

  read_board( filename );

  print_board(0, 0);

  search_all();

  printf("End: %d (%d sec)\n", time(0), time(0) - start_time);
  printf("States seen: %d\n", seen);

  return 0;
}
