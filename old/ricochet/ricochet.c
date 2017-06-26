
/* This is a "smarter" Ricochet Robot solver. 
   It was written by Tom 7 in the late days of 2002 and
   early days of 2003, and then slowly improved after that.

   I removed support for wildcard goals, because, who cares? (You can
   always run the program 4 times, if you like.)

   Some of the code from Alan deLespinasse's public domain brute-force
   implementation. His program will always find the shortest solution,
   but only if it is less than about 10 moves deep (otherwise it will
   take forever!!).

   I have slightly different requirements: Since it's probably
   impractical to find the actual shortest solution, we want to be
   able to find *a* solution quickly, and then be able to improve upon
   that solution.

   I've improved upon his in the following ways: 

   First, use a brute-force search procedure, but treat a "move" as
   any path that a single robot can take around the board. (Then, of
   course, no robot ever moves twice in a row). The idea behind this
   is that most solutions involve long sequences of moves with the
   same robot, but few robot "switches". Calculating the set of
   reachable squares is fairly simple. (DONE)

   Improve the slide calculation by using a precomputed table if
   no other robots are in the slide row/col. Without this, the
   program spends half its time in the sliding function. (DONE --
   but this slows the program down significantly. Why?)

   Second, hash intermediate board states (small: just the location of
   the four robots) to prevent useless repetition. (DONE)

   Third, order moves by heuristics? (is non-target robot within one
   row/column of the goal? etc.) (NOT DONE)

*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

/* slide_where or fast_slide_where.
   somehow using the cached version makes
   the program a lot slower. Weird! Stick
   with slide_where for now. */
// #define SLIDE_CACHE

/* hashing improves performance, but
   costs memory. */
// #define USE_HASHING

/* Maximum depth of the search tree (i.e. max number of robot switches). */
#define MAX_DEPTH 15

/* Maximum length of a single solution in moves. */
#define MAX_SOLUTION_SIZE 256

#define NUM_ROBOTS 4

const char version[] = "$Id: ricochet.c,v 1.14 2005/05/23 03:49:33 tom7 Exp $"
#ifdef USE_HASHING
" (hashing)"
#endif
#ifdef SLIDE_CACHE
" (fastslide)"
#endif

;

#ifdef SLIDE_CACHE
#define SLIDE_HOW fast_slide_where
#else
#define SLIDE_HOW slide_where
#endif

/* Ricochet Robot is played on a 16x16 grid. This program represents
   it as an 18x18 array, so that the edges don't have to be handled as
   a special case; the cells on the boundary are always solid walls.
   Well, actually it's a 1-dimensional array with 324 (18*18)
   elements, but it acts like an 18x18 array in row-major order.

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

/* XXX what works best here? char? int?
   don't forget to change the memcpys...
*/
char board[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
char initial_board[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];

/* memoize the destination when sliding in some
   direction from a spot. This can't be used if
   there's a robot in the way, obviously, but
   it saves an expensive for loop over the board
   if we detect (in constant time) that there is
   no possible robot collision. */
int slideu[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
int slided[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
int slidel[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
int slider[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];

/* Index of the goal (target). */
int goal_location;
/* Index of each robot's location. */
int robot_location[NUM_ROBOTS];
int initial_robots[NUM_ROBOTS];

/* keep track of the current solution state as a robot number to move
   and his destination. We can use this to rebuild the solution later.
*/
int current_depth = 0;
int robot_moved[ MAX_DEPTH+2 ];
int destination[ MAX_DEPTH+2 ];

/* This is the best actual solution found so far, as robot numbers and
   directions.
 */
char best_rsol[MAX_SOLUTION_SIZE+2];
char best_dsol[MAX_SOLUTION_SIZE+2];
int best_solution_size = MAX_SOLUTION_SIZE;

unsigned int total_solutions = 0;

/* This is a memo table for the moveone function.
   We store the location of the four robots,
   and, for each one, a depth for which we know
   there is no solution. For instance, if we call
   
   moveone(2, 5);
   
   ... and it returns 0 (no solutions), we write this
   entry into the hash table and set he.depth[2] to
   5. (If it was already higher, then we leave it.)

   XXX there is probably some relation to the other
   depths. can we set the rest to 4?
*/

typedef struct {
  short state[NUM_ROBOTS];
  char depth[NUM_ROBOTS];
  struct hashentry * next;
} hashentry;

/* find an actual prime! */
#define HASH_SIZE 313371
hashentry ** table;
int tablesize;

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
sign.
*/

/* some old versions of visual C++ need this */
#if 0
int max(int a, int b) {
  if (a < b) return b;
  else return a;
}
#endif

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

    do {
      c = fgetc( f );
      if (feof(f))
        board_error( y*2, "Unexpected end of file" );
    } while (c!='|');

    board[ y*BOARD_REP_WIDTH ] |= LEFT;
    board[ y*BOARD_REP_WIDTH + 1 ] |= RIGHT;

    for (x=1; x<=BOARD_WIDTH; x++) {
      c =  fgetc( f );
      switch (c) {
#define CASE(cc) \
     case '0' + cc: \
       if (robotfound[cc]) board_error (y*2, "duplicate robot #"#cc); \
       robotfound[cc] = 1; \
       robotx[cc] = x; roboty[cc] = y; \
       break
     
        CASE(0);
        CASE(1);
        CASE(2);
        CASE(3);

      case '@':
        if (target_found) board_error( y*2, "duplicate target" );
        target_found = 1;
        targetx = x;
        targety = y;
        break;

      case ' ':
        break;
      default:
        board_error( y*2, "r, g, b, y, R, G, B, Y, *, or space expected" );
      }

      c = fgetc( f );
      if (c=='|') {
        board[ y*BOARD_REP_WIDTH + x ] |= LEFT;
        board[ y*BOARD_REP_WIDTH + x+1 ] |= RIGHT;
      }
      else if (c!=' ')
        board_error( y*2, "space or | expected" );
    }
  }

  fclose( f );

  if (!target_found)
    board_error( 34, "No target found" );

  for (x=0; x<NUM_ROBOTS; x++) {
    if (!robotfound[x])
      board_error( 34, "Robot(s) missing" );
  }

  for (x=0; x<NUM_ROBOTS; x++) {
    place_robot( x, robotx[ x ], roboty[ x ] );
  }

  goal_location = targety*BOARD_REP_WIDTH+targetx;

  memcpy(initial_board, board, BOARD_REP_WIDTH * BOARD_REP_HEIGHT);
  memcpy(initial_robots, robot_location, NUM_ROBOTS * sizeof (int));
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

/* Using the board b, compute the fastest way for
   the bot to reach its goal. Return 0 if the
   solution won't fit. */
int shortest_path_to(int bot, int goal,
                     char * rsol, char * dsol, int * slen) {

  /* I use breadth-first search. Dijkstra's algorithm might
     be faster, but I think it has too much overhead
     (certainly it is more trouble to program!) */

  /* Since we visit spaces breadth-first, we never have
     to worry about lowering the distance of something we
     find. */

  int already[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];

  /* when we first find a node, mark the location
     we came from! */
  int back[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];


  int queue[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
  int ql = 0;

  int frontier[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
  int frontl = 0;

  int dist = 0;
  int orig_spot;

  int try = 0;

  for(try = 0; try < BOARD_REP_WIDTH * BOARD_REP_HEIGHT; try++)
    already[try] = 0;
  
  orig_spot = queue[ql++] = robot_location[bot];
  remove_robot( orig_spot );

  /* the queue is the list of locations reachable in
     'dist' steps. eat up the queue and put all
     adjacent locations into the frontier.
  */

  for (;;) {

#if 0
      printf("(bot %d) Reachable in %d moves:\n", bot, dist);
      for(try = 0; try < ql; try ++) {
        printf("   %d/%d\n",
               queue[try] % BOARD_REP_WIDTH, 
               queue[try] / BOARD_REP_WIDTH);
      }
      print_board(queue, ql);
#endif

    if (ql == 0) { 
      int marks[1] = {orig_spot};
      printf ("??????? queue empty!\n"); 
      print_board(marks, 1);
      exit(0);
    }
    for(try = 0; try < ql; try ++) {
      int dir, dest;
      int src = queue[try];
      for(dir = 1; dir < 9; dir <<= 1) {
        if ((dest = SLIDE_HOW(src, dir)) && !already[dest]) {

          already[dest] = 1;
          back[dest] = src;
          frontier[frontl++] = dest;

          if (dest == goal) {
            int unused = dist ++;
            int sl = *slen + dist;
            int cl = goal;
            *slen = sl;

            put_robot(bot, dest);
            //            printf("Got to goal in %d moves.\n", dist);
            if (sl >= MAX_SOLUTION_SIZE) return 0;

#define DELTA_TO_DIR(dest, src) ((dest>src) ? (((dest-src) % BOARD_REP_WIDTH) ? RIGHT : DOWN) : (((src - dest) % BOARD_REP_WIDTH) ? LEFT : UP))

            sl--;
            for(; dist > 0; dist --) {
#if 0
              printf("dist %d sl %d cl %d/%d back[cl] %d/%d\n",
                     dist, sl, cl % BOARD_REP_WIDTH,
                     cl / BOARD_REP_WIDTH,
                     back[cl] % BOARD_REP_WIDTH,
                     back[cl] / BOARD_REP_WIDTH);
#endif                
              rsol[sl] = bot;
              dsol[sl] = DELTA_TO_DIR(cl, back[cl]);
              cl = back[cl];
              sl --;
            }

            return 1;
          }
        }
      }
    }
  
    /* now the work queue is empty and the frontier
       is filled with things I can reach at the next
       distance. Loop. */

    memcpy(queue, frontier, frontl * sizeof (int));
    ql = frontl;
    frontl = 0;
    dist++;
  }

}

void print_solution() {
  int oo, last = -1;
  for(oo = 0; oo < best_solution_size; oo ++) {
    if (best_rsol[oo] != last)
      printf("\n  Robot %d ", best_rsol[oo]);
    printf("%s ", direction_names[best_dsol[oo]]);
    last = best_rsol[oo];
  }
  printf("\n");
}

/* once I've found a solution, I need to see how long it actually is,
   and what the actual moves were. If it's better than anything we've
   found, it replaces the best one.

   Finding a solution happens pretty infrequently, so we don't need to
   be so ridiculously efficiency-conscious here.
*/

void make_solution () {

  char bold[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];
  int rloc_old[NUM_ROBOTS];

  /* robot numbers and their directions */
  char rsol[MAX_SOLUTION_SIZE+2];
  char dsol[MAX_SOLUTION_SIZE+2];
  int s, slen = 0;

  /* save board state */
  memcpy(rloc_old, robot_location, NUM_ROBOTS * sizeof (int));
  memcpy(bold, board, BOARD_REP_WIDTH * BOARD_REP_HEIGHT);

#if 0
  printf("Solution:\n");
  { int is; for(is = 0; is < current_depth; is ++) {
    printf("  rob #%d to %d/%d\n", 
           robot_moved[is],
           destination[is] % BOARD_REP_WIDTH, 
           destination[is] / BOARD_REP_WIDTH);
  } }
#endif

  memcpy(board, initial_board, BOARD_REP_WIDTH * BOARD_REP_HEIGHT);
  memcpy(robot_location, initial_robots, NUM_ROBOTS * sizeof (int));

  /* b has the state of the initial board. To find the solution,
     for each destination run shortest_path_to to find the
     shortest way to move the robot to its destination. */

  for(s = 0; s < current_depth; s ++) {
#if 0
    printf("move robot %d at %d/%d to %d/%d (%d moves so far)\n",
           robot_moved[s],
           robot_location[robot_moved[s]] % BOARD_REP_WIDTH,
           robot_location[robot_moved[s]] / BOARD_REP_WIDTH,
           destination[s] % BOARD_REP_WIDTH,
           destination[s] / BOARD_REP_WIDTH, slen);
#endif

    if (!shortest_path_to(robot_moved[s],
                          destination[s],
                          rsol, dsol, &slen)) goto out;
    /* don't bother continuing if we're already
       over. */
    if (slen >= best_solution_size) goto out;
  }

  /* if we get here then we've beaten the best solution! */

  memcpy(best_rsol, rsol, slen);
  memcpy(best_dsol, dsol, slen);
  best_solution_size = slen;
  printf("New best solution size is %d:", best_solution_size);
  print_solution();

 out:
  /* restore board state */
  memcpy(robot_location, rloc_old, NUM_ROBOTS * sizeof (int));
  memcpy(board, bold, BOARD_REP_WIDTH * BOARD_REP_HEIGHT);

}

/* tell where a robot at 'loc' would wind up if slid in direction d.
   returns 0 if motion is impossible. Does not modify anything. */ 
int slide_where(int loc, int direction ) {
  int delta = direction_deltas[ direction ];
  int block_mask = 0x10 | direction;
  int new_loc = loc + delta;
  if (board[ new_loc ] & block_mask) return 0;
  do {
    loc = new_loc;
    new_loc += delta;
  } while ( (board[ new_loc ] & block_mask) == 0 );
  return loc;
}

int fast_slide_where(int loc, int dir) {
  int i;

  switch (dir) {
  case UP:
    if ((robot_location[0] < loc &&
	 (loc - robot_location[0]) % BOARD_REP_WIDTH == 0) ||
	(robot_location[1] < loc &&
	 (loc - robot_location[1]) % BOARD_REP_WIDTH == 0) ||
	(robot_location[2] < loc &&
	 (loc - robot_location[2]) % BOARD_REP_WIDTH == 0) ||
	(robot_location[3] < loc &&
	 (loc - robot_location[3]) % BOARD_REP_WIDTH == 0))
      return slide_where(loc, dir);
    else return slideu[loc];
  case DOWN:
    if ((robot_location[0] > loc &&
	 (robot_location[0] - loc) % BOARD_REP_WIDTH == 0) ||
	(robot_location[1] > loc &&
	 (robot_location[1] - loc) % BOARD_REP_WIDTH == 0) ||
	(robot_location[2] > loc &&
	 (robot_location[2] - loc) % BOARD_REP_WIDTH == 0) ||
	(robot_location[3] > loc &&
	 (robot_location[3] - loc) % BOARD_REP_WIDTH == 0))
      return slide_where(loc, dir);
    else return slided[loc];
  case RIGHT:
    if ((robot_location[0] > loc &&
	 robot_location[0] <= slider[loc]) ||
	(robot_location[1] > loc &&
	 robot_location[1] <= slider[loc]) ||
	(robot_location[2] > loc &&
	 robot_location[2] <= slider[loc]) ||
	(robot_location[3] > loc &&
	 robot_location[3] <= slider[loc]))
      return slide_where(loc, dir);
    else return slider[loc];

  case LEFT:
    if ((robot_location[0] < loc && 
	 robot_location[0] >= slidel[loc]) ||
	(robot_location[1] < loc && 
	 robot_location[1] >= slidel[loc]) ||
	(robot_location[2] < loc && 
	 robot_location[2] >= slidel[loc]) ||
	(robot_location[3] < loc && 
	 robot_location[3] >= slidel[loc]))
      return slide_where(loc, dir);
    else return slidel[loc];
    
  default:
    return slide_where(loc, dir);

  }

}


/* XXX hashing: this is a little problematic because
   we decide on a robot for moveone. A better solution
   might be to essentially memoize moveone with a
   char[4] in the hash entry. (One depth count for each
   robot). Memoization is a bit easier to reason about
   than hashing, too. */

/* consult hash table. Return 1 if there is known to
   be NO solutions from the board state in d robot
   switches. XXX args
*/
   

int boardmatch(hashentry * h) {
  int u;
  for(u = 0; u < NUM_ROBOTS; u ++)
    if (h->state[u] != robot_location[u]) return 0;
  return 1;
}

unsigned int hashcode() {
  return robot_location[0] ^ (robot_location[1] << 9) |
    (robot_location[2] << 18) ^ (robot_location[3] << 23);
}

int board_is_useless(int bot, int d) {
  unsigned int h = hashcode() % HASH_SIZE;
  hashentry * tmp;

  /* see if the board is there. */
  for(tmp = table[h]; 
      tmp; 
      tmp = (void*)tmp -> next) {
    if (boardmatch(tmp)) {
      return (tmp->depth[bot] >= d);
    }
  }

  return 0;

}

/* mark that we were unable to find any solutions for
   the current board with d robot switches. XXX args */
void mark_board_useless(int bot, int d) {
  unsigned int h = hashcode() % HASH_SIZE;
  int u;
  hashentry * tmp;

  /* see if the board is there. */
  for(tmp = table[h]; tmp; tmp = (void*)tmp -> next) {
    if (boardmatch(tmp)) {
      tmp->depth[bot] = max(tmp->depth[bot], d);
      return;
    }
  }

  /* add new one at head of list. */
  tmp = table[h];
  table[h] = (hashentry*) malloc(sizeof (hashentry));
  table[h]->next = (void*)tmp;
  table[h]->depth[bot] = d;
  for(u = 0; u < NUM_ROBOTS; u ++) {
    if (u != bot) table[h]->depth[u] = -1;
    table[h]->state[u] = robot_location[u];
  }
}


int moveone( int bot, int depth, int moves ) {

#define MAKEMOVE(dest) { put_robot(bot, (dest)); robot_moved[current_depth] = bot; destination[current_depth++] = (dest); }

#define UNMAKEMOVE(dest) { remove_robot( dest ); current_depth --; }

  /* distance to places we've already visited on this turn.
     -1 if unseen. */
  int already[BOARD_REP_WIDTH * BOARD_REP_HEIGHT];

  /* all possible destinations of this robot */
  int dests[BOARD_REP_WIDTH * BOARD_REP_HEIGHT + 1];

  /* dl = length of dests.
     try = index into dests -- anything after this
     hasn't been explored yet.
     dir, oo, dest = temp vars */
  int dl = 0, dir, dest, try = 0, oo, orig_spot;

  /* count to see if there are any solutions from this board
     at this depth. If not, we'll mark it in the hash table
     as useless. XXX new way, see above */
  int success = 0;

#ifdef USE_HASHING
  if (board_is_useless(bot, depth)) return 0;
#endif

  for(oo = 0; oo < (BOARD_REP_WIDTH * BOARD_REP_HEIGHT); oo ++) 
    already[oo] = -1;

  orig_spot = dests[dl++] = robot_location[bot];
  already[orig_spot] = 0;
  remove_robot( orig_spot ); /* no self-collisions */
  while (try < dl) {
#if 0
    printf("try %d, |dests| %d, it's: ", try, dl);
    for(oo = 0; oo < dl; oo ++) {
      printf("%d/%d ", dests[oo] % BOARD_REP_WIDTH, 
             dests[oo] / BOARD_REP_WIDTH);
    }
    printf("\n");
#endif

    for(dir = 1; dir < 9; dir <<= 1) {
      if ((dest = SLIDE_HOW(dests[try], dir)) && 
          (already[dest] < 0)) {
        already[dest] = already[dests[try]] + 1;
        dests[dl++] = dest;

	if ((moves + already[dest]) >= best_solution_size) goto done;
      }
    }

    try ++;
  }

 done:;

#if 0
  printf("Robot %d can reach:\n", bot);
  print_board(dests, dl);
  for(oo = 0; oo < dl; oo ++) {
    printf("    %d/%d in %d moves\n",
	   dests[oo] % BOARD_REP_WIDTH, 
	   dests[oo] / BOARD_REP_WIDTH,
	   already[dests[oo]]);
  }
#endif

  if (bot == 0 && (already[goal_location] >= 0)) {
    int is;

    if (moves + already[goal_location] < best_solution_size) {
      /* solution found! */
      MAKEMOVE(goal_location);

      make_solution();

      UNMAKEMOVE(goal_location);
      total_solutions++;
    }

    success = 1;
    /* here, reify solution. */
  } else if (depth == 1) {
    /* last move -- only move rob 0.
       if we just moved rob 0, there's no soln. */

    int l;
    if (bot)
      for (l = 1; l < dl && (moves + already[dests[l]] < (best_solution_size - 1)); l ++) {
	MAKEMOVE(dests[l]);
	success |= moveone(0, 0, moves + already[dests[l]]);
	UNMAKEMOVE(dests[l]);
      }

  } else if (depth > 0) { 
    /* general. */
    int r, l;
    /* don't bother with the empty move, which is
       always dests[0]. */
    for (l=1; l < dl && (moves + already[dests[l]] < (best_solution_size - 1)); l ++) {

      MAKEMOVE(dests[l]);
      for (r=0; r < NUM_ROBOTS; r ++) {
	/* no sense in moving twice in a row... */
	if (r != bot) {
	  success |= moveone(r, depth - 1, moves + already[dests[l]]);
	}
      } 
      UNMAKEMOVE(dests[l]);

    }
  }

  /* undo any work we've done. */
  put_robot(bot, orig_spot);

#ifdef USE_HASHING
  if (!success) {
    mark_board_useless(bot, depth);
  }
#endif

  return success;
}

void usage() {
  printf("Usage: ricochet [options]\n"
         "Options:\n"
         "  -b <filename>      File to read board from."
         " Default is \"board.txt\".\n"
         "  -min <min_depth>   Minimum number of moves to try."
         " Default is 1.\n"
         "  -max <max_depth>   Maximum number of moves to try."
         " Default is %d.\n"
         "   <max_depth> should be greater than or equal to <min_depth>.\n"
         "   <max_depth> should be no more than 15.\n", MAX_DEPTH);

  exit(-1);
}

void dotable(int dir, int * slidetable) {
  int i, j, loc;
  for(i=1; i <= BOARD_HEIGHT; i++)
    for(j=1; j <= BOARD_WIDTH; j++) {
      loc = (i * BOARD_REP_WIDTH) + j;
      slidetable[loc] = slide_where(loc, dir);
    }
}

int main( int argc, char **argv ) {
  int depth, min_depth=1, max_depth=MAX_DEPTH;
  char *filename = "board.txt";
  int n, r;

  int start_time = time(0);

  /* allocate hash table */
#ifdef USE_HASHING
  table = (hashentry**) malloc(HASH_SIZE * sizeof (hashentry*));

  for(n = 0; n < HASH_SIZE; n ++) 
    table[n] = 0;
#endif

  for (n=1; n<argc; n++) {
    if (strcmp( argv[n], "-b" )==0) {
      n++;
      if (n>=argc) {usage(); return -1;}
      filename = argv[n];
    } else if (strcmp( argv[n], "-version" ) == 0) {
      printf("%s\n", version);
      return 0;
    } else if (strcmp( argv[n], "-min" )==0) {
      n++;
      if (n>=argc) { usage(); return -1; }
      if (sscanf( argv[n], "%d", &min_depth ) != 1)
        { usage(); return -1; }
      
    }
    else if (strcmp( argv[n], "-max" )==0) {
      n++;
      if (n>=argc) { usage(); return -1; }
      if (sscanf( argv[n], "%d", &max_depth ) != 1)
        { usage(); return -1; }
    }
    else
      { usage(); return -1; }
  }

  if (min_depth < 1 || 
      max_depth > MAX_DEPTH ||
      min_depth > max_depth)
    { usage(); return -1; }

  read_board( filename );

  /* initialize the sliding tables for an empty board. */

  for(r=0; r < NUM_ROBOTS; r++) {
    remove_robot(robot_location[r]);
  }

  dotable(UP, slideu);
  dotable(DOWN, slided);
  dotable(LEFT, slidel);
  dotable(RIGHT, slider);

  /* (put the robots back where they were) */
  for(r=0; r < NUM_ROBOTS; r++) {
    put_robot(r, robot_location[r]);
  }

  /* try moving the target robot directly */
  printf("Trying to move robot directly there...\n");
  moveone(0, 0, 0);

  for (depth = 1; 
       depth <= max_depth && (depth+1) < best_solution_size; 
       depth++) {
    printf("Searching for %d-switch solutions...\n", depth);
    for (r=0; r < NUM_ROBOTS; r ++)
      moveone(r, depth, 0);
    if (total_solutions) printf("    ... %d solution%s.\n", 
				total_solutions,
				total_solutions==1?"":"s");
    total_solutions = 0;
  }

  printf("End: %d (%d sec)\n", time(0), time(0) - start_time);

  free(table);
  return 0;
}
