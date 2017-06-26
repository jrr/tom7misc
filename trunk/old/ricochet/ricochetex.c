
/* This "extended" ricochet robot solver allows for some extra stuff.
   None of this is in pure ricochet robot, so I don't want to pollute
   that code. However, it is useful for, say, solving Escape levels.

   This is a "smarter" Ricochet Robot solver. 
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
/* XXX don't turn this on! it doesn't work in the extended
   version */
// #define SLIDE_CACHE

/* hashing improves performance, but
   costs memory. */
/* XXX don't turn this on! it doesn't work */
// #define USE_HASHING

/* Maximum depth of the search tree (i.e. max number of robot switches). */
#define MAX_DEPTH 15

/* Maximum length of a single solution in moves. */
#define MAX_SOLUTION_SIZE 256

/* no bit tricks here, so make this as large as you want (at your own
   peril! */
#define NUM_ROBOTS 5

const char version[] = "$Id: ricochetex.c,v 1.2 2005/05/28 18:13:20 tom7 Exp $"
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

void permute_and_try(int depth, int s, int * rb);

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

  /* initialize the array of places that we've already been.
     we treat any avoidable square as somewhere we've already
     been, so that we don't try to go back. */
  for(try = 0; try < BOARD_REP_WIDTH * BOARD_REP_HEIGHT; try++)
    already[try] = avoid[try];
  
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

#if 1
  printf("Solution:\n");
  { int is; for(is = 0; is < current_depth; is ++) {
    printf("  rob #%d to %d/%d\n", 
           robot_moved[is],
           destination[is] % BOARD_REP_WIDTH, 
           destination[is] / BOARD_REP_WIDTH);
  } }
  /* XXX recover solution and print it! */
  exit(0);
#endif

  memcpy(board, initial_board, BOARD_REP_WIDTH * BOARD_REP_HEIGHT);
  memcpy(robot_location, initial_robots, NUM_ROBOTS * sizeof (int));

  /* b has the state of the initial board. To find the solution,
     for each destination run shortest_path_to to find the
     shortest way to move the robot to its destination. */

  for(s = 0; s < current_depth; s ++) {
#if 1
    print_board(0, 0);
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
  exit(0);
  /* restore board state */
  memcpy(robot_location, rloc_old, NUM_ROBOTS * sizeof (int));
  memcpy(board, bold, BOARD_REP_WIDTH * BOARD_REP_HEIGHT);

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

  for(oo = 0; oo < (BOARD_REP_WIDTH * BOARD_REP_HEIGHT); oo ++) {
    if (avoid[oo]) already[oo] = 9999999;
    else already[oo] = -1;
  }

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

  /* if we're moving the target bot, and we've reached
     the goal, then this bot can exit. But we're only
     done if this was the LAST BOT. */
  if (bot == 0 && (already[goal_location] >= 0)) {
    int is;

    /* robot exits */
    if (moves + already[goal_location] < best_solution_size) {
      MAKEMOVE(goal_location);

      if (robot_location[1] < 0) {
	/* full solution found! */
	/* XXX delete bot anyway? */

	make_solution();
	
	total_solutions++;
      } else {
	/* solved this bot, but need to do the rest...
	   
	we shift it so that bot 0 disappears, and bot n
	becomes bot n-1 for n>0.
	
	we don't need to record this anywhere, because the
	solution recoverer can tell that when a bot goes
	onto the goal, it gets deleted. */
	int l, r;

	// printf("-------- bot 0 reached goal ---------\n");
	// print_board(0, 0);

	remove_robot(goal_location);
	for(l = 0; l < (NUM_ROBOTS - 1); l ++) {
	  robot_location[l] = robot_location[l + 1];
	  /* don't need to change anything else, since
	     the flags in the board don't care which
	     bot is which. */
	}

	/* since we deleted the first one, there is empty
	   space here now */
	robot_location[NUM_ROBOTS - 1] = -1;

	// printf("hit goal:");
	// print_board(0, 0);

	// printf("-------- try submoves: ---------\n");
	// print_board(0, 0);

	/* now continue ... */
	for (r=0; r < NUM_ROBOTS; r ++) {
	  /* no sense in moving twice in a row.
	     also, robots that are gone (negative position)
	     of course should not move. */
	  if (robot_location[r] >= 0) {
	    success |= moveone(r, depth - 1, moves + already[goal_location]);
	  }
	} 

	// printf("-------- back! ---------\n");
	// print_board(0, 0);


	/* undo move to backtrack... */

	/* make room for the bot again */
	for(l = NUM_ROBOTS - 1; l > 0; l --) {
	  robot_location[l] = robot_location[l - 1];
	}

	/* put him back */
	put_robot(0, goal_location);

	// printf("-------- bot has been returned ---------\n");
	// print_board(0, 0);

      }

      UNMAKEMOVE(goal_location);
    }

    success = 1;
    /* here, reify solution. */
  } else if (depth == 1) {
    /* last move -- only move rob 0.
       if we just moved rob 0, there's no soln. */
    /* PERF if there are more robots than depth, lose */

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
    for (l=1; 
	 l < dl && (moves + already[dests[l]] < (best_solution_size - 1)); 
	 l ++) {

      MAKEMOVE(dests[l]);
      for (r=0; r < NUM_ROBOTS; r ++) {
	/* no sense in moving twice in a row.
	   also, robots that are gone (negative position)
	   of course should not move. */
	if (r != bot && robot_location[r] >= 0) {
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

  int real_bots[NUM_ROBOTS];

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

  print_board(0, 0);

  memcpy(real_bots, robot_location, sizeof(int) * NUM_ROBOTS);

  /* try moving the target robot directly */
  /* FIXME this is bogus, because it doesn't solve for all
     robots (so we can't solve some trivial levels) */
  // printf("Trying to move robot directly there...\n");
  // moveone(0, 0, 0);

  for (depth = 1; 
       depth <= max_depth && (depth+1) < best_solution_size; 
       depth++) {

    int z;
    /* try each of the NUM_ROBOTS! permutations */
    memcpy(robot_location, real_bots, sizeof(int) * NUM_ROBOTS);
    
    printf("Trying depth %d...\n", depth);
    permute_and_try(depth, 0, real_bots);

    if (total_solutions) printf("    ... %d solution%s.\n", 
				total_solutions,
				total_solutions==1?"":"s");
    total_solutions = 0;
  }

  printf("End: %d (%d sec)\n", time(0), time(0) - start_time);

  free(table);
  return 0;
}
  
void permute_and_try(int depth, int s, int * rb) {
  static int chosen_perm[NUM_ROBOTS];
  int i, r, z, u;

  if (s < NUM_ROBOTS) {
    for(z = 0; z < NUM_ROBOTS; z++) {
      /* if it hasn't been used already, we can use it */
      for(u = 0; u < s; u ++) {
	if (chosen_perm[u] == z) goto next;
      }

      /* use it. */
      chosen_perm[s] = z;
      permute_and_try(depth, s + 1, rb);

    next:;
    }
  } else {
    /* got whole permutation, so ... */
    
    /* fill in robot_location array */
    for(i = 0; i < NUM_ROBOTS; i ++) {
      printf("%d", chosen_perm[i]);
      robot_location[i] = rb[chosen_perm[i]];
    }
    printf(".. ");
    
    /* try with each starting robot */
    for (r=0; r < NUM_ROBOTS; r ++) moveone(r, depth, 0);
  }

}
