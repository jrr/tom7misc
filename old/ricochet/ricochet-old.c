/* Program to find Ricochet Robot solutions.

Author: Alan deLespinasse
aldel@alum.mit.edu
http://www.aldel.com 

See http://www.ricochetrobot.com for information about the game.

This program is free.  Feel free to redistribute it, improve it,
incorporate it into other programs, steal bits of code from it for
unrelated applications, let it evolve into a malevolent intelligence
that will destroy humanity, or do anything else you want with it.  If
you do any of the above, though, you should probably give me credit,
or you'll feel guilty. */


#include <stdio.h>
#include <string.h>

/* Maximum depth of the search tree (i.e. max number of moves). */
#define MAX_DEPTH 15

#define NUM_ROBOTS 4

/*

Ricochet Robot is played on a 16x16 grid.  This program represents it
as an 18x18 array, so that the edges don't have to be handled as a
special case; the cells on the boundary are always solid walls.  Well,
actually it's a 1-dimensional array with 324 (18*18) elements, but it
acts like an 18x18 array in row-major order.

Each element of the array is a char.  The char has a bit for each
direction; if the bit is 1, there is a wall that prevents entry by
robots moving in that direction.  See definitions of DOWN, UP, RIGHT
and LEFT below.  In addition, bit 4 (the "16" bit) is set if there is
a robot in that location.  So, for example, if a robot is moving down,
it can enter a cell on the grid as long as bit 0 and bit 4 are both 0;
otherwise it can't.

*/

#define BOARD_WIDTH 16
#define BOARD_HEIGHT 16

#define BOARD_REP_WIDTH (BOARD_WIDTH+2)
#define BOARD_REP_HEIGHT (BOARD_HEIGHT+2)

#define DOWN 1
#define UP 2
#define RIGHT 4
#define LEFT 8


int print_solution_board = 0;   /* If 1, board is printed after solving. */

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

int direction_opposites[] = { 0, 2, 1, 0, 8, 0, 0, 0, 4 };

char board[BOARD_REP_WIDTH*BOARD_REP_HEIGHT];

/* The names of the colors of the robots.  The order may change,
   because the robot that has to get to the goal is always robot
   number 0 (it makes the solution search easier and faster).
   (Unless, of course, the goal is the "wildcard" goal, in which case
   any robot will do, and a different version of the solution search
   is used). So the names are filled in when the board configuration
   is read from the file. */
char *robot_colors[NUM_ROBOTS];

int wildcard_goal;              /* Set to 1 if the goal is the "wildcard". */
int goal_location;              /* Index of the goal (target). */
int robot_location[NUM_ROBOTS]; /* Index of each robot's location. */

/* These globals implement a stack that keeps track of the moves
   currently being attempted.  The stack starts at depth 1 because the
   PRUNE_REVERSES code below checks a potential move against the
   previous one, which is at location current_depth-1 in the stack.
   This way it doesn't need to handle an empty stack as a special
   case; it just looks at current_depth-1 and sees that the robot
   moved on the previous turn was -1, which is definitely not the
   robot it is considering moving. */
int current_depth = 1;
int robot_moved[ MAX_DEPTH+2 ] = { -1 };
int direction_moved[ MAX_DEPTH+2 ] = { -1 };

/* These globals count the number of solutions attempted and the
   number that actually worked.  total_tries has to be 64 bits because
   the number of attempts can easily be more than 4294967295.  With
   the Microsoft compiler, change "long long" to "__int64", and change
   the "Total attempts" printf at the bottom to use "%I64u" as the
   format code (or something like that). */
unsigned __int64 total_tries = 0;
unsigned int total_solutions = 0;

/* Put robot n on the board at column x, row y (for initialization only). */
void place_robot( int n, int x, int y ) {
  robot_location[n] = y*BOARD_REP_WIDTH+x;
  board[ y*BOARD_REP_WIDTH+x ] |= 0x10;
}

void board_error( int linenum, char *message ) {
  printf("Error in board at line %d: %s\n", linenum, message);
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
|    g|                         |
+-+ + + + + + + + + + + + + + + +
|             |             |   |
+ +-+ + + + + +-+ + + +-+ + +-+ +
| |y                    |       |
+ + + + + + + +-+-+ + + + + + + +
|             |   |             |
+ + + + + + + +   + + + + + + + +
|             |   |             |
+ + + + + + + +-+-+ + + +-+ + + +
|       |                b|     |
+ + + + +-+ +-+ + + + + + + + +-+
|           |                   |
+-+ + + + + + + + + + + + + + + +
|                               |
+ + + + + + + +-+ +-+ + + + + + +
|               | |             |
+ +-+ + + + + + + + + + + + + + +
|   |                       |   |
+ + + + + + + + + + + + + + +-+ +
|      G|               |       |
+ + + +-+ + + + + + + +-+ + + + +
|         |                r|   |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

The +s are optional but highly recommended.  Lower case letters are
robots; the upper case letter is the goal.  The wildcard goal is
represented by a *.  There must be exactly one of each robot and one
goal.

Here's a blank board to start with.

+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + +-+-+ + + + + + + +
|             |   |             |
+ + + + + + + +   + + + + + + + +
|             |   |             |
+ + + + + + + +-+-+ + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+ + + + + + + + + + + + + + + + +
|                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

*/

void read_board( char *filename ) {
#define RED 0
#define GREEN 1
#define BLUE 2
#define YELLOW 3
#define WILD 4

  static char *local_color_names[NUM_ROBOTS] = {
    "red",
    "green",
    "blue",
    "yellow"
  };

  int x, y;

  int target_found = 0;
  int robotx[NUM_ROBOTS], roboty[NUM_ROBOTS], robotfound[NUM_ROBOTS];
  int targetcolor;
  int targetx, targety;
  char c;

  FILE *f = fopen( filename, "r" );

  if (f==NULL) {
    printf("Can't open file %s\n", filename);
    return ; /* XXX */
  }

  wildcard_goal = 0;
  
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
      case 'r':
        if (robotfound[RED])
          board_error( y*2, "duplicate r" );
        robotfound[RED] = 1;
        robotx[RED] = x;
        roboty[RED] = y;
        break;
      case 'g':
        if (robotfound[GREEN])
          board_error( y*2, "duplicate g" );
        robotfound[GREEN] = 1;
        robotx[GREEN] = x;
        roboty[GREEN] = y;
        break;
      case 'b':
        if (robotfound[BLUE])
          board_error( y*2, "duplicate b" );
        robotfound[BLUE] = 1;
        robotx[BLUE] = x;
        roboty[BLUE] = y;
        break;
      case 'y':
        if (robotfound[YELLOW])
          board_error( y*2, "duplicate y" );
        robotfound[YELLOW] = 1;
        robotx[YELLOW] = x;
        roboty[YELLOW] = y;
        break;
      case 'R':
        if (target_found) board_error( y*2, "duplicate target" );
        target_found = 1;
        targetx = x;
        targety = y;
        targetcolor = RED;
        break;
      case 'G':
        if (target_found) board_error( y*2, "duplicate target" );
        target_found = 1;
        targetx = x;
        targety = y;
        targetcolor = GREEN;
        break;
      case 'B':
        if (target_found) board_error( y*2, "duplicate target" );
        target_found = 1;
        targetx = x;
        targety = y;
        targetcolor = BLUE;
        break;
      case 'Y':
        if (target_found) board_error( y*2, "duplicate target" );
        target_found = 1;
        targetx = x;
        targety = y;
        targetcolor = YELLOW;
        break;
      case '*':
        if (target_found) board_error( y*2, "duplicate target" );
        target_found = 1;
        targetx = x;
        targety = y;
        targetcolor = WILD;
        wildcard_goal = 1;
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

  if (wildcard_goal) {
    y = 0;
  }
  else {
    robot_colors[0] = local_color_names[ targetcolor ];
    place_robot( 0, robotx[ targetcolor ], roboty[ targetcolor ] );
    y = 1;
  }
  for (x=0; x<NUM_ROBOTS; x++) {
    if (x==targetcolor) continue;
    robot_colors[ y ] = local_color_names[ x ];
    place_robot( y, robotx[ x ], roboty[ x ] );
    y++;
  }
  goal_location = targety*BOARD_REP_WIDTH+targetx;

#undef RED
#undef GREEN
#undef BLUE
#undef YELLOW
#undef WILD
}

void print_board() {
  int x, y, loc, n, check_goal;

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
      for (n=0; n<NUM_ROBOTS; n++) {
        if (loc == robot_location[n]) {
          putc( robot_colors[n][0], stdout );
          check_goal = 0;
          break;
        }
      }
      if (check_goal) {
        if (loc == goal_location) {
          if (wildcard_goal)
            putc( '*', stdout );
          else
            putc( robot_colors[n][0] + 'A' - 'a', stdout );
        }
        else
          putc( ' ', stdout );
      }
    }
    putc( '\n', stdout );
  }
}

void print_solution() {
  /* Skip the first thing on the stack-- it's not a real move */
  int i=1;
  printf("Solution in %d moves:\n", current_depth-1);
  for (;;) {
    printf( "%s %s", robot_colors[ robot_moved[i] ],
            direction_names[ direction_moved[i] ]);
    i++;
    if (i==current_depth) break;
    printf(", ");
  }
  printf("\n");
  if (print_solution_board)
    print_board();
}

void remove_robot( int old_loc ) {
  board[ old_loc ] &= 0xEF;
}

void put_robot( int n, int new_loc ) {
  robot_location[n] = new_loc;
  board[ new_loc ] |= 0x10;
}

int slide_robot( int n, int direction ) {
  int loc = robot_location[n];
  int delta = direction_deltas[ direction ];
  int block_mask = 0x10 | direction;
  int new_loc = loc + delta;
  int previous_location;
  if (board[ new_loc ] & block_mask) return 0;
  robot_moved[ current_depth ] = n;
  direction_moved[ current_depth ] = direction;
  previous_location = loc;
  current_depth++;
  remove_robot( loc );
  do {
    loc = new_loc;
    new_loc += delta;
  } while ( (board[ new_loc ] & block_mask) == 0 );
  put_robot( n, loc );
  return previous_location;
}

void unslide( int n, int loc ) {
  current_depth--;
  remove_robot( robot_location[n] );
  put_robot( n, loc );
}

/* The extra code used when PRUNE_REVERSES is defined prevents the
   mistake of moving a robot in one direction, then immediately moving
   it in the other. */
#define PRUNE_REVERSES

/* The ricochet function is actually called with a depth of one less
   than the number of moves to try.  So sue me. */
void ricochet( int depth ) {
  int robotn, direction, robotlimit, prev_loc;

#ifdef PRUNE_REVERSES
  int samerobot;
  int prevdepth = current_depth - 1;
  int prevrobot = robot_moved[ prevdepth ];
  int reverse_dir = direction_opposites[ direction_moved[ prevdepth ] ];
#endif

  /* This prevents it from moving any of the non-goal robots for the
     final move, since that wouldn't do any good. */
  if (depth)
    robotlimit = NUM_ROBOTS;
  else
    robotlimit = 1;

  for ( robotn=0; robotn<robotlimit; robotn++ ) {

#ifdef PRUNE_REVERSES
    if ( robotn == prevrobot )
      samerobot = 1;
    else
      samerobot = 0;
#endif

    for ( direction=1; direction < 9 && !total_solutions; direction<<=1 ) {
      if (

#ifdef PRUNE_REVERSES
        !(samerobot && (direction == reverse_dir)) &&
#endif

        (prev_loc = slide_robot( robotn, direction)) )
      {
        total_tries++;
        if (robot_location[0] == goal_location) {
          print_solution();
          total_solutions++;
        }
        else if (depth)
          ricochet( depth-1 );
        unslide( robotn, prev_loc );
      }
    }
  }
}

void usage() {
  printf("Usage: ricochet [options]\n");
  printf("Options:\n");
  printf("  -b <filename>      File to read board from."
         " Default is \"board.txt\".\n");
  printf("  -min <min_depth>   Minimum number of moves to try."
         " Default is 1.\n");
  printf("  -max <max_depth>   Maximum number of moves to try."
         " Default is %d.\n", MAX_DEPTH);
  printf("   <max_depth> should be greater than or equal to <min_depth>.\n");
  printf("   <max_depth> should be no more than 15.\n");
  printf("  -pb                Print board when solution is found.\n");
}

int main( int argc, char **argv ) {
  int depth, min_depth=1, max_depth=MAX_DEPTH;
  char *filename = "board.txt";
  int n;

  for (n=1; n<argc; n++) {
    if (strcmp( argv[n], "-b" )==0) {
      n++;
      if (n>=argc) {usage(); return -1;}
      filename = argv[n];
    }
    else if (strcmp( argv[n], "-min" )==0) {
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
    else if (strcmp( argv[n], "-pb" )==0) {
      print_solution_board = 1;
    }
    else
      { usage(); return -1; }
  }

  if (min_depth < 1 || 
      max_depth > MAX_DEPTH ||
      min_depth > max_depth)
    { usage(); return -1; }

  read_board( filename );

  for (depth=min_depth; depth<=max_depth; depth++) {
    printf("Searching for %d-move solutions...\n", depth);
    ricochet( depth-1 );
    if (total_solutions) break;
  }

  printf("Total attempts: %I64u\n", total_tries);
  printf("Total solutions: %d\n", total_solutions);

  return 0;
}
