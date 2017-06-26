#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define size 8
#define last 63

#define ONCHAR "O"
#define OFFCHAR "-"

#define LOGFILE "log.txt"

#define STARTBEST 1

#define WAITTIME 10000000

typedef unsigned char uchar;
typedef unsigned int uint;
/* global for speed, not pragmatism */
/* with nested functions there'd be no need for this nonsense.
   Get it together, K&R! */
uchar * board = 0; 
// int size;

void recursetry(int);
int hassquare (int,int,int,int);
void saveboard();
void writeboard(FILE *);

uint count,bestcount=STARTBEST, HAS_ITER=0, REC_ITER=0,oldtime=0;
int starttime;
// int last;

int main (int argc, char ** argv) {

  starttime = time(0);

  if (argc < 2) {
    printf("%s: board size on the command line, you dolt!\n",*argv);
    exit(0);
  }

//  size = atoi(argv[1]);
//  last = (size*size) -1;
  if (size <= 1) {
    printf("%s: Board size must be >1!\n",*argv);
    exit(0);
  }

  if (!( board = (uchar *)calloc(1,size*size))) {
    printf("%s: No memory.\n",*argv);
    exit(0);
  }

  count = 0;
  recursetry(0);
  free(board);

  printf("> At termination:\n> Calls to hassquare: %d\n>"
	 " Calls to recursetry: %d\n"
	 "> Total seconds: (%d)", HAS_ITER, REC_ITER, time(0)-starttime);

  return 0;
}

void recursetry(int where) {
  int s,x1 = where & 7, y1 = where >> 3;

  REC_ITER ++;

  board[where]++;
  
  /* did this ruin anything? */
  count++;
  for(s=0; s<where; s++)
    /* avoid division */
    if (hassquare(x1, y1, x&7, x>>3)) goto badfail;


  if (count > bestcount) {
    bestcount = count;
    saveboard();
    writeboard(stdout);
  }

  if (REC_ITER > oldtime) {
    writeboard(stdout);
    oldtime = REC_ITER + WAITTIME;
  }

  if (where < last) recursetry(where+1);

 badfail:
  board[where]--;
  count--;
  if (where < last) recursetry(where+1);
  
}

#define BRD(a,b) (board[(a)|((b) << 3)])

int hassquare (int x1, int y1, int x2, int y2) {
 
  /*  checks if (index) has a square w/ diagonals x1,y2,x2,y2 */

  int dx, dy, offset, ny1, ny2, nx1, nx2;

  HAS_ITER ++;

  /*  if (x1 == x2 && y1 == y2) return 0; */
  
  dx = x2 - x1;
  dy = y2 - y1;
   
  offset = dx - dy;
  if (offset & 1) return 0;
  else {
    offset >>= 1;
    /*  it will work */
    
    ny1 = y1 - offset;
    ny2 = y2 + offset;
    nx1 = x2 - offset;
    nx2 = x1 + offset;
    if ((nx1 | nx2 | ny1 | ny2)&~7L) return 0;
    if ((nx1 < 0 || nx2 < 0 || ny1 < 0 || ny2 < 0) return 0;
    if (BRD(nx1, ny1) && BRD(nx2, ny2)) return 1;
    else return 0;
    
  }
  
}

void saveboard () {

  FILE * outie;

  if ((outie = fopen(LOGFILE,"a"))) {
    writeboard(outie);

  fclose(outie);
  }
}

void writeboard(FILE * outie) {
  int x,y;
  time_t ttt = time(NULL);
  fprintf(outie, "> (%d) %s> Count: %d (best %d)\n> Calls to hassquare: %d\n> Calls to recursetry: %d\n",
	  ttt - starttime, ctime(&ttt), count, bestcount, HAS_ITER, REC_ITER);
  
  for (y=0;y<size;y++) {
    fprintf(outie,"     ");
    for (x=0;x<size;x++) {
      if (BRD(x,y))
	fprintf(outie, ONCHAR " ");
      else 
	fprintf(outie, OFFCHAR " ");
    }
    fprintf(outie,"\n");
  }
  fprintf(outie,"\n");

}

