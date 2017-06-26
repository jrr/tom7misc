
/* Distributed under the terms of the GNU GPL, see
   http://www.gnu.org/copyleft/gpl.html for details */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <unistd.h>

void dispterm(int i);

typedef struct {
  int tty;
  int waittime; /* for usleep */
} seq;

int main (int argc, char ** argv) {
  int termcount = 0;
  int i, n;
  seq * terms = 0;
  char * tt;
  int DEFWAIT = 5000000;
  for (i=1; i < argc; i++) {
    if (*argv[i] == '-') {
      /* argument */
      if (argv[i][1] == 'w') {}
      else {
	printf ("Unknown argument '%c'\n", argv[i][1]);
	exit(-1);
      }
    } else if ((tt = strchr(argv[i], '-'))) {
      /* contains range */
      int tmp = 0, k;
      for (k=0;argv[i][k] != '-';k++) {
	tmp *= 10;
	tmp += argv[i][k] - '0';
      }
      /* 3-5  would give two, but there are 3 4 5 here. */
      termcount += 1 + abs(tmp - atoi(tt+1));
    } else {
      /* is a single terminal */
      termcount ++;
    }
  }

  if (!termcount) {
    printf("cycles between terminals\nusage: %s 5 6 -w1000 7 -w8000 1-3\n"
	   "-wnnn sets following terminals to wait for nnn/1000 seconds.\n",
	   *argv);
    exit(-1);
  }
  
  terms = (seq*)malloc(sizeof (seq) * termcount);
  
  n = 0;

  for (i=1; i < argc; i++) {
    if (*argv[i] == '-') {
      if (argv[i][1] == 'w') {
	DEFWAIT = atoi(argv[i] + 2) * 1000;
      }
    } else if ((tt = strchr(argv[i], '-'))) {
      /* contains range */
      int tmp = 0, k, tnp;
      for (k=0;argv[i][k] != '-';k++) {
	tmp *= 10;
	tmp += argv[i][k] - '0';
      }
      /* 3-5  would give two, but there are 3 4 5 here. */
      tnp = atoi(tt+1);
      
      if (tnp > tmp) {
	/* foreward */
	for (k=tmp;k<=tnp;k++) {
	  terms[n].tty = k;
	  terms[n++].waittime = DEFWAIT;
	}
      } else {
	/* backwards */
	for (k=tmp;k>=tnp;k--) {
	  terms[n].tty = k;
	  terms[n++].waittime = DEFWAIT;
	}
      }
    } else {
      terms[n].tty = atoi(argv[i]);
      terms[n++].waittime = DEFWAIT;
    }
  }

  for (;;)
    for (i=0;i < termcount; i++) {
      dispterm(terms[i].tty);
      usleep(terms[i].waittime);
    }

}

void dispterm(int i) {
  static char nnn[128];
  printf("\n");
  sprintf(nnn, "cat /dev/vcs%d", i);
  system(nnn);
}

/* $Id: scanterm.c,v 1.1.1.1 2001/04/08 03:43:49 tom7 Exp $ */
