
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef WIN32

   #include <io.h>
   #include <fcntl.h>

   #define snprintf _snprintf

#endif

/* tile [-c n] informat outformat
   takes a bunch of tile descriptions on
   stdin and writes them on stdout. 

   gfx:

   *.++*.++
   *+++*+++
   *+++*+++
   ########
   ++*.++*.
   ++*+++*+
   ++*+++*+
   ########

   (using chars .+*#)

   hex:

   0A 2A F4 ...

   (upper- or lower-case)

   bin:

   Raw binary values.

   If count (c) is given, it affects the placement of newlines in the
   output (by pretending that n bytes preceeded it). This is useful
   only for hex and gfx output styles.

*/

#define CHARS ".+*#"

int main (int argc, char ** argv) {

#ifdef WIN32
  _setmode (_fileno (stdout), O_BINARY);
  _setmode (_fileno (stdin), O_BINARY);
#endif

  if (argc < 3) {
    fprintf(stderr, 
	    "usage: %s [-c n] informat outformat\n"
	    "\nConverts GB tiles from various formats (as a filter).\n"
	    "Where format is g[fx] h[ex], or b[in].\n"
	    "gfx format uses the .+*# characters.\n",
	    "-c n adjusts whitespace by n units (experiment).\n", 
	    *argv);
    exit(-1);
  }
  
  int inmode  = 0; 
  int outmode = 0; 

  unsigned char byte1;
  unsigned char byte2;

  unsigned int i = 0;
  int count = 0;
  
  for(int arg=1;arg < argc; arg++) {
    if (!strcmp(argv[arg], "-c")) {
      count = atoi(argv[++arg]);
    } else if (inmode == 0) {
      inmode = 32 | *argv[arg]; 
    } else if (outmode == 0) {
      outmode = 32 | *argv[arg];
    } else {
      fprintf(stderr, "%s: too many arguments.\n", *argv);
    }
  }

#define UNCHARS(c) (((c) == (unsigned)*CHARS) ? 0 : ((c) == (unsigned)CHARS[1]) ? 1 : ((c) == (unsigned)CHARS[2]) ? 2 : 3)

  /* ----- counts as whitespace for marking boundaries */

#define isws(c) ((c) == ' ' || (c) == '\n' || (c) == '\t' || (c) == '\r' || (c) == '-')

#define HEX(a,b) (( ((a) | 4400) % 55 ) | ( ( ((b) | 4400) % 55 ) << 4))

#define READWSMAYBE(b) \
    while(EOF != (i = getchar()) && isws(i)) ; \
    if (i == EOF) goto done; \
    b = i;

#define READWS(b) \
    while(EOF != (i = getchar()) && isws(i)) ; \
    if (i == EOF) { \
       fprintf(stderr, "%s: premature eof\n", *argv); \
       exit(-1); \
    } \
    b = i;

#define READMAYBE(b) \
  if (EOF == (i = getchar())) goto done; \
  b = i;

#define READSURE(b) \
  if (EOF == (i = getchar())) { \
    fprintf(stderr, "%s: premature eof\n", *argv); \
    exit(-1); \
  } \
  b = i;

  for(;;) {
    byte1 = byte2 = 0;
    switch (inmode) {
    case 'g':
      {
	unsigned int a[8];
      
	READWSMAYBE(a[0]);
	a[0] = UNCHARS(a[0]);
	for(int zz=1; zz < 8; zz++) {
	  READWS(a[zz]);
	  a[zz] = UNCHARS(a[zz]);
	}

	for(int yy=0; yy < 8; yy++) {
	  byte1 <<= 1;
	  byte2 <<= 1;
	  byte1 |= !! (a[yy]&1);
	  byte2 |= !! (a[yy]&2);
	}
      }
      break;
    case 'h': 
      {
	unsigned int a, b, c, d;
	READWSMAYBE(a);
	READWS(b);
	READWS(c);
	READWS(d);
	byte1 = HEX(b,a);
	byte2 = HEX(d,c);
      }
      break;
    case 'b':
      READMAYBE(byte1);
      READSURE(byte2);
      break;
    default:
      fprintf(stderr, "%s: unknown inmode '%c'", *argv, inmode);
      exit(-1);
    }

    switch(outmode) {
    case 'g':

#define BIT(n) (CHARS[ ((!!(byte2 & (1 << n))) << 1) | \
		         (!!(byte1 & (1 << n))) ])

      fprintf(stdout,
	      "%c%c%c%c%c%c%c%c\n",
	      BIT(7),
	      BIT(6),
	      BIT(5),
	      BIT(4),
	      BIT(3),
	      BIT(2),
	      BIT(1),
	      BIT(0));

      count = (count + 2) & 15;
      if (!count) putchar('\n');
	   
      break;
    case 'h':
      fprintf(stdout, "%02X %02X", byte1, byte2);
      count = (count + 2) & 15;
      if (count) putchar(' ');
      else putchar('\n');
      break;
    case 'b':
      putchar(byte1);
      putchar(byte2);
      break;
    default:
      fprintf(stderr, "%s: unknown outmode '%c'", *argv, outmode);
    }
  }
  /* C needs labelled breaks! */
  done:

  return (0);
}
