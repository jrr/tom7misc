
#include <stdlib.h>
#include <stdio.h>
#include <string>

#ifdef WIN32

#include <io.h>
#include <fcntl.h>

#endif

int main (int argc, char ** argv) {

#ifdef WIN32
  _setmode (_fileno (stdout), O_BINARY);
  _setmode (_fileno (stdin), O_BINARY);
#endif

  unsigned char trans[256]; /* encode/decode (symmetric) */

  for(unsigned int i=0; i < 256; i++) {
    trans[i] = i;
  }

  if (argc < 2) {
    fprintf(stderr, "%s: Need a map file on the standard input.\n", *argv);
    exit(-1);
  }

  FILE * mf;
  if ((mf = fopen(argv[1], "rb"))) {
    unsigned int l = 0;
    unsigned int r = 0;
    unsigned int z = 0;

#define isws(c) ((c) == ' ' || (c) == '\n' || (c) == '\t' || (c) == '\r')

#define READOO(i) \
    /* consume whitespace */ \
    while(EOF != (z = fgetc(mf)) && isws(z)) ; \
    if (z == EOF) break; /* bad */ \
    if (z == '?') { /* literal char */ \
      i = fgetc(mf); \
      if (i == EOF) break; \
    } else if (z == '#') { \
      z = fgetc(mf); \
      if (z == EOF) break; \
      i = fgetc(mf); \
      if (i == EOF) break; \
      i = (( i | 4400 ) % 55) + ((( z | 4400) % 55) << 4); \
    } else { \
      fprintf(stderr, \
	      "%s: bad char in mapfile %s: '%c'\n", *argv, argv[1], i); \
      exit(-1); \
    }

    for(;;) {
      READOO(l);
      READOO(r);
      trans[l] = r;
      trans[r] = l;
    }

  } else {
    fprintf(stderr, "%s: Mapfile %s unreadable.\n", *argv, argv[1]);
    exit(-1);
  }

#if 0
  for(int ii=0; ii < 256; ii++) {
    printf("trans[%2X] = %2X\n", (unsigned int)ii, (unsigned int)trans[ii]);
  }
  printf("\n\n");
#endif

  for(int a; EOF != (a = getchar());) {
    putchar(trans[a]);
  }

  return 0;
}
