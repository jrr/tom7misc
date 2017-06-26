
#include <stdlib.h>
#include <stdio.h>

#ifdef WIN32

#include <process.h>

   #include <io.h>
   #include <fcntl.h>

   #define snprintf _snprintf

#else

   #include <unistd.h>

#endif

#include <string>

using namespace std;

/* pipe

   pipe prog1 args1 ... @ prog2 args2 ... @ progn ... @

   pipe reads from stdin to a file,
   runs prog1 on that file (as a filter),
   then prog2 on that file (as a filter),
    ...
   then prints the file to stdout.

   don't forget the trailing @ !!!

*/

int main (int argc, char ** argv) {
  int iter = 0;
  char fname1[128];
  char fname2[128];
  char cmd[512];

#ifdef WIN32
  _setmode (_fileno (stdout), O_BINARY);
  _setmode (_fileno (stdin), O_BINARY);
#endif


  int a;
  sprintf(fname1, "temp-%d-%d", getpid(), iter++);

  FILE * go;
  if (go = fopen(fname1, "wb")) {
    while (EOF != (a = getchar()))
      fputc(a, go);
  } else {
    fprintf(stderr, "%s: can't open temp file '%s'\n", fname1);
    exit(-1);
  }
  fclose(go);

  string accum;
  for(int i=1;i<argc;i++) {
    if (strcmp(argv[i], "@")) {
      accum += argv[i];
      accum += ' ';
    } else {
      sprintf(fname2, "temp-%d-%d", getpid(), iter++);
      snprintf(cmd, 510, "%s < %s > %s", accum.c_str(), fname1, fname2);
      system(cmd);
      unlink(fname1);
      accum = "";
      strcpy(fname1, fname2);
    }
  }

  if (go = fopen(fname1, "rb")) {
    while (EOF != (a = fgetc(go)))
      putchar(a);
  } else {
    fprintf(stderr, "%s: can't open temp file '%s' (for reading)\n", fname1);
    exit(-1);
  }
  
  fclose(go);

  unlink(fname1);

  return 0;
}
