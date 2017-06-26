/* dataflow network in unix C with forks/pipes.
   - Tom 7 / 1999 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

typedef struct {int a,b;} ii;

void pipes(ii * a,  ii *);

ii  integers(int);
void sendblock(ii,const char*);

ii filternotmult(ii insock, int multof);
ii primes(ii insock);

const char * itoa(int);
const char * getblock(ii);

int main () {
  ii i = primes(integers(2));
 
  for (;;)
    printf("%s\n", getblock(i));

  return 0;
}


ii primes(ii insock) {
  ii retme, child;
  
  pipes(&retme, &child);
  
  if (fork()) return retme;
  else for (;;) {
    int x = atoi(getblock(insock));
    sendblock(child,itoa(x));
    insock = filternotmult(insock, x);
  }
}

ii filternotmult(ii insock, int multof) {
  
  ii retme, child;
  
  pipes(&retme,&child);
  
  if (fork()) return retme;
  else for(;;) {
    retme.a = atoi(getblock(insock));
    if (retme.a % multof) sendblock(child,itoa(retme.a));
  }
  
}

ii integers(int start) {
  ii retme, child;
  
  pipes(&retme,&child);

  if (fork()) return retme;
  else for(;;) sendblock(child,itoa(start++));

}

void pipes(ii * a,  ii * b) {
  int one[2], two[2];

  if (-1 == pipe(one) || -1 == pipe(two)) {
    printf("No more file descriptors!\n");
    exit(1);
  }

  a->a = two[0];
  a->b = one[1];
  b->a = one[0];
  b->b = two[1];
}
  
static char inbuf[64];

const char * getblock(ii z) {
  if ( -1 == read(z.a, inbuf, 63) || -1 == write(z.b,"*",1))
    exit(0);
  return inbuf;
}

void sendblock(ii z, const char * msg) {
  if (-1 == write(z.b,msg,strlen(msg)) || 
      -1 == read(z.a, inbuf, 63))
    exit(0);
}

char g[128];
const char * itoa(int d) {
  sprintf(g,"%d", d);
  return g;
}

