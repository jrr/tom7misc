#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h> /* for memset */

struct ii {int a,b;};

void pipes(ii * a,  ii *);

ii  integers(int);
void makesock(int*,int*), 
  sendblock(ii,const char*);

ii filternotmult(ii insock, int multof);
ii primes(ii insock);

const char * itoa(int);
const char * getblock(ii);

//void ignoresigpipe(int);

int main () {
  //  signal(SIGPIPE, ignoresigpipe);
    
  printf("main is (%d)\n", getpid());

  ii i = primes(integers(2));
 
  for (;;)
    printf("(%s)\n", getblock(i));

  return 0;
}


ii primes(ii insock) {
  ii retme, child;
  
  pipes(&retme, &child);
  
  if (fork()) return retme;
  else for (;;) {
    retme.a = atoi(getblock(insock));
    sendblock(child,itoa(retme.a));
    insock = filternotmult(insock, retme.a);
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
  /* forks a child process, returns a port on localhost which streams
     integers */
  ii retme, child;
  
  pipes(&retme,&child);

  if (fork()) return retme;
  else {
    printf("integers is (%d)\n", getpid());
    for(;;) sendblock(child,itoa(start++));
  }
}

void pipes(ii * a,  ii * b) {
  int one[2], two[2];
  
  pipe(one);
  pipe(two);

  a->a = two[0];
  a->b = one[1];
  b->a = one[0];
  b->b = two[1];
}
  
static char inbuf[64];

const char * getblock(ii z) {
  
  if ( -1 == read(z.a, inbuf, 63)) {
    perror("getblock");
    printf("%d: getblock: recv error, exiting\n", getpid());
    exit(0);
  }

  if ( (-1 == write(z.b,"*",1))) {
    perror("getblock");
    printf("%d: getblock: send error, exiting\n", getpid());
    exit(0);
  } 
  
  return inbuf;
}

void sendblock(ii z, const char * msg) {

  /* nothing */
  
  if ( (-1 == write(z.b,msg,strlen(msg)))) {
    perror("sendblock");
    printf("%d: send error, exiting\n", getpid());
    exit(0); /* unable to send; program exited */
  } 
  
  /* wait for ack */

  if ( -1 == read(z.a, inbuf, 63)) {
    perror("sendblock");
    printf("%d: recv error, exiting\n", getpid());
    exit(0); /* ditto */
  }

}

char g[128];
const char * itoa(int d) {
  sprintf(g,"%d", d);
  return g;
}
