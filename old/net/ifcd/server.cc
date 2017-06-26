#include "ifc.h"

extern char id_server[];
char id_server[] = "$Id: server.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";

//************************//
#define DEVEL
//************************//

char * iendl = "\n";

int serversock;

list<user>    ** USERS    = 0;
list<channel> ** CHANNELS = 0;

list<pending> *  PENDING  = 0;

void tests();
void tests() {

  list<int> * lame = 0;
  for (int x=30;x--;) {
    printf("(listpush)\n");
    listpush(lame, new int(3));
    lame -> validate();
  }
  
  while (lame) {

    for (list<int> * l = lame; l ; l = l -> next) {
      
      if (rand() & 7 == 7) {
	printf("%p->remove(%p)\n",l,lame);
	l->remove(lame);
	lame->validate();
	goto outs;
      }
    }
  outs:;
  }
   
}

int main () {

  //  tests();
  
  init();

  serverloop();

  clean(0);
  
  return 0; /* lint */
}

void clean(int sig) {
  printf("***** ifcd shutting down\n");

  for (int x=0;x<HASHSIZE;x++) {
    delete USERS[x];
    delete CHANNELS[x];
  }
  
  delete [] USERS;
  delete [] CHANNELS;

  close(serversock);
  if (sig) exit(-1); else exit (0);
}

char * ols[] = {
"IFC's not IRC.",
"A dragon is just a frog that has eaten a fire scroll.",
"My hovercraft is full of eels.",
"Plod, Diplodocus",
"Giant Deserted Dancefloor",
"Like Thor on a Nordic Track",
"Conventional Wisdom is nothing more than Conventional.",
"Short sentences seem true.",
"\"I am Capital Z, I am The End.\"",
"Poor minds also think alike.",
"The use of adverbs gets you nowhere fast.",
"Everyone Wants to see that Groovy Thing...",

0};

// A Dragon is just a frog that has eaten a fire scroll.
char * oneliner() {
  /* return a random one-liner */
  static int counter=0;
  if (ols[counter]) return ols[counter++];
  else return ols[counter=0];
}
