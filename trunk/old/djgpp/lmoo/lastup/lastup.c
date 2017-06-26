#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int main (int argc,char**argv ){
     int a = time(NULL);
     FILE * poo;
     if (argc != 2) {
          printf ("%s filename\n", argv[0]);
          exit(-1);
     }
     if ((poo = fopen(argv[1],"wb+"))) {
          fprintf(poo,"%s\n", ctime(&a));
          fclose(poo);
     } else printf ("Couldn't open file %s.\n", argv[1]);
     exit(0);
}
