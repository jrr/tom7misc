
#include <stdio.h>

main (int argc, char ** argv) {

     int * data, len,n,score,x,c;

     if (argc<2) {
          printf("%s length\n",argv[0]);
          exit(-1);
     }
     len = atoi(argv[1]);

     if (!(data = (int*)malloc((1<<len)*sizeof (int)))) {
          printf("No memory.\n");
          exit(-1);
     }

     for (n=0;n<(1<<len);n++) data[n]=0;

     while (1) {
          n=len;
          score=0;
          while (n--) {
                    if (EOF == (c=getc(stdin))) goto gone;
                    score |= (c-'0')<<n;
                    }
          data[score]++;
     }
     gone:
     score=0;
     for (n=0;n<(1<<len);n++) { if (score < data[n]) score=data[n]; }
     for (n=0;n<(1<<len);n++) {
          for (x=len;x--;) putchar ('0' + ((n&(1<<x)?1:0)   ));
          printf (": %d :", data[n]);
          for (x=(20.*(data[n]/(float)score));x--;) putchar('*');
          putchar('\n');
     }
     free (data);
}
