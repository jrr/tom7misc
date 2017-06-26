
#include <stdio.h>
#include <stdlib.h>


main (int argc, char**argv) {
     FILE * inways;

     if (argc==2) {
          if (inways = fopen(argv[1],"rb")) {
               long sum=0;
               int c;

               while ((c = getc(inways)) != EOF)
                    sum += c;

               fclose(inways);

               printf("%s: %04X %04X\n",argv[1],65535&(sum>>16),
                                                65535&sum);

          } else { printf ("Unable to open %s.\n", argv[1]); }

     } else { printf ("%s filename\n",argv[0]); }

exit (0);
}
