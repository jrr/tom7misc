#include <string>
#include <iostream.h>
#include <stdio.h>

#define isprinting(c) (((c) > 32) && ((c) < 255))

int main (int argc, char**argv) {
     if (argc != 2) { cout << "printbin file\n"; exit(0); }

     FILE * inways;

     if (!(inways = fopen(argv[1],"rb"))) {
               cout << "Can't open " << argv[1] << ".\n";
               exit(-1);
          }

     string aside;
     int waiting=0;
     int x=0,c='z';
     for (;;) {
     if ((c==EOF) || ((c = getc(inways))==EOF)     ) waiting=1;
          if (x && !(x%16)) {
               cout << "    " << aside << endl;
               aside = "";
               if (waiting) break;
          } else if (x && !(x%8)) cout << " ";
          if (waiting) cout << "   ";
               else {printf("%02X ", (unsigned char)c);
          if (isprinting((unsigned char)c)) aside += (char)c ;
                         else   aside +=        string(".");
                    }
     x++;
     }
     fclose(inways);
}
