#include <stdio.h>

#define uchar unsigned char

#define s(a,b,c) m=a[(b)];a[(b)]=a[(c)];a[(c)]=m;

void evolve(uchar*box);
void printbox(uchar*box);
int iter=-1;
int main (int argc, char ** argv) {
        int z,x,c,m;
        uchar box[256], K[256],j=0,IP=0;
        FILE * source = stdin, 
             * dest   = stdout;
      /*
        source = fopen("test.in","rb+");
        dest   = fopen("test.out","wb+");
        */

     if (argc != 2) { printf("%s key\n", argv[0]); exit(-1); }

     z = strlen(argv[1]);
     for (x=0;x<256;x++) K[box[x]=x]=argv[1][x%z];
     for (x=0;x<256;x++) { j+=box[x]+K[x]; s(box,x,j); }

        while (EOF != (c=fgetc(source))) {
              fputc(c ^ box[IP++],dest);
              x=127;
              while (x--) 
                   evolve(box);
             }

if (source != stdin )        fclose(source);
if (dest   != stdout)        fclose(dest);

return 0;
}

#define BOX(c) box[(c)&255]

int ip;

void evolve(uchar*box) {
     
    int size,x;
    ip &= 255;
    size = 1 + (box[ip]&7);
        switch (box[ip] >>6) {
        case 0:
           for (x=1;x<=size;x++) BOX(BOX(ip)+x) +=  BOX(ip+x);
           break;
        case 1:
           for (x=1;x<=size;x++) BOX(BOX(ip)+x) += ~BOX(ip+x);
           break;
        case 2:
           for (x=1;x<=size;x++) BOX(BOX(ip)+x) ^=  BOX(ip+x);
           break;
        case 3:
           for (x=1;x<=size;x++) BOX(BOX(ip)+x) ^= ~BOX(ip+x);
           break;
        }
      ip+=size+1;
}
