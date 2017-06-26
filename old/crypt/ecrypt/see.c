
#include <stdio.h>
#include <pc.h>
#include <conio.h>

#define uchar unsigned char

#define s(a,b,c) m=a[(b)];a[(b)]=a[(c)];a[(c)]=m;

void evolve(uchar*box);
void printbox(uchar*box);
unsigned long long iter=0;
int main (int argc, char ** argv) {
        int x=0,j=0,z=0,m=0;
        uchar box[256], K[256];

     if (argc != 2) { printf("%s key\n", argv[0]); exit(-1); }
     z = strlen(argv[1]);
     for (x=0;x<256;x++) K[box[x]=x]=argv[1][x%z];
     for (x=0;x<256;x++) { j+=box[x]+K[x]; s(box,x,j); }

     _setcursortype ( _NOCURSOR );

     clrscr();
                while (!kbhit()) { evolve(box);
                if (!(++iter&0xFFFFFF)) printbox(box); }

     ScreenSetCursor(20,0);
                getch();

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
           for (x=0;x<size;x++) BOX(BOX(ip)+x) += BOX(ip+x);
           break;
        case 1:
           for (x=0;x<size;x++) BOX(BOX(ip)+x) += ~BOX(ip+x);
           break;
        case 2:
           for (x=0;x<size;x++) BOX(BOX(ip)+x) ^= BOX(ip+x);
           break;
        case 3:
           for (x=0;x<size;x++) BOX(BOX(ip)+x) ^= ~BOX(ip+x);
           break;
        }
      ip+=size+1;
}

void printbox (uchar*box) {
     int x,ones=0,zeros=0,y;
     ScreenSetCursor(0,0);
     for (x=0;x<256;x++) cprintf("%02X%s",box[x],((x+1)%16)?(" "):("\r\n"));
     cprintf("\r\n");
     for (x=0;x<256;x++)
          for (y=0;y<8;y++) if (box[x] & (1<<y)) ones++; else zeros++;
     cprintf ("\r\n    is: %lld  "
              "\r\n    1s: %d  "
              "\r\n    0s: %d  \r\n", iter,ones, zeros);
}
