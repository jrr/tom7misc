#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <dos.h>
#include <conio.h>
#include <string.h>
#include <sys\movedata.h>
#include <sys\nearptr.h>
#include <pc.h>
#include <go32.h>
#include <dpmi.h>

#define SPEEDCOMP     2.3          // speed compensation factor for moving parts //
                       // don't want to over complicate things //
char *scrptr;

typedef struct 
{
     unsigned char red,green,blue;
} pal;

pal palette[256];
char xbuffer[128], ybuffer[128];
char ScreenBuffer[64000];
union REGS r;

/*--------------------------------------------------------------------------*/
// Sets the graphics mode 320x200 with 256 colours //

void Set320x200(void)
{
   r.h.ah=0;
   r.h.al=0x13;
   int86(0x10, &r, &r);
}

/*--------------------------------------------------------------------------*/
// Sets 80 column text mode, 25 rows //

void SetTextMode(void)
{
   r.h.ah=0;
   r.h.al=0x03;
   int86(0x10, &r, &r);
}

/*--------------------------------------------------------------------------*/
// sets a pixel on the screen with the specified colour //
// Most C setpixel calls are very slow, so we write our own one here. //

void setpixel(long x, long y, char col)
{
     *(scrptr + (y*320) + x) = col;     
}

/*--------------------------------------------------------------------------*/
// sets a pixel on the screen buffer with the specified colour //

void setbpixel(unsigned int x, unsigned int y, char col)
{
     int k;
     char * stt;
     k = y*320;
     stt = (char *) ScreenBuffer + k + x;
     * (stt++) = col;
     * stt = col;
     stt += 319;
     * (stt++) = col;
     * stt = col;
     //*((char*) ScreenBuffer + (k) + x) = col;
}

/*--------------------------------------------------------------------------*/
// copies the screen buffer to the real screen, and clears the virtual screen //

void BuffertoScreen(void)
{
     memcpy(scrptr, ScreenBuffer, 64000);     
//     memset(ScreenBuffer, 0, 64000);
}

/*--------------------------------------------------------------------------*/
// sets the palette - surprising eh ? //

void SetPalette(pal *palptr)
{
     short i;
     disable();
     outportw(0x3c8,0);
     for (i=0; i<256; i++)
     {
          outportb(0x3c9, palptr[i].blue>>2);
          outportb(0x3c9, palptr[i].green>>2);
          outportb(0x3c9, palptr[i].red>>2);
     }
     enable();
}

/*--------------------------------------------------------------------------*/
// I'll let you guess what this does //
// I use my own palette format, so if you want to change the palette then you
// will need to re-write Load & SetPalette.

void LoadPalette( char *fname, pal *palbuf )
{
     short i;
     FILE *fp;
     
     fp = fopen( fname, "rb" );
     if (fp==NULL) {
          printf("Error loading %s !\n", fname);
          exit(1);
     }
     fseek(fp, 18, SEEK_CUR);
     for (i=0; i<256; i++)
     {
          fread( (char*) palbuf, 1, 3, fp );          
          palbuf++;
     }     
     fclose( fp );
}

/*--------------------------------------------------------------------------*/

void tomDraw () {
     int y,x,c;float angle;
     int ox1,oy1,ox2,oy2;
     int dx1,dy1,dx2,dy2;

//     int k;
     char * stt;

     dx1 = 2;
     dy2 = 4;
     dx2 = -3;
     dy1 = -2;
     ox1 = oy1 = 100;
     ox2 = 42;
     oy2 = 170;
     do {
     // Recalcitrate origins:
     ox1 += dx1;
     ox2 += dx2;
     oy1 += dy1;
     oy2 += dy2;
if (ox1>319) { dx1 = - dx1; ox1 = 319; }
if (ox2>319) { dx2 = - dx2; ox2 = 319; }
if (ox1<1) {   dx1 = - dx1; ox1 = 1;   }
if (ox2<1) {   dx2 = - dx2; ox2 = 1;   }
if (oy1>199) { dy1 = - dy1; oy1 = 199; }
if (oy2>199) { dy2 = - dy2; oy2 = 199; }
if (oy1<1) {   dy1 = - dy1; oy1 = 1;   }
if (oy2<1) {   dy2 = - dy2; oy2 = 1;   }

     for (y=0; y<200; y+=2)
     for (x=0; x<320; x+=2) {
            // different neat effects.
//          c = ((int)(sqrt(pow(x-ox1,2)+pow(y-oy1,2)) * sqrt(pow(x-ox2,2)+pow(y-oy2,2)))) >> 3;
//          c = ((int)(sqrt(pow(x-ox1,2)+pow(y-oy1,2)) - sqrt(pow(x-ox2,2)+pow(y-oy2,2))));
//            c = ((int)(sqrt(pow(x-ox1,2)+pow(y-oy1,2)) + sqrt(pow(x-ox2,2)+pow(y-oy2,2)))) >> 1;
//            c = ((int)(40*sin(sqrt(pow(x-ox1,2)+pow(y-oy1,2))/10) + 35*sin(sqrt(pow(x-ox2,2)+pow(y-oy2,2))/7)));
            c = ((int)(100*sin(sqrt(pow(x-ox1,2)+pow(y-oy1,2))/10) + 150*sin(sqrt(pow(x-ox2,2)+pow(y-oy2,2))/7)));
            // I think the last one is the best, though slow.

          if (c > 255) c = 512-c;
          c = c % 256;
          if (c== 0 || c==255) c = 254;

     stt = (char *) ScreenBuffer + (y*320) + x;
     * (stt++) = c;
     * stt = c;
     stt += 319;
     * (stt++) = c;
     * stt = c;
     //*((char*) ScreenBuffer + (k) + x) = col;

     //     setbpixel(x,y,c);
     }
     BuffertoScreen();
     } while (!kbhit());
}

int distance(int xorigin, int yorigin, int x, int y) {
     return sqrt(pow(x-xorigin,2)+pow(y-yorigin,2));
}

/*--------------------------------------------------------------------------*/
// set up what happens when :) //

int main (void)
{
    __djgpp_nearptr_enable();
     scrptr = (char*) (__djgpp_conventional_base + 0xa0000);

     LoadPalette("tom1.pal", palette);
     Set320x200();
     SetPalette(palette);
     tomDraw();
     getch();
     
     SetTextMode();
     return(0);
}     

/*--------------------------------------------------------------------------*/
