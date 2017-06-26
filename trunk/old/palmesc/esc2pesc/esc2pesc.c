#include <stdio.h>
#include <stdlib.h>

#define LASTTILE 45
int fogetc(FILE ** c);
int fogetc(FILE ** c) {
  int _a = fgetc(*c); 
  /*  printf("(called fog: [%d])\n",_a); */
  if (_a == EOF){printf("unexpected EOF\n"); exit(-1);}
  else {  return _a; }
  return 0;
}

static unsigned char transform[LASTTILE] = {
  255, 
  0,       /* floor */
  1,       /* red */
  6,       /* blue */
  8,       /* grey  = box w/ filled square*/
  9,       /* green = [X] */
  11,      /* exit = ! */
  13,      /* hole = black dot */
  36,      /* yellow = boulder */
  5,       /* shooter = starburst */
  4,       /* panel = O */
  6,       /* stopsign = blue */
  6,6,6,6, /* arrows = blue */
  3,       /* rough = grey */
  2,       /* electric = /\/\/ */
  16,      /* electro button on */
  17,      /* electro button off */
  15,      /* transporter = Box, square, X */
  14,      /* breakable = crackcrap */
  19,18,   /* horiz, vert sliders */
  7,10,    /* zero, one tiles */
  21,25,26,
  22,23,20,/* vert, topright, topleft, botright, botleft, horiz */
  24,      /* pink */
  27,28,29,/* BGR buttons */
  33,30,
  34,31,
  35,32   /* BGR up/dn */
};

typedef struct {
  unsigned char t,x,y,z,o;
} tiletype;

#define MAXX 18
#define MAXY 10

static tiletype * board = 0;

int main (int argc, char ** argv) {
  FILE * inways, * outways;
  int y;
  char name[512];
  if (argc != 2) {
    printf("%s: use as %s filename (.esc assumed)\n", argv[0],argv[0]);
    exit(0);
  }

  board = malloc(MAXX*MAXY * sizeof (tiletype));
  
  sprintf(name,"%s.esc",argv[1]);

  inways = fopen(name,"rb+");

  sprintf(name,"out.psc");
  outways = fopen(name,"wb+");

  if (!(inways && outways)) {
    printf("Couldn't open those files.\n");
    exit(-1);
  }

  /*
    for (y=0;y<(MAXY*MAXX);y++)
    fputc(transform[fgetc(inways)],outways);
    for (y=0;y<(MAXY*MAXX);y++)
    fputc(fgetc(inways),outways);
    for (y=0;y<(MAXY*MAXX);y++)
    fputc(fgetc(inways),outways);

    for (y=0;y<26;y++)
    fputc(fgetc(inways),outways);

  */

  for (y=0;y<(MAXY*MAXX);y++)
    fprintf(outways,"%3d,%s",(transform[fogetc(&inways)]),(!((y+1)%18))?"\n":"");
  for (y=0;y<(MAXY*MAXX);y++)
    fprintf(outways,"%3d,%s",(fogetc(&inways)-1),(!((y+1)%18))?"\n":"");
  for (y=0;y<(MAXY*MAXX);y++)
    fprintf(outways,"%3d,%s",(fogetc(&inways)-1),(!((y+1)%18))?"\n":"");

  for (y=0;y<2;y++)
    fprintf(outways,"%3d,%s",(fogetc(&inways)-1),(!((y+1)%18))?"\n":"");

  for (y=0;y<24;y++)
    fprintf(outways,"%3d,%s",(fogetc(&inways)),(!((y+1)%18))?"\n":"");
  
  fprintf(outways,"0\n");

  fclose(inways);
  fclose(outways);

  return 0;
}

