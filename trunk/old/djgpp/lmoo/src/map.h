#ifndef LUDUS_MAP_H
#define LUDUS_MAP_H

#include <stdlib.h>
#include <stdio.h>
#include <string>

#define ushort unsigned short
#define uchar unsigned char
#define uint unsigned int

typedef struct {
     int w,h;
     ushort * dat[3];
     uchar  * clip;
} lmap;

lmap load_map(string & mapname,
              string & tileset,
              string & defmidi,
              string & palfile,
              string & mapinfo,
              string & objfile,
              string & shortname,
              uchar & flags1,
              uchar & flags2,
              uchar & flags3,
              string filename);

int geteightsnul(FILE * in, string & out);
int fgetword(FILE * in, int & w);
int fgetdword(FILE * in, int & w);
int fgetstringat(FILE * inways, string & super, int loc);

#endif
