/* ----------------------------------------------=[ global.h ]=------------

     Global header file; includes for both clients and servers.

 -------------------=[ LUDUS ]=------------------------------------------ */

#ifndef LUDUS_GLOBAL_H
#define LUDUS_GLOBAL_H

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <string.h>

#define uint   unsigned int
#define ulong  unsigned long
#define uchar  unsigned char
#define ushort unsigned short

#ifdef LUDUS_OFFICIAL_BUILD

  #include "auth.h"

#endif

#ifdef LUDUS_CLIENT
  #include <allegro.h>
  #include "text.h"
  #include "dirt.h"
  #include "map.h"
  #include "window.h"
  #include "limp.h"
  #include "colors.h"
  #include "dict.h"

  extern string DATAF;
  #define DATAFI ((char*)DATAF.c_str()) /* I love C++ */

#endif

void bootlog(string n,char*,int);
#endif
