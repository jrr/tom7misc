#ifndef _TM7_TOM_H
#define _TM7_TOM_H

#include <string>
#include <time.h>
#include <sys/ioctl.h>

#define ZERO(c) bzero((char *)&c,sizeof (c))

void   clean  (int);

void   info   (const char *);
void   sinfo  (string);
void   fatal  (const char *);
int    kbhit  (      ); 
void   info   (int, const char *);
string pad    (const char *, short);
string esc    (string);
string cesc   (const char *, int);
#endif
