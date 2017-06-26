
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string>

#include "aux.h"

extern char id_tomhash[];
char id_tomhash[] = "$Id: tomhash.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";

int main () {
  string s;
  while (cin >> s) {
    
    printf("     case 0x%08X:       /* %s */\n"
	   "     \n"
	   "     break;\n", tomhash(s), s.c_str());
  }

  return 0;

}
