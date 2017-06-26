
#include "dictionary.h"

#include "aux.h" /* for tomhash */

/* simple hash table dictionary */

extern char id_dictionary[];
char id_dictionary[] = "$Id: dictionary.cc,v 1.1 2001/04/08 04:00:23 tom7 Exp $";


dict :: dict (const string & s) {

  printf("dict: initializing dict from \"%s\"\n", s.c_str());

  ifstream f(s.c_str());

  int x = 0;

  for (int i = DICTHASH; i--; ) table[i] = 0;

  if (!f) { 
    printf ("dict error: Can't open wordlist \"%s\"\n", s.c_str());
    return;
  }

  string w;
  while (f >> w) {
    /* NOTE: validate words */

    if (w.length() > 2 && w.length() <= 12) {
      ++ x;
      w = lowercase(w);
      int h = tomhash(w) % DICTHASH;
      table[h] = new dictnode(w, table[h]);
    } /* otherwise ignore word */

  }

  f.close();
    
  printf("dict: inserted %d words\n", x);

}

bool dict :: lookup (const string & s) { /* pass in lowercase */
  
  for (  dictnode * n = table [  tomhash(s) % DICTHASH  ];
	 n;
	 n = n -> next )
    if (s == n -> key) return true;
    
  return false;
}
