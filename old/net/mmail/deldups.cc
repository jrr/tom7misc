
/* remove duplicates from the email list.
   duplicates are by e-mail address only (second field);
   case insensitive.  All records must be well formed!

   ./deldups < weird-al-list.txt > no-dupes.txt
*/

/* tom7 - 6.25.99 */

#include <stdio.h>
#include <string>
#include <iostream.h>

unsigned int tomhash(const char * s);

/* if it loops forever, try this */
#define HASHSIZE 14999

struct item {
  int used;
  string name;
  string email;
  int id;
  int unsubkey;
  item() : used(0) {}
  item(string a, string b, int i, int u) 
    : used(1), name(a), email(b), id(i), unsubkey(u) {}
};

item table[HASHSIZE];

int main () {
  int linecount = 0,
    goodcount = 0;

  string name, email;
  int id, unsubkey;
  
  while (cin >> name >> email >> id >> unsubkey) {
   
    /* insert into hash table if it's not there */

    linecount ++;
    int n = tomhash(email.c_str()) % HASHSIZE,r=2;
    while (table[n].used) {
      /* is it a duplicate? */
      if (!strcasecmp(table[n].email.c_str(),email.c_str())) goto ooh;
      n *= r++;
      n %= HASHSIZE;
    }
    /* not found, so insert */
    table[n] = item(name, email, id, unsubkey);
    ++ goodcount;
  ooh:
    ;
  }

  /* print whole table */

  for (int i=0;i<HASHSIZE;i++) {
    if (table[i].used) {
      printf("%s %s %d %d\n", 
	     table[i].name.c_str(),
	     table[i].email.c_str(),
	     table[i].id,
	     table[i].unsubkey);
    }
  }

  fprintf(stderr,
	  "Done.\n"
	  "Read: %d\n"
	  "Good: %d\n",
	  linecount,
	  goodcount);

  return 0;
}

unsigned int tomhash(const char * s) {
  unsigned int start = 0xA5C3D791;
  unsigned int len = 1;
  for(;*s;s++) {
    start ^= 32|(*s); /* case insensitive */

    start = (start << 13) | (start >> 19);

    start ^= (len << 3);
    
    len += 357;
    len *= 17;
  }

  return start;
}
