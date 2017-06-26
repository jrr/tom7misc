#include "global.h"
#include <fstream.h>

string dictionary :: lookup ( string key ) {
//  printf("(lookup) head: %p\n", head);
  for (dentry * temp = head; temp; temp = temp->next) {
//    printf("(lookup) temp: %p\n", temp);
//    printf(" Is it [%s]?\n", temp->key.c_str());
    if (temp->key == key) return temp->data;
  }
  return "(nil)";
}

void dictionary :: insert ( string key, string data ) {
//  printf("(( [%s] -> [%s] (%p) ))\n", key.c_str(), data.c_str(),head);
  head = new dentry(key,data,head);
//  printf("(insert)-----Now head is %p\n", head);
//  printf("(insert)-----> {[%s],[%s],(%p)}\n", key.c_str(),
//               data.c_str(),head);
}

void dictionary :: readfile (const char * fna) {
  head = 0;
  ifstream inways(fna);
  if (!inways) { /* ?? */ printf("Couldn't open dict %s.\n", fna); exit(-1); }

  string wrd;
  string key;
  string data, blah;

  while (inways >> blah && blah != "%*") ; /* eat */

  while (inways >> key) {
     while (inways >> wrd && wrd != "%*") data += wrd + ' ';
     insert(key,data.substr(0,data.length()-1));
     data = "";
  }

  inways.close();

//  printf("(end of readfile()): last key == %s (%p)\n", head->key.c_str(),head);
}

dictionary ::~ dictionary () {
//    printf("Destructing dict.\n");
    for (;head;) {
      dentry * temp = head;
      head = head->next;
      delete temp;
    }
}

