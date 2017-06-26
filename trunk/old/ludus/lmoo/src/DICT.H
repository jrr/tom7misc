#ifndef TM7_DICTIONARY_H
#define TM7_DICTIONARY_H

#include <string>

struct dentry {
   string key;
   string data;
   dentry * next;
   dentry (string a,string b,dentry* c) { key=a; data=b; next=c; }
};

struct dictionary {
   dentry * head;
   dictionary () {head=NULL;}
  ~dictionary () ;
   void readfile (const char * fna);
   string lookup(string);
   void insert(string,string);
};

#endif
