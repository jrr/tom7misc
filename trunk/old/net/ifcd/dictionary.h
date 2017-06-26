
#ifndef TOM7_DICT_H
#define TOM7_DICT_H

#include <string>
#include <fstream.h>
#include <stdio.h>

#define DICTHASH 100003

struct dictnode {
  
  dictnode (const string & k, dictnode * n = 0) : key(k), next(n) {}
  
  string key;
  dictnode * next;

  private:

  dictnode (); /* don't use */

};

struct dict {

  dict(const string & s); /* from infile */

  bool lookup(const string & s); /* want lowercase */

  private:
  
  dict(); /* don't use */

  dictnode * table [ DICTHASH ];

};

#endif
