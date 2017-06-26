#ifndef _TM7_AUX_H
#define _TM7_AUX_H

#include <string>

int wmatch(string,string);
string losewhites(string);
string word(string,int);
string wordsat(string,int);
string cut (string,int,int);
string lastcolons(int,string);
int tomhash (string s);

string itos(int);

string pctesc (string s);

string lowercase(string);
string uppercase(string);

#endif
