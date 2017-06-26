/* Contains source modified from limp, distributed under the
   gpl. Therefore, this code is also GPL.
   http://www.gnu.org/ (c) 1998 Tom Murphy 7 */

extern char id_aux[];
char id_aux[] = "$Id: auxil.cc,v 1.1 2005/02/03 00:19:04 tom7 Exp $";

//#include "icy.h"
#include "tom.h"
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream.h>
#include "aux.h"
#define whitespc(c) ((c)==' '||(c)=='\t'||(c)=='\n'||(c)=='\r')
#define uint unsigned int

string itos(int n) {
  char boof[128];
  sprintf(boof,"%d",n);
  return (string) boof;
}

/* optimize me */
string lowercase(string s) {
  string out;
  for (uint x=0;x<s.length();x++)
    if (s[x] >= 'A' && s[x] <= 'Z') out += (char)(s[x]|32);
    else out += s[x];
  return out;
}

string uppercase(string s) {
  string out;
  for (uint x=0;x<s.length();x++)
    if (s[x] >= 'a' && s[x] <= 'z') out += (char)(s[x]&~32);
    else out += s[x];
  return out;
}

string lastcolons(int skips,string in) {
  for (uint x=0;x<in.length();x++)
    if(in[x] == ':')
      if (skips) skips--;
      else return in.substr(x+1,in.length()-x);
  return word(in,-1); /* otherwise return last word */
}

int wmatch(string wc, string test) {
  // wc of form some*.*, etc.
  /*
    const char * wc = wcs.c_str();
    const char * test = tests.c_str();
    uint wcl = wcs.length();
    uint testl = tests.length();
  */
  //  cout << "wmatch([" << wc << "],[" << test << "])\n";

  uint x;
  // walk up to the first * in wc
  for (x=0;x<wc.length();x++) {
    if (wc[x]=='*') break;
    else if (wc[x] != test[x]) return 0;
  }
  if (x>= wc.length()) return 1; // no *'s
  string frag;
  int bloc=x++; // NPOS IS NOT UINT!
  // otherwise now look for fragments, repeat.
  for (;x<wc.length();x++) {
    if (wc[x] == '*') {
      if (frag == "") continue;
      //      cout << "bloc: " << bloc << endl;
      if (((int)(bloc = test.find(frag,bloc))) == (int)-1) {
	return 0;
      } else { bloc += frag.length(); frag=""; }
    } else frag += wc[x];
  }
  // check remaining fragment
  if (frag == "") return 1;
  /*
    cout << "got to test remaning fragment bit\n";
    cout << "frag: [" << frag << "], tests: [" << tests
    << "]\n";
  */
  if (frag.length() > test.length()) return 0;
  // walk through, must end the test string with frag
  test = test.substr(test.length()-frag.length(),frag.length());
  if (test.length() != frag.length()) return 0;
  for (x=0;x<frag.length();x++) 
     if (frag[x] != test[x]) return 0;
  return 1;
}

string losewhites(string in) {
  uint x=0;
  while (whitespc(in[x]) && x<in.length()) { x++; }
  uint y=in.length()-1;
  while (whitespc(in[y]) && y>x) { y--; }
  return in.substr(x,(y-x)+1);
}

string wordsat(string line,int foff) {
  int loc = 1;
  for(uint x=0;x<line.length();x++) {
    switch(line[x]) {
    case ' ' :
    case '\n':
    case '\r':
      loc = 1;
      break;
    default:
      if (loc==1)
	if (!(foff--)) return line.substr(x,line.length() - x);
      loc = 0;
    }
  }
  return "";
}

string word(string line,int foff) {
  line = ' ' + line + ' ';
  if (foff < 0) {
    /* count backwards: */
    int y = (-foff) -1, whites=1,endpos=-1;
    for (int x=line.length()-1;x--;) {
      if (whites) {

	if (!whitespc(line[x])) {
	  // reached nonwhite character.
	  if (!y) {
	    // this is our word.
	    endpos=x;
	    whites=0;
	  } else {
	    y--;
	    whites=0;
	  }
	}
      } else { // (!whites:)
	if (whitespc(line[x])) {
	  if (!y && endpos != -1) {
	    return line.substr(x+1,endpos-x);
	  } else {
	    whites=1;
	  }
	}
      }
    }
    return "";
    /*
      limp_error ((string)"There is no word "+ itos(foff)
      + (string)" in \"" + line + (string)"\"!");
      RET_ERROR;
    */
  } else { // foff is nonnegative
    /* count forwards: */
    int y = foff, whites=1,startpos=-1;
    for (uint x=0;x<line.length();x++) {
      if (whites) {
	if (!whitespc(line[x])) {
	  // reached nonwhite character.
	  if (!y) {
	    // this is our word.
	    startpos=x;
	    whites=0;
	  } else {
	    y--;
	    whites=0;
	  }
	}
      } else { // (!whites:)
	if (whitespc(line[x])) {
	  if (!y && (startpos != -1)) {
	    return line.substr(startpos,x-startpos);
	  } else {
	    whites=1;
	  }
	}
      }
    }
    return "";
    /*
      limp_error ((string)"There is no word "+ itos(foff)
      + (string)" in \"" + line + (string)"\"!");
      RET_ERROR;
    */
  }
}

string cut(string in, int a, int b) {

  int start;
  int len;

  if (a < 0) {
    start = in.length()+a;
    if (b) {
      len = b;
    } else {
      len = -b;
    }
  } else {
    start = a;
    len = b;
  }

  if ((uint)(start+len) > in.length()) {
    /*
      limp_error ("cut size [" + itos(a) + string(",") + itos(b)
      + string("] too "
      " long for \"") + in + "\".");
     RET_ERROR;
     */
    return "";
  }
  return in.substr(start,len);
}

char rotbox[16] = { 0,4,1,7,5,13,14,3,2,15,10,6,11,8,12,9 };

#define ROL(a,b) (((a)<<(b)) | ((a)>>(32-b)))
#define ROR(a,b) (((a)>>(b)) | ((a)<<(32-b)))

int tomhash (string s) {
  unsigned int acc=0;
  for (unsigned int x=0;x<s.length();x++) {
    acc = ROL(acc,3);
    acc ^= (s[x]*10007) % 65537;
    acc = ROR(acc,rotbox[(s[x]+x)&15]);
  }
  return acc &0x7fFfFfFf;
}

/* escape string for vsprintf */
/* optimize me    ( substr? )*/
string pctesc(string s) {
  string out;
  for(uint x=0;x < s.length();x++) {
    switch (s[x]) {
    case '%': out += string("%%");
    default: out += s[x];
    } 
  }
}
