#include <iostream.h>
#include <string>
#include "tom.h"
#include <time.h>
#include "epoch.h"
#include <stdlib.h>
#include <stdio.h>
//#include <sys/filio.h>

int kbhit() {
  int i;
  ioctl(0,FIONREAD,&i);
  return i;
}

void sinfo(string msg) {
   info(msg.c_str());
}

string pad(const char * in, short len) {
   string out=""; char x=1;
   for (int n=0;n<len;n++) if (x && (x=in[n])) out += x; else out += '\0';
   return out;
}
//inline string cesc(const char*b){return esc((string)b);};

string esc(string in) {
   return cesc(in.c_str(),in.length());
}

string cesc(const char * in, int len){
// escapes nonprinting characters
    string out="";
    char buf[16];
    for (int n=0;n<len;n++) 
      if (((unsigned char)in[n])<32 || ((unsigned char)in[n])>=127) {
      sprintf(buf,"\xF7%02x\xF7",(unsigned char)in[n]);
      out += buf;
      } else out += in[n];
    return out;
}

void fatal (const char * message) {
  time_t timeo;
  time(&timeo);
  char * c = ctime(&timeo);
  c[strlen(c)-1] = 0; // why is there a newline in here?
  cout << "(" << c << ") FATAL ERROR: [" << ((message)?message:"...") << "]\n";
  // shutdown
  exit(-1);
}

void info (const char * message) {
  time_t timeo; 
  time(&timeo);
  char * c = ctime(&timeo);
  c[strlen(c)-6] = 0; // Remove the end stuff.
  cout << "[" << (c + 4) << "]: " << message << endl;
}

void info (int idx, const char * message) {
  time_t timeo;
  time(&timeo);
  char * c = ctime(&timeo);
  c[strlen(c)-6]=0;
  cout << "[["<<idx<<"] "<< (c+4) << "]: " << message <<endl;
}
