#ifndef _ESOCKET_H_
#define _ESOCKET_H_

#include <string>

enum { ES_OKAY, ES_ALSOOK, ES_WAITPACKET, ES_DUNNO, ES_WAITDATA, 
       ES_DISCONNECTED, ES_VERYBAD, ES_PACKET_READY, ES_NOTCONNECTED, };

class esocket {
  int sock;
  int bytestoread;
  int bytesread;
  int status;
  char *buf;
public:  
  //int isPacketReady();
  int    getpacketlen();
  string getpacket();
  int writePacket(const char *b, int len);
  esocket();
  esocket(int s);
  esocket(char *addr, int port);
 ~esocket();
  //int update();
  int getpacketstatus();
  int closesock();
  int connectTo(char *addr, int port);
  int goNoDelay();
  int sendmsg(const char *);
  int sendmsg(string);
};

string binword(int b);

short wordbin(unsigned const char s1, unsigned const char s2);
int hackbin(unsigned char c1, unsigned char c2);
string hackbinword(int num);
#endif



