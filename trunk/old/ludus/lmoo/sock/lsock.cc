// ---=[ Ludus Sockets Implementation ]=---------------------------------.
//                           prices subject to change and availability   |
//   not to be used as a floatation device.                              |
// ----------------------------------------------=[ lsock.cc ]=----------'

#include "lsock.h"

lsock::lsock() {
  s.Init (AF_INET, SOCK_STREAM, IPPROTO_TCP);
}

lsock::connect(long addy, int port) {
  Connect = (InetAddress) {AF_INET, htons (port), addy};
  s.Connect (&Connect, sizeof (InetAddress));
}

lsock::send(char * text, int length) {
  s.Send (text, length, 0, &Connect, sizeof (InetAddress) );
}

lsock::send(string text) {
  s.Send ((void *)text.c_str(), text.length(), 0, &Connect, sizeof(InetAddress) );
}

lsock::~lsock() { }



