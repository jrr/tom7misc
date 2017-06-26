#if 0
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#endif


#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <unistd.h>
#include <errno.h>

/* C primitives for ML SockPrim. 
   In general, I try to write as little C code as possible;
   much of the wrapping for these functions is in sockprim.sml.
*/

/* constants used in sockprim.sml. I think I need to do it this way
   because _prim is gone. */

int ML_SOCK_STREAM = SOCK_STREAM;
int ML_SOCK_DGRAM = SOCK_DGRAM;
int ML_AF_UNIX = AF_UNIX;
int ML_AF_INET = AF_INET;

int ML_MSG_OOB = MSG_OOB;
int ML_MSG_DONTROUTE = MSG_DONTROUTE;
int ML_MSG_NOSIGNAL = MSG_NOSIGNAL;
int ML_MSG_PEEK = MSG_PEEK;


/* functions used */

int ml_getpeer(int s, int * outaddr) {
  
  struct sockaddr_in tmp;
  socklen_t ignored = sizeof (struct sockaddr_in);

  if (getpeername(s, (struct sockaddr*)&tmp, &ignored)) {
    /* failure */
    return 0;
  }
  
  *outaddr = ntohl(tmp.sin_addr.s_addr);

  return 1;
}

int ml_alloc_fdset () {
  fd_set * s = (fd_set*) malloc( sizeof (fd_set) );
  if (s) FD_ZERO(s);
  return (int) s;
}

void ml_add_fdset (int fd, fd_set * s) {
  FD_SET(fd, s);
}

int ml_check_fdset (int fd, fd_set * s) {
  return FD_ISSET(fd, s);
}

void ml_select (int n, fd_set * ins, fd_set * outs, fd_set * exns, 
		int sec, int usec) {
  struct timeval t;
  t.tv_sec = sec;
  t.tv_usec = usec;

  select(n, ins, outs, exns, &t);
}

int ml_bind_inet (int sock, int inaddr, int port) {
  struct sockaddr_in addr;

  memset(&addr, 0, sizeof (struct sockaddr_in));

  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inaddr;
  addr.sin_port = htons(port);

  return bind(sock, (struct sockaddr*)&addr, sizeof (struct sockaddr_in));

}

int ml_errno () {
  return errno;
}

int ml_accept(int s, int * inaddr, int * port) {

  int unused = sizeof (struct sockaddr_in);
  int conn;
  struct sockaddr_in address;

  conn = accept(s, (struct sockaddr*)&address, &unused);

  *inaddr = address.sin_addr.s_addr;
  *port = ntohs(address.sin_port);

  return conn;

}

int ml_send(int s, const char * vec, int start, int len, int flags) {
  return send(s, vec + start, len, flags);
}

int ml_recv(int s, char * arr, int start, int len, int flags) {
  return recv(s, arr + start, len, flags);
}

int ml_sendto(int s, const char * vec, int start, int len, int inaddr, int port, int flags) {
  struct sockaddr_in addr;

  memset(&addr, 0, sizeof (struct sockaddr_in));

  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inaddr;
  addr.sin_port = htons(port);
  
  return sendto(s, vec + start, len, flags, (struct sockaddr*)&addr, sizeof (struct sockaddr_in));
}

/* sets addr and port for caller */
int ml_recvfrom(int s, char * arr, int start, int len, int flags, int * addr, int * port) {
  int n;
  int flen;
  struct sockaddr_in from;

  /* XXX necessary? */
  memset(&from, 0, sizeof (struct sockaddr_in));

  flen = sizeof (struct sockaddr_in);

  n = recvfrom(s, arr + start, len, flags, (struct sockaddr*)&from, &flen);

  /* ok even if recvfrom failed */
  *addr = from.sin_addr.s_addr;
  *port = ntohs(from.sin_port);

  return n;
}
