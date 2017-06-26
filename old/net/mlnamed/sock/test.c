
#include <sys/time.h>
#include <netinet/in.h>
#include <unistd.h>
#include <errno.h>

int main () {
  int s, err, i;
  struct sockaddr_in addr;

  printf("socket(%d,%d,0)\n", AF_INET, SOCK_STREAM);
  s = socket(AF_INET, SOCK_STREAM, 0);

  printf("ret: %d\n", s);

  if (s == -1) {
    printf("....? ");
    perror("oops ");
  }

  memset(&addr, 0, sizeof (struct sockaddr_in));

  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(3490);

  for(i=0; i < sizeof (struct sockaddr_in); i++) {
    printf("%02X ", 255 & ((unsigned char*)&addr)[i]);
  }
  printf("\nbind_inet(%d, %08X, %d);\n", s, INADDR_ANY, 3490);


  err = bind(s, (struct sockaddr*)&addr, sizeof (struct sockaddr_in));
  if (err) {
    printf(".... ");
    perror("hey ");
  }

}
