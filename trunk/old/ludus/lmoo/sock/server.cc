#include <stdio.h>
#include <string.h>
#include <pc.h>

#include "winsock.h"
#include "bootp.h"

char buf [16384];
int main (void)
{
  SocketInit ();

  printf ("\r\n\r\n");

  int a, b;
  Socket s (AF_INET, SOCK_STREAM, IPPROTO_TCP);

  InetAddress Bind = (InetAddress) {AF_INET, htons (0), 0};
  s.Bind (&Bind, sizeof (InetAddress));
  printf ("Bind        : ");
  PrintIP (Bind);

  InetAddress Sock;
  s.GetSockName (&Sock, sizeof (InetAddress));
  printf ("GetSockName : ");
  PrintIP (Sock);

  //
  // Run program
  // Do http://127.0.0.1:correct_port_number in Netscape
  // Program reports info
  // Netscape gets file.
  //

  s.Listen (5);

  do
  {
    InetAddress Connect;
    Socket z (s, &Connect, sizeof (InetAddress));

    if (z._Socket)
    {
      a = 16384;
      z.SetOption (&a, SOL_SOCKET, SO_SNDBUF, 4);
      z.SetOption (&a, SOL_SOCKET, SO_RCVBUF, 4);

      printf ("Connect     : ");
      PrintIP (Connect);

      z.GetSockName (&Sock, sizeof (InetAddress));
      printf ("GetSockName : ");
      PrintIP (Sock);

      InetAddress Peer;
      z.GetPeerName (&Peer, sizeof (InetAddress));
      printf ("GetPeerName : ");
      PrintIP (Peer);

      a = 0;
      do
      {
        a += z.Recv (&buf [a], 16384, 0, &Connect, sizeof (InetAddress));
        buf [a] = 0;
      } while (!strchr (buf, 0x0d));
      printf (buf);

      if (*((int *) buf) == 0x20544547)  // "GET "
      {
        *((int *) buf) = 0x73636f64;  // Replace "GET " with "docs"
        for (a = 0; (buf [a] != ' ') && buf [a]; ++ a);

        buf [a] = 0;
        if (buf [a - 1] == '/') strcat (buf, "index.htm");
           // Add "index.html" if the url ends with a '/'.

        FILE *f;
        if ((f = fopen (buf, "rb")))
        {
          do
          {
            a = fread (buf, 1, 16384, f);

            if (a != -1) for (b = 0;
                              b < a;
                              b += z.Send (&buf [b], a - b, 0, &Connect, sizeof (InetAddress)));
          } while (a && (a != -1));
          fclose (f);
        } else
        { // File Not Found
          char message [] = "File Not Found\r\n";
          z.Send (message, sizeof (message), 0, &Connect, sizeof (InetAddress));
        }
      }
    }
  } while (!kbhit ());

  return 0;
}

