
#include "http.h"
#include "escape-util.h"

// XXX this can be improved by using vectors..
#include <vector>
#include <string>

using namespace std;

#define DMSG if (log_message != nullptr) (*log_message)

namespace {
struct HTTP_ : public HTTP {
  HTTP_();
  ~HTTP_() override;
  void setua(string) override;
  bool connect(string host, int port = 80) override;
  HTTPResult get(string path, string &out) override;
  HTTPResult gettempfile(string path, string &file) override;
  HTTPResult put(const string &path,
                 formalist *items,
                 string &out) override;

  void SetCallback(std::function<void(int, int)> cb) override {
    callback = std::move(cb);
  }

 private:

  virtual FILE *TempFile(string &f);

  virtual string readrest();
  virtual string readresttofile();
  virtual string readn(int);
  virtual string readntofile(int);

  virtual HTTPResult req_general(string req, string &res, bool tofile);
  virtual HTTPResult get_general(string path, string &arg, bool tofile);
  virtual void bye() {
    if (conn) {
      SDLNet_TCP_Close(conn);
      conn = 0;
    }
  }

  std::function<void(int, int)> callback = [](int a, int b){};

  string ua;

  IPaddress remote;

  TCPsocket conn = 0;

  /* for virtual servers */
  string host;
};

HTTPResult HTTP_::get(string path, string &out_) {
  DMSG(EscapeUtil::ptos(this) + " get(" + path + ")\n");
  return get_general(path, out_, false);
}

HTTPResult HTTP_::gettempfile(string path, string &out_) {
  DMSG(EscapeUtil::ptos(this) + " gettempfile(" + path + ")\n");
  return get_general(path, out_, true);
}

HTTP_::HTTP_() {
  remote.host = 0;
  remote.port = 0;
  log_message = nullptr;
}

HTTP_::~HTTP_() {
  /* ? */
  if (conn) {
    DMSG(EscapeUtil::ptos(this) + " destroy\n");
    SDLNet_TCP_Close(conn);
  }
}

void HTTP_::setua(string s) {
  ua = std::move(s);
}

bool HTTP_::connect(string chost, int port) {
  DMSG(EscapeUtil::ptos(this) + " connect '" + chost + "':" + itos(port) + "\n");

  /* should work for "snoot.org" or "128.2.194.11" */
  if (SDLNet_ResolveHost(&remote, (char *)chost.c_str(), port)) {
    DMSG(EscapeUtil::ptos(this) + " can't resolve: " +
         (string)(SDLNet_GetError()) + "\n");
    return false;
  }

  host = chost;

  DMSG(EscapeUtil::ptos(this) + " ok\n");

  return true;
}

void append(string &s, char *vec, unsigned int l) {
  unsigned int slen = s.length();
  unsigned int nlen = l + slen;
  string ret(nlen, '*');

  for (unsigned int u = 0; u < slen; u++) {
    ret[u] = s[u];
  }
  for (unsigned int v = 0; v < l; v++) {
    ret[v + slen] = vec[v];
  }

  s = ret;
}

bool sendall(TCPsocket socket, string d) {
  int len = d.length();
  if (len != SDLNet_TCP_Send(socket, (void*)d.c_str(), len)) return false;
  else return true;
}

HTTPResult HTTP_::put(const string &path,
                      formalist *items,
                      string &out) {

  /* large positive randomish number */
  int bnd = 0x10000000 | (0x7FFFFFFE & (EscapeUtil::random()));

  string boundary = "---------------------------" + itos(bnd);

  /* precompute body because we needs its length */
  string body = "--" + boundary;

  /* in loop, body ends with boundary ( no \r\n ) */
  while (items) {
    switch (items->ty) {
    case FT_ARG:
      body += "\r\nContent-Disposition: form-data; name=\"" +
        items->name + "\"\r\n\r\n" +
        items->content;
      break;
    default:
    case FT_FILE:
      body += "\r\nContent-Disposition: form-data; name=\"" +
        items->name + "\"; filename=\"" +
        items->filename + "\"\r\n"
        "Content-Type: application/octet-stream\r\n\r\n" +
        items->content;
      break;
    }
    body += "\r\n--" + boundary;
    items = items->next;
  }

  body += "--\r\n";

  int clen = body.length();

  /* XXX if I use http/1.1 here, result has some extra
     crap at the beginning */
  string hdr =
    "POST " + path + " HTTP/1.0\r\n"
    "User-Agent: " + ua + "\r\n"
    "Host: " + host + "\r\n"
    "Accept: */*\r\n"
    //    "Connection: close\r\n"
    "Content-Type: multipart/form-data; boundary=" + boundary + "\r\n"
    "Content-Length: " + itos(clen) + "\r\n"
    "\r\n";

  return req_general(hdr + body, out, false);
}


/*
  GET /abcd HTTP/1.0
  User-Agent: Wget/1.5.3
  Host: gs82.sp.cs.cmu.edu:8888
  Accept: * / * (together)
*/

HTTPResult HTTP_::get_general(string path, string &res, bool tofile) {
  string req =
    "GET " + path + " HTTP/1.0\r\n"
    "User-Agent: " + ua + "\r\n"
    "Host: " + host + "\r\n"
    "Accept: */*\r\n"
    "\r\n";

  return req_general(req, res, tofile);
}

HTTPResult HTTP_::req_general(string req, string &res, bool tofile) {
  DMSG(EscapeUtil::ptos(this) + " conn@" + EscapeUtil::ptos(conn) +
        " req_general: \n[" + req + "]\n");

  /* we don't use keep-alive now. each request is a new
     connection. */
  bye();
  if (! (conn = SDLNet_TCP_Open(&remote))) {
    DMSG(EscapeUtil::ptos(this) + " can't connect: " +
          (string)(SDLNet_GetError()) + "\n");
    return HTTPResult::ERROR_OTHER;
  }

  DMSG(EscapeUtil::ptos(this) + " connected.\n");

  if (!sendall(conn, req)) {
    /* error. try again? */
    DMSG(EscapeUtil::ptos(this) + " can't send: " +
         (string)(SDLNet_GetError()) + "\n");
    SDLNet_TCP_Close(conn);
    conn = 0;
    return HTTPResult::ERROR_OTHER;
  }

  DMSG(EscapeUtil::ptos(this) + " sent request.\n");

  /* read headers. */
  string cline;
  char c;
  int first = 1;

  /* could save other headers, but this
     is the only one we really need. */
  int contentlen = -1;
  enum cty { CT_NONE, CT_CLOSE, CT_KEEP, };
  cty connecttype = CT_NONE;

  /* first will be true if this is the first line of the
     response. cline holds the partially completed line.

     unfortunately we must read single bytes at this point
     so that we don't accidentally move into the data area.
  */
  for (;;) {
    if (SDLNet_TCP_Recv(conn, &c, 1) != 1) {
      DMSG(EscapeUtil::ptos(this) + " can't recv: " +
           (string)(SDLNet_GetError()) + "\n");
      printf("Error in recv.\n");
      bye();
      return HTTPResult::ERROR_OTHER;
    }

    {
      string ss = " ";
      ss[0] = c;
      DMSG((string)"[" + ss + "]");
    }

    if (c == '\r') continue;
    if (c == '\n') {
      /* process a line. */
      string line = cline;
      cline = "";

      if (first) {

        /* HTTP/1.x */
        EscapeUtil::chop(line);
        /* 200 */
        string status = EscapeUtil::chop(line);

        if (status != "200") {
          /* XXX or whatever ... */
          DMSG(EscapeUtil::ptos(this) + " got status code " + status + "\n");
          /* close connection, since we don't want to read
             anything. */
          bye();
          return HTTPResult::ERROR_404;
        }
        first = 0;
      } else { /* not first line */
        if (line == "") {
          /* empty line means read content! */
          DMSG("++content mode++\n");
          goto readcontent;
        } else { /* is a header line */

          DMSG("header line [" + line + "]\n");

          string field = EscapeUtil::chop(line);

          /*
            HTTP/1.1 200 OK
            Date: Sun, 28 Sep 2003 21:07:49 GMT
            Server: Apache/1.3.26 (Unix) mod_fastcgi/2.2.12
            Last-Modified: Thu, 05 Dec 2002 15:22:12 GMT
            ETag: "c784b-158-3def6f24"
            Accept-Ranges: bytes
            Content-Length: 344
            Connection: close
            Content-Type: text/html

            (content)
          */

          if (field == "Content-Length:") {
            string l = EscapeUtil::chop(line);
            contentlen = atoi(l.c_str());
            DMSG("content length is " + itos (contentlen) + "\n");
          } else if (field == "Connection:") {
            string how = EscapeUtil::lcase(EscapeUtil::chop(line));
            if (how == "close")
              connecttype = CT_CLOSE;
            else if (how == "keepalive")
              connecttype = CT_KEEP;
            else {
              /* bad header */
              DMSG("bad connection type\n");
              bye();
              return HTTPResult::ERROR_OTHER;
            }
          } else {
            /* ignored */
          }
        }

      }

    } else { /* c != \n */
      cline += c;
    }
  } /* for ever */

    /* expect contentlen > -1,
       or connection == CT_CLOSE. (checked)
       connection is in state ready to receive data.
    */
 readcontent:

  if (contentlen == -1) {

    if (connecttype != CT_CLOSE) {
      DMSG("content length but not close\n");
      bye();
      return HTTPResult::ERROR_OTHER;
    }

    /* read until failure */

    if (tofile) {
      res = readresttofile();
      /* printf("rtof: %s\n", res.c_str()); */
      return HTTPResult::OK;
    } else {
      res = readrest();
      return HTTPResult::OK;
    }

  } else {
    /* have content length */

    if (tofile) {
      res = readntofile(contentlen);
      /* printf("ntof: %s\n", res.c_str()); */
      return HTTPResult::OK;
    } else {
      res = readn(contentlen);
      return HTTPResult::OK;
    }

  }

  /* XXX unreachable */
  return HTTPResult::ERROR_OTHER;
}

#define BUFLEN 1024

/* XXX use EscapeUtil::tempfile */
FILE *HTTP_::TempFile(string &f) {
  static int call = 0;
  int pid = EscapeUtil::getpid();
  int tries = 256;
  call++;
  while (tries--) {
    char fname[256];
    sprintf(fname, "dl%d%04X%04X.deleteme", call, pid,
            (int)(0xFFFF & EscapeUtil::random()));

    FILE *ret = EscapeUtil::open_new(fname);
    if (ret) {
      f = fname;
      return ret;
    }
  }
  return 0;
}

string HTTP_::readresttofile() {
  string fname;
  FILE *ff = TempFile(fname);

  DMSG("reading to file ... not logged.\n");

  if (!ff) return "";

  /* printf("fname %s\n", fname.c_str()); */

  char buf[BUFLEN];

  int n = 0;

  int x;
  while ((x = SDLNet_TCP_Recv(conn, buf, BUFLEN)) > 0) {
    fwrite(buf, 1, x, ff);
    callback(n += x, -1);
  }

  fclose(ff);

  SDLNet_TCP_Close(conn);
  conn = 0;

  return fname;
}

string HTTP_::readrest() {
  string acc;

  char buf[BUFLEN];

  int x;

  int n = 0;

  DMSG("reading rest...\n");

  while ((x = SDLNet_TCP_Recv(conn, buf, BUFLEN)) > 0) {
    append(acc, buf, x);
    callback(n += x, -1);
  }

  SDLNet_TCP_Close(conn);
  conn = 0;

  DMSG("contents\n[" + acc + "]\n");

  return acc;
}

string HTTP_::readn(int n) {
  vector<char> buf;
  buf.resize(n);

  DMSG("reading " + itos(n) + "...\n");

  int total = n;
  int rem = n;
  int done = 0;
  int x;
  while (rem > 0) {
    x = SDLNet_TCP_Recv(conn, buf.data() + done, rem);
    if (x <= 0) return "";

    done += x;
    rem -= x;
    callback(done, total);
  }

  string ret = "";
  append(ret, buf.data(), n);

  DMSG("contents\n[" + ret + "]\n");

  return ret;
}

string HTTP_::readntofile(int n) {
  int total = n;
  int rem = n;

  string fname;
  FILE *ff = TempFile(fname);

  if (!ff) return "";

  /* printf("fname %s\n", fname.c_str()); */

  char buf[BUFLEN];

  int done = 0;
  int x;
  while (rem > 0) {
    x = SDLNet_TCP_Recv(conn, buf, std::min(BUFLEN,rem));
    if (x <= 0) {
      fclose(ff);
      printf("bad exit from readntofile\n");
      return "";
    }

    fwrite(buf, 1, x, ff);

    rem -= x;
    callback(done += x, total);
  }

  fclose(ff);
  return fname;
}
}  // namespace

/* export through http interface */
HTTP *HTTP::Create() {
  return new HTTP_();
}
