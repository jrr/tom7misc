
/* maximally simple interface to HTTP
   using SDL_net */

#ifndef __HTTP_H
#define __HTTP_H

#include "SDL.h"
#include "SDL_net.h"
#include <math.h>

#include <string>

#include "httputil.h"

/* Before using the HTTP class, you must initialize SDL_net
   with SDLNet_Init(). */

/* results from GET/POST/etc. */
enum httpresult {
  HT_OK, HT_404, HT_ERROR,
};

/* XXX allow return codes that cancel download */
struct httpcallback {
  /* in bytes. total will be -1 if unknown. */
  virtual void progress(int recvd, int total) = 0;
};

/* interface only */
struct HTTP {
  static HTTP *Create();

  /* set user-agent */
  virtual void setua(std::string) = 0;
  /* ... other stuff ... */

  /* doesn't really connect -- just sets host:port for
     later requests. might fail if can't look up hostname. */
  virtual bool connect(std::string host, int port = 80) = 0;

  /* download the entire thing to a string */
  virtual httpresult get(std::string path, std::string &out) = 0;
  
  /* create a temp file (in the cwd) and download to that.
     return the name of the temp file */
  virtual httpresult gettempfile(std::string path, std::string &file) = 0;

  /* use post, allowing to upload files */
  virtual httpresult put(const std::string &path,
			 formalist *items,
			 std::string &out) = 0;

  /* set callback object. This object is never freed. */
  virtual void setcallback(httpcallback *) = 0;

  /* this will be called with various debugging
     messages, if non-null */
  void (*log_message)(const std::string &s);

  virtual ~HTTP() {};
};

#endif
