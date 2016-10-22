
/* maximally simple interface to HTTP
   using SDL_net */

#ifndef __HTTP_H
#define __HTTP_H

#include "SDL.h"
#include "SDL_net.h"

#include <functional>
#include <string>

#include "httputil.h"

/* Before using the HTTP class, you must initialize SDL_net
   with SDLNet_Init(). */

/* results from GET/POST/etc. */
enum class HTTPResult {
  OK, ERROR_404, ERROR_OTHER,
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
  virtual HTTPResult get(std::string path, std::string &out) = 0;

  /* create a temp file (in the cwd) and download to that.
     return the name of the temp file */
  virtual HTTPResult gettempfile(std::string path, std::string &file) = 0;

  /* use post, allowing to upload files */
  virtual HTTPResult put(const std::string &path,
                         formalist *items,
                         std::string &out) = 0;

  /* set callback object. This object is never freed. */
  virtual void SetCallback(std::function<void(int, int)> callback) = 0;

  /* this will be called with various debugging
     messages, if non-null */
  void (*log_message)(const std::string &s);

  virtual ~HTTP() {};
};

#endif
