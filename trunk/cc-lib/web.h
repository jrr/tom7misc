#ifndef __CCLIB_WEB_H
#define __CCLIB_WEB_H

#include <cstdint>
#include <string>
#include <vector>
#include <utility>
#include <functional>

struct WebServer {
  static WebServer *Create();
  virtual ~WebServer();

  virtual int ListenOn(uint16_t port) = 0;
  
  // XXX could just be destructor?
  virtual void Stop() = 0;

  // Add a handler. Handlers are checked (in the order that they are added)
  // for the path in a request. If a handler's prefix matches the request,
  // the handler function is called on the request and must return a response.
  //
  // The handler function can 
  //
  // To do custom dispatching, just register the prefix "/" which starts all
  // valid requests.
  struct Request;
  struct Response;
  using Handler = std::function<Response(const Request &)>;
  virtual void AddHandler(const std::string &prefix, Handler handler) = 0;
  
  // Built-in handler that displays server stats.
  // e.g. server.AddHandler("/stats", server.StatsHandler());
  // virtual Handler StatsHandler() const = 0;
  // TODO: Static file handler, directory listing, that kinda stuff.

  /* You'll look directly at this struct to handle HTTP requests. It's initialized
     by setting everything to 0 */
  struct Request {
    /* HTTP method (GET, POST, PUT, ...) */
    std::string method;
    /* HTTP version string (HTTP/1.0) */
    std::string version;
    /* HTTP path/URI ( /index.html?name=Forrest%20Heller ) */
    std::string path;
    /* null-terminated HTTP path/URI that has been %-unescaped. Used for a file serving */
    char pathDecoded[1024];
    size_t pathDecodedLength = 0;

    // Content length as sent by the client. 
    int content_length = 0;
    /* the request body. Used for POST forms and JSON blobs */
    std::string body;
    /* HTTP request headers - use headerInRequest to find the header you're looking for. */
    std::vector<std::pair<std::string, std::string>> headers;
    /* warning: request line strings truncated? */
    bool bodyTruncated = false;

    // Utilities
    // Find a header's value (case insensitive) or return nullptr.
    const std::string *GetHeader(const std::string &name);
  };

  /* You create one of these for the server to send. Use one of the responseAlloc functions.
     You can fill out the body field using the heapString* functions. You can also specify a
     filenameToSend which will be sent using regular file streaming. This is so you don't have
     to load the entire file into memory all at once to send it. */
  struct Response {
    // e.g. 404
    int code = 0;
    std::string body;
    // e.g. "Not Found"
    std::string status;
    // e.g. "text/html; charset=UTF-8"
    std::string content_type;
    std::vector<std::pair<std::string, std::string>> extra_headers;
  };

  // Utilities.

  // Returns e.g. "text/html; charset=UTF-8" for a file whose name ends in ".html".
  // This is quite incomplete; beyond simple stuff try apache2's module?
  static std::string GuessMIMEType(const std::string &filename, const std::string &contents);
  // Basic, slow: Replace " < & etc. with HTML entities.
  static std::string HTMLEscape(const std::string &input);
  
protected:
  // Use factory method.
  WebServer() {};
};

#endif
