#ifndef _CC_LIB_WEBSERVER_H
#define _CC_LIB_WEBSERVER_H

#include <cstdint>
#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <optional>

struct WebServer {
  static WebServer *Create();
  virtual ~WebServer();

  // If successful, keeps listening until Stop is called (e.g. in
  // another thread), and then returns true.
  // Returns false on failure (e.g. port already in use).
  virtual bool ListenOn(uint16_t port) = 0;
  
  // XXX could just be destructor?
  virtual void Stop() = 0;

  // Add a handler. Handlers are checked (in the order that they are added)
  // for the path in a request. If a handler's prefix matches the request,
  // the handler function is called on the request and must return a response.
  //
  // To do custom dispatching, just register the prefix "/" which starts all
  // valid requests.
  struct Request;
  struct Response;
  using Handler = std::function<Response(const Request &)>;
  virtual void AddHandler(const std::string &prefix, Handler handler) = 0;
  
  // Built-in handler that displays server stats.
  // e.g. server.AddHandler("/stats", server.GetStatsHandler());
  virtual Handler GetStatsHandler() const = 0;
  // TODO: Static file handler, directory listing, that kinda stuff.

  // An incoming request.
  struct Request {
    /* HTTP method (GET, POST, PUT, ...) */
    std::string method;
    /* HTTP version string (HTTP/1.0) */
    std::string version;
    /* HTTP path/URI ( /index.html?name=Forrest%20Heller ) */
    std::string path;

    // Content length as sent by the client. 
    int content_length = 0;
    /* the request body. Used for POST forms and JSON blobs */
    std::string body;
    /* HTTP request headers - use headerInRequest to find the header
       you're looking for. */
    std::vector<std::pair<std::string, std::string>> headers;
    /* warning: request line strings truncated? */
    bool body_truncated = false;

    // Utilities
    // Find a header's value (case insensitive) or return nullptr.
    const std::string *GetHeader(const std::string &name) const;
    // Decoded URL parameters in the order they appear in the URL.
    // A parameter can appear more than once. Unparseable parameters
    // may be discarded.
    std::vector<std::pair<std::string, std::string>> Params() const;

    // Get the first occurrence of the url param and decode it,
    // or return nullopt if not present.
    std::optional<std::string> StringURLParam(const std::string &name) const;
    // Get the first occurrence of the url param, decode and parse
    // it as an integer, or return nullopt otherwise.
    std::optional<int64_t> IntURLParam(const std::string &name) const;
  };

  /* You create one of these for the server to send. Use one of the
     responseAlloc functions. You can fill out the body field using
     the heapString* functions. You can also specify a filenameToSend
     which will be sent using regular file streaming. This is so you
     don't have to load the entire file into memory all at once to
     send it. */
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

  // Named counters (global to the process), revealed by StatsHandler.
  // Thread-safe.
  struct Counter {
    virtual void IncrementBy(int64_t v) = 0;
    inline void Increment() { IncrementBy(1LL); }
    virtual void SetTo(int64_t v) = 0;
    virtual int64_t Value() = 0;
    
  protected:
    // Use WebServer::GetCounter.
    Counter();
  };

  static Counter *GetCounter(const std::string &name);
  
  // Utilities.

  // Returns e.g. "text/html; charset=UTF-8" for a file whose name
  // ends in ".html". This is quite incomplete; beyond simple stuff
  // try apache2's module?
  static std::string GuessMIMEType(const std::string &filename,
				   const std::string &contents);
  // Basic, slow: Replace " < & etc. with HTML entities.
  static std::string HTMLEscape(const std::string &input);
  
protected:
  // Use factory method.
  WebServer() {};
};

#endif
