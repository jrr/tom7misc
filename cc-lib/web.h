#ifndef __CCLIB_WEB_H
#define __CCLIB_WEB_H

#include <cstdint>
#include <string>
#include <vector>
#include <utility>


// XXX implementation detail to .cc
  enum class RequestParseState  {
    Method,
    Path,
    Version,
    HeaderName,
    HeaderValue,
    CR,
    CRLF,
    CRLFCR,
    Body,
    Done,
  };


struct WebServer {
  static WebServer *Create();
  virtual ~WebServer();

  virtual int ListenOn(uint16_t port) = 0;
  
  // XXX could just be destructor?
  virtual void Stop() = 0;


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
    int contentLength = 0;
    /* the request body. Used for POST forms and JSON blobs */
    std::string body;
    /* HTTP request headers - use headerInRequest to find the header you're looking for. */
    std::vector<std::pair<std::string, std::string>> headers;
    /* Since this has many fixed fields, we report when we went over the limit */
    struct Warnings {
      /* request line strings truncated? */
      bool bodyTruncated = false;
    } warnings;

    /* internal state for the request parser */
    RequestParseState state = RequestParseState::Method;
    std::string partial_header_name, partial_header_value;
  };

  /* You create one of these for the server to send. Use one of the responseAlloc functions.
     You can fill out the body field using the heapString* functions. You can also specify a
     filenameToSend which will be sent using regular file streaming. This is so you don't have
     to load the entire file into memory all at once to send it. */
  struct Response {
    int code = 0;
    std::string body;
    char *filenameToSend = nullptr;
    char *status = nullptr;
    char *contentType = nullptr;
    char *extraHeaders = nullptr; // can be nullptr
    ~Response();
  };


protected:
  // Use factory method.
  WebServer() {};
};

/* You fill in this function. Look at request->path for the requested URI */
WebServer::Response* createResponseForRequest(const WebServer::Request* request);

/* use these in createWebServer::ResponseForRequest */
/* Allocate a response with an initial body size that you can strcpy to */
WebServer::Response* responseAlloc(int code, const char* status, const char* contentType);
/* Serve a file from documentRoot. If you just want to serve the current directory over HTTP just do "."
To serve out the current directory like a normal web server do:
responseAllocServeFileFromRequestPath("/", request->path, request->pathDecoded, ".")
To serve files with a prefix do this:
responseAllocServeFileFromRequestPath("/release/current", request->path, request->pathDecoded, "/var/root/www/release-5.0.0") so people will go to:
http://55.55.55.55/release/current and be served /var/root/www/release-5.0.0 */
WebServer::Response* responseAllocServeFileFromRequestPath(const char* pathPrefix, const char* requestPath, const char* requestPathDecoded, const char* documentRoot);
/* You can use heapStringAppend*(&response->body) to dynamically generate the body */
WebServer::Response* responseAllocHTML(const char* html);
WebServer::Response* responseAllocHTMLWithStatus(int code, const char* status, std::string html);

/* If you leave the MIMETypeOrNULL nullptr, the MIME type will be auto-detected */
WebServer::Response* responseAllocWithFile(const char* filename, const char* MIMETypeOrNULL);
/* Error messages for when the request can't be handled properly */
WebServer::Response* responseAlloc400BadRequestHTML(const char* errorMessage);
WebServer::Response* responseAlloc404NotFoundHTML(const char* resourcePathOrNull);
WebServer::Response* responseAlloc500InternalErrorHTML(const char* extraInformationOrNull);

/* Wrappers around strdupDecodeGetorPOSTParam */
char* strdupDecodeGETParam(const char* paramNameIncludingEquals, const struct WebServer::Request* request, const char* valueIfNotFound);
char* strdupDecodePOSTParam(const char* paramNameIncludingEquals, const struct WebServer::Request* request, const char* valueIfNotFound);
/* You can pass this the request->path for GET or request->body.contents for POST. Accepts nullptr for paramString for convenience */
char* strdupDecodeGETorPOSTParam(const char* paramNameIncludingEquals, const char* paramString, const char* valueIfNotFound);
/* If you want to echo back HTML into the value="" attribute or display some user output this will help you (like &gt; &lt;) */
char* strdupEscapeForHTML(const char* stringToEscape);
/* If you have a file you reading/writing across connections you can use this provided pthread mutex so you don't have to make your own */
/* Need to inspect a header in a request? */
const std::string *headerInRequest(const char* headerName, const struct WebServer::Request* request);

#endif
