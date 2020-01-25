
/*
  Experimental, in-progress C++ing and simplification of
  'EmbeddedWebServer'. Works but is a mess. License at the bottom
  of the file.

 */

#include "web.h"

/*
Tom's notes:
 - On mingw-64, -DWIN32
 - To link, needed -lws2_32.
*/

/*

This is a very simple web server that you can embed in your
application to both handle requests dynamically and serve files. The
idea is that it has no dependencies and is really easy to drop into a
project. Here's the simplest way to get started:

XXX UPDATE DOCS!
deleted file stuff

1. Call acceptConnectionsUntilStoppedFromEverywhereIPv4(nullptr), which
   will initialize a new server and block. Note: If you just want to
   take connections from a specific inteface/localhost you can use
   acceptConnectionsUntilStopped

2. Fill out createResponseForRequest. Use the responseAlloc* functions
   to return a response or take over the connection yourself and
   return nullptr. The easiest way to serve static files is
   responseAllocServeFileFromRequestPath. The easiest way to serve
   HTML is responseAllocHTML. The easiest way to serve JSON is
   responseAllocJSON. The server will free() your response once it's
   been sent. See the README for a quick example and the EWSDemo.cpp
   file for more examples such as file serving, HTML form processing,
   and JSON.

EWS runs on Windows, Linux, and Mac OS X. It currently requires
dynamic memory especially when dealing with strings. It is *not
suitable for Internet serving* because it has not been thoroughly
designed+tested for security. It uses a thread per connection model,
where each HTTP connection is handled by a newly spawned thread. This
lets certain requests take a long time to handle while other requests
can still quickly be handled.

Tips:

* For debugging use connectionDebugStringCreate
* Gain extra debugging by enabling ews_print_debug
* If you want a clean server shutdown you can use serverInit() +
  acceptConnectionsUntilStopped() + serverDeInit()

See web_test.cc for example.

*/


#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <time.h>
#include <signal.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>

#include <thread>
#include <mutex>
#include <condition_variable>
#include <string>
#include <cstring>
#include <vector>
#include <utility>
#include <string_view>

#ifdef WIN32
  #include <WinSock2.h>
  #include <Ws2tcpip.h>
  #include <Windows.h>

  using ssize_t = int64_t;
  using sockettype = SOCKET;
#else
  #include <unistd.h>
  #include <sys/socket.h>
  #include <netdb.h>
  #include <ifaddrs.h>
  #include <sys/stat.h>
  #include <strings.h>

  using sockettype = int;
#endif


using string_view = std::string_view;
using string = std::string;
template<class T>
using vector = std::vector<T>;
template<class A, class B>
using pair = std::pair<A, B>;

using Request = WebServer::Request;
using Response = WebServer::Response;
using Handler = WebServer::Handler;

using int64 = int64_t;
using uint64 = uint64_t;

/* You can turn these prints on/off. ews_printf generally prints
   warnings + errors while ews_print_debug prints mundane
   information */
#define ews_printf printf
//#define ews_printf(...)
#define ews_printf_debug printf
// #define ews_printf_debug(...)

struct MutexLock {
  explicit MutexLock(std::mutex *m) : m(m) { m->lock(); }
  ~MutexLock() { m->unlock(); }
  std::mutex *m;
};


/* Quick nifty options */
static constexpr bool OptionPrintWholeRequest = false;
/* /status page - makes quite a few things take a lock to update
   counters but it doesn't make much of a difference. This isn't
   something like Nginx or Haywire */
static constexpr bool OptionIncludeStatusPageAndCounters = true;

/* These bound the memory used by a request. */
#define REQUEST_MAX_BODY_LENGTH (128 * 1024 * 1024) /* (rather arbitrary) */

/* the buffer in connection used for sending and receiving. Should be
   big enough to fread(buffer) -> send(buffer) */
#define SEND_RECV_BUFFER_SIZE (16 * 1024)

// Trying to avoid StringPrintf dependency in here, maybe unwisely...
static string Itoa(int64 i) {
  char buf[32];
  sprintf(buf, "%lld", i);
  return buf;
}

/* This contains a full HTTP connection. For every connection, a thread is spawned
   and passed this struct */
struct ServerImpl;
struct Connection {
  explicit Connection(ServerImpl *server) : server(server) {
    // (original code calloc 0's everything which requestParse depends on)
    memset(&remoteAddr, 0, sizeof(struct sockaddr_storage));
  }

  /* Just allocate the buffers in the connection. These go at the beginning of
     the connection in the hopes that they are 'more aligned' */
  char sendRecvBuffer[SEND_RECV_BUFFER_SIZE] = {};
  sockettype socketfd = 0;
  /* Who connected? */
  struct sockaddr_storage remoteAddr;
  socklen_t remoteAddrLength = 0;
  char remoteHost[128] = {};
  char remotePort[16] = {};
  int64 bytes_sent = 0;
  int64 bytes_received = 0;
  Request request;
  /* points back to the server, usually used for the server's globalMutex */
  struct ServerImpl* server = nullptr;
};


static void ignoreSIGPIPE();


struct ServerImpl : public WebServer {
  ServerImpl();

  void Stop() override;
  int ListenOn(uint16_t port) override;

  int AcceptConnectionsUntilStopped(const struct sockaddr* address,
				    socklen_t addressLength);

  vector<pair<string, Handler>> handlers;
  void AddHandler(const string &prefix, Handler h) override {
    handlers.emplace_back(prefix, h);
  }
  Handler GetHandler(const Request &request) const;

  Handler GetDefaultHandler() const {
    return [](const Request &ignored) {
	Response fail;
	fail.code = 404;
	fail.status = "Not Found";
	fail.content_type = "text/html; charset=UTF-8";
	fail.body = "<!doctype html>Not found (no handler)\n";
	return fail;
      };
  }
  
  std::mutex globalMutex;
  bool shouldRun = true;
  sockettype listenerfd = 0;

  /* The rest of the vars just have to do with shutting down the server cleanly.
     It's a lot of work, actually! Much simpler when I just let it run forever */
  bool stopped = false;
  std::mutex stoppedMutex;
  std::condition_variable stoppedCond;

  int activeConnectionCount = 0;
  std::condition_variable connectionFinishedCond;
  std::mutex connectionFinishedLock;
};

Handler ServerImpl::GetHandler(const Request &request) const {
  // In c++20, can use s->starts_with(prefix).
  auto StartsWith = [](string_view big, string_view little) {
      if (big.size() < little.size()) return false;
      return big.substr(0, little.size()) == little;
    };
  
  for (const auto &p : handlers) {
    const string &prefix = p.first;
    // XXX use decoded path
    if (StartsWith(request.path, prefix))
      return p.second;
  }
  return GetDefaultHandler();
}


// TODO: Garbage?

/* use these in createWebServer::ResponseForRequest */
/* Allocate a response with an empty body and no headers */
WebServer::Response* responseAlloc(int code, std::string status, std::string contentType);
WebServer::Response* responseAllocHTML(std::string html);
WebServer::Response* responseAllocHTMLWithStatus(int code, std::string status, std::string html);

/* Error messages for when the request can't be handled properly */
WebServer::Response* responseAlloc400BadRequestHTML(std::string error);
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


/* these counters exist solely for the purpose of the /status demo.
   TODO: Move these to Server object??
 */
static std::mutex counters_lock;
static struct Counters {
  int64 bytesReceived = 0;
  int64 bytesSent = 0;
  int64 totalConnections = 0;
  int64 activeConnections = 0;
} counters;

struct PathInformation {
  bool exists;
  bool isDirectory;
};

static void printIPv4Addresses(uint16_t portInHostOrder);
static void callWSAStartupIfNecessary(void);

#ifdef WIN32 /* Windows implementations of functions available on Linux/Mac OS X */

/* windows function aliases */
#define close(x) closesocket(x)

#endif


static void connectionHandlerThread(void* connectionPointer);

static void URLDecode(const char* encoded, char* decoded, size_t decodedCapacity) {
  enum URLDecodeState {
    Normal,
    PercentFirstDigit,
    PercentSecondDigit
  };
  
  /* We found a value. Unescape the URL. This is probably filled with bugs */
  size_t deci = 0;
  /* these three vars unescape % escaped things */
  URLDecodeState state = Normal;
  char firstDigit = '\0';
  char secondDigit;
  while (1) {
    /* break out the exit conditions */
    /* need to store a null char in decoded[decodedCapacity - 1] */
    if (deci >= decodedCapacity - 1) {
      break;
    }
    /* no encoding string left to process */
    if (*encoded == '\0') {
      break;
    }

    switch (state) {
    case Normal:
      if ('%' == *encoded) {
	state = PercentFirstDigit;
      } else if ('+' == *encoded) {
	decoded[deci] = ' ';
	deci++;
      } else {
	decoded[deci] = *encoded;
	deci++;
      }
      break;
    case PercentFirstDigit:
      // copy the first digit, get the second digit
      firstDigit = *encoded;
      state = PercentSecondDigit;
      break;
    case PercentSecondDigit: {
      secondDigit = *encoded;
      int decodedEscape;
      char hexString[] = {firstDigit, secondDigit, '\0'};
      int items = sscanf(hexString, "%02x", &decodedEscape);
      if (1 == items) {
	decoded[deci] = (char) decodedEscape;
	deci++;
      } else {
	ews_printf("Warning: Unable to decode hex string 0x%s from %s", hexString, encoded);
      }
      state = Normal;
    }
      break;
    }
    encoded++;
  }
  decoded[deci] = '\0';
}


const string *Request::GetHeader(const string &header_name) {
  for (const auto &p : headers) {
    if (0 == strcasecmp(p.first.c_str(), header_name.c_str())) {
      return &p.second;
    }
  }
  return nullptr;
}

#if 0
// TODO: These are static utilties that can be exposed to the user. They're not
// needed in this code. Should port to std::string etc.
/* Aggressively escape strings for URLs. This adds the %12%0f stuff */
static char* strdupEscapeForURL(const char* stringToEscape) {
  struct HeapString escapedString;
  heapStringInit(&escapedString);
  const char* p = stringToEscape;
  while ('\0' != *p) {
    bool isULetter = *p >= 'A' && *p <= 'Z';
    bool isLLetter = *p >= 'a' && *p <= 'z';
    bool isNumber = *p >= '0' && *p <= '9';
    bool isAcceptablePunctuation = ('.' == *p || '/' == *p || '-' == *p);
    if (isULetter || isLLetter || isNumber || isAcceptablePunctuation) {
      heapStringAppendChar(&escapedString, *p);
    } else {
      // huh I guess %02x doesn't work in Windows?? holy cow
      uint8_t pu8 = (uint8_t)*p;
      uint8_t firstDigit = (pu8 & 0xf0) >> 4;
      uint8_t secondDigit = pu8 & 0xf;
      heapStringAppendFormat(&escapedString, "%%%x%x", firstDigit, secondDigit);
    }
    p++;
  }
  return escapedString.contents;
}
#endif

string WebServer::HTMLEscape(const string &input) {
  string output;
  output.reserve(input.size());
  for (char c : input) {
    switch (c) {
    case '"':
      output += "&quot;";
      break;
    case '&':
      output += "&amp;";
      break;
    case '\'':
      output += "&#039;";
      break;
    case '<':
      output += "&lt;";
      break;
    case '>':
      output += "&gt;";
      break;
    default:
      output.push_back(c);
    }
  }
  return output;
}


#if 0
struct HeapString connectionDebugStringCreate(const Connection* connection) {
  struct HeapString debugString;
  heapStringInit(&debugString);
  // heapStringAppendFormat(&debugString, "%s %s from %s:%s\n", connection->request.method, connection->request.path, connection->remoteHost, connection->remotePort);
  heapStringAppendFormat(&debugString, "Request URL Path decoded to '%s'\n", connection->request.pathDecoded);
  heapStringAppendFormat(&debugString, "Bytes sent:%" PRId64 "\n", connection->status.bytesSent);
  heapStringAppendFormat(&debugString, "Bytes received:%" PRId64 "\n", connection->status.bytesReceived);
  heapStringAppendFormat(&debugString, "Final request parse state:%d\n", connection->request.state);
  heapStringAppendFormat(&debugString, "Header count:%" PRIu64 "\n",
			 (uint64) connection->request.headers.size());
  bool firstHeader = true;
  heapStringAppendString(&debugString, "\n*** Request Headers ***\n");
  for (const auto &header : connection->request.headers) {
    if (firstHeader) {
      firstHeader = false;
    }
    heapStringAppendFormat(&debugString, "'%s' = '%s'\n",
			   header.first.c_str(),
			   header.second.c_str());
  }
  if (!connection->request.body.empty()) {
    heapStringAppendFormat(&debugString, "\n*** Request Body ***\n%s\n",
			   connection->request.body.c_str());
  }
  heapStringAppendFormat(&debugString, "\n*** Request Warnings ***\n");
  bool hadWarnings = false;
  if (connection->request.bodyTruncated) {
    heapStringAppendString(&debugString,
			   "bodyTruncated - you can increase REQUEST_MAX_BODY_LENGTH");
    hadWarnings = true;
  }
  if (!hadWarnings) {
    heapStringAppendString(&debugString, "No warnings\n");
  }
  return debugString;
}
#endif

// allocates a response with content = malloc(contentLength + 1) so you can write
// null-terminated strings to it
Response* responseAlloc(int code, string status, string content_type) {
  Response* response = new Response;
  response->code = code;
  response->status = std::move(status);
  response->content_type = std::move(content_type);
  return response;
}

Response* responseAllocHTML(string html) {
  return responseAllocHTMLWithStatus(200, "OK", std::move(html));
}

Response* responseAllocHTMLWithStatus(int code, string status, string html) {
  Response* response = responseAlloc(code, status, "text/html; charset=UTF-8");
  response->body = std::move(html);
  return response;
}

Response* responseAlloc400BadRequestHTML(string error) {
  return responseAllocHTMLWithStatus(
      400, "Bad Request",
      (string)
      "<html><head><title>400 - Bad Request</title></head>"
      "<body>The request made was invalid: " + error +
      (string)"</body></html>");
}

Response* responseAlloc404NotFoundHTML(const char* resourcePathOrNull) {
  if (nullptr == resourcePathOrNull) {
    return responseAllocHTMLWithStatus(
	404, "Not Found",
	"<html><head><title>404 Not Found</title></head><body>"
	"The resource you specified could not be found</body></html>");
  } else {
    return responseAllocHTMLWithStatus(
	404, "Not Found",
	(string)
	"<html><head><title>404 Not Found</title></head><body>"
	"The resource you specified ('" +
	resourcePathOrNull
	+ "') could not be found</body></html>");
  }
}

Response* responseAlloc500InternalErrorHTML(const char* extraInformationOrNull) {
  if (nullptr == extraInformationOrNull) {
    return responseAllocHTMLWithStatus(
	500, "Internal Error",
	"<html><head><title>500 Internal Error</title></head>"
	"<body>There was an internal error while completing your request</body></html>");
  } else {
    return responseAllocHTMLWithStatus(
	500, "Internal Error",
	(string)
	"<html><head><title>500 Internal Error</title></head>"
	"<body>There was an internal error while completing your request. " +
	(string)extraInformationOrNull +
	(string)"</body></html>");
  }
}

/* parses a typical HTTP request looking for the first line: GET /path HTTP/1.0\r\n 
   Data is passed incrementally to Parse, and the object keeps some internal state. */
struct RequestParser { 
  explicit RequestParser(Request *request) : request(request) {}
  Request *request = nullptr;
  
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

  /* internal state for the request parser */
  RequestParseState state = RequestParseState::Method;
  string partial_header_name, partial_header_value;

  bool HaveFirstLine() const {
    return state != RequestParseState::Method &&
      state != RequestParseState::Path;
  }

  bool Finished() const {
    return state == RequestParseState::Done;
  }
  
  void Parse(const char* requestFragment, size_t requestFragmentLength) {
    for (size_t i = 0; i < requestFragmentLength; i++) {
      char c = requestFragment[i];
      switch (state) {
      case RequestParseState::Method:
	if (c == ' ') {
	  state = RequestParseState::Path;
	} else {
	  request->method += c;
	}
	break;
      case RequestParseState::Path:
	if (c == ' ') {
	  /* we are done parsing the path, decode it */
	  URLDecode(request->path.c_str(), request->pathDecoded, sizeof(request->pathDecoded));
	  state = RequestParseState::Version;
	} else {
	  request->path += c;
	}
	break;
      case RequestParseState::Version:
	if (c == '\r') {
	  state = RequestParseState::CR;
	} else {
	  request->version += c;
	}
	break;
      case RequestParseState::HeaderName:
	if (c == ':') {
	  state = RequestParseState::HeaderValue;
	} else if (c == '\r') {
	  // Invalid header...
	  partial_header_name.clear();
	  state = RequestParseState::CR;
	} else  {
	  // Accumulate into temporary state, which is flushed when a complete
	  // header value is found.
	  partial_header_name += c;
	}
	break;
      case RequestParseState::HeaderValue:
	// skip leading spaces.
	if (c == ' ' && partial_header_value.empty()) {
	  /* intentionally skipped */
	} else if (c == '\r') {
	  // Only accept if we have non-empty data.
	  if (!partial_header_name.empty() &&
	      !partial_header_value.empty()) {
	    request->headers.emplace_back(partial_header_name,
					  partial_header_value);
	  }
	  partial_header_name.clear();
	  partial_header_value.clear();
	  state = RequestParseState::CR;
	} else {
	  partial_header_value += c;
	}
	break;
      case RequestParseState::CR:
	if (c == '\n') {
	  state = RequestParseState::CRLF;
	} else {
	  state = RequestParseState::HeaderName;
	}
	break;
      case RequestParseState::CRLF:
	if (c == '\r') {
	  state = RequestParseState::CRLFCR;
	} else {
	  state = RequestParseState::HeaderName;
	  /* this is the first character of the header - replay the HeaderName case so
	     this character gets appended */
	  i--;
	}
	break;
      case RequestParseState::CRLFCR:
	if (c == '\n') {
	  /* assume the request state is done unless we have some Content-Length,
	     which would come from something like a JSON blob */
	  state = RequestParseState::Done;
	  const string* contentLengthHeader = request->GetHeader("Content-Length");
	  if (nullptr != contentLengthHeader) {
	    ews_printf_debug("Incoming request has a body of length %s\n",
			     contentLengthHeader->c_str());
	    /* Note that this limits content length to < 2GB on Windows */
	    long content_length = 0;
	    if (1 == sscanf(contentLengthHeader->c_str(), "%ld", &content_length)) {
	      if (content_length > REQUEST_MAX_BODY_LENGTH) {
		request->bodyTruncated = true;
		content_length = REQUEST_MAX_BODY_LENGTH;
	      }
	      if (content_length < 0) {
		ews_printf_debug("Warning: Incoming request has negative "
				 "content length: %ld\n", content_length);
		content_length = 0;
	      }
	      request->content_length = content_length;
	      request->body.reserve(content_length);
	      state = RequestParseState::Body;
	    }
	  }
	} else {
	  state = RequestParseState::HeaderName;
	}
	break;
      case RequestParseState::Body:
	// PERF can copy in bigger chunks...
	request->body.push_back(c);
	if ((int64)request->body.size() == (int64)request->content_length) {
	  state = RequestParseState::Done;
	}
	break;
      case RequestParseState::Done:
	break;
      }
    }
  }
};

static void requestPrintWarnings(const Request* request,
				 const char* remoteHost, const char* remotePort) {
  if (request->bodyTruncated) {
    ews_printf("Warning: Request from %s:%s body was truncated to %" PRIu64 " bytes\n",
	       remoteHost, remotePort, (uint64)request->body.size());
  }
}


int ServerImpl::ListenOn(uint16_t portInHostOrder) {
  /* In order to keep the code really short I've just assumed
     we want to bind to 0.0.0.0, which is all available interfaces.
     What you actually want to do is call getaddrinfo on command line arguments
     to let users specify the interface and port */
  struct sockaddr_in anyInterfaceIPv4 = {0};
  // also popular inet_addr("127.0.0.1") which is INADDR_LOOPBACK
  anyInterfaceIPv4.sin_addr.s_addr = htonl(INADDR_ANY);
  anyInterfaceIPv4.sin_family = AF_INET;
  anyInterfaceIPv4.sin_port = htons(portInHostOrder);
  return AcceptConnectionsUntilStopped((struct sockaddr*) &anyInterfaceIPv4,
				       sizeof(anyInterfaceIPv4));
}

int ServerImpl::AcceptConnectionsUntilStopped(const struct sockaddr* address,
					      socklen_t addressLength) {
  callWSAStartupIfNecessary();
  /* resolve the local address we are binding to so we can print it out later */
  char addressHost[256];
  char addressPort[20];
  int nameResult = getnameinfo(address, addressLength, addressHost,
			       sizeof(addressHost), addressPort, sizeof(addressPort),
			       NI_NUMERICHOST | NI_NUMERICSERV);
  if (0 != nameResult) {
    ews_printf("AcceptConnectionsUntilStopped: Could not get numeric host name "
	       "and/or port for the argument address.\n");
    strcpy(addressHost, "Unknown");
    strcpy(addressPort, "Unknown");
  }
  listenerfd = socket(address->sa_family, SOCK_STREAM, IPPROTO_TCP);
  if (listenerfd  <= 0) {
    ews_printf("Could not create listener socket: %s = %d\n", strerror(errno), errno);
    return 1;
  }
  /* SO_REUSEADDR tells the kernel to re-use the bind address in certain circumstances.
     I've always found when making debug/test servers that I want this option, 
     especially on Mac OS X */
  int reuse = 1;
  int result = setsockopt(listenerfd, SOL_SOCKET, SO_REUSEADDR, (char*)&reuse, sizeof(reuse));
  if (0 != result) {
    ews_printf("Failed to setsockopt SO_REUSEADDR = true with %s = %d. "
	       "Continuing because we might still succeed...\n",
	       strerror(errno), errno);
  }

  if (address->sa_family == AF_INET6) {
    int ipv6only = 0;
    result = setsockopt(listenerfd, IPPROTO_IPV6, IPV6_V6ONLY,
			(char*)&ipv6only, sizeof(ipv6only));
    if (0 != result) {
      ews_printf("Failed to setsockopt IPV6_V6ONLY = true with %s = %d. "
		 "This is not supported on BSD/macOS\n", strerror(errno), errno);
    }
  }

  result = bind(listenerfd, address, addressLength);
  if (0 != result) {
    ews_printf("Could not bind to %s:%s %s = %d\n",
	       addressHost, addressPort, strerror(errno), errno);
    return 1;
  }
  /* listen for the maximum possible amount of connections */
  result = listen(listenerfd, SOMAXCONN);
  if (0 != result) {
    ews_printf("Could not listen for SOMAXCONN (%d) connections. %s = %d. "
	       "Continuing because we might still succeed...\n",
	       SOMAXCONN, strerror(errno), errno);
  }
  /* print out the addresses we're listening on. 
     Special-case IPv4 0.0.0.0 bind-to-all-interfaces */
  bool printed = false;
  if (address->sa_family == AF_INET) {
    const struct sockaddr_in* addressIPv4 = (const struct sockaddr_in*) address;
    if (INADDR_ANY == addressIPv4->sin_addr.s_addr) {
      printIPv4Addresses(ntohs(addressIPv4->sin_port));
      printed = true;
    }
  }
  if (!printed) {
    ews_printf("Listening for connections on %s:%s\n", addressHost, addressPort);
  }
  /* allocate a connection (which sets connection->remoteAddrLength) and accept the
     next inbound connection */
  Connection* nextConnection = new Connection(this);
  while (shouldRun) {
    nextConnection->remoteAddrLength = sizeof(nextConnection->remoteAddr);
    nextConnection->socketfd =
      accept(listenerfd, (struct sockaddr*) &nextConnection->remoteAddr,
	     &nextConnection->remoteAddrLength);
    // XXX Apparently socketfd unsigned on win32? If so, this is not the right
    // way to be testing this?
    if (-1 == (int)nextConnection->socketfd) {
      if (errno == EINTR) {
	ews_printf("accept was interrupted, continuing if server.shouldRun is true...\n");
	continue;
      }
      if (errno == EBADF) {
	ews_printf("accept was stopped because the file descriptor is invalid (EBADF). "
		   "This is probably because you closed it?\n");
	continue;
      }
      ews_printf("exiting because accept failed (probably interrupted) %s = %d\n",
		 strerror(errno), errno);
      break;
    }

    {
      MutexLock ml(&connectionFinishedLock);
      activeConnectionCount++;
    }

    /* we just received a new connection, spawn a thread */
    std::thread connectionThread(connectionHandlerThread, nextConnection);
    connectionThread.detach();
    nextConnection = new Connection(this);
  }


  {
    MutexLock ml(&globalMutex);
    if (0 != listenerfd && errno != EBADF) {
      close(listenerfd);
    }
  }

  delete nextConnection;

  {
    std::unique_lock<std::mutex> lock_count(connectionFinishedLock);
    while (activeConnectionCount > 0) {
      ews_printf_debug("Active connection cound is %d, waiting for it go to 0...\n",
		       activeConnectionCount);
      // XXX can use predicate
      connectionFinishedCond.wait(lock_count);
    }
  }

  stoppedMutex.lock();
  stopped = true;
  // XXX PERF: "the notifying thread does not need to hold the lock on the same mutex...
  // in fact doing so is a pessimization"
  stoppedCond.notify_all();
  stoppedMutex.unlock();
  return 0;
}


static int SendResponse(Connection* connection, const Response &response,
			ssize_t* bytesSent) {

  string response_header = "HTTP/1.1 ";
  response_header += Itoa(response.code);
  response_header += " ";
  response_header += response.status;
  response_header += "\r\nContent-Type: ";
  response_header += response.content_type;
  response_header += "\r\nContent-Length: ";
  response_header += Itoa(response.body.size());
  response_header += "\r\nServer: cc-lib\r\n";
  for (const auto &h : response.extra_headers) {
    response_header += h.first;
    response_header += ": ";
    response_header += h.second;
    response_header += "\r\n";
  }
  response_header += "\r\n";

  int64 sendResult =
    send(connection->socketfd, response_header.c_str(), response_header.size(), 0);
  if (sendResult != (int64)response_header.size()) {
    ews_printf("Failed to respond to %s:%s because we could not send the HTTP "
	       "response *header*. send returned %ld with %s = %d\n",
               connection->remoteHost,
               connection->remotePort,
               (long) sendResult,
               strerror(errno),
               errno);
    return -1;
  }
  *bytesSent = *bytesSent + sendResult;
  
  /* Second, if a response body exists, send that */
  if (!response.body.empty()) {
    sendResult = send(connection->socketfd, response.body.c_str(), response.body.size(), 0);
    if (sendResult != (ssize_t) response.body.size()) {
      ews_printf("Failed to respond to %s:%s because we could not send the HTTP "
		 "response *body*. send returned %" PRId64 " with %s = %d\n",
		 connection->remoteHost,
		 connection->remotePort,
		 (int64) sendResult,
		 strerror(errno),
		 errno);
      return -1;
    }
    *bytesSent = *bytesSent + sendResult;
  }
  return 0;
}


static void connectionHandlerThread(void* connectionPointer) {
  Connection* connection = (Connection*) connectionPointer;
  getnameinfo((struct sockaddr*) &connection->remoteAddr, connection->remoteAddrLength,
	      connection->remoteHost, sizeof(connection->remoteHost),
	      connection->remotePort, sizeof(connection->remotePort),
	      NI_NUMERICHOST | NI_NUMERICSERV);
  ews_printf_debug("New connection from %s:%s...\n",
		   connection->remoteHost, connection->remotePort);
  if (OptionIncludeStatusPageAndCounters) {
    MutexLock ml(&counters_lock);
    counters.activeConnections++;
    counters.totalConnections++;
  }

  /* first read the request + request body */
  bool madeRequestPrintf = false;
  bool foundRequest = false;
  ssize_t bytesRead;
  RequestParser parser(&connection->request);
  while ((bytesRead = recv(connection->socketfd, connection->sendRecvBuffer,
			   SEND_RECV_BUFFER_SIZE, 0)) > 0) {
    if (OptionPrintWholeRequest) {
      fwrite(connection->sendRecvBuffer, 1, bytesRead, stdout);
    }
    connection->bytes_received += bytesRead;
    parser.Parse(connection->sendRecvBuffer, bytesRead);
    if (parser.HaveFirstLine() && !madeRequestPrintf) {
      ews_printf_debug("Request from %s:%s: %s to %s HTTP version %s\n",
		       connection->remoteHost,
		       connection->remotePort,
		       connection->request.method.c_str(),
		       connection->request.path.c_str(),
		       connection->request.version.c_str());
      madeRequestPrintf = true;
    }
    if (parser.Finished()) {
      foundRequest = true;
      break;
    }
  }
  
  requestPrintWarnings(&connection->request, connection->remoteHost, connection->remotePort);


  ssize_t bytesSent = 0;
  if (foundRequest) {
    Handler handler = connection->server->GetHandler(connection->request);
    Response response = handler(connection->request);

    int result = SendResponse(connection, response, &bytesSent);
    if (0 == result) {
      ews_printf_debug("%s:%s: Responded with HTTP %d %s length %" PRId64 "\n",
		       connection->remoteHost,
		       connection->remotePort,
		       response.code,
		       response.status.c_str(),
		       (int64)bytesSent);
    }
    
    connection->bytes_sent = bytesSent;

  } else {
    ews_printf("No request found from %s:%s? Closing connection. Here's the last "
	       "bytes we received in the request (length %" PRIi64 "). The total "
	       "bytes received on this connection: %" PRIi64 " :\n",
	       connection->remoteHost, connection->remotePort, (int64) bytesRead,
	       connection->bytes_received);
    if (bytesRead > 0) {
      fwrite(connection->sendRecvBuffer, 1, bytesRead, stdout);
    }
  }
  /* we're done */
  close(connection->socketfd);

  {
    MutexLock ml(&counters_lock);
    counters.bytesSent += (ssize_t) connection->bytes_sent;
    counters.bytesReceived += (ssize_t) connection->bytes_received;
    counters.activeConnections--;
  }
  ews_printf_debug("Connection from %s:%s closed\n",
		   connection->remoteHost, connection->remotePort);
  {
    connection->server->connectionFinishedLock.lock();
    connection->server->activeConnectionCount--;
    connection->server->connectionFinishedCond.notify_all();
    connection->server->connectionFinishedLock.unlock();
  }
  delete connection;
  return;
}

static bool StringEndsWith(const string &big, const char* endsWith) {
  size_t bigLength = big.size();
  size_t endsWithLength = strlen(endsWith);
  if (bigLength < endsWithLength) {
    return false;
  }

  for (size_t i = 0; i < endsWithLength; i++) {
    size_t bigIndex = i + (bigLength - endsWithLength);
    if (big[bigIndex] != endsWith[i]) {
      return false;
    }
  }
  return true;
}

string WebServer::GuessMIMEType(const string &filename, const string &contents) {
  // http://libpng.org/pub/png/spec/1.2/PNG-Structure.html
  static constexpr uint8_t PNGMagic[] = {137, 80, 78, 71, 13, 10, 26, 10};
  // http://www.onicos.com/staff/iz/formats/gif.html
  static constexpr uint8_t GIFMagic[] = {'G', 'I', 'F'};
  // ehh pretty shaky http://www.fastgraph.com/help/jpeg_header_format.html
  static constexpr uint8_t JPEGMagic[] = {0xFF, 0xD8};

  // PNG?
  if (contents.size() >= 8) {
    if (0 == memcmp(PNGMagic, contents.c_str(), sizeof(PNGMagic))) {
      return "image/png";
    }
  }
  // GIF?
  if (contents.size() >= 3) {
    if (0 == memcmp(GIFMagic, contents.c_str(), sizeof(GIFMagic))) {
      return "image/gif";
    }
  }
  // JPEG?
  if (contents.size() >= 2) {
    if (0 == memcmp(JPEGMagic, contents.c_str(), sizeof(JPEGMagic))) {
      return "image/jpeg";
    }
  }
  /* just start guessing based on file extension.
     assume utf-8 where it makes sense. */
  if (StringEndsWith(filename, ".html") || StringEndsWith(filename, ".htm")) {
    return "text/html; charset=UTF-8";
  }
  if (StringEndsWith(filename, ".css")) {
    return "text/css";
  }
  if (StringEndsWith(filename, ".gz")) {
    return "application/x-gzip";
  }
  if (StringEndsWith(filename, ".js")) {
    return "application/javascript; charset=UTF-8";
  }
  /* is it a plain text file? Just inspect the first 100 bytes or so for ASCII */
  bool plaintext = true;
  for (int i = 0; (size_t)i < contents.size() && i < 100; i++) {
    // XXX allows some low ascii
    if (contents[i] < 9 || contents[i] > 127) {
      plaintext = false;
      break;
    }
  }
  if (plaintext) {
    return "text/plain";
  }
  /* well that's pretty much all the different file types in existence :) */
  return "application/binary";
}

ServerImpl::ServerImpl() {
  ignoreSIGPIPE();
}

void ServerImpl::Stop() {
  {
    MutexLock ml(&globalMutex);
    shouldRun = false;
    if (listenerfd >= 0) {
      close(listenerfd);
    }
  }

  std::unique_lock<std::mutex> lock_stopped(stoppedMutex);
  stoppedCond.wait(lock_stopped, [this]() {
      return this->stopped;
    });
}


WebServer *WebServer::Create() {
  return new ServerImpl;
}

WebServer::~WebServer() {}

/* Platform specific stubs/handlers */

#ifdef WIN32

static void printIPv4Addresses(uint16_t portInHostOrder){
  /* I forgot how to do this */
  ews_printf("Listening on port %u\n", portInHostOrder);
}


static void callWSAStartupIfNecessary() {
  // try to create a socket, and if that fails because of
  // uninitialized winsock, then initialize winsock
  SOCKET testsocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (INVALID_SOCKET == testsocket && WSANOTINITIALISED == WSAGetLastError()) {
    WSADATA data = { 0 };
    int result = WSAStartup(MAKEWORD(2, 2), &data);
    if (0 != result) {
      ews_printf("Calling WSAStartup failed! It returned %d with GetLastError() = %lld\n",
		 result, (int64)GetLastError());
      abort();
    }
  } else {
    close(testsocket);
  }
}

static void ignoreSIGPIPE() {}

#else
// Linux, Mac, etc.

static void SIGPIPEHandler(int signal) {
  (void) signal;
  /* SIGPIPE happens any time we try to send() and the connection is closed.
     So we just ignore it and check the return code of send...*/
  ews_printf_debug("Ignoring SIGPIPE\n");
}

static void ignoreSIGPIPE() {
  void* previousSIGPIPEHandler = (void*) signal(SIGPIPE, &SIGPIPEHandler);
  if (nullptr != previousSIGPIPEHandler && previousSIGPIPEHandler != &SIGPIPEHandler) {
    ews_printf("Warning: Uninstalled previous SIGPIPE handler:%p and installed our "
	       "handler which ignores SIGPIPE\n", previousSIGPIPEHandler);
  }
}

static void callWSAStartupIfNecessary() {

}

static void printIPv4Addresses(uint16_t portInHostOrder) {
  struct ifaddrs* addrs = nullptr;
  getifaddrs(&addrs);
  struct ifaddrs* p = addrs;
  while (nullptr != p) {
    if (nullptr != p->ifa_addr && p->ifa_addr->sa_family == AF_INET) {
      char hostname[256];
      getnameinfo(p->ifa_addr, sizeof(struct sockaddr_in),
		  hostname, sizeof(hostname), nullptr, 0, NI_NUMERICHOST);
      ews_printf("Listening %s:%u\n", hostname, portInHostOrder);
    }
    p = p->ifa_next;
  }
  if (nullptr != addrs) {
    freeifaddrs(addrs);
  }
}

#endif

/*
Based on EmbeddableWebServer, Copyrightg (c) 2016, 2019 Forrest
Heller, Martin Pulec, Daniel Barry.
https://www.forrestheller.com/embeddable-c-web-server/

Released under the BSD 2-clause license:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
 */
