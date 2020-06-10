
/*
  Experimental, in-progress C++ing and simplification of
  'EmbeddedWebServer'. Works but is a mess. License at the bottom
  of the file.

 */

#include "webserver.h"

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
#include <unordered_map>

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
// #define ews_printf(...)
#define ews_printf_debug printf
// #define ews_printf_debug(...)

struct MutexLock {
  explicit MutexLock(std::mutex *m) : m(m) { m->lock(); }
  ~MutexLock() { m->unlock(); }
  std::mutex *m;
};

struct CounterImpl final : public WebServer::Counter {
  explicit CounterImpl(string s) : name(std::move(s)) {}

  std::mutex m;
  const string name;
  void IncrementBy(int64 v) override {
    MutexLock ml(&m);
    value += v;
  }
  void SetTo(int64 v) override {
    MutexLock ml(&m);
    value = v;
  }
  
  int64 value = 0LL;
};

struct Counters {
  std::mutex m;
  // Canonical order.
  vector<CounterImpl *> counters;
  // Fast access.
  std::unordered_map<string, CounterImpl *> counter_map;

  CounterImpl *GetCounter(const string &s) {
    MutexLock ml(&m);
    CounterImpl *&c = counter_map[s];
    if (c == nullptr) {
      c = new CounterImpl(s);
      counters.push_back(c);
    }
    return c;
  }
};

WebServer::Counter::Counter() {}

// Get counters, allocating them if needed.
Counters *GetCounters() {
  static Counters *c = new Counters;
  return c;
}

WebServer::Counter *WebServer::GetCounter(const string &name) {
  return GetCounters()->GetCounter(name);
}


/* Quick nifty options */
static constexpr bool OptionPrintWholeRequest = false;

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

/* This contains a full HTTP connection. For every connection, a
   thread is spawned and passed this struct */
struct ServerImpl;
struct Connection {
  explicit Connection(ServerImpl *server) : server(server) {
    // (original code calloc 0's everything which requestParse depends on)
    memset(&remote_addr, 0, sizeof(struct sockaddr_storage));
  }

  /* Just allocate the buffers in the connection. These go at the beginning of
     the connection in the hopes that they are 'more aligned' */
  char send_recv_buffer[SEND_RECV_BUFFER_SIZE] = {};
  sockettype socketfd = 0;
  /* Who connected? */
  struct sockaddr_storage remote_addr;
  socklen_t remote_addr_length = 0;
  char remote_host[128] = {};
  char remote_port[16] = {};
  int64 bytes_sent = 0;
  int64 bytes_received = 0;
  Request request;
  /* points back to the server, usually used for the server's global_mutex */
  struct ServerImpl *server = nullptr;
};


static void ignoreSIGPIPE();


struct ServerImpl final : public WebServer {
  ServerImpl();

  void Stop() override;
  int ListenOn(uint16_t port) override;

  int AcceptConnectionsUntilStopped(const struct sockaddr *address,
				    socklen_t addressLength);
  
  vector<pair<string, Handler>> handlers;
  void AddHandler(const string &prefix, Handler h) override {
    MutexLock ml(&global_mutex);
    handlers.emplace_back(prefix, h);
  }
  Handler GetHandler(const Request &request);

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

  Handler GetStatsHandler() const {
    return [](const Request &ignored) {
	Response resp;
	resp.code = 200;
	resp.status = "OK";
	resp.content_type = "text/html; charset=UTF-8";
	resp.body = "<!doctype html>\n"
	  "<style>\n"
	  "  body { font: 12px Verdana,Helvetica,sans-serif; }\n"
	  "</style>\n"
	  "<body>\n"

	  "<table><tr><th>Counter</th><th>Value</th></tr>\n";

	{
	  Counters *counters = GetCounters();
	  MutexLock ml(&counters->m);
	  for (CounterImpl *counter : counters->counters) {
	    MutexLock mlc(&counter->m);
	    resp.body += "<tr><td>";
	    resp.body += counter->name;
	    resp.body += "</td><td>";
	    resp.body += Itoa(counter->value);
	    resp.body += "</td><tr>\n";
	  }
	}

	resp.body += "</table></body>\n";
	
	return resp;
      };
  }
  
  std::mutex global_mutex;
  bool should_run = true;
  sockettype listenerfd = 0;

  /* The rest of the vars just have to do with shutting down the
     server cleanly. It's a lot of work, actually! Much simpler when I
     just let it run forever. */
  bool stopped = false;
  std::mutex stopped_mutex;
  std::condition_variable stopped_cond;

  int active_connection_count = 0;
  std::condition_variable connection_finished_cond;
  std::mutex connection_finished_mutex;

  Counter *total_connections = nullptr;
  Counter *active_connections = nullptr;
  Counter *bytes_sent = nullptr;
  Counter *bytes_received = nullptr;
};

Handler ServerImpl::GetHandler(const Request &request) {
  MutexLock ml(&global_mutex);
  // In c++20, can use s->starts_with(prefix).
  auto StartsWith = [](string_view big, string_view little) {
      if (big.size() < little.size()) return false;
      return big.substr(0, little.size()) == little;
    };

  printf("Get handler for [%s]\n", request.path.c_str());
  fflush(stdout);
  
  for (const auto &p : handlers) {
    const string &prefix = p.first;
    // XXX use decoded path
    if (StartsWith(request.path, prefix))
      return p.second;
  }
  return GetDefaultHandler();
}

static void printIPv4Addresses(uint16_t portInHostOrder);
static void callWSAStartupIfNecessary(void);

#ifdef WIN32 /* Windows implementations of functions available on Linux/Mac OS X */

/* windows function aliases */
#define close(x) closesocket(x)

#endif


static void connectionHandlerThread(void *connection_pointer);

static string URLDecode(const char *encoded) {
  string decoded;
  
  enum URLDecodeState {
    Normal,
    PercentFirstDigit,
    PercentSecondDigit
  };
  
  /* these three vars unescape % escaped things */
  URLDecodeState state = Normal;
  char first_digit = '\0';
  char second_digit = '\0';
  while (1) {
    /* break out the exit conditions */
    /* no encoding string left to process */
    if (*encoded == '\0') {
      break;
    }

    switch (state) {
    case Normal:
      if ('%' == *encoded) {
	state = PercentFirstDigit;
      } else if ('+' == *encoded) {
	decoded.push_back(' ');
      } else {
	decoded.push_back(*encoded);
      }
      break;
    case PercentFirstDigit:
      // copy the first digit, get the second digit
      first_digit = *encoded;
      state = PercentSecondDigit;
      break;
    case PercentSecondDigit: {
      // XXX Do this without sscanf. -tom7
      second_digit = *encoded;
      int decoded_escape;
      char hex_string[] = {first_digit, second_digit, '\0'};
      int items = sscanf(hex_string, "%02x", &decoded_escape);
      if (1 == items) {
	decoded.push_back((char)decoded_escape);
      } else {
	ews_printf("Warning: Unable to decode hex string 0x%s from %s",
		   hex_string, encoded);
      }
      state = Normal;
    }
      break;
    }
    encoded++;
  }
  return decoded;
}


const string *Request::GetHeader(const string &header_name) const {
  for (const auto &p : headers) {
    if (0 == strcasecmp(p.first.c_str(), header_name.c_str())) {
      return &p.second;
    }
  }
  return nullptr;
}

std::vector<std::pair<std::string, std::string>> Request::Params() const {
  // Parameters are separated from path by '?'.
  size_t qpos = path.find('?');
  if (qpos == string::npos) return {};
  
  std::string params = path.substr(qpos + 1, string::npos);
  // ez-cheezy way to make sure each k=v is terminated by an
  // ampersand.
  params.push_back('&');
  
  vector<pair<string, string>> kvs;
  size_t start = 0;
  for (;;) {
    size_t apos = params.find('&', start);
    if (apos == string::npos) break;
    
    string kv = params.substr(start, apos - start);
    size_t epos = kv.find('=');
    if (epos != string::npos) {
      string k = kv.substr(0, epos);
      string v = kv.substr(epos + 1, string::npos);
      kvs.emplace_back(URLDecode(k.c_str()), URLDecode(v.c_str()));
    } else {
      // Skip it...
    }
    start = apos + 1;
  }
  return kvs;
}

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

/* parses a typical HTTP request looking for the first line: 
   GET /path HTTP/1.0\r\n 
   Data is passed incrementally to Parse, and the object keeps some 
   internal state. */
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
  
  void Parse(const char *requestFragment, size_t requestFragmentLength) {
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
	  /* this is the first character of the header - replay the
	     HeaderName case so this character gets appended */
	  i--;
	}
	break;
      case RequestParseState::CRLFCR:
	if (c == '\n') {
	  /* assume the request state is done unless we have some
	     Content-Length, which would come from something like a
	     JSON blob */
	  state = RequestParseState::Done;
	  const string *contentLengthHeader =
	    request->GetHeader("Content-Length");
	  if (nullptr != contentLengthHeader) {
	    ews_printf_debug("Incoming request has a body of length %s\n",
			     contentLengthHeader->c_str());
	    /* Note that this limits content length to < 2GB on Windows */
	    long content_length = 0;
	    if (1 == sscanf(contentLengthHeader->c_str(), "%ld",
			    &content_length)) {
	      if (content_length > REQUEST_MAX_BODY_LENGTH) {
		request->body_truncated = true;
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

static void requestPrintWarnings(const Request *request,
				 const char *remote_host,
				 const char *remote_port) {
  if (request->body_truncated) {
    ews_printf("Warning: Request from %s:%s body was truncated "
	       "to %" PRIu64 " bytes\n",
	       remote_host, remote_port, (uint64)request->body.size());
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

int ServerImpl::AcceptConnectionsUntilStopped(const struct sockaddr *address,
					      socklen_t addressLength) {
  callWSAStartupIfNecessary();
  /* resolve the local address we are binding to so we can print it out later */
  char addressHost[256];
  char addressPort[20];
  int nameResult = getnameinfo(address, addressLength, addressHost,
			       sizeof(addressHost), addressPort,
			       sizeof(addressPort),
			       NI_NUMERICHOST | NI_NUMERICSERV);
  if (0 != nameResult) {
    ews_printf("AcceptConnectionsUntilStopped: Could not get numeric host name "
	       "and/or port for the argument address.\n");
    strcpy(addressHost, "Unknown");
    strcpy(addressPort, "Unknown");
  }
  listenerfd = socket(address->sa_family, SOCK_STREAM, IPPROTO_TCP);
  if (listenerfd  <= 0) {
    ews_printf("Could not create listener socket: %s = %d\n",
	       strerror(errno), errno);
    return 1;
  }
  /* SO_REUSEADDR tells the kernel to re-use the bind address in
     certain circumstances. I've always found when making debug/test
     servers that I want this option, especially on Mac OS X */
  int reuse = 1;
  int result = setsockopt(listenerfd, SOL_SOCKET, SO_REUSEADDR,
			  (char*)&reuse, sizeof(reuse));
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
		 "This is not supported on BSD/macOS\n",
		 strerror(errno), errno);
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
    const struct sockaddr_in *addressIPv4 = (const struct sockaddr_in*) address;
    if (INADDR_ANY == addressIPv4->sin_addr.s_addr) {
      printIPv4Addresses(ntohs(addressIPv4->sin_port));
      printed = true;
    }
  }
  if (!printed) {
    ews_printf("Listening for connections on %s:%s\n",
	       addressHost, addressPort);
  }
  /* allocate a connection (which sets connection->remote_addr_length)
     and accept the next inbound connection */
  Connection *next_connection = new Connection(this);
  while (should_run) {
    next_connection->remote_addr_length = sizeof(next_connection->remote_addr);
    next_connection->socketfd =
      accept(listenerfd, (struct sockaddr*) &next_connection->remote_addr,
	     &next_connection->remote_addr_length);
    // XXX Apparently socketfd unsigned on win32? If so, this is not the right
    // way to be testing this?
    if (-1 == (int)next_connection->socketfd) {
      if (errno == EINTR) {
	ews_printf("accept was interrupted, continuing if server.should_run "
		   "is true...\n");
	continue;
      }
      if (errno == EBADF) {
	ews_printf("accept was stopped because the file descriptor is "
		   "invalid (EBADF). This is probably because you closed "
		   "it?\n");
	continue;
      }
      ews_printf("exiting because accept failed (probably interrupted) "
		 "%s = %d\n",
		 strerror(errno), errno);
      break;
    }

    {
      MutexLock ml(&connection_finished_mutex);
      active_connection_count++;
    }

    /* we just received a new connection, spawn a thread */
    std::thread connectionThread(connectionHandlerThread, next_connection);
    connectionThread.detach();
    next_connection = new Connection(this);
  }


  {
    MutexLock ml(&global_mutex);
    if (0 != listenerfd && errno != EBADF) {
      close(listenerfd);
    }
  }

  delete next_connection;

  {
    std::unique_lock<std::mutex> lock_count(connection_finished_mutex);
    while (active_connection_count > 0) {
      ews_printf_debug(
	  "Active connection cound is %d, waiting for it go to 0...\n",
	  active_connection_count);
      // XXX can use predicate
      connection_finished_cond.wait(lock_count);
    }
  }

  stopped_mutex.lock();
  stopped = true;
  // XXX PERF: "the notifying thread does not need to hold the lock on
  // the same mutex... in fact doing so is a pessimization"
  stopped_cond.notify_all();
  stopped_mutex.unlock();
  return 0;
}


static int SendResponse(Connection *connection, const Response &response,
			ssize_t *bytesSent) {

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
               connection->remote_host,
               connection->remote_port,
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
      ews_printf(
	  "Failed to respond to %s:%s because we could not send the HTTP "
	  "response *body*. send returned %" PRId64 " with %s = %d\n",
	  connection->remote_host,
	  connection->remote_port,
	  (int64) sendResult,
	  strerror(errno),
	  errno);
      return -1;
    }
    *bytesSent = *bytesSent + sendResult;
  }
  return 0;
}


static void connectionHandlerThread(void *connection_pointer) {
  Connection *connection = (Connection*) connection_pointer;
  getnameinfo(
      (struct sockaddr*) &connection->remote_addr,
      connection->remote_addr_length,
      connection->remote_host, sizeof(connection->remote_host),
      connection->remote_port, sizeof(connection->remote_port),
      NI_NUMERICHOST | NI_NUMERICSERV);
  ews_printf_debug("New connection from %s:%s...\n",
		   connection->remote_host, connection->remote_port);
  ServerImpl *server = connection->server;
  server->active_connections->Increment();
  server->total_connections->Increment();

  /* first read the request + request body */
  bool madeRequestPrintf = false;
  bool foundRequest = false;
  ssize_t bytesRead;
  RequestParser parser(&connection->request);
  while ((bytesRead = recv(connection->socketfd, connection->send_recv_buffer,
			   SEND_RECV_BUFFER_SIZE, 0)) > 0) {
    if (OptionPrintWholeRequest) {
      fwrite(connection->send_recv_buffer, 1, bytesRead, stdout);
    }
    connection->bytes_received += bytesRead;
    parser.Parse(connection->send_recv_buffer, bytesRead);
    if (parser.HaveFirstLine() && !madeRequestPrintf) {
      ews_printf_debug("Request from %s:%s: %s to %s HTTP version %s\n",
		       connection->remote_host,
		       connection->remote_port,
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
  
  requestPrintWarnings(&connection->request,
		       connection->remote_host, connection->remote_port);


  ssize_t bytesSent = 0;
  if (foundRequest) {
    Handler handler = connection->server->GetHandler(connection->request);
    Response response = handler(connection->request);

    int result = SendResponse(connection, response, &bytesSent);
    if (0 == result) {
      ews_printf_debug("%s:%s: Responded with HTTP %d %s length %" PRId64 "\n",
		       connection->remote_host,
		       connection->remote_port,
		       response.code,
		       response.status.c_str(),
		       (int64)bytesSent);
    }
    
    connection->bytes_sent = bytesSent;

  } else {
    ews_printf(
	"No request found from %s:%s? Closing connection. Here's the last "
	"bytes we received in the request (length %" PRIi64 "). The total "
	"bytes received on this connection: %" PRIi64 " :\n",
	connection->remote_host, connection->remote_port, (int64) bytesRead,
	connection->bytes_received);
    if (bytesRead > 0) {
      fwrite(connection->send_recv_buffer, 1, bytesRead, stdout);
    }
  }
  /* we're done */
  close(connection->socketfd);

  server->bytes_sent->IncrementBy(connection->bytes_sent);
  server->bytes_received->IncrementBy(connection->bytes_received);
  server->active_connections->IncrementBy(-1LL);
  
  ews_printf_debug("Connection from %s:%s closed\n",
		   connection->remote_host, connection->remote_port);
  {
    connection->server->connection_finished_mutex.lock();
    connection->server->active_connection_count--;
    connection->server->connection_finished_cond.notify_all();
    connection->server->connection_finished_mutex.unlock();
  }
  delete connection;
  return;
}

static bool StringEndsWith(const string &big, const char *ends_with) {
  size_t big_length = big.size();
  size_t ends_with_length = strlen(ends_with);
  if (big_length < ends_with_length) {
    return false;
  }

  for (size_t i = 0; i < ends_with_length; i++) {
    size_t big_index = i + (big_length - ends_with_length);
    if (big[big_index] != ends_with[i]) {
      return false;
    }
  }
  return true;
}

string WebServer::GuessMIMEType(const string &filename,
				const string &contents) {
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
  /* is it a plain text file? Just inspect the first 100 bytes or so
     for ASCII */
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
  total_connections = GetCounter("total connections");
  active_connections = GetCounter("active connections");
  bytes_sent = GetCounter("bytes sent");
  bytes_received = GetCounter("bytes received");
  
  ignoreSIGPIPE();
}

void ServerImpl::Stop() {
  {
    MutexLock ml(&global_mutex);
    should_run = false;
    if (listenerfd >= 0) {
      close(listenerfd);
    }
  }

  std::unique_lock<std::mutex> lock_stopped(stopped_mutex);
  stopped_cond.wait(lock_stopped, [this]() {
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
      ews_printf("Calling WSAStartup failed! It returned %d with "
		 "GetLastError() = %lld\n",
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
  void *previousSIGPIPEHandler = (void*) signal(SIGPIPE, &SIGPIPEHandler);
  if (nullptr != previousSIGPIPEHandler &&
      previousSIGPIPEHandler != &SIGPIPEHandler) {
    ews_printf(
	"Warning: Uninstalled previous SIGPIPE handler:%p and installed our "
	"handler which ignores SIGPIPE\n", previousSIGPIPEHandler);
  }
}

static void callWSAStartupIfNecessary() {

}

static void printIPv4Addresses(uint16_t portInHostOrder) {
  struct ifaddrs *addrs = nullptr;
  getifaddrs(&addrs);
  struct ifaddrs *p = addrs;
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
Based on EmbeddableWebServer, Copyright (c) 2016, 2019 Forrest
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
