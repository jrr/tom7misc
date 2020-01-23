
/*
  
  Experimental, maybe doomed, C++ing and simplification of of
  EmbeddedWebServer. Doesn't work yet. License at the bottom
  of the file.

 */

// TODO: header/cc
// heapstring -> std::string, at least in external interface

/*
Tom's notes:
 - looks like this could be gutted and turned into a simple portable
   web server. main value here is socket stuff is done for us.
 - On mingw-64, I -DWIN32 and -D__WIN32__ (not sure if both are necessary)
 - To link, needed -lws2_32.
 - It successfully serves a connection and then aborts "terminate called without an active exception", perhaps fixed now (I think this was because I wasn't detaching the connection threads.)


/* 

This is a very simple web server that you can embed in your
application to both handle requests dynamically and serve files. The
idea is that it has no dependencies and is really easy to drop into a
project. Here's the simplest way to get started:

1. Call acceptConnectionsUntilStoppedFromEverywhereIPv4(NULL), which
   will initialize a new server and block. Note: If you just want to
   take connections from a specific inteface/localhost you can use
   acceptConnectionsUntilStopped

2. Fill out createResponseForRequest. Use the responseAlloc* functions
   to return a response or take over the connection yourself and
   return NULL. The easiest way to serve static files is
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

* Use the heapStringAppend*(&response->body) functions to dynamically
  build a body (see the HTML form POST demo)
* For debugging use connectionDebugStringCreate
* Gain extra debugging by enabling ews_print_debug
* If you want a clean server shutdown you can use serverInit() +
  acceptConnectionsUntilStopped() + serverDeInit()

See web_test.cc for example.

*/


#include <thread>
#include <mutex>
#include <condition_variable>
#include <string>
#include <cstring>

using string = std::string;

/* You can turn these prints on/off. ews_printf generally prints
   warnings + errors while ews_print_debug prints mundane
   information */
#define ews_printf printf
//#define ews_printf(...)
//#define ews_printf_debug printf
#define ews_printf_debug(...)

#include <stdbool.h>

struct MutexLock {
  explicit MutexLock(std::mutex *m) : m(m) { m->lock(); }
  ~MutexLock() { m->unlock(); }
  std::mutex *m;
};


/* Quick nifty options */
static bool OptionPrintWholeRequest = false;
/* /status page - makes quite a few things take a lock to update
   counters but it doesn't make much of a difference. This isn't
   something like Nginx or Haywire */
static bool OptionIncludeStatusPageAndCounters = true;
/* If using responseAllocServeFileFromRequestPath and no index.html is
   found, serve up the directory */
static bool OptionListDirectoryContents = true;
/* Print the entire server response to every request */
static bool OptionPrintResponse = false;

/* These bound the memory used by a request. The headers used to be
   dynamically allocated but I've made them hard coded because: 1.
   Memory used by a request should be bounded 2. It was responsible
   for 2 * headersCount allocations every request */
#define REQUEST_MAX_HEADERS 64
#define REQUEST_HEADERS_MAX_MEMORY (8 * 1024)
#define REQUEST_MAX_BODY_LENGTH (128 * 1024 * 1024) /* (rather arbitrary) */

/* the buffer in connection used for sending and receiving. Should be
   big enough to fread(buffer) -> send(buffer) */
#define SEND_RECV_BUFFER_SIZE (16 * 1024)
/* contains the Response HTTP status and headers */
#define RESPONSE_HEADER_SIZE 1024

#define EMBEDDABLE_WEB_SERVER_VERSION_STRING "1.1.2"
// major = [31:16] minor = [15:8] build = [7:0]
#define EMBEDDABLE_WEB_SERVER_VERSION 0x00010102 

/* has someone already enabled _CRT_SECURE_NO_WARNINGS? If so, don't
   enable it again. If not, disable it for us. */
#ifdef _CRT_SECURE_NO_WARNINGS
#define UNDEFINE_CRT_SECURE_NO_WARNINGS 0
#else
#define UNDEFINE_CRT_SECURE_NO_WARNINGS 1
#define _CRT_SECURE_NO_WARNINGS 1
#endif //_CRT_SECURE_NO_WARNINGS

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

#ifdef WIN32
#include <WinSock2.h>
#include <Ws2tcpip.h>
#include <Windows.h>

typedef int64_t ssize_t;
typedef SOCKET sockettype;
#else
#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ifaddrs.h>
#include <sys/stat.h>
#include <dirent.h>
#include <strings.h>
typedef int sockettype;
#endif


// XXX enum class
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
  EatHeaders,
  Done,
};

/* just a calloc'd C string on the heap */
struct HeapString {
    char* contents; // null-terminated, at least length+1
    size_t length; // this is updated by the heapString* functions
    size_t capacity;
};

/* a string pointing to the request->headerStringPool */
struct PoolString {
    char* contents; // null-terminated
    size_t length;
};

struct Header {
    struct PoolString name;
    struct PoolString value;
};

/* You'll look directly at this struct to handle HTTP requests. It's initialized
   by setting everything to 0 */
struct Request {
  /* null-terminated HTTP method (GET, POST, PUT, ...) */
  char method[64];
  size_t methodLength = 0;
  /* null-terminated HTTP version string (HTTP/1.0) */
  char version[16];
  size_t versionLength = 0;
  /* null-terminated HTTP path/URI ( /index.html?name=Forrest%20Heller ) */
  char path[1024];
  size_t pathLength = 0;
  /* null-terminated HTTP path/URI that has been %-unescaped. Used for a file serving */
  char pathDecoded[1024];
  size_t pathDecodedLength = 0;

  // Content length as sent by the client. 
  int contentLength = 0;
  /* the request body. Used for POST forms and JSON blobs */
  string body;
  /* HTTP request headers - use headerInRequest to find the header you're looking for. These used to be a linked list and that worked well, but it seemed overkill */
  struct Header headers[REQUEST_MAX_HEADERS];
  size_t headersCount = 0;
  /* the this->headers point at this string pool */
  char headersStringPool[REQUEST_HEADERS_MAX_MEMORY];
  size_t headersStringPoolOffset = 0;
  /* Since this has many fixed fields, we report when we went over the limit */
  struct Warnings {
    /* Was some header information discarded because there was not enough room in the pool? */
    bool headersStringPoolExhausted = false;
    /* Were there simply too many headers in this request for us to handle them all? */
    bool tooManyHeaders = false;
    /* request line strings truncated? */
    bool methodTruncated = false;
    bool versionTruncated = false;
    bool pathTruncated = false;
    bool bodyTruncated = false;
  } warnings;
  /* internal state for the request parser */
  RequestParseState state = RequestParseState::Method;
};

struct ConnectionStatus {
  int64_t bytesSent = 0;
  int64_t bytesReceived = 0;
};

/* This contains a full HTTP connection. For every connection, a thread is spawned
 and passed this struct */
struct Server;
struct Connection {
  explicit Connection(Server *server) : server(server) {
    // (original code calloc 0's everything which requestParse depends on)
    memset(&remoteAddr, 0, sizeof(struct sockaddr_storage));
  }
  
  /* Just allocate the buffers in the connection. These go at the beginning of
     the connection in the hopes that they are 'more aligned' */
  char sendRecvBuffer[SEND_RECV_BUFFER_SIZE] = {};
  char responseHeader[RESPONSE_HEADER_SIZE] {};
  sockettype socketfd = 0;
  /* Who connected? */
  struct sockaddr_storage remoteAddr;
  socklen_t remoteAddrLength = 0;
  char remoteHost[128] = {};
  char remotePort[16] = {};
  struct ConnectionStatus status;
  Request request;
  /* points back to the server, usually used for the server's globalMutex */
  struct Server* server = nullptr;
};

/* You create one of these for the server to send. Use one of the responseAlloc functions.
 You can fill out the body field using the heapString* functions. You can also specify a
 filenameToSend which will be sent using regular file streaming. This is so you don't have
 to load the entire file into memory all at once to send it. */
struct Response {
  int code;
  struct HeapString body;
  char* filenameToSend;
  char* status;
  char* contentType;
  char* extraHeaders; // can be NULL
};

#ifdef WIN32
static void ignoreSIGPIPE() {}
#else
static void ignoreSIGPIPE() {
  void* previousSIGPIPEHandler = (void*) signal(SIGPIPE, &SIGPIPEHandler);
  if (NULL != previousSIGPIPEHandler && previousSIGPIPEHandler != &SIGPIPEHandler) {
    ews_printf("Warning: Uninstalled previous SIGPIPE handler:%p and installed our "
	       "handler which ignores SIGPIPE\n", previousSIGPIPEHandler);
  }
}
#endif




struct Server {
  Server();
    
  void Stop();  
  int AcceptConnectionsUntilStoppedFromEverywhereIPv4(uint16_t portInHostOrder);
  int AcceptConnectionsUntilStopped(const struct sockaddr* address, socklen_t addressLength);


  std::mutex globalMutex;
  bool shouldRun = true;
  sockettype listenerfd = 0;
  /* User field for whatever - if your request handler you can do connection->server->tag */
  void* tag = nullptr;

  /* The rest of the vars just have to do with shutting down the server cleanly.
     It's a lot of work, actually! Much simpler when I just let it run forever */
  bool stopped = false;
  std::mutex stoppedMutex;
  std::condition_variable stoppedCond;

  int activeConnectionCount = 0;
  std::condition_variable connectionFinishedCond;
  std::mutex connectionFinishedLock;
};

#ifndef __printflike
#define __printflike(...) // clang (and maybe GCC) has this macro that can check printf/scanf format arguments
#endif


/* You fill in this function. Look at request->path for the requested URI */
struct Response* createResponseForRequest(const struct Request* request, struct Connection* connection);

/* use these in createResponseForRequest */
/* Allocate a response with an initial body size that you can strcpy to */
struct Response* responseAlloc(int code, const char* status, const char* contentType, size_t contentsLength);
/* Serve a file from documentRoot. If you just want to serve the current directory over HTTP just do "."
To serve out the current directory like a normal web server do:
responseAllocServeFileFromRequestPath("/", request->path, request->pathDecoded, ".")
To serve files with a prefix do this:
responseAllocServeFileFromRequestPath("/release/current", request->path, request->pathDecoded, "/var/root/www/release-5.0.0") so people will go to:
http://55.55.55.55/release/current and be served /var/root/www/release-5.0.0 */
struct Response* responseAllocServeFileFromRequestPath(const char* pathPrefix, const char* requestPath, const char* requestPathDecoded, const char* documentRoot);
/* You can use heapStringAppend*(&response->body) to dynamically generate the body */
struct Response* responseAllocHTML(const char* html);
struct Response* responseAllocHTMLWithFormat(const char* format, ...) __printflike(1, 0);
struct Response* responseAllocHTMLWithStatus(int code, const char* status, const char* html);
struct Response* responseAllocJSON(const char* json);
struct Response* responseAllocJSONWithStatus(int code, const char* status, const char* json);
struct Response* responseAllocJSONWithFormat(const char* format, ...) __printflike(1, 0);
struct Response* responseAllocWithFormat(int code, const char* status, const char* contentType, const char* format, ...) __printflike(3, 0);
/* If you leave the MIMETypeOrNULL NULL, the MIME type will be auto-detected */
struct Response* responseAllocWithFile(const char* filename, const char* MIMETypeOrNULL);
/* Error messages for when the request can't be handled properly */
struct Response* responseAlloc400BadRequestHTML(const char* errorMessage);
struct Response* responseAlloc404NotFoundHTML(const char* resourcePathOrNull);
struct Response* responseAlloc500InternalErrorHTML(const char* extraInformationOrNull);

/* Wrappers around strdupDecodeGetorPOSTParam */
char* strdupDecodeGETParam(const char* paramNameIncludingEquals, const struct Request* request, const char* valueIfNotFound);
char* strdupDecodePOSTParam(const char* paramNameIncludingEquals, const struct Request* request, const char* valueIfNotFound);
/* You can pass this the request->path for GET or request->body.contents for POST. Accepts NULL for paramString for convenience */
char* strdupDecodeGETorPOSTParam(const char* paramNameIncludingEquals, const char* paramString, const char* valueIfNotFound);
/* If you want to echo back HTML into the value="" attribute or display some user output this will help you (like &gt; &lt;) */
char* strdupEscapeForHTML(const char* stringToEscape);
/* If you have a file you reading/writing across connections you can use this provided pthread mutex so you don't have to make your own */
/* Need to inspect a header in a request? */
const struct Header* headerInRequest(const char* headerName, const struct Request* request);
/* Get a debug string representing this connection that's easy to print out. wrap it in HTML <pre> tags */
struct HeapString connectionDebugStringCreate(const struct Connection* connection);
/* Some really basic dynamic string handling. AppendChar and AppendFormat allocate enough memory and
 these strings are null-terminated so you can pass them into regular string functions. */
void heapStringInit(struct HeapString* string);
void heapStringFreeContents(struct HeapString* string);
void heapStringSetToCString(struct HeapString* heapString, const char* cString);
void heapStringAppendChar(struct HeapString* string, char c);
void heapStringAppendFormat(struct HeapString* string, const char* format, ...) __printflike(2, 0);
void heapStringAppendString(struct HeapString* string, const char* stringToAppend);
void heapStringAppendFormatV(struct HeapString* string, const char* format, va_list ap);
void heapStringAppendHeapString(struct HeapString* target, const struct HeapString* source);
/* functions that help when serving files */
const char* MIMETypeFromFile(const char* filename, const uint8_t* contents, size_t contentsLength);

/* runs quick unit tests in the demo app */
void EWSUnitTestsRun(void);

/* Internal implementation stuff */

/* these counters exist solely for the purpose of the /status demo */
static std::mutex counters_lock;
static struct Counters {
    int64_t bytesReceived = 0;
    int64_t bytesSent = 0;
    int64_t totalConnections = 0;
    int64_t activeConnections = 0;
    int64_t heapStringAllocations = 0;
    int64_t heapStringReallocations = 0;
    int64_t heapStringFrees = 0;
    int64_t heapStringTotalBytesReallocated = 0;
} counters;

#ifndef MIN
#define MIN(a, b) ((a < b) ? a : b)
#endif

struct PathInformation {
    bool exists;
    bool isDirectory;
};

static void responseFree(struct Response* response);
static void printIPv4Addresses(uint16_t portInHostOrder);
static void requestParse(struct Request* request, const char* requestFragment, size_t requestFragmentLength);
static size_t heapStringNextAllocationSize(size_t required);
static void poolStringStartNewString(struct PoolString* poolString, struct Request* request);
static void poolStringAppendChar(struct Request* request, struct PoolString* string, char c);
static bool strEndsWith(const char* big, const char* endsWith);
static void callWSAStartupIfNecessary(void);
static int pathInformationGet(const char* path, struct PathInformation* info);
static int sendResponseBody(struct Connection* connection, const struct Response* response, ssize_t* bytesSent);
static int sendResponseFile(struct Connection* connection, const struct Response* response, ssize_t* bytesSent);
static int snprintfResponseHeader(char* destination, size_t destinationCapacity, int code, const char* status, const char* contentType, const char* extraHeaders, size_t contentLength);

#ifdef WIN32 /* Windows implementations of functions available on Linux/Mac OS X */

/* windows function aliases */
#define strdup(string) _strdup(string)
#define unlink(file) _unlink(file)
#define close(x) closesocket(x)
#define gai_strerror_ansi(x) gai_strerrorA(x)

#else // WIN32

/// linux/mac, just used in error reporting XXX delete

#define gai_strerror_ansi(x) gai_strerror(x)

#endif // Linux/Mac OS X

static void connectionHandlerThread(void* connectionPointer);
static struct Response* createResponseForRequestAutoreleased(const struct Request* request, struct Connection* connection);

typedef enum {
    URLDecodeTypeWholeURL,
    URLDecodeTypeParameter
} URLDecodeType;

static void URLDecode(const char* encoded, char* decoded, size_t decodedCapacity, URLDecodeType type);

const struct Header* headerInRequest(const char* headerName, const struct Request* request) {
    for (size_t i = 0; i < request->headersCount; i++) {
        assert(NULL != request->headers[i].name.contents);
        if (0 == strcasecmp(request->headers[i].name.contents, headerName)) {
            return &request->headers[i];
        }
    }
    return NULL;
}

static char* strdupIfNotNull(const char* strToDup) {
    if (NULL == strToDup) {
        return NULL;
    }
    return strdup(strToDup);
}

typedef enum {
    URLDecodeStateNormal,
    URLDecodeStatePercentFirstDigit,
    URLDecodeStatePercentSecondDigit
} URLDecodeState;

char* strdupDecodeGETorPOSTParam(const char* paramNameIncludingEquals, const char* paramString, const char* valueIfNotFound) {
    assert(strstr(paramNameIncludingEquals, "=") != NULL && "You have to pass an equals sign after the param name, like 'name='");
    /* The string passed is actually NULL -- this is accepted because it's more convenient */
    if (NULL == paramString) {
        return strdupIfNotNull(valueIfNotFound);
    }
    /* Find the paramString ("name=") */
    const char* paramStart = strstr(paramString, paramNameIncludingEquals);
    if (NULL == paramStart) {
        return strdupIfNotNull(valueIfNotFound);
    }
    /* Ok paramStart points at -->"name=" ; let's make it point at "=" */
    paramStart = strstr(paramStart, "=");
    if (NULL == paramStart) {
        ews_printf("It's very suspicious that we couldn't find an equals sign after searching for '%s' in '%s'\n", paramStart, paramString);
        return strdupIfNotNull(valueIfNotFound);
    }
    /* We need to skip past the "=" */
    paramStart++;
    /* Oh man! End of string is right here */
    if ('\0' == *paramStart) {
        char* empty = (char*) malloc(1);
        empty[0] = '\0';
        return empty;
    }
    size_t maximumPossibleLength = strlen(paramStart);
    char* decoded = (char*) malloc(maximumPossibleLength + 1);
    URLDecode(paramStart, decoded, maximumPossibleLength + 1, URLDecodeTypeParameter);
    return decoded;
}

char* strdupDecodeGETParam(const char* paramNameIncludingEquals, const struct Request* request, const char* valueIfNotFound) {
    return strdupDecodeGETorPOSTParam(paramNameIncludingEquals, request->path, valueIfNotFound);
}

char* strdupDecodePOSTParam(const char* paramNameIncludingEquals, const struct Request* request, const char* valueIfNotFound) {
  return strdupDecodeGETorPOSTParam(paramNameIncludingEquals, request->body.c_str(),
				    valueIfNotFound);
}

typedef enum {
    PathStateNormal,
    PathStateSep,
    PathStateDot,
} PathState;

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

char* strdupEscapeForHTML(const char* stringToEscape) {
    struct HeapString escapedString;
    heapStringInit(&escapedString);
    size_t stringToEscapeLength = strlen(stringToEscape);
    if (0 == stringToEscapeLength) {
        char* empty = (char*) malloc(1);
        *empty = '\0';
        return empty;
    }
    for (size_t i = 0; i < stringToEscapeLength; i++) {
        // this is an excerpt of some things translated by the PHP htmlentities function
        char c = stringToEscape[i];
        switch (c) {
            case '"':
                heapStringAppendFormat(&escapedString, "&quot;");
                break;
            case '&':
                heapStringAppendFormat(&escapedString, "&amp;");
                break;
            case '\'':
                heapStringAppendFormat(&escapedString, "&#039;");
                break;
            case '<':
                heapStringAppendFormat(&escapedString, "&lt;");
                break;
            case '>':
                heapStringAppendFormat(&escapedString, "&gt;");
                break;
            case ' ':
                heapStringAppendFormat(&escapedString, "&nbsp;");
                break;
            default:
                heapStringAppendChar(&escapedString, c);
                break;
        }
    }
    return escapedString.contents;
}

/* Is someone using ../ to try to read a directory outside of the documentRoot? */
static bool pathEscapesDocumentRoot(const char* path) {
    int subdirDepth = 0;
    PathState state = PathStateNormal;
    bool isFirstChar = true;
    while ('\0' != *path) {
        switch (state) {
            case PathStateNormal:
                if ('/' == *path || '\\' == *path) {
                    state = PathStateSep;
                } else if (isFirstChar && '.' == *path) {
                    state = PathStateDot;
                } else if (isFirstChar) {
                    subdirDepth++;
                }
                isFirstChar = false;
                break;
            case PathStateSep:
                if ('.' == *path) {
                    state = PathStateDot;
                } else if ('/' != *path && '\\' != *path) {
                    subdirDepth++;
                    state = PathStateNormal;
                }
                break;
            case PathStateDot:
                if ('/' == *path) {
                    state = PathStateSep;
                } else if ('.' == *path) {
                    subdirDepth--;
                    state = PathStateNormal;
                } else {
                    state = PathStateNormal;
                }
                break;
        }
        path++;
    }
    if (subdirDepth < 0) {
        return true;
    }
    return false;
}

static void URLDecode(const char* encoded, char* decoded, size_t decodedCapacity, URLDecodeType type) {
    /* We found a value. Unescape the URL. This is probably filled with bugs */
    size_t deci = 0;
    /* these three vars unescape % escaped things */
    URLDecodeState state = URLDecodeStateNormal;
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
        /* If we are decoding only a parameter then stop at & */
        if (*encoded == '&' && URLDecodeTypeParameter == type) {
            break;
        }
        switch (state) {
            case URLDecodeStateNormal:
                if ('%' == *encoded) {
                    state = URLDecodeStatePercentFirstDigit;
                } else if ('+' == *encoded) {
                    decoded[deci] = ' ';
                    deci++;
                } else {
                    decoded[deci] = *encoded;
                    deci++;
                }
                break;
            case URLDecodeStatePercentFirstDigit:
                // copy the first digit, get the second digit
                firstDigit = *encoded;
                state = URLDecodeStatePercentSecondDigit;
                break;
            case URLDecodeStatePercentSecondDigit:
            {
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
                state = URLDecodeStateNormal;
            }
                break;
        }
        encoded++;
    }
    decoded[deci] = '\0';
}

static void heapStringReallocIfNeeded(struct HeapString* string, size_t minimumCapacity) {
    if (minimumCapacity <= string->capacity) {
        return;
    }
    /* to avoid many reallocations every time we call AppendChar, round up to the next power of two */
    string->capacity = heapStringNextAllocationSize(minimumCapacity);
    assert(string->capacity > 0 && "We are about to allocate a string with 0 capacity. We should have checked this condition above");
    bool previouslyAllocated = string->contents != NULL;
    /* Sometimes string->contents is NULL. realloc handles that case so no need for an extra if (NULL) malloc else realloc */
    string->contents = (char*) realloc(string->contents, string->capacity);
	/* zero out the newly allocated memory */
    memset(&string->contents[string->length], 0, string->capacity - string->length);
    if (OptionIncludeStatusPageAndCounters) {
      MutexLock ml(&counters_lock);
      if (previouslyAllocated) {
	counters.heapStringReallocations++;
      } else {
	counters.heapStringAllocations++;
      }
      counters.heapStringTotalBytesReallocated += string->capacity;
    }
}

static size_t heapStringNextAllocationSize(size_t required) {
    /* start with something reasonalbe that responses could fit into. The idea here
     is to avoid constant reallocation when dynamically building responses. */
    size_t powerOf2 = 256;
    while (powerOf2 < required) {
        powerOf2 *= 2;
    }
    return powerOf2;
}

void heapStringAppendChar(struct HeapString* string, char c) {
    heapStringReallocIfNeeded(string, string->length + 2);
    string->contents[string->length] = c;
    string->length++;
	/* this should already be null-terminated but we'll be extra safe for web scale ^_^ */
    string->contents[string->length] = '\0';
}

void heapStringAppendFormat(struct HeapString* string, const char* format, ...) {
    va_list ap;
    va_start(ap, format);
    heapStringAppendFormatV(string, format, ap);
    va_end(ap);
}

void heapStringSetToCString(struct HeapString* heapString, const char* cString) {
    size_t cStringLength = strlen(cString);
    heapStringReallocIfNeeded(heapString, cStringLength + 1);
    memcpy(heapString->contents, cString, cStringLength);
    heapString->length = cStringLength;
    heapString->contents[heapString->length] = '\0';
}

void heapStringAppendString(struct HeapString* string, const char* stringToAppend) {
    size_t stringToAppendLength = strlen(stringToAppend);
    /* just exit early if the string length is too small */
    if (0 == stringToAppendLength) {
        return;
    }
    /* realloc and append the string */
    size_t requiredCapacity = stringToAppendLength + string->length + 1;
    heapStringReallocIfNeeded(string, requiredCapacity);
    memcpy(&string->contents[string->length], stringToAppend, stringToAppendLength);
    string->length += stringToAppendLength;
    string->contents[string->length] = '\0';
}

void heapStringAppendHeapString(struct HeapString* target, const struct HeapString* source) {
    heapStringReallocIfNeeded(target, target->length + source->length + 1);
    memcpy(&target->contents[target->length], source->contents, source->length);
    target->length += source->length;
    target->contents[target->length] = '\0';
}

static bool heapStringIsSaneCString(const struct HeapString* heapString) {
    if (NULL == heapString->contents) {
        if (heapString->capacity != 0) {
            ews_printf("The heap string %p has a capacity of %" PRIu64 "but it's contents are NULL\n", heapString, (uint64_t) heapString->capacity);
            return false;
        }
        if (heapString->length != 0) {
            ews_printf("The heap string %p has a length of %" PRIu64 " but capacity is 0 and contents are NULL\n", heapString, (uint64_t) heapString->capacity);
            return false;
        }
        return true;
    }
    if (heapString->capacity <= heapString->length) {
        ews_printf("Heap string %p has probably overwritten invalid memory because the capacity (%" PRIu64 ") is <= length (%" PRIu64 "), which is a big nono. The capacity must always be 1 more than the length since the contents is null-terminated for convenience.\n",
            heapString, (uint64_t) heapString->capacity, (uint64_t)heapString->length);
        return false;
    }

    if (strlen(heapString->contents) != heapString->length) {
        ews_printf("The %p strlen(heap string contents) (%" PRIu64 ") is not equal to heapString length (%" PRIu64 "), which is not correct for a C string. This can be correct if we're sending something like a PNG image which can contain '\\0' characters",
               heapString,
               (uint64_t) strlen(heapString->contents),
               (uint64_t) heapString->length);
        return false;
    }
    return true;
}

void heapStringAppendFormatV(struct HeapString* string, const char* format, va_list ap) {
    /* Figure out how many characters it would take to print the string */
    va_list apCopy;
    va_copy(apCopy, ap);
    size_t appendLength = vsnprintf(NULL, 0, format, ap);
    size_t requiredCapacity = string->length + appendLength + 1;
    heapStringReallocIfNeeded(string, requiredCapacity);
    assert(string->capacity >= string->length + appendLength + 1);
    /* perform the actual vsnprintf that does the work */
    size_t actualAppendLength = vsnprintf(&string->contents[string->length], string->capacity - string->length, format, apCopy);
    string->length += appendLength;
    assert(actualAppendLength == appendLength && "We called vsnprintf twice with the same format and value arguments and got different string lengths");
    /* explicitly null terminate in case I messed up the vsnprinf logic */
    string->contents[string->length] = '\0';
}

void heapStringInit(struct HeapString* string) {
    string->capacity = 0;
    string->contents = NULL;
    string->length = 0;
}

void heapStringFreeContents(struct HeapString* string) {
    if (NULL != string->contents) {
        assert(string->capacity > 0 && "A heap string had a capacity > 0 with non-NULL contents which implies a malloc(0)");
        free(string->contents);
        string->contents = NULL;
        string->capacity = 0;
        string->length = 0;
        if (OptionIncludeStatusPageAndCounters) {
	  MutexLock ml(&counters_lock);
	  counters.heapStringFrees++;
        }
    } else {
        assert(string->capacity == 0 && "Why did a string with a NULL contents have a capacity > 0? This is not correct and may indicate corruption");
    }
}

struct HeapString connectionDebugStringCreate(const struct Connection* connection) {
    struct HeapString debugString;
    heapStringInit(&debugString);
    heapStringAppendFormat(&debugString, "%s %s from %s:%s\n", connection->request.method, connection->request.path, connection->remoteHost, connection->remotePort);
    heapStringAppendFormat(&debugString, "Request URL Path decoded to '%s'\n", connection->request.pathDecoded);
    heapStringAppendFormat(&debugString, "Bytes sent:%" PRId64 "\n", connection->status.bytesSent);
    heapStringAppendFormat(&debugString, "Bytes received:%" PRId64 "\n", connection->status.bytesReceived);
    heapStringAppendFormat(&debugString, "Final request parse state:%d\n", connection->request.state);
    heapStringAppendFormat(&debugString, "Header pool used:%" PRIu64 "\n", (uint64_t) connection->request.headersStringPoolOffset);
    heapStringAppendFormat(&debugString, "Header count:%" PRIu64 "\n", (uint64_t) connection->request.headersCount);
    bool firstHeader = true;
    heapStringAppendString(&debugString, "\n*** Request Headers ***\n");
    for (size_t i = 0; i < connection->request.headersCount; i++) {
        if (firstHeader) {
            firstHeader = false;
        }
        const struct Header* header = &connection->request.headers[i];
        heapStringAppendFormat(&debugString, "'%s' = '%s'\n", header->name.contents, header->value.contents);
    }
    if (!connection->request.body.empty()) {
      heapStringAppendFormat(&debugString, "\n*** Request Body ***\n%s\n",
			     connection->request.body.c_str());
    }
    heapStringAppendFormat(&debugString, "\n*** Request Warnings ***\n");
    bool hadWarnings = false;
    if (connection->request.warnings.headersStringPoolExhausted) {
        heapStringAppendString(&debugString, "headersStringPoolExhausted - try upping REQUEST_HEADERS_MAX_MEMORY\n");
        hadWarnings = true;
    }
    if (connection->request.warnings.tooManyHeaders) {
        heapStringAppendString(&debugString, "tooManyHeaders - try upping REQUEST_MAX_HEADERS\n");
        hadWarnings = true;
    }
    if (connection->request.warnings.methodTruncated) {
        heapStringAppendString(&debugString, "methodTruncated - you can increase the size of method[]\n");
        hadWarnings = true;
    }
    if (connection->request.warnings.pathTruncated) {
        heapStringAppendString(&debugString, "pathTruncated - you can increase the size of path[]\n");
        hadWarnings = true;
    }
    if (connection->request.warnings.versionTruncated) {
        heapStringAppendString(&debugString, "versionTruncated - you can increase the size of version[]\n");
        hadWarnings = true;
    }
    if (connection->request.warnings.bodyTruncated) {
        heapStringAppendString(&debugString, "bodyTruncated - you can increase REQUEST_MAX_BODY_LENGTH");
        hadWarnings = true;
    }
    if (!hadWarnings) {
        heapStringAppendString(&debugString, "No warnings\n");
    }
    return debugString;
}

static void poolStringStartNewString(struct PoolString* poolString, struct Request* request) {
    /* always re-initialize the length...just in case */
    poolString->length = 0;

    /* this is the first string in the pool */
    if (0 == request->headersStringPoolOffset) {
        poolString->contents = request->headersStringPool;
        return;
    }
    /* the pool string is full - don't initialize anything writable and ensure any writing to this pool string crashes */
    if (request->headersStringPoolOffset >= REQUEST_HEADERS_MAX_MEMORY - 1 - sizeof('\0')) { // we need to store one character (-1) and a null character at the end of the last string sizeof('\0')
        poolString->length = 0;
        poolString->contents = NULL;
        request->warnings.headersStringPoolExhausted = true;
        return;
    }
    /* there's already another string in the pool - we need to skip the null character (the string pool is initialized to 0s by calloc) */
    request->headersStringPoolOffset++;
    poolString->contents = &request->headersStringPool[request->headersStringPoolOffset];
    poolString->length = 0;
}

static void poolStringAppendChar(struct Request* request, struct PoolString* string, char c) {
    if (request->headersStringPoolOffset >= REQUEST_HEADERS_MAX_MEMORY - 1 - sizeof('\0')) { // we need to store one character (-1) and a null character at the end of the last string sizeof('\0')
        request->warnings.headersStringPoolExhausted = true;
        return;
    }
    string->contents[string->length] = c;
    string->length++;
    request->headersStringPoolOffset++;
}

// allocates a response with content = malloc(contentLength + 1) so you can write null-terminated strings to it
struct Response* responseAlloc(int code, const char* status, const char* contentType,
			       size_t bodyCapacity) {
  struct Response* response = (struct Response*) calloc(1, sizeof(*response));
  response->code = code;
  heapStringInit(&response->body);
  response->body.capacity = bodyCapacity;
  response->body.length = 0;
  if (response->body.capacity > 0) {
    response->body.contents = (char*) calloc(1, response->body.capacity);
    if (OptionIncludeStatusPageAndCounters) {
      MutexLock ml(&counters_lock);
      counters.heapStringAllocations++;
    }
  }
  response->contentType = strdupIfNotNull(contentType);
  response->status = strdupIfNotNull(status);
  return response;
}

struct Response* responseAllocHTML(const char* html) {
    return responseAllocHTMLWithStatus(200, "OK", html);
}

struct Response* responseAllocHTMLWithFormat(const char* format, ...) {
    struct Response* response = responseAlloc(200, "OK", "text/html; charset=UTF-8", 0);
    va_list ap;
    va_start(ap, format);
    heapStringAppendFormatV(&response->body, format, ap);
    va_end(ap);
    return response;
}

struct Response* responseAllocHTMLWithStatus(int code, const char* status, const char* html) {
    struct Response* response = responseAlloc(code, status, "text/html; charset=UTF-8", 0);
    heapStringSetToCString(&response->body, html);
    return response;
}

struct Response* responseAllocJSON(const char* json) {
    return responseAllocJSONWithStatus(200, "OK", json);
}

struct Response* responseAllocJSONWithFormat(const char* format, ...) {
    struct Response* response = responseAlloc(200, "OK", "application/json", 0);
    va_list ap;
    va_start(ap, format);
    heapStringAppendFormatV(&response->body, format, ap);
    va_end(ap);
    return response;
}

struct Response* responseAllocJSONWithStatus(int code, const char* status, const char* json) {
    struct Response* response = responseAlloc(code, status, "application/json", 0);
    heapStringSetToCString(&response->body, json);
    return response;
}

struct Response* responseAllocWithFormat(int code, const char* status, const char* contentType, const char* format, ...) {
    struct Response* response = responseAlloc(code, status, contentType, 0);
    va_list ap;
    va_start(ap, format);
    heapStringAppendFormatV(&response->body, format, ap);
    va_end(ap);
    return response;
}

static bool requestMatchesPathPrefix(const char* requestPathDecoded, const char* pathPrefix, size_t* matchLength) {
    /* special case: pathPrefix is "" - matching everything*/
    if (pathPrefix[0] == '\0') {
        return true;
    }
    /* special case: requestPathDecoded is "" - match nothing */
    if (requestPathDecoded[0] == '\0') {
        return false;
    }
    /* cases to think about:
    pathPrefix  = "/releases/current" OR  "/releases/current/"
    requestPath = "/releases/current/" OR "/releases/current" */
    size_t requestPathDecodedLength = strlen(requestPathDecoded);
    size_t pathPrefixLength = strlen(pathPrefix);
    /* we checked for zero-length strings above so it's afe to do this*/
    bool pathPrefixEndsWithSlash = pathPrefix[pathPrefixLength - 1] == '/';
    bool requestPathDecodedEndsWithSlash = requestPathDecoded[requestPathDecodedLength - 1] == '/';
    if (requestPathDecoded == strstr(requestPathDecoded, pathPrefix)) {
        /* if this code path finds a match, it will always be the full path prefix that's matched */
        if (NULL != matchLength) {
            *matchLength = pathPrefixLength;
        }
        /* ok we just matched pathPrefix = "/releases/current" but what if requestPathDecoded is "/releases/currentXXX"? */
        if (!pathPrefixEndsWithSlash) {
            /* pathPrefix is equal to requestPathDecoded */
            if (pathPrefixLength == requestPathDecodedLength) {
                assert(0 == strcmp(pathPrefix, requestPathDecoded) && "I'm assuming that the strings match with the length check + strstr above and I hope that's true. If not, that check needs to be replaced with real (0 == strcmp)");
                return true;
            }
            assert(requestPathDecodedLength > pathPrefixLength && "In the following code I am assuming that the requestPathDecoded is indeed longer than the path prefix because of the length check I did above (even though that was ==, the strstr has to match it)");
            /* pathPrefix = "/releases/current" and requestPathDecoded = "/releases/currentX" - we needs X to be a /
            We want pathPrefix to match "/releases/current/" but not "/releases/current1"	*/
            if (requestPathDecoded[pathPrefixLength] == '/') {
                return true;
            }
            /* this is the case where pathPrefix = "/releases/current" and requestPathDecoded = "/releases/current1" */
            return false;
        }
        return true;
    }
    /* It can still be the case that pathPrefix is "/releases/current/" and requestPathDecoded is "/releases/current".
    It's tempting to just put an assert and make the user fix it but we'll use some extra code to handle it. */
    if (requestPathDecodedLength == pathPrefixLength - 1) {
        /* make sure we're specifically matchig "/releases/current/" with "/releases/current" and not something like "/releases/currentX" */
        if (pathPrefixEndsWithSlash && !requestPathDecodedEndsWithSlash) {
            if (0 == strncmp(requestPathDecoded, pathPrefix, requestPathDecodedLength)) {
                if (NULL != matchLength) {
                    *matchLength = requestPathDecodedLength;
                }
                return true;
            }
        }
    }
    return false;
}
/*
Here's how the path logic works:

Let's say I want to serve traffic on the 'releases/current' path out of the 'EWS-1.1' directory. EWS-1.1 has no index.html
pathPrefix = "/releases/current" OR "/releases/current/"
requestPath = "/releases/current" OR "/releases/current/" OR "/releases/current/page.html"

Performed in requestMatchesPathPrefix, which will give us the match length
1. match pathPrefix with requestPath and figure out requestPathSuffix
See requestMatchesPathPrefix for more information
requestPathSuffix = requestPath - pathPrefix or rather:
requestPathSuffix = (sometimes!) requestPath + strlen(pathPrefix)
requestPathSuffix = (other times!) requestPath + strlen(pathPrefix) - 1

requestPathSuffix will now sometimes have a leading "/" or not, depending on how requestMatchesPathPrefix went

Some motivating cases:
If requestPath = "/releases/current" then requestPathSuffix = ""
If requestPath = "/releases/current/" then requestPathSuffix = "/"
if requestPath = "/releases/current/page.html" then requestPathSuffix = "/page.html"
Note:
If requestPath = "/releases/currentX" (where X is not a slash character) then we need to reject this request

2. It turns out that requestPathSuffix can sometimes start with a "/" or be empty or be something arbitrary
If requestPathSuffix starts with a "/" or "\" go ahead and skip the initial / and \'s. So if requestPath
is "/releases/current/page.html" then requestPathSuffix should be "page.html"
If requestPath

* Now requestPathSuffix is some arbitrary (and untrustable!) string (without the "/")

3. We can check if requestPathSuffix will leave the document root with pathEscapesDocumentRoot(requestPathSuffix)

4. We can form a file path with the documentRoot (which has no trailing / or \ !)
documentRoot = "EWS-1.1"
filePath = documentRoot + "/" + requestPathSuffix
filePath = "EWS-1.1" + "/" + "page.html"
filePath = "EWS-1.1" + "/" + ""
filePath = "EWS-1.1" + "/" + "docs/blahblah/.."

5. There is one tricky thing with directories. If filePath is a directory we will serve the
directory contents with links to the files. We need to figure out how to link to the files
If the browser sent a URL that ends in a / we can use totally relative links like:
<a href="code.cpp">code.cpp</a> (for something like http://x.com/release/current/)
But if the URL does not end in a / we need to do:
<a href="/release/current/code.cpp">code.cpp</a>
I'll call out this step below
*/
struct Response* responseAllocServeFileFromRequestPath(const char* pathPrefix,
						       const char* requestPath,
						       const char* requestPathDecoded,
						       const char* documentRoot) {
  if (NULL == pathPrefix) {
    ews_printf_debug("responseAllocServeFileFromRequestPath(): The user passed in NULL for pathPrefix so we just defaulted to / for them. Whatever.\n");
    pathPrefix = "/";
  }
  assert(NULL != requestPath && "The requestPath should not be NULL. It can be empty, but not NULL. Pass request->path");
  assert(NULL != requestPathDecoded && "The requestPathDecoded should not be NULL. It can be empty, but not NULL. Pass request->requestPathDecoded");
  // Step 1 (see above)
  size_t matchLength = 0;
  /* Do we even match this path? Also figure out the suffix (note the use of the _decoded_ path -- otherwise we would have %12s and stuff everywhere */
  if (!requestMatchesPathPrefix(requestPathDecoded, pathPrefix, &matchLength)) {
    return responseAlloc400BadRequestHTML("You requested the server to serve a path it doesn't know. Use the <code>requestMatchesPathPrefix</code> before passing this path. Or use a <code>pathPrefix</code> of <code>/</code> to have the server serve files from all URLs.");
  }
  const char* requestPathSuffix = requestPathDecoded + matchLength;
  // Step 2 (see above)
  while ('/' == *requestPathSuffix || '\\' == *requestPathSuffix) {
    requestPathSuffix++;
  }
  // Step 3 (see above)
  if (pathEscapesDocumentRoot(requestPathSuffix)) {
    return responseAllocHTMLWithStatus(
	403, "Forbidden",
	"<html><head><title>Forbidden</title></head>"
	"<body>You are not allowed to access this URL</body></html>");
  }
  // Step 4 (see above)
  struct HeapString filePath;
  heapStringInit(&filePath);
  heapStringSetToCString(&filePath, documentRoot);
  bool pathSuffixIsNotEmpty = '\0' != *requestPathSuffix;
  if (pathSuffixIsNotEmpty) {
    heapStringAppendChar(&filePath, '/');
    heapStringAppendString(&filePath, requestPathSuffix);
  }
  struct PathInformation pathInfo;

  return responseAllocHTMLWithStatus(200, "OK", "<!doctype html>You got it!\n");
}

struct Response* responseAlloc400BadRequestHTML(const char* errorMessage) {
  if (NULL == errorMessage) {
    errorMessage = "An unspecified error occurred";
  }
  return responseAllocWithFormat(
      400, "Bad Request", "text/html; charset=UTF-8",
      "<html><head><title>400 - Bad Request</title></head>"
      "<body>The request made was invalid. %s", errorMessage);
}

struct Response* responseAlloc404NotFoundHTML(const char* resourcePathOrNull) {
  if (NULL == resourcePathOrNull) {
    return responseAllocHTMLWithStatus(
	404, "Not Found",
	"<html><head><title>404 Not Found</title></head><body>"
	"The resource you specified could not be found</body></html>");
  } else {
    return responseAllocWithFormat(
	404, "Not Found", "text/html; charset=UTF-8",
	"<html><head><title>404 Not Found</title></head><body>"
	"The resource you specified ('%s') could not be found</body></html>",
	resourcePathOrNull);
  }
}

struct Response* responseAlloc500InternalErrorHTML(const char* extraInformationOrNull) {
  if (NULL == extraInformationOrNull) {
    return responseAllocHTMLWithStatus(
	500, "Internal Error",
	"<html><head><title>500 Internal Error</title></head>"
	"<body>There was an internal error while completing your request</body></html>");
  } else {
    return responseAllocWithFormat(
	500, "Internal Error", "text/html; charset=UTF-8",
	"<html><head><title>500 Internal Error</title></head>"
	"<body>There was an internal error while completing your request. %s</body></html>",
	extraInformationOrNull);
  }
}

struct Response* responseAllocWithFile(const char* filename, const char* MIMETypeOrNULL) {
  struct Response* response = responseAlloc(200, "OK", MIMETypeOrNULL, 0);
  response->filenameToSend = strdup(filename);
  return response;
}

static void responseFree(struct Response* response) {
  if (NULL != response->status) {
    free(response->status);
  }
  if (NULL != response->filenameToSend) {
    free(response->filenameToSend);
  }
  if (NULL != response->contentType) {
    free(response->contentType);
  }
  if (NULL != response->extraHeaders) {
    free(response->extraHeaders);
  }
  heapStringFreeContents(&response->body);
  free(response);
}

/* Only grab another header if we have space for it. This was revealed to be open for attack by afl-fuzz! */
static RequestParseState stateHeaderNameIfSpaceLeft(struct Request* request) {
  if (request->headersCount < REQUEST_MAX_HEADERS) {
    if (!request->warnings.headersStringPoolExhausted) {
      return RequestParseState::HeaderName;
    }
  } else {
    request->warnings.tooManyHeaders = true;
  }
  return RequestParseState::EatHeaders;
}

/* parses a typical HTTP request looking for the first line: GET /path HTTP/1.0\r\n */
static void requestParse(struct Request* request, const char* requestFragment, size_t requestFragmentLength) {
  for (size_t i = 0; i < requestFragmentLength; i++) {
    char c = requestFragment[i];
    switch (request->state) {
    case RequestParseState::Method:
      if (c == ' ') {
	request->state = RequestParseState::Path;
      } else if (request->methodLength < sizeof(request->method) - 1) {
	request->method[request->methodLength] = c;
	request->methodLength++;
      } else {
	request->warnings.methodTruncated = true;
      }
      break;
    case RequestParseState::Path:
      if (c == ' ') {
	/* we are done parsing the path, decode it */
	URLDecode(request->path, request->pathDecoded, sizeof(request->pathDecoded), URLDecodeTypeWholeURL);
	request->state = RequestParseState::Version;
      } else if (request->pathLength < sizeof(request->path) - 1) {
	request->path[request->pathLength] = c;
	request->pathLength++;
      } else {
	request->warnings.pathTruncated = true;
      }
      break;
    case RequestParseState::Version:
      if (c == '\r') {
	request->state = RequestParseState::CR;
      } else if (request->versionLength < sizeof(request->version) - 1) {
	request->version[request->versionLength] = c;
	request->versionLength++;
      } else {
	request->warnings.versionTruncated = true;
      }
      break;
    case RequestParseState::HeaderName:
      assert(request->headersCount < REQUEST_MAX_HEADERS && "Parsing the request header name assumes we have space for more headers");
      if (c == ':') {
	request->state = RequestParseState::HeaderValue;
      } else if (c == '\r') {
	request->state = RequestParseState::CR;
      } else  {
	/* if this is the first character in this header name, then initialize the string pool */
	if (NULL == request->headers[request->headersCount].name.contents) {
	  poolStringStartNewString(&request->headers[request->headersCount].name, request);
	}
	/* store the header name in the string pool */
	poolStringAppendChar(request, &request->headers[request->headersCount].name, c);
      }
      break;
    case RequestParseState::HeaderValue:
      assert(request->headersCount < REQUEST_MAX_HEADERS && "Parsing the request header value assumes we have space for more headers");
      /* skip the first space if we are saving this to header */
      if (c == ' ' && request->headers[request->headersCount].value.length == 0) {
	/* intentionally skipped */
      } else if (c == '\r') {
	/* only go to the next header if we were able to fill this one out - it is important to check both,
	   especially in the case of a header like ": safdasdf" */
	if (request->headers[request->headersCount].value.length > 0 && request->headers[request->headersCount].name.length > 0) {
	  request->headersCount++;
	}
	request->state = RequestParseState::CR;
      } else {
	assert(request->headersCount < REQUEST_MAX_HEADERS && "Parsing the request header value assumes we have space for more headers");
	/* if this is the first character in this header name, then initialize the string pool */
	if (NULL == request->headers[request->headersCount].value.contents) {
	  poolStringStartNewString(&request->headers[request->headersCount].value, request);
	}
	/* store the header name in the string pool */
	poolStringAppendChar(request, &request->headers[request->headersCount].value, c);
      }
      break;
    case RequestParseState::CR:
      if (c == '\n') {
	request->state = RequestParseState::CRLF;
      } else {
	request->state = stateHeaderNameIfSpaceLeft(request);
      }
      break;
    case RequestParseState::CRLF:
      if (c == '\r') {
	request->state = RequestParseState::CRLFCR;
      } else {
	request->state = stateHeaderNameIfSpaceLeft(request);
	if (RequestParseState::HeaderName == request->state) {
	  /* this is the first character of the header - replay the HeaderName case so this character gets appended */
	  i--;
	}
      }
      break;
    case RequestParseState::CRLFCR:
      if (c == '\n') {
	/* assume the request state is done unless we have some Content-Length, 
	   which would come from something like a JSON blob */
	request->state = RequestParseState::Done;
	const struct Header* contentLengthHeader = headerInRequest("Content-Length", request);
	if (NULL != contentLengthHeader) {
	  ews_printf_debug("Incoming request has a body of length %s\n",
			   contentLengthHeader->value.contents);
	  /* Note that this limits content length to < 2GB on Windows */
	  long contentLength = 0;
	  if (1 == sscanf(contentLengthHeader->value.contents, "%ld", &contentLength)) {
	    if (contentLength > REQUEST_MAX_BODY_LENGTH) {
	      request->warnings.bodyTruncated = true;
	      contentLength = REQUEST_MAX_BODY_LENGTH;
	    }
	    if (contentLength < 0) {
	      ews_printf_debug("Warning: Incoming request has negative "
			       "content length: %ld\n", contentLength);
	      contentLength = 0;
	    }
	    request->contentLength = contentLength;
	    request->body.reserve(contentLength);
	    request->state = RequestParseState::Body;
	  }
	}
      } else {
	request->state = stateHeaderNameIfSpaceLeft(request);
      }
      break;
    case RequestParseState::EatHeaders:
      /* we have no more room for headers right now */
      if (c == '\r') {
	request->state = RequestParseState::CR;
      }
      break;
    case RequestParseState::Body:
      // PERF can copy in bigger chunks...
      request->body.push_back(c);
      if (request->body.size() == request->contentLength) {
	request->state = RequestParseState::Done;
      }
      break;
    case RequestParseState::Done:
      break;
    }
  }
}

static void requestPrintWarnings(const struct Request* request, const char* remoteHost, const char* remotePort) {
  if (request->warnings.headersStringPoolExhausted) {
    ews_printf("Warning: Request from %s:%s exhausted the header string pool so some "
	       "information will be lost. You can try increasing REQUEST_HEADERS_MAX_MEMORY "
	       "which is currently %ld bytes\n", remoteHost, remotePort,
	       (long) REQUEST_HEADERS_MAX_MEMORY);
  }
  if (request->warnings.tooManyHeaders) {
    ews_printf("Warning: Request from %s:%s had too many headers and we dropped some. "
	       "You can try increasing REQUEST_MAX_HEADERS which is currently %ld\n",
	       remoteHost, remotePort, (long) REQUEST_MAX_HEADERS);
  }
  if (request->warnings.methodTruncated) {
    ews_printf("Warning: Request from %s:%s method was truncated to %s\n",
	       remoteHost, remotePort, request->method);
  }
  if (request->warnings.pathTruncated) {
    ews_printf("Warning: Request from %s:%s path was truncated to %s\n",
	       remoteHost, remotePort, request->path);
  }
  if (request->warnings.versionTruncated) {
    ews_printf("Warning: Request from %s:%s version was truncated to %s\n",
	       remoteHost, remotePort, request->version);
  }
  if (request->warnings.bodyTruncated) {
    ews_printf("Warning: Request from %s:%s body was truncated to %" PRIu64 " bytes\n",
	       remoteHost, remotePort, (uint64_t)request->body.size());
  }
}

static void SIGPIPEHandler(int signal) {
  (void) signal;
  /* SIGPIPE happens any time we try to send() and the connection is closed. 
     So we just ignore it and check the return code of send...*/
  ews_printf_debug("Ignoring SIGPIPE\n");
}


int Server::AcceptConnectionsUntilStoppedFromEverywhereIPv4(uint16_t portInHostOrder) {
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

int Server::AcceptConnectionsUntilStopped(const struct sockaddr* address,
					  socklen_t addressLength) {
  callWSAStartupIfNecessary();
  /* resolve the local address we are binding to so we can print it out later */
  char addressHost[256];
  char addressPort[20];
  int nameResult = getnameinfo(address, addressLength, addressHost, sizeof(addressHost), addressPort, sizeof(addressPort), NI_NUMERICHOST | NI_NUMERICSERV);
  if (0 != nameResult) {
    ews_printf("Warning: Could not get numeric host name and/or port for the address you passed to acceptConnectionsUntilStopped. getnameresult returned %d, which is %s. Not a huge deal but i really should have worked...\n", nameResult, gai_strerror_ansi(nameResult));
    strcpy(addressHost, "Unknown");
    strcpy(addressPort, "Unknown");
  }
  listenerfd = socket(address->sa_family, SOCK_STREAM, IPPROTO_TCP);
  if (listenerfd  <= 0) {
    ews_printf("Could not create listener socket: %s = %d\n", strerror(errno), errno);
    return 1;
  }
  /* SO_REUSEADDR tells the kernel to re-use the bind address in certain circumstances.
     I've always found when making debug/test servers that I want this option, especially on Mac OS X */
  int result;
  int reuse = 1;
  result = setsockopt(listenerfd, SOL_SOCKET, SO_REUSEADDR, (char*)&reuse, sizeof(reuse));
  if (0 != result) {
    ews_printf("Failed to setsockopt SO_REUSEADDR = true with %s = %d. Continuing because we might still succeed...\n", strerror(errno), errno);
  }

  if (address->sa_family == AF_INET6) {
    int ipv6only = 0;
    result = setsockopt(listenerfd, IPPROTO_IPV6, IPV6_V6ONLY, (char*)&ipv6only, sizeof(ipv6only));
    if (0 != result) {
      ews_printf("Failed to setsockopt IPV6_V6ONLY = true with %s = %d. This is not supported on BSD/macOS\n", strerror(errno), errno);
    }
  }

  result = bind(listenerfd, address, addressLength);
  if (0 != result) {
    ews_printf("Could not bind to %s:%s %s = %d\n", addressHost, addressPort, strerror(errno), errno);
    return 1;
  }
  /* listen for the maximum possible amount of connections */
  result = listen(listenerfd, SOMAXCONN);
  if (0 != result) {
    ews_printf("Could not listen for SOMAXCONN (%d) connections. %s = %d. Continuing because we might still succeed...\n", SOMAXCONN, strerror(errno), errno);
  }
  /* print out the addresses we're listening on. Special-case IPv4 0.0.0.0 bind-to-all-interfaces */
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
  /* allocate a connection (which sets connection->remoteAddrLength) and accept the next inbound connection */
  Connection* nextConnection = new Connection(this);
  while (shouldRun) {
    nextConnection->remoteAddrLength = sizeof(nextConnection->remoteAddr);
    nextConnection->socketfd = accept(listenerfd, (struct sockaddr*) &nextConnection->remoteAddr, &nextConnection->remoteAddrLength);
    if (-1 == nextConnection->socketfd) {
      if (errno == EINTR) {
	ews_printf("accept was interrupted, continuing if server.shouldRun is true...\n");
	continue;
      }
      if (errno == EBADF) {
	ews_printf("accept was stopped because the file descriptor is invalid (EBADF). This is probably because you closed it?\n");
	continue;
      }
      ews_printf("exiting because accept failed (probably interrupted) %s = %d\n", strerror(errno), errno);
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


static int sendResponse(struct Connection* connection, const struct Response* response,
			ssize_t* bytesSent) {
  if (response->body.length > 0) {
    return sendResponseBody(connection, response, bytesSent);
  }
  if (NULL != response->filenameToSend) {
    return sendResponseFile(connection, response, bytesSent);
  }
  ews_printf("Error: the request for '%s' failed because there was neither a response "
	     "body nor a filenameToSend\n", connection->request.path);
  assert(0 && "See above ews_printf");
  return 1;
}

static int sendResponseBody(struct Connection* connection, const struct Response* response,
			    ssize_t* bytesSent) {
  /* First send the response HTTP headers */
  int headerLength =
    snprintfResponseHeader(connection->responseHeader,
			   sizeof(connection->responseHeader),
			   response->code,
			   response->status,
			   response->contentType,
			   response->extraHeaders,
			   response->body.length);
  ssize_t sendResult =
    send(connection->socketfd, connection->responseHeader, headerLength, 0);
  if (sendResult != headerLength) {
    ews_printf("Failed to respond to %s:%s because we could not send the HTTP "
	       "response *header*. send returned %ld with %s = %d\n",
               connection->remoteHost,
               connection->remotePort,
               (long) sendResult,
               strerror(errno),
               errno);
    return -1;
  }
  if (OptionPrintResponse) {
    fwrite(connection->responseHeader, 1, headerLength, stdout);
  }
  *bytesSent = *bytesSent + sendResult;
  /* Second, if a response body exists, send that */
  if (response->body.length > 0) {
    sendResult = send(connection->socketfd, response->body.contents, response->body.length, 0);
    if (sendResult != (ssize_t) response->body.length) {
      ews_printf("Failed to respond to %s:%s because we could not send the HTTP "
		 "response *body*. send returned %" PRId64 " with %s = %d\n",
		 connection->remoteHost,
		 connection->remotePort,
		 (int64_t) sendResult,
		 strerror(errno),
		 errno);
      return -1;
    }
    if (OptionPrintResponse) {
      fwrite(response->body.contents, 1, response->body.length, stdout);
    }
    *bytesSent = *bytesSent + sendResult;
  }
  return 0;
}

static int sendResponseFile(struct Connection* connection,
			    const struct Response* response,
			    ssize_t* bytesSent) {
  /* If you were writing a high-performance web server you could use
     sendfile, memory map the file, or any number of exciting things. But
     here we just fread the first 100 bytes to figure out MIME type, then rewind
     and send the file ~16KB at a time. */
  struct Response* errorResponse = NULL;
  FILE* fp = fopen(response->filenameToSend, "rb");
  int result = 0;
  long fileLength;
  ssize_t sendResult;
  int headerLength;
  size_t actualMIMEReadSize;
  const char* contentType = NULL;
  const size_t MIMEReadSize = 100;
  if (NULL == fp) {
    ews_printf("Unable to satisfy request for '%s' because we could not open the file '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
    errorResponse = responseAlloc404NotFoundHTML(connection->request.path);
    goto exit;
  }
  /* If the MIME type if specified in the response->contentType, use that. Otherwise try to guess with MIMETypeFromFile */
  if (NULL != response->contentType) {
    contentType = response->contentType;
  } else {
    assert(sizeof(connection->sendRecvBuffer) >= MIMEReadSize);
    actualMIMEReadSize = fread(connection->sendRecvBuffer, 1, MIMEReadSize, fp);
    if (0 == actualMIMEReadSize) {
      ews_printf("Unable to satisfy request for '%s' because we could read the first bunch of bytes to determine MIME type '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
      errorResponse = responseAlloc500InternalErrorHTML("fread for MIME type detection failed");
      goto exit;
    }
    contentType = MIMETypeFromFile(response->filenameToSend, (const uint8_t*)connection->sendRecvBuffer, actualMIMEReadSize);
    ews_printf_debug("Detected MIME type '%s' for file '%s'\n", contentType, response->filenameToSend);
  }
  /* get the file length, laboriously checking for errors */
  result = fseek(fp, 0, SEEK_END);
  if (0 != result) {
    ews_printf("Unable to satisfy request for '%s' because we could not fseek to the end of the file '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
    errorResponse = responseAlloc500InternalErrorHTML("fseek to end of file failed");
    goto exit;
  }
  fileLength = ftell(fp);
  if (fileLength < 0) {
    ews_printf("Unable to satisfy request for '%s' because we could not ftell on the file '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
    errorResponse = responseAlloc500InternalErrorHTML("ftell to determine file length failed");
    goto exit;
  }
  result = fseek(fp, 0, SEEK_SET);
  if (0 != result) {
    ews_printf("Unable to satisfy request for '%s' because we could not fseek to the beginning of the file '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
    errorResponse = responseAlloc500InternalErrorHTML("fseek to beginning of file to start sending failed");
    goto exit;
  }

  /* now we have the file length + MIME TYpe and we can send the header */
  headerLength = snprintfResponseHeader(connection->responseHeader, sizeof(connection->responseHeader), response->code, response->status, contentType, response->extraHeaders, fileLength);
  sendResult = send(connection->socketfd, connection->responseHeader, headerLength, 0);
  if (sendResult != headerLength) {
    ews_printf("Unable to satisfy request for '%s' because we could not send the HTTP header '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
    result = 1;
    goto exit;
  }
  if (OptionPrintResponse) {
    fwrite(connection->responseHeader, 1, headerLength, stdout);
  }
  *bytesSent = sendResult;
  /* read the whole file, just buffering into the connection buffer, and sending it out to the socket */
  while (!feof(fp)) {
    size_t bytesRead = fread(connection->sendRecvBuffer, 1, sizeof(connection->sendRecvBuffer), fp);
    if (0 == bytesRead) { /* peacefull end of file */
      break;
    }
    if (ferror(fp)) {
      ews_printf("Unable to satisfy request for '%s' because there was an error freading. '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
      errorResponse = responseAlloc500InternalErrorHTML("Could not fread to send over socket");
      goto exit;
    }
    /* send the data out the socket to the network */
    sendResult = send(connection->socketfd, connection->sendRecvBuffer, bytesRead, 0);
    if (sendResult != (ssize_t) bytesRead) {
      ews_printf("Unable to satisfy request for '%s' because there was an error sending bytes. '%s' %s = %d\n", connection->request.path, response->filenameToSend, strerror(errno), errno);
      result = 1;
      goto exit;
    }
    if (OptionPrintResponse) {
      fwrite(connection->sendRecvBuffer, 1, bytesRead, stdout);
    }

    *bytesSent = *bytesSent + sendResult;
  }
 exit:
  if (NULL != fp) {
    fclose(fp);
  }
  if (NULL != errorResponse) {
    ews_printf("Instead of satisfying the request for '%s' we encountered an error and will return %d %s\n", connection->request.path, response->code, response->status);
    ssize_t errorBytesSent = 0;
    result = sendResponseBody(connection, errorResponse, &errorBytesSent);
    *bytesSent = *bytesSent + errorBytesSent;
    return result;
  }
  return result;
}

static struct Response* createResponseForRequestAutoreleased(const struct Request* request,
							     struct Connection* connection) {
  // XXX?? this was a werid OBJC wrapper and can probably go?
  return createResponseForRequest(request, connection);
}

static void connectionHandlerThread(void* connectionPointer) {
  struct Connection* connection = (struct Connection*) connectionPointer;
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
  while ((bytesRead = recv(connection->socketfd, connection->sendRecvBuffer,
			   SEND_RECV_BUFFER_SIZE, 0)) > 0) {
    if (OptionPrintWholeRequest) {
      fwrite(connection->sendRecvBuffer, 1, bytesRead, stdout);
    }
    connection->status.bytesReceived += bytesRead;
    requestParse(&connection->request, connection->sendRecvBuffer, bytesRead);
    if ((connection->request.state != RequestParseState::Method &&
	 connection->request.state != RequestParseState::Path) &&
	!madeRequestPrintf) {
      ews_printf_debug("Request from %s:%s: %s to %s HTTP version %s\n",
		       connection->remoteHost,
		       connection->remotePort,
		       connection->request.method,
		       connection->request.path,
		       connection->request.version);
      madeRequestPrintf = true;
    }
    if (connection->request.state == RequestParseState::Done) {
      foundRequest = true;
      break;
    }
  }
  requestPrintWarnings(&connection->request, connection->remoteHost, connection->remotePort);
  ssize_t bytesSent = 0;
  if (foundRequest) {
    struct Response* response = createResponseForRequestAutoreleased(&connection->request, connection);
    if (NULL != response) {
      int result = sendResponse(connection, response, &bytesSent);
      if (0 == result) {
	ews_printf_debug("%s:%s: Responded with HTTP %d %s length %" PRId64 "\n", connection->remoteHost, connection->remotePort, response->code, response->status, (int64_t)bytesSent);
      } else {
	/* sendResponse already printed something out, don't add another ews_printf */
      }
      responseFree(response);
      connection->status.bytesSent = bytesSent;
    } else {
      ews_printf("%s:%s: You have returned a NULL response - I'm assuming you took over the request handling yourself.\n", connection->remoteHost, connection->remotePort);
    }
  } else {
    ews_printf("No request found from %s:%s? Closing connection. Here's the last bytes we received in the request (length %" PRIi64 "). The total bytes received on this connection: %" PRIi64 " :\n", connection->remoteHost, connection->remotePort, (int64_t) bytesRead, connection->status.bytesReceived);
    if (bytesRead > 0) {
      fwrite(connection->sendRecvBuffer, 1, bytesRead, stdout);
    }
  }
  /* Alright - we're done */
  close(connection->socketfd);

  {
    MutexLock ml(&counters_lock);
    counters.bytesSent += (ssize_t) connection->status.bytesSent;
    counters.bytesReceived += (ssize_t) connection->status.bytesReceived;
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

/* Apache2 has a module called MIME magic or something which does a
   really good version of this. */
const char* MIMETypeFromFile(const char* filename, const uint8_t* contents,
			     size_t contentsLength) {
  // http://libpng.org/pub/png/spec/1.2/PNG-Structure.html
  static const uint8_t PNGMagic[] = {137, 80, 78, 71, 13, 10, 26, 10};
  // http://www.onicos.com/staff/iz/formats/gif.html
  static const uint8_t GIFMagic[] = {'G', 'I', 'F'};
  // ehh pretty shaky http://www.fastgraph.com/help/jpeg_header_format.html
  static const uint8_t JPEGMagic[] = {0xFF, 0xD8};

  // PNG?
  if (contentsLength >= 8) {
    if (0 == memcmp(PNGMagic, contents, sizeof(PNGMagic))) {
      return "image/png";
    }
  }
  // GIF?
  if (contentsLength >= 3) {
    if (0 == memcmp(GIFMagic, contents, sizeof(GIFMagic))) {
      return "image/gif";
    }
  }
  // JPEG?
  if (contentsLength >= 2) {
    if (0 == memcmp(JPEGMagic, contents, sizeof(JPEGMagic))) {
      return "image/jpeg";
    }
  }
  /* just start guessing based on file extension */
  if (strEndsWith(filename, "html") || strEndsWith(filename, "htm")) {
    return "text/html; charset=UTF-8"; // kind of naughty: assume UTF-8
  }
  if (strEndsWith(filename, "css")) {
    return "text/css";
  }
  if (strEndsWith(filename, "gz")) {
    return "application/x-gzip";
  }
  if (strEndsWith(filename, "js")) {
    return "application/javascript";
  }
  /* is it a plain text file? Just inspect the first 100 bytes or so for ASCII */
  bool plaintext = true;
  for (size_t i = 0; i < MIN(contentsLength, 100); i++) {
    if (contents[i] > 127) {
      plaintext = false;
      break;
    }
  }
  if (plaintext) {
    return "text/plain";
  }
  /* well that's pretty much all the different file types in existence */
  return "application/binary";
}

static bool strEndsWith(const char* big, const char* endsWith) {
  size_t bigLength = strlen(big);
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

static int snprintfResponseHeader(char* destination, size_t destinationCapacity,
				  int code, const char* status, const char* contentType,
				  const char* extraHeaders, size_t contentLength) {
  if (NULL == extraHeaders) {
    extraHeaders = "";
  }
  return snprintf(destination,
		  destinationCapacity,
		  "HTTP/1.1 %d %s\r\n"
		  "Content-Type: %s\r\n"
		  "Content-Length: %" PRIu64 "\r\n"
		  "Server: Embeddable Web Server/" EMBEDDABLE_WEB_SERVER_VERSION_STRING "\r\n"
		  "%s"
		  "\r\n",
		  code,
		  status,
		  contentType,
		  (uint64_t)contentLength,
		  extraHeaders);
}


Server::Server() {
  ignoreSIGPIPE();
}
  
void Server::Stop() {
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



/* Platform specific stubs/handlers */

#ifdef WIN32

static void printIPv4Addresses(uint16_t portInHostOrder){
    /* I forgot how to do this */
    ews_printf("(Printing bound interfaces is not supported on Windows. "
	       "Try http://127.0.0.1:%u if you bound to all addresses or the "
	       "localhost.)\n", portInHostOrder);
}


static void callWSAStartupIfNecessary() {
  // try to create a socket, and if that fails because of
  // uninitialized winsock, then initialize winsock
  SOCKET testsocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (SOCKET_ERROR == testsocket && WSANOTINITIALISED == WSAGetLastError()) {
    WSADATA data = { 0 };
    int result = WSAStartup(MAKEWORD(2, 2), &data);
    if (0 != result) {
      ews_printf("Calling WSAStartup failed! It returned %d with GetLastError() = %d\n",
		 result, GetLastError());
      abort();
    }
  } else {
    close(testsocket);
  }
}

#if UNDEFINE_CRT_SECURE_NO_WARNINGS
#undef _CRT_SECURE_NO_WARNINGS
#endif

#else /* Linux + Mac OS X*/

static void callWSAStartupIfNecessary() {

}

static void printIPv4Addresses(uint16_t portInHostOrder) {
  struct ifaddrs* addrs = NULL;
  getifaddrs(&addrs);
  struct ifaddrs* p = addrs;
  while (NULL != p) {
    if (NULL != p->ifa_addr && p->ifa_addr->sa_family == AF_INET) {
      char hostname[256];
      getnameinfo(p->ifa_addr, sizeof(struct sockaddr_in), hostname, sizeof(hostname), NULL, 0, NI_NUMERICHOST);
      ews_printf("Probably listening on http://%s:%u\n", hostname, portInHostOrder);
    }
    p = p->ifa_next;
  }
  if (NULL != addrs) {
    freeifaddrs(addrs);
  }
}

static int pathInformationGet(const char* path, struct PathInformation* info) {
  struct stat st;
  int result = stat(path, &st);
  if (0 != result) {
    /* There was an error. If the error is just "file not found" say
       the file doesn't exist */
    if (ENOENT == errno) {
      info->exists = false;
      info->isDirectory = false;
      return 0;
    }
    return 1;
  }
  /* We know the path exists. Is it a directory? */
  info->exists = true;
  if (S_ISDIR(st.st_mode)) {
    info->isDirectory = true;
  } else {
    info->isDirectory = false;
  }
  return 0;
}

#endif // WIN32 or Linux/Mac OS X

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
