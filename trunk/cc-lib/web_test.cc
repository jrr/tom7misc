#include "web.cc"  /// XXX
#include <time.h>

static struct Server server;
static void acceptConnectionsThread() {
    serverInit(&server);
    const uint16_t portInHostOrder = 8080;
    acceptConnectionsUntilStoppedFromEverywhereIPv4(&server, portInHostOrder);
    return;
}

struct Response* createResponseForRequest(const struct Request* request, struct Connection* connection) {
    time_t t;
    time(&t);
    return responseAllocHTMLWithFormat("<html><h1>The time is seconds is %ld</h1></html>", t);
}

int main() {
  std::thread server_thread(acceptConnectionsThread);
  // pthread_t threadHandle;
  // pthread_create(&threadHandle, NULL, &acceptConnectionsThread, NULL);
    // rest of the program
    while (1) {
    }
    return 0;
}
