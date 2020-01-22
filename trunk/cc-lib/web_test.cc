#include "web.cc"  /// XXX
#include <time.h>
#include <thread>
#include <chrono>


static void ServerThread() {
  Server server;
  const uint16_t portInHostOrder = 8080;
  server.AcceptConnectionsUntilStoppedFromEverywhereIPv4(portInHostOrder);
  return;
}

struct Response* createResponseForRequest(const struct Request* request, struct Connection* connection) {
  time_t t;
  time(&t);
  return responseAllocHTMLWithFormat("<html><h1>The time is seconds is %ld</h1></html>", t);
}

int main() {
  std::thread server_thread(ServerThread);
  while (1) {
    // using namespace std::chrono_literals;
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }
  server_thread.join();
  return 0;
}
