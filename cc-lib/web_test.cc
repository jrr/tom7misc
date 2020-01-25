#include "web.h"

#include <time.h>
#include <thread>
#include <chrono>

#include "base/stringprintf.h"

static void ServerThread() {
  // Note: Never stopped/deleted
  WebServer *server = WebServer::Create();
  server->ListenOn(8080);
  return;
}

WebServer::Response* createResponseForRequest(const WebServer::Request* request) {
  string ret = StringPrintf("<html><h1>The time is seconds is %lld</h1></html>",
			    (int64)time(nullptr));
  return responseAllocHTML(ret);
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
