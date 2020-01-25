#include "web.h"

#include <time.h>
#include <thread>
#include <chrono>

#include "base/stringprintf.h"

static void ServerThread() {
  // Note: Never stopped/deleted
  WebServer *server = WebServer::Create();
  WebServer::Counter *connections = server->GetCounter("(test connections)");
  server->AddHandler("/stats", server->GetStatsHandler());
  server->AddHandler("/",
		     [connections](const WebServer::Request &request) {
		       connections->Increment();
		       WebServer::Response response;
		       response.code = 200;
		       response.status = "OK";
		       response.content_type = "text/html; charset=UTF-8";
		       response.body =
			 StringPrintf("<html><h1>The time is seconds is %lld</h1></html>",
				      (int64)time(nullptr));
		       return response;
		     });
  server->ListenOn(8080);
  return;
}

int main() {
  std::thread server_thread(ServerThread);
  while (1) {
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }
  server_thread.join();
  return 0;
}
