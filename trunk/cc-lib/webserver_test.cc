#include "webserver.h"

#include <time.h>
#include <thread>
#include <chrono>

#include "util.h"
#include "base/stringprintf.h"

using namespace std;

static WebServer::Response SlashHandler(const WebServer::Request &request) {
  WebServer::Response response;
  response.code = 200;
  response.status = "OK";
  response.content_type = "text/html; charset=UTF-8";

  string table = "<table>\n";
  for (const auto &[k, v] : request.Params()) {
    StringAppendF(&table,
                  "<tr><td>%s</td><td>%s</td><tr>\n",
                  WebServer::HTMLEscape(k).c_str(),
                  WebServer::HTMLEscape(v).c_str());
  }
  table += "</table>\n";
  response.body =
    StringPrintf(
        "<html><h1>The time in seconds is %lld</h1>\n"
        "<p>Path: %s\n"
        "<p>Params:\n"
        "%s"
        "</html>",
        (int64)time(nullptr),
        request.path.c_str(),
        table.c_str());
  return response;
}

static WebServer::Response TestParamsHandler(
    const WebServer::Request &request) {
  WebServer::Response response;
  response.code = 200;
  response.status = "OK";
  response.content_type = "text/html; charset=UTF-8";

  auto sparam = request.StringURLParam("string");
  auto iparam = request.IntURLParam("int");

  StringAppendF(&response.body,
                "<p>Value of param 'string': '%s'\n",
                sparam.has_value() ?
                WebServer::HTMLEscape(sparam.value()).c_str() :
                "(absent)");

  if (iparam.has_value()) {
    StringAppendF(&response.body,
                  "<p>Value of param 'int': '%lld'\n",
                  iparam.value());
  } else {
    StringAppendF(&response.body,
                  "<p>The param 'int' is not present or isn't an integer.\n");
  }
  return response;
}

static void ServerThread() {
  // Note: Never stopped/deleted
  WebServer *server = WebServer::Create();
  WebServer::Counter *connections = server->GetCounter("(test connections)");
  server->AddHandler("/stats", server->GetStatsHandler());
  server->AddHandler("/favicon.ico",
                     [](const WebServer::Request &request) {
                       WebServer::Response response;
                       response.code = 200;
                       response.status = "OK";
                       response.content_type = "image/png";
                       response.body = Util::ReadFile("favicon.png");
                       return response;
                     });
  server->AddHandler("/inc",
                     [connections](const WebServer::Request &request) {
                       connections->Increment();
                       WebServer::Response response;
                       response.code = 200;
                       response.status = "OK";
                       response.content_type = "text/html; charset=UTF-8";
                       response.body =
                         StringPrintf("Counter: %lld\n",
                                      connections->Value());
                       return response;
                     });
  server->AddHandler("/params", TestParamsHandler);
  server->AddHandler("/", SlashHandler);
  server->ListenOn(8008);
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
