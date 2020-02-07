#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <cstdint>
#include <unordered_map>
#include <thread>
#include <mutex>

#include <mysql++.h>

#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/util.h"
#include "../cc-lib/web.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/image.h"

#include "onewire.h"
#include "database.h"


using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using uint64 = uint64_t;


struct Server {
  Server(Database *db) : db(db) {
    server = WebServer::Create();
    CHECK(server);
    favicon = Util::ReadFile("favicon.png");
    diagram_svg = Util::ReadFile("diagram.svg");
    
    server->AddHandler("/stats", server->GetStatsHandler());
    server->AddHandler("/favicon.ico",
		       [this](const WebServer::Request &request) {
			 WebServer::Response response;
			 response.code = 200;
			 response.status = "OK";
			 response.content_type = "image/png";
			 response.body = this->favicon;
			 return response;
		       });
    server->AddHandler("/diagram",
		       [this](const WebServer::Request &req) {
			 return Diagram(req);
		       });


    server->AddHandler("/graph.png",
		       [this](const WebServer::Request &req) {
			 return Graph(req);
		       });
    
    // Fallback handler.
    // TODO: Make a good default home-page here?
    server->AddHandler("/",
		       [this](const WebServer::Request &req) {
			 return Table(req);
		       });
    // Detach listening thread.
    listen_thread = std::thread([this](){
	this->server->ListenOn(8080);
      });
  }

  string GetRGB(float f) {
    float r, g, b;
    if (f < 32.0f) {
      r = g = b = 0.0;
    } else if (f > 200.0f) {
      r = 1.0f;
      g = b = 0.0f;
    } else {
      // XXX better color map!
      r = ((f - 32.0f) / (200.0f - 32.0f));
      g = b = 0.0f;
    }
    return StringPrintf("#%02x%02x%02x",
			(uint8)(r * 255.0f),
			(uint8)(g * 255.0f),
			(uint8)(b * 255.0f));
  }
  
  WebServer::Response Diagram(const WebServer::Request &request) {
    std::unordered_map<string, pair<int64, uint32>> temps = db->LastTemp();
    WebServer::Response r;
    r.code = 200;
    r.status = "OK";
    r.content_type = "text/html; charset=UTF-8";
    r.body = "<!doctype html>\n"
      "<style>\n";

    string substituted_svg = diagram_svg;
    
    for (const auto &p : temps) {
      float celsius = (float)p.second.second / 1000.0f;
      float fahrenheit = celsius * (9.0f / 5.0f) + 32.0f;
      string rgb = GetRGB(fahrenheit);
      StringAppendF(&r.body,
		    "  #%s path { fill: %s !important; }\n",
		    p.first.c_str(),
		    rgb.c_str());
      substituted_svg = Util::Replace(substituted_svg,
				      StringPrintf("[[%s]]", p.first.c_str()),
				      StringPrintf("%.1f&deg;", fahrenheit));
    }
		   
    r.body += "</style>\n";

    StringAppendF(&r.body, "Diagram:<p>\n");
    r.body += substituted_svg;

    return r;
  }

  WebServer::Response Graph(const WebServer::Request &request) {
    // TODO: Make these settable by url params!
    int64 time_end = time(nullptr);
    int64 time_start = time_end - 3600;

    // TODO: Dynamic size
    int width = 1920;
    int height = 1080;
    
    std::unordered_map<string, vector<pair<int64, uint32>>> temps =
      db->AllTempsIn(time_start, time_end);


    ImageRGBA graph(width, height);
    graph.Clear32(0xF7F7F7FF);

    // TODO: Grid, axis labels, etc.
    const int64 time_width = time_end - time_start;
    for (const auto &[name, values] : temps) {
      // TODO pick different colors!
      uint32 color = 0x003377AA;
      for (const auto &[timestamp, microdegsc] : values) {
	double fx = (timestamp - time_start) / (double)time_width;
	int x = fx * width;
	double fy = 1.0f - (microdegsc / 100000.0f);
	int y = fy * height;
	graph.BlendPixel32(x, y, color);
	graph.BlendPixel32(x + 1, y, color & 0xFFFFFF7F);
	graph.BlendPixel32(x - 1, y, color & 0xFFFFFF7F);
	graph.BlendPixel32(x, y + 1, color & 0xFFFFFF7F);
	graph.BlendPixel32(x, y - 1, color & 0xFFFFFF7F);
      }
    }

    // TODO also render a key? Or this can be in a container html
    // page perhaps.
    
    WebServer::Response r;
    r.code = 200;
    r.status = "OK";
    r.content_type = "image/png";
    r.body = graph.SaveToString();
    return r;
  }
  
  WebServer::Response Table(const WebServer::Request &request) {
    std::unordered_map<string, pair<int64, uint32>> temps = db->LastTemp();
    WebServer::Response r;
    r.code = 200;
    r.status = "OK";
    r.content_type = "text/html; charset=UTF-8";
    r.body =
      StringPrintf("<!doctype html>\n"
		   "<style>\n"
		   " body { font: 12px verdana,helvetica,sans-serif }\n"
		   "</style>\n");

    StringAppendF(&r.body, "<table>\n");
    for (const auto &p : temps) {
      float celsius = (float)p.second.second / 1000.0f;
      float fahrenheit = celsius * (9.0f / 5.0f) + 32.0f;
      StringAppendF(&r.body,
		    "<tr><td>%s</td><td>%lld</td><td>%.2f &deg;C</td>"
		    "<td>%.2f &deg;F</tr>\n",
		    p.first.c_str(),
		    p.second.first,
		    celsius,
		    fahrenheit);
    }
    StringAppendF(&r.body, "</table>\n");

    return r;
  }

  
  ~Server() {
    server->Stop();
    listen_thread.join();
  }

  WebServer *server = nullptr;
  Database *db = nullptr;
  string favicon;
  string diagram_svg;
  std::thread listen_thread;
};


int main(int argc, char **argv) {
  Database db;
  Server server(&db);

  OneWire onewire;
  WebServer::GetCounter("probes found")->
    IncrementBy((int64)onewire.probes.size());
  
  int64 start = time(nullptr);
  int64 readings = 0LL;
  for (;;) {
    for (auto &p : onewire.probes) {
      uint32 microdegs_c = 0;
      if (p.second.Temperature(&microdegs_c)) {
	string s = db.WriteTemp(p.first, microdegs_c);
	readings++;
	double elapsed = time(nullptr) - start;
	printf("%s (%s): %u  (%.2f/sec)\n",
	       p.first.c_str(), s.c_str(), microdegs_c,
	       readings / elapsed);
	// Perf could save these in probe struct?
	WebServer::GetCounter(s + " last")->SetTo(microdegs_c);
	WebServer::GetCounter(s + " #")->Increment();
      } else {
	printf("%s: ERROR\n", p.first.c_str());
      }
    }
  }

  printf("SERVER OK\n");

  return 0;
}
