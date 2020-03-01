#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <cstdint>
#include <unordered_map>
#include <thread>
#include <mutex>
#include <chrono>
#include <cmath>

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

// Bright colors for a black background.
static constexpr uint32 COLORS[] = {
  0xFFFFFFFF,
  0xFF0000FF,
  0xFF9000FF,
  0xFFE400FF,
  0xBFFF00FF,
  0x00FF00FF,
  0x7dFFA5FF,
  0x00FFD8FF,
  0x00AEFFFF,
  0x4258FFFF,
  0x9778FFFF,
  0x9F09FFFF,
  0xFF09D9FF,
  0xFF6AA9FF,
  // ...
};

static constexpr int NUM_COLORS = sizeof (COLORS) / sizeof (uint32);

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

    // And keepalive.
    keepalive_thread = std::thread([this](){
	this->KeepAlive();
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
    vector<pair<Database::Probe, pair<int64, uint32>>> temps = db->LastTemp();
    WebServer::Response r;
    r.code = 200;
    r.status = "OK";
    r.content_type = "text/html; charset=UTF-8";
    r.body = "<!doctype html>\n"
      "<style>\n";

    string substituted_svg = diagram_svg;
    
    for (const auto &[probe, cur] : temps) {
      float celsius = (float)cur.second / 1000.0f;
      float fahrenheit = celsius * (9.0f / 5.0f) + 32.0f;
      string rgb = GetRGB(fahrenheit);
      StringAppendF(&r.body,
		    "  #%s path { fill: %s !important; }\n",
		    probe.name.c_str(),
		    rgb.c_str());
      substituted_svg =
	Util::Replace(substituted_svg,
		      StringPrintf("[[%s]]", probe.name.c_str()),
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

    // in microdegs c
    double min_temp =   0000.0;
    double max_temp = 100000.0;
    const double temp_width = max_temp - min_temp;
    

    // XXX todo timing info
    auto db_start = std::chrono::steady_clock::now();
    std::vector<pair<Database::Probe, vector<pair<int64, uint32>>>> temps =
      db->AllTempsIn(time_start, time_end);
    auto db_end = std::chrono::steady_clock::now();

    auto blit_start = std::chrono::steady_clock::now();
    ImageRGBA graph(width, height);
    graph.Clear32(0x000000FF);

    // Grid line every 10 F.
    for (int degs = 40; degs <= 200; degs += 10) {
      double microdegsc = (degs - 32) * (5.0f / 9.0f) * 1000.0f;
      double fy = 1.0f - ((microdegsc - min_temp) / temp_width);
      const int y = fy * height;
      for (int x = 0; x < width; x++) {
	graph.SetPixel32(x, y, 0x272727FF);
      }
      string label = StringPrintf("%d F", degs);
      graph.BlendText32(3, y + 2, 0x777777FF, label);
    }
    

    // Draw key. Would be nice if these were somehow labeling the
    // lines themselves, but it's a tricky layout problem!
    vector<tuple<int, int, uint32>> label_end;
    {
      static constexpr int KEY_X = 100;
      int y = 50;
      for (const auto &[probe, _] : temps) {
	uint32 color = COLORS[probe.id % NUM_COLORS];
	// Slightly transparent
	// color = (color & 0xFFFFFF00) | 0x000000AA;
	string label =
	  StringPrintf("%s: %s",
		       probe.name.c_str(),
		       probe.desc.c_str());
	graph.BlendText32(KEY_X, y, color, label);
	label_end.emplace_back(KEY_X + 4 + (label.size() * 9), y + 4,
			       color);
	y += 12;
      }
    }

    // Corner must be at least this far.
    int max_x = 0;
    for (auto [x, y, c] : label_end) max_x = std::max(x, max_x);
    // Now assign corner x locations for each label.
    vector<tuple<int, int, uint32, int>> corners;
    {
      for (int i = 0; i < (int)label_end.size(); i++) {
	auto [x, y, c] = label_end[i];
	corners.emplace_back(x, y, c,
			     max_x + 12 + (label_end.size() - i) * 12);
      }
    }
    
    const int64 time_width = time_end - time_start;
    CHECK(corners.size() == temps.size());
    for (int i = 0; i < (int)temps.size(); i++) {
      const auto &[probe, values] = temps[i];
      auto [lx, ly, color, target_cx] = corners[i];
      // Slightly transparent
      // color = (color & 0xFFFFFF00) | 0x000000AA;

      // x coordinate closest to target_cx, and corresponding y
      // on the temperature line.
      int cx = -1, cy = -1;
      int best_dist = width * 2;
      
      int prev_x = -1, prev_y = -1;
      for (const auto &[timestamp, microdegsc] : values) {
	double fx = (timestamp - time_start) / (double)time_width;
	int x = std::round(fx * width);
	double fy = 1.0f - ((microdegsc - min_temp) / temp_width);
	int y = std::round(fy * height);

	int target_dist = std::abs(x - target_cx);
	if (target_dist < best_dist) {
	  cx = x;
	  cy = y;
	  best_dist = target_dist;
	}
	
	// If points are too close, it looks terrible. Just skip
	// the point.
	if (prev_x > 0.0f && x - prev_x < 1.0f)
	  continue;
	
	if (prev_x >= 0.0f)
	  graph.BlendLine32(prev_x, prev_y, x, y, color);
	prev_x = x;
	prev_y = y;
	// graph.BlendPixel32(x, y, color);
	
	/*
	graph.BlendPixel32(x + 1, y, color & 0xFFFFFF7F);
	graph.BlendPixel32(x - 1, y, color & 0xFFFFFF7F);
	graph.BlendPixel32(x, y + 1, color & 0xFFFFFF7F);
	graph.BlendPixel32(x, y - 1, color & 0xFFFFFF7F);
	*/
      }

      // Now draw the corner indicator.
      uint32 lite_color = (color & 0xFFFFFF00) | 0x0000007F;
      graph.BlendLine32(lx, ly, cx, ly, lite_color);
      graph.BlendLine32(cx, ly + 1, cx, cy, lite_color);      
    }

    auto blit_end = std::chrono::steady_clock::now();

    auto png_start = std::chrono::steady_clock::now();
    string out = graph.SaveToString();
    auto png_end = std::chrono::steady_clock::now();

    printf("Graph timing:\n"
	   "  db: %lld ms\n"
	   "  img: %lld ms\n"
	   "  png: %lld ms\n",
	   (int64)std::chrono::duration_cast<std::chrono::milliseconds>(
	       db_end - db_start).count(),
	   (int64)std::chrono::duration_cast<std::chrono::milliseconds>(
	       blit_end - blit_start).count(),
	   (int64)std::chrono::duration_cast<std::chrono::milliseconds>(
	       png_end - png_start).count());
    
    WebServer::Response r;
    r.code = 200;
    r.status = "OK";
    r.content_type = "image/png";
    r.body = std::move(out);
    return r;
  }
  
  WebServer::Response Table(const WebServer::Request &request) {
    vector<pair<Database::Probe, pair<int64, uint32>>> temps = db->LastTemp();
    WebServer::Response r;
    r.code = 200;
    r.status = "OK";
    r.content_type = "text/html; charset=UTF-8";
    r.body =
      StringPrintf("<!doctype html>\n"
		   "<style>\n"
		   " table {\n"
		   "   border-spacing: 3px; border-collapse: separate;\n"
		   "}\n"
		   " body { font: 12px verdana,helvetica,sans-serif }\n"
		   "</style>\n");

    StringAppendF(&r.body, "<table>\n");
    const int64 now = time(nullptr);
    for (const auto &[probe, cur] : temps) {
      float celsius = (float)cur.second / 1000.0f;
      float fahrenheit = celsius * (9.0f / 5.0f) + 32.0f;
      StringAppendF(&r.body,
		    "<tr><td>%s</td><td>%s</td>"
		    "<td>%lld</td><td>%lld sec. ago</td><td>%.2f &deg;C</td>"
		    "<td>%.2f &deg;F</tr>\n",
		    probe.name.c_str(),
		    probe.desc.c_str(),
		    cur.first,
		    now - cur.first,
		    celsius,
		    fahrenheit);
    }
    StringAppendF(&r.body, "</table>\n");

    return r;
  }

  void KeepAlive() {
    for (;;) {
      // Wait 30 seconds between pings, but not before checking should_die...
      for (int i = 0; i < 30; i++) {
	{
	  MutexLock ml(&should_die_m);
	  if (should_die) return;
	}
	std::this_thread::sleep_for(std::chrono::seconds(1));
      }
      printf("(ping)\n");
      db->Ping();
    }
  }
  
  ~Server() {
    {
      MutexLock ml(&should_die_m);
      should_die = true;
    }
    server->Stop();
    listen_thread.join();
    keepalive_thread.join();
  }

  WebServer *server = nullptr;
  Database *db = nullptr;
  std::mutex should_die_m;
  bool should_die = false;
  string favicon;
  string diagram_svg;
  std::thread listen_thread;
  std::thread keepalive_thread;
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
