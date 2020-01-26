#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <cstdint>
#include <unordered_map>
#include <thread>
#include <mutex>

#include <mysql++.h>

#include "onewire.h"

#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/util.h"
#include "../cc-lib/web.h"
#include "../cc-lib/threadutil.h"


using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

// Manages database connection.
struct Database {
  using Connection = mysqlpp::Connection;
  using Query = mysqlpp::Query;
  using StoreQueryResult = mysqlpp::StoreQueryResult;

  struct Probe {
    int id = 0;
    string name;
    string desc;
    Probe() {}
  };

  std::mutex database_m; // Coarse locking.
  WebServer::Counter *written = nullptr;
  WebServer::Counter *failed = nullptr;
  map<string, string> config;
  map<string, Probe> probes;
  const string configfile = "database-config.txt";
  const string database_name = "tempo";

  Connection conn{false};

  Database() {
    written = WebServer::GetCounter("temps written");
    failed = WebServer::GetCounter("failed queries");

    config = Util::ReadFileToMap(configfile);
    const string server = config["server"];
    const string user = config["user"];
    const string password = config["password"];
    CHECK(!server.empty()) << "Specify in " << configfile;
    CHECK(!user.empty()) << "Specify in " << configfile;
    CHECK(!user.empty()) << "Specify in " << configfile;
    CHECK(conn.connect(database_name.c_str(),
		       server.c_str(),
		       user.c_str(),
		       password.c_str()));

    // Just read all the probes into a local map.
    Query q = conn.query(
	"select id, code, name, description from probe order by id");
    StoreQueryResult res = q.store();
    CHECK(res) << "Probe setup query failed";
    for (size_t i = 0; i < res.num_rows(); i++) {
      const int id = res[i]["id"];
      const char *code = res[i]["code"];
      const char *name = res[i]["name"];
      const char *desc = res[i]["description"];
      probes[code].id = id;
      probes[code].name = name;
      probes[code].desc = desc;
      printf("%d. %s: %s (%s)\n", id, code, name, desc);
    }
    WebServer::GetCounter("probes in db")->IncrementBy((int64)probes.size());
  }

  // code should be like "28-000009ffbb20"
  string WriteTemp(const string &code, int microdegs_c) {
    MutexLock ml(&database_m);
    auto it = probes.find(code);
    if (it == probes.end()) {
      // printf("Unknown probe %s!\n", code.c_str());
      return "???";
    }

    const int id = it->second.id;
    uint64 now = time(nullptr);
    string qs = StringPrintf("insert into tempo.reading "
			     "(timestamp, probeid, microdegsc) "
			     "values (%llu, %d, %d)",
			     now, id, microdegs_c);
    Query q = conn.query(qs.c_str());
    if (q.exec()) {
      written->Increment();
    } else {
      failed->Increment();
    }
    return it->second.name;
  }

  // Get all the temperatures (collated by probe name) in the given interval.
  std::unordered_map<string, vector<pair<int64, uint32>>> AllTempsIn(int64 time_start,
								     int64 time_end) {
    std::unordered_map<int, string> probe_by_id;
    for (auto &p : probes) probe_by_id[p.second.id] = p.second.name;

    // This can be done as one query of course, but we can make smaller queries by
    // performing a separate one for each probe. (Not obvious which way is better?)
    std::unordered_map<string, vector<pair<int64, uint32>>> out;
    for (const auto &p : probe_by_id) {
      MutexLock ml(&database_m);

      string qs = StringPrintf("select timestamp, microdegsc "
			       "from tempo.reading "
			       "where probeid = %d "
			       "and timestamp >= %lld "
			       "and timestamp <= %lld "
			       "order by timestamp",
			       p.first,
			       time_start,
			       time_end);
      Query q = conn.query(qs.c_str());
      StoreQueryResult res = q.store();
      if (!res) {
	failed->Increment();
	continue;
      }

      vector<pair<int64, uint32>> vec;
      vec.reserve(res.num_rows());
      for (size_t i = 0; i < res.num_rows(); i++) {
	const int64 id = res[i]["timestamp"];
	const uint32 microdegsc = res[i]["microdegsc"];
	vec.emplace_back(id, microdegsc);
      }
      out[p.second] = std::move(vec);
    }

    return out;
  }

  // Get all the temperatures (collated by probe name) in the given interval.
  std::unordered_map<string, pair<int64, uint32>> LastTemp() {
    std::unordered_map<int, string> probe_by_id;
    for (auto &p : probes) probe_by_id[p.second.id] = p.second.name;

    std::unordered_map<string, pair<int64, uint32>> out;
    for (const auto &p : probe_by_id) {
      MutexLock ml(&database_m);

      string qs = StringPrintf("select timestamp, microdegsc "
			       "from tempo.reading "
			       "where probeid = %d "
			       "order by timestamp desc "
			       "limit 1",
			       p.first);
      printf("%s\n", qs.c_str());
      Query q = conn.query(qs.c_str());
      StoreQueryResult res = q.store();
      if (!res || res.num_rows() != 1) {
	failed->Increment();
	continue;
      }

      const int64 id = res[0]["timestamp"];
      const uint32 microdegsc = res[0]["microdegsc"];
      out[p.second] = make_pair(id, microdegsc);
    }

    return out;
  }

};

struct Server {
  Server(Database *db) : db(db) {
    server = WebServer::Create();
    CHECK(server);
    favicon = Util::ReadFile("favicon.png");

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
    
    // TODO!
    server->AddHandler("/",
		       [this](const WebServer::Request &req) {
			 return Diagram(req);
		       });

    // Detach listening thread.
    listen_thread = std::thread([this](){
	this->server->ListenOn(8080);
      });
  }

  WebServer::Response Diagram(const WebServer::Request &request) {
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
		    "<tr><td>%s</td><td>%lld</td><td>%.2f &deg;C</td><td>%.2f &deg;F</tr>\n",
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
  std::thread listen_thread;
};


int main(int argc, char **argv) {
  Database db;
  Server server(&db);

  OneWire onewire;
  WebServer::GetCounter("probes found")->IncrementBy((int64)onewire.probes.size());
  
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
