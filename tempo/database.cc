#include "database.h"

#include <unordered_map>
#include <string>
#include <map>
#include <cstdint>

#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "threadutil.h"

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using uint64 = uint64_t;
using int64 = int64_t;

Database::Database() {
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

string Database::WriteTemp(const string &code, int microdegs_c) {
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

std::unordered_map<string, vector<pair<int64, uint32>>>
Database::AllTempsIn(int64 time_start, int64 time_end) {
  std::unordered_map<int, string> probe_by_id;
  for (auto &p : probes) probe_by_id[p.second.id] = p.second.name;

  // This can be done as one query of course, but we can make
  // smaller queries by performing a separate one for each probe.
  // (Not obvious which way is better?)
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

std::unordered_map<string, pair<int64, uint32>> Database::LastTemp() {
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
    // printf("%s\n", qs.c_str());
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
