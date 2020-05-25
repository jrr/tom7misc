#include "database.h"

#include <unordered_map>
#include <string>
#include <map>
#include <cstdint>
#include <unistd.h>

#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"
#include "threadutil.h"
#include "pi/netutil.h"
#include "periodically.h"

#include <mysql++.h>
#include <dbdriver.h>

using namespace std;
using uint8 = uint8_t;
using uint16 = uint16_t;
using uint32 = uint32_t;
using uint64 = uint64_t;
using int64 = int64_t;

static constexpr int SECONDS_BETWEEN_WRITES = 20;
static constexpr int SECONDS_BETWEEN_UPDATE_SEENS = 61;
static constexpr int SECONDS_BETWEEN_PINGS = 32;

static string Escape(string s) {
  mysqlpp::DBDriver::escape_string_no_conn(&s, nullptr, 0);
  return s;
}

// TODO: It's possible that this program is using mysql++ incorrectly
// despite the coarse locking. See advice from the documentation:
//   https://tangentsoft.com/mysqlpp/doc/html/userman/threads.html
// ... that says you must call thread_start in each thread before
// using the connection object (though this is at odds with the
// thread-per-request approach that WebServer takes, ugh).

Database::Database() {
  written = WebServer::GetCounter("readings written");
  batches = WebServer::GetCounter("batches written");
  failed = WebServer::GetCounter("failed queries");

  config = Util::ReadFileToMap(configfile);

  {
    const auto iface = NetUtil::BestGuessIPWithMAC();
    CHECK(iface.has_value()) << "Unable to determine IP / MAC address!";
    // Used as the primary key in the device table.
    const auto [ip, mac] = *iface;

    {
      const auto [a, b, c, d] = ip;
      ipaddress = StringPrintf("%d.%d.%d.%d", a, b, c, d);
    }

    {
      const auto [a, b, c, d, e, f] = mac;
      // The key is human readable as a compromise for usability, but
      // since it's used as the primary key we drop the colons.
      mac_key = StringPrintf("%02x%02x%02x" "%02x%02x%02x",
			     a, b, c,  d, e, f);
    }
  }

  CHECK(Connect());  

  // Might make sense to do this when we reconnect too, but then we'd
  // at least want to recalculate our IP address.
  {
    const int64 now = time(nullptr);
    string qs = StringPrintf(
	"replace into device (mac, lastseen, ipaddress, location) "
	"values (\"%s\", %llu, \"%s\", \"%s\")",
	mac_key.c_str(),
	now,
	ipaddress.c_str(),
	Escape(config["location"]).c_str());
    Query q = conn.query(qs);
    CHECK(q.exec()) << "Couldn't register device in database?\n" << qs;
  }
  
  // Just read all the probes into a local map.
  Query q = conn.query(
      "select id, type, code, name, description from probe order by id");
  StoreQueryResult res = q.store();
  CHECK(res) << "Probe setup query failed";
  for (size_t i = 0; i < res.num_rows(); i++) {
    const int id = res[i]["id"];
    const int type = res[i]["type"];
    CHECK(type == TEMPERATURE ||
	  type == HUMIDITY) << "Unsupported probe type " << type;
    const char *code = res[i]["code"];
    const char *name = res[i]["name"];
    const char *desc = res[i]["description"];
    probes[code].id = id;
    probes[code].type = (ProbeType)type;
    probes[code].name = name;
    probes[code].desc = desc;
    printf("%d. %s: %s (%s)\n", id, code, name, desc);
  }
  WebServer::GetCounter("probes in db")->IncrementBy((int64)probes.size());

  periodic_thread = std::thread([this](){
      this->PeriodicThread();
    });
}

Database::~Database() {
  {
    MutexLock ml(&database_m);
    should_die = true;
  }

  periodic_thread.join();
  // XXX close database connection cleanly
}

// Must hold lock.
void Database::Ping() {
  bool ok = conn.ping();
  if (ok) {
    WebServer::GetCounter("successful ping")->Increment();
  } else {
    conn.disconnect();
    if (Connect()) {
      WebServer::GetCounter("reconnected")->Increment();
    } else {
      WebServer::GetCounter("failed to reconnect")->Increment();
    }
  }
}

// Internal. Should hold the lock or otherwise guarantee exclusive
// access to the connection object.
bool Database::Connect() {
  const string server = config["server"];
  const string user = config["user"];
  const string password = config["password"];
  CHECK(!server.empty()) << "Specify in " << configfile;
  CHECK(!user.empty()) << "Specify in " << configfile;
  CHECK(!user.empty()) << "Specify in " << configfile;

  return conn.connect(database_name.c_str(),
		      server.c_str(),
		      user.c_str(),
		      password.c_str());
}

const Database::Probe *Database::GetProbe(const string &code) {
  auto it = probes.find(code);
  if (it == probes.end()) return nullptr;
  return &it->second;
}

string Database::WriteValue(const string &code, uint32_t value) {
  MutexLock ml(&database_m);
  auto it = probes.find(code);
  if (it == probes.end()) {
    // printf("Unknown probe %s!\n", code.c_str());
    return "???";
  }

  const int id = it->second.id;
  const uint64 now = time(nullptr);

  batch.emplace_back(now, id, value);
  
  return it->second.name;
}

void Database::PeriodicThread() {
  // This doesn't need to run very often, so we just wake up
  // approximately every second and see if there's anything to do.
  // Wouldn't be too hard to support ms-level events here, though.
  Periodically write_p(SECONDS_BETWEEN_WRITES);
  Periodically update_seen_p(SECONDS_BETWEEN_UPDATE_SEENS);
  Periodically ping_p(SECONDS_BETWEEN_PINGS);
  for (;;) {
    // n.b. can wake early on signal, which is fine...
    sleep(1);

    {
      MutexLock ml(&database_m);
      if (ping_p.ShouldRun()) {
	Ping();
      }
      
      if (write_p.ShouldRun()) {
	Write();
      }

      if (update_seen_p.ShouldRun()) {
	UpdateLastSeen();
      }

      if (should_die) return;
    }
  }
}

// Must hold lock.
void Database::UpdateLastSeen() {
  int64 now = time(nullptr);
  string qs =
    StringPrintf("update tempo.device "
		 "set lastseen = %llu "
		 "where mac = \"%s\"",
		 now, mac_key.c_str());
  Query q = conn.query(qs);
  if (!q.exec())
    failed->Increment();
}

// Must hold lock.
void Database::Write() {
  if (batch.empty()) return;

  // TODO: If we are disconnected for a very long time, we should
  // probably clear the batch in smaller chunks? Or just enforce
  // a maximum size for it with some ring buffer etc.?
  
  string qs = "insert into tempo.reading "
    "(timestamp, probeid, value) "
    "values ";

  bool first = true;
  for (const auto [t, id, microdegs_c] : batch) {
    if (!first) qs.push_back(',');
    StringAppendF(&qs, " row(%llu, %d, %d)", t, id, microdegs_c);
    first = false;
  }

  Query q = conn.query(qs);
  if (q.exec()) {
    written->IncrementBy(batch.size());
    batches->Increment();
    batch.clear();
  } else {
    failed->Increment();
  }
}

std::vector<pair<Database::Probe, vector<pair<int64, uint32>>>>
Database::AllReadingsIn(int64 time_start, int64 time_end) {
  // This can be done as one query of course, but we can make
  // smaller queries by performing a separate one for each probe.
  // (Not obvious which way is better?)
  std::vector<pair<Probe, vector<pair<int64, uint32>>>> out;
  for (const auto &p : probes) {
    const Probe &probe = p.second;
    MutexLock ml(&database_m);

    string qs = StringPrintf("select timestamp, value "
			     "from tempo.reading "
			     "where probeid = %d "
			     "and timestamp >= %lld "
			     "and timestamp <= %lld "
			     "order by timestamp",
			     probe.id,
			     time_start,
			     time_end);
    Query q = conn.query(qs);
    StoreQueryResult res = q.store();
    if (!res) {
      failed->Increment();
      continue;
    }

    vector<pair<int64, uint32>> vec;
    vec.reserve(res.num_rows());
    for (size_t i = 0; i < res.num_rows(); i++) {
      const int64 id = res[i]["timestamp"];
      const uint32 microdegsc = res[i]["value"];
      vec.emplace_back(id, microdegsc);
    }
    out.emplace_back(probe, std::move(vec));
  }

  return out;
}

vector<pair<Database::Probe, pair<int64_t, uint32_t>>>
Database::LastReading() {
  vector<pair<Database::Probe, pair<int64_t, uint32_t>>> out;
  for (const auto &p : probes) {
    const Probe &probe = p.second;
    MutexLock ml(&database_m);

    string qs = StringPrintf("select timestamp, value "
			     "from tempo.reading "
			     "where probeid = %d "
			     "order by timestamp desc "
			     "limit 1",
			     probe.id);
    // printf("%s\n", qs.c_str());
    Query q = conn.query(qs);
    StoreQueryResult res = q.store();
    if (!res || res.num_rows() != 1) {
      failed->Increment();
      continue;
    }

    const int64 id = res[0]["timestamp"];
    const uint32 microdegsc = res[0]["value"];
    out.emplace_back(probe, make_pair(id, microdegsc));
  }

  return out;
}

/*
  struct Device {
    string mac;
    int64_t lastseen;
    string ip;
    string location;
  };
*/
vector<Database::Device> Database::GetDevices() {
  string qs = StringPrintf("select mac, lastseen, ipaddress, location "
			   "from tempo.device "
			   "order by mac");
  MutexLock ml(&database_m);
  Query q = conn.query(qs);
  StoreQueryResult res = q.store();
  if (!res) {
    failed->Increment();
    return {};
  }

  vector<Device> vec;
  vec.reserve(res.num_rows());
  for (size_t i = 0; i < res.num_rows(); i++) {
    Device device;
    device.mac = (string)res[i]["mac"];
    device.lastseen = res[i]["lastseen"];
    device.ipaddress = (string)res[i]["ipaddress"];
    device.location = (string)res[i]["location"];
    vec.emplace_back(std::move(device));
  }
  return vec;
}
