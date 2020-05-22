
#include <string>
#include <map>
#include <mutex>
#include <cstdint>
#include <thread>

#include <mysql++.h>

#include "../cc-lib/web.h"

using namespace std;

// Manages database connection.
struct Database final {
  using Connection = mysqlpp::Connection;
  using Query = mysqlpp::Query;
  using StoreQueryResult = mysqlpp::StoreQueryResult;

  // All probes store integer data but we interpret/display
  // it differently depending on the type.
  enum ProbeType {
    INVALID = 0,
    TEMPERATURE = 1,
    HUMIDITY = 2,
  };
  
  struct Probe {
    int id = 0;
    ProbeType type = INVALID;
    string name;
    string desc;
    Probe() {}
  };

  std::mutex database_m; // Coarse locking.
  WebServer::Counter *written = nullptr;
  WebServer::Counter *batches = nullptr;
  WebServer::Counter *failed = nullptr;
  map<string, string> config;
  // Indexed by hex code.
  map<string, Probe> probes;
  const string configfile = "database-config.txt";
  const string database_name = "tempo";

  Connection conn{false};

  Database();
  ~Database();
  
  // Ping the server, and try to reconnect if it fails.
  // Should just call this periodically (every few minutes).
  // TODO: Do this from the periodicthread instead.
  void Ping();
  
  // Returns a pointer to the probe or nullptr if not found.
  // (Note that the pointer can be invalidated by map operations.)
  const Probe *GetProbe(const string &code);
  
  // code should be like "28-000009ffbb20" for onewire.
  // Returns the probe's name.
  // For temperature readings, integer value is in microdegrees Celsius.
  // For humidity readings, ....TODO
  // Values are batched up to reduce database writes.
  string WriteValue(const string &code, int microdegs_c);

  // Get all the readings (collated by probe) in the given interval.
  vector<pair<Probe, vector<pair<int64_t, uint32_t>>>>
  AllReadingsIn(int64_t time_start, int64_t time_end);

  // Get all the readings (collated by probe) in the given interval.
  vector<pair<Probe, pair<int64_t, uint32_t>>> LastReading();

private:
  // Write the batch to the database now (if possible). Must hold lock.
  void Write();
  // Mark this device as recently alive in the database.
  void UpdateLastSeen();
  
  // Runs in the background and does periodic tasks, like Write.
  void PeriodicThread();

  bool should_die = false;
  vector<tuple<int64_t, int, uint32_t>> batch;
  std::thread periodic_thread;
  string mac_key, ipaddress;
  bool Connect();
};
  
