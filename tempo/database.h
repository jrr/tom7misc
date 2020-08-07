
#include <string>
#include <map>
#include <mutex>
#include <cstdint>
#include <thread>
#include <set>
#include <optional>
#include <memory>

#include <mysql++.h>

#include "../cc-lib/webserver.h"
#include "../cc-lib/arcfour.h"

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
  WebServer::Counter *notwritten = nullptr;  
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
    
  // Returns a pointer to the probe or nullptr if not found.
  // (Note that the pointer can be invalidated by map operations.)
  const Probe *GetProbe(const string &code);
  
  // code should be like "28-000009ffbb20" for onewire.
  // Returns the probe's name.
  // For temperature readings, integer value is in microdegrees Celsius.
  // For humidity readings, integer value is in basis points, ranging
  //     from 0 (= 0% RH) to 10,000 (= 100% RH).
  // Values are batched up to reduce database writes.
  string WriteValue(const string &code, uint32_t value);

  // Get all the readings (collated by probe) in the given interval.
  vector<pair<Probe, vector<pair<int64_t, uint32_t>>>>
  AllReadingsIn(int64_t time_start, int64_t time_end);

  // Get some readings (collated by probe) in the given interval. If
  // the probes set is non-empty, only return for the probes whose ids
  // appear in the set. Tries to reduce the number of values (assuming
  // we get about 1 read per second).
  vector<pair<Probe, vector<pair<int64_t, uint32_t>>>>
  SmartReadingsIn(int64_t time_start, int64_t time_end,
		  const std::set<int> &probes_included);
  
  // Get all the readings (collated by probe) in the given interval.
  vector<pair<Probe, pair<int64_t, uint32_t>>> LastReading();

  // Read all the devices from the database.
  struct Device {
    string mac;
    int64_t lastseen;
    string ipaddress;
    string location;
    string rev;
  };
  vector<Device> GetDevices();
  
private:
  // Write the batch to the database now (if possible). Must hold lock.
  void Write();
  // Mark this device as recently alive in the database.
  void UpdateLastSeen();
  // Ping the server, and try to reconnect if it fails.
  // Automatically called every few minutes by the periodic thread.
  void Ping();
  
  // Runs in the background and does periodic tasks, like Write.
  void PeriodicThread();

  // (linear time...)
  std::optional<Probe> ProbeById(int id) const;
  
  std::unique_ptr<ArcFour> rc;
  bool should_die = false;
  // timestamp, probe, value, sample_key
  vector<tuple<int64_t, int, uint32_t, uint16_t>> batch;
  std::thread periodic_thread;
  string mac_key, ipaddress;
  bool Connect();
};
  
