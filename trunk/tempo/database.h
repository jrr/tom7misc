
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

  struct Probe {
    int id = 0;
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
  void Ping();
  
  // Returns a pointer to the probe or nullptr if not found.
  // (Note that the pointer can be invalidated by map operations.)
  const Probe *GetProbe(const string &code);
  
  // code should be like "28-000009ffbb20"
  // Returns the probe's name.
  // Batches temperatures to reduce database writes.
  string WriteTemp(const string &code, int microdegs_c);

  // Get all the temperatures (collated by probe) in the given interval.
  vector<pair<Probe, vector<pair<int64_t, uint32_t>>>>
  AllTempsIn(int64_t time_start, int64_t time_end);

  // Get all the temperatures (collated by probe) in the given interval.
  vector<pair<Probe, pair<int64_t, uint32_t>>> LastTemp();

private:
  // Write the batch to the database now (if possible). Must hold lock.
  void Write();
  // Implements write thread.
  void WriteThread();

  bool should_die = false;
  vector<tuple<int64_t, int, uint32_t>> batch;
  std::thread write_thread;
  bool Connect();
};
  
