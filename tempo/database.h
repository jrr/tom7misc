
#include <string>
#include <map>
#include <mutex>
#include <cstdint>

#include <mysql++.h>

#include "../cc-lib/web.h"

using namespace std;

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

  Database();

  // code should be like "28-000009ffbb20"
  string WriteTemp(const string &code, int microdegs_c);

  // Get all the temperatures (collated by probe name) in the given interval.
  std::unordered_map<string, vector<pair<int64_t, uint32_t>>>
  AllTempsIn(int64_t time_start, int64_t time_end);

  // Get all the temperatures (collated by probe name) in the given interval.
  std::unordered_map<string, pair<int64_t, uint32_t>> LastTemp();
};
  
