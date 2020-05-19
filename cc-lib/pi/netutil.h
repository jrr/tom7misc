#ifndef __CCLIB_NETUTIL_H
#define __CCLIB_NETUTIL_H

#include <map>
#include <string>
#include <cstdint>
#include <utility>
#include <optional>

struct NetUtil {

  using ip4 = std::tuple<uint8_t, uint8_t, uint8_t, uint8_t>;
  
  static std::map<std::string, ip4> GetIP4Interfaces();

  // Like above, but tries to figure out which one is our "real"
  // external-facing IP address.
  static std::optional<ip4> BestGuessIPAddress();

};

#endif
