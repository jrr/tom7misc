#include "netutil.h"

#include <arpa/inet.h>
#include <sys/socket.h>
#include <linux/if_packet.h>
#include <netdb.h>
#include <ifaddrs.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/if_link.h>

#include <map>
#include <string>
#include <optional>
#include <utility>

using namespace std;
using ip4 = NetUtil::ip4;
using mac = NetUtil::mac;

// based on the manpage for getifaddrs.
std::map<std::string, ip4> NetUtil::GetIP4Interfaces() {
  // This allocates a linked list in ifaddr.
  struct ifaddrs *ifaddr = nullptr;
  if (getifaddrs(&ifaddr) == -1)
    return {};

  map<string, ip4> ret;
  for (struct ifaddrs *ifa = ifaddr;
       ifa != nullptr;
       ifa = ifa->ifa_next) {
    if (ifa->ifa_addr == nullptr)
      continue;

    switch (ifa->ifa_addr->sa_family) {
    case AF_INET: {
      const char *name = ifa->ifa_name;
      // sockaddr_in means inet, not "in"!
      const sockaddr_in *inet = (const sockaddr_in *)ifa->ifa_addr;
      const uint32_t addr = ntohl(inet->sin_addr.s_addr);

      ret.emplace(name, make_tuple((addr >> 24) & 0xFF,
                                   (addr >> 16) & 0xFF,
                                   (addr >> 8) & 0xFF,
                                   addr & 0xFF));
      break;
    }
    default:
      // AF_INET6 also interesting, but not supported here.
      continue;
    }
  }

  freeifaddrs(ifaddr);
  return ret;
}

std::map<std::string, mac> NetUtil::GetMACAddresses() {
  struct ifaddrs *ifaddr = nullptr;
  if (getifaddrs(&ifaddr) == -1)
    return {};

  map<string, mac> ret;
  for (struct ifaddrs *ifa = ifaddr;
       ifa != nullptr;
       ifa = ifa->ifa_next) {
    if (ifa->ifa_addr == nullptr)
      continue;

    switch (ifa->ifa_addr->sa_family) {
    case AF_PACKET: {
      const char *name = ifa->ifa_name;
      // sockaddr_ll is "link level"
      const sockaddr_ll *ll = (const sockaddr_ll *)ifa->ifa_addr;
      const uint8_t *mac = &ll->sll_addr[0];
      ret.emplace(name, make_tuple(mac[0], mac[1], mac[2],
                                   mac[3], mac[4], mac[5]));
      break;
    }
    default:
      continue;
    }
  }

  freeifaddrs(ifaddr);
  return ret;
}


std::optional<std::pair<ip4, mac>> NetUtil::BestGuessIPWithMAC() {
  // TODO: Better logic here.
  // Avoid 127.0.0.1.
  // Prefer interfaces named "eth0", "eth1", "wlan0", etc.
  // Prefer "real" internet addresses over 10.* and 192.168.*
  // And prefer those NAT addresses over "link local" addresses
  // like 169.254.* (have seen this in practice while the network
  // comes up).
  map<string, ip4> ips = GetIP4Interfaces();
  map<string, mac> macs = GetMACAddresses();

  map<string, std::pair<ip4, mac>> ifaces;
  for (const auto &[name, ip] : ips) {
    auto it = macs.find(name);
    if (it != macs.end()) {
      ifaces[name] = make_pair(ip, it->second);
    }
  }
  
  if (ifaces.find("eth0") != ifaces.end()) return {ifaces["eth0"]};
  if (ifaces.find("wlan0") != ifaces.end()) return {ifaces["wlan0"]};
  if (ifaces.begin() != ifaces.end()) return {ifaces.begin()->second};
  return {};
}

std::optional<ip4> NetUtil::BestGuessIPAddress() {
  auto opt = BestGuessIPWithMAC();
  if (opt) return {std::get<0>(*opt)};

  // Don't fail (because we didn't find a MAC) if there are ANY
  // interfaces with IP addresses.
  map<string, ip4> ips = GetIP4Interfaces();
  if (ips.empty()) return {};
  return {ips.begin()->second};
}

