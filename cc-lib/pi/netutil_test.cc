
#include "netutil.h"

using namespace std;
using ip4 = NetUtil::ip4;
using mac = NetUtil::mac;

int main(int argc, char **argv) {
  for (const auto [name, ip] : NetUtil::GetIP4Interfaces()) {
    auto [a, b, c, d] = ip;
    printf("%s: %d.%d.%d.%d\n", name.c_str(), a, b, c, d);
  }

  for (const auto [name, mac] : NetUtil::GetMACAddresses()) {
    auto [a, b, c, d, e, f] = mac;
    printf("%s: %02x:%02x:%02x:%02x:%02x:%02x\n", name.c_str(), a, b, c, d, e, f);
  }
  
  printf("Best guess IP (only): ");
  optional<ip4> ip = NetUtil::BestGuessIPAddress();
  if (ip) {
    auto [a, b, c, d] = *ip;
    printf("%d.%d.%d.%d\n", a, b, c, d);
  } else {
    printf("(nullopt!)\n");
  }

  printf("Best guess IP with MAC: ");
  optional<pair<ip4, mac>> opt = NetUtil::BestGuessIPWithMAC();
  if (opt) {
    auto [a, b, c, d] = std::get<0>(*opt);
    auto [e, f, g, h, i, j] = std::get<1>(*opt);
    printf("%d.%d.%d.%d  %02x:%02x:%02x:%02x:%02x:%02x\n", a, b, c, d,
           e, f, g, h, i, j);
  } else {
    printf("(nullopt!)\n");
  }
  
  // This test can't really fail (other than crashing); you have to
  // inspect the output.
  return 0;
}
