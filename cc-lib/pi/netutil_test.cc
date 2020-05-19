
#include "netutil.h"

using namespace std;
using ip4 = NetUtil::ip4;

int main(int argc, char **argv) {
  for (const auto [name, ip] : NetUtil::GetIP4Interfaces()) {
    auto [a, b, c, d] = ip;
    printf("%s: %d.%d.%d.%d\n", name.c_str(), a, b, c, d);
  }

  printf("Best guess: ");
  optional<ip4> ip = NetUtil::BestGuessIPAddress();
  if (ip) {
    auto [a, b, c, d] = *ip;
    printf("%d.%d.%d.%d\n", a, b, c, d);
  } else {
    printf("(nullopt!)\n");
  }

  // This test can't really fail (other than crashing); you have to
  // inspect the output.
  return 0;
}
