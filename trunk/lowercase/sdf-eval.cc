
#include <memory>
#include <string>

#include "network.h"
#include "font-problem.h"

using namespace std;


int main(int argc, char **argv) {

  std::unique_ptr<Network> make_lowercase, make_uppercase;

  // Try loading from disk; null on failure.
  printf("Load networks...\n");
  make_lowercase.reset(Network::ReadNetworkBinary("net0.val"));
  make_uppercase.reset(Network::ReadNetworkBinary("net1.val"));
  
  CHECK(make_lowercase.get() != nullptr);
  CHECK(make_uppercase.get() != nullptr);  

  // XXX somehow needs to be shared?
  // (Right now the default instance is the one used in train.cc.)
  static constexpr FontProblem::SDFConfig SDF_CONFIG;

  printf("Render image...\n");  
  FontProblem::RenderSDF("helvetica.ttf",
                         *make_lowercase,
                         *make_uppercase,
                         SDF_CONFIG,
                         "evaloneoff");


  printf("Done.\n");
  return 0;
}
