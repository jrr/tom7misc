
#include <memory>
#include <string>

#include "network.h"
#include "font-problem.h"

using namespace std;


int main(int argc, char **argv) {

  std::unique_ptr<Network> net;

  // Try loading from disk; null on failure.
  printf("Load network...\n");
  net.reset(Network::ReadNetworkBinary("net.val"));

  CHECK(net.get() != nullptr);

  printf("Render image...\n");

  // XXX somehow needs to be shared?
  static constexpr int ROW0_MAX_PTS = 38;
  static constexpr int ROW1_MAX_PTS = 14;
  static constexpr int ROW2_MAX_PTS = 10;
  vector<int> row_max_points = {
    ROW0_MAX_PTS,
    ROW1_MAX_PTS,
    ROW2_MAX_PTS,
  };


  FontProblem::RenderVector("helvetica.ttf",
			    *net,
			    row_max_points,
			    "eval.png");


  printf("Done.\n");
  return 0;
}
