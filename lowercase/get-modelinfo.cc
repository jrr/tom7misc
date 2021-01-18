
#include "modelinfo.h"

#include <memory>
#include <string>
#include <cstdint>
#include <cmath>

#include "network.h"
#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;

int main(int argc, char **argv) {
  const string modelfile = argc > 1 ? (string)argv[1] : "net0.val";
  
  // Try loading from disk; null on failure.
  printf("Load network from %s...\n", modelfile.c_str());
  std::unique_ptr<Network> net(Network::ReadNetworkBinary(modelfile));
  
  CHECK(net.get() != nullptr) << modelfile;

  // Get histogram of weights per layer.
  // Only layers with inputs (i.e. not the input layer) have weights.
  const int num_histos = net->num_layers;
    
  static constexpr int HISTOW = 800;
  static constexpr int HISTOH = 256;
  static constexpr int MARGIN = 4;
  
  const int WIDTH = HISTOW * 2 + MARGIN;
  const int HEIGHT = HISTOH * num_histos;

  const ImageRGBA img = ModelInfo(*net, WIDTH, HEIGHT,
				  // {-0.0000001f},
				  // {+0.0000001f},
				  nullopt,
				  nullopt,
				  nullopt,
				  nullopt);
  
  const string outfile = argc > 2 ? (string)argv[2] : "modelinfo.png";
  img.Save(outfile);
  
  printf("Wrote %s.\n", outfile.c_str());
  return 0;
}
