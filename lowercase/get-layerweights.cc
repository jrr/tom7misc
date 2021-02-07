
#include "modelinfo.h"

#include <memory>
#include <string>
#include <cstdint>
#include <cmath>
#include <time.h>

#include "network.h"
#include "base/logging.h"
#include "base/stringprintf.h"

using namespace std;

int main(int argc, char **argv) {
  CHECK(argc == 3) << "Usage:\nget-layerweights.exe net0.val outputbasename";
  const string modelfile = argv[1];
  const string outputbase = argv[2];
  
  // Try loading from disk; null on failure.
  printf("Load network from %s...\n", modelfile.c_str());
  std::unique_ptr<Network> net(Network::ReadNetworkBinary(modelfile));
  
  CHECK(net.get() != nullptr) << modelfile;

  for (int i = 0; i < net->layers.size(); i++) {
    const ImageRGBA img = ModelInfo::LayerWeights(*net, i, false);

    const string outfile = StringPrintf("%s-layer%d.png",
                                        outputbase.c_str(), i);
    img.Save(outfile);
    printf("Wrote %s.\n", outfile.c_str());
  }

  return 0;
}
