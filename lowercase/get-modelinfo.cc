
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
  const string modelfile = argc > 1 ? (string)argv[1] : "net0.val";

  // Try loading from disk; null on failure.
  printf("Load network from %s...\n", modelfile.c_str());
  std::unique_ptr<Network> net(Network::ReadNetworkBinary(modelfile));

  CHECK(net.get() != nullptr) << modelfile;

  // Get histogram of weights per layer.
  // Only layers with inputs (i.e. not the input layer) have weights.
  const int num_histos = net->num_layers;

  static constexpr int HISTOW = 800;
  static constexpr int HISTOH = 220;
  static constexpr int MARGIN = 4;

  const int WIDTH = HISTOW * 2 + MARGIN;
  const int HEIGHT = HISTOH * num_histos;

  const ImageRGBA histos = ModelInfo::Histogram(*net, WIDTH, HEIGHT,
                                                // {-0.0000001f},
                                                // {+0.0000001f},
                                                nullopt,
                                                nullopt,
                                                nullopt,
                                                nullopt);

  char dates[128] = {};
  time_t tt = time(nullptr);
  // XXX even though TZ is set and 'date +"%H:%M"' works as expected
  // in mingw, this reports UTC time?
  tt -= 3600 * 5;

  strftime(dates, 127, "%d %b %Y  %H:%M", localtime(&tt));
  vector<string> lines = {
    StringPrintf("%s  round %lld   examples %lld   bytes %lld   real layers %d",
                 dates,
                 net->rounds, net->examples, net->Bytes(), net->num_layers),
    StringPrintf("  Input: %dx%dx%d = %d                %lld total params",
                 net->width[0], net->height[0], net->channels[0], net->num_nodes[0],
                 net->TotalParameters()),
  };

  for (int layer_idx = 0; layer_idx < net->num_layers; layer_idx++) {
    const Network::Layer &layer = net->layers[layer_idx];
    const int width = net->width[layer_idx + 1];
    const int height = net->height[layer_idx + 1];
    const int channels = net->channels[layer_idx + 1];
    const int num_nodes = net->num_nodes[layer_idx + 1];
    const int ipn = layer.indices_per_node;
    const char *types = layer.type == LAYER_DENSE ? "DENSE" :
      layer.type == LAYER_SPARSE ? "SPARSE" : "???";
    lines.push_back(
        StringPrintf("Layer %d: %dx%dx%d = %d (%s). ipn %d",
                     layer_idx, width, height, channels, num_nodes,
                     types, ipn));
  }

  const int TOP = 20 * lines.size();
  ImageRGBA img(histos.Width(), histos.Height() + TOP);
  img.Clear32(0x000000FF);
  img.BlendImage(0, TOP, histos);
  for (int i = 0; i < lines.size(); i++) {
    img.BlendText2x32(0, i * 20, 0xCCCCCCFF, lines[i]);
  }

  const string outfile = argc > 2 ? (string)argv[2] : "modelinfo.png";
  img.Save(outfile);

  printf("Wrote %s.\n", outfile.c_str());
  return 0;
}
