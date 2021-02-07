
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <unordered_set>

#include "threadutil.h"
#include "randutil.h"
#include "arcfour.h"
#include "base/logging.h"
#include "base/stringprintf.h"

#include "timer.h"

#include "network.h"
#include "network-util.h"

// XXX
#include "font-problem.h"

using namespace std;

using int64 = int64_t;

// Take some layer and create another layer right after it,
// with the same dimensions (this means that any layers that
// follow can just keep their existing node references). The
// new layer has 0 bias, weight 1 "on the diagonal", and other
// random connections with zero weight.
static constexpr int LAYER_TO_COPY = 3;

// Indices per node for the newly added layer. One of them will
// be used to copy the corresponding existing node. Others can
// be claimed by custom code below; the rest will be random.
static constexpr int NEW_IPN = 256;


// Create a new network with a copy of the indicated layer. The new
// layer is just the sparse identity (ipn = 1).
static Network *DeepenNetwork(ArcFour *rc, const Network &old_net, int layer_idx) {

  // Specs for the new layer.
  int prev_width = old_net.width[layer_idx + 1];
  int prev_height = old_net.height[layer_idx + 1];
  // TODO: Support multichannel layers.
  CHECK(old_net.channels[layer_idx + 1] == 1);
  int prev_num_nodes = old_net.num_nodes[layer_idx + 1];
  CHECK(prev_num_nodes == prev_width * prev_height);

  vector<int> num_nodes = old_net.num_nodes;
  vector<int> width = old_net.width;
  vector<int> height = old_net.height;
  vector<int> channels = old_net.channels;
  vector<uint32_t> renderstyle = old_net.renderstyle;

  printf("Input network num_nodes:");
  for (int nn : num_nodes) {
    printf(" %d", nn);
  }
  printf("\n");
  fflush(stdout);

  auto MakeCopy = [layer_idx](auto &v) {
      printf("size %d   %d + 1 = %d\n", v.size(), layer_idx, layer_idx + 1);
      const auto prev = v[layer_idx + 1];
      v.insert(v.begin() + layer_idx + 1 + 1, prev);
    };

  MakeCopy(num_nodes);
  MakeCopy(width);
  MakeCopy(height);
  MakeCopy(channels);
  MakeCopy(renderstyle);

  vector<int> indices_per_node;
  vector<TransferFunction> transfer_functions;
  for (const Network::Layer &layer : old_net.layers) {
    indices_per_node.push_back(layer.indices_per_node);
    transfer_functions.push_back(layer.transfer_function);
  }

  // Start with just one node.
  indices_per_node.insert(indices_per_node.begin() + layer_idx + 1, 1);
  // RELU and LEAKY_RELU work here, but it has to be the identity
  // with weight 1.0.
  transfer_functions.insert(transfer_functions.begin() + layer_idx + 1, LEAKY_RELU);

  printf("New network num_nodes:");
  for (int nn : num_nodes) {
    printf(" %d", nn);
  }
  printf("\n");

  Network *net = new Network(num_nodes, indices_per_node, transfer_functions);
  CHECK(net->num_layers == old_net.num_layers + 1);
  net->width = width;
  net->height = height;
  net->channels = channels;
  net->renderstyle = renderstyle;

  net->rounds = old_net.rounds;
  net->examples = old_net.examples;
  
  CHECK(layer_idx + 1 < net->layers.size());

  // Layers before; same index.
  for (int i = 0; i < layer_idx + 1; i++)
    net->layers[i] = old_net.layers[i];
  // Layers after; shifted by one.
  for (int i = layer_idx + 1; i < old_net.layers.size(); i++)
    net->layers[i + 1] = old_net.layers[i];

  
  // And initialize the one new layer.
  Network::Layer *new_layer = &net->layers[layer_idx + 1];
  CHECK(new_layer->indices_per_node == 1);
  CHECK(new_layer->transfer_function == LEAKY_RELU);
  new_layer->type = LAYER_SPARSE;
  // Sparse identity matrix.
  new_layer->indices.clear();
  new_layer->weights.clear();
  new_layer->biases.clear();
  for (int i = 0; i < prev_num_nodes; i++) {
    new_layer->indices.push_back(i);
    new_layer->weights.push_back(1.0f);
    new_layer->biases.push_back(0.0f);
  }

  Network::ComputeInvertedIndices(net);
  net->StructuralCheck();

  return net;
}

// Just modifies the indices on the given layer, so this is pretty easy.
static void DensifyLayer(ArcFour *rc, Network *net, int layer_idx) {
  EZLayer ez(*net, layer_idx);

  // this code is written expecting the layer to be the same dimensions,
  // and to already contain one identity node.
  const int width = net->width[layer_idx];
  const int height = net->height[layer_idx];
  const int previous_layer_size = net->num_nodes[layer_idx];
  CHECK(ez.ipn == 1);
  CHECK(width == net->width[layer_idx + 1]);
  CHECK(height == net->height[layer_idx + 1]);
  CHECK(previous_layer_size == net->num_nodes[layer_idx + 1]);    

  for (int src_idx = 0; src_idx < ez.nodes.size(); src_idx++) {
    EZLayer::Node &node = ez.nodes[src_idx];
    node.inputs.reserve(NEW_IPN);
    CHECK(node.inputs.size() == 1);
    std::unordered_set<uint32_t> used;
    used.insert(node.inputs[0].index);
    
    auto MaybeAdd = [&node, &used](int idx) {
        if (used.find(idx) == used.end()) {
          EZLayer::OneIndex oi;
          oi.index = idx;
          oi.weight = 0.0f;
          node.inputs.push_back(oi);
          used.insert(idx);
          return true;
        } else {
          return false;
        }
      };
    
    // This is a custom policy for the lowercase problem.

    // Always depend on the final 26 nodes (these are letter predictions)
    // and the nodes before that (just so that we have some shared dense
    // portion).
    static constexpr int LAST_NODES = 26 * 2;
    CHECK(previous_layer_size >= LAST_NODES);
    for (int i = 0; i < LAST_NODES; i++) {
      MaybeAdd(previous_layer_size - 1 - i);
    }

    static constexpr int SDF_SIZE = FontProblem::SDFConfig().sdf_size;
    static constexpr int NEIGHBORHOOD = 4;
    // In the SDF region, add nodes from the immediate neighborhood.
    if (src_idx < SDF_SIZE * SDF_SIZE) {
      int y = src_idx / SDF_SIZE;
      int x = src_idx % SDF_SIZE;
      for (int dy = -NEIGHBORHOOD; dy <= NEIGHBORHOOD; dy++) {
        for (int dx = -NEIGHBORHOOD; dx <= NEIGHBORHOOD; dx++) {
          int ny = y + dy;
          int nx = x + dx;
          if (ny >= 0 && nx >= 0 &&
              ny < SDF_SIZE && nx <= SDF_SIZE) {
            int nidx = ny * SDF_SIZE + nx;
            MaybeAdd(nidx);
          }
        }
      }
    }

    CHECK(node.inputs.size() <= NEW_IPN);
    
    // The rest, randomly assign.
    while (node.inputs.size() < NEW_IPN) {
      (void)MaybeAdd(RandTo(rc, previous_layer_size));
    }
  }

  ez.ipn = NEW_IPN;
  ez.Repack(net, layer_idx);
  net->ReallocateInvertedIndices();
  Network::ComputeInvertedIndices(net, 6);
}

static void DoDeepen(const string &input_file, const string &output_file) {
  std::unique_ptr<Network> input_net{Network::ReadNetworkBinary(input_file)};

  ArcFour rc(StringPrintf("%lld,%lld,%lld",
                          (int64)time(nullptr),
                          input_net->rounds,
                          input_net->Bytes()));

  input_net->StructuralCheck();

  CHECK(LAYER_TO_COPY < input_net->layers.size()) << "LAYER_TO_COPY out of bounds?";

  std::unique_ptr<Network> output_net{DeepenNetwork(&rc, *input_net, LAYER_TO_COPY)};
  CHECK(output_net.get() != nullptr);

  DensifyLayer(&rc, output_net.get(), LAYER_TO_COPY + 1);
  
  printf("Structural check...\n");
  output_net->StructuralCheck();
  Network::SaveNetworkBinary(*output_net, output_file);
}

int main(int argc, char **argv) {
  CHECK(argc >= 2) << "\n\nUsage:\nwiden.exe net.val [output.val]";

  const string infile = argv[1];
  const string outfile = argc > 2 ? argv[2] : "net-deepened.val";

  DoDeepen(infile, outfile);
  return 0;
}

