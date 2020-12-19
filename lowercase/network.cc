
#include "network.h"

#include <cstdio>
#include <cmath>

#include <algorithm>
#include <utility>
#include <cstdint>
#include <string>
#include <vector>

#include "base/logging.h"
#include "base/stringprintf.h"

#include "threadutil.h"

using namespace std;

using int64 = int64_t;

const char *TransferFunctionName(TransferFunction tf) {
  switch (tf) {
  case SIGMOID: return "SIGMOID";
  case RELU: return "RELU";
  case LEAKY_RELU: return "LEAKY_RELU";
  default: return "??INVALID??";
  }
}

const char *LayerTypeName(LayerType lt) {
  switch (lt) {
  case LAYER_DENSE: return "LAYER_DENSE";
  case LAYER_SPARSE: return "LAYER_SPARSE";
  default: return "??INVALID??";
  }
}

Network::Network(vector<int> num_nodes,
		 vector<int> indices_per_node,
		 vector<TransferFunction> transfer_functions) :
  num_layers(num_nodes.size() - 1),
  num_nodes(num_nodes) {
  CHECK(num_nodes.size() >= 1) << "Must include input layer.";
  CHECK_EQ(num_layers, indices_per_node.size());
  CHECK_EQ(num_layers, transfer_functions.size());
  layers.resize(num_layers);
  for (int i = 0; i < num_layers; i++) {
    Layer &layer = layers[i];
    layer.indices_per_node = indices_per_node[i];
    layer.transfer_function = transfer_functions[i];
    layer.indices.resize(indices_per_node[i] * num_nodes[i + 1], 0);
    layer.weights.resize(indices_per_node[i] * num_nodes[i + 1], 0.0);
    layer.biases.resize(num_nodes[i + 1], 0.0);
  }

  inverted_indices.resize(num_layers);
  for (int i = 0; i < num_layers; i++) {
    InvertedIndices &ii = inverted_indices[i];
    ii.start.resize(num_nodes[i], 0);
    ii.length.resize(num_nodes[i], 0);
    ii.output_indices.resize(indices_per_node[i] * num_nodes[i + 1], 0);
  }
}

Network *Network::Clone(const Network &other) {
  return new Network(other);
}

int64 Network::Bytes() const {
  int64 ret = sizeof *this;
  ret += sizeof num_nodes[0] * num_nodes.size();
  ret += sizeof width[0] * width.size();
  ret += sizeof height[0] * height.size();
  ret += sizeof channels[0] * channels.size();
  // Layer structs.
  for (int i = 0; i < num_layers; i++) {
    ret += sizeof layers[i] +
      sizeof layers[i].indices[0] * layers[i].indices.size() +
      sizeof layers[i].weights[0] * layers[i].weights.size() +
      sizeof layers[i].biases[0] * layers[i].biases.size();
  }
  // Inverted index structs.
  for (int i = 0; i < num_layers; i++) {
    ret += sizeof inverted_indices[i] +
      sizeof inverted_indices[i].start[0] * inverted_indices[i].start.size() +
      sizeof inverted_indices[i].length[0] * inverted_indices[i].length.size() +
      sizeof inverted_indices[i].output_indices[0] *
	  inverted_indices[i].output_indices.size();
  }

  return ret;
}

void Network::NaNCheck(const char *message) const {
  bool has_nans = false;
  vector<std::pair<int, int>> layer_nans;
  for (const Layer &layer : layers) {
    int w = 0, b = 0;
    for (float f : layer.weights) if (std::isnan(f)) w++;
    for (float f : layer.biases) if (std::isnan(f)) b++;
    layer_nans.emplace_back(w, b);
    if (w > 0 || b > 0) has_nans = true;
  }
  if (has_nans) {
    string err;
    for (int i = 0; i < layer_nans.size(); i++) {
      err += StringPrintf("(real) layer %d. %d/%d weights, %d/%d biases\n",
			  i,
			  layer_nans[i].first, layers[i].weights.size(),
			  layer_nans[i].second, layers[i].biases.size());
    }
    CHECK(false) << "[" << message << "] The network has NaNs :-(\n" << err;
  }
}


void Network::StructuralCheck() const {
  // TODO: Other checks!
  for (int i = 0; i < num_layers; i++) {
    const Layer &layer = layers[i];
    const int num_prev_nodes = num_nodes[i];
    const int num_this_nodes = num_nodes[i + 1];
    CHECK(layer.indices.size() == num_this_nodes * layer.indices_per_node);

    // Check indices are in bounds. Unsigned ints so this is just
    // the upper-bound check.
    for (const uint32 idx : layer.indices) {
      CHECK(idx < num_prev_nodes);
    }

    // If dense, check that they are the expected regular structure.
    if (layer.type == LAYER_DENSE) {
      CHECK(layer.indices_per_node == num_prev_nodes);
      for (int n = 0; n < num_this_nodes; n++) {
	for (int p = 0; p < num_prev_nodes; p++) {
	  CHECK(layer.indices[n * layer.indices_per_node + p] == p);
	}
      }
    }
  }
  
  CheckInvertedIndices();
}

void Network::CheckInvertedIndices() const {
  for (int layer = 0; layer < num_layers; layer++) {
    const vector<uint32> &indices = layers[layer].indices;
    const Network::InvertedIndices &inv = inverted_indices[layer];
    CHECK_EQ(num_nodes[layer + 1] * layers[layer].indices_per_node,
	     indices.size());
    // Need one start/length pair for every node in the source layer.
    CHECK_EQ(num_nodes[layer], inv.start.size());
    CHECK_EQ(num_nodes[layer], inv.length.size());
    // But the output size is determined by the next layer.
    CHECK_EQ(num_nodes[layer + 1] * layers[layer].indices_per_node,
	     inv.output_indices.size());
    // z is a node id from the src layer.
    for (int z = 0; z < inv.start.size(); z++) {
      // i is the index within the compacted inverted index.
      for (int i = inv.start[z]; i < inv.start[z] + inv.length[z]; i++) {
	// Global index into 'indices'.
	CHECK(i >= 0);
	CHECK(i < inv.output_indices.size());
	const int gidx = inv.output_indices[i];
	CHECK(gidx >= 0);
	CHECK(gidx < indices.size());
	// This should map back to our current node id.
	CHECK_EQ(indices[gidx], z);
      }
    }
  }
}

void Network::ComputeInvertedIndices(Network *net, int max_parallelism) {
  // Computes the values for inverted_indices[layer]. Note that
  // although we use the [layer] offset throughout, this is really
  // talking about the gap between layers, with the 0th element's
  // index being the way the first hidden layer uses the inputs, and
  // the 0th element's inverted index being about the way the inputs map
  // to the first hidden layer.
  auto OneLayer = [net](int layer) {
    CHECK_GE(layer, 0);
    CHECK_LT(layer, net->layers.size());
    const int src_num_nodes = net->num_nodes[layer];
    const int dst_num_nodes = net->num_nodes[layer + 1];
    CHECK_LT(layer, net->num_layers);
    CHECK_LT(layer, net->inverted_indices.size());
    vector<uint32> *start = &net->inverted_indices[layer].start;
    vector<uint32> *length = &net->inverted_indices[layer].length;
    // Number of nodes depends on size of source layer.
    CHECK_EQ(src_num_nodes, start->size());
    CHECK_EQ(src_num_nodes, length->size());
    vector<uint32> *inverted = &net->inverted_indices[layer].output_indices;
    // But this has to account for all the nodes on the destination layer.
    CHECK_EQ(net->layers[layer].indices_per_node * dst_num_nodes,
	     inverted->size());

    printf("ComputeInvertedIndices layer %d...\n", layer);
    fflush(stdout);
    
    // Indexed by node id in the source layer.
    vector<vector<uint32>> occurrences;
    occurrences.resize(net->num_nodes[layer]);
    for (int dst_indices_idx = 0;
	 dst_indices_idx < net->layers[layer].indices_per_node * dst_num_nodes;
	 dst_indices_idx++) {
      // This index gets put into exactly one place in occurrences.
      CHECK(dst_indices_idx < net->layers[layer].indices.size());
      const int src_nodes_idx = net->layers[layer].indices[dst_indices_idx];
      CHECK(src_nodes_idx >= 0) << src_nodes_idx;
      CHECK(src_nodes_idx < occurrences.size()) << src_nodes_idx << " vs "
						<< occurrences.size();
      occurrences[src_nodes_idx].push_back(dst_indices_idx);
    }

    printf("Sort layer %d...\n", layer);
    fflush(stdout);

    // These can be in arbitrary order, but sort each subvector, for
    // locality of access and better compression.
    for (vector<uint32> &v : occurrences) {
      std::sort(v.begin(), v.end());
    }

    printf("Flatten layer %d...\n", layer);
    fflush(stdout);

    // Now flatten.
    int flat_size = 0;
    for (int src_nodes_idx = 0;
	 src_nodes_idx < src_num_nodes;
	 src_nodes_idx++) {
      (*start)[src_nodes_idx] = flat_size;
      (*length)[src_nodes_idx] = occurrences[src_nodes_idx].size();

      for (const int val : occurrences[src_nodes_idx]) {
	(*inverted)[flat_size] = val;
	flat_size++;
      }
    }
    CHECK_EQ(dst_num_nodes * net->layers[layer].indices_per_node, flat_size);
  };

  UnParallelComp(net->num_layers, OneLayer, max_parallelism);
}

// Caller owns new-ly allocated Network object.
Network *Network::ReadNetworkBinary(const string &filename) {
  printf("Reading [%s]\n", filename.c_str());
  FILE *file = fopen(filename.c_str(), "rb");
  if (file == nullptr) {
    printf("  ... failed. If it's present, there may be a "
	   "permissions problem?\n");
    fflush(stdout);
    return nullptr;
  }

  auto Read64 = [file]() {
    int64_t i;
    CHECK(!feof(file));
    CHECK(1 == fread(&i, 8, 1, file));
    return i;
  };
  auto Read32 = [file]() {
    int32_t i;
    CHECK(!feof(file));
    CHECK(1 == fread(&i, 4, 1, file));
    return i;
  };
  auto ReadFloat = [file]() {
    float value;
    CHECK(!feof(file));
    CHECK(1 == fread(&value, 4, 1, file));
    return value;
  };
  auto ReadFloats = [&ReadFloat](vector<float> *vec) {
    for (int i = 0; i < vec->size(); i++) {
      (*vec)[i] = ReadFloat();
    }
  };

  CHECK(Read32() == Network::FORMAT_ID) << "Wrong magic number!";

  int64 round = Read64();
  int64 examples = Read64();
  // These values determine the size of the network vectors.
  int file_num_layers = Read32();
  CHECK_GE(file_num_layers, 0);
  printf("%s: %lld rounds, %lld examples, %d layers.\n",
	 filename.c_str(), round, examples, file_num_layers);
  vector<int> num_nodes(file_num_layers + 1, 0);
  printf("%s: num nodes: ", filename.c_str());
  for (int i = 0; i < file_num_layers + 1; i++) {
    num_nodes[i] = Read32();
    printf("%d ", num_nodes[i]);
  }
  printf("\n");
  
  vector<int> width, height, channels;
  vector<uint32_t> renderstyle;
  for (int i = 0; i < file_num_layers + 1; i++)
    width.push_back(Read32());
  for (int i = 0; i < file_num_layers + 1; i++)
    height.push_back(Read32());
  for (int i = 0; i < file_num_layers + 1; i++)
    channels.push_back(Read32());
  for (int i = 0; i < file_num_layers + 1; i++)
    renderstyle.push_back(Read32());

  CHECK(num_nodes.size() == width.size());
  CHECK(num_nodes.size() == height.size());
  CHECK(num_nodes.size() == channels.size());
  CHECK(num_nodes.size() == renderstyle.size());
  
  for (int i = 0; i < file_num_layers + 1; i++) {
    printf("Layer %d: %d x %d x %d (as %08x)\n",
	   i - 1, width[i], height[i], channels[i], renderstyle[i]);
  }
  
  printf("\n%s: indices per node/fns/type: ", filename.c_str());
  vector<int> indices_per_node(file_num_layers, 0);
  vector<TransferFunction> transfer_functions(file_num_layers, SIGMOID);
  vector<LayerType> layer_types(file_num_layers, LAYER_DENSE);
  for (int i = 0; i < file_num_layers; i++) {
    indices_per_node[i] = Read32();
    TransferFunction tf = (TransferFunction)Read32();
    CHECK(tf >= 0 && tf < NUM_TRANSFER_FUNCTIONS) << tf;
    transfer_functions[i] = tf;
    LayerType lt = (LayerType)Read32();
    CHECK(lt >= 0 && lt < NUM_LAYER_TYPES) << lt;
    layer_types[i] = lt;
    printf("%d %s %s ",
	   indices_per_node[i],
	   TransferFunctionName(tf),
	   LayerTypeName(lt));
  }
  printf("\n");

  std::unique_ptr<Network> net{
    new Network{num_nodes, indices_per_node, transfer_functions}};
  net->width = width;
  net->height = height;
  net->channels = channels;
  net->renderstyle = renderstyle;
  
  net->rounds = round;
  net->examples = examples;
  
  // Read Layer structs.
  for (int i = 0; i < file_num_layers; i++) {
    LayerType type = layer_types[i];
    net->layers[i].type = type;
    switch(type) {
    case LAYER_SPARSE:
      for (int j = 0; j < net->layers[i].indices.size(); j++) {
	net->layers[i].indices[j] = Read32();
      }
      break;
    case LAYER_DENSE: {
      // (layer 0 is the input layer)
      const int prev_num_nodes = net->num_nodes[i];
      const int num_nodes = net->num_nodes[i + 1];
      CHECK_EQ(net->layers[i].indices.size(), prev_num_nodes * num_nodes) <<
	"For a dense layer, indices per node should be the size of "
	"the previous layer! prev * cur: " << prev_num_nodes <<
	" * " << num_nodes << " = " << prev_num_nodes * num_nodes <<
	" but got " << net->layers[i].indices.size();
      int64 offset = 0;
      for (int n = 0; n < num_nodes; n++) {
	for (int p = 0; p < prev_num_nodes; p++) {
	  net->layers[i].indices[offset] = p;
	  offset++;
	}
      }
      break;
    }
    default:
      CHECK(false) << "Unsupported layer type " << type;
      break;
    }
    ReadFloats(&net->layers[i].weights);
    ReadFloats(&net->layers[i].biases);
  }

  fclose(file);
  printf("Read from %s.\n", filename.c_str());

  // Now, fill in the inverted indices. These are not stored in the file.

  printf("Invert index:\n");
  ComputeInvertedIndices(net.get());
  printf("Check it:\n");
  net->StructuralCheck();
  // CheckInvertedIndices(*net);

  return net.release();
}

void Network::SaveNetworkBinary(const Network &net,
				const string &filename) {
  // Not portable, obviously.
  FILE *file = fopen(filename.c_str(), "wb");
  auto Write64 = [file](int64_t i) {
    CHECK(1 == fwrite(&i, 8, 1, file));
  };
  auto Write32 = [file](int32_t i) {
    CHECK(1 == fwrite(&i, 4, 1, file));
  };
  auto WriteFloat = [file](float value) {
    CHECK(1 == fwrite(&value, 4, 1, file));
  };
  auto WriteFloats = [&WriteFloat](const vector<float> &vec) {
    for (float f : vec)
      WriteFloat(f);
  };

  Write32(Network::FORMAT_ID);
  Write64(net.rounds);
  Write64(net.examples);
  Write32(net.num_layers);
  CHECK(net.num_nodes.size() == net.num_layers + 1);
  CHECK(net.width.size() == net.num_layers + 1) << net.width.size();
  CHECK(net.height.size() == net.num_layers + 1);
  CHECK(net.channels.size() == net.num_layers + 1);
  CHECK(net.renderstyle.size() == net.num_layers + 1);
  
  for (const int i : net.num_nodes) Write32(i);
  for (const int w : net.width) Write32(w);
  for (const int h : net.height) Write32(h);
  for (const int c : net.channels) Write32(c);
  for (const uint32 s : net.renderstyle) Write32(s);
  
  for (const Network::Layer &layer : net.layers) {
    Write32(layer.indices_per_node);
    Write32(layer.transfer_function);
    Write32(layer.type);
  }

  for (const Network::Layer &layer : net.layers) {
    switch (layer.type) {
    case LAYER_SPARSE:
      for (const uint32 idx : layer.indices) Write32(idx);
      break;
    case LAYER_DENSE:
      // Don't write dense layers; the structure is computable.
      break;
    default:
      CHECK(false) << "Unknown layer type!";
    }
    WriteFloats(layer.weights);
    WriteFloats(layer.biases);
  }

  // Inverted indices are not written.
  printf("Wrote %s.\n", filename.c_str());
  fclose(file);
}
