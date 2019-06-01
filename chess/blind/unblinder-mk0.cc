// This is a checkpoint of the unblinder model that I ran for 140k+
// rounds. It can load from disk and run purely on the CPU, so is
// fairly simple, and matches the Unblinder interface.

// Note that as of r3805 (6 Jan 2019) it actually matches the Network
// struct from network.h; it could just be using that or a tag of it.
// Not clear whether it's better to have it sealed within this interface,
// shared with head (and keep it working) or have network-mk0.cc, or
// what.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <cmath>
#include <algorithm>
#include <tuple>
#include <utility>
#include <set>
#include <vector>
#include <map>

#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/util.h"
#include "../cc-lib/vector-util.h"
#include "../cc-lib/base/macros.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/gtl/top_n.h"
#include "../chess.h"

#include "unblinder.h"
#include "unblinder-mk0.h"

using namespace std;

using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;

using uint32 = uint32_t;
using uint64 = uint64_t;

using Move = Position::Move;

static constexpr bool VERBOSE = false;

namespace {

#define NUM_CONTENTS 13
static constexpr int kPieceToContents[16] = {
  // 0 = empty in both representations.
  0,
  // White first (MSB not set).
  1, // PAWN = 1,
  2, // KNIGHT = 2,
  3, // BISHOP = 3,
  4, // ROOK = 4,
  5, // QUEEN = 5,
  6, // KING = 6,
  4, // C_ROOK = 7,
  // black empty is invalid, but we can just
  // treat it as empty.
  0,
  7, // PAWN = 1,
  8, // KNIGHT = 2,
  9, // BISHOP = 3,
  10, // ROOK = 4,
  11, // QUEEN = 5,
  12, // KING = 6,
  10, // C_ROOK = 7,
};

static constexpr int kContentsToPiece[NUM_CONTENTS] = {
  Position::EMPTY,
  Position::WHITE | Position::PAWN,
  Position::WHITE | Position::KNIGHT,
  Position::WHITE | Position::BISHOP,
  Position::WHITE | Position::ROOK,
  Position::WHITE | Position::QUEEN,
  Position::WHITE | Position::KING,
  Position::BLACK | Position::PAWN,
  Position::BLACK | Position::KNIGHT,
  Position::BLACK | Position::BISHOP,
  Position::BLACK | Position::ROOK,
  Position::BLACK | Position::QUEEN,
  Position::BLACK | Position::KING,
};

// The input is a 64-bit number, represented as an 8x8 matrix of
// floats.
//
// In the output, there are several ways we could do it. One very
// simple thing would be to have 64 floats and then discretize so that
// [0, 1/7) is empty, [1/7, 2/7) is pawn, [2/7, 3/7) is knight, etc.
// The main problem with this is the desire for continuity; is a pawn
// really "almost" a knight in any way? So instead, each output is 13
// different floats (6 per color + 1 neutral empty); only the correct
// one (e.g. EMPTY) gets filled with 1.0 and the remainder are zeroes.
//
// In addition to the 64 * 7 floats in the output layer, we also have
// a single float representing the current turn (0 = white, 1 = black)
// four floats for the four castling states (1 = allowed, 0 = not).
// Ignoring en passant for now.

static constexpr int OUTPUT_LAYER_SIZE =
  // For each square, one probability for each of the 13 possible contents.
  13 * 64 +
  // Four castling probabilities.
  4 +
  // Current turn.
  1;

enum class RenderStyle {
  RGB,
  FLAT,
  CHESSBITS,
  CHESSBOARD,
};

enum TransferFunction {
  SIGMOID = 0,
  RELU,
  LEAKY_RELU,
  NUM_TRANSFER_FUNCTIONS,
};

static const char *TransferFunctionName(TransferFunction tf) {
  switch (tf) {
  case SIGMOID: return "SIGMOID";
  case RELU: return "RELU";
  case LEAKY_RELU: return "LEAKY_RELU";
  default: return "??INVALID??";
  }
}

#if 0
struct NetworkConfiguration { 

  // Note that these must have num_layers + 1 entries.
  // The number of nodes in each layer is width * height * channels.
  // Channels tells us how many nodes there are per logical pixel;
  // for color output it's typical that this is 3. All that channels
  // really do is tell us how to align inputs when creating the
  // node indices during initialization. (And of course in the
  // input and output layers, it's expected that the channels will
  // be initialized/read in some meaningful way.)

  // Note that the last layer is given as a flat vector, which makes spatial heuristics for
  // index assignment fail; this layer needs to be highly connected.

  const int num_layers = 4;
  const vector<int> width =    { 8,  32,  64, 9, OUTPUT_LAYER_SIZE, };
  const vector<int> height =   { 8,  32,  64, 9, 1, };
  const vector<int> channels = { 1,   1,   3, 7, 1, };
  const vector<int> indices_per_node =
    {     64, 1024, 12288, 9 * 9 * 7, };

  const vector<TransferFunction> transfer_functions = { 
    LEAKY_RELU,
    LEAKY_RELU,
    LEAKY_RELU,
    LEAKY_RELU,
  };

  const vector<RenderStyle> style = {
    RenderStyle::CHESSBITS,
    RenderStyle::RGB,
    RenderStyle::RGB,
    RenderStyle::FLAT,
    RenderStyle::CHESSBOARD, 
  };

  
  // num_nodes = width * height * channels
  // //  indices_per_node = indices_per_channel * channels
  // //  vector<int> indices_per_node;
  vector<int> num_nodes;
  NetworkConfiguration() {
    CHECK_EQ(width.size(), height.size());
    CHECK_EQ(width.size(), style.size());
    CHECK_EQ(num_layers + 1, height.size());
    CHECK_EQ(num_layers, indices_per_node.size());
    for (int i = 0; i < num_layers + 1; i++) {
      CHECK(width[i] >= 1);
      CHECK(height[i] >= 1);
      CHECK(channels[i] >= 1);
      num_nodes.push_back(width[i] * height[i] * channels[i]);
    }
  }
};
#endif

struct Network {
  // Creates arrays of the appropriate size, but all zeroes. Note that this uninitialized
  // network is invalid, since the inverted indices are not correct.
  Network(vector<int> num_nodes,
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

  int64 Bytes() const {
    int64 ret = sizeof *this;
    ret += sizeof num_nodes[0] * num_nodes.size();
    ret += sizeof width[0] * width.size();
    ret += sizeof height[0] * height.size();
    ret += sizeof channels[0] * channels.size();
    // Layer structs.
    for (int i = 0; i < num_layers; i++) {
      ret += sizeof layers[i] + sizeof layers[i].indices[0] * layers[i].indices.size() +
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

  void CopyFrom(const Network &other) {
    CHECK_EQ(this->num_layers, other.num_layers);
    this->num_nodes = other.num_nodes;
    this->width = other.width;
    this->height = other.height;
    this->channels = other.channels;
    this->layers = other.layers;
    this->inverted_indices = other.inverted_indices;
  }

  void NaNCheck(const char *message) const {
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
  
  // Just used for serialization. Whenever changing the interpretation
  // of the data in an incomplete way, please change.
  static constexpr uint32 FORMAT_ID = 0x2700072DU;

  // The number of "real" layers, that is, not counting the input.
  const int num_layers;

  // num_layers + 1. num_nodes[0] is the size of the input layer.
  vector<int> num_nodes;
  // Parallel to num_nodes. These don't affect the network's behavior,
  // just its rendering. num_nodes[i] == width[i] * height[i] * channels[i].
  vector<int> width, height, channels;
  
  // "Real" layer; none for the input.
  struct Layer {
    // Same number of input indices for each node.
    int indices_per_node;
    // The transfer function used to compute the output from the
    // input indices.
    TransferFunction transfer_function;
    // indices_per_node * num_nodes[l + 1], flat, node-major
    vector<uint32> indices;
    // indices_per_node * num_nodes[l + 1]; Parallel to indices.
    vector<float> weights;
    // num_nodes[l + 1]. One per node.
    vector<float> biases;
  };

  struct InvertedIndices {
    // For a given node, where do I output to in the next layer?
    // Note that nodes don't all have the same number of outputs.
    // This is a packed structure to facilitate GPU operations.
    //
    // For a given node, where do my output indices start in
    // the indices array, and how many are there?
    // num_nodes[i]
    vector<uint32> start;
    vector<uint32> length;

    // Packed array of indices. Since every node on the next layer has
    // exactly layers[l].indices_per_node inputs, this will be of size
    // layers[l].indices_per_node * num_nodes[l + 1]. However, any
    // given node on this layer may be used more or fewer times.
    //
    // The value here gives the index into the indices/weights vectors
    // for the next layer. If for each index i within the span (defined
    // by inverted_indices[layer].start[z]) for node id z
    // let gidx = inverted_indices[layer].output_indices[i]
    // and then layers[layer].indices[gidx] == z. (The same for the weight
    // vector gives us the weight, which is the point, and dividing
    // by INDICES_PER_NODE gives us the output node.) As such, this is
    // a permutation of 0..(num_nodes[ii] * layers[ii].indices_per_node - 1).
    vector<uint32> output_indices;
  };

  // num_layers
  vector<Layer> layers;
  // There are also num_layers of these, but be careful about the
  // offset. The 0th inverted index is about the gap between the input
  // layer (otherwise not represented in the network, except for its
  // size in num_nodes[0]) and the first hidden layer. The last one is
  // about the last gap, not the output layer, since the output layer
  // is not indexed by anything.
  vector<InvertedIndices> inverted_indices;

  // Rounds trained. This matters when restarting from disk, because
  // for example the learning rate depends on the round.
  int64 rounds = 0;
  // Total number of training examples processed.
  int64 examples = 0;
};

static void CheckInvertedIndices(const Network &net) {
  for (int layer = 0; layer < net.num_layers; layer++) {
    const vector<uint32> &indices = net.layers[layer].indices;
    const Network::InvertedIndices &inv = net.inverted_indices[layer];
    CHECK_EQ(net.num_nodes[layer + 1] * net.layers[layer].indices_per_node,
	     indices.size());
    // Need one start/length pair for every node in the source layer.
    CHECK_EQ(net.num_nodes[layer], inv.start.size());
    CHECK_EQ(net.num_nodes[layer], inv.length.size());
    // But the output size is determined by the next layer.
    CHECK_EQ(net.num_nodes[layer + 1] * net.layers[layer].indices_per_node,
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

static void ComputeInvertedIndices(Network *net) {
  // Computes the values for inverted_indices[layer]. Note that
  // although we use the [layer] offset throughout, this is really
  // talking about the gap between layers, with the 0th element's
  // index being the way the first hidden layer uses the inputs, and
  // the 0th element's inverted index being about the way the inputs map
  // to the first hidden layer.
  auto OneLayer = [net](int layer) {
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
    CHECK_EQ(net->layers[layer].indices_per_node * dst_num_nodes, inverted->size());

    // Indexed by node id in the source layer.
    vector<vector<uint32>> occurrences;
    occurrences.resize(net->num_nodes[layer]);
    for (int dst_indices_idx = 0;
	 dst_indices_idx < net->layers[layer].indices_per_node * dst_num_nodes;
	 dst_indices_idx++) {
      // This index gets put into exactly one place in occurrences.
      const int src_nodes_idx = net->layers[layer].indices[dst_indices_idx];
      occurrences[src_nodes_idx].push_back(dst_indices_idx);
    }

    // These can be in arbitrary order, but sort each subvector, for
    // locality of access and better compression.
    for (vector<uint32> &v : occurrences) {
      std::sort(v.begin(), v.end());
    }

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

  ParallelComp(net->num_layers, OneLayer, 12);
}

// Caller owns new-ly allocated Network object.
static Network *ReadNetworkBinary(const string &filename) {
  if (VERBOSE) printf("Reading [%s]\n", filename.c_str());
  FILE *file = fopen(filename.c_str(), "rb");
  if (file == nullptr) {
    if (VERBOSE) printf("  ... failed. If it's present, there may be a "
			"permissions problem?\n");
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
  if (VERBOSE) printf("%s: %d layers.\n", filename.c_str(), file_num_layers);
  vector<int> num_nodes(file_num_layers + 1, 0);
  if (VERBOSE) printf("%s: num nodes: ", filename.c_str());
  for (int i = 0; i < file_num_layers + 1; i++) {
    num_nodes[i] = Read32();
    if (VERBOSE) printf("%d ", num_nodes[i]);
  }

  vector<int> width, height, channels;
  for (int i = 0; i < file_num_layers + 1; i++)
    width.push_back(Read32());
  for (int i = 0; i < file_num_layers + 1; i++)
    height.push_back(Read32());
  for (int i = 0; i < file_num_layers + 1; i++)
    channels.push_back(Read32());
  CHECK(num_nodes.size() == width.size());
  CHECK(num_nodes.size() == height.size());
  CHECK(num_nodes.size() == channels.size());

  // printf("\n%s: indices per node/fns: ", filename.c_str());
  vector<int> indices_per_node(file_num_layers, 0);
  vector<TransferFunction> transfer_functions(file_num_layers, SIGMOID);
  for (int i = 0; i < file_num_layers; i++) {
    indices_per_node[i] = Read32();
    TransferFunction tf = (TransferFunction)Read32();
    CHECK(tf >= 0 && tf < NUM_TRANSFER_FUNCTIONS) << tf;
    transfer_functions[i] = tf;
    if (VERBOSE) printf("%d %s ",
			indices_per_node[i],
			TransferFunctionName(tf));
  }
  if (VERBOSE) printf("\n");

  std::unique_ptr<Network> net{new Network{num_nodes, indices_per_node, transfer_functions}};
  net->width = width;
  net->height = height;
  net->channels = channels;
  
  net->rounds = round;
  net->examples = examples;
  
  // Read Layer structs.
  for (int i = 0; i < file_num_layers; i++) {
    for (int j = 0; j < net->layers[i].indices.size(); j++) {
      net->layers[i].indices[j] = Read32();
    }
    ReadFloats(&net->layers[i].weights);
    ReadFloats(&net->layers[i].biases);
  }

  fclose(file);
  if (VERBOSE) printf("Read from %s.\n", filename.c_str());

  // Now, fill in the inverted indices. These are not stored in the file.

  if (VERBOSE) printf("Invert index:\n");
  ComputeInvertedIndices(net.get());
  if (VERBOSE) printf("Check it:\n");
  CheckInvertedIndices(*net);

  return net.release();
}

#if 0
static void SaveNetworkBinary(const Network &net, const string &filename) {
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
  for (const int i : net.num_nodes) Write32(i);
  for (const int w : net.width) Write32(w);
  for (const int h : net.height) Write32(h);
  for (const int c : net.channels) Write32(c);
  
  for (const Network::Layer &layer : net.layers) {
    Write32(layer.indices_per_node);
    Write32(layer.transfer_function);
  }

  for (const Network::Layer &layer : net.layers) {
    for (const uint32 idx : layer.indices) Write32(idx);
    WriteFloats(layer.weights);
    WriteFloats(layer.biases);
  }

  // Inverted indices are not written.
  printf("Wrote %s.\n", filename.c_str());
  fclose(file);
}
#endif

// A stimulation is an evaluation (perhaps an in-progress one) of a
// network on a particular input; when it's complete we have the
// activation value of each node on each layer, plus the input itself.
struct Stimulation {
  explicit Stimulation(const Network &net) : num_layers(net.num_layers),
					     num_nodes(net.num_nodes) {
    values.resize(num_layers + 1);
    for (int i = 0; i < values.size(); i++)
      values[i].resize(num_nodes[i], 0.0f);
  }
  int64 Bytes() const {
    int64 ret = sizeof *this;
    for (int i = 0; i < values.size(); i++) {
      ret += sizeof values[i] + sizeof values[i][0] * values[i].size();
    }
    return ret;
  }

  // Same as in Network.
  const int num_layers;
  // num_layers + 1
  const vector<int> num_nodes;

  // Keep track of what's actually been computed?

  // Here the outer vector has size num_layers + 1; first is the input.
  // Inner vector has size num_nodes[i], and just contains their output values.
  vector<vector<float>> values;

  void CopyFrom(const Stimulation &other) {
    CHECK_EQ(this->num_layers, other.num_layers);
    CHECK_EQ(this->num_nodes.size(), other.num_nodes.size());
    for (int i = 0; i < this->num_nodes.size(); i++) {
      CHECK_EQ(this->num_nodes[i], other.num_nodes[i]);
    }
    this->values = other.values;
  }

  void NaNCheck(const char *message) const {
    bool has_nans = false;
    vector<int> layer_nans;
    for (const vector<float> &layer : values) {
      int v = 0;
      for (float f : layer) if (std::isnan(f)) v++;
      layer_nans.push_back(v);
      if (v > 0) has_nans = true;
    }
    if (has_nans) {
      string err;
      for (int i = 0; i < layer_nans.size(); i++) {
	err += StringPrintf("stim layer %d. %d/%d values\n",
			    i,
			    layer_nans[i], values[i].size());
      }
      CHECK(false) << "[" << message << "] The stimulation has NaNs :-(\n" << err;
    }
  }
};

struct UnblinderMk0Impl : public Unblinder {
  string ModelInfo() const override {
    return StringPrintf("%lld rounds, %lld examples",
			net->rounds,
			net->examples);
  }

  void Stimulate(uint64 bits, Stimulation *stim) const {
    // Initialize input layer.
    Unblinder::Layer64(bits, &stim->values[0]);

    // PERF: For this size of network we probably benefit from parallelizing a little...
    for (int src = 0; src < net->num_layers; src++) {

      // Note: this one is hard coded, since we know it's the function used throughout
      CHECK(net->layers[src].transfer_function == TransferFunction::LEAKY_RELU);
      auto Forward =
	[](double potential) -> float {
	  return (potential < 0.0f) ? potential * 0.01f : potential;
	};

      const vector<float> &src_values = stim->values[src];
      vector<float> *dst_values = &stim->values[src + 1];
      const vector<float> &biases = net->layers[src].biases;
      const vector<float> &weights = net->layers[src].weights;
      const vector<uint32> &indices = net->layers[src].indices;
      const int indices_per_node = net->layers[src].indices_per_node;
      const int num_nodes = net->num_nodes[src + 1];
      for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
	// Start with bias.
	double potential = biases[node_idx];
	const int my_weights = node_idx * indices_per_node;
	const int my_indices = node_idx * indices_per_node;

	for (int i = 0; i < indices_per_node; i++) {
	  const float w = weights[my_weights + i];
	  const int srci = indices[my_indices + i];
	  const float v = src_values[srci];
	  potential += w * v;
	}
	const float out = Forward(potential);
	(*dst_values)[node_idx] = out;
      }
    }
  }
  
  Position Unblind(bool single_king, uint64 bits) const override {
    Stimulation stim{*net};
    Stimulate(bits, &stim);
    // Now the final layer in the stimulation reflects our prediction.
    return PositionFromLayer(single_king, stim.values.back());   
  }

  static Position PositionFromLayer(bool single_king,
				    const vector<float> &layer) {
    CHECK(layer.size() == OUTPUT_LAYER_SIZE);
    Position ret;

    static constexpr int WKING = 6;
    static constexpr int BKING = 12;
    
    // If single_king mode is set, populate these with the most likely
    // overall row/col for the white/black kings.
    int wkr = -1, wkc = -1;
    int bkr = -1, bkc = -1;
    if (single_king) {
      struct King {
	King(int r, int c, float p) : r(r), c(c), p(p) {}
	int r = 0;
	int c = 0;
	float p = 0.0;
      };

      struct KingCmp {
	bool operator()(const King &a, const King &b) {
	  return a.p > b.p;
	};
      };

      // Best scores for each. Note the subtlety that the highest
      // score might occur for both kings on the same square, but we
      // can't assign both of them to it. So we keep the top two.
      gtl::TopN<King, KingCmp> top_white(2);
      gtl::TopN<King, KingCmp> top_black(2);
      
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  // Find the contents with the highest score.
	  const int cidx = (r * 8 + c) * NUM_CONTENTS;
	  top_white.push(King(r, c, layer[cidx + WKING]));
	  top_black.push(King(r, c, layer[cidx + BKING]));
	}
      }

      std::unique_ptr<vector<King>> whites{top_white.Extract()};
      std::unique_ptr<vector<King>> blacks{top_black.Extract()};
      CHECK(whites->size() == 2);
      CHECK(blacks->size() == 2);

      int wi = 0, bi = 0;
      if ((*whites)[wi].r == (*blacks)[bi].r &&
	  (*whites)[wi].c == (*blacks)[bi].c) {
	// Tricky case where they are both predicted to be in the
	// same spot.
	if ((*whites)[wi].p > (*blacks)[bi].p) {
	  // White wins; move black to its second prediction.
	  bi++;
	} else {
	  wi++;
	}
      }
      
      wkr = (*whites)[wi].r;
      wkc = (*whites)[wi].c;
      bkr = (*blacks)[bi].r;
      bkc = (*blacks)[bi].c;
    }
    
    for (int r = 0; r < 8; r++) {
      for (int c = 0; c < 8; c++) {

	auto MostLikelyPiece =
	  [single_king](const vector<float> &vals, int start_idx) {
	    int maxi = 0;
	    float maxp = vals[start_idx];
	    static_assert(BKING != 0 && WKING != 0);
	    for (int i = 1; i < NUM_CONTENTS; i++) {
	      // Don't ever choose kings in single_king mode.
	      if (single_king && (i == BKING || i == WKING))
		continue;
	      
	      if (vals[start_idx + i] > maxp) {
		maxi = i;
		maxp = vals[start_idx + i];
	      }
	    }
	    return maxi;
	  };

	if (r == wkr && c == wkc) {
	  ret.SetPiece(r, c, Position::WHITE | Position::KING);
	} else if (r == bkr && c == bkc) {
	  ret.SetPiece(r, c, Position::BLACK | Position::KING);
	} else {
	  // Find the contents with the highest score.
	  int cidx = (r * 8 + c) * NUM_CONTENTS;
	  const int maxi = MostLikelyPiece(layer, cidx);
	  uint8 piece = kContentsToPiece[maxi];
	  ret.SetPiece(r, c, piece);
	}
      }
    }

    // Castling flags.
    bool bqf = layer[64 * NUM_CONTENTS + 0] > 0.5f;
    bool bkf = layer[64 * NUM_CONTENTS + 1] > 0.5f;
    bool wqf = layer[64 * NUM_CONTENTS + 2] > 0.5f;
    bool wkf = layer[64 * NUM_CONTENTS + 3] > 0.5f;

    // Only set castling if it would keep the board legal.
    if (ret.PieceAt(0, 4) == (Position::BLACK | Position::KING)) {
      if (bqf && ret.PieceAt(0, 0) == (Position::BLACK | Position::ROOK)) {
	ret.SetPiece(0, 0, Position::BLACK | Position::C_ROOK);
      }
      if (bkf && ret.PieceAt(0, 7) == (Position::BLACK | Position::ROOK)) {
	ret.SetPiece(0, 7, Position::BLACK | Position::C_ROOK);
      }
    }

    if (ret.PieceAt(7, 4) == (Position::WHITE | Position::KING)) {
      if (wqf && ret.PieceAt(7, 0) == (Position::WHITE | Position::ROOK)) {
	ret.SetPiece(7, 0, Position::WHITE | Position::C_ROOK);
      }
      if (wkf && ret.PieceAt(7, 7) == (Position::WHITE | Position::ROOK)) {
	ret.SetPiece(7, 7, Position::WHITE | Position::C_ROOK);
      }
    }

    // Whose move?
    bool black_move = layer[64 * NUM_CONTENTS + 4] > 0.5f;
    ret.SetBlackMove(black_move);

    return ret;
  }

  std::unique_ptr<Network> net;
};

}  // namespace

Unblinder *UnblinderMk0::LoadFromFile(const string &filename) {
  UnblinderMk0Impl *ub = new UnblinderMk0Impl;
  ub->net.reset(ReadNetworkBinary(filename));
  return ub;
}
