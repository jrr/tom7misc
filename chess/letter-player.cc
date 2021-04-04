
#include <string>
#include <memory>
#include <cstdint>

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "player.h"
#include "chess.h"
#include "player-util.h"
#include "threadutil.h"
#include "image.h"

using int64 = int64_t;

using Move = Position::Move;
using namespace std;

static constexpr bool VERBOSE = false;

namespace {

// Bunch of code copied here from ../lowercase, cloned to keep it
// as hermtic as possible.

/// ----- network.h -----
enum TransferFunction {
  SIGMOID = 0,
  RELU = 1,
  LEAKY_RELU = 2,

  NUM_TRANSFER_FUNCTIONS,
};

enum LayerType {
  // Every node takes input from every node in the previous layer.
  // (indices_per_node = size of the previous layer). This is the most
  // expressive setup, but also the largest. Training and prediction
  // can be more efficient (per weight) because of the regular
  // structure.
  LAYER_DENSE = 0,
  // Explicitly specify the input nodes. Every node has the same
  // number of inputs. Some overhead to store these indices. Really
  // the only option for large layers, though.
  LAYER_SPARSE = 1,
  // TODO: Convolutional

  NUM_LAYER_TYPES,
};

// How to draw a layer's stimulations in UIs. Has no effect in network
// code itself. (If we standardize the rendering code, this enum should
// go with that.)
enum RenderStyle : uint32_t {
  // One pixel per channel.
  RENDERSTYLE_FLAT = 0,
  // Assign channels as RGB. Makes most sense when there
  // are three channels.
  RENDERSTYLE_RGB = 1,

  // Rest of the range is reserved for users.
  RENDERSTYLE_USER = 0xF0000000,
};

const char *TransferFunctionName(TransferFunction tf);

struct Stimulation;

struct Network {
  template<class T> using vector = std::vector<T>;
  using string = std::string;

  // Creates arrays of the appropriate size, but all zeroes. Note that
  // this uninitialized network is invalid, since the inverted indices
  // are not correct.
  Network(vector<int> num_nodes,
          vector<int> indices_per_node,
          vector<TransferFunction> transfer_functions);

  // Size of network in RAM. Note that this includes the indices
  // and inverted indices for dense layers (which are indeed still stored)
  // even though they are not used or represented on disk.
  int64_t Bytes() const;

  void CopyFrom(const Network &other) {
    CHECK_EQ(this->num_layers, other.num_layers);
    this->num_nodes = other.num_nodes;
    this->width = other.width;
    this->height = other.height;
    this->channels = other.channels;
    this->renderstyle = other.renderstyle;
    this->layers = other.layers;
    this->inverted_indices = other.inverted_indices;
    this->rounds = other.rounds;
    this->examples = other.examples;
  }

  // Check for structural well-formedness (layers are the right size;
  // indices are in bounds; dense layers have the expected regular
  // structure; inverted indices are correct). Aborts if something is
  // wrong. Doesn't check weight values (see NaNCheck).
  void StructuralCheck() const;
  // Check the inverted indices specifically. Maybe can just
  // be private.
  void CheckInvertedIndices() const;

  // Note: These use local byte order, so the serialized format is not
  // portable.
  static Network *ReadNetworkBinary(const string &filename);
  static void SaveNetworkBinary(const Network &net, const string &filename);

  // If the number of nodes or indices per node change, this can be
  // used to reallocate the inverted index buffers; then you must call
  // ComputeInvertedIndices to put the network in a valid state.
  void ReallocateInvertedIndices();
  static void ComputeInvertedIndices(Network *net, int max_parallelism = 8);

  // Run the network to fill out the stimulation. The Stimulation
  // must be the right size (i.e. created from this Network) and
  // the input layer should be filled.
  void RunForward(Stimulation *stim) const;
  // Same, but only one layer. src_layer is the input layer.
  void RunForwardLayer(Stimulation *stim, int src_layer) const;
  // Same, but print lots of garbage and abort if a NaN is encountered
  // at any point.
  void RunForwardVerbose(Stimulation *stim) const;

  // Just used for serialization. Whenever changing the interpretation
  // of the data in an incomplete way, please change.
  static constexpr uint32_t FORMAT_ID = 0x27000730U;

  // The number of "real" layers, that is, not counting the input.
  const int num_layers;

  // num_layers + 1. num_nodes[0] is the size of the input layer.
  vector<int> num_nodes;
  // Parallel to num_nodes. These don't affect the network's behavior,
  // just its rendering. num_nodes[i] == width[i] * height[i] * channels[i].
  vector<int> width, height, channels;
  // Same, but a hint to the UI about how to render. Normal for this
  // to contain values outside the enum (i.e. in USER_RENDERSTYLE range).
  vector<uint32_t> renderstyle;

  // "Real" layer; none for the input.
  struct Layer {
    // Same number of input indices for each node.
    int indices_per_node = 0;
    // The transfer function used to compute the output from the
    // input indices.
    TransferFunction transfer_function = LEAKY_RELU;
    // Whether the layer is sparse or dense. We currently still store
    // the indices for dense layers, but they can also just be deduced
    // from the dimensions. (PERF: Save the ram?)
    LayerType type = LAYER_SPARSE;
    // indices_per_node * num_nodes[l + 1], flat, node-major
    // If type = LAYER_DENSE, then for n < num_nodes[l + 1],
    // indices[n * indices_per_node + i] = i.
    vector<uint32_t> indices;
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
    vector<uint32_t> start;
    vector<uint32_t> length;

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
    vector<uint32_t> output_indices;
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
  int64_t rounds = 0;
  // Total number of training examples processed.
  int64_t examples = 0;

private:
  // Value type, but require calling Clone explicitly.
  Network(const Network &other) = default;
};

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

  // Empty, useless stimulation, but can be used to initialize
  // vectors, etc.
  Stimulation() : num_layers(0) {}
  Stimulation(const Stimulation &other) = default;

  int64_t Bytes() const;

  // TODO: would be nice for these to be const, but then we can't have an
  // assignment operator.
  // Same as in Network.
  int num_layers;
  // num_layers + 1
  std::vector<int> num_nodes;

  // Keep track of what's actually been computed?

  // Here the outer vector has size num_layers + 1; first is the input.
  // Inner vector has size num_nodes[i], and just contains their output values.
  std::vector<std::vector<float>> values;

  void CopyFrom(const Stimulation &other) {
    CHECK_EQ(this->num_layers, other.num_layers);
    CHECK_EQ(this->num_nodes.size(), other.num_nodes.size());
    for (int i = 0; i < this->num_nodes.size(); i++) {
      CHECK_EQ(this->num_nodes[i], other.num_nodes[i]);
    }
    this->values = other.values;
  }
};


// ------- network.cc --------

using namespace std;

using uint32 = uint32_t;
using int64 = int64_t;

const char *TransferFunctionName(TransferFunction tf) {
  switch (tf) {
  case SIGMOID: return "SIGMOID";
  case RELU: return "RELU";
  case LEAKY_RELU: return "LEAKY_RELU";
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

  ReallocateInvertedIndices();
}

void Network::ReallocateInvertedIndices() {
  inverted_indices.resize(num_layers);
  for (int i = 0; i < num_layers; i++) {
    InvertedIndices &ii = inverted_indices[i];
    ii.start.resize(num_nodes[i], 0);
    ii.length.resize(num_nodes[i], 0);
    ii.output_indices.resize(layers[i].indices_per_node * num_nodes[i + 1], 0);
  }
}

void Network::RunForward(Stimulation *stim) const {
  for (int src = 0; src < num_layers; src++) {
    RunForwardLayer(stim, src);
  }
}

void Network::RunForwardLayer(Stimulation *stim, int src_layer) const {
  // PERF avoid dispatching on every node
  const TransferFunction transfer_function =
    layers[src_layer].transfer_function;
  auto Forward =
    [transfer_function](float potential) -> float {
      switch (transfer_function) {
      case SIGMOID:
        return 1.0f / (1.0f + expf(-potential));
      case RELU:
        return (potential < 0.0f) ? 0.0f : potential;
      case LEAKY_RELU:
        return (potential < 0.0f) ? potential * 0.01f : potential;
      default:
        CHECK(false) << "Unimplemented transfer function " <<
          TransferFunctionName(transfer_function);
        return 0.0f;
      }
    };

  const vector<float> &src_values = stim->values[src_layer];
  vector<float> *dst_values = &stim->values[src_layer + 1];
  const vector<float> &biases = layers[src_layer].biases;
  const vector<float> &weights = layers[src_layer].weights;
  const vector<uint32> &indices = layers[src_layer].indices;
  const int indices_per_node = layers[src_layer].indices_per_node;
  const int number_of_nodes = num_nodes[src_layer + 1];

  // PERF in parallel
  for (int node_idx = 0; node_idx < number_of_nodes; node_idx++) {
    // Start with bias.
    float potential = biases[node_idx];
    const int my_weights = node_idx * indices_per_node;
    const int my_indices = node_idx * indices_per_node;

    // PERF could support dense layers more efficiently
    for (int i = 0; i < indices_per_node; i++) {
      const float w = weights[my_weights + i];
      int srci = indices[my_indices + i];
      const float v = src_values[srci];
      potential += w * v;
    }

    float out = Forward(potential);
    (*dst_values)[node_idx] = out;
  }
}

void Network::StructuralCheck() const {
  // TODO: Other checks!
  CHECK(layers.size() == num_layers);
  CHECK(width.size() == num_layers + 1);
  CHECK(height.size() == num_layers + 1);
  CHECK(channels.size() == num_layers + 1);
  CHECK(renderstyle.size() == num_layers + 1);

  CHECK(inverted_indices.size() == num_layers);

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

    // printf("ComputeInvertedIndices layer %d...\n", layer);
    // fflush(stdout);

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

    // printf("Sort layer %d...\n", layer);
    // fflush(stdout);

    // These can be in arbitrary order, but sort each subvector, for
    // locality of access and better compression.
    for (vector<uint32> &v : occurrences) {
      std::sort(v.begin(), v.end());
    }

    // printf("Flatten layer %d...\n", layer);
    // fflush(stdout);

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
  FILE *file = fopen(filename.c_str(), "rb");
  if (file == nullptr) {
    printf("Reading %s failed. If it's present, there may be a "
           "permissions problem?\n", filename.c_str());
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
  // printf("%s: %lld rounds, %lld examples, %d layers.\n",
  // filename.c_str(), round, examples, file_num_layers);
  vector<int> num_nodes(file_num_layers + 1, 0);
  // printf("%s: num nodes: ", filename.c_str());
  for (int i = 0; i < file_num_layers + 1; i++) {
    num_nodes[i] = Read32();
    // printf("%d ", num_nodes[i]);
  }
  // printf("\n");

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

  for (int w : width) CHECK(w > 0);
  for (int h : height) CHECK(h > 0);
  for (int c : channels) CHECK(c > 0);

  /*
  for (int i = 0; i < file_num_layers + 1; i++) {
    printf("Layer %d: %d x %d x %d (as %08x)\n",
           i - 1, width[i], height[i], channels[i], renderstyle[i]);
  }
  */

  // printf("\n%s: indices per node/fns/type: ", filename.c_str());
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
    /*
    printf("%d %s %s ",
           indices_per_node[i],
           TransferFunctionName(tf),
           LayerTypeName(lt));
    */
  }
  // printf("\n");

  std::unique_ptr<Network> net{
    new Network{num_nodes, indices_per_node, transfer_functions}};
  net->width = width;
  net->height = height;
  net->channels = channels;
  net->renderstyle = renderstyle;

  net->rounds = round;
  net->examples = examples;

  int64 large_weights = 0, large_biases = 0;

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

    static constexpr float LARGE_WEIGHT = 8.0f;
    static constexpr float LARGE_BIAS = 128.0f;
    for (float f : net->layers[i].weights) {
      if (f > LARGE_WEIGHT || f < -LARGE_WEIGHT) {
        large_weights++;
      }
    }
    for (float f : net->layers[i].biases) {
      if (f > LARGE_BIAS || f < -LARGE_BIAS) {
        large_biases++;
      }
    }
  }

  if (large_weights > 0 || large_biases > 0) {
    printf("Warning: %lld large weights and %lld large biases\n",
           large_weights, large_biases);
  }

  fclose(file);
  // printf("Read from %s.\n", filename.c_str());

  // Now, fill in the inverted indices. These are not stored in the file.

  // printf("Invert index:\n");
  ComputeInvertedIndices(net.get());
  // printf("Check it:\n");
  net->StructuralCheck();
  // CheckInvertedIndices(*net);

  return net.release();
}

struct FontProblem {
  static inline uint8_t FloatByte(float f) {
    const int x = roundf(f * 255.0f);
    return std::clamp(x, 0, 255);
  }

  // A tiny bitmap, like a letter. Intended for 8x8 bitmaps,
  // but can be used for any size <= 8x8.
  struct Image8x8 {
    inline void SetPixel(int x, int y, bool v) {
      int b = y * 8 + x;
      if (v) {
        bits |= ((uint64_t)1 << b);
      } else {
        bits &= ~((uint64_t)1 << b);
      }
    }
    inline bool GetPixel(int x, int y) const {
      int b = y * 8 + x;
      return (bits >> b) & 1;
    }

    uint64_t bits = 0;
  };

  // Specific to the 36x36 bitmap problem I mostly worked with;
  // tuned for speed. The 8x8 bitmap is placed at 4,6, which
  // empirically seems to be the "right" place for it.
  //
  // Note that these SDFs agree with SDFFromBitmap but are not
  // really right (they are too "low res") because that routine
  // assumes the bitmap is at least as big as the output sdf.
  static ImageA SDF36From8x8(Image8x8 bits);

  static void SDFFillVector(const ImageA &sdf,
                            vector<float> *buffer) {
    int idx = 0;
    for (int y = 0; y < sdf.Height(); y++) {
      for (int x = 0; x < sdf.Width(); x++) {
        (*buffer)[idx++] = sdf.GetPixel(x, y) / 255.0f;
      }
    }
  }

  // Must use a network generated by the previous.
  // Returns the vector directly to avoid a copy.
  static std::vector<float>
  RunSDFModelPredOnly(const Network &net,
                      const ImageA &sdf_input) {
    Stimulation stim{net};
    SDFFillVector(sdf_input, &stim.values[0]);
    net.RunForward(&stim);
    const std::vector<float> &output = stim.values.back();
    CHECK(output.size() == 26);
    return std::move(stim.values.back());
  }
};

ImageA FontProblem::SDF36From8x8(Image8x8 img) {
  constexpr int sdf_size = 36;
  // nominal size of image.
  constexpr int size = 18;

  constexpr float FALLOFF_PER_PIXEL = 15.0f;
  constexpr uint8 ONEDGE = 220u;
  constexpr float scale = size / (float)sdf_size;

  constexpr int LEFT = 4;
  constexpr int TOP = 6;
  constexpr int WIDTH = 8;
  constexpr int HEIGHT = 8;

  // true = white = inside letter
  auto Color = [&img](int x, int y) {
      if (x < LEFT || y < TOP ||
          x >= (LEFT + WIDTH) ||
          y >= (TOP + HEIGHT)) return false;
      return img.GetPixel(x - LEFT, y - TOP) > 0;
    };


  // Only need to search from LEFT - 1 to LEFT - 1 + WIDTH + 2, etc.
  auto GetSqDistanceTo = [&Color, size](int x, int y, bool c,
                                        int squared_bound) -> int {
      int min_sqdist = squared_bound;

      // min and max (inclusive) ranges to search for pixels.
      const int min_x = LEFT - 1;
      const int max_x = LEFT + WIDTH + 1;
      const int min_y = TOP - 1;
      const int max_y = TOP + HEIGHT + 1;

      // we do a symmetric expansion, but only need to cover the
      // rectangle above. find the maximum axis-aligned distance to
      // any of those points from x/y.
      const int xsize = std::max(abs(x - min_x), abs(x - max_x));
      const int ysize = std::max(abs(y - min_y), abs(y - max_y));
      // Instead of scanning from top to bottom, use larger and larger
      // offsets but try both positive and negative. Goal is to be
      // able to exit when the pixels being tested must be outside
      // our current bound.
      for (int dy = 0; dy <= ysize; dy++) {
        const int dys = dy * dy;
        // dy is always getting bigger; if we already found a pixel
        // closer than this distance, just counting the vertical,
        // we can't improve.
        if (dys >= min_sqdist) break;

        // We'll explore dy and dx in both direction.
        // Note this harmlessly tests pixels twice when dy or dx is
        // 0, but it seems better to avoid the branching?
        // Seems to be a better tradeoff to do the outer loop out
        // here so we can skip the entire x loop sometimes (when
        // outside the image entirely).
        for (int sy : {-dy, +dy}) {
          int yy = y + sy;
          if (yy < TOP - 1 || yy > TOP + HEIGHT) continue;

          for (int dx = 0; dx <= xsize; dx++) {
            const int dxs = dx * dx;
            // can do a similar test here but it is not
            // faster?
            if (dys + dxs >= min_sqdist) break;

            // might be faster to check bounds here rather
            // than in Color, although it gets a bit gross.
            for (int sx : {-dx, +dx}) {
              const int xx = x + sx;
              if (Color(xx, yy) == c) {
                if (dys + dxs < min_sqdist) {
                  min_sqdist = dys + dxs;
                }
              }
            }
          }
        }
      }
      // CHECK(min_sqdist <= squared_bound) <<
      //     min_sqdist << " vs " << squared_bound;
      return min_sqdist;
    };

  constexpr float oscale = 1.0f / scale;
  ImageA sdf(sdf_size, sdf_size);

  // As we populate the SDF, we can use the previous computed pixel
  // (that was searching for the same color) to give us a bound on the
  // search. OK for this to span rows, although then the bounds get
  // momentarily bad. Start with "infinite" though.
  const int MAX_DIST = size * 2;
  struct Last {
    // Position of the last pixel for which we
    // got a distance, for the given color.
    int x, y;
    // The actual distance we computed for that pixel.
    float dist;
  };
  std::array<Last, 2> last = {Last{.x = 0, .y = 0, .dist = (float)MAX_DIST},
                              Last{.x = 0, .y = 0, .dist = (float)MAX_DIST}};

  for (int sy = 0; sy < sdf_size; sy++) {
    for (int sx = 0; sx < sdf_size; sx++) {
      int ix = roundf((float)sx * scale);
      int iy = roundf((float)sy * scale);

      const bool color = Color(ix, iy);

      // Whatever we computed for the last pixel of the same color
      // gives us a bound using the triangle inequality (worst case
      // is our distance to that pixel, plus its distance to the
      // goal).
      Last &prev = last[color ? 1 : 0];
      int dx = ix - prev.x;
      int dy = iy - prev.y;
      float bound = prev.dist + sqrtf(dx * dx + dy * dy);
      int sq_bound = ceilf(bound * bound);
      // printf("dx %d dy %d prev %.2f bound %.2f sq %d\n",
      // dx, dy, prev.dist, bound, sq_bound);

      const int sqdist = GetSqDistanceTo(ix, iy, !color, sq_bound);
      const float dist = sqrtf(sqdist);

      // Save distance for next time we search this color.
      prev.x = ix;
      prev.y = iy;
      prev.dist = dist;

      // Distance in sdf space.
      const float sdf_dist = dist * oscale * FALLOFF_PER_PIXEL;

      const float signed_dist = color ? sdf_dist : -sdf_dist;
      int value = roundf((float)ONEDGE + signed_dist);
      sdf.SetPixel(sx, sy, std::clamp(value, 0, 255));
    }
  }

  return sdf;
}

// Singleton since it allocates a large block of memory. Never freed.
static Network *GetNetwork() {
  // This was the lowercase model, but trimmed so that it only predicted
  // the 26 letter parts, no SDF nodes. Then it was culled for completely
  // unreferenced nodes, but there are probably lots of weak parts that
  // could just be trimmed. Still, takes only 1ms or so to run.
  static Network *the_network =
    Network::ReadNetworkBinary("letterpred.val");
  return the_network;
}

// We can save a lot of time by caching predictions on common boards
// (result is small and reusable across letters since we predict all
// of them at once). XXX but we should keep this from growing without
// bound, with some kind of LRU?
struct ScoreCache {
  std::optional<float> Get(uint64 key, int letter) {
    ReadMutexLock ml(&m);
    auto it = cache.find(key);
    if (it == cache.end()) return {};
    else return it->second[letter];
  }

  void Insert(uint64 key, std::array<float, 26> value) {
    WriteMutexLock ml(&m);
    cache[key] = std::move(value);
  }

  std::shared_mutex m;
  std::unordered_map<uint64, std::array<float, 26>> cache;
};

static FontProblem::Image8x8 Get8x8(const Position &pos) {
  FontProblem::Image8x8 img8x8;
  for (int y = 0; y < 8; y++) {
    for (int x = 0; x < 8; x++) {
      img8x8.SetPixel(x, y, pos.PieceAt(y, x) != Position::EMPTY);
    }
  }
  return img8x8;
}

static float GetScore(const Position &pos, int letter) {
  static ScoreCache *cache = new ScoreCache;

  FontProblem::Image8x8 img8x8 = Get8x8(pos);

  if (std::optional<float> fo = cache->Get(img8x8.bits, letter)) {
    return fo.value();
  }

  // Compute new result.
  const ImageA sdf = FontProblem::SDF36From8x8(img8x8);
  vector<float> pred =
    FontProblem::RunSDFModelPredOnly(*GetNetwork(), sdf);

  // PERF:
  //  - is vector or array better in the unordered_map?
  //  - This is O(26*26)... might be better to do the work on
  //    each lookup instead of up front?
  std::array<float, 26> scores;
  for (int target = 0; target < 26; target++) {
    // Distance from [0, .. 0, 1, 0, ... 0]
    // This produced the best looking results in hallucinate.exe,
    // and it should make the different letters play differently,
    // at least!
    float penalty = 0.0f;
    for (int j = 0; j < 26; j++) {
      double p = pred[j];
      if (j == target) {
        penalty += fabs(1.0 - p);
      } else {
        penalty += fabs(p);
      }
    }

    scores[target] = penalty;
  }

  float ret = scores[letter];
  cache->Insert(img8x8.bits, std::move(scores));
  return ret;
}

struct LetterPlayer : public StatelessPlayer {
  LetterPlayer(const string &name,
                int letter) : name(name),
                              letter(letter) {
  }

  bool IsDeterministic() const override { return true; }

  Move MakeMove(const Position &orig_pos, Explainer *explainer) override {
    Position pos = orig_pos;
    std::vector<Position::Move> legal = pos.GetLegalMoves();

    // Compute score for each move.
    std::vector<std::pair<Move, float>> results;
    results.reserve(legal.size());
    for (Move m : legal) {
      results.emplace_back(
          m,
          pos.MoveExcursion(m,
                            [this, &pos]() {
                              return GetScore(pos, letter);
                            }));
    }

    const auto [best_move, best_score] =
      PlayerUtil::GetBest(results,
                          [](const std::pair<Move, float> &a,
                             const std::pair<Move, float> &b) {
                            // Want lower score, because these are
                            // distances from the ideal vector.
                            return a.second < b.second;
                          });

    if (explainer != nullptr) {
      // XXX render move scores too...
      const FontProblem::Image8x8 img8x8 = Get8x8(pos);
      ImageRGBA explained(300, 400);
      explained.Clear32(0xFFFFFFFF);
      const ImageA sdf = FontProblem::SDF36From8x8(img8x8);
      explained.BlendImage(0, 0, sdf.GreyscaleRGBA().ScaleBy(3));
      explainer->SetGraphic(sdf.GreyscaleRGBA().ScaleBy(3));

      std::sort(results.begin(), results.end(),
                [](const std::pair<Move, float> &a,
                   const std::pair<Move, float> &b) {
                  return a.second < b.second;
                });
      int ypos = 0;
      int xpos = 36 * 3 + 4;
      for (const auto &[move, score] : results) {
        // Make move again to get position.
        Position mpos = pos;
        mpos.ApplyMove(move);
        FontProblem::Image8x8 mimg8x8 = Get8x8(mpos);

        const string movestring = pos.ShortMoveString(move);
        constexpr int SCALE = 2;

        for (int y = 0; y < 8; y++) {
          for (int x = 0; x < 8; x++) {
            bool b = mimg8x8.GetPixel(x, y);
            for (int dy = 0; dy < SCALE; dy++) {
              for (int dx = 0; dx < SCALE; dx++) {
                explained.SetPixel32(xpos + x * SCALE + dx,
                                     ypos + y * SCALE + dy,
                                     b ? 0x000000FF : 0xEEEEEEFF);
              }
            }
          }
        }

        int YM = (SCALE * 8 - 9) / 2;
        if (YM < 0) YM = 0;
        
        explained.BlendText32(xpos + 8 * SCALE + 2,
                              ypos + YM, 0x000000FF,
                              StringPrintf("%s. %.3f",
                                           movestring.c_str(),
                                           -score));
        ypos += 8 * SCALE + 8;
      }
      
      explainer->SetGraphic(explained);
    }


    return best_move;
  }

  string Name() const override {
    return name;
  }

  string Desc() const override {
    return StringPrintf("Make moves that make the resulting bitmap, "
                        "treated as an 8x8 image, look most like the "
                        "capital letter '%c', as judged by the 'make "
                        "lowercase' network.", 'A' + letter);
  }

  const string name;
  const int letter;
};

}  // namespace

Player *Letter(int letter) {
  return new MakeStateless<LetterPlayer, string, int>(
      StringPrintf("letter_%c", 'a' + letter), letter);
}

