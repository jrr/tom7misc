
// CPU version of network.

#ifndef __NETWORK_H
#define __NETWORK_H

#include <vector>
#include <string>
#include <cstdint>
#include "../../cc-lib/base/logging.h"

enum TransferFunction {
  SIGMOID = 0,
  RELU,
  LEAKY_RELU,
  NUM_TRANSFER_FUNCTIONS,
};

const char *TransferFunctionName(TransferFunction tf);

struct Network {
  template<class T> using vector = std::vector<T>;
  using string = std::string;
  
  // Creates arrays of the appropriate size, but all zeroes. Note that
  // this uninitialized network is invalid, since the inverted indices
  // are not correct.
  Network(vector<int> num_nodes,
	  vector<int> indices_per_node,
	  vector<TransferFunction> transfer_functions);

  int64_t Bytes() const;

  void CopyFrom(const Network &other) {
    CHECK_EQ(this->num_layers, other.num_layers);
    this->num_nodes = other.num_nodes;
    this->width = other.width;
    this->height = other.height;
    this->channels = other.channels;
    this->layers = other.layers;
    this->inverted_indices = other.inverted_indices;
  }

  void NaNCheck(const char *message) const;

  static Network *Clone(const Network &other);
  
  static Network *ReadNetworkBinary(const string &filename);
  static void SaveNetworkBinary(const Network &net, const string &filename);

  // Just make these members??
  static void CheckInvertedIndices(const Network &net);
  static void ComputeInvertedIndices(Network *net, int max_parallelism = 8);
  
  // Just used for serialization. Whenever changing the interpretation
  // of the data in an incomplete way, please change.
  static constexpr uint32_t FORMAT_ID = 0x2700072DU;

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

// TODO: Move this stuff at least into .cc, if not their own headers?

// A stimulation is an evaluation (perhaps an in-progress one) of a
// network on a particular input; when it's complete we have the
// activation value of each node on each layer, plus the input itself.
template<class T>
struct StimulationT {
  explicit StimulationT(const Network &net) : num_layers(net.num_layers),
					      num_nodes(net.num_nodes) {
    values.resize(num_layers + 1);
    for (int i = 0; i < values.size(); i++)
      values[i].resize(num_nodes[i], 0.0f);
  }
  
  // Empty, useless stimulation, but can be used to initialize
  // vectors, etc.
  StimulationT() : num_layers(0) {}
  StimulationT(const StimulationT &other) = default;
  
  int64_t Bytes() const {
    int64_t ret = sizeof *this;
    for (int i = 0; i < values.size(); i++) {
      ret += sizeof values[i] + sizeof values[i][0] * values[i].size();
    }
    return ret;
  }

  // TODO: would be nice for these to be const, but then we can't have an
  // assignment operator.
  // Same as in Network.
  int num_layers;
  // num_layers + 1
  std::vector<int> num_nodes;

  // Keep track of what's actually been computed?

  // Here the outer vector has size num_layers + 1; first is the input.
  // Inner vector has size num_nodes[i], and just contains their output values.
  std::vector<std::vector<T>> values;

  void CopyFrom(const StimulationT &other);
  void NaNCheck(const char *message) const;
};

using Stimulation = StimulationT<float>;
using StimulationD = StimulationT<double>;

struct Errors {
  explicit Errors(const Network &net) : num_layers(net.num_layers),
					num_nodes(net.num_nodes) {
    error.resize(num_layers);
    for (int i = 0; i < error.size(); i++) {
      error[i].resize(num_nodes[i + 1], 0.0f);
    }
  }
  // Empty, useless errors, but can be used to initialize vectors etc.
  Errors() : num_layers(0) {}
  Errors(const Errors &other) = default;

  // Would be nice for these to be const, but then we can't have an
  // assignment operator.
  int num_layers;
  // The first entry here is unused (it's the size of the input layer,
  // which doesn't get errors), but we keep it like this to be
  // consistent with Network and Stimulation.
  std::vector<int> num_nodes;
  int64_t Bytes() const {
    int64_t ret = sizeof *this;
    for (int i = 0; i < error.size(); i++)
      ret += sizeof error[i] + sizeof error[i][0] * error[i].size();
    return ret;
  }

  void CopyFrom(const Errors &other);
  
  // These are the delta terms in Mitchell. We have num_layers of
  // them, where the error[0] is the first real layer (we don't
  // compute errors for the input) and error[num_layers] is the error
  // for the output.
  std::vector<std::vector<float>> error;
};

// The stimulation must match the network, and its input layer must be
// filled. Run the network forward, populating the rest of the
// stimulation in place.
extern void ForwardStimulation(const Network &net, Stimulation *stim);
extern void ForwardStimulationD(const Network &net, StimulationD *stim);

#endif
