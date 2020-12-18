
// CPU version of network.
// Idea is to keep this pretty general and eventually promote it to
// cc-lib, but for now I've been cloning and making improvements
// with each ML project.

#ifndef _LOWERCASE_NETWORK_H
#define _LOWERCASE_NETWORK_H

#include <vector>
#include <string>
#include <cstdint>

#include "base/logging.h"

enum TransferFunction {
  SIGMOID = 0,
  RELU,
  LEAKY_RELU,

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
  LAYER_SPARSE,
  // TODO: Convolutional

  NUM_LAYER_TYPES,
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

  // Size of network in RAM.
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

  // Check for NaN weights and abort if any are found.
  void NaNCheck(const char *message) const;

  // Check for structural well-formedness (layers are the right size;
  // indices are in bounds; dense layers have the expected regular
  // structure; inverted indices are correct). Aborts if something is
  // wrong. Doesn't check weight values (see NaNCheck).
  void StructuralCheck() const;
  // Check the inverted indices specifically. Maybe can just
  // be private.
  void CheckInvertedIndices() const;
  
  static Network *Clone(const Network &other);

  // Note: These use local byte order, so the serialized format is not
  // portable.
  static Network *ReadNetworkBinary(const string &filename);
  static void SaveNetworkBinary(const Network &net, const string &filename);

  static void ComputeInvertedIndices(Network *net, int max_parallelism = 8);

  // TODO: Add CPU inference, at least.
  
  // Just used for serialization. Whenever changing the interpretation
  // of the data in an incomplete way, please change.
  static constexpr uint32_t FORMAT_ID = 0x2700072FU;

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
    // Whether the layer is sparse or dense. We currently still store
    // the indices for dense layers, but they can also just be deduced
    // from the dimensions. (PERF: Save the ram?)
    LayerType type;
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

#endif
