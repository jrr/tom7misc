
// Utilties for working with the Network objects, but that are not
// "core" enough to be part of Network itself.

#ifndef _LOWERCASE_NETWORK_UTIL_H
#define _LOWERCASE_NETWORK_UTIL_H

#include <string>
#include <vector>
#include <cstdint>

#include "base/logging.h"
#include "base/stringprintf.h"
#include "util.h"

#include "network.h"

// More convenient representation for a network layer, which can be
// used for resizing operations like in cull.exe.
// TODO: Use this for vacuum, and maybe in widen too.
struct EZLayer {
  int width = 0;
  int height = 0;
  int ipn = 0;
  
  // The indices are hard to work with in their flat representation;
  // make a vector of weighted indices per node.
  struct OneIndex {
    uint32_t index = 0u;
    float weight = 0.0f;
  };

  struct Node {
    float bias = 0.0f;
    std::vector<OneIndex> inputs;
  };

  std::vector<Node> nodes;

  EZLayer(const Network &net, int layer_idx) {
    CHECK(layer_idx >= 0);
    CHECK(layer_idx < net.num_layers);
    const int num_nodes = net.num_nodes[layer_idx + 1];
    const Network::Layer *layer = &net.layers[layer_idx];
    ipn = layer->indices_per_node;

    width = net.width[layer_idx + 1];
    height = net.height[layer_idx + 1];
    CHECK(net.channels[layer_idx + 1]) << "Flatten channels first";
    CHECK(width * height == num_nodes);
    
    CHECK(layer->indices.size() == layer->weights.size());
    CHECK(layer->indices.size() == ipn * num_nodes);
    nodes.resize(num_nodes);

    for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
      Node &node = nodes[node_idx];
      node.bias = layer->biases[node_idx];
      node.inputs.reserve(ipn);
      for (int idx = 0; idx < ipn; idx++) {
	OneIndex oi;
	oi.index = layer->indices[node_idx * ipn + idx];
	oi.weight = layer->weights[node_idx * ipn + idx];
	node.inputs.push_back(oi);
      }
    }
  }


  // After changing the number of nodes, this can be used to update
  // the width/height. Tries to make a rectangular with a non-degenerate
  // aspect ratio.
  void MakeWidthHeight() {
    int num_nodes = nodes.size();
    std::vector<int> factors = Util::Factorize(num_nodes);
    CHECK(!factors.empty()) << num_nodes << " has no factors??";

    // XXX Does this greedy approach produce good results?
    int ww = factors.back(), hh = 1;
    factors.pop_back();

    for (int f : factors) {
      if (ww < hh)
	ww *= f;
      else
	hh *= f;
    }

    CHECK(ww * hh == num_nodes);
    width = ww;
    height = hh;
  }
  
  // Packs inputs and biases back into the layer. Requires that the
  // width/height match the number of nodes (maybe call
  // MakeWidthHeight). Does not update the inverted indices!
  void Repack(Network *net, int layer_idx) {
    const int num_nodes = nodes.size();
    CHECK(num_nodes == width * height);

    Network::Layer *layer = &net->layers[layer_idx];
    net->width[layer_idx + 1] = width;
    net->height[layer_idx + 1] = height;
    net->num_nodes[layer_idx + 1] = num_nodes;
    layer->indices_per_node = ipn;
    
    if (ipn == net->num_nodes[layer_idx]) {
      layer->type = LAYER_DENSE;
    } else {
      layer->type = LAYER_SPARSE;
    }
    
    // Sort all index lists, and check that they're the right
    // size.
    auto CompareByIndex =
      [](const OneIndex &a, const OneIndex &b) {
	return a.index < b.index;
      };

    for (Node &node : nodes) {
      CHECK(node.inputs.size() == ipn);
      std::sort(node.inputs.begin(), node.inputs.end(), CompareByIndex);
    }

    // Copy nodes back into layer.
    layer->indices.clear();
    layer->weights.clear();
    layer->biases.clear();
    layer->indices.reserve(num_nodes * ipn);
    layer->weights.reserve(num_nodes * ipn);
    for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
      const Node &node = nodes[node_idx];
      layer->biases.push_back(node.bias);
      for (int i = 0; i < ipn; i++) {
	layer->indices.push_back(node.inputs[i].index);
	layer->weights.push_back(node.inputs[i].weight);
      }
    }
  }

};

#endif
