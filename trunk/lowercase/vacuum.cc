
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <set>

#include "../../cc-lib/threadutil.h"
#include "../../cc-lib/randutil.h"
#include "../../cc-lib/arcfour.h"
#include "../../cc-lib/base/logging.h"

#include "timer.h"

#include "network.h"

using namespace std;

using int64 = int64_t;

// TODO PERF: After vacuuming, there can be nodes with no references
// whatsoever; these will never do anything again. We should remove them.

// You can get some sense of the weight distribution by using
// modelinfo.exe, which generates histograms of weights and biases per
// layer.

// Really, using an absolute threshold doesn't make that much sense,
// since downstream weights could be huge! Probably we should be doing
// some kind of flow to see how much it could be contributing. Or just
// throw away a constant fraction of each layer?

// 0.01f worked fine for my chess unblinder. With weight decay, weights
// may be really small (.0001 may be a better starting point, for example).
static constexpr float THRESHOLD = 0.0001f;

// Configure the set of layers (as indices into layers[]) to vacuum.
// It's probably safest to vacuum one at a time, followed by some
// training to tune.
//
// It may be counterproductive to vacuum DENSE layers, since these
// have a more efficient representation and training/inference code.
// If you're not throwing out half the layer or more, you'd probably
// be better off just zeroing weights for the same effect (but then
// why bother?)
static const std::set<int> VACUUM_LAYERS{2};

static void VacuumLayer(Network *net, int layer_idx) {
  CHECK(layer_idx >= 0);
  CHECK(layer_idx < net->num_layers);
  const int num_nodes = net->num_nodes[layer_idx + 1];
  Network::Layer *layer = &net->layers[layer_idx];
  const int ipn = layer->indices_per_node;

  // The indices are hard to work with in their flat representation;
  // make a vector of weighted indices per node.
  struct OneIndex {
    uint32_t index = 0u;
    float weight = 0.0f;
  };

  auto CompareByWeightDesc =
    [](const OneIndex &a, const OneIndex &b) {
      return fabs(b.weight) < fabs(a.weight);
    };

  printf("Layer %d: Unpack %d nodes / %d indices per node\n",
	 layer_idx, num_nodes, ipn);
  fflush(stdout);
  CHECK(layer->indices.size() == layer->weights.size());
  CHECK(layer->indices.size() == ipn * num_nodes);
  vector<vector<OneIndex>> node_indices;
  node_indices.resize(num_nodes);
  {
    for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
      node_indices[node_idx].reserve(ipn);
      for (int idx = 0; idx < ipn; idx++) {
	OneIndex oi;
	oi.index = layer->indices[node_idx * ipn + idx];
	oi.weight = layer->weights[node_idx * ipn + idx];
	node_indices[node_idx].push_back(oi);
      }
      
      // Sort descending by weight.
      std::sort(node_indices[node_idx].begin(),
		node_indices[node_idx].end(),
		CompareByWeightDesc);
    }
  }

  printf("Layer %d: Truncate to threshold %.6f.\n", layer_idx,
	 THRESHOLD);
  int trunc_to = ipn;
  for (;;) {
    CHECK(trunc_to > 0) << "ALL of the weights for the entire "
      "layer (" << layer_idx << ") are below the threshold (" <<
      THRESHOLD << ")!?";

    // PERF this could actually be done with binary search.
    // Can we reduce it?
    bool ok = true;
    for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
      float w = node_indices[node_idx][trunc_to - 1].weight;
      if (fabs(w) >= THRESHOLD) {
	printf("Layer %d: Node %d has a weight "
	       "of %.6f in column %d; done.\n",
	       layer_idx,
	       node_idx,
	       w, trunc_to - 1);
	ok = false;
	break;
      }
    }

    if (!ok) {
      break;
    }
    trunc_to--;
  }

  printf("Layer %d: Dropping %d of %d indices.\n",
	 layer_idx, ipn - trunc_to, ipn);

  // Put back in ascending index order; the network can be in
  // any order, but this is probably best for memory locality
  // sake.
  auto CompareByIndex =
    [](const OneIndex &a, const OneIndex &b) {
      return a.index < b.index;
    };

  for (vector<OneIndex> &indices : node_indices) {
    indices.resize(trunc_to);
    std::sort(indices.begin(), indices.end(), CompareByIndex);
  }

  if (layer->indices_per_node != trunc_to &&
      layer->type == LAYER_DENSE) {
    printf("Layer %d is no longer DENSE.\n", layer_idx);
    layer->type = LAYER_SPARSE;
  }
    
  layer->indices_per_node = trunc_to;

  // Now re-pack!
  layer->indices.clear();
  layer->weights.clear();
  layer->indices.reserve(num_nodes * trunc_to);
  layer->weights.reserve(num_nodes * trunc_to);
  for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
    for (int i = 0; i < trunc_to; i++) {
      layer->indices.push_back(node_indices[node_idx][i].index);
      layer->weights.push_back(node_indices[node_idx][i].weight);
    }
  }
}

static void VacuumNetwork(Network *net) {
  // Each node in the network is fed weighted inputs from the previous
  // layer; these are called "indices" in this code. (We store them
  // sparsely, so they are actually the indices of the nodes on the
  // previous layer.)
  
  // The network format requires the same number of indices for each
  // node on a layer. So we're going to shrink that number across
  // all nodes, such that we drop (only) indices whose weights are
  // very small (below the threshold).

  net->StructuralCheck();

  for (int i = 0; i < net->layers.size(); i++) {
    const int num_nodes = net->num_nodes[i + 1];
    Network::Layer *layer = &net->layers[i];
    const int ipn = layer->indices_per_node;

    int64 bytes =
      // Biases
      num_nodes * 4 +
      // Weights
      (num_nodes * ipn) * 4;
    
    printf("Layer %d has %d nodes, %d indices per node = %d, %.1fMB\n",
	   i, num_nodes, ipn, num_nodes * ipn,
	   bytes / (1024.0 * 1024.0));
    fflush(stdout);
  }
  
  UnParallelComp(net->layers.size(),
		 [net](int i) {
		   if (VACUUM_LAYERS.find(i) != VACUUM_LAYERS.end())
		     VacuumLayer(net, i);
		 },
		 24);

  // We changed the number of indices per node, so we need to resize
  // the inverted indices (and recompute them).
  
  // Maybe this could be a helper in Network. It's taken from
  // the constructor.
  net->inverted_indices.resize(net->num_layers);
  for (int i = 0; i < net->num_layers; i++) {
    Network::InvertedIndices &ii = net->inverted_indices[i];
    ii.start.resize(net->num_nodes[i], 0xBEEF);
    ii.length.resize(net->num_nodes[i], 0xDEAD);
    ii.output_indices.resize(
	net->layers[i].indices_per_node * net->num_nodes[i + 1], 0xCAFE);
  }

  printf("Recompute inverted indices...\n");
  fflush(stdout);
  Network::ComputeInvertedIndices(net, 24);
  printf("Structural check...\n");
  fflush(stdout);
  net->StructuralCheck();
  printf("Done.\n");
  fflush(stdout);
}

int main(int argc, char **argv) {

  CHECK(argc >= 2) << "\n\nUsage:\nvacuum.exe net.val [output.val]";
  
  Timer model_timer;
  std::unique_ptr<Network> net{Network::ReadNetworkBinary(argv[1])};
  fprintf(stderr, "Loaded model in %.2fs\n",
	  model_timer.MS() / 1000.0);
  fflush(stderr);

  Timer vacuum_timer;
  VacuumNetwork(net.get());
  fprintf(stderr, "Vacuumed model in %.2fs\n",
	  vacuum_timer.MS() / 1000.0);
  fflush(stderr);

  string outfile = argc > 2 ? argv[2] : "net-vacuumed.val";
  Network::SaveNetworkBinary(*net, outfile);
  
  return 0;
}

