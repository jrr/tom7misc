
#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <set>
#include <unordered_set>

#include "threadutil.h"
#include "randutil.h"
#include "arcfour.h"
#include "base/logging.h"
#include "base/stringprintf.h"

#include "timer.h"

#include "network.h"
#include "network-util.h"

using namespace std;

using int64 = int64_t;

// How many nodes should we add on the target layers?
//
// Note that this is applied to the dimension being widened (width or
// height) and in extreme cases could result in a lot of roundoff
// error. Ignored for FROM_NETWORK, which just adds all the nodes
// from the network.
static constexpr float ADD_NODE_FRAC = 0.10f;
static_assert(ADD_NODE_FRAC > 0.0f);

enum class Dimension {
  WIDTH,
  HEIGHT,
  ONE_DIMENSIONAL,
  // requires SRC_NETWORK to be defined
  FROM_NETWORK,
  // TODO: Channels also makes sense
};

static constexpr char *SRC_NETWORK = "makefeatures.val";
static constexpr Dimension DIMENSION = Dimension::ONE_DIMENSIONAL;

// The layer (as an index into layers[]) to widen. It's probably
// safest to do some tuning after each widen operation.
// Not clear if widening from top to bottom or bottom to top is
// better, or whether that matters?
static constexpr int WIDEN_LAYER = 3;

// On the next layer, we'll add some indices (increasing
// indices_per_node) to reference only these new added
// nodes. This is the rate at which nodes are referenced
// by each node in the next layer.

// FIXME: Need to handle DENSE layers!
static constexpr float ADD_INDEX_RATE = 0.15f;
static_assert(ADD_INDEX_RATE > 0.0f,
	      "no point to add nodes if not references");
static_assert(ADD_INDEX_RATE <= 1.0f,
	      "can at most make reference to every node");

// Initial weights are uniformly generated in
// [-INITIAL_WEIGHT, INITIAL_WEIGHT]. Assuming the network
// already has significant training, this should probably
// be very small. (Should experiment with this?)
static constexpr double INITIAL_WEIGHT = 0.1; // 00001;

[[maybe_unused]]
static float Uniform(ArcFour *rc) {
  constexpr double IVAL_WIDTH = 2.0 * INITIAL_WEIGHT;
  // Uniform in [0,1]
  const double d = (double)Rand32(rc) / (double)0xFFFFFFFF;
  return (float)((IVAL_WIDTH * d) - (double)INITIAL_WEIGHT);
}

// Input weights for a newly created node. These should be
// nonzero (so that the node isn't born dead), but very
// small (so that it is minimally disruptive to the current
// behavior).
static float NewNodeInputWeight(ArcFour *rc) {
  return Uniform(rc);
}

// Use of a new node on the subsequent layer.
static float NewUseWeight(ArcFour *rc) {
  // return Uniform(rc);
  return 0.0f;
}

// Get num input indices from the previous layer for a single new
// node. They must be distinct. They don't have to be random.
static vector<int> GetInputIndices(ArcFour *rc, int previous_layer_size, int num) {
  // Always add from the last 200 nodes, which are some
  // generated features.

  std::unordered_set<int> indices;
  indices.reserve(num);
  // XXXXXXXX make this a proper config value
  static constexpr int NUM_LAST = 185;
  static constexpr uint8 P_LAST = 140;
  int idx = previous_layer_size - 1;
  for (int i = 0; i < NUM_LAST && indices.size() < num; i++) {
    CHECK(idx >= 0);

    if (rc->Byte() < P_LAST) {
      indices.insert(idx);
    }
    idx--;
  }

  // Fill the rest of the budget uniformly.
  vector<int> all;
  all.reserve(previous_layer_size);
  for (int i = 0; i < previous_layer_size; i++)
    all.push_back(i);
  Shuffle(rc, &all);

  for (int i = 0; i < all.size() && indices.size() < num; i++) {
    // Might already be there; does nothing.
    indices.insert(all[i]);
  }

  // Convert to vector.
  vector<int> ret;
  ret.reserve(indices.size());
  for (int i : indices) ret.push_back(i);
  CHECK(ret.size() == num);
  return ret;
}

// Modifies the network in place. May remap node indices, and adds new
// ones. Returns mapping from old idx to new (first vector), and
// returns the vector of node indices that were added (second vector).
// These should be used to remap the next layer's input indices, and
// expand them so that the newly added nodes are not dead.
//
// TODO: Lots of fiddly code, redundant with EZLayer. Can we perhaps
// work from the EZLayer node structure?
static std::pair<vector<int>, vector<int>>
WidenLayer3D(ArcFour *rc, Network *net, int layer_idx) {
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

  struct Node {
    vector<OneIndex> inputs;
    float bias = 0.0f;
  };

  const int width = net->width[layer_idx + 1];
  const int height = net->height[layer_idx + 1];
  const int channels = net->channels[layer_idx + 1];
  CHECK(width * height * channels == num_nodes);
  
  printf("Layer %d: Unpack %dx%dx%d = %d nodes w/ %d ipn\n",
	 layer_idx,
	 width,
	 height,
	 channels,
	 num_nodes, ipn);
  fflush(stdout);
  CHECK(layer->indices.size() == layer->weights.size());
  CHECK(layer->indices.size() == ipn * num_nodes);
  CHECK(layer->biases.size() == num_nodes);
  
  // rows of cols of channels
  vector<vector<vector<Node>>> nodes;
  nodes.resize(height);
  for (auto &v : nodes) {
    v.resize(width);
    for (auto &vv : v) {
      vv.resize(channels);
    }
  }

  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      for (int c = 0; c < channels; c++) {
	const int node_idx = y * width * channels + x * channels + c;
	Node &node = nodes[y][x][c];
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
  }

  // Resize the layer, adding nodes along the dimension requested.
  // Save the new indices since we need to add them in the next layer.

  const int previous_layer_size = net->num_nodes[layer_idx];
  CHECK(ipn <= previous_layer_size);
  auto NewNode = [&rc, ipn, previous_layer_size]() {
      // Totally random. Not clear how we would use width/height here.
      // If the layer is multichannel, we could insist that we generate
      // nodes for each channel from the same "pixel", but we currently
      // do not do that.
      Node node;
      node.bias = 0.0f;
      node.inputs.reserve(ipn);
      
      vector<int> indices = GetInputIndices(rc, previous_layer_size, ipn);
      
      for (int i = 0; i < ipn; i++) {
	OneIndex oi;
	oi.index = indices[i];
	oi.weight = NewNodeInputWeight(rc);
	node.inputs.push_back(oi);
      }

      return node;
    };

  switch (DIMENSION) {
  case Dimension::WIDTH: {
    const int add_width = std::max((int)roundf(width * ADD_NODE_FRAC), 1);
    printf("Width: %d + %d becomes %d\n", width, add_width, width + add_width);
    // In each row, add cells.
    for (vector<vector<Node>> &row : nodes) {
      for (int x = 0; x < add_width; x++) {
	vector<Node> new_cell;
	for (int c = 0; c < channels; c++) {
	  new_cell.push_back(NewNode());
	}
	row.push_back(std::move(new_cell));
      }
    }
    break;
  }
  case Dimension::HEIGHT: {
    const int add_height = std::max((int)roundf(height * ADD_NODE_FRAC), 1);
    printf("Height: %d + %d becomes %d\n", height, add_height, height + add_height);
    for (int y = 0; y < add_height; y++) {
      // Add an entire row.
      vector<vector<Node>> new_row;
      for (int x = 0; x < width; x++) {
	vector<Node> new_cell;
	for (int c = 0; c < channels; c++) {
	  new_cell.push_back(NewNode());
	}
	new_row.push_back(std::move(new_cell));
      }
      nodes.push_back(std::move(new_row));
    }
    break;
  }
  default:
    LOG(FATAL) << "Unknown dimension?";
  }
  
  // Now convert back.

  const int new_width = nodes[0].size();
  const int new_height = nodes.size();
  const int new_channels = channels;
  const int new_num_nodes = new_width * new_height * new_channels;
  
  // Put inputs back in ascending order by index, to improve
  // locality of memory access.
  {
    auto CompareByIndex =
      [](const OneIndex &a, const OneIndex &b) {
	return a.index < b.index;
      };

    CHECK(nodes.size() == new_height);
    for (auto &v : nodes) {
      CHECK(v.size() == new_width);
      for (auto &vv : v) {
	CHECK(vv.size() == new_channels);
	for (auto &vvv : vv) {
	  CHECK(vvv.inputs.size() == ipn);
	  std::sort(vvv.inputs.begin(), vvv.inputs.end(),
		    CompareByIndex);
	}
      }
    }
  }


  net->width[layer_idx + 1] = new_width;
  net->height[layer_idx + 1] = new_height;
  net->channels[layer_idx + 1] = new_channels;
  net->num_nodes[layer_idx + 1] = new_num_nodes;

  vector<int> added_indices;
  vector<int> rewritten_indices(num_nodes, -1);
  
  // Now re-pack!
  layer->indices.clear();
  layer->weights.clear();
  layer->biases.clear();
  layer->indices.reserve(num_nodes * ipn);
  layer->weights.reserve(num_nodes * ipn);

  for (int y = 0; y < new_height; y++) {
    for (int x = 0; x < new_width; x++) {
      for (int c = 0; c < new_channels; c++) {
	const int node_idx = y * new_width * new_channels + x * new_channels + c;

	if (y < height && x < width && c < channels) {
	  // Map old idx to new idx.
	  const int old_idx = y * width * channels + x * channels + c;
	  rewritten_indices[old_idx] = node_idx;
	} else {
	  // Node is newly added.
	  added_indices.push_back(node_idx);
	}

	CHECK(node_idx == layer->biases.size());
	const Node &node = nodes[y][x][c];
	layer->biases.push_back(node.bias);

	CHECK(ipn == node.inputs.size());
	for (int idx = 0; idx < ipn; idx++) {
	  const OneIndex oi = node.inputs[idx];
	  layer->indices.push_back(oi.index);
	  layer->weights.push_back(oi.weight);
	}
      }
    }
  }
  CHECK(layer->biases.size() == new_num_nodes);

  for (int i : rewritten_indices) {
    CHECK(i >= 0);
  }
  return {rewritten_indices, added_indices};
}

// Same as above, but does not try to preserve the geometry of the
// existing network; it just adds nodes on the end and refactors
// the width and height to match. Used for ONE_DIMENSIONAL expansion.
static std::pair<vector<int>, vector<int>>
WidenLayer1D(ArcFour *rc, Network *net, int layer_idx) {
  EZLayer ez(*net, layer_idx);

  // Add nodes to the end.

  const int previous_layer_size = net->num_nodes[layer_idx];
  CHECK(ez.ipn <= previous_layer_size);
  // XXX share with 3D version?
  // XXX Here we've abandoned the spatial meaning of hidden layers
  // because of the resizing. But for the first hidden layer, it
  // definitely still makes sense to reference nearby pixels if the
  // input is an image! Should make this an option when generating
  // node indices. (alternatively, use makefeatures and from_network!)
  auto NewNode = [&rc, &ez, previous_layer_size]() {
      EZLayer::Node node;
      node.bias = 0.0f;
      node.inputs.reserve(ez.ipn);

      vector<int> indices = GetInputIndices(rc, previous_layer_size, ez.ipn);
      
      for (int i = 0; i < ez.ipn; i++) {
	CHECK(i < indices.size());
	CHECK(indices[i] >= 0 && indices[i] < previous_layer_size);
	EZLayer::OneIndex oi;
	oi.index = indices[i];
	oi.weight = NewNodeInputWeight(rc);
	node.inputs.push_back(oi);
      }

      return node;
    };

  CHECK(DIMENSION == Dimension::ONE_DIMENSIONAL);

  const int old_num_nodes = ez.nodes.size();

  // Existing nodes keep the same indices.
  vector<int> rewritten_indices;
  rewritten_indices.reserve(old_num_nodes);
  for (int i = 0; i < old_num_nodes; i++)
    rewritten_indices.push_back(i);
  
  vector<int> added_indices;
  const int add_nodes = std::max((int)roundf(old_num_nodes * ADD_NODE_FRAC), 1);
  printf("1D: %d + %d becomes %d nodes\n", old_num_nodes, add_nodes,
	 old_num_nodes + add_nodes);
  for (int i = 0; i < add_nodes; i++) {
    added_indices.push_back(ez.nodes.size());
    ez.nodes.push_back(NewNode());
  }

  ez.MakeWidthHeight();
  
  ez.Repack(net, layer_idx);
  
  return {rewritten_indices, added_indices};
}

// Like 1D, but just copies features from another network (typically
// a fake one produced by makefeatures.exe).
static std::pair<vector<int>, vector<int>>
WidenFromNetwork(ArcFour *rc, Network *net, int layer_idx, const string &srcfile) {
  std::unique_ptr<Network> srcnet{Network::ReadNetworkBinary(srcfile)};  
  
  EZLayer ez(*net, layer_idx);

  EZLayer ezsrc(*srcnet, layer_idx);
  
  // Add nodes to the end.

  const int previous_layer_size = net->num_nodes[layer_idx];
  // Or I guess the source could be smaller...
  CHECK_EQ(srcnet->num_nodes[layer_idx], previous_layer_size);
  // But these really need to be the same unless we pad or something.
  CHECK_EQ(ez.ipn, ezsrc.ipn);
  CHECK(ez.ipn <= previous_layer_size);

  CHECK(DIMENSION == Dimension::FROM_NETWORK);

  const int old_num_nodes = ez.nodes.size();
  const int add_nodes = ezsrc.nodes.size();
  
  // Existing nodes keep the same indices.
  vector<int> rewritten_indices;
  rewritten_indices.reserve(old_num_nodes);
  for (int i = 0; i < old_num_nodes; i++)
    rewritten_indices.push_back(i);
  
  vector<int> added_indices;
  printf("From network: %d + %d becomes %d nodes\n", old_num_nodes, add_nodes,
	 old_num_nodes + add_nodes);

  for (const EZLayer::Node &srcnode : ezsrc.nodes) {
    added_indices.push_back(ez.nodes.size());
    ez.nodes.push_back(srcnode);
  }

  ez.MakeWidthHeight();
  
  ez.Repack(net, layer_idx);
  
  return {rewritten_indices, added_indices};
}


// Dispatch to the appropriate method.
static std::pair<vector<int>, vector<int>>
WidenLayer(ArcFour *rc, Network *net, int layer_idx) {
  switch (DIMENSION) {
  case Dimension::WIDTH:
  case Dimension::HEIGHT:
    return WidenLayer3D(rc, net, layer_idx);    
  case Dimension::ONE_DIMENSIONAL:
    return WidenLayer1D(rc, net, layer_idx);
  case Dimension::FROM_NETWORK:
    return WidenFromNetwork(rc, net, layer_idx, SRC_NETWORK);
  default:
    LOG(FATAL) << "Unknown dimension?";
    // return {{}, {}};
  }
}

// given a remapping and some new indices we created in the previous
// layer, update all the indices, and add references to the new ones
// on the indicated layer, increasing its indices_per_node.
static void RemapAndAdd(ArcFour *rc, Network *net, int layer_idx,
			const vector<int> &remapped_indices,
			const vector<int> &added_indices) {
  CHECK(layer_idx >= 0);
  CHECK(layer_idx < net->num_layers);
  const int num_nodes = net->num_nodes[layer_idx + 1];
  Network::Layer *layer = &net->layers[layer_idx];
  const int ipn = layer->indices_per_node;

  CHECK(layer->type != LAYER_DENSE) << "Unimplemented";
  
  CHECK(added_indices.size() > 0);
  const int add_ipn =
    std::max((int)(ADD_INDEX_RATE * added_indices.size()), 1);

  printf("Layer %d: ipn %d + %d = %d.\n",
	 layer_idx, ipn, add_ipn, ipn + add_ipn);
  
  struct OneIndex {
    uint32_t index = 0u;
    float weight = 0.0f;
  };

  // We keep shuffling this array so that it's a random permutation of
  // the added indices.
  vector<int> shuffled_indices = added_indices;
  
  CHECK(layer->indices.size() == layer->weights.size());
  CHECK(layer->indices.size() == ipn * num_nodes);
  vector<vector<OneIndex>> node_indices;
  node_indices.resize(num_nodes);
  for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
    node_indices[node_idx].reserve(ipn + add_ipn);
    for (int idx = 0; idx < ipn; idx++) {
      OneIndex oi;
      const int old_idx = layer->indices[node_idx * ipn + idx];
      CHECK(old_idx >= 0 && old_idx < remapped_indices.size());
      const int new_idx = remapped_indices[old_idx];
      oi.index = new_idx;
      oi.weight = layer->weights[node_idx * ipn + idx];
      node_indices[node_idx].push_back(oi);
    }

    Shuffle(rc, &shuffled_indices);
    for (int i = 0; i < add_ipn; i++) {
      OneIndex oi;
      oi.index = shuffled_indices[i];
      // Idea is that these nodes start with no downstream effect,
      // but can get picked up if they would have produced
      // a useful nudge.
      oi.weight = NewUseWeight(rc);
      node_indices[node_idx].push_back(oi);
    }
  }

  // Put back in ascending index order; the network can be in
  // any order, but this is probably best for memory locality
  // sake.
  auto CompareByIndex =
    [](const OneIndex &a, const OneIndex &b) {
      return a.index < b.index;
    };

  const int new_ipn = ipn + add_ipn;
  
  for (vector<OneIndex> &indices : node_indices) {
    CHECK(indices.size() == new_ipn);
    std::sort(indices.begin(), indices.end(), CompareByIndex);
  }

  layer->indices_per_node = new_ipn;

  // Now re-pack!
  layer->indices.clear();
  layer->weights.clear();
  layer->indices.reserve(num_nodes * new_ipn);
  layer->weights.reserve(num_nodes * new_ipn);
  for (int node_idx = 0; node_idx < num_nodes; node_idx++) {
    CHECK(node_indices[node_idx].size() == new_ipn);
    for (int i = 0; i < new_ipn; i++) {
      layer->indices.push_back(node_indices[node_idx][i].index);
      layer->weights.push_back(node_indices[node_idx][i].weight);
    }
  }
}



static void WidenNetwork(Network *net) {
  ArcFour rc(StringPrintf("%lld,%lld,%lld",
			  (int64)time(nullptr),
			  net->rounds,
			  net->Bytes()));
  
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

  CHECK(WIDEN_LAYER < net->layers.size()) << "WIDEN_LAYER out of bounds?";
  CHECK(WIDEN_LAYER != net->layers.size() - 1) << "Not widening the last "
    "layer because it's probably a mistake; the structure of the problem "
    "being trained would have to change. But you can try it by removing "
    "this check if you want.";

  const auto [rewritten_indices, added_indices] =
    WidenLayer(&rc, net, WIDEN_LAYER);
  // And refer to them on the next layer (unless there is none; see
  // above CHECK).
  if (WIDEN_LAYER + 1 < net->layers.size())
    RemapAndAdd(&rc, net, WIDEN_LAYER + 1, rewritten_indices, added_indices);

  // We changed the number of indices per node, so we need to resize
  // the inverted indices (and recompute them).
  net->ReallocateInvertedIndices();  

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
  CHECK(argc >= 2) << "\n\nUsage:\nwiden.exe net.val [output.val]";
  
  Timer model_timer;
  std::unique_ptr<Network> net{Network::ReadNetworkBinary(argv[1])};
  fprintf(stderr, "Loaded model in %.2fs\n",
	  model_timer.MS() / 1000.0);
  fflush(stderr);

  Timer widen_timer;
  WidenNetwork(net.get());
  fprintf(stderr, "Widened model in %.2fs\n",
	  widen_timer.MS() / 1000.0);
  fflush(stderr);

  string outfile = argc > 2 ? argv[2] : "net-widened.val";
  Network::SaveNetworkBinary(*net, outfile);
  
  return 0;
}

