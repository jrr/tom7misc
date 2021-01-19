
// Simplify a network by removing nodes that never have significant
// activation.

#include <string>
#include <vector>
#include <shared_mutex>
#include <cstdint>
#include <set>
#include <unordered_map>
#include <unordered_set>

#include "threadutil.h"
#include "randutil.h"
#include "arcfour.h"
#include "base/logging.h"
#include "base/stringprintf.h"
#include "image.h"
#include "util.h"

#include "timer.h"
#include "network.h"
#include "network-util.h"

using namespace std;

using int64 = int64_t;

// If activation never exceeds this amount, remove the node.
static constexpr float THRESHOLD = 0.00001f;

// Layer to cull.
static constexpr int CULL_LAYER = 2;

// How many random inputs to try. Approximate since we do the same number
// per thread.
static constexpr int NUM_SAMPLES = 327680;

// Accumulate actual activations into absolute maxes of the same size.
// PERF: We won't use the accumulated input layer; just computing it for
// regularity here...
// Not thread safe.
static void AccumulateLayer(const Stimulation &actual, Stimulation *maxes,
			    int layer_idx) {
  const vector<float> &vs = actual.values[layer_idx];
  vector<float> *ms = &maxes->values[layer_idx];
  CHECK(vs.size() == ms->size());
  for (int j = 0; j < vs.size(); j++) {
    float v = fabs(vs[j]);
    (*ms)[j] = std::max((*ms)[j], v);
  }
}

static void Accumulate(const Stimulation &actual, Stimulation *maxes) {
  CHECK(actual.values.size() == maxes->values.size());
  for (int layer_idx = 0; layer_idx < actual.values.size(); layer_idx++) {
    AccumulateLayer(actual, maxes, layer_idx);
  }
}


// TODO: Here we only randomize the input layer and check what gets
// stimulated downstream. This may be a little dangerous because we
// fail to activate some rare but useful feature (imagine a successfully
// trained network that's recognizing faces) -- it may never appear
// in random inputs. Perhaps it would make more sense to randomize
// each layer just to check how the next layer gets activated.
static Stimulation RandomlyStimulate(ArcFour *rc, const Network &net) {
  // If true, sometimes replace a hidden layer's activations with
  // random noise. Don't accumulate that of course, but use it to
  // stimulate the next layer. This is more conservative. Without
  // this, rare features can sometimes be incorrectly detected as dead
  // because we never found an input that activates them.
  static constexpr bool RERANDOMIZE = true;
  
  static constexpr int THREADS = 17;
  static_assert(THREADS > 1, "thread 0 is used for special patterns");
  static constexpr int SAMPLES_PER_THREAD = NUM_SAMPLES / THREADS;

  vector<ArcFour *> rcs;
  for (int i = 0; i < THREADS; i++) rcs.push_back(Substream(rc, i));

  vector<Stimulation> maxes =
    ParallelMapi(rcs, [&net](int idx, ArcFour *rc) {
	Stimulation m(net);

	auto Acc = [&net, &m](Stimulation *r) {
	    net.RunForward(r);
	    Accumulate(*r, &m);
	  };
	
	if (idx == 0) {
	  Stimulation r(net);
	  // All on.
	  for (float &v : r.values[0]) v = 1.0f;
	  Acc(&r);
	  // All off.
	  for (float &v : r.values[0]) v = 0.0f;
	  Acc(&r);

	  // Checkerboards of various scales.
	  // Reasonable to assume some spatial meaning for
	  // the input layer,
	  const int w = net.width[0];
	  const int h = net.height[0];
	  CHECK(net.channels[0] == 1) <<
	    "could implement multichannel here";

	  int iters = 0;
	  for (int size = 1; size < w; size++) {
	    for (int xo = 0; xo < size / 2; xo++) {
	      for (int yo = 0; yo < size / 2; yo ++) {
		for (int parity = 0; parity < 2; parity++) {
		  // Fill image...
		  for (int y = 0; y < h; y++) {
		    for (int x = 0; x < w; x++) {
		      int p = (((x + xo) / size) + ((y + yo) / size)) % 2;
		      int idx = y * w + x;
		      r.values[0][idx] = (p == parity) ? 1.0f : 0.0f;
		    }
		  }
		  Acc(&r);
		  iters++;
		  // don't let this one be too much longer.
		  // but these patterns do seem to be important at
		  // coaxing some features to activate.
		  if (iters > (SAMPLES_PER_THREAD * 2)) {
		    printf("Exited checkerboard early.");
		    return m;
		  }
		}
	      }
	    }
	  }
	  
	} else {
	  // General case is uniformly random.
	  for (int i = 0; i < SAMPLES_PER_THREAD; i++) {
	    Stimulation r(net);
	    for (float &v : r.values[0])
	      v = RandDouble(rc);

	    // (no reason to accumulate the input layer)
	    
	    for (int src_layer = 0; src_layer < r.values.size() - 1; src_layer++) {
	      net.RunForwardLayer(&r, src_layer);
	      AccumulateLayer(r, &m, src_layer + 1);

	      // If rerandomization is on, just replace the destination layer with
	      // random noise.
	      if (RERANDOMIZE && rc->Byte() < 64) {
		for (float &v : r.values[src_layer + 1]) {
		  // Here, activation ranges from -1 to 1?
		  v = (RandDouble(rc) * 2.0) - 1.0;
		}
	      }
	    }
				  
	    Acc(&r);
	  }
	}
	return m;
      }, THREADS);

  for (ArcFour *rc : rcs) delete rc;
  
  Stimulation total(net);
  for (const Stimulation &m : maxes) Accumulate(m, &total);
  return total;
}

// Returns remapping of node ids on this layer, so that the next layer
// can be patched up. If an index is missing from the map, it was
// deleted.
static std::unordered_map<int, int>
CullLayer(ArcFour *rc,
	  Network *net,
	  int layer_idx,
	  const vector<float> &max_activation) {

  EZLayer ez(*net, layer_idx);
  CHECK(max_activation.size() == ez.nodes.size());
  
  std::unordered_map<int, int> remapping;
  int removed = 0;
  {
    vector<EZLayer::Node> out;
    out.reserve(ez.nodes.size());
    for (int i = 0; i < ez.nodes.size(); i++) {
      if (max_activation[i] >= THRESHOLD) {
	// Keep.
	remapping[i] = out.size();
	out.push_back(ez.nodes[i]);
      } else {
	// Otherwise, it's discarded.
	removed++;
      }
    }
    ez.nodes = std::move(out);
  }

  ez.MakeWidthHeight();
  printf("Removed %d nodes. New size %dx%d = %d.\n",
	 removed, ez.width, ez.height, ez.nodes.size());

  ez.Repack(net, layer_idx);
  
  return remapping;
}

static void FixupLayer(ArcFour *rc, Network *net, int layer_idx,
		       const std::unordered_map<int, int> &remapping) {
  EZLayer ez(*net, layer_idx);

  // Random permutation of valid indices in the previous layer, which
  // we can use to replace indices that were deleted.
  vector<int> available;
  for (int i = 0; i < net->num_nodes[layer_idx]; i++)
    available.push_back(i);
  Shuffle(rc, &available);

  // First, make a pass over all nodes to find the minimum number that
  // were deleted. This may be zero. But if we deleted from every node,
  // we can reduce ipn instead of replacing.
  int min_deleted = ez.ipn;
  for (const EZLayer::Node &node : ez.nodes) {
    int deleted = 0;
    for (const EZLayer::OneIndex &oi : node.inputs)
      if (remapping.find(oi.index) == remapping.end())
	deleted++;
    min_deleted = std::min(min_deleted, deleted);
  }

  ez.ipn -= min_deleted;
  
  printf("We deleted at least %d input(s) from every node. ipn now %d\n",
	 min_deleted, ez.ipn);

  // Now remap, and add new indices wherever we're less than ipn.
  for (EZLayer::Node &node : ez.nodes) {
    vector<EZLayer::OneIndex> new_inputs;
    std::unordered_set<int> used;
    for (const EZLayer::OneIndex &oi : node.inputs) {
      auto it = remapping.find(oi.index);
      if (it == remapping.end()) {
	// Deleted. Skip.
      } else {
	EZLayer::OneIndex noi;
	noi.index = it->second;
	noi.weight = oi.weight;
	new_inputs.push_back(noi);
	used.insert(noi.index);
      }
    }

    CHECK(new_inputs.size() <= ez.ipn);
    int ai = 0;
    while (new_inputs.size() < ez.ipn) {
      CHECK(ai < available.size()) << "Ran out of available indices? "
	"I think this should not be possible because we already reduced "
	"ipn to account for deleted nodes.";
      if (used.find(available[ai]) == used.end()) {
	EZLayer::OneIndex noi;
	noi.index = available[ai];
	noi.weight = 0.0f;
	new_inputs.push_back(noi);
	used.insert(noi.index);
      }
      ai++;
    }
    Shuffle(rc, &available);

    node.inputs = std::move(new_inputs);
  }
    
  ez.Repack(net, layer_idx);
}

static void CullNetwork(ArcFour *rc, Network *net,
			const Stimulation &max_stim) {
  net->StructuralCheck();

  CHECK(CULL_LAYER < net->layers.size()) << "CULL_LAYER out of bounds?";
  CHECK(CULL_LAYER != net->layers.size() - 1) << "Not culling the last "
    "layer because it's probably a mistake; the structure of the problem "
    "being trained would have to change. But you can try it by removing "
    "this check if you want.";

  // If missing from the remapping, it is deleted.
  std::unordered_map<int, int> remapping =
    CullLayer(rc, net, CULL_LAYER, max_stim.values[CULL_LAYER + 1]);
  if (CULL_LAYER + 1 < net->layers.size())
    FixupLayer(rc, net, CULL_LAYER + 1, remapping);
  
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


static uint8 FloatByte(float f) {
  int x = roundf(f * 255.0f);
  return std::clamp(x, 0, 255);
}

int main(int argc, char **argv) {

  CHECK(argc >= 2) << "\n\nUsage:\ncull.exe net.val [output.val]";
  
  Timer model_timer;
  std::unique_ptr<Network> net{Network::ReadNetworkBinary(argv[1])};
  fprintf(stderr, "Loaded model in %.2fs\n",
	  model_timer.MS() / 1000.0);
  fflush(stderr);

  Timer stimulate_timer;
  ArcFour rc(StringPrintf("%lld,%lld,%lld",
			  (int64)time(nullptr),
			  net->rounds,
			  net->Bytes()));
  const Stimulation max_stim = RandomlyStimulate(&rc, *net);
  
  // TODO: Also allow loading this same thing from the training
  // process (could be via an image?). The random stimulation has
  // shown weakness at triggering activations of non-dead nodes
  // in simple tests.
  
  fprintf(stderr, "Randomly stimulated in %.2fs\n",
	  stimulate_timer.MS() / 1000.0);

  // Write image of max stimulation values.
  {
    constexpr int MARGIN = 4;
    int mw = 0, th = 0;
    for (int layer_idx = 0;
	 layer_idx < max_stim.values.size();
	 layer_idx++) {
      CHECK(layer_idx < net->width.size());
      CHECK(net->channels[layer_idx] == 1) << "This process does not "
	"support channels != 1. Should preprocess the network to multiply "
	"width by channels so we can set channels == 1.";
      mw = std::max(mw, net->width[layer_idx]);
      th += net->height[layer_idx];
    }

    int WIDTH = mw;
    int HEIGHT = th + max_stim.values.size() * MARGIN;
    ImageRGBA img(WIDTH, HEIGHT);
    img.Clear32(0x00007FFF);
    int starty = 0;
    for (int layer_idx = 0;
	 layer_idx < max_stim.values.size();
	 layer_idx++) {
      int w = net->width[layer_idx];
      int h = net->height[layer_idx];
      for (int y = 0; y < h; y++) {
	for (int x = 0; x < w; x++) {
	  float f = max_stim.values[layer_idx][y * w + x];
	  uint8 v = FloatByte(f);
	  img.SetPixel(x, starty + y, v, v, v, 0xFF);
	}
      }
      starty += h + MARGIN;
    }

    img.Save("random-stimulation.png");
  }

  Timer cull_timer;
  CullNetwork(&rc, net.get(), max_stim);
  fprintf(stderr, "Culled model in %.2fs\n",
	  cull_timer.MS() / 1000.0);
  fflush(stderr);

  string outfile = argc > 2 ? argv[2] : "net-culled.val";
  Network::SaveNetworkBinary(*net, outfile);

  return 0;
}

