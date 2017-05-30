// This code was forked from ../redi, so read there for some
// development history / thoughts.

#include "../cc-lib/sdl/sdlutil.h"
#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/chars.h"
#include "../cc-lib/sdl/font.h"

#include <CL/cl.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <chrono>
#include <algorithm>
#include <tuple>
#include <utility>
#include <set>
#include <vector>
#include <map>
#include <unordered_set>
#include <deque>

#include "../cc-lib/base/stringprintf.h"
#include "../cc-lib/base/logging.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/util.h"
#include "../cc-lib/stb_image.h"
#include "../cc-lib/stb_image_write.h"
#include "../cc-lib/vector-util.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"
#include "../cc-lib/base/macros.h"
#include "../cc-lib/color-util.h"
#include "../cc-lib/image.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"

#include "clutil.h"
#include "timer.h"
#include "constants.h"

using namespace std;

using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;

using uint32 = uint32_t;
using uint64 = uint64_t;

// For checks in performance-critical code that should be skipped when
// we're confident the code is working and want speed.
#if 0
# define ECHECK(a)
# define ECHECK_EQ(a, b)
# define ECHECK_LT(a, b)
# define ECHECK_GT(a, b)
# define ECHECK_LE(a, b)
# define ECHECK_GE(a, b)
#else
# define ECHECK(a) CHECK(a)
# define ECHECK_EQ(a, b) CHECK_EQ(a, b)
# define ECHECK_LT(a, b) CHECK_LT(a, b)
# define ECHECK_GT(a, b) CHECK_GT(a, b)
# define ECHECK_LE(a, b) CHECK_LE(a, b)
# define ECHECK_GE(a, b) CHECK_GE(a, b)
#endif

// Graphics.
#define FONTWIDTH 9
#define FONTHEIGHT 16
static Font *font = nullptr;
#define SCREENW 1920
#define SCREENH 1280
static SDL_Surface *screen = nullptr;

  // NES NTSC Palette, in RGB triplets.
static constexpr uint8 ntsc_palette[64 * 3] = {
  0x80,0x80,0x80, 0x00,0x3D,0xA6, 0x00,0x12,0xB0, 0x44,0x00,0x96,
  0xA1,0x00,0x5E, 0xC7,0x00,0x28, 0xBA,0x06,0x00, 0x8C,0x17,0x00,
  0x5C,0x2F,0x00, 0x10,0x45,0x00, 0x05,0x4A,0x00, 0x00,0x47,0x2E,
  0x00,0x41,0x66, 0x00,0x00,0x00, 0x05,0x05,0x05, 0x05,0x05,0x05,
  0xC7,0xC7,0xC7, 0x00,0x77,0xFF, 0x21,0x55,0xFF, 0x82,0x37,0xFA,
  0xEB,0x2F,0xB5, 0xFF,0x29,0x50, 0xFF,0x22,0x00, 0xD6,0x32,0x00,
  0xC4,0x62,0x00, 0x35,0x80,0x00, 0x05,0x8F,0x00, 0x00,0x8A,0x55,
  0x00,0x99,0xCC, 0x21,0x21,0x21, 0x09,0x09,0x09, 0x09,0x09,0x09,
  0xFF,0xFF,0xFF, 0x0F,0xD7,0xFF, 0x69,0xA2,0xFF, 0xD4,0x80,0xFF,
  0xFF,0x45,0xF3, 0xFF,0x61,0x8B, 0xFF,0x88,0x33, 0xFF,0x9C,0x12,
  0xFA,0xBC,0x20, 0x9F,0xE3,0x0E, 0x2B,0xF0,0x35, 0x0C,0xF0,0xA4,
  0x05,0xFB,0xFF, 0x5E,0x5E,0x5E, 0x0D,0x0D,0x0D, 0x0D,0x0D,0x0D,
  0xFF,0xFF,0xFF, 0xA6,0xFC,0xFF, 0xB3,0xEC,0xFF, 0xDA,0xAB,0xEB,
  0xFF,0xA8,0xF9, 0xFF,0xAB,0xB3, 0xFF,0xD2,0xB0, 0xFF,0xEF,0xA6,
  0xFF,0xF7,0x9C, 0xD7,0xE8,0x95, 0xA6,0xED,0xAF, 0xA2,0xF2,0xDA,
  0x99,0xFF,0xFC, 0xDD,0xDD,0xDD, 0x11,0x11,0x11, 0x11,0x11,0x11,
};
struct NTSC {
  static void Get(int idx, uint8 *r, uint8 *g, uint8 *b) {
    std::tie(*r, *g, *b) = Get(idx);
  }

  static std::tuple<uint8, uint8, uint8> Get(int idx) {
    ECHECK(idx >= 0 && idx < 64) << idx;
    return make_tuple(ntsc_palette[idx * 3 + 0],
		      ntsc_palette[idx * 3 + 1],
		      ntsc_palette[idx * 3 + 2]);
  }
};

// Maps RGB values in 0-1 to NES palette indices.
struct PaletteMap {
  // Bit resolution of color channels. 7 bits = 2MB.
  static constexpr int BITS = 7;
  static constexpr int SHIFT = 8 - BITS;
  static constexpr int RADIX = 1 << BITS;

  PaletteMap() {
    struct LAB {
      float l, a, b;
    };
    vector<LAB> neslab;
    neslab.reserve(64);
    for (int i = 0; i < 64; i++) {
      uint8 r, g, b;
      std::tie(r, g, b) = NTSC::Get(i);
      LAB lab;
      ColorUtil::RGBToLAB(r / 255.0f, g / 255.0f, g / 255.0f,
			  &lab.l, &lab.a, &lab.b);
      neslab.push_back(lab);
    }

    nearest.reserve(RADIX * RADIX * RADIX);
    for (int r = 0; r < RADIX; r++) {
      const float fr = r / (RADIX - 1.0f);
      for (int g = 0; g < RADIX; g++) {
	const float fg = g / (RADIX - 1.0f);
	for (int b = 0; b < RADIX; b++) {
	  const float fb = b / (RADIX - 1.0f);
	  float ll, aa, bb;
	  ColorUtil::RGBToLAB(fr, fg, fb, &ll, &aa, &bb);
	  float best_de = std::numeric_limits<float>::infinity();
	  int best = -1;
	  for (int i = 0; i < 64; i++) {
	    const float de = ColorUtil::DeltaE(ll, aa, bb,
					       neslab[i].l,
					       neslab[i].a,
					       neslab[i].b);
	    if (de < best_de) {
	      best_de = de;
	      best = i;
	    }
	  }
	  CHECK(best >= 0);
	  nearest.push_back(best);
	}
      }
    }
  }

 private:
  vector<uint8> nearest;
};

PaletteMap *palette_map = nullptr;

// Thread-safe, so shared between train and ui threads.
static CL *global_cl = nullptr;

std::mutex print_mutex;
#define Printf(fmt, ...) do {		\
  MutexLock Printf_ml(&print_mutex);		\
  printf(fmt, ##__VA_ARGS__);			\
  } while (0);

template<class C>
static void DeleteElements(C *cont) {
  for (auto &elt : *cont) {
    delete elt;
  }
  cont->clear();
}

// Communication between threads.
static bool train_should_die = false;
std::mutex train_should_die_m;
static bool train_done = false;
std::mutex train_done_m;

static uint8 FloatByte(float f) {
  if (f <= 0.0f) return 0;
  if (f >= 1.0f) return 255;
  else return f * 255.0;
}

static constexpr float ByteFloat(uint8 b) {
  return (b / 255.0);
}

// Some thinking aloud about convolutions:
//
// For a convolution, a network with a small(er) number of inputs is
// run on multiple different segments of a larger input array. This
// is inherently spatial, and so requires some notion of "pixel". (A
// pixel may be mapped to multiple nodes, however). Say we have a
// network with a 16x16 pixel input size, which produces a 24x24
// pixel output. This can be run on any input plane of size IWxIH,
// with IW >= 16 and IH >= 16. We execute it on every rectangle ((x,
// y), (x + 15, y + 15)) where 0 <= x <= IW - 16 and 0 <= y <= IH -
// 16 and where x and y are divisible by in_x_stride and in_y_stride
// (typically 1--need to make sure this evenly divides something,
// XXX). Each execution produces a 24x24 pixel rectangle, and there
// are (IW - 16) * (IH - 16) / (in_x_stride * in_y_stride) of them.
//
// The output rectangles are combined by overlapping them, at
// intervals of out_x_stride and out_y_stride. If these are also 1,
// the output is barely any bigger: On the x dimension, we have IW -
// 16 of them, each overlapping on consecutive pixels, so this just
// has width IW - 16 + 24, or IW + 8. In the middle of the output
// image, the pixels receive 24*24 overlapping output values. We
// should average these values, so as not to burden the network
// itself with distinguishing the edge outputs (which get as few as
// 1 pixel to average; only one rectangle touches a corner) from the
// center. (It would be a simple matter for it to learn a constant
// scaling factor.) This may require a bit of smarts in computing
// the errors/weight updates. Anyway, the in_ and out_ strides should
// probably be related to the amount of inflation or compression that
// the network itself does, so in the case of 16x16->24x24, maybe
// strides of 2->3 would be right.
//
// Now note that a regular layer is just a special case of a
// convolutional one, where W=IW, H=IH, and in_stride=1 (or,
// anything). The output size can also be different from the input,
// and they don't need to have any special relationship; we only
// need the strides to divide the sizes, and 1 always will. It is
// maybe useful to treat regular layers as special cases of
// convolutional in the C++ code, but in the performance-sensitive
// kernel code we probably shouldn't. For example, there's no point
// in performing an average operation when there can be only 1
// contributor to each pixel, and generating these averages may
// involve computing intermediate outputs.

// So let's say that a System is a spatial arrangement of networks.
// The System understands pixels (is spatial) and the network is
// just about interconnections of nodes. The System parameters are
// static; all we learn during the training process are bias terms
// and weights in the networks. However, training still operates on
// the System level, because during the forward pass we need to
// perform the convolution operation (running sub-networks on
// multiple rectangles and averaging); during the error propagation
// we need to do the inverse; and the other steps are probably
// impacted as well.

struct SystemConfiguration {
  // idea: Something that flattens the tree-structured configuration
  // into vector, by assigning each node an id in [0, radix)?
};

struct System;

// Stimulation of a single System. This is basically activations for
// an input and output layer, plus anything that is needed internally.
struct GPUStimulation {
  // Deletes any memory allocated on the GPU.
  virtual ~GPUStimulation();
  // How many bytes are used, approximately? Used for diagnostics
  // or maybe some day scheduling.
  virtual int64 Size() const;

  // XXX sub-stimulations?

  // Every stimulation belongs to a single system instance.
  System *system = nullptr;
};

struct GPUErrors {
  // Deletes any memory allocated on the GPU.
  virtual ~GPUErrors();
  virtual int64 Size() const;

  // Every Errors belongs to a single system instance, and came from
  // a single stimulation instance. (XXX true, but do we need the
  // stimulation pointer? I think we may want to delete the stimulation
  // as soon as we have errors.)
  System *system = nullptr;
  Stimulation *stimulation = nullptr;
};


// Note that with this setup we get no (GPU-native) parallelism across
// multiple stimulations (or errors/updates), but
//  - They can still be run in parallel CPU threads, with two running
//    the same kernel at the same time. We just may not be able to make
//    the best use of GPU data parallelism in this situation.
//  - There is typically plenty of on-GPU parallelism within the
//    execution of a system on a single stimulation.
struct System {
  // A System must have fixed input/ouput dimensions. A pixel consists
  // of one or more channels, e.g., 3 for RGB or 1 for "brightness"
  // only. (Note that a pixel could represent other things, like
  // an audio sample.)
  int input_width, input_height, input_channels;
  int output_width, output_height, output_channels;

  // The hardest part of doing this well will be to arrange the
  // network (actually, the dynamic state from stimulation, error, and
  // updates) in GPU memory in such a way that we can avoid copying
  // between subsystems. But let's postpone that problem, instead just
  // thinking about making something that works.

  // Allocate a new uninitialized stimulation for this system.
  virtual GPUStimulation *NewStimulation() = 0;
  // Maybe should be member of the stimulation?
  virtual GPUErrors *NewErrors(GPUStimulation *stim) = 0;
  
  // Initialize the stimulation from the results of the previous
  // phase; typically this is a single previous system. In the case of
  // something like an input layer, it may do nothing (because it has
  // already been initialized?).
  virtual void Initialize(
      const vector<GPUStimulation *> &prev, GPUStimulation *stim) = 0;
  // Run the forward pass resulting in a stimulation that is completely
  // populated.
  virtual void Forward(GPUStimulation *stim) = 0;

  virtual void PropagateErrors(
      
  
};


#define NEIGHBORHOOD 1  
struct NetworkConfiguration {
  const int num_layers = 5;
  // Note that these must have num_layers + 1 entries.
  // The number of nodes in each layer is width * height * channels.
  // Channels tells us how many nodes there are per logical pixel;
  // for color output it's typical that this is 3. All that channels
  // really do is tell us how to align inputs when creating the
  // node indices during initialization. (And of course in the
  // input and output layers, it's expected that the channels will
  // be initialized/read in some meaningful way.)
  const vector<int> width =   { 256, 128, 64, 128, 256, 256, };
  const vector<int> height =  { 240, 120, 60, 120, 240, 240, };
  const vector<int> channels = { 1,   1,   1,  2,   3,   3, };
  // For 1 gigabyte layer sizes:
  // 2^30 = 1GB        = 1073741824
  //   / 4-byte floats = 268435456
  //   / 256 / 240     = 4396
  // const vector<int> indices_per_node = { 1024, 1024, 1024, 1024, };
  const vector<int> indices_per_channel = { 16, 16, 16, 32, 32, };
  // num_nodes = width * height * channels
  // indices_per_node = indices_per_channel * channels
  vector<int> num_nodes;
  vector<int> indices_per_node;
  NetworkConfiguration() {
    CHECK_EQ(width.size(), height.size());
    CHECK_EQ(num_layers + 1, height.size());
    CHECK_EQ(num_layers, indices_per_channel.size());
    for (int i = 0; i < num_layers + 1; i++) {
      num_nodes.push_back(width[i] * height[i] * channels[i]);
    }
    for (int i = 0; i < indices_per_channel.size(); i++) {
      indices_per_node.push_back(channels[i + 1] * indices_per_channel[i]);
    }
  }
};

struct Network {
  // Creates arrays of the appropriate size, but all zeroes. Note that this uninitialized
  // network is invalid, since the inverted indices are not correct.
  Network(vector<int> num_nodes, vector<int> indices_per_node) :
      num_layers(num_nodes.size() - 1),
      num_nodes(num_nodes) {
    CHECK(num_nodes.size() >= 1) << "Must include input layer.";
    CHECK_EQ(num_layers, indices_per_node.size());
    layers.resize(num_layers);
    for (int i = 0; i < num_layers; i++) {
      Layer &layer = layers[i];
      layer.indices_per_node = indices_per_node[i];
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
    this->layers = other.layers;
    this->inverted_indices = other.inverted_indices;
  }

  // Just used for serialization. Whenever changing the interpretation
  // of the data in an incomplete way, please change.
  static constexpr uint32 FORMAT_ID = 0x2700072BU;

  // The number of "real" layers, that is, not counting the input.
  const int num_layers;

  // num_layers + 1. num_nodes[0] is the size of the input layer.
  vector<int> num_nodes;

  struct Layer {
    int indices_per_node;
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
  Printf("Reading [%s]\n", filename.c_str());
  FILE *file = fopen(filename.c_str(), "rb");
  if (file == nullptr) {
    printf("  ... failed. If it's present, there may be a "
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
  // These values determine the size of the network vectors.
  int file_num_layers = Read32();
  CHECK_GE(file_num_layers, 0);
  printf("%s: %d layers.\n", filename.c_str(), file_num_layers);
  vector<int> num_nodes(file_num_layers + 1, 0);
  printf("%s: num nodes: ", filename.c_str());
  for (int i = 0; i < file_num_layers + 1; i++) {
    num_nodes[i] = Read32();
    printf("%d ", num_nodes[i]);
  }
  printf("\n%s: indices per node: ", filename.c_str());

  vector<int> indices_per_node(file_num_layers, 0);
  for (int i = 0; i < file_num_layers; i++) {
    indices_per_node[i] = Read32();
    printf("%d ", indices_per_node[i]);
  }
  printf("\n");

  std::unique_ptr<Network> net{new Network{num_nodes, indices_per_node}};
  net->rounds = round;

  // Read Layer structs.
  for (int i = 0; i < file_num_layers; i++) {
    for (int j = 0; j < net->layers[i].indices.size(); j++) {
      net->layers[i].indices[j] = Read32();
    }
    ReadFloats(&net->layers[i].weights);
    ReadFloats(&net->layers[i].biases);
  }

  fclose(file);
  Printf("Read from %s.\n", filename.c_str());

  // Now, fill in the inverted indices. These are not stored in the file.

  printf("Invert index:\n");
  ComputeInvertedIndices(net.get());
  printf("Check it:\n");
  CheckInvertedIndices(*net);

  return net.release();
}

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
  Write32(net.num_layers);
  for (const int i : net.num_nodes) Write32(i);
  for (const Network::Layer &layer : net.layers) Write32(layer.indices_per_node);

  for (const Network::Layer &layer : net.layers) {
    for (const uint32 idx : layer.indices) Write32(idx);
    WriteFloats(layer.weights);
    WriteFloats(layer.biases);
  }

  // Inverted indices are not written.
  Printf("Wrote %s.\n", filename.c_str());
  fclose(file);
}

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
};

struct Errors {
  explicit Errors(const Network &net) : num_layers(net.num_layers),
					num_nodes(net.num_nodes) {
    error.resize(num_layers);
    for (int i = 0; i < error.size(); i++) {
      error[i].resize(num_nodes[i + 1], 0.0f);
    }
  }
  const int num_layers;
  // The first entry here is unused (it's the size of the input layer,
  // which doesn't get errors), but we keep it like this to be
  // consistent with Network and Stimulation.
  const vector<int> num_nodes;
  int64 Bytes() const {
    int64 ret = sizeof *this;
    for (int i = 0; i < error.size(); i++)
      ret += sizeof error[i] + sizeof error[i][0] * error[i].size();
    return ret;
  }

  // These are the delta terms in Mitchell. We have num_layers of
  // them, where the error[0] is the first real layer (we don't
  // compute errors for the input) and error[num_layers] is the error
  // for the output.
  vector<vector<float>> error;
};

// Network that lives entirely on the GPU, but can be copied back to
// the Network object.
struct NetworkGPU {
  // XXX DESTRUCTOR.
  NetworkGPU(CL *cl, Network *net) : cl(cl), net(net) {

    layers.resize(net->layers.size());
    for (int layer = 0; layer < net->layers.size(); layer++) {
      layers[layer].indices =
	MoveMemoryToGPU(cl->context, cl->queue, true, &net->layers[layer].indices);
      layers[layer].weights =
	MoveMemoryToGPU(cl->context, cl->queue, false, &net->layers[layer].weights);
      layers[layer].biases =
	MoveMemoryToGPU(cl->context, cl->queue, false, &net->layers[layer].biases);
    }

    inverted_indices.resize(net->inverted_indices.size());
    for (int layer = 0; layer < net->layers.size(); layer++) {
      inverted_indices[layer].start =
	MoveMemoryToGPUConst(cl->context, cl->queue, net->inverted_indices[layer].start);
      inverted_indices[layer].length =
	MoveMemoryToGPUConst(cl->context, cl->queue, net->inverted_indices[layer].length);
      inverted_indices[layer].output_indices =
	MoveMemoryToGPUConst(cl->context, cl->queue,
			     net->inverted_indices[layer].output_indices);
    }

    clFinish(cl->queue);
  }

  // Read the weights and biases (which is the only thing that can change) from
  // GPU back to the Network object. Not thread safe!
  void ReadFromGPU() {
    for (int layer = 0; layer < net->layers.size(); layer++) {
      ReadTo(layers[layer].weights, &net->layers[layer].weights);
      ReadTo(layers[layer].biases, &net->layers[layer].biases);
    }
    clFinish(cl->queue);
  }

  // Like CopyBufferFromGPUTo, but don't wait for the command to finish.
  template<class T>
  void ReadTo(cl_mem buf, vector<T> *vec) {
    CHECK_SUCCESS(clEnqueueReadBuffer(cl->queue, buf, CL_TRUE, 0, sizeof (T) * vec->size(),
				      vec->data(),
				      // No wait-list or event.
				      0, nullptr,
				      nullptr));
  }

  struct Layer {
    // Const
    cl_mem indices;
    cl_mem weights;
    cl_mem biases;
  };

  struct InvertedIndices {
    // Const
    cl_mem start;
    // Const
    cl_mem length;
    // Const
    cl_mem output_indices;
  };

  vector<Layer> layers;
  vector<InvertedIndices> inverted_indices;

  CL *cl;
  Network *net;
 private:
  DISALLOW_COPY_AND_ASSIGN(NetworkGPU);
};

// Data on the GPU for a single example in a single training round. Can
// be reused across rounds.
struct TrainingRoundGPU {
  TrainingRoundGPU(CL *cl, const Network &net) : cl(cl), net(&net) {
    for (int i = 0; i < net.num_layers + 1; i++) {
      stimulations.push_back(
	  CreateUninitializedGPUMemory<float>(cl->context, net.num_nodes[i]));
    }

    for (int i = 0; i < net.num_layers; i++) {
      errors.push_back(
	  CreateUninitializedGPUMemory<float>(cl->context, net.num_nodes[i + 1]));
    }

    expected = CreateUninitializedGPUMemory<float>(cl->context,
						   net.num_nodes[net.num_layers]);
  }

  void LoadInput(const vector<float> &inputs) {
    CHECK_EQ(inputs.size(), net->num_nodes[0]);
    CopyBufferToGPU(cl->queue, inputs, stimulations[0]);
  }

  void LoadExpected(const vector<float> &values) {
    CHECK_EQ(values.size(), net->num_nodes[net->num_layers]);
    CopyBufferToGPU(cl->queue, values, expected);
  }

  // Way to export Errors?
  void ExportStimulation(Stimulation *stim) {
    CHECK_EQ(stim->values.size(), stimulations.size());
    for (int i = 0; i < stim->values.size(); i++) {
      CopyBufferFromGPUTo(cl->queue, stimulations[i], &stim->values[i]);
    }
  }

  // num_nodes + 1 layers. 0th is input, final is the output.
  vector<cl_mem> stimulations;
  // num_nodes layers.
  vector<cl_mem> errors;
  // Size of final stimulation.
  cl_mem expected;

  ~TrainingRoundGPU() {
    for (cl_mem m : stimulations) {
      CHECK_SUCCESS(clReleaseMemObject(m));
    }
    for (cl_mem m : errors) {
      CHECK_SUCCESS(clReleaseMemObject(m));
    }
    CHECK_SUCCESS(clReleaseMemObject(expected));
  }

  CL *cl;
  const Network *net;
private:
  DISALLOW_COPY_AND_ASSIGN(TrainingRoundGPU);
};

struct ForwardLayerCL {
  explicit ForwardLayerCL(CL *cl) : cl(cl) {
    const string kernel_src =
      Util::ReadFile("constants.h") + "\n" +
      Util::ReadFile("forwardlayer.cl");

    std::tie(program, kernel) = cl->BuildOneKernel(kernel_src, "ForwardLayer");
  }

  struct ForwardContext {
    ForwardContext(ForwardLayerCL *parent, NetworkGPU *net_gpu, int layer) :
      parent(parent), net_gpu(net_gpu), layer(layer) {
      indices = net_gpu->layers[layer].indices;
      weights = net_gpu->layers[layer].weights;
      biases = net_gpu->layers[layer].biases;
    }

    // TODO: Do we really want to share the same command queue across threads?
    // Presumably clFinish can't tell "this thread's commands" apart from others,
    // so we may be prematurely waiting/running other thread's work.
    void Forward(TrainingRoundGPU *train) {
      ECHECK_LT(layer + 1, train->stimulations.size());

      CL *cl = parent->cl;

      cl_mem src_values = train->stimulations[layer];
      cl_mem dst_values = train->stimulations[layer + 1];

      // Printf("Setup kernel..\n");

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	MutexLock ml(&parent->m);

	cl_int indices_per_node = net_gpu->net->layers[layer].indices_per_node;
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 0, sizeof (cl_int), (void *)&indices_per_node));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 1, sizeof (cl_mem), (void *)&src_values));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 2, sizeof (cl_mem), (void *)&indices));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 3, sizeof (cl_mem), (void *)&weights));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 4, sizeof (cl_mem), (void *)&biases));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 5, sizeof (cl_mem), (void *)&dst_values));

	size_t global_work_offset[] = { 0 };
        size_t global_work_size[] = { (size_t)(net_gpu->net->num_nodes[layer + 1]) };
	// Printf("Run FL Kernel.\n");
	Timer kernel_timer;
	CHECK_SUCCESS(clEnqueueNDRangeKernel(cl->queue, parent->kernel,
					     // work dimensions
					     1,
					     // global work offset
					     global_work_offset,
					     // global work size
					     global_work_size,
					     // local work size
					     nullptr,
					     // no wait list
					     0, nullptr,
					     // no event
					     nullptr));
	clFinish(cl->queue);
	kernel_ms += kernel_timer.MS();
      }
    }

    ~ForwardContext() {
    }

    cl_mem indices;
    cl_mem weights;
    cl_mem biases;
    ForwardLayerCL *parent = nullptr;
    NetworkGPU *net_gpu = nullptr;
    const int layer;
    double kernel_ms = 0.0;
  };

  ~ForwardLayerCL() {
    CHECK_SUCCESS(clReleaseKernel(kernel));
    CHECK_SUCCESS(clReleaseProgram(program));
  }

  CL *cl = nullptr;
  // Owned:
  cl_program program;
  cl_kernel kernel;

  std::mutex m;
};

// Set the error values; this is almost just a memcpy so don't bother doing it
// on GPU.
struct SetOutputErrorCL {
  explicit SetOutputErrorCL(CL *cl) : cl(cl) {
    const string kernel_src =
      Util::ReadFile("constants.h") + "\n" +
      Util::ReadFile("setoutputerror.cl");
    std::tie(program, kernel) = cl->BuildOneKernel(kernel_src, "SetOutputError");
  }

  struct Context {
    Context(SetOutputErrorCL *parent, NetworkGPU *net_gpu) :
      parent(parent), net_gpu(net_gpu) {}

    void SetOutputError(TrainingRoundGPU *train) {
      CL *cl = parent->cl;

      // All three memories here have num_nodes floats.
      int num_nodes = net_gpu->net->num_nodes[net_gpu->net->num_layers];
      cl_mem actual_outputs = train->stimulations.back();
      cl_mem expected = train->expected;
      cl_mem output_error = train->errors.back();

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	MutexLock ml(&parent->m);

	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 0, sizeof (cl_mem), (void *)&actual_outputs));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 1, sizeof (cl_mem), (void *)&expected));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 2, sizeof (cl_mem), (void *)&output_error));

	size_t global_work_offset[] = { 0 };
        size_t global_work_size[] = { (size_t)(num_nodes) };

	CHECK_SUCCESS(clEnqueueNDRangeKernel(cl->queue, parent->kernel,
					     // work dimensions
					     1,
					     // global work offset
					     global_work_offset,
					     // global work size
					     global_work_size,
					     // local work size
					     nullptr,
					     // no wait list
					     0, nullptr,
					     // no event
					     nullptr));
	clFinish(cl->queue);
      }
    }

   private:
    SetOutputErrorCL *parent = nullptr;
    NetworkGPU *net_gpu = nullptr;
  };


 private:
  CL *cl = nullptr;
  // Owned:
  cl_program program;
  cl_kernel kernel;

  std::mutex m;

  DISALLOW_COPY_AND_ASSIGN(SetOutputErrorCL);
};


struct BackwardLayerCL {
  explicit BackwardLayerCL(CL *cl) : cl(cl) {
    const string kernel_src =
      Util::ReadFile("constants.h") + "\n" +
      Util::ReadFile("backwardlayer.cl");

    std::tie(program, kernel) = cl->BuildOneKernel(kernel_src, "BackwardLayer");
  }

  struct Context {
    Context(BackwardLayerCL *parent, NetworkGPU *net_gpu, int dst_layer) :
      parent(parent), net_gpu(net_gpu), dst_layer(dst_layer) {
      // CL *cl = parent->cl;

      const int gap = dst_layer;
      // const int src_layer = dst_layer - 1;

      starts = net_gpu->inverted_indices[gap].start;
      lengths = net_gpu->inverted_indices[gap].length;
      inverted_index = net_gpu->inverted_indices[gap].output_indices;
      dst_weights = net_gpu->layers[dst_layer].weights;
    }

    void Backward(TrainingRoundGPU *train) {
      CL *cl = parent->cl;

      const int gap = dst_layer;
      const int src_layer = dst_layer - 1;

      cl_mem src_output = train->stimulations[src_layer + 1];
      cl_mem dst_error = train->errors[dst_layer];

      // This is the source layer, but num_nodes is offset by one since it includes
      // the size of the input layer as element 0.
      int src_num_nodes = net_gpu->net->num_nodes[src_layer + 1];
      cl_mem src_error = train->errors[src_layer];

      CHECK_EQ(src_num_nodes, net_gpu->net->inverted_indices[gap].start.size());

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	MutexLock ml(&parent->m);

	cl_int dst_indices_per_node = net_gpu->net->layers[dst_layer].indices_per_node;
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 0, sizeof (cl_int),
				     (void *)&dst_indices_per_node));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 1, sizeof (cl_mem), (void *)&starts));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 2, sizeof (cl_mem), (void *)&lengths));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 3, sizeof (cl_mem), (void *)&inverted_index));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 4, sizeof (cl_mem), (void *)&dst_weights));
	CHECK_SUCCESS(
	    clSetKernelArg(parent->kernel, 5, sizeof (cl_mem), (void *)&src_output));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 6, sizeof (cl_mem), (void *)&dst_error));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 7, sizeof (cl_mem), (void *)&src_error));

	size_t global_work_offset[] = { 0 };
	size_t global_work_size[] = { (size_t)src_num_nodes };
	Timer kernel_timer;
	CHECK_SUCCESS(clEnqueueNDRangeKernel(cl->queue, parent->kernel,
					     // work dimensions
					     1,
					     // global work offset
					     global_work_offset,
					     // global work size
					     global_work_size,
					     // local work size
					     nullptr,
					     // no wait list
					     0, nullptr,
					     // no event
					     nullptr));
	clFinish(cl->queue);
	kernel_ms += kernel_timer.MS();
      }
    }

    ~Context() {}

    cl_mem starts, lengths, inverted_index, dst_weights;
    BackwardLayerCL *parent = nullptr;
    NetworkGPU *net_gpu = nullptr;
    const int dst_layer;
    double kernel_ms = 0.0;
  };

  ~BackwardLayerCL() {
    CHECK_SUCCESS(clReleaseKernel(kernel));
    CHECK_SUCCESS(clReleaseProgram(program));
  }

  CL *cl = nullptr;
  // Owned:
  cl_program program;
  cl_kernel kernel;

  std::mutex m;
};

struct UpdateWeightsCL {
  explicit UpdateWeightsCL(CL *cl) : cl(cl) {
    const string kernel_src =
      Util::ReadFile("constants.h") + "\n" +
      Util::ReadFile("updateweights.cl");

    std::tie(program, kernel) = cl->BuildOneKernel(kernel_src, "UpdateWeights");
  }

  struct Context {
    Context(UpdateWeightsCL *parent, NetworkGPU *net_gpu, int layer) :
      parent(parent), net_gpu(net_gpu), layer(layer) {

      layer_indices = net_gpu->layers[layer].indices;
      layer_weights = net_gpu->layers[layer].weights;
      layer_biases = net_gpu->layers[layer].biases;
    }

    void Update(float learning_rate, TrainingRoundGPU *train, int layer) {
      CL *cl = parent->cl;

      // Really can't run these in parallel because of concurrent writes to net.
      MutexLock ml(&parent->m);

      cl_mem layer_error = train->errors[layer];
      cl_mem layer_values = train->stimulations[layer];

      const int num_nodes = net_gpu->net->num_nodes[layer + 1];
      cl_int indices_per_node = net_gpu->net->layers[layer].indices_per_node;
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 0, sizeof (cl_float), (void *)&learning_rate));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 1, sizeof (cl_int), (void *)&indices_per_node));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 2, sizeof (cl_mem), (void *)&layer_error));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 3, sizeof (cl_mem), (void *)&layer_indices));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 4, sizeof (cl_mem), (void *)&layer_values));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 5, sizeof (cl_mem), (void *)&layer_weights));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 6, sizeof (cl_mem), (void *)&layer_biases));

      size_t global_work_offset[] = { 0 };
      size_t global_work_size[] = { (size_t)num_nodes };
      CHECK_SUCCESS(clEnqueueNDRangeKernel(cl->queue, parent->kernel,
					   // work dimensions
					   1,
					   // global work offset
					   global_work_offset,
					   // global work size
					   global_work_size,
					   // local work size
					   nullptr,
					   // no wait list
					   0, nullptr,
					   // no event
					   nullptr));
      clFinish(cl->queue);
    }

    void Finish() {
      CL *cl = parent->cl;
      clFinish(cl->queue);
    }

    cl_mem layer_indices, layer_weights, layer_biases;
    UpdateWeightsCL *parent = nullptr;
    NetworkGPU *net_gpu;
    const int layer;
    double kernel_ms = 0.0;
  };

  ~UpdateWeightsCL() {
    CHECK_SUCCESS(clReleaseKernel(kernel));
    CHECK_SUCCESS(clReleaseProgram(program));
  }

  CL *cl = nullptr;
  // Owned:
  cl_program program;
  cl_kernel kernel;

  std::mutex m;
};

// Make indices. This assumes that nodes are 2D "pixel" data, where on
// each layer we have width[l] * height[l] pixels, with channels[l]
// nodes per pixel. Row-major order.
//
// We mostly sample from a Gaussian near each pixel, but:
//  - we reject duplicates (inefficient),
//  - we reject pixels off the image (doesn't make sense; wrapping around
//      would work but an image is not a torus)
//  - we require that a small neighborhood around the pixel is mapped
//      directly (especially the pixel itself; this preserves spatial
//      locality and makes sure we don't have any statically dead nodes).
static void MakeIndices(const vector<int> &width,
			const vector<int> &channels,
			ArcFour *rc, Network *net) {
  CHECK_EQ(width.size(), net->num_layers + 1);
  CHECK_EQ(channels.size(), net->num_layers + 1);  
  // static constexpr int NEIGHBORHOOD = 5;

  static_assert(NEIGHBORHOOD >= 0, "must include the pixel itself.");
  auto OneNode = [](ArcFour *rc, RandomGaussian *gauss,
		    int64 *rejected, int64 *duplicate,
		    int indices_per_node,
		    int src_width, int src_height, int src_channels,
		    int dst_width, int dst_height, int dst_channels,
		    int idx) -> vector<uint32> {

    // Whenever we read the neighborhood, we include all source channels.
    CHECK((NEIGHBORHOOD * 2 + 1) * (NEIGHBORHOOD * 2 + 1) *
	  src_channels <= indices_per_node) << "neighborhood doesn't fit in indices!";
    // Which pixel is this?
    const int dst_nodes_per_row = dst_width * dst_channels;
    const int c = idx % dst_channels;
    (void)c;
    const int x = (idx % dst_nodes_per_row) / dst_channels;
    const int y = idx / dst_nodes_per_row;

    const double xf = x / (double)dst_width;
    const double yf = y / (double)dst_height;

    // Use hash set for deduplication; we re-sort for locality of access later.
    unordered_set<int> indices;
    // clips xx,yy if they are out of the image. cc must be a valid channel index.
    auto AddNodeByCoordinates = [src_width, src_height, src_channels, &indices,
				 rejected, duplicate](int xx, int yy, int cc) {
      ECHECK_GE(cc, 0);
      ECHECK_LT(cc, src_channels);
      if (xx < 0 || yy < 0 || xx >= src_width || yy >= src_height) {
	++*rejected;
	return;
      }
      int idx = (yy * src_width * src_channels) + xx * src_channels + cc;
      ECHECK_GE(idx, 0);
      ECHECK_LT(idx, src_width * src_height * src_channels);
      auto p = indices.insert(idx);
      if (!p.second) ++*duplicate;
    };

    // Find the closest corresponding pixel in the src layer; add all its channels.
    const int cx = round(xf * src_width);
    const int cy = round(yf * src_height);
    for (int ny = -NEIGHBORHOOD; ny <= NEIGHBORHOOD; ny++) {
      for (int nx = -NEIGHBORHOOD; nx <= NEIGHBORHOOD; nx++) {
        // Note that the pixel may be clipped.
	for (int nc = 0; nc < src_channels; nc++) {
	  AddNodeByCoordinates(cx + nx, cy + ny, nc);
	}
      }
    }

    CHECK_LE(indices.size(), indices_per_node);

    // XXX Select this dynamically based on how many unused nodes
    // are even left?
    #if 0
    static constexpr double stddev = 1 / 16.0;

    // Sample gaussian pixels.
    while (indices.size() < indices_per_node) {
      double dx = gauss->Next() * stddev;
      double dy = gauss->Next() * stddev;

      AddNodeByCoordinates((int)round((xf + dx) * src_width),
			   (int)round((yf + dy) * src_height));
    }
    #else

    // XXXXX
    int hood = NEIGHBORHOOD;
    while (indices.size() < indices_per_node) {
      hood++;
      const int cx = round(xf * src_width);
      const int cy = round(yf * src_height);
      for (int ny = -hood; ny <= hood; ny++) {
	for (int nx = -hood; nx <= hood; nx++) {
	  // In the interests of getting more spatial
	  // dispersion, only add one channel at random. As
	  // we expand we can try these multiple times, so
	  // pixels closer to the center are more likely to
	  // have all channels used.
	  int nc = RandTo(rc, src_channels);
	  AddNodeByCoordinates(cx + nx, cy + ny, nc);
	  if (indices.size() == indices_per_node) goto done;
	}
      }
    }
  done:;
    #endif

    CHECK_EQ(indices_per_node, indices.size());
    vector<uint32> ret;
    ret.reserve(indices_per_node);
    for (int idx : indices) {
      CHECK_GE(idx, 0);
      CHECK_LT(idx, src_width * src_height * src_channels);
      ret.push_back(idx);
    }
    return ret;
  };

  // This must access rc serially.
  vector<ArcFour *> rcs;
  for (int i = 0; i < net->num_layers; i++) rcs.push_back(Substream(rc, i));

  ParallelComp(net->num_layers, [&width, &channels, &rcs, &OneNode, &net](int layer) {
    const int indices_per_node = net->layers[layer].indices_per_node;
    Printf("Intializing %d indices for layer %d...\n", indices_per_node, layer);
    vector<uint32> *layer_indices = &net->layers[layer].indices;
    CHECK_LT(layer + 1, width.size());
    CHECK_LT(layer + 1, channels.size());
    CHECK_LT(layer + 1, net->num_nodes.size());
    const int src_width = width[layer];
    const int src_channels = channels[layer];
    CHECK_EQ(0, net->num_nodes[layer] % (src_width * src_channels));
    const int src_height = net->num_nodes[layer] / (src_width * src_channels);
    const int dst_width = width[layer + 1];
    const int dst_channels = channels[layer + 1];
    CHECK_EQ(0, net->num_nodes[layer + 1] % (dst_width * dst_channels));
    const int dst_height = net->num_nodes[layer + 1] / (dst_width * dst_channels);
    RandomGaussian gauss{rcs[layer]};
    int64 rejected = 0LL, duplicate = 0LL;
    for (int node_idx = 0; node_idx < dst_height * dst_width * dst_channels; node_idx++) {
      vector<uint32> indices = OneNode(rcs[layer], &gauss, &rejected, &duplicate,
				       indices_per_node,
				       src_width, src_height, src_channels,
				       dst_width, dst_height, dst_channels,
				       node_idx);
      // Sort them, for better locality of access later.
      std::sort(indices.begin(), indices.end());
      CHECK_EQ(indices_per_node, indices.size());
      const int start_idx = node_idx * indices_per_node;
      for (int i = 0; i < indices_per_node; i++) {
	ECHECK_LT(i, indices.size());
	ECHECK_LT(start_idx + i, layer_indices->size())
	  << "start " << start_idx
	  << " i " << i
	  << " indices size " << layer_indices->size()
	  << " indices per node "
	  << indices_per_node;
	(*layer_indices)[start_idx + i] = indices[i];
      }
      if (node_idx % 1000 == 0) {
	Printf("  %d. [%d/%d] %.1f%% (%lld rejected %lld dupe)\n",
	       layer,
	       node_idx, dst_height * dst_width * dst_channels,
	       (100.0 * node_idx) / (dst_height * dst_width * dst_channels),
	       rejected, duplicate);
      }
    }
    Printf("... done with layer %d.\n", layer);
  }, 12);

  Printf("DeleteElements:\n");
  DeleteElements(&rcs);
  Printf("Exiting MakeIndices.\n");
}

// Randomize the weights in a network. Doesn't do anything to indices.
static void RandomizeNetwork(ArcFour *rc, Network *net) {
  auto RandomizeFloats = [](float mag, ArcFour *rc, vector<float> *vec) {
    for (int i = 0; i < vec->size(); i++) {
      (*vec)[i] = mag * (RandFloat(rc) - 0.5f);
    }
  };

  // This must access rc serially.
  vector<ArcFour *> rcs;
  for (int i = 0; i < net->num_layers; i++) rcs.push_back(Substream(rc, i));

  // But now we can do all layers in parallel.
  ParallelComp(net->num_layers, [rcs, &RandomizeFloats, &net](int layer) {
    // XXX such hacks. How to best initialize?
    RandomizeFloats(powf(0.025f, layer + 1.0), rcs[layer], &net->layers[layer].biases);
    RandomizeFloats(1.0f / (net->layers[layer].indices_per_node * (layer + 5)),
		    rcs[layer], &net->layers[layer].weights);
  }, 12);

  DeleteElements(&rcs);
}


// These must be initialized before starting the UI thread!
static constexpr int NUM_VIDEO_STIMULATIONS = 7;
static constexpr int EXPORT_EVERY = 10;
std::mutex video_export_m;
int current_round = 0;
double rounds_per_second = 0.0;
vector<Stimulation> current_stimulations;
Network *current_network = nullptr;
static bool allow_updates = true;
static bool dirty = true;

static void ExportRound(int r) {
  MutexLock ml(&video_export_m);
  if (allow_updates) {
    current_round = r;
    // dirty = true;
  }
}

static void ExportRoundsPerSec(double rps) {
  MutexLock ml(&video_export_m);
  if (allow_updates) {
    rounds_per_second = rps;
    // dirty = true;
  }
}

static void ExportNetworkToVideo(const Network &net) {
  MutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK(current_network != nullptr);
    current_network->CopyFrom(net);
    dirty = true;
  }
}

static void ExportStimulusToVideo(int example_id, const Stimulation &stim) {
  MutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK_GE(example_id, 0);
    CHECK_LT(example_id, current_stimulations.size());
    current_stimulations[example_id].CopyFrom(stim);
    dirty = true;
  }
}

static void UIThread() {
  const NetworkConfiguration config;
  int mousex = 0, mousey = 0;
  (void)mousex; (void)mousey;
  for (;;) {
    // int round = ReadWithLock(&video_export_m, &current_round);
    {
      MutexLock ml(&video_export_m);
      if (dirty) {
	sdlutil::clearsurface(screen, 0x0);
	string menu = StringPrintf("  round ^3%d ^1|  ^3%0.4f^0 rps",
				   current_round,
				   rounds_per_second);

	for (int s = 0; s < NUM_VIDEO_STIMULATIONS; s++) {
	  const Stimulation &stim = current_stimulations[s];
	  CHECK(stim.values.size() == config.num_layers + 1);
	  int ystart = 4;
	  for (int l = 0; l < stim.values.size(); l++) {
	    for (int y = 0; y < config.height[l]; y++) {
	      for (int x = 0; x < config.width[l]; x++) {
		int yy = ystart + y;
		if (yy >= SCREENH) break;
		
		// XXX allow other sizes -- find the max width!
		int xx = 4 + s * 260 + x;
		int cidx = y * config.width[l] * config.channels[l] + x * config.channels[l];
		switch (config.channels[l]) {
		case 0: break;
		case 1: {
		  const uint8 v = FloatByte(stim.values[l][cidx]);
		  sdlutil::drawpixel(screen, xx, yy, v, v, v);
		  break;
		}
		case 2: {
		  const uint8 r = FloatByte(stim.values[l][cidx + 0]);
		  const uint8 g = FloatByte(stim.values[l][cidx + 1]);
		  sdlutil::drawpixel(screen, xx, yy, r, g, 0);
		  break;
		}
		default:
		  // If more than 3, later ones are just ignored.
		case 3: {
		  const uint8 r = FloatByte(stim.values[l][cidx + 0]);
		  const uint8 g = FloatByte(stim.values[l][cidx + 1]);
		  const uint8 b = FloatByte(stim.values[l][cidx + 2]);
		  sdlutil::drawpixel(screen, xx, yy, r, g, b);
		  break;
		}
		}
	      }
	    }
	    ystart += config.height[l];
	    ystart += 4;
	  }
	}

	font->draw(2, 2, menu);
	SDL_Flip(screen);
	dirty = false;
      }
    }

    if (ReadWithLock(&train_done_m, &train_done)) {
      Printf("UI thread saw that training finished.\n");
      return;
    }

    SDL_Event event;
    if (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
	Printf("QUIT.\n");
	return;

      } else if (event.type == SDL_MOUSEMOTION) {
	SDL_MouseMotionEvent *e = (SDL_MouseMotionEvent*)&event;

	mousex = e->x;
	mousey = e->y;
	// (and immediately redraw)

      } else if (event.type == SDL_KEYDOWN) {
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  Printf("ESCAPE.\n");
	  return;
	default:;
	}
      }
    } else {
      SDL_Delay(1000);
    }
  }
}


static void TrainThread() {
  Timer setup_timer;

  // "Eval" here means generating frames of a movie to make a video.
  // If EVAL_ONLY is true, we skip training, and write a frame on every round
  // (since that's all that the round does).
  static constexpr bool EVAL_ONLY = true;
  // Should be the index of the next eval-%d.png to write, for
  // multi-session training (XXX get this from a checkpoint file or something).
  const int FRAMES_ALREADY_DONE = []() {
    string nextframe = Util::ReadFile("eval/nextframe.txt");
    return atoi(nextframe.c_str());
  }();
  printf("Next eval frame: %d\n", FRAMES_ALREADY_DONE);
  
  // Should be the movie index (can be anything within bounds) that this segment
  // of the movie starts at, for example to show two different models consecutively
  // without having to show the same eval gameplay over and over. FRAMES_ALREADY_DONE
  // will be taken into account.
  static constexpr int EVAL_MOVIE_START = 0; // = 3533;  // For mario, End of world 1-1.
  // const string eval_romfile = "mario.nes";
  // const string eval_moviefile = "mario-long-again.fm2";
  // static constexpr int EVAL_MOVIE_START = 0;
  const string eval_romfile = "metroid.nes";
  const string eval_moviefile = "metroid2.fm2";

  // Source game for training.
  const string train_romfile = "mario.nes";
  const string train_moviefile = "mario-long-three.fm2";

  // To generate training examples, we sample randomly from the input movie and
  // then perturb the state a little. Snapshots every few frames allow us to
  // quickly seek within the movie, but use more RAM and make startup slower.
  static constexpr int SNAPSHOT_EVERY = 20;

  // Number of training examples per round of training.
  static constexpr int EXAMPLES_PER_ROUND = 48;
  // On a verbose round, we write a network checkpoint and maybe some
  // other stuff to disk.
  static constexpr int VERBOSE_ROUND_EVERY = 250;

  int eval_frame_num = FRAMES_ALREADY_DONE;
  int eval_movie_idx = eval_frame_num + EVAL_MOVIE_START;


  string start_seed = StringPrintf("%d  %lld", getpid(), (int64)time(NULL));
  Printf("Start seed: [%s]\n", start_seed.c_str());
  ArcFour rc(start_seed);
  rc.Discard(2000);

  // Create kernels right away so that we get any compilation errors early.
  ForwardLayerCL forwardlayer{global_cl};
  SetOutputErrorCL setoutputerror{global_cl};
  BackwardLayerCL backwardlayer{global_cl};
  UpdateWeightsCL updateweights{global_cl};

  // Load the existing network from disk or create the initial one.
  Timer initialize_network_timer;
  std::unique_ptr<Network> net{ReadNetworkBinary("net.val")};

  if (net.get() == nullptr) {
    Printf("Initializing new network...\n");
    NetworkConfiguration nc;
    net.reset(new Network(nc.num_nodes, nc.indices_per_node));
    Printf("Randomize weights:\n");
    RandomizeNetwork(&rc, net.get());
    Printf("Gen indices:\n");
    MakeIndices(nc.width, nc.channels, &rc, net.get());
    Printf("Invert indices:\n");
    ComputeInvertedIndices(net.get());
    CheckInvertedIndices(*net);

    Printf("Writing network so we don't have to do that again...\n");
    if (!EVAL_ONLY) SaveNetworkBinary(*net, "net.val");
  }

  Printf("Initialized network in %.1fms.\n", initialize_network_timer.MS());

  NetworkGPU net_gpu{global_cl, net.get()};

  if (ReadWithLock(&train_should_die_m, &train_should_die))
    return;

  Printf("Network uses %.2fMB of storage (without overhead).\n",
	 net->Bytes() / (1024.0 * 1024.0));

  // We use the same structures to hold all the stimulations and errors
  // now, on the GPU.
  vector<TrainingRoundGPU *> training;
  for (int i = 0; i < EXAMPLES_PER_ROUND; i++)
    training.push_back(new TrainingRoundGPU{global_cl, *net});

  auto ShouldDie = [&net, &eval_frame_num]() {
    bool should_die = ReadWithLock(&train_should_die_m, &train_should_die);
    if (should_die) {
      Printf("Train thread signaled death.\n");
      Printf("Saving to net.val...\n");
      if (!EVAL_ONLY) SaveNetworkBinary(*net, "net.val");
      Util::WriteFile("eval/nextframe.txt", StringPrintf("%d\n", eval_frame_num));
    }
    return should_die;
  };

  // Prepare corpus.
  printf("Generating corpus from ROM and movie...\n");
  vector<uint8> train_movie = SimpleFM2::ReadInputs(train_moviefile);
  // If unused, truncate training data to make startup faster.
  if (EVAL_ONLY) train_movie.resize(std::min((size_t)100, train_movie.size()));
  struct Snapshot {
    int movie_idx;
    vector<uint8> save;
  };

  vector<Snapshot> snapshots;
  snapshots.reserve(1 + train_movie.size() / SNAPSHOT_EVERY);

  Printf("Populating snapshots every %d frames...\n", SNAPSHOT_EVERY);
  {
    std::unique_ptr<Emulator> emu{Emulator::Create(train_romfile)};
    CHECK(emu.get() != nullptr);
    uint64 snapshot_bytes = 0ULL;
    for (int i = 0; i < train_movie.size(); i++) {
      if (i % SNAPSHOT_EVERY == 0) {
	snapshots.resize(snapshots.size() + 1);
	Snapshot *snapshot = &snapshots.back();
	snapshot->movie_idx = i;
	snapshot->save = emu->SaveUncompressed();
	snapshot_bytes += snapshot->save.size();
      }
      emu->StepFull(train_movie[i], 0);
    }
    Printf("Using %.2fMB for snapshots (save states).\n",
	   snapshot_bytes / (1024.0 * 1024.0));
  }

  std::unique_ptr<Emulator> eval_emu{Emulator::Create(eval_romfile)};
  CHECK(eval_emu.get() != nullptr) << eval_romfile;
  const vector<uint8> eval_movie = SimpleFM2::ReadInputs(eval_moviefile);
  CHECK(!eval_movie.empty()) << eval_moviefile;
  const vector<uint8> eval_start_state = eval_emu->SaveUncompressed();
  auto ShouldEmitEvalFrame = [](int round_num) {
    if (EVAL_ONLY) return true;
    if (round_num < 2000) return true;
    else if (round_num < 4000) return (round_num % 10) == 0;
    else if (round_num < 8000) return (round_num % 100) == 0;
    else return (round_num % 200) == 0;
  };

  // Number of threads to allow for simultaneous writing of frames.
  Asynchronously write_frames{EVAL_ONLY ? 2 : 8};

  if (eval_movie_idx > 0) Printf("Fast forwarding eval movie...\n");
  for (int i = 0; i < eval_movie_idx; i++) eval_emu->StepFull(eval_movie[i], 0);

  if (ShouldDie()) return;

  struct TrainingExample {
    // XXX memory, etc.
    vector<float> indices;
    vector<float> vals;
  };
  // Training examples don't depend on the learning process, so are produced
  // in a separate thread. This mutex protects the deque (only).
  std::mutex training_examples_m;
  // XXX could just be vector, actually?
  deque<TrainingExample> training_examples;

  auto PopulateExampleFromEmu = [](const Emulator &emu,
				   TrainingExample *example) {
    // XXX memory, etc.
    const uint8 *indices = emu.RawIndexedImage();
    example->vals.reserve(256 * 240 * 3);
    for (int i = 0; i < 256 * 240; i++) {
      const int idx = indices[i] & Emulator::INDEX_MASK;
      static constexpr float idx_denom = 1.0f / 63.0f;
      static constexpr float rgb_denom = 1.0f / 255.0f;
      // XXX reorder based on clustering, luminance, anything?
      example->indices.push_back((float)idx * idx_denom);
      uint8 r, g, b;
      std::tie(r, g, b) = NTSC::Get(idx);
      example->vals.push_back((float)r * rgb_denom);
      example->vals.push_back((float)g * rgb_denom);
      example->vals.push_back((float)b * rgb_denom);
    }
  };

  auto MakeTrainingExamplesThread = [train_romfile, &train_movie, &snapshots,
				     &training_examples_m, &training_examples,
				     &PopulateExampleFromEmu](int idx) {
    Printf("Training thread %d startup.\n", idx);
    ArcFour rc{StringPrintf("make_examples %d", idx)};
    std::unique_ptr<Emulator> emu{Emulator::Create(train_romfile)};

    for (;;) {
      if (ReadWithLock(&train_should_die_m, &train_should_die)) {
	return;
      }

      training_examples_m.lock();
      // Make sure we have plenty of examples so that learning doesn't stall.
      if (training_examples.size() < EXAMPLES_PER_ROUND * 2) {
	training_examples_m.unlock();

	// Choose a movie frame to seek to. We have to emulate one frame in order
	// to see an image, so here we're actually picking the frame before the
	// one that will be the training example. Therefore it can't be the last one.
	const int frame_num = RandTo(&rc, train_movie.size() - 1);
	const int snapshot_num = frame_num / SNAPSHOT_EVERY;
	const Snapshot &snapshot = snapshots[snapshot_num];
	emu->LoadUncompressed(snapshot.save);
	// Run until we reach the chosen frame, and then one more.
	CHECK_EQ(snapshot.movie_idx, snapshot_num * SNAPSHOT_EVERY);
	const int runframes = 1 + (frame_num % SNAPSHOT_EVERY);
	CHECK_LT(snapshot.movie_idx + runframes, train_movie.size());
	for (int j = snapshot.movie_idx; j < snapshot.movie_idx + runframes; j++) {
	  emu->StepFull(train_movie[j], 0);
	}

	// To get a bit more entropy in the training set, maybe run a
	// few random buttons from the movie, which may perturb the
	// state a little without producing unrealistic inputs.
	for (int randomactions = RandTo(&rc, 3) * 2; randomactions--;) {
	  const uint8 input = train_movie[RandTo(&rc, train_movie.size())];
	  for (int steps = 1 + RandTo(&rc, 12); steps--;) {
	    emu->StepFull(input, 0);
	  }
	}

	TrainingExample example;
	PopulateExampleFromEmu(*emu, &example);

	{
	  MutexLock ml(&training_examples_m);
	  training_examples.push_back(std::move(example));
	}
      } else {
	training_examples_m.unlock();
	std::this_thread::sleep_for(10ms);
      }
    }
    Printf("Training example generator exiting.\n");
  };

  // PERF could skip in eval_only mode, but it's harmless
  std::thread emu_thread_1{MakeTrainingExamplesThread, 1};
  std::thread emu_thread_2{MakeTrainingExamplesThread, 2};
  ThreadJoiner join_emu_thread_1{&emu_thread_1};
  ThreadJoiner join_emu_thread_2{&emu_thread_2};

  if (ShouldDie()) return;

  // Training round: Loop over all images in random order.
  double setup_ms = 0.0, stimulation_init_ms = 0.0, forward_ms = 0.0,
    fc_init_ms = 0.0, bc_init_ms = 0.0, kernel_ms = 0.0, backward_ms = 0.0,
    output_error_ms = 0.0, update_ms = 0.0, writing_ms = 0.0, error_history_ms = 0.0,
    eval_ms = 0.0;
  Timer total_timer;
  for (int rounds_executed = 0; ; rounds_executed++) {
    if (ShouldDie()) return;
    Printf("\n\n ** NET ROUND %d (%d in this process) **\n", net->rounds, rounds_executed);

    // When starting from a fresh network, consider this:
    //   // std::min(0.95, std::max(0.10, 4 * exp(-0.2275 * (round_number + 1)/3.0)));

    const float round_learning_rate =
      std::min(0.125, std::max(0.002, 2 * exp(-0.2275 * (net->rounds + 1)/3.0)));
    // const float round_learning_rate = 0.0025;

    Printf("Learning rate: %.4f\n", round_learning_rate);

    // This is pretty slow, but starts running less and less
    // frequently as the round increases.
    if (ShouldEmitEvalFrame(net->rounds)) {
      Timer eval_timer;
      Printf("Generating eval frame.\n");
      // Loop if we need to.
      if (eval_movie_idx == eval_movie.size()) {
	if (EVAL_ONLY) {
	  Printf("Exhausted movie in eval-only mode.\n");
	  return;
	}
	eval_movie_idx = 0;
	eval_emu->LoadUncompressed(eval_start_state);
      }

      eval_emu->StepFull(eval_movie[eval_movie_idx++], 0);
      TrainingExample example;
      PopulateExampleFromEmu(*eval_emu, &example);
      // Allocate on heap so it can be deleted asynchronously.
      TrainingRoundGPU *train = new TrainingRoundGPU{global_cl, *net};
      // Initialize input layer of stimulation.
      train->LoadInput(example.indices);
      for (int src = 0; src < net->num_layers; src++) {
	ForwardLayerCL::ForwardContext fc(&forwardlayer, &net_gpu, src);
	fc.Forward(train);
      }
      // Now write to image:
      write_frames.Run([&net, train, round_number = net->rounds,
                        eval_frame_num, round_learning_rate]() {
	Stimulation stim{*net};
	train->ExportStimulation(&stim);
	delete train;
	// Figure out how big the graphic needs to be.
	NetworkConfiguration config;
	CHECK_EQ(config.width.size(), stim.values.size());
	int width = 64;
	int height = font->height + 2;
	for (int i = 0; i < stim.values.size(); i++) {
	  height += config.height[i] + 4;
	  width = max(width, config.width[i] + 8);
	}

	// Hmm, can I safely do this outside the UI thread?
	SDL_Surface *surf = sdlutil::makesurface(width, height, true);
	sdlutil::clearsurface(surf, 0xFF000000);
	// Draw each stimulation into it...
	// XXX: Shared code with UIThread; make it a function...
	int ystart = font->height + 2;
	for (int l = 0; l < stim.values.size(); l++) {
	  for (int y = 0; y < config.height[l]; y++) {
	    for (int x = 0; x < config.width[l]; x++) {
	      int yy = ystart + y;
	      int xx = 4 + x;

	      int cidx = y * config.width[l] * config.channels[l] + x * config.channels[l];
	      switch (config.channels[l]) {
	      case 0: break;
	      case 1: {
		const uint8 v = FloatByte(stim.values[l][cidx]);
		sdlutil::drawpixel(surf, xx, yy, v, v, v);
		break;
	      }
	      case 2: {
		const uint8 r = FloatByte(stim.values[l][cidx + 0]);
		const uint8 g = FloatByte(stim.values[l][cidx + 1]);
		sdlutil::drawpixel(surf, xx, yy, r, g, 0);
		break;
	      }
	      default:
		// If more than 3, later ones are just ignored.
	      case 3: {
		const uint8 r = FloatByte(stim.values[l][cidx + 0]);
		const uint8 g = FloatByte(stim.values[l][cidx + 1]);
		const uint8 b = FloatByte(stim.values[l][cidx + 2]);
		sdlutil::drawpixel(surf, xx, yy, r, g, b);
		break;
	      }
	      }
	    }
	  }
	  ystart += config.height[l];
	  ystart += 4;
	}
	font->drawto(surf, 4, 0,
		     // XXX training time, etc.
		     StringPrintf("Round ^3%d^<, rate ^3%.3f^<",
				  round_number, round_learning_rate));
	CHECK(sdlutil::SavePNG(StringPrintf("eval/eval-%d.png", eval_frame_num), surf));
	SDL_FreeSurface(surf);
      });
      eval_frame_num++;
      eval_ms += eval_timer.MS();
    }

    if (ShouldDie()) return;
    // Everything after this is training, so skip.
    if (EVAL_ONLY) continue;


    bool is_verbose_round = 0 == ((rounds_executed /* + 1 */) % VERBOSE_ROUND_EVERY);
    if (is_verbose_round) {
      Printf("Writing network:\n");
      net_gpu.ReadFromGPU();
      SaveNetworkBinary(*net, "network-checkpoint.bin");
    }

    Printf("Export network:\n");
    ExportRound(net->rounds);
    if (rounds_executed % EXPORT_EVERY == 0) {
      net_gpu.ReadFromGPU();
      ExportNetworkToVideo(*net);
    }

    Timer setup_timer;
    Printf("Setting up batch:\n");

    vector<TrainingExample> examples;
    examples.reserve(EXAMPLES_PER_ROUND);
    do {
      if (!examples.empty()) {
	Printf("Blocked grabbing examples (still need %d)...\n",
	       EXAMPLES_PER_ROUND - examples.size());
      }
      MutexLock ml{&training_examples_m};
      while (examples.size() < EXAMPLES_PER_ROUND &&
	     !training_examples.empty()) {
	examples.push_back(std::move(training_examples.front()));
	training_examples.pop_front();
      }
    } while (examples.size() < EXAMPLES_PER_ROUND);

    Printf("Setting up expected:\n");
    vector<vector<float>> expected = Map(examples, [](const TrainingExample &te) {
      return te.vals;
    });

    setup_ms += setup_timer.MS();

    CHECK_EQ(examples.size(), expected.size());

    // TODO: may make sense to parallelize this loop somehow, so that we can parallelize
    // CPU/GPU duties?

    // Run a batch of images all the way through. (Each layer requires significant setup.)
    Printf("Creating stimulations...\n");
    Timer stimulation_init_timer;

    Printf("Setting input layer of Stimulations...\n");
    // These are just memory copies; easy to do in parallel.
    CHECK_EQ(examples.size(), training.size());
    UnParallelComp(examples.size(),
		   [&examples, &training](int i) {
		     training[i]->LoadInput(examples[i].indices);
		   }, 16);
    stimulation_init_ms += stimulation_init_timer.MS();

    if (ShouldDie()) return;
    // The loop over layers must be in serial.
    for (int src = 0; src < net->num_layers; src++) {
      Printf("FWD Layer %d: ", src);
      Timer fc_init_timer;
      ForwardLayerCL::ForwardContext fc(&forwardlayer, &net_gpu, src);
      fc_init_ms += fc_init_timer.MS();

      // PERF could be parallel, but watch out about loading the GPU with
      // too many simultaneous value src/dst buffers.
      Timer forward_timer;
      Printf("Parallelcomp...\n");
      UnParallelComp(examples.size(),
		     [&net, rounds_executed, num_examples = examples.size(),
		      &fc, &training](int example_idx) {
		     fc.Forward(training[example_idx]);
		     if (example_idx % 10 == 0) {
		       Printf("[%d/%d] (%.2f%%) ", example_idx, num_examples,
			      100.0 * example_idx / num_examples);
		     }

		     if (rounds_executed % EXPORT_EVERY == 0 &&
			 example_idx < NUM_VIDEO_STIMULATIONS) {
		       // XXX this uses unintialized/stale memory btw
		       Stimulation stim{*net};
		       training[example_idx]->ExportStimulation(&stim);
		       // Copy to screen.
		       ExportStimulusToVideo(example_idx, stim);
		     }
		   }, 12);
      forward_ms += forward_timer.MS();
      kernel_ms += fc.kernel_ms;
      Printf("\n");
    }

    const int num_examples = examples.size();
    // But, don't need to keep this allocated.
    examples.clear();

    if (ShouldDie()) return;
    Printf("Error calc.\n");
    Timer output_error_timer;
    UnParallelComp(num_examples,
		   [&setoutputerror, &net_gpu, &training, &expected](int example) {
		     training[example]->LoadExpected(expected[example]);
		     SetOutputErrorCL::Context sc{&setoutputerror, &net_gpu};
		     sc.SetOutputError(training[example]);
		     Printf(".");
		   }, 12);
    output_error_ms += output_error_timer.MS();
    Printf("\n");

    if (ShouldDie()) return;
    Printf("Backwards:\n");
    // Also serial, but in reverse.
    Timer backward_timer;
    // We do NOT propagate errors to the input layer, so dst is strictly greater than 0.
    for (int dst = net->num_layers - 1; dst > 0; dst--) {
      Printf("BWD Layer %d: ", dst);

      Timer bc_init_timer;
      BackwardLayerCL::Context bc{&backwardlayer, &net_gpu, dst};
      bc_init_ms += bc_init_timer.MS();

      UnParallelComp(num_examples,
		     [num_examples, &training, &bc](int example) {
		       bc.Backward(training[example]);
		       if (example % 10 == 0) {
			 Printf("[%d/%d] (%.2f%%) ", example, (int)num_examples,
				100.0 * example / num_examples);
		       }
		     }, 12);
      Printf("\n");
    }
    backward_ms += backward_timer.MS();


    if (ShouldDie()) return;
    Printf("Update weights:\n");
    Timer update_timer;

    // Don't parallelize! These are all writing to the same network weights. Each
    // call is parallelized, though.
    for (int layer = 0; layer < net->num_layers; layer++) {
      UpdateWeightsCL::Context uc{&updateweights, &net_gpu, layer};

      // XXX trying making this dynamic -- more nodes means slower learning?
      // (never actually ran this)
      // Maybe an off by one error here on the indices_per_node to use?
      // float layer_learning_rate = (round_learning_rate * 20f) /
      // net->layer[layer].indices_per_node;

      // PERF Faster to try to run these in parallel (maybe parallelizing memory traffic
      // with kernel execution -- but we can't run the kernels at the same time).
      for (int example = 0; example < num_examples; example++) {
	uc.Update(round_learning_rate, training[example], layer);
      }

      // Now we leave the network on the GPU, and the version in the Network object will
      // be out of date. But flush the command queue. (why? I guess make sure that we're
      // totally done writing since other parts of the code assume concurrent reads are ok?)
      uc.Finish();
      Printf("[%d/%d] = (%.2f%%) ", layer, net->num_layers, layer * 100.0 / net->num_layers);
    }
    update_ms += update_timer.MS();
    Printf("\n");

    if (ShouldDie()) return;

    net->rounds++;

    double total_ms = total_timer.MS();
    auto Pct = [total_ms](double d) { return (100.0 * d) / total_ms; };
    double denom = rounds_executed + 1;
    ExportRoundsPerSec(denom / (total_ms / 1000.0));
    Printf("Total so far %.1fs.\n"
	   "Time per round: %.1fs.\n"
	   "We spent %.1fms in setup (%.1f%%),\n"
	   "%.1fms in stimulation init (%.1f%%),\n"
	   "%.1fms in eval (main thread; amortized) (%.1f%%),\n"
	   "%.1fms in forward layer (%.1f%%),\n"
	   "%.1fms in fc init (%.1f%%),\n"
	   "%.1fms in forward layer kernel (at most; %.1f%%).\n"
	   "%.1fms in bc init (%.1f%%),\n"
	   "%.1fms in backwards pass (%.1f%%),\n"
	   "%.1fms in error for output layer (%.1f%%),\n"
	   "%.1fms in error history diagnostics (%.1f%%),\n"
	   "%.1fms in updating weights (%.1f%%),\n"
	   "%.1fms in writing images (%.1f%%),\n",
	   total_ms / 1000.0,
	   (total_ms / 1000.0) / denom,
	   setup_ms / denom, Pct(setup_ms),
	   stimulation_init_ms / denom, Pct(stimulation_init_ms),
	   eval_ms / denom, Pct(eval_ms),
	   forward_ms / denom, Pct(forward_ms),
	   fc_init_ms / denom, Pct(fc_init_ms),
	   kernel_ms / denom, Pct(kernel_ms),
	   bc_init_ms / denom, Pct(bc_init_ms),
	   backward_ms / denom, Pct(backward_ms),
	   output_error_ms / denom, Pct(output_error_ms),
	   error_history_ms / denom, Pct(error_history_ms),
	   update_ms / denom, Pct(update_ms),
	   writing_ms / denom, Pct(writing_ms));
  }

  Printf(" ** Done. **");

  WriteWithLock(&train_done_m, &train_done, true);
}


int SDL_main(int argc, char **argv) {
  if (!SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS)) {
    LOG(FATAL) << "Unable to go to BELOW_NORMAL priority.\n";
  }

  /* Initialize SDL and network, if we're using it. */
  CHECK(SDL_Init(SDL_INIT_VIDEO |
		 SDL_INIT_TIMER |
		 SDL_INIT_AUDIO) >= 0);
  fprintf(stderr, "SDL initialized OK.\n");

  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);

  screen = sdlutil::makescreen(SCREENW, SCREENH);
  CHECK(screen);

  font = Font::create(screen,
		      "font.png",
		      FONTCHARS,
		      FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font != nullptr) << "Couldn't load font.";

  global_cl = new CL;

  Printf("Initializing NES NTSC palette map.\n");
  palette_map = new PaletteMap;
  Printf("Done.\n");

  {
    // XXX Maybe UIThread should be an object, then...
    printf("Allocating video network/stimulations/data...");
    MutexLock ml(&video_export_m);
    NetworkConfiguration nc;
    current_network = new Network{nc.num_nodes, nc.indices_per_node};
    for (int i = 0; i < NUM_VIDEO_STIMULATIONS; i++) {
      current_stimulations.emplace_back(*current_network);
    }
    printf("OK.\n");
  }

  std::thread train_thread(&TrainThread);

  UIThread();

  Printf("Killing train thread (might need to wait for round to finish)...\n");
  WriteWithLock(&train_should_die_m, &train_should_die, true);
  train_thread.join();

  Printf("Train is dead; now UI exiting.\n");

  SDL_Quit();
  return 0;
}
