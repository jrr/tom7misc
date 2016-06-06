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

#include <algorithm>
#include <tuple>
#include <utility>
#include <set>
#include <vector>
#include <map>
#include <unordered_set>

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "util.h"
#include "timer.h"
#include "stb_image.h"
#include "stb_image_write.h"
#include "stb_truetype.h"

#include "threadutil.h"
#include "clutil.h"
#include "randutil.h"

#include "constants.h"

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"

using namespace std;

// Little byte machine.
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

struct ImageRGBA {
  static ImageRGBA *Load(const string &filename) {
    vector<uint8> ret;
    int width, height, bpp_unused;
    uint8 *stb_rgba = stbi_load(filename.c_str(), 
				&width, &height, &bpp_unused, 4);
    const int bytes = width * height * 4;
    ret.resize(bytes);
    if (stb_rgba == nullptr) return nullptr;
    memcpy(ret.data(), stb_rgba, bytes);
    // Does this move image data all the way in, or do we need to
    // write a move constructor manually? Better way?
    return new ImageRGBA(std::move(ret), width, height);
  }

  ImageRGBA(const vector<uint8> &rgba, int width, int height) 
    : width(width), height(height), rgba(rgba) {
    CHECK(rgba.size() == width * height * 4);
  }

  void Save(const string &filename) {
    CHECK(rgba.size() == width * height * 4);
    stbi_write_png(filename.c_str(), width, height, 4, rgba.data(), 4 * width);
  }

  ImageRGBA *Copy() const {
    return new ImageRGBA(rgba, width, height);
  }

  // TODO:: Save a vector of images of the same size in a grid.
  const int width, height;
  vector<uint8> rgba;
};

static uint8 FloatByte(float f) {
  if (f <= 0.0f) return 0;
  if (f >= 1.0f) return 255;
  else return f * 255.0;
}

static constexpr float ByteFloat(uint8 b) {
  return (b / 255.0);
}

#if 0
static void WriteLayerAsImage(const string &filename, const vector<float> &values) {
  vector<uint8> rgba;
  // TODO: Write other channels as separate transparent images?
  for (int i = 0; i < NUM_NODES / NPP; i++) {
    for (int j = 0; j < 3; j++) {
      const float f = values[i * NPP + j];
      rgba.push_back(FloatByte(f));
    }
    rgba.push_back(0xFF);
  }
  CHECK_EQ(rgba.size(), SIZE * SIZE * 4);
  ImageRGBA img(rgba, SIZE, SIZE);
  img.Save(filename);
  Printf("Wrote %s...\n", filename.c_str());
 
  // XXX PERF and as text file.
  if (false) {
    string tf = (string)"text-" + filename + (string)".txt";
    FILE *ftxt = fopen(tf.c_str(), "wb");
    for (int i = 0; i < values.size(); i++) {
      if (i % 8 == 0) fprintf(ftxt, "\n");
      fprintf(ftxt, "% f ", values[i]);
    }
    fclose(ftxt);
    Printf("And %s.\n", tf.c_str());
  }
}
#endif

// Single-channel bitmap.
struct ImageA {
  ImageA(const vector<uint8> &alpha, int width, int height)
    : width(width), height(height), alpha(alpha) {
    CHECK(alpha.size() == width * height);
  }
  const int width, height;
  vector<uint8> alpha;
};

static void BlitChannel(uint8 r, uint8 g, uint8 b, const ImageA &channel, 
			int xpos, int ypos,
			ImageRGBA *dest) {
  for (int y = 0; y < channel.height; y++) {
    int dsty = ypos + y;
    if (dsty >= 0 && dsty < dest->height) {
      for (int x = 0; x < channel.width; x++) {
	int dstx = xpos + x;
	if (dstx >= 0 && dstx <= dest->width) {
	  int sidx = x + channel.width * y;
	  int ch = channel.alpha[sidx];

	  auto Blend = [&dest](int idx, uint8 val, uint8 a) {
	    uint8 old = dest->rgba[idx];
	    uint8 oma = 0xFF - a;
	    uint8 replacement = ((oma * (int)old) + (a * (int)val)) >> 8;
	    dest->rgba[idx] = replacement;
	  };

	  int didx = dstx * 4 + (dsty * dest->width) * 4;
	  Blend(didx + 0, r, ch);
	  Blend(didx + 1, g, ch);
	  Blend(didx + 2, b, ch);
	  dest->rgba[didx + 3] = 0xFF;
	}
      }
    }
  }
}

struct NetworkConfiguration {
  const int num_layers = 4;
  // Note that these must have num_layers + 1 entries.
  const vector<int> widths = { 256, 256, 256, 256, 256, };
  const vector<int> heights = { 240, 240, 240, 240, 240 };
  // For 1 gigabyte layer sizes:
  // 2^30 = 1GB        = 1073741824
  //   / 4-byte floats = 268435456
  //   / 256 / 256     = 4396
  const vector<int> indices_per_node = { 1024, 1024, 1024, 1024, };
  vector<int> num_nodes;
  NetworkConfiguration() {
    CHECK_EQ(widths.size(), heights.size());
    CHECK_EQ(num_layers + 1, heights.size());
    CHECK_EQ(num_layers, indices_per_node.size());
    for (int i = 0; i < num_layers + 1; i++) {
      num_nodes.push_back(widths[i] * heights[i]);
    }
  }
};

struct Network {
  // Creates arrays of the appropriate size, but all zeroes. Note that this uninitialized
  // network is invalid, since the inverted indices are not correct.
  Network(vector<int> num_nodes, vector<int> indices_per_node) : num_layers(num_nodes.size() - 1),
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
	sizeof inverted_indices[i].output_indices[0] * inverted_indices[i].output_indices.size();
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
  static constexpr uint32 FORMAT_ID = 0x2700072AU;
  
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

static void WriteNetworkText(const Network &net, const string &filename) {
  const bool truncate = net.Bytes() > 20000000LL;
  if (truncate) {
    Printf("Writing trucated network because it's too big.\n");
  }
  FILE *f = fopen(filename.c_str(), "wb");
  for (int layer = 0; layer < net.num_layers; layer++) {
    fprintf(f, "\n"
	    "===================\n"
	    "Layer %d:\n"
	    "===================\n", layer);

    const int max_nodes = truncate ? min(net.num_nodes[layer + 1], 8) : net.num_nodes[layer + 1];
    for (int node_idx = 0; node_idx < max_nodes; node_idx++) {
      fprintf(f, "n %d: % f ", node_idx, net.layers[layer].biases[node_idx]);
      int indices_per_node = net.layers[layer].indices_per_node;
      for (int i = 0; i < indices_per_node; i++) {
	fprintf(f, " + %f*n%d",
		net.layers[layer].weights[node_idx * indices_per_node + i],
		net.layers[layer].indices[node_idx * indices_per_node + i]);
      }
      fprintf(f, "\n");
    }
  }
  fclose(f);
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
  explicit Errors(const Network &net) : num_layers(net.num_layers), num_nodes(net.num_nodes) {
    error.resize(num_layers);
    for (int i = 0; i < error.size(); i++) {
      error[i].resize(num_nodes[i + 1], 0.0f);
    }
  }
  const int num_layers;
  // The first entry here is unused (it's the size of the input layer, which doesn't
  // get errors), but we keep it like this to be consistent with Network and Stimulation.
  const vector<int> num_nodes;
  int64 Bytes() const {
    int64 ret = sizeof *this;
    for (int i = 0; i < error.size(); i++)
      ret += sizeof error[i] + sizeof error[i][0] * error[i].size();
    return ret;
  }

  // These are the delta terms in Mitchell. We have num_layers of them, where
  // the error[0] is the first real layer (we don't compute errors for the input)
  // and error[num_layers] is the error for the output.
  vector<vector<float>> error;
};

// Set the error values; this is almost just a memcpy so don't bother doing it
// on GPU.
static void SetOutputError(const Stimulation &stim, const vector<float> &expected, Errors *err) {
  // One more value vector than layers, since we have values for the input "layer" too.
  const int num_layers = stim.num_layers;
  ECHECK(stim.values.size() == num_layers + 1);
  const vector<float> &output = stim.values[num_layers];

  const int num_output_nodes = output.size();
  ECHECK_EQ(num_output_nodes, expected.size());
  ECHECK(err->error.size() == num_layers);
  vector<float> *output_error = &err->error[num_layers - 1];
  ECHECK_EQ(num_output_nodes, output_error->size());
  for (int k = 0; k < num_output_nodes; k++) {
    // Here we want to multiply by the derivative, sigma'(input),
    // which is sigma(input) * (1.0 - sigma(input)), and we already have
    // sigma(input) -- it's the output.
    const float out_k = output[k];
    // Note in some presentations this is out_k - expected_k.
    (*output_error)[k] = out_k * (1.0 - out_k) * (expected[k] - out_k);
  }
}

#if 0
// XXX should update this C++ code or just delete it.

// Propagate the error backwards from the dst_layer to the src_layer.
// (Note that the error terms go FROM the destination TO the source;
// here I'm keeping the same terminology about "source" and
// "destination" based on how data flows in the normal forward
// direction.)
//
// This is now a kernel, so this function is uncalled. But it should
// still work.
static void BackwardsError(const Network &net, const Stimulation &stim,
			   int dst_layer, Errors *err) {
  // If we have
  //
  //   inputs     h        h'    outputs
  //     o ------ o ------ o ------ o 
  //     o ------ o ------ o ------ o
  //     o ------ o ------ o ------ o
  //         layer 0   layer 1   layer 2
  //         error 0   error 1   error 2
  //       gap 0    gap 1     gap 2
  //   vals 0   vals 1   vals 2   vals 3
  //
  // We also have error deltas for each real layer. We are propagating the error
  // from layer dst_layer to layer dst_layer-1, which is the gap dst_layer.
  //
  // Errors only go to real layers, so the dest layer is 1 at minimum.
  ECHECK_GT(dst_layer, 0);
  const int gap = dst_layer;

  const int src_layer = dst_layer - 1;
  ECHECK_GE(src_layer, 0);

  // Note that stim has an extra data layer in it because it does
  // represent the values of the input layer, thus the +1 here.
  ECHECK_LT(src_layer + 1, stim.values.size());
  const vector<float> &src_output = stim.values[src_layer + 1];

  // The inverted index is really in the gap.
  ECHECK_LT(gap, net.inverted_indices_start.size());
  ECHECK_LT(gap, net.inverted_indices_length.size());
  const vector<uint32> &starts = net.inverted_indices_start[gap];
  const vector<uint32> &lengths = net.inverted_indices_length[gap];
  const vector<uint32> &inverted_index = net.inverted_indices[gap];
  const vector<uint32> &dst_indices = net.indices[dst_layer];
  (void)dst_indices;  // Suppress lint -- only used for debug check.
  const vector<float> &dst_weights = net.weights[dst_layer];

  // One error layer for each real layer (not the input).
  ECHECK_LT(dst_layer, err->error.size());
  const vector<float> &dst_error = err->error[dst_layer];
  vector<float> *src_error = &err->error[src_layer];
  // Loop over every node in the previous layer, index h.
  for (int h = 0; h < NUM_NODES; h++) {
    const float out_h = src_output[h];
    // Unpack inverted index for this node, so that we can loop over all of
    // the places its output is sent.
    const uint32 start = starts[h];
    const uint32 length = lengths[h];
    
    // The error for a hidden node is the sum of all the errors for
    // the next layer, but modulated by the weight of the edge.
    double weighted_error_sum = 0.0;
    for (int i = start; i < start + length; i++) {
      const int gidx = inverted_index[i];
      // gidx is an index into the index and weights vectors on the
      // destination layer.
      ECHECK_EQ(dst_indices[gidx], h);
      // Compute from the index which destination node it belongs to.
      const int dst_node_idx = gidx / INDICES_PER_NODE;
      weighted_error_sum += dst_weights[gidx] * dst_error[dst_node_idx];
    }
    
    (*src_error)[h] = out_h * (1.0f - out_h) * weighted_error_sum;
  }
}

// In some presentations, this is positive, and others, negative. It
// all comes down to the signs of the error; whether we compute
// (expected - actual) or (actual - expected). Either way, we want to be DECREASING
// error, not INCREASING it. For this setup, positive.
// static constexpr float LEARNING_RATE = +0.05f;
// static constexpr float LEARNING_RATE = +0.05f;
// Learning rate should be a small positive constant (0 < lr < 1) constant, probably
// around 0.05, and decreasing as we run more rounds.
static void UpdateWeights(const float learning_rate,
			  Network *net, const Stimulation &stim, const Errors &err) {
  // This one is doing a simple thing with a lot of parallelism, but unfortunately
  // writes would collide if we tried to add in all the weight updates in parallel.
  // Not clear what the best approach is here. Parallelizing over layers is easy,
  // at least.
  
  // Here we parallelize over all nodes in all layers; updating the weights and
  // bias for that node in a chunk.
  const int num_layers = net->num_layers;
  ParallelComp(num_layers * NUM_NODES,
	       [learning_rate, &net, &stim, &err, num_layers](int work_id) {
		 // Update the weights for this node.
		 const int layer = work_id % num_layers;
		 const int node_idx = work_id / num_layers;

		 // Error term is for this node.
		 // Since error is not represented for the input layer, 'layer' is
		 // the correct index. (That is, the 0th layer is the earliest one
		 // that has weights.)
		 ECHECK_LT(layer, err.error.size());
		 ECHECK_LT(node_idx, err.error[layer].size());
		 const float delta_j = err.error[layer][node_idx];
		 const float learning_rate_times_delta_j = learning_rate * delta_j;

		 vector<float> *layer_weights = &net->weights[layer];
		 const vector<uint32> &layer_indices = net->indices[layer];

		 // Note since stim has an additional layer for the input, layer
		 // here is referring to the output values of the previous layer,
		 // which is what we want. (The 0th layer is the earliest one
		 // that we read: the input layer.)
		 ECHECK_LT(layer, stim.values.size());
		 const vector<float> &layer_values = stim.values[layer];

		 // There is one weight for each input index.
		 for (int input_idx = 0; input_idx < INDICES_PER_NODE; input_idx++) {
		   const int gidx = INDICES_PER_NODE * node_idx + input_idx;
		   ECHECK_GE(gidx, 0);
		   ECHECK_LT(gidx, layer_indices.size());

		   // Offset of the node, which we use to get its output.
		   const int src_idx = layer_indices[gidx];
		   ECHECK_GE(src_idx, 0);
		   ECHECK_LT(src_idx, NUM_NODES);

		   ECHECK_LT(src_idx, layer_values.size());
		   const float x_ji = layer_values[src_idx];

		   (*layer_weights)[gidx] += learning_rate_times_delta_j * x_ji;
		 }

		 // The bias terms are basically the same, but the output of that
		 // node is 1. There's just one per node.
		 ECHECK_LT(layer, net->biases.size());
		 ECHECK_LT(node_idx, net->biases[layer].size());
		 net->biases[layer][node_idx] += learning_rate_times_delta_j;
	       }, 12);
}

#endif

struct ForwardLayerCL {
  explicit ForwardLayerCL(CL *cl) : cl(cl) {
    const string kernel_src = 
      Util::ReadFile("constants.h") + "\n" +
      Util::ReadFile("forwardlayer.cl");

    std::tie(program, kernel) = cl->BuildOneKernel(kernel_src, "ForwardLayer");
  }

  struct ForwardContext {
    ForwardContext(ForwardLayerCL *parent, const Network &net, int layer) :
      parent(parent), net(net), layer(layer) {
      CL *cl = parent->cl;
      /*
      Printf("layer %d sizes: %d %d %d\n", layer,
	     net.layers[layer].indices.size(),
	     net.layers[layer].weights.size(),
	     net.layers[layer].biases.size());
      */
      indices = MoveMemoryToGPUConst(cl->context, cl->queue, net.layers[layer].indices);
      weights = MoveMemoryToGPUConst(cl->context, cl->queue, net.layers[layer].weights);
      biases = MoveMemoryToGPUConst(cl->context, cl->queue, net.layers[layer].biases);
      // Printf("Created ForwardContext.\n");
    }

    // TODO: Do we really want to share the same command queue across threads?
    // Presumably clFinish can't tell "this thread's commands" apart from others,
    // so we may be prematurely waiting/running other thread's work.
    void Forward(Stimulation *stim) {
      ECHECK_LT(layer + 1, stim->values.size());

      /*
      for (const int idx : net.layers[layer].indices) {
	CHECK_GE(idx, 0);
	CHECK_LT(idx, stim->values[layer].size());
      }
      Printf("Checked Indices.\n");
      */
      
      CL *cl = parent->cl;

      // Printf("Copy src/dst values..\n");
      // Technically these are thread-safe, but we should avoid moving lots of memory
      // onto the GPU before we can use it, because our working set for one kernel call
      // is pretty sizable compared to total gpu memory!
      // Printf("stim value sizes %d -> %d\n",
      // stim->values[layer].size(), stim->values[layer + 1].size());
      cl_mem src_values = MoveMemoryToGPUConst(cl->context, cl->queue, stim->values[layer]);
      cl_mem dst_values = CreateUninitializedGPUMemory<float>(cl->context,
							      stim->values[layer + 1].size());

      // Printf("Setup kernel..\n");
      
      // Can't have multiple threads setting a kernel's argument at one time.
      {
	MutexLock ml(&parent->m);

	cl_int indices_per_node = net.layers[layer].indices_per_node;
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 0, sizeof(cl_int), (void *)&indices_per_node));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 1, sizeof(cl_mem), (void *)&src_values));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 2, sizeof(cl_mem), (void *)&indices));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 3, sizeof(cl_mem), (void *)&weights));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 4, sizeof(cl_mem), (void *)&biases));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 5, sizeof(cl_mem), (void *)&dst_values));

	size_t global_work_offset[] = { 0 };
        size_t global_work_size[] = { (size_t)(stim->values[layer + 1].size()) };
	// Printf("Run FL Kernel.\n");
	Timer kernel_timer;
	CHECK(CL_SUCCESS == clEnqueueNDRangeKernel(cl->queue, parent->kernel, 
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

      // Immediately release stuff we don't need any more; other threads may be trying
      // to get GPU resources in parallel.
      CHECK_SUCCESS(clReleaseMemObject(src_values));

      CopyBufferFromGPUTo<float>(cl->queue, dst_values, &stim->values[layer + 1]);

      CHECK_SUCCESS(clReleaseMemObject(dst_values));
    }
    
    ~ForwardContext() {
      CHECK_SUCCESS(clReleaseMemObject(indices));
      CHECK_SUCCESS(clReleaseMemObject(weights));
      CHECK_SUCCESS(clReleaseMemObject(biases));
    }

    cl_mem indices;
    cl_mem weights;
    cl_mem biases;
    ForwardLayerCL *parent = nullptr;
    const Network &net;
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

struct BackwardLayerCL {
  explicit BackwardLayerCL(CL *cl) : cl(cl) {
    const string kernel_src = 
      Util::ReadFile("constants.h") + "\n" +
      Util::ReadFile("backwardlayer.cl");

    std::tie(program, kernel) = cl->BuildOneKernel(kernel_src, "BackwardLayer");
  }

  struct BackwardContext {
    BackwardContext(BackwardLayerCL *parent, const Network &net, int dst_layer) :
      parent(parent), net(net), dst_layer(dst_layer) {
      CL *cl = parent->cl;

      const int gap = dst_layer;
      // const int src_layer = dst_layer - 1;

      starts = MoveMemoryToGPUConst(cl->context, cl->queue,
				    net.inverted_indices[gap].start);
      lengths = MoveMemoryToGPUConst(cl->context, cl->queue,
				    net.inverted_indices[gap].length);

      inverted_index = MoveMemoryToGPUConst(cl->context, cl->queue,
					    net.inverted_indices[gap].output_indices);

      dst_weights = MoveMemoryToGPUConst(cl->context, cl->queue,
					  net.layers[dst_layer].weights);
    }

    void Backward(const Stimulation &stim, Errors *err) {
      CL *cl = parent->cl;

      const int gap = dst_layer;
      const int src_layer = dst_layer - 1;

      //  const vector<float> &dst_error = err->error[dst_layer];
      //  vector<float> *src_error = &err->error[src_layer];

      cl_mem src_output = MoveMemoryToGPUConst(cl->context, cl->queue,
					       stim.values[src_layer + 1]);
      cl_mem dst_error = MoveMemoryToGPUConst(cl->context, cl->queue,
					       err->error[dst_layer]);

      // This is the source layer, but num_nodes is offset by one since it includes
      // the size of the input layer as element 0.
      int src_num_nodes = net.num_nodes[src_layer + 1];
      cl_mem src_error = CreateUninitializedGPUMemory<float>(cl->context, src_num_nodes);
      CHECK_EQ(src_num_nodes, net.inverted_indices[gap].start.size());
      
      // Can't have multiple threads setting a kernel's argument at one time.
      {
	MutexLock ml(&parent->m);

	cl_int dst_indices_per_node = net.layers[dst_layer].indices_per_node;
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 0, sizeof(cl_int),
				     (void *)&dst_indices_per_node));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 1, sizeof(cl_mem), (void *)&starts));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 2, sizeof(cl_mem), (void *)&lengths));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 3, sizeof(cl_mem), (void *)&inverted_index));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 4, sizeof(cl_mem), (void *)&dst_weights));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 5, sizeof(cl_mem), (void *)&src_output));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 6, sizeof(cl_mem), (void *)&dst_error));
	CHECK_SUCCESS(clSetKernelArg(parent->kernel, 7, sizeof(cl_mem), (void *)&src_error));

	size_t global_work_offset[] = { 0 };
	size_t global_work_size[] = { (size_t)src_num_nodes };
	Timer kernel_timer;
	CHECK(CL_SUCCESS == clEnqueueNDRangeKernel(cl->queue, parent->kernel, 
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

      // Immediately release stuff we don't need any more; other threads may be trying
      // to get GPU resources in parallel.
      CHECK_SUCCESS(clReleaseMemObject(src_output));
      CHECK_SUCCESS(clReleaseMemObject(dst_error));

      CopyBufferFromGPUTo<float>(cl->queue, src_error, &err->error[src_layer]);

      CHECK_SUCCESS(clReleaseMemObject(src_error));
    }
    
    ~BackwardContext() {
      CHECK_SUCCESS(clReleaseMemObject(starts));
      CHECK_SUCCESS(clReleaseMemObject(lengths));
      CHECK_SUCCESS(clReleaseMemObject(inverted_index));
      CHECK_SUCCESS(clReleaseMemObject(dst_weights));
    }

    cl_mem starts, lengths, inverted_index, dst_weights;
    BackwardLayerCL *parent = nullptr;
    const Network &net;
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

  struct UpdateContext {
    UpdateContext(UpdateWeightsCL *parent, Network *net, int layer) :
      parent(parent), net(net), layer(layer) {
      CL *cl = parent->cl;

      layer_indices = MoveMemoryToGPUConst(cl->context, cl->queue, net->layers[layer].indices);
      layer_weights = MoveMemoryToGPU(cl->context, cl->queue, false, &net->layers[layer].weights);
      layer_biases = MoveMemoryToGPU(cl->context, cl->queue, false, &net->layers[layer].biases);
    }

    void Update(float learning_rate, const Stimulation &stim, const Errors &err, int layer) {
      CL *cl = parent->cl;

      // Really can't run these in parallel because of concurrent writes to net.
      MutexLock ml(&parent->m);

      cl_mem layer_error = MoveMemoryToGPUConst(cl->context, cl->queue,
						err.error[layer]);
      cl_mem layer_values = MoveMemoryToGPUConst(cl->context, cl->queue,
						 stim.values[layer]);

      const int num_nodes = net->num_nodes[layer + 1];
      cl_int indices_per_node = net->layers[layer].indices_per_node;
      CHECK_SUCCESS(clSetKernelArg(parent->kernel, 0, sizeof(cl_float), (void *)&learning_rate));
      CHECK_SUCCESS(clSetKernelArg(parent->kernel, 1, sizeof(cl_int),
				   (void *)&indices_per_node));      
      CHECK_SUCCESS(clSetKernelArg(parent->kernel, 2, sizeof(cl_mem), (void *)&layer_error));
      CHECK_SUCCESS(clSetKernelArg(parent->kernel, 3, sizeof(cl_mem), (void *)&layer_indices));
      CHECK_SUCCESS(clSetKernelArg(parent->kernel, 4, sizeof(cl_mem), (void *)&layer_values));
      CHECK_SUCCESS(clSetKernelArg(parent->kernel, 5, sizeof(cl_mem), (void *)&layer_weights));
      CHECK_SUCCESS(clSetKernelArg(parent->kernel, 6, sizeof(cl_mem), (void *)&layer_biases));

      size_t global_work_offset[] = { 0 };
      size_t global_work_size[] = { (size_t)num_nodes };
      CHECK(CL_SUCCESS == clEnqueueNDRangeKernel(cl->queue, parent->kernel,
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
      CHECK_SUCCESS(clReleaseMemObject(layer_error));
      CHECK_SUCCESS(clReleaseMemObject(layer_values));
    }

    void Finish() {
      CL *cl = parent->cl;
      CopyBufferFromGPUTo(cl->queue, layer_weights, &net->layers[layer].weights);
      CopyBufferFromGPUTo(cl->queue, layer_biases, &net->layers[layer].biases);
      clFinish(cl->queue);
    }

    ~UpdateContext() {
      CHECK_SUCCESS(clReleaseMemObject(layer_indices));
      CHECK_SUCCESS(clReleaseMemObject(layer_weights));
      CHECK_SUCCESS(clReleaseMemObject(layer_biases));
    }

    cl_mem layer_indices, layer_weights, layer_biases;
    UpdateWeightsCL *parent = nullptr;
    Network *net;
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

#if 0
static void InitializeLayerFromImage(const ImageRGBA *rgba, vector<float> *values) {
  CHECK_EQ(SIZE, rgba->width);
  CHECK_EQ(SIZE, rgba->height);
  CHECK_EQ(values->size(), NUM_NODES);
  int dst = 0;
  for (int i = 0; i < SIZE * SIZE; i++) {
    (*values)[dst++] = ByteFloat(rgba->rgba[4 * i + 0]);
    (*values)[dst++] = ByteFloat(rgba->rgba[4 * i + 1]);
    (*values)[dst++] = ByteFloat(rgba->rgba[4 * i + 2]);
    for (int j = 0; j < NPP - 3; j++) {
      (*values)[dst++] = 0.0f;
    }
  }
  CHECK_EQ(dst, NUM_NODES);
}
#endif

// Make indices. This assumes that nodes are 2D pixel data where we have
// SIZE * SIZE pixels, NPP nodes per pixel, in row-major order.
//
// We mostly sample from a Gaussian near each pixel, but:
//  - we reject duplicates (inefficient),
//  - we reject pixels off the image (doesn't make sense; wrapping around
//      would work but an image is not a torus)
//  - we require that a small neighborhood around the pixel is mapped
//      directly (especially the pixel itself; this preserves spatial
//      locality and makes sure we don't have any statically dead nodes).
static void MakeIndices(const vector<int> &widths, ArcFour *rc, Network *net) {
  CHECK_EQ(widths.size(), net->num_layers + 1);
  // static constexpr int NEIGHBORHOOD = 5;

  static_assert(NEIGHBORHOOD >= 0, "must include the pixel itself.");
  auto OneNode = [](RandomGaussian *gauss,
		    int64 *rejected, int64 *duplicate,
		    int indices_per_node,
		    int src_width, int src_height,
		    int dst_width, int dst_height, int idx) -> vector<uint32> {

    CHECK((NEIGHBORHOOD * 2 + 1) * (NEIGHBORHOOD * 2 + 1) <= indices_per_node) <<
        "neighborhood doesn't fit in indices!";
    const int x = idx % dst_width;
    const int y = idx / dst_width;

    const double xf = x / (double)dst_width;
    const double yf = y / (double)dst_height;
    
    static constexpr double stddev = 1 / 16.0;
    
    // Use set for deduplication; we re-sort for locality of access later.
    unordered_set<int> indices;
    // clips xx,yy if they are out of the image.
    auto AddNodeByCoordinates = [src_width, src_height, &indices,
				 rejected, duplicate](int xx, int yy) {
      if (xx < 0 || yy < 0 || xx >= src_width || yy >= src_height) {
	++*rejected;
	return;
      }
      int idx = (yy * src_width) + xx;
      ECHECK_GE(idx, 0);
      ECHECK_LT(idx, src_width * src_height);
      auto p = indices.insert(idx);
      if (!p.second) ++*duplicate;
    };

    // Find the closest corresponding pixel in the src layer.
    const int cx = round(xf * src_width);
    const int cy = round(yf * src_height);
    for (int ny = -NEIGHBORHOOD; ny <= NEIGHBORHOOD; ny++) {
      for (int nx = -NEIGHBORHOOD; nx <= NEIGHBORHOOD; nx++) {
	// Note that the pixel may be clipped.
	AddNodeByCoordinates(cx + nx, cy + ny);
      }
    }

    CHECK_LE(indices.size(), indices_per_node);

    #if 1
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
	  // Note that the pixel may be clipped.
	  AddNodeByCoordinates(cx + nx, cy + ny);
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
      CHECK_LT(idx, src_width * src_height);
      ret.push_back(idx);
    }
    return ret;
  };

  // This must access rc serially.
  vector<ArcFour *> rcs;
  for (int i = 0; i < net->num_layers; i++) rcs.push_back(Substream(rc, i));

  ParallelComp(net->num_layers, [&widths, &rcs, &OneNode, &net](int layer) {
    Printf("Intializing indices for layer %d...\n", layer);
    const int indices_per_node = net->layers[layer].indices_per_node;
    vector<uint32> *layer_indices = &net->layers[layer].indices;
    CHECK_LT(layer + 1, widths.size());
    CHECK_LT(layer + 1, net->num_nodes.size());
    const int src_width = widths[layer];
    CHECK_EQ(0, net->num_nodes[layer] % src_width);
    const int src_height = net->num_nodes[layer] / src_width;
    const int dst_width = widths[layer + 1];
    CHECK_EQ(0, net->num_nodes[layer + 1] % dst_width);
    const int dst_height = net->num_nodes[layer + 1] / dst_width;
    RandomGaussian gauss{rcs[layer]};
    int64 rejected = 0LL, duplicate = 0LL;
    for (int node_idx = 0; node_idx < dst_height * dst_width; node_idx++) {
      vector<uint32> indices = OneNode(&gauss, &rejected, &duplicate,
				       indices_per_node,
				       src_width, src_height,
				       dst_width, dst_height, node_idx);
      // Sort them, for better locality of access later.
      std::sort(indices.begin(), indices.end());
      CHECK_EQ(indices_per_node, indices.size());
      const int start_idx = node_idx * indices_per_node;
      for (int i = 0; i < indices_per_node; i++) {
	ECHECK_LT(i, indices.size());
	ECHECK_LT(start_idx + i, layer_indices->size()) << "start " << start_idx
							<< " i " << i
							<< " indices size " << layer_indices->size()
							<< " indices per node "
							<< indices_per_node;
	(*layer_indices)[start_idx + i] = indices[i];
      }
      if (node_idx % 100 == 0) {
	Printf("  [%d/%d] %.1f%% (%lld rejected %lld dupe)\n",
	       node_idx, dst_height * dst_width,
	       (100.0 * node_idx) / (dst_height * dst_width),
	       rejected, duplicate);
      }
    }
  }, 12);

  DeleteElements(&rcs);
}

// Randomize the weights in a network. Doesn't do anything to indices.
static void RandomizeNetwork(ArcFour *rc, Network *net) {
  auto RandomizeFloats = [](ArcFour *rc, vector<float> *vec) {
    for (int i = 0; i < vec->size(); i++) {
      (*vec)[i] = RandFloat(rc) * 0.1f - 0.05f;
    }
  };

  // This must access rc serially.
  vector<ArcFour *> rcs;
  for (int i = 0; i < net->num_layers; i++) rcs.push_back(Substream(rc, i));

  // But now we can do all layers in parallel.
  ParallelComp(net->num_layers, [rcs, &RandomizeFloats, &net](int layer) {
    RandomizeFloats(rcs[layer], &net->layers[layer].biases);
    RandomizeFloats(rcs[layer], &net->layers[layer].weights);
  }, 12);
  
  DeleteElements(&rcs);
}

template<class A, class F>
static auto Map(const vector<A> &vec, const F &f) -> vector<decltype(f(vec[0]))> {
  using B = decltype(f(vec[0]));
  vector<B> ret;
  ret.resize(vec.size());
  for (int i = 0; i < vec.size(); i++) {
    ret[i] = f(vec[i]);
  }
  return ret;
}

template<class A, class F>
static void App(const vector<A> &vec, const F &f) {
  for (const auto &elt : vec) f(elt);
}

// These must be initialized before starting the UI thread!
static constexpr int NUM_VIDEO_STIMULATIONS = 7;
std::mutex video_export_m;
int current_round = 0;
vector<Stimulation> current_stimulations;
Network *current_network = nullptr;
static bool allow_updates = true;
static bool dirty = true;

static void ExportRound(int r) {
  MutexLock ml(&video_export_m);
  if (allow_updates) {
    current_round = r;
    dirty = true;
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
  int mousex = 0, mousey = 0;
  (void)mousex; (void)mousey;
  for (;;) {
    // int round = ReadWithLock(&video_export_m, &current_round);
    {
      MutexLock ml(&video_export_m);
      if (dirty) {
	sdlutil::clearsurface(screen, 0x0);
	string menu = StringPrintf("  round %d", current_round);

	for (int s = 0; s < NUM_VIDEO_STIMULATIONS; s++) {
	  const Stimulation &stim = current_stimulations[s];
	  for (int l = 0; l < stim.values.size(); l++) {
	    // XXX allow other sizes!
	    for (int y = 0; y < 240; y++) {
	      for (int x = 0; x < 256; x++) {
		int yy = 4 + l * 246 + y;
		int xx = 4 + s * 260 + x;
		uint8 v = FloatByte(stim.values[l][y * 256 + x]);
		sdlutil::drawpixel(screen, xx, yy, v, v, v);
	      }
	    }
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
  
  string start_seed = StringPrintf("%d  %lld", getpid(), (int64)time(NULL));
  Printf("Start seed: [%s]\n", start_seed.c_str());
  ArcFour rc(start_seed);
  rc.Discard(2000);

  // Create kernels right away so that we get compilation errors early.
  ForwardLayerCL forwardlayer{global_cl};
  BackwardLayerCL backwardlayer{global_cl};
  UpdateWeightsCL updateweights{global_cl};

  // Replacing these functions; don't warn.
  // (void)BackwardsError;
  // (void)UpdateWeights;

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
    MakeIndices(nc.widths, &rc, net.get());
    Printf("Invert indices:\n");
    ComputeInvertedIndices(net.get());
    CheckInvertedIndices(*net);

    Printf("Writing network so we don't have to do that again...\n");
    SaveNetworkBinary(*net, "net.val");
  }

  Printf("Initialized network in %.1fms.\n", initialize_network_timer.MS());
  
  Printf("Network uses %.2fMB of storage (without overhead).\n", 
	 net->Bytes() / (1024.0 * 1024.0));

  {
    Stimulation stim{*net};
    Printf("A stimulation uses %.2fMB.\n", stim.Bytes() / (1024.0 * 1024.0));
  }

  auto ShouldDie = [&net]() {
    bool should_die = ReadWithLock(&train_should_die_m, &train_should_die);
    if (should_die) {
      Printf("Train thread signaled death.\n");
      Printf("Saving...\n");
      SaveNetworkBinary(*net, "network-onexit.bin");
    }
    return should_die;
  };

  // Prepare corpus.
  printf("Generating corpus from ROM and movie...\n");
  std::unique_ptr<Emulator> emu{Emulator::Create("mario.nes")};
  CHECK(emu.get() != nullptr);
  vector<uint8> movie = SimpleFM2::ReadInputs("mario-long-three.fm2");
  struct Snapshot {
    int movie_idx;
    vector<uint8> save;
  };
  vector<Snapshot> snapshots;
  snapshots.reserve(2000);
  vector<uint8> start_save = emu->SaveUncompressed();
  
  while (snapshots.size() < 2000) {
    Printf("Populating snapshots from beginning (%d)...\n", snapshots.size());
    emu->LoadUncompressed(start_save);
    for (int i = 0; i < movie.size(); i++) {
      if (i > 100 && i < movie.size() - 100 && rc.Byte() < 0x9) {
	snapshots.resize(snapshots.size() + 1);
	Snapshot *snapshot = &snapshots.back();
	snapshot->movie_idx = i;
	snapshot->save = emu->SaveUncompressed();
      }
      emu->StepFull(movie[i], 0);
    }
  }
  
  // Training round: Loop over all images in random order.
  double setup_ms = 0.0, stimulation_init_ms = 0.0, forward_ms = 0.0,
    fc_init_ms = 0.0, bc_init_ms = 0.0, kernel_ms = 0.0, backward_ms = 0.0, output_error_ms = 0.0,
    update_ms = 0.0, writing_ms = 0.0, emulation_ms = 0.0;
  static constexpr int MAX_ROUNDS = 50000; // 10000;
  static constexpr int EXAMPLES_PER_ROUND = 48;
  static constexpr int VERBOSE_ROUND_EVERY = 250;

  Timer total_timer;
  for (int round_number = 0; round_number < MAX_ROUNDS; round_number++) {
    if (ShouldDie()) return;
    Printf("\n\n ** ROUND %d **\n", round_number);
    
    // When starting from a fresh network, consider this:
    const float round_learning_rate = 
      std::min(0.9, std::max(0.05, 2 * exp(-0.2275 * (round_number + 1)/3.0)));
    // const float round_learning_rate = 0.0025;

    Printf("Learning rate: %.4f\n", round_learning_rate);

    bool is_verbose_round = 0 == ((round_number /* + 1 */) % VERBOSE_ROUND_EVERY);
    if (is_verbose_round) {
      Printf("Writing network:\n");
      WriteNetworkText(*net, StringPrintf("network-%d.txt", round_number));
      SaveNetworkBinary(*net, "network-checkpoint.bin");
    }

    Printf("Export network:\n");
    ExportRound(round_number);
    ExportNetworkToVideo(*net);

    Timer setup_timer;
    Printf("Setting up batch:\n");

    struct TrainingExample {
      // XXX memory, etc.
      vector<uint8> rgba;
      vector<float> vals;
    };
    vector<TrainingExample> examples;
    examples.reserve(EXAMPLES_PER_ROUND);
    for (int i = 0; i < EXAMPLES_PER_ROUND; i++) {
      const Snapshot &snapshot = snapshots[RandTo(&rc, snapshots.size())];
      const int frames = RandTo(&rc, 60);
      emu->LoadUncompressed(snapshot.save);
      Timer emulation_timer;
      for (int j = snapshot.movie_idx; j < snapshot.movie_idx + frames && j < movie.size(); j++) {
	emu->StepFull(movie[j], 0);
      }

      for (int randomactions = RandTo(&rc, 3) * 2; randomactions--;) {
	const uint8 input = movie[RandTo(&rc, movie.size())];
	for (int steps = 1 + RandTo(&rc, 12); steps--;) {
	  emu->StepFull(input, 0);
	}
      }
      emulation_ms += emulation_timer.MS();

      examples.resize(examples.size() + 1);
      TrainingExample *example = &examples.back();
      // XXX memory, etc.
      example->rgba = emu->GetImage();
      vector<float> vals(256 * 240, 0.0f);
      for (int i = 0; i < 256 * 240; i++) {
	uint8 r = example->rgba[i * 4];
	uint8 g = example->rgba[i * 4 + 1];
	uint8 b = example->rgba[i * 4 + 2];
	// uint8 a = example->rgba[i * 4 + 3];
	CHECK_LT(i, vals.size());
	vals[i] = ((float)r + (float)g + (float)b) / (255.0f * 3.0f);
      }
      example->vals = std::move(vals);
    }

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
    vector<Stimulation> stims;
    stims.reserve(examples.size());
    for (int i = 0; i < examples.size(); i++) stims.emplace_back(*net);
    vector<Errors> errs;
    errs.reserve(examples.size());
    for (int i = 0; i < examples.size(); i++) errs.emplace_back(*net);

    {
      // Diagnostic only.
      int64 stim_bytes = 0ll, err_bytes = 0ll;
      for (int i = 0; i < examples.size(); i++) {
	stim_bytes += stims[i].Bytes();
	err_bytes += errs[i].Bytes();
      }
      Printf("Size for all stimulations: %.1fMB, errors: %.1fMB\n",
	     stim_bytes / (1000.0 * 1000.0),
	     err_bytes / (1000.0 * 1000.0));
    }

    Printf("Setting input layer of Stimulations...\n");
    // These are just memory copies; easy to do in parallel.
    CHECK_EQ(examples.size(), stims.size());
    ParallelComp(examples.size(),
		 [&examples, &stims](int i) {
		   CHECK_EQ(examples[i].vals.size(), stims[i].values[0].size());
		   stims[i].values[0] = examples[i].vals;
		 }, 16);
    stimulation_init_ms += stimulation_init_timer.MS();

    if (ShouldDie()) return;
    // The loop over layers must be in serial.
    for (int src = 0; src < net->num_layers; src++) {
      Printf("FWD Layer %d: ", src);
      Timer fc_init_timer;
      ForwardLayerCL::ForwardContext fc(&forwardlayer, *net, src);
      fc_init_ms += fc_init_timer.MS();

      // PERF could be parallel, but watch out about loading the GPU with
      // too many simultaneous value src/dst buffers.
      Timer forward_timer;
      Printf("Parallelcomp...\n");
      ParallelComp(examples.size(),
		   [&examples, &fc, &stims](int example_idx) {
		     fc.Forward(&stims[example_idx]);
		     if (example_idx % 10 == 0) {
		       Printf("[%d/%d] (%.2f%%) ", example_idx, (int)examples.size(),
			      100.0 * example_idx / examples.size());
		     }
		     
		     if (example_idx < NUM_VIDEO_STIMULATIONS) {
		       // Copy to screen.
		       ExportStimulusToVideo(example_idx, stims[example_idx]);
		     }
		   }, 12);
      forward_ms += forward_timer.MS();
      kernel_ms += fc.kernel_ms;
      Printf("\n");
    }
    // TODO PERF: Can kill transformed input eagerly, if having memory pressure issues.

    // Write layer as image doesn't work on these
    #if 0
    if (is_verbose_round) {
      if (ShouldDie()) return;
      // Write outputs as graphics.
      Timer writing_timer;
      ParallelComp(min((int)examples.size(), 10),
		   [&stims, round_number](int example) {
		     const Stimulation &stim = stims[example];
		     for (int i = 0; i < stim.values.size(); i++) {
		       string filename = StringPrintf("round-%d-ex-%d-layer-%d.png",
						      round_number,
						      example,
						      i);
		       WriteLayerAsImage(filename, stim.values[i]);
		     }
		   }, 16);
      writing_ms += writing_timer.MS();
    }
    #endif

    const int num_examples = examples.size();
    // But, don't need to keep this allocated.
    examples.clear();

    if (ShouldDie()) return;
    Printf("Error calc.\n");
    Timer output_error_timer;
    ParallelComp(num_examples,
		 [num_examples, &expected, &stims, &errs](int example) {
		   SetOutputError(stims[example], expected[example], &errs[example]);
		 }, 12);
    output_error_ms += output_error_timer.MS();
   
    CHECK_EQ(num_examples, errs.size());
    CHECK_EQ(num_examples, stims.size());

    if (ShouldDie()) return;
    Printf("Backwards:\n");
    // Also serial, but in reverse.
    Timer backward_timer;
    // We do NOT propagate errors to the input layer, so dst is strictly greater than 0.
    for (int dst = net->num_layers - 1; dst > 0; dst--) {
      Printf("BWD Layer %d: ", dst);

      Timer bc_init_timer;
      BackwardLayerCL::BackwardContext bc{&backwardlayer, *net, dst};
      bc_init_ms += bc_init_timer.MS();

      ParallelComp(num_examples,
		   [num_examples, &stims, &errs, &bc](int example) {
		     bc.Backward(stims[example], &errs[example]);
		     // BackwardsError(net, stims[example], dst, &errs[example]);
		     if (example % 5 == 0) {
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

    // for (int example = 0; example < num_examples; example++) {
    //   UpdateWeights(round_learning_rate, &net, stims[example], errs[example]);
    // }

    for (int layer = 0; layer < net->num_layers; layer++) {
      UpdateWeightsCL::UpdateContext uc(&updateweights, net.get(), layer);

      // PERF Faster to try to run these in parallel (maybe parallelizing memory traffic
      // with kernel execution -- but we can't run the kernels at the same time).
      for (int example = 0; example < num_examples; example++) {
	uc.Update(round_learning_rate, stims[example], errs[example], layer);
      }
      
      // Must call this to copy weights back!
      uc.Finish();
      Printf("[%d/%d] = (%.2f%%) ", layer, net->num_layers, layer * 100.0 / net->num_layers);
    }
    update_ms += update_timer.MS();
    Printf("\n");
      
    if (ShouldDie()) return;

    double total_ms = total_timer.MS();
    auto Pct = [total_ms](double d) { return (100.0 * d) / total_ms; };
    double denom = round_number + 1;
    Printf("Total so far %.1fs.\n"
	   "Time per round: %.1fs.\n"
	   "We spent %.1fms in setup (%.1f%%),\n"
	   "%.1fms in emulation (%.1f%%),\n"
	   "%.1fms in stimulation init (%.1f%%),\n"
	   "%.1fms in forward layer (%.1f%%),\n"
	   "%.1fms in fc init (%.1f%%),\n"
	   "%.1fms in forward layer kernel (at most; %.1f%%).\n"
	   "%.1fms in bc init (%.1f%%),\n"
	   "%.1fms in backwards pass (%.1f%%),\n"
	   "%.1fms in error for output layer (%.1f%%),\n"
	   "%.1fms in updating weights (%.1f%%),\n"
	   "%.1fms in writing images (%.1f%%),\n",
	   total_ms / 1000.0,
	   (total_ms / 1000.0) / denom,
	   setup_ms, Pct(setup_ms),
	   emulation_ms, Pct(emulation_ms),
	   stimulation_init_ms, Pct(stimulation_init_ms),
	   forward_ms / denom, Pct(forward_ms),
	   fc_init_ms / denom, Pct(fc_init_ms),
	   kernel_ms / denom, Pct(kernel_ms),
	   bc_init_ms / denom, Pct(bc_init_ms),
	   backward_ms / denom, Pct(backward_ms),
	   output_error_ms / denom, Pct(output_error_ms),
	   update_ms / denom, Pct(update_ms),
	   writing_ms / denom, Pct(writing_ms));
  }

  Printf(" ** Done. **");

  WriteWithLock(&train_done_m, &train_done, true);
}


int SDL_main(int argc, char* argv[]) {

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

  {
    printf("Allocating video network/stimulations...");
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
  /*
    BlitChannel(0xFF, 0x0, 0x0, *font, 
		30, 30,
		corpus[0]);

    printf("Save it..\n");
    corpus[0]->Save("testi.png");

    printf("Done.\n");
  */
  // ui_thread.join();

  SDL_Quit();
  return 0;
}
