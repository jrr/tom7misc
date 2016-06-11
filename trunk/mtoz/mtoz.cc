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
#include "../cc-lib/stb_truetype.h"
#include "../cc-lib/vector-util.h"
#include "../cc-lib/threadutil.h"
#include "../cc-lib/randutil.h"

#include "clutil.h"
#include "timer.h"

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

// Single-channel bitmap.
struct ImageA {
  ImageA(const vector<uint8> &alpha, int width, int height)
    : width(width), height(height), alpha(alpha) {
    CHECK(alpha.size() == width * height);
  }
  const int width, height;
  vector<uint8> alpha;
};

// #define NEIGHBORHOOD 15
// #define NEIGHBORHOOD 1
// #define NEIGHBORHOOD 10
#define NEIGHBORHOOD 1
struct NetworkConfiguration {
  const int num_layers = 5;
  // Note that these must have num_layers + 1 entries.
  const vector<int> widths = { 256, 128, 64, 128, 256, 256, };
  const vector<int> heights = { 240, 120, 60, 120, 240, 240, };
  // For 1 gigabyte layer sizes:
  // 2^30 = 1GB        = 1073741824
  //   / 4-byte floats = 268435456
  //   / 256 / 256     = 4396
  // const vector<int> indices_per_node = { 1024, 1024, 1024, 1024, };
  const vector<int> indices_per_node = { 16, 16, 16, 16, 32, };
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

// Set the error values; this is almost just a memcpy so don't bother doing it
// on GPU.
static void SetOutputError(const Stimulation &stim,
			   const vector<float> &expected, Errors *err) {
  // One more value vector than layers, since we have values for the
  // input "layer" too.
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
      
      CL *cl = parent->cl;

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
    const int indices_per_node = net->layers[layer].indices_per_node;
    Printf("Intializing %d indices for layer %d...\n", indices_per_node, layer);
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
      if (node_idx % 1000 == 0) {
	Printf("  %d. [%d/%d] %.1f%% (%lld rejected %lld dupe)\n",
	       layer,
	       node_idx, dst_height * dst_width,
	       (100.0 * node_idx) / (dst_height * dst_width),
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

static constexpr int NUM_ERROR_HISTORY_SAMPLES = 1000;
struct ErrorHistory {
  void AddSample(vector<double> v) {
    if (total_error.size() > NUM_ERROR_HISTORY_SAMPLES) {
      // PERF: Use circular buffer.
      static_assert(NUM_ERROR_HISTORY_SAMPLES > 10, "precondition");
      total_error.erase(total_error.begin(), total_error.begin() + 10);
    }
    total_error.push_back(std::move(v));
  }

  int Depth() const {
    return total_error.size();
  }
  // depth = 0 means most recent sample.
  double Sample(int depth, int layer) const {
    return total_error[total_error.size() - (depth + 1)][layer];
  }
  
private:
  // outer: num_samples, inner: num_layers
  vector<vector<double>> total_error;
};


// These must be initialized before starting the UI thread!
static constexpr int NUM_VIDEO_STIMULATIONS = 7;
static constexpr int EXPORT_EVERY = 10;
std::mutex video_export_m;
int current_round = 0;
double rounds_per_second = 0.0;
vector<Stimulation> current_stimulations;
Network *current_network = nullptr;
ErrorHistory *error_history = nullptr;
static bool allow_updates = true;
static bool dirty = true;

static void ExportRound(int r) {
  MutexLock ml(&video_export_m);
  if (allow_updates) {
    current_round = r;
    dirty = true;
  }
}

static void ExportRoundsPerSec(double rps) {
  MutexLock ml(&video_export_m);
  if (allow_updates) {
    rounds_per_second = rps;
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

static void ExportErrorHistorySample(vector<double> v) {
  MutexLock ml(&video_export_m);
  error_history->AddSample(std::move(v));
  dirty = true;
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
	    // XXX allow other sizes!
	    for (int y = 0; y < config.heights[l]; y++) {
	      for (int x = 0; x < config.widths[l]; x++) {
		int yy = ystart + y;
		int xx = 4 + s * 260 + x;
		uint8 v = FloatByte(stim.values[l][y * config.widths[l] + x]);
		sdlutil::drawpixel(screen, xx, yy, v, v, v);
	      }
	    }
	    ystart += config.heights[l];
	    ystart += 4;
	  }
	}

	// Error history.
	if (error_history->Depth() > 0) {
	  int yy = 12;
	  for (int layer = 0; layer < config.num_layers; layer++) {
	    for (int i = 0; i < std::min(error_history->Depth(), 9); i++) {
	      font->draw(12, yy, StringPrintf("^%c%.8f",
					      i == 0 ? '3' : '1',
					      error_history->Sample(i, layer)));
	      yy += font->height;
	    }
	    yy += 6;
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

  // Create kernels right away so that we get any compilation errors early.
  ForwardLayerCL forwardlayer{global_cl};
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
    MakeIndices(nc.widths, &rc, net.get());
    Printf("Invert indices:\n");
    ComputeInvertedIndices(net.get());
    CheckInvertedIndices(*net);

    Printf("Writing network so we don't have to do that again...\n");
    SaveNetworkBinary(*net, "net.val");
  }

  Printf("Initialized network in %.1fms.\n", initialize_network_timer.MS());

  if (ReadWithLock(&train_should_die_m, &train_should_die))
    return;
  
  Printf("Network uses %.2fMB of storage (without overhead).\n", 
	 net->Bytes() / (1024.0 * 1024.0));

  static constexpr int MAX_ROUNDS = 50000; // 10000;
  static constexpr int EXAMPLES_PER_ROUND = 48;
  static constexpr int VERBOSE_ROUND_EVERY = 250;
  
  {
    Stimulation stim{*net};
    Errors err{*net};
    Printf("A Stimulation uses %.2fMB, and an Errors uses %.2fMB.\n",
	   stim.Bytes() / (1024.0 * 1024.0),
	   err.Bytes() / (1024.0 * 1024.0));
    Printf("This is %.2fMB + %.2fMB total across a round of examples.\n",
	   (stim.Bytes() * EXAMPLES_PER_ROUND) / (1024.0 * 1024.0),
	   (err.Bytes() * EXAMPLES_PER_ROUND) / (1024.0 * 1024.0));
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
  const string train_romfile = "mario.nes";
  vector<uint8> train_movie = SimpleFM2::ReadInputs("mario-long-three.fm2");
  struct Snapshot {
    int movie_idx;
    vector<uint8> save;
  };
 
  static constexpr int SNAPSHOT_EVERY = 20;
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
  
  if (ShouldDie()) return;

  struct TrainingExample {
    // XXX memory, etc.
    vector<uint8> rgba;
    vector<float> vals;
  };
  // Training examples don't depend on the learning process, so are produced
  // in a separate thread. This mutex protects the deque (only).
  std::mutex training_examples_m;
  // XXX could just be vector, actually?
  deque<TrainingExample> training_examples;
  
  auto MakeTrainingExamplesThread = [train_romfile, &train_movie, &snapshots,
				     &training_examples_m, &training_examples](int idx) {
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

	// To get a bit more entropy in the training set, maybe run a few random buttons
	// from the movie, which may perturb the state a little without producing unrealistic
	// inputs.
	for (int randomactions = RandTo(&rc, 3) * 2; randomactions--;) {
	  const uint8 input = train_movie[RandTo(&rc, train_movie.size())];
	  for (int steps = 1 + RandTo(&rc, 12); steps--;) {
	    emu->StepFull(input, 0);
	  }
	}

	TrainingExample example;
	// XXX memory, etc.
	example.rgba = emu->GetImage();
	vector<float> vals(256 * 240, 0.0f);
	for (int i = 0; i < 256 * 240; i++) {
	  const uint8 r = example.rgba[i * 4];
	  const uint8 g = example.rgba[i * 4 + 1];
	  const uint8 b = example.rgba[i * 4 + 2];
	  // uint8 a = example.rgba[i * 4 + 3];
	  ECHECK_LT(i, vals.size());
	  vals[i] = ((float)r + (float)g + (float)b) / (255.0f * 3.0f);
	}
	example.vals = std::move(vals);

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

  std::thread emu_thread_1{MakeTrainingExamplesThread, 1};
  std::thread emu_thread_2{MakeTrainingExamplesThread, 2};
  ThreadJoiner join_emu_thread_1{&emu_thread_1};
  ThreadJoiner join_emu_thread_2{&emu_thread_2};
  
  if (ShouldDie()) return;
    
  // Training round: Loop over all images in random order.
  double setup_ms = 0.0, stimulation_init_ms = 0.0, forward_ms = 0.0,
    fc_init_ms = 0.0, bc_init_ms = 0.0, kernel_ms = 0.0, backward_ms = 0.0, output_error_ms = 0.0,
    update_ms = 0.0, writing_ms = 0.0, error_history_ms = 0.0;
  Timer total_timer;
  for (int round_number = 0; round_number < MAX_ROUNDS; round_number++) {
    if (ShouldDie()) return;
    Printf("\n\n ** ROUND %d **\n", round_number);
    
    // When starting from a fresh network, consider this:
    //   // std::min(0.95, std::max(0.10, 4 * exp(-0.2275 * (round_number + 1)/3.0)));
    
    // const float round_learning_rate =
    //     std::min(0.125, std::max(0.05, 2 * exp(-0.2275 * (round_number + 1)/3.0)));
    const float round_learning_rate = 0.0025;

    Printf("Learning rate: %.4f\n", round_learning_rate);

    bool is_verbose_round = 0 == ((round_number /* + 1 */) % VERBOSE_ROUND_EVERY);
    if (is_verbose_round) {
      Printf("Writing network:\n");
      // WriteNetworkText(*net, StringPrintf("network-%d.txt", round_number));
      SaveNetworkBinary(*net, "network-checkpoint.bin");
    }

    Printf("Export network:\n");
    ExportRound(round_number);
    if (round_number % EXPORT_EVERY == 0)
      ExportNetworkToVideo(*net);

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
    vector<Stimulation> stims;
    stims.reserve(examples.size());
    for (int i = 0; i < examples.size(); i++) stims.emplace_back(*net);
    vector<Errors> errs;
    errs.reserve(examples.size());
    for (int i = 0; i < examples.size(); i++) errs.emplace_back(*net);

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
		   [round_number, &examples, &fc, &stims](int example_idx) {
		     fc.Forward(&stims[example_idx]);
		     if (example_idx % 10 == 0) {
		       Printf("[%d/%d] (%.2f%%) ", example_idx, (int)examples.size(),
			      100.0 * example_idx / examples.size());
		     }

		     if (round_number % EXPORT_EVERY == 0 &&
			 example_idx < NUM_VIDEO_STIMULATIONS) {
		       // Copy to screen.
		       ExportStimulusToVideo(example_idx, stims[example_idx]);
		     }
		   }, 12);
      forward_ms += forward_timer.MS();
      kernel_ms += fc.kernel_ms;
      Printf("\n");
    }
    // TODO PERF: Can kill transformed input eagerly, if having memory pressure issues.

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
		     if (example % 10 == 0) {
		       Printf("[%d/%d] (%.2f%%) ", example, (int)num_examples,
			      100.0 * example / num_examples);
		     }
		   }, 12);
      Printf("\n");
    }
    backward_ms += backward_timer.MS();

    // Compute error history; diagnostic only.
    Timer error_history_timer;
    {
      vector<double> error_total(net->num_layers, 0.0);
      App(errs, [&net, &error_total](const Errors &err) {
	for (int layer = 0; layer < net->num_layers; layer++)
	  for (float f : err.error[layer])
	    error_total[layer] += f;
      });

      ExportErrorHistorySample(error_total);
      FILE *f = fopen("error-history.txt", "a");
      CHECK(f);
      fprintf(f, "%d. ", round_number);
      for (int i = 0; i < error_total.size(); i++) {
	fprintf(f, " %.8f", error_total[i]);
      }
      fprintf(f, "\n");
      fclose(f);
    }
    error_history_ms += error_history_timer.MS();


    if (ShouldDie()) return;
    Printf("Update weights:\n");
    Timer update_timer;

    // Don't parallelize! These are all writing to the same network weights. Each
    // call is parallelized, though.
    for (int layer = 0; layer < net->num_layers; layer++) {
      UpdateWeightsCL::UpdateContext uc(&updateweights, net.get(), layer);

      // XXX trying making this dynamic -- more nodes means slower learning?
      // (never actually ran this)
      // Maybe an off by one error here on the indices_per_node to use?
      // float layer_learning_rate = (round_learning_rate * 20f) /
      // net->layer[layer].indices_per_node;
      
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
    ExportRoundsPerSec(denom / (total_ms / 1000.0));
    Printf("Total so far %.1fs.\n"
	   "Time per round: %.1fs.\n"
	   "We spent %.1fms in setup (%.1f%%),\n"
	   "%.1fms in stimulation init (%.1f%%),\n"
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

  {
    // XXX Maybe UIThread should be an object, then...
    printf("Allocating video network/stimulations/data...");
    MutexLock ml(&video_export_m);
    NetworkConfiguration nc;
    current_network = new Network{nc.num_nodes, nc.indices_per_node};
    for (int i = 0; i < NUM_VIDEO_STIMULATIONS; i++) {
      current_stimulations.emplace_back(*current_network);
    }
    error_history = new ErrorHistory;
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
