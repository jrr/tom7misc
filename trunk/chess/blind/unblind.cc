// This code was forked from ../../mtoz, which came from ../../redi,
// so check that for some development history / thoughts.

// In this first experiment, I try to predict the actual board state
// (pieces and colors, castling ability, en passant?) from the 64-bit
// 'blinded' version. This representation is of course ambiguous, so
// the result can only be probabilistic.

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
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
#include <shared_mutex>

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

#include "../chess.h"
#include "../pgn.h"
#include "../bigchess.h"

#include "clutil.h"
#include "timer.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

static constexpr int VERBOSE = 1;

using namespace std;

using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;

using uint32 = uint32_t;
using uint64 = uint64_t;

using Move = Position::Move;

// We use a different compiled kernel depending on the layer's
// transfer function (these are in the inner loops, so we want
// to avoid overhead). The implementation of the transfer
// function is straightforward. The derivative is given in
// terms of the *output* of the transfer function, because
// this is the most natural/efficient for the sigmoid, and
// can be done (a bit less naturally) for ReLU.
static const char *SIGMOID_FN = 
  "#define FORWARD(potential) (1.0f / (1.0f + exp(-potential)))\n"
  // This wants to be given the actual output value f(potential).
  "#define DERIVATIVE(fx) (fx * (1.0f - fx))\n";

static const char *RELU_FN =
  "#define FORWARD(potential) ((potential < 0.0f) ? 0.0f : potential)\n"
  // This is normally given as x < 0 ? 0 : 1, but note that f(x)
  // tells us which side of 0 the input is on (retaining the
  // not-very-important ambiguity at exactly 0), anyway. So we define
  // it in terms of f(x) to maintain the same interface we use for
  // sigmoid.
  "#define DERIVATIVE(fx) ((fx < 0.0f) ? 0.0f : 1.0f)\n";

// Like RELU but slight slope in the "zero" region.
static const char *LEAKY_RELU_FN =
  "#define FORWARD(potential) ((potential < 0.0f) ? potential * 0.01f : potential)\n"
  // See note above.
  "#define DERIVATIVE(fx) ((fx < 0.0f) ? 0.01f : 1.0f)\n";

#define NUM_CONTENTS 13
static constexpr int kPieceToContents[16] = {
  // 0 = empty in both representations.
  0,
  // White first (MSB not set).
  1, // PAWN = 1,
  2, // KNIGHT = 2,
  3, // BISHOP = 3,
  4, // ROOK = 4,
  5, // QUEEN = 5,
  6, // KING = 6,
  4, // C_ROOK = 7,
  // black empty is invalid, but we can just
  // treat it as empty.
  0,
  7, // PAWN = 1,
  8, // KNIGHT = 2,
  9, // BISHOP = 3,
  10, // ROOK = 4,
  11, // QUEEN = 5,
  12, // KING = 6,
  10, // C_ROOK = 7,
};

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

static constexpr int MAX_GAMES = 2'000'000;
// XXX can be much bigger; position is only 33 bytes.
static constexpr int MAX_POSITIONS = 1'000'000;
static constexpr int MAX_PGN_PARALLELISM = 8;

// Thread-safe, so shared between train and ui threads.
static CL *global_cl = nullptr;

std::shared_mutex print_mutex;
#define Printf(fmt, ...) do {				\
    WriteMutexLock Printf_ml(&print_mutex);		\
    printf(fmt, ##__VA_ARGS__);				\
    fflush(stdout);					\
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
std::shared_mutex train_should_die_m;
static bool train_done = false;
std::shared_mutex train_done_m;

static uint8 FloatByte(float f) {
  if (f <= 0.0f) return 0;
  if (f >= 1.0f) return 255;
  else return f * 255.0;
}

static constexpr float ByteFloat(uint8 b) {
  return b * (1.0 / 255.0f);
}

// The input is a 64-bit number, represented as an 8x8 matrix of
// floats.
//
// In the output, there are several ways we could do it. One very
// simple thing would be to have 64 floats and then discretize so that
// [0, 1/7) is empty, [1/7, 2/7) is pawn, [2/7, 3/7) is knight, etc.
// The main problem with this is the desire for continuity; is a pawn
// really "almost" a knight in any way? So instead, each output is 13
// different floats (6 per color + 1 neutral empty); only the correct
// one (e.g. EMPTY) gets filled with 1.0 and the remainder are zeroes.
//
// In addition to the 64 * 7 floats in the output layer, we also have
// a single float representing the current turn (0 = white, 1 = black)
// four floats for the four castling states (1 = allowed, 0 = not).
// Ignoring en passant for now.

static constexpr int OUTPUT_LAYER_SIZE =
  // For each square, one probability for each of the 13 possible contents.
  13 * 64 +
  // Four castling probabilities.
  4 +
  // Current turn.
  1;

enum class RenderStyle {
  RGB,
  FLAT,
  CHESSBITS,
  CHESSBOARD,
};

enum TransferFunction {
  SIGMOID = 0,
  RELU,
  LEAKY_RELU,
  NUM_TRANSFER_FUNCTIONS,
};

static const char *TransferFunctionName(TransferFunction tf) {
  switch (tf) {
  case SIGMOID: return "SIGMOID";
  case RELU: return "RELU";
  case LEAKY_RELU: return "LEAKY_RELU";
  default: return "??INVALID??";
  }
}

#define NEIGHBORHOOD 1
struct NetworkConfiguration { 

  // Note that these must have num_layers + 1 entries.
  // The number of nodes in each layer is width * height * channels.
  // Channels tells us how many nodes there are per logical pixel;
  // for color output it's typical that this is 3. All that channels
  // really do is tell us how to align inputs when creating the
  // node indices during initialization. (And of course in the
  // input and output layers, it's expected that the channels will
  // be initialized/read in some meaningful way.)

  // Note that the last layer is given as a flat vector, which makes spatial heuristics for
  // index assignment fail; this layer needs to be highly connected.

  /*
  const int num_layers = 11;
  const vector<int> width =    { 8,  16, 32, 32, 32, 32, 32, 32, 32,  32,   8,
				 OUTPUT_LAYER_SIZE, };
  const vector<int> height =   { 8,  16, 32, 32, 32, 32, 32, 32, 32,  32,   9,  1, };
  const vector<int> channels = { 1,   1,  1,  2,  3,  7,  7,  8,  8,  10,  13,  1, };
  const vector<int> indices_per_channel = { 64, 256, 256, 256, 256, 256, 64, 64, 64, 64,
					    8 * 9 * 13, };

  const vector<RenderStyle> style =
    { RenderStyle::CHESSBITS,
      RenderStyle::FLAT,
      RenderStyle::FLAT, RenderStyle::FLAT, RenderStyle::FLAT, RenderStyle::FLAT,
      RenderStyle::FLAT, RenderStyle::FLAT, RenderStyle::FLAT, RenderStyle::FLAT,
      RenderStyle::FLAT,
      RenderStyle::CHESSBOARD, };
  */

  const int num_layers = 3;
  const vector<int> width =    { 8,  32,  9, OUTPUT_LAYER_SIZE, };
  const vector<int> height =   { 8,  32,  9, 1, };
  const vector<int> channels = { 1,   1,  7, 1, };
  const vector<int> indices_per_node =
    {     64, 1024, 9 * 9 * 7, };

  const vector<TransferFunction> transfer_functions = { 
#if 0
						       SIGMOID, 
    SIGMOID, 
    // Use SIGMOID for output layer, since we want probabilities in the ouptut.
    SIGMOID,
#endif
						       LEAKY_RELU,
						       LEAKY_RELU,
						       LEAKY_RELU,
  };

  const vector<RenderStyle> style = {
    RenderStyle::CHESSBITS,
    RenderStyle::RGB,
    RenderStyle::FLAT,
    RenderStyle::CHESSBOARD, 
  };

  
  // num_nodes = width * height * channels
  // //  indices_per_node = indices_per_channel * channels
  // //  vector<int> indices_per_node;
  vector<int> num_nodes;
  NetworkConfiguration() {
    CHECK_EQ(width.size(), height.size());
    CHECK_EQ(width.size(), style.size());
    CHECK_EQ(num_layers + 1, height.size());
    CHECK_EQ(num_layers, indices_per_node.size());
    for (int i = 0; i < num_layers + 1; i++) {
      CHECK(width[i] >= 1);
      CHECK(height[i] >= 1);
      CHECK(channels[i] >= 1);
      num_nodes.push_back(width[i] * height[i] * channels[i]);
    }
    /*
    for (int i = 0; i < indices_per_channel.size(); i++) {
      CHECK(indices_per_channel[i] >= 1);
      indices_per_node.push_back(channels[i + 1] * indices_per_channel[i]);
    }
    */
  }
};

struct Network {
  // Creates arrays of the appropriate size, but all zeroes. Note that this uninitialized
  // network is invalid, since the inverted indices are not correct.
  Network(vector<int> num_nodes,
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

  void NaNCheck() const {
    bool has_nans = false;
    vector<std::pair<int, int>> layer_nans;
    for (const Layer &layer : layers) {
      int w = 0, b = 0;
      for (float f : layer.weights) if (std::isnan(f)) w++;
      for (float f : layer.biases) if (std::isnan(f)) b++;
      layer_nans.emplace_back(w, b);
      if (w > 0 || b > 0) has_nans = true;
    }
    if (has_nans) {
      string err;
      for (int i = 0; i < layer_nans.size(); i++) {
	err += StringPrintf("(real) layer %d. %d/%d weights, %d/%d biases\n",
			    i,
			    layer_nans[i].first, layers[i].weights.size(),
			    layer_nans[i].second, layers[i].biases.size());
      }
      CHECK(false) << "The network has NaNs :-(\n" << err;
    }
  }
  
  // Just used for serialization. Whenever changing the interpretation
  // of the data in an incomplete way, please change.
  static constexpr uint32 FORMAT_ID = 0x2700072CU;

  // The number of "real" layers, that is, not counting the input.
  const int num_layers;

  // num_layers + 1. num_nodes[0] is the size of the input layer.
  vector<int> num_nodes;

  // "Real" layer; none for the input.
  struct Layer {
    // Same number of input indices for each node.
    int indices_per_node;
    // The transfer function used to compute the output from the
    // input indices.
    TransferFunction transfer_function;
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
  printf("\n%s: indices per node/fns: ", filename.c_str());

  vector<int> indices_per_node(file_num_layers, 0);
  vector<TransferFunction> transfer_functions(file_num_layers, SIGMOID);
  for (int i = 0; i < file_num_layers; i++) {
    indices_per_node[i] = Read32();
    TransferFunction tf = (TransferFunction)Read32();
    CHECK(tf >= 0 && tf < NUM_TRANSFER_FUNCTIONS) << tf;
    transfer_functions[i] = tf;
    printf("%d %s ",
	   indices_per_node[i],
	   TransferFunctionName(tf));
  }
  printf("\n");

  std::unique_ptr<Network> net{new Network{num_nodes, indices_per_node, transfer_functions}};
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
  for (const Network::Layer &layer : net.layers) {
    Write32(layer.indices_per_node);
    Write32(layer.transfer_function);
  }

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

  void CopyFrom(const Errors &other) {
    CHECK_EQ(this->num_layers, other.num_layers);
    CHECK_EQ(this->num_nodes.size(), other.num_nodes.size());
    for (int i = 0; i < this->num_nodes.size(); i++) {
      CHECK_EQ(this->num_nodes[i], other.num_nodes[i]);
    }
    this->error = other.error;
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
    if (CHECK_NANS) for (float f : inputs) CHECK(!std::isnan(f));
    CopyBufferToGPU(cl->queue, inputs, stimulations[0]);
  }

  void LoadExpected(const vector<float> &values) {
    CHECK_EQ(values.size(), net->num_nodes[net->num_layers]);
    if (CHECK_NANS) for (float f : values) CHECK(!std::isnan(f));
    CopyBufferToGPU(cl->queue, values, expected);
  }

  void ExportStimulation(Stimulation *stim) {
    CHECK_EQ(stim->values.size(), stimulations.size());
    for (int i = 0; i < stim->values.size(); i++) {
      CopyBufferFromGPUTo(cl->queue, stimulations[i], &stim->values[i]);
    }
  }

  void ExportErrors(Errors *err) {
    CHECK_EQ(err->error.size(), errors.size());
    for (int i = 0; i < err->error.size(); i++) {
      CopyBufferFromGPUTo(cl->queue, errors[i], &err->error[i]);
    }
  }

  void ExportOutput(vector<float> *out) {
    CopyBufferFromGPUTo(cl->queue, stimulations.back(), out);
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

static std::pair<std::vector<cl_program>, std::vector<cl_kernel>> 
  MakeTransferKernels(CL *cl, const char *base_file, const char *function_name) {
  std::vector<cl_program> programs;
  std::vector<cl_kernel> kernels;
  string base_src = Util::ReadFile(base_file);
  for (int tf = 0; tf < NUM_TRANSFER_FUNCTIONS; tf++) {
    cl_program program;
    cl_kernel kernel;
    string kernel_src;
    switch (tf) {
    case SIGMOID: kernel_src += SIGMOID_FN; break;
    case RELU: kernel_src += RELU_FN; break;
    case LEAKY_RELU: kernel_src += LEAKY_RELU_FN; break;
    default:
      CHECK(false) << "Invalid transfer function " << tf;
    }
    kernel_src += base_src;
    std::tie(program, kernel) = cl->BuildOneKernel(kernel_src, function_name);
    programs.push_back(program);
    kernels.push_back(kernel);
  }
  return make_pair(programs, kernels);
}

struct ForwardLayerCL {
  explicit ForwardLayerCL(CL *cl) : cl(cl) {
    std::tie(programs, kernels) = MakeTransferKernels(cl, "forwardlayer.cl", "ForwardLayer");
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

      const TransferFunction transfer_function =
	net_gpu->net->layers[layer].transfer_function;
      cl_kernel kernel = parent->kernels[transfer_function];

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	WriteMutexLock ml(&parent->m);

	cl_int indices_per_node = net_gpu->net->layers[layer].indices_per_node;
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 0, sizeof (cl_int), (void *)&indices_per_node));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 1, sizeof (cl_mem), (void *)&src_values));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 2, sizeof (cl_mem), (void *)&indices));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 3, sizeof (cl_mem), (void *)&weights));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 4, sizeof (cl_mem), (void *)&biases));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 5, sizeof (cl_mem), (void *)&dst_values));

	size_t global_work_offset[] = { 0 };
        size_t global_work_size[] = { (size_t)(net_gpu->net->num_nodes[layer + 1]) };
	// Printf("Run FL Kernel.\n");
	Timer kernel_timer;
	CHECK_SUCCESS(clEnqueueNDRangeKernel(cl->queue, kernel,
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
    for (auto &k : kernels)
      CHECK_SUCCESS(clReleaseKernel(k));
    for (auto &p : programs)
      CHECK_SUCCESS(clReleaseProgram(p));
  }

  CL *cl = nullptr;
  // Owned:
  std::vector<cl_program> programs;
  std::vector<cl_kernel> kernels;

  std::shared_mutex m;
};

// Set the error values; this is almost just a memcpy.
struct SetOutputErrorCL {
  explicit SetOutputErrorCL(CL *cl) : cl(cl) {
    std::tie(programs, kernels) = MakeTransferKernels(cl, "setoutputerror.cl", "SetOutputError");
  }

  struct Context {
    Context(SetOutputErrorCL *parent, NetworkGPU *net_gpu) :
      parent(parent), net_gpu(net_gpu) {}

    void SetOutputError(TrainingRoundGPU *train) {
      CL *cl = parent->cl;

      const Network *net = net_gpu->net;
      // Errors are always computed for the output layer, which is the last one.
      const TransferFunction transfer_function = net->layers.back().transfer_function;
      
      cl_kernel kernel = parent->kernels[transfer_function];

      // All three memories here have num_nodes floats.
      int num_nodes = net->num_nodes[net->num_layers];
      cl_mem actual_outputs = train->stimulations.back();
      cl_mem expected = train->expected;
      cl_mem output_error = train->errors.back();

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	WriteMutexLock ml(&parent->m);

	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 0, sizeof (cl_mem), (void *)&actual_outputs));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 1, sizeof (cl_mem), (void *)&expected));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 2, sizeof (cl_mem), (void *)&output_error));

	size_t global_work_offset[] = { 0 };
        size_t global_work_size[] = { (size_t)(num_nodes) };

	CHECK_SUCCESS(clEnqueueNDRangeKernel(cl->queue, kernel,
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

  ~SetOutputErrorCL() {
    for (auto &k : kernels)
      CHECK_SUCCESS(clReleaseKernel(k));
    for (auto &p : programs)
      CHECK_SUCCESS(clReleaseProgram(p));
  }

 private:
  CL *cl = nullptr;
  // Owned:
  std::vector<cl_program> programs;
  std::vector<cl_kernel> kernels;

  std::shared_mutex m;

  DISALLOW_COPY_AND_ASSIGN(SetOutputErrorCL);
};

// Propagate errors backwards. Note that errors flow from "dst" to "src".
struct BackwardLayerCL {
  explicit BackwardLayerCL(CL *cl) : cl(cl) {
    std::tie(programs, kernels) = MakeTransferKernels(cl, "backwardlayer.cl", "BackwardLayer");
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
      const Network *net = net_gpu->net;
      const int gap = dst_layer;
      const int src_layer = dst_layer - 1;

      const TransferFunction transfer_function = net->layers[src_layer].transfer_function;
      
      cl_kernel kernel = parent->kernels[transfer_function];

      cl_mem src_output = train->stimulations[src_layer + 1];
      cl_mem dst_error = train->errors[dst_layer];

      // This is the source layer, but num_nodes is offset by one since it includes
      // the size of the input layer as element 0.
      int src_num_nodes = net->num_nodes[src_layer + 1];
      cl_mem src_error = train->errors[src_layer];

      CHECK_EQ(src_num_nodes, net->inverted_indices[gap].start.size());

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	WriteMutexLock ml(&parent->m);

	cl_int dst_indices_per_node = net->layers[dst_layer].indices_per_node;
	CHECK_SUCCESS(clSetKernelArg(kernel, 0, sizeof (cl_int),
				     (void *)&dst_indices_per_node));
	CHECK_SUCCESS(clSetKernelArg(kernel, 1, sizeof (cl_mem), (void *)&starts));
	CHECK_SUCCESS(clSetKernelArg(kernel, 2, sizeof (cl_mem), (void *)&lengths));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 3, sizeof (cl_mem), (void *)&inverted_index));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 4, sizeof (cl_mem), (void *)&dst_weights));
	CHECK_SUCCESS(
	    clSetKernelArg(kernel, 5, sizeof (cl_mem), (void *)&src_output));
	CHECK_SUCCESS(clSetKernelArg(kernel, 6, sizeof (cl_mem), (void *)&dst_error));
	CHECK_SUCCESS(clSetKernelArg(kernel, 7, sizeof (cl_mem), (void *)&src_error));

	size_t global_work_offset[] = { 0 };
	size_t global_work_size[] = { (size_t)src_num_nodes };
	Timer kernel_timer;
	CHECK_SUCCESS(clEnqueueNDRangeKernel(cl->queue, kernel,
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
    for (auto &k : kernels)
      CHECK_SUCCESS(clReleaseKernel(k));
    for (auto &p : programs)
      CHECK_SUCCESS(clReleaseProgram(p));
  }

  CL *cl = nullptr;
  // Owned:
  std::vector<cl_program> programs;
  std::vector<cl_kernel> kernels;

  std::shared_mutex m;
};

struct UpdateWeightsCL {
  explicit UpdateWeightsCL(CL *cl) : cl(cl) {
    const string kernel_src =
      Util::ReadFile("updateweights.cl");
    // Note that this one doesn't depend on the transfer function/derivative.
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
      WriteMutexLock ml(&parent->m);

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

  std::shared_mutex m;
};

// TODO: Allow specifying a strategy for index assignment. For chess,
// taking full rows, columns, and diagonals is probably better than
// random gaussians! Make it easy to specify a fully-connected layer,
// at least.
//
// TODO: If the number of indices requested is close to the number
// available (or equal to it), then this approach is super inefficient.
// Could instead randomly delete indices.
//
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

  static_assert(NEIGHBORHOOD >= 0, "must include the pixel itself.");
  auto OneNode = [](ArcFour *rc, RandomGaussian *gauss,
		    int64 *rejected, int64 *duplicate,
		    int indices_per_node,
		    int src_width, int src_height, int src_channels,
		    int dst_width, int dst_height, int dst_channels,
		    int idx) -> vector<uint32> {

    CHECK(indices_per_node <= src_width * src_height * src_channels) <<
    "Can't get " << indices_per_node << " distinct indices from a layer with " <<
    src_width << " x " << src_height << " x " << src_channels <<
    " = " << (src_width * src_height * src_channels) << " sources";
						
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
    RandomGaussian gauss{rc};
    for (int i = 0; i < vec->size(); i++) {
      (*vec)[i] = mag * gauss.Next();
    }
  };

  // This must access rc serially.
  vector<ArcFour *> rcs;
  for (int i = 0; i < net->num_layers; i++) rcs.push_back(Substream(rc, i));

  // But now we can do all layers in parallel.
  ParallelComp(net->num_layers, [rcs, &RandomizeFloats, &net](int layer) {
    // XXX such hacks. How to best initialize?

    /*
    RandomizeFloats(powf(0.025f, (layer / 3.0f) + 1.0), rcs[layer], &net->layers[layer].biases);
    RandomizeFloats(1.0f / (net->layers[layer].indices_per_node * ((layer / 3.0f) + 5)),
		    rcs[layer], &net->layers[layer].weights);
    */

    for (float &f : net->layers[layer].biases) f = 0.0f;
    // RandomizeFloats(0.000025f, rcs[layer], &net->layers[layer].biases);
    RandomizeFloats(0.025f, rcs[layer], &net->layers[layer].weights);
  }, 12);

  DeleteElements(&rcs);
}


// These must be initialized before starting the UI thread!
static constexpr int NUM_VIDEO_STIMULATIONS = 6;
// static constexpr int EXPORT_EVERY = 16;
static constexpr int EXPORT_EVERY = 1;
static std::shared_mutex video_export_m;
static int current_round = 0;
static double rounds_per_second = 0.0;
static vector<Stimulation> current_stimulations;
static vector<Errors> current_errors;
static Network *current_network = nullptr;
static bool allow_updates = true;
static bool dirty = true;
static double current_learning_rate = 0.0;
static double current_total_error = 0.0;

static void ExportRound(int r) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    current_round = r;
    // dirty = true;
  }
}

static void ExportRoundsPerSec(double rps) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    rounds_per_second = rps;
    // dirty = true;
  }
}

static void ExportLearningRate(double rl) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    current_learning_rate = rl;
    // dirty = true;
  }
}

static void ExportNetworkToVideo(const Network &net) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK(current_network != nullptr);
    current_network->CopyFrom(net);
    dirty = true;
  }
}

static void ExportStimulusToVideo(int example_id, const Stimulation &stim) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK_GE(example_id, 0);
    CHECK_LT(example_id, current_stimulations.size());
    current_stimulations[example_id].CopyFrom(stim);
    dirty = true;
  }
}

static void ExportErrorsToVideo(int example_id, const Errors &err) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK_GE(example_id, 0);
    CHECK_LT(example_id, current_errors.size());
    current_errors[example_id].CopyFrom(err);
    dirty = true;
  }
}

static void ExportTotalErrorToVideo(double t) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    current_total_error = t;
  }
}

static std::pair<int, int> PixelSize(const NetworkConfiguration &config, int layer) {
  switch (config.style[layer]) {
  default:
  case RenderStyle::RGB:
    // Could do a hybrid where we use 3 channels per pixel, but still show them all
    return make_pair(config.width[layer], config.height[layer]);    
  case RenderStyle::FLAT:
    return make_pair(config.width[layer] * config.channels[layer], config.height[layer]);
  case RenderStyle::CHESSBITS:
    return {48, 48};
  case RenderStyle::CHESSBOARD:
    return {256, 286};
  }
}

static int ErrorWidth(const NetworkConfiguration &config, int layer) {
  switch (config.style[layer]) {
  case RenderStyle::CHESSBOARD:
    return 8;
  default:
    return config.width[layer];
  }
}

static constexpr bool DRAW_ERRORS = true;

// Draw error pixels to the screen.
template<int SCALE>
static void Error2D(const vector<float> &errs,
		    // Screen coordinates.
		    int xpos, int ypos,
		    // Unscaled width/height.
		    int width, int height) {
  static_assert(SCALE >= 1);
  for (int y = 0; y < height; y++) {
    const int ystart = ypos + y * SCALE;
    for (int x = 0; x < width; x++) {
      const int idx = y * width + x;
      const int xstart = xpos + x * SCALE;
      // Allow this to not be rectangular, e.g. chessboard.
      uint8 r = ((x + y) & 1) ? 0xFF : 0x00, g = 0x00, b = ((x + y) & 1) ? 0xFF : 0x00;
      if (idx < errs.size()) {
	const float f = errs[idx];
	// Use different colors for negative/positive. 
	// XXX: Show errors greater than magnitude 1!
	if (f < 0.0f) {
	  r = FloatByte(-f);
	  g = 0;
	  b = 0;
	} else {
	  r = 0;
	  g = FloatByte(f);
	  b = 0;
	}
      }
	
      // Hopefully this gets unrolled.
      for (int yy = 0; yy < SCALE; yy++) {
	for (int xx = 0; xx < SCALE; xx++) {
	  sdlutil::drawpixel(screen, xstart + xx, ystart + yy, r, g, b);
	}
      }
    }
  }
};


static void UIThread() {
  const NetworkConfiguration config;
  int mousex = 0, mousey = 0;
  (void)mousex; (void)mousey;
  int vlayer = 0;
  
  for (;;) {
    // int round = SharedReadWithLock(&video_export_m, &current_round);
    {
      ReadMutexLock ml(&video_export_m);
      if (dirty) {
	sdlutil::clearsurface(screen, 0x0);
	const char *paused_msg = allow_updates ? "" : " [^2VIDEO PAUSED]";
	string menu = StringPrintf("  round ^3%d ^1|  ^3%0.4f^0 rps    "
				   "^1%.6f^< learning rate   ^1%.6f^< total err%s",
				   current_round,
				   rounds_per_second,
				   current_learning_rate,
				   current_total_error,
				   paused_msg);

	int max_width = 0;
	for (int layer = 0; layer < config.num_layers + 1; layer++) {
	  int w = PixelSize(config, layer).first;
	  if (DRAW_ERRORS)
	    w += ErrorWidth(config, layer) * 2 + 2;
	  max_width = std::max(w, max_width);
	}
	max_width += 4;
	
	for (int s = 0; s < NUM_VIDEO_STIMULATIONS; s++) {
	  const Stimulation &stim = current_stimulations[s];
	  const Errors &err = current_errors[s];
	  CHECK(stim.values.size() == config.num_layers + 1);
	  CHECK(stim.values.size() == config.style.size());
	  CHECK(err.error.size() == config.num_layers);
	  
	  const int xstart = 4 + s * max_width;
	  if (xstart >= SCREENW)
	    break;

	  
	  int ystart = 24;
	  for (int l = 0; l < stim.values.size(); l++) {

	    switch (config.style[l]) {
	    case RenderStyle::CHESSBITS: {
	      for (int r = 0; r < 8; r++) {
		int yy = ystart + r * 6;
		for (int c = 0; c < 8; c++) {
		  int xx = xstart + c * 6;
		  
		  bool has = stim.values[l][r * 8 + c] > 0.5f;
		  bool black = (r + c) & 1;
		  // uint8 rr = black ? 134 : 255;
		  // uint8 gg = black ? 166 : 255;
		  // uint8 bb = black ? 102 : 221;
		  uint8 rr = black ? 194 : 255;
		  uint8 gg = black ? 226 : 255;
		  uint8 bb = black ? 162 : 231;

		  for (int y = 0; y < 6; y++) {
		    for (int x = 0; x < 6; x++) {
		      if (has && x > 0 && y > 0 && x < 5 && y < 5) {
			sdlutil::drawpixel(screen, xx + x, yy + y, 22, 22, 22);
		      } else {
			sdlutil::drawpixel(screen, xx + x, yy + y, rr, gg, bb);
		      }
		    }
		  }
		}
	      }
	      break;
	    }

	    case RenderStyle::CHESSBOARD: {
	      for (int r = 0; r < 8; r++) {
		int yy = ystart + r * 32;
		for (int c = 0; c < 8; c++) {
		  int xx = xstart + c * 32;

		  // Find the contents with the highest score.
		  int cidx = (r * 8 + c) * NUM_CONTENTS;
		  int maxi = 0;
		  float maxp = stim.values[l][cidx];
		  for (int i = 1; i < NUM_CONTENTS; i++) {
		    if (stim.values[l][cidx + i] > maxp) {
		      maxi = i;
		      maxp = stim.values[l][cidx + i];
		    }
		  }
		  
		  bool black = (r + c) & 1;
		  // Background
		  uint8 rr = black ? 134 : 255;
		  uint8 gg = black ? 166 : 255;
		  uint8 bb = black ? 102 : 221;

		  sdlutil::FillRectRGB(screen, xx, yy, 32, 32, rr, gg, bb);
		  string str = " ";
		  str[0] = " PKBRQKpkbrqk"[maxi];
		  font->draw(xx + 10, yy, str);
		  for (int i = 0; i < NUM_CONTENTS; i++) {
		    const uint8 v = FloatByte(stim.values[l][cidx + i]);
		    int x = xx + 2 + i * 2;
		    sdlutil::drawpixel(screen, x,     yy + 28,     v, v, v);
		    sdlutil::drawpixel(screen, x + 1, yy + 28,     v, v, v);
		    sdlutil::drawpixel(screen, x,     yy + 28 + 1, v, v, v);
		    sdlutil::drawpixel(screen, x + 1, yy + 28 + 1, v, v, v);
		  }
		}
	      }

	      // Castling flags.
	      for (int c = 0; c < 4; c++) {
		const uint8 v = FloatByte(stim.values[l][64 * NUM_CONTENTS + c]);
		sdlutil::FillRectRGB(screen, xstart + c * 32 + (c >= 2 ? 130 : 0),
				     ystart + 32 * 8, 30, 8, v, v, v);
		if (v < 127)
		  sdlutil::drawbox(screen, xstart + c * 32 + (c >= 2 ? 130 : 0),
				   ystart + 32 * 8, 30, 8, 0xFF, 0xFF, 0xFF);
		
	      }
	      
	      // Whose move is it? 1.0 means black, so subtract from 255.
	      const uint8 v = 255 - FloatByte(stim.values[l][64 * NUM_CONTENTS + 4]);
	      sdlutil::FillRectRGB(screen, xstart, ystart + 32 * 8 + 8, 32 * 8, 8, v, v, v);
	      if (v < 127)
		sdlutil::drawbox(screen, xstart, ystart + 32 * 8 + 8, 32 * 8, 8,
				 0xFF, 0xFF, 0xFF);
	      
	      break;
	    }
	      
	    case RenderStyle::RGB:
	      for (int y = 0; y < config.height[l]; y++) {
		int yy = ystart + y;
		if (yy >= SCREENH) break;

		for (int x = 0; x < config.width[l]; x++) {
		  
		  int xx = xstart + x;
		  if (x >= SCREENW) break;
		
		  int cidx = y * config.width[l] * config.channels[l] + x * config.channels[l];

		  // XXX! Need to support more than 3 channels for chess. Color is dubious
		  // anyway?
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
	      break;

	    case RenderStyle::FLAT:
	      for (int y = 0; y < config.height[l]; y++) {
		const int yy = ystart + y;
		for (int x = 0; x < config.width[l] * config.channels[l]; x++) {
		  const int xx = xstart + x;
		  if (x >= SCREENW) break;
		  const int cidx = y * config.width[l] * config.channels[l] + x;
		  const uint8 v = FloatByte(stim.values[l][cidx]);
		  sdlutil::drawpixel(screen, xx, yy, v, v, v);
		}
	      }
	      break;
	    }

	    // Now errors.
	    if (DRAW_ERRORS && l > 0) {
	      const int exstart = xstart + PixelSize(config, l).first;
	      const vector<float> &errs = err.error[l - 1];
	      switch (config.style[l]) {
	      case RenderStyle::CHESSBITS:
		Error2D<4>(errs, exstart, ystart, 8, 8);
		break;
	      case RenderStyle::FLAT:
	      case RenderStyle::RGB:
		Error2D<2>(errs, exstart, ystart,
			   config.width[l] * config.channels[l],
			   config.height[l]);
	      case RenderStyle::CHESSBOARD:
		Error2D<2>(errs, exstart, ystart,
			   // Note: height extends beyond the array.
			   8 * 13, 9);
	      }
	    }

	    ystart += PixelSize(config, l).second + 4;
	  }


	  if (vlayer >= 0) {
	    double tot = 0.0;
	    int yz = ystart + 4;
	    const vector<float> &vals = stim.values[vlayer];
	    for (int i = 0; i < vals.size(); i++) {
	      tot += vals[i];
	      if (i < 32) {
		if (vlayer > 0) {
		  const float e = vlayer > 0 ? err.error[vlayer - 1][i] : 0.0;
		  // Font color.
		  const int sign = (e == 0.0) ? 4 : (e < 0.0f) ? 2 : 5;
		  font->draw(xstart, yz, StringPrintf("^1%.9f ^%d%.12f", vals[i], sign, e));
		} else {
		  font->draw(xstart, yz, StringPrintf("^1%.9f", vals[i]));
		}
		yz += FONTHEIGHT;
	      }
	    }
	    font->draw(xstart, yz, StringPrintf("[%d] tot: %.9f", vlayer, tot));
	  }
	}

	font->draw(2, 2, menu);
	SDL_Flip(screen);
	dirty = false;
      }
    }

    if (SharedReadWithLock(&train_done_m, &train_done)) {
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
	case SDLK_SPACE:
	  {
	    WriteMutexLock ml(&video_export_m);
	    allow_updates = !allow_updates;
	  }
	  break;
	  
	case SDLK_v:
	  vlayer++;
	  {
	    ReadMutexLock ml(&video_export_m);
	    // Allow vlayer to go to -1 (off), but wrap around
	    // below that.
	    if (vlayer < -1) {
	      // There are actually num_layers + 1 stimulations.
	      vlayer = config.num_layers;
	    } else if (vlayer > config.num_layers) {
	      vlayer = -1;
	    }
	    dirty = true;
	  }
	  break;
	default:;
	}
      }
    } else {
      SDL_Delay(1000);
    }
  }
}

static std::shared_mutex training_positions_m;
static std::vector<Position> training_positions;

struct GameProcessor {
  GameProcessor() {}

  // If false, we don't even look at the game.
  bool Eligible(const PGN &pgn) {
    // Ignore games that don't finish.
    if (pgn.result == PGN::Result::OTHER) {
      return false;
    }
    
    if (pgn.GetTermination() != PGN::Termination::NORMAL) {
      return false;
    }

    return true;
  }

  void DoWork(const string &pgn_text) {
    PGN pgn;
    CHECK(parser.Parse(pgn_text, &pgn));

    if (!Eligible(pgn)) return;

    Position pos;
    for (int i = 0; i < pgn.moves.size(); i++) {
      const PGN::Move &m = pgn.moves[i];
      Move move;
      const bool move_ok = pos.ParseMove(m.move.c_str(), &move);

      if (!move_ok) {
	fprintf(stderr, "Bad move %s from full PGN:\n%s",
		m.move.c_str(), pgn_text.c_str());
	// There are a few messed up games in 2016 and earlier.
	// Return early if we find such a game.
	{
	  WriteMutexLock ml(&bad_games_m);
	  bad_games++;
	}
	return;
      }

      pos.ApplyMove(move);

      // XXX don't just add every position!
      {
	WriteMutexLock ml(&training_positions_m);
	if (training_positions.size() >= MAX_POSITIONS)
	  return;
	
	training_positions.push_back(pos);
      }
    }
  }

  string Status() {
    return "";
    /*
    ReadMutexLock ml(&topn_m);
    size_t s = topn.size();
    return s > 0 ? StringPrintf(" (found %d)", (int)s) : "";
    */
  }
  
  std::shared_mutex bad_games_m;
  int64 bad_games = 0LL;
  PGNParser parser;
};


// In parallel, load a bunch of games (into RAM) as training data (into
// training_positions).
static void LoadGamesThread() {
  const string games_file = "d:/chess/lichess_db_standard_rated_2017-02.pgn";
  {
    WriteMutexLock ml(&training_positions_m);
    training_positions.reserve(MAX_POSITIONS);
  }
  
  GameProcessor processor;

  auto DoWork = [&processor](const string &s) { processor.DoWork(s); };

  // TODO: How to get this to deduce second argument at least?
  auto work_queue =
    std::make_unique<WorkQueue<string, decltype(DoWork), 1>>(DoWork,
							     MAX_PGN_PARALLELISM);

  const int64 start = time(nullptr);

  {
    PGNTextStream stream(games_file.c_str());
    string game;
    while (stream.NextPGN(&game)) {
      work_queue->Add(std::move(game));
      game.clear();

      const int64 num_read = stream.NumRead();
      if (num_read % 20000LL == 0) {
	int64 done, in_progress, pending, positions;
	work_queue->Stats(&done, &in_progress, &pending);
	const bool should_pause = pending > 5000000;
	const char *pausing = should_pause ? " (PAUSE READING)" : "";

	{
	  ReadMutexLock ml(&training_positions_m);
	  positions = training_positions.size();
	}

	Printf("[Still reading; %lld games at %.1f/sec] %lld %lld %lld [%lld pos] %s%s\n",
	       num_read,
	       num_read / (double)(time(nullptr) - start),
	       done, in_progress, pending,
	       positions,
	       pausing,
	       processor.Status().c_str());
	if (MAX_GAMES > 0 && num_read >= MAX_GAMES)
	  break;

	{
	  ReadMutexLock ml(&train_should_die_m);
	  if (train_should_die) {
	    work_queue->Abandon();
	    return;
	  }
	}
	
	if (positions >= MAX_POSITIONS) {
	  Printf("Enough positions!\n");
	  work_queue->Abandon();
	  return;
	}
	
	if (should_pause)
	  std::this_thread::sleep_for(60s);
      }
    }
  }
  work_queue->SetNoMoreWork();

  while (work_queue->StillRunning()) {
    int64 done, in_progress, pending, positions;
    work_queue->Stats(&done, &in_progress, &pending);

    {
      ReadMutexLock ml(&training_positions_m);
      positions = training_positions.size();
    }

    Printf("[Done reading] %lld %lld %lld %.2f%% [%lld pos] %s\n",
	   done, in_progress, pending,
	   (100.0 * (double)done) / (in_progress + done + pending),
	   positions,
	   processor.Status().c_str());
    if (positions >= MAX_POSITIONS) {
      work_queue->Abandon();
      break;
    }
    std::this_thread::sleep_for(10s);
  }

  Printf("Work queue finalizing...\n");
  work_queue.reset(nullptr);
  Printf("Done loading training examples.\n");
}

static void TrainThread() {
  Timer setup_timer; 

  // Number of training examples per round of training.
  // XXX This is probably still way too low. These are very small for chess (a
  // stimulation just needs the node activation values, so it's much smaller than
  // the network itself, which has to store weights for each incoming edge).
  static constexpr int EXAMPLES_PER_ROUND = 128; // 4096;
  static constexpr int EXAMPLE_QUEUE_TARGET = std::max(EXAMPLES_PER_ROUND * 2, 1024);
  // On a verbose round, we write a network checkpoint and maybe some
  // other stuff to disk. XXX: Do this based on time, since rounds speed can vary
  // a lot based on other parameters!
  static constexpr int VERBOSE_ROUND_EVERY = 250;

  string start_seed = StringPrintf("%d  %lld", getpid(), (int64)time(nullptr));
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
    net.reset(new Network(nc.num_nodes, nc.indices_per_node, nc.transfer_functions));
    Printf("Randomize weights:\n");
    RandomizeNetwork(&rc, net.get());
    Printf("Gen indices:\n");
    MakeIndices(nc.width, nc.channels, &rc, net.get());
    Printf("Invert indices:\n");
    ComputeInvertedIndices(net.get());
    CheckInvertedIndices(*net);

    Printf("Writing network so we don't have to do that again...\n");
    SaveNetworkBinary(*net, "net.val");
  }

  Printf("Initialized network in %.1fms.\n", initialize_network_timer.MS());

  NetworkGPU net_gpu{global_cl, net.get()};

  if (SharedReadWithLock(&train_should_die_m, &train_should_die))
    return;

  Printf("Network uses %.2fMB of storage (without overhead).\n",
	 net->Bytes() / (1024.0 * 1024.0));
  {
    Stimulation tmp(*net);
    int64 stim_bytes = tmp.Bytes();
    Printf("A stimulation is %.2fMB, so for %d examples we need %.2fMB\n",
	   stim_bytes / (1024.0 * 1024.0), EXAMPLES_PER_ROUND,
	   (stim_bytes * EXAMPLES_PER_ROUND) / (1024.0 * 1024.0));
  }

  // We use the same structures to hold all the stimulations and errors
  // now, on the GPU.
  vector<TrainingRoundGPU *> training;
  for (int i = 0; i < EXAMPLES_PER_ROUND; i++)
    training.push_back(new TrainingRoundGPU{global_cl, *net});

  auto ShouldDie = [&net]() {
    bool should_die = SharedReadWithLock(&train_should_die_m, &train_should_die);
    if (should_die) {
      Printf("Train thread signaled death.\n");
      Printf("Saving to net.val...\n");
      SaveNetworkBinary(*net, "net.val");
      // Util::WriteFile("eval/nextframe.txt", StringPrintf("%d\n", eval_frame_num));
    }
    return should_die;
  };

  // Number of threads to allow for simultaneous writing of frames.
  // Asynchronously write_frames{EVAL_ONLY ? 2 : 8};

  if (ShouldDie()) return;
  
  struct TrainingExample {
    vector<float> input;
    vector<float> output;
  };
  // Training examples don't depend on the learning process, so are produced
  // in a separate thread. This mutex protects the deque (only).
  std::shared_mutex training_examples_m;
  // XXX could just be vector, actually?
  deque<TrainingExample> training_examples;

  auto PopulateExampleFromPos =
    [](const Position &pos, TrainingExample *example) {
      example->input.resize(64);
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  example->input[r * 8 + c] = pos.PieceAt(r, c) != Position::EMPTY ? 1.0f : 0.0f;
	}
      }

      example->output.resize(OUTPUT_LAYER_SIZE);
      for (int i = 0; i < OUTPUT_LAYER_SIZE; i++) example->output[i] = 0.0f;
      for (int r = 0; r < 8; r++) {
	for (int c = 0; c < 8; c++) {
	  const int cont = kPieceToContents[pos.PieceAt(r, c)];
	  example->output[(r * 8 + c) * NUM_CONTENTS + cont] = 1.0f;
	}
      }

      // Four castling flags.
      example->output[64 * NUM_CONTENTS + 0] = pos.CanStillCastle(false, false) ? 1.0f : 0.0f;
      example->output[64 * NUM_CONTENTS + 1] = pos.CanStillCastle(false, true)  ? 1.0f : 0.0f;
      example->output[64 * NUM_CONTENTS + 2] = pos.CanStillCastle(true,  false) ? 1.0f : 0.0f;
      example->output[64 * NUM_CONTENTS + 3] = pos.CanStillCastle(true,  true)  ? 1.0f : 0.0f;

      // And the current player's move.
      example->output[64 * NUM_CONTENTS + 4] = pos.BlackMove() ? 1.0f : 0.0f;
    };

  auto MakeTrainingExamplesThread = [&training_examples_m,
				     &training_examples,
				     &PopulateExampleFromPos]() {
    Printf("Training example thread startup.\n");
    ArcFour rc("make examples");
    
    for (;;) {
      if (SharedReadWithLock(&train_should_die_m, &train_should_die)) {
	return;
      }
      
      training_examples_m.lock_shared();
      // Make sure we have plenty of examples so that learning doesn't stall.
      if (training_examples.size() < EXAMPLE_QUEUE_TARGET) {
	training_examples_m.unlock_shared();

	TrainingExample example;

	{
	  training_positions_m.lock_shared();
	  
	  if (training_positions.size() < (MAX_POSITIONS / 10)) {
	    training_positions_m.unlock_shared();
	    Printf("Not enough training data loaded yet!\n");
	    std::this_thread::sleep_for(1s);
	    continue;
	  } else {
	    const int idx = RandTo(&rc, training_positions.size());
	    PopulateExampleFromPos(training_positions[idx], &example);
	    training_positions_m.unlock_shared();

	    {
	      WriteMutexLock ml(&training_examples_m);
	      training_examples.push_back(std::move(example));
	    }
	  }
	}

      } else {
	training_examples_m.unlock_shared();
	std::this_thread::sleep_for(10ms);
      }
    }
    Printf("Training example generator exiting.\n");
  };

  std::thread examples_thread{MakeTrainingExamplesThread};
  ThreadJoiner join_examples_thread{&examples_thread};

  if (ShouldDie()) return;

  // Training round: Loop over all images in random order.
  double setup_ms = 0.0, stimulation_init_ms = 0.0, forward_ms = 0.0,
    fc_init_ms = 0.0, bc_init_ms = 0.0, kernel_ms = 0.0, backward_ms = 0.0,
    output_error_ms = 0.0, update_ms = 0.0, writing_ms = 0.0, error_history_ms = 0.0,
    eval_ms = 0.0;
  Timer total_timer;
  for (int rounds_executed = 0; ; rounds_executed++) {
    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("\n\n");
    Printf("** NET ROUND %d (%d in this process) **\n", net->rounds, rounds_executed);

    // When starting from a fresh network, consider this:
    
    //    const float round_learning_rate =
    //      std::min(0.95, std::max(0.10, 4 * exp(-0.2275 * (net->rounds / 100.0 + 1)/3.0)));

    const float round_learning_rate =
      std::min(0.10, std::max(0.002, 4 * exp(-0.2275 * (net->rounds / 100.0 + 1)/3.0)));

    // const float round_learning_rate =
    //     std::min(0.125, std::max(0.002, 2 * exp(-0.2275 * (net->rounds + 1)/3.0)));

    // const float round_learning_rate =
    // std::min(0.125, std::max(0.002, 2 * exp(-0.2275 * (net->rounds / 1000.0 + 1)/3.0)));

    // const float round_learning_rate = 0.0025;

    if (VERBOSE > 2) Printf("Learning rate: %.4f\n", round_learning_rate);

    if (ShouldDie()) return;

    bool is_verbose_round = 0 == ((rounds_executed /* + 1 */) % VERBOSE_ROUND_EVERY);
    if (is_verbose_round) {
      Printf("Writing network:\n");
      net_gpu.ReadFromGPU();
      SaveNetworkBinary(*net, "network-checkpoint.bin");
    }

    if (VERBOSE > 2) Printf("Export network:\n");
    ExportRound(net->rounds);
    ExportLearningRate(round_learning_rate);
    if (rounds_executed % EXPORT_EVERY == 0) {
      net_gpu.ReadFromGPU();
      net->NaNCheck();
      ExportNetworkToVideo(*net);
    }

    Timer setup_timer;
    if (VERBOSE > 2) Printf("Setting up batch:\n");

    vector<TrainingExample> examples;
    examples.reserve(EXAMPLES_PER_ROUND);
    do {
      if (!examples.empty()) {
	if (VERBOSE > 0) Printf("Blocked grabbing examples (still need %d)...\n",
				EXAMPLES_PER_ROUND - examples.size());
	std::this_thread::sleep_for(100ms);
      }
      WriteMutexLock ml{&training_examples_m};
      while (examples.size() < EXAMPLES_PER_ROUND &&
	     !training_examples.empty()) {
	examples.push_back(std::move(training_examples.front()));
	training_examples.pop_front();
      }
    } while (examples.size() < EXAMPLES_PER_ROUND);

    if (VERBOSE > 2) Printf("Setting up expected:\n");
    vector<vector<float>> expected = Map(examples, [](const TrainingExample &te) {
      return te.output;
    });

    setup_ms += setup_timer.MS();

    CHECK_EQ(examples.size(), expected.size());

    // TODO: may make sense to pipeline this loop somehow, so that we can parallelize
    // CPU/GPU duties?

    // Run a batch of images all the way through. (Each layer requires significant setup.)
    if (VERBOSE > 2) Printf("Creating stimulations...\n");
    Timer stimulation_init_timer;

    if (VERBOSE > 2) Printf("Setting input layer of Stimulations...\n");
    // These are just memory copies; easy to do in parallel.
    CHECK_EQ(examples.size(), training.size());
    ParallelComp(examples.size(),
		 [&examples, &training](int i) {
		   training[i]->LoadInput(examples[i].input);
		 }, 16);
    stimulation_init_ms += stimulation_init_timer.MS();

    if (ShouldDie()) return;
    // The loop over layers must be in serial.
    for (int src = 0; src < net->num_layers; src++) {
      if (VERBOSE > 2) Printf("FWD Layer %d: ", src);
      Timer fc_init_timer;
      ForwardLayerCL::ForwardContext fc(&forwardlayer, &net_gpu, src);
      fc_init_ms += fc_init_timer.MS();

      // PERF could be parallel, but watch out about loading the GPU with
      // too many simultaneous value src/dst buffers.
      Timer forward_timer;
      if (VERBOSE > 3) Printf("Parallelcomp...\n");
      ParallelComp(examples.size(),
		   [&net, rounds_executed, num_examples = examples.size(),
		    &fc, &training](int example_idx) {
		     fc.Forward(training[example_idx]);
		     /*
		     if (example_idx % 10 == 0) {
		       Printf("[%d/%d] (%.2f%%) ", example_idx, num_examples,
			      100.0 * example_idx / num_examples);
		     }
		     */

		     if (rounds_executed % EXPORT_EVERY == 0 &&
			 example_idx < NUM_VIDEO_STIMULATIONS) {
		       // XXX this uses unintialized/stale memory btw
		       Stimulation stim{*net};
		       training[example_idx]->ExportStimulation(&stim);
		       // Copy to screen.
		       ExportStimulusToVideo(example_idx, stim);
		     }
		   }, 16);
      forward_ms += forward_timer.MS();
      kernel_ms += fc.kernel_ms;
      if (VERBOSE > 2) Printf("\n");
    }

    // Compute total error.
    if (rounds_executed % EXPORT_EVERY == 0) {
      CHECK_EQ(examples.size(), training.size());
      // We don't use the stimulus, because we want the total over all
      // examples (but we only export enough for the video above), and
      // only need the final output values, not internal layers.
      double total_error = 0.0;
      vector<float> values;
      values.resize(net->num_nodes[net->num_layers]);
      CHECK(values.size() == OUTPUT_LAYER_SIZE) << "Chess-specific check; ok to delete";
      for (int i = 0; i < examples.size(); i++) {
	training[i]->ExportOutput(&values);
	CHECK(examples[i].output.size() == values.size());
	for (int j = 0; j < values.size(); j++) {
	  total_error += fabs(examples[i].output[j] - values[j]);
	}
      }
      ExportTotalErrorToVideo(total_error / (double)examples.size());
    }
    
    const int num_examples = examples.size();
    // But, don't need to keep this allocated.
    examples.clear();

    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("Error calc.\n");
    Timer output_error_timer;
    ParallelComp(num_examples,
		 [&setoutputerror, &net_gpu, &training, &expected](int example) {
		   // PERF could pipeline this copy earlier
		   training[example]->LoadExpected(expected[example]);
		   SetOutputErrorCL::Context sc{&setoutputerror, &net_gpu};
		   sc.SetOutputError(training[example]);
		   /* Printf("."); */
		 }, 16);
    output_error_ms += output_error_timer.MS();
    if (VERBOSE > 2) Printf("\n");

    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("Backwards:\n");
    // Also serial, but in reverse.
    Timer backward_timer;
    // We do NOT propagate errors to the input layer, so dst is strictly greater than 0.
    for (int dst = net->num_layers - 1; dst > 0; dst--) {
      if (VERBOSE > 2) Printf("BWD Layer %d: ", dst);

      Timer bc_init_timer;
      BackwardLayerCL::Context bc{&backwardlayer, &net_gpu, dst};
      bc_init_ms += bc_init_timer.MS();

      ParallelComp(num_examples,
		   [num_examples, &training, &bc](int example) {
		     bc.Backward(training[example]);
		     /*
		     if (example % 10 == 0) {
		       Printf("[%d/%d] (%.2f%%) ", example, (int)num_examples,
			      100.0 * example / num_examples);
		     }
		     */
		   }, 16);
      if (VERBOSE > 2) Printf("\n");
    }
    backward_ms += backward_timer.MS();

    if (rounds_executed % EXPORT_EVERY == 0) {
      for (int example_idx = 0; example_idx < NUM_VIDEO_STIMULATIONS; example_idx++) {
	Errors err{*net};
	training[example_idx]->ExportErrors(&err);
	ExportErrorsToVideo(example_idx, err);
      }
    }
    
    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("Update weights:\n");
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
      /*
      Printf("[%d/%d] = (%.2f%%) ", layer, net->num_layers, layer * 100.0 / net->num_layers);
      */
    }
    update_ms += update_timer.MS();
    if (VERBOSE > 2) Printf("\n");

    if (ShouldDie()) return;

    net->rounds++;

    double total_ms = total_timer.MS();
    auto Pct = [total_ms](double d) { return (100.0 * d) / total_ms; };
    double denom = rounds_executed + 1;
    ExportRoundsPerSec(denom / (total_ms / 1000.0));
    if (VERBOSE > 1)
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

  SharedWriteWithLock(&train_done_m, &train_done, true);
}


int SDL_main(int argc, char **argv) {
  // XXX This is specific to my machine. You probably want to remove it.
  // Assumes that processors 0-16 are available.
  CHECK(SetProcessAffinityMask(GetCurrentProcess(), 0xF));

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
    WriteMutexLock ml(&video_export_m);
    NetworkConfiguration nc;
    current_network = new Network{nc.num_nodes, nc.indices_per_node, nc.transfer_functions};
    for (int i = 0; i < NUM_VIDEO_STIMULATIONS; i++) {
      current_stimulations.emplace_back(*current_network);
      current_errors.emplace_back(*current_network);
    }
    printf("OK.\n");
  }

  std::thread load_games_thread(&LoadGamesThread);
  
  std::thread train_thread(&TrainThread);

  UIThread();

  Printf("Killing train thread (might need to wait for round to finish)...\n");
  SharedWriteWithLock(&train_should_die_m, &train_should_die, true);
  load_games_thread.join();
  train_thread.join();

  Printf("Train is dead; now UI exiting.\n");

  SDL_Quit();
  return 0;
}

