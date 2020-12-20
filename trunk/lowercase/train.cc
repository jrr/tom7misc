// This code was forked from ../chess/blind/unblind.cc, which
// came from ../../mtoz, which came from ../../redi,
// so check that for some development history / thoughts.

// This first experiment tries to predict the letter's shape as
// Bezier (only) curves. I don't really expect this to work but
// it may be funny/interesting.



// TODO: Output error history during training. Could just concatenate
// it to a file. Would be nice to get it over a large number of examples
// (more than the batch size) to reduce variance.

// TODO: Fix error display.

// TODO: 2x/3x for RGB/FLAT.

// TODO: Show timer breakdown in GUI.

// TODO: Now we pass stuff to the video thread at multiple different moments,
// so they can be desynchronized. Should do something to synchronize this?

// TODO: In preparation for having multiple models that we care about,
// would be good to have the network configuration completely stored within
// the serialized format, so that we can mix and match models (at least
// multiple ones in the same process, if not spread out over code versions).
//   - TODO: Network now stores width/height/channels; restructure so
//     that rendering uses these.
//   - TODO: Had to get rid of const field in Stimulation and Errors,
//     because we copy assign them to video. Maybe better to heap
//     allocate; it's fine to use the copy constructor.

#include "SDL.h"
#include "SDL_main.h"
#include "sdl/sdlutil.h"
#include "sdl/font.h"

#include <CL/cl.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <cmath>
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

#include "base/stringprintf.h"
#include "base/logging.h"
#include "arcfour.h"
#include "util.h"
#include "stb_image.h"
#include "stb_image_write.h"
#include "vector-util.h"
#include "threadutil.h"
#include "randutil.h"
#include "base/macros.h"
#include "color-util.h"
#include "image.h"
#include "lines.h"

#include "loadfonts.h"
#include "network.h"

#include "clutil.h"
#include "timer.h"
#include "top.h"

#include "../bit7/embed9x9.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

static constexpr int VERBOSE = 2;
// Perform somewhat expensive sanity checking for NaNs.
// (Beware: Also enables some other really expensive diagnostics.)
// XXX PERF turn off once it's working!
static constexpr bool CHECK_NANS = true;

using namespace std;

using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;

using uint32 = uint32_t;
using uint64 = uint64_t;

using Contour = TTF::Contour;

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

// Defined at the bottom.
static std::optional<string> GetExclusiveApp();

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

static constexpr int MAX_FONTS = 100'000;
static constexpr int ENOUGH_FONTS = 100;

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

// The input is the coordinates of each contour.
//  - We normalize these so that they are nominally in [0,1].
//    (See ttf.h).
//  - To keep a regular structure, a contour starts with
//    the start coordinates, then consists only of cubic Bezier
//    splines. Each one is given as its control point, then as
//    the end point.
//  - Since a letter shape can have multiple contours, we need
//    some way to delimit them. Simplest way is to fix a maximum
//    number of contours, and points per contour, so that it's
//    representable in a matrix.

// Almost all the fonts (9100 out of 9135 in the first data set)
// have at most 3 contours per letter (and 3 is by far the most
// common maximum value, presumably for letters like B). So it
// seems like a good choice for this experiment.
//
// The three contours don't have to be the same length. If sorted
// by length, the 8795/9135 have 100 or fewer points in their
// longest path, 8770 have 25 or fewer in the second longest,
// and 8930 have 16 or fewer in the third.
//
// Each contour/row has:
//   2 floats for start pos
//   2*2 floats for each bezier curve
//   so (100 * 4 + 2) + (25 * 4 + 2) + (16 * 4 + 2) =
//      402 + 102 + 66 = 570
//
// Note that 0,0 is a valid coordinate and not that weird. What do
// we do when the path is shorter than the row allows? Note we
// also need to be able to recognize such a thing to render the
// output.
// Couple options:
//   - Just pad with zeroes or -1 or whatever.
//   - Duplicate points (making zero-length beziers) so that the
//     row is filled.
//       - could just duplicate the last point, but maybe better
//         to spread it out.
//   - Make the path longer by splitting beziers into multiple
//     segments.

// The output is the same shape.
// Additionally, consider adding a one-hot prediction of the letter
// being lowercased, to coax the network to distinguish between
// letters.

static constexpr int ROW0_MAX_PTS = 100;
static constexpr int ROW1_MAX_PTS = 25;
static constexpr int ROW2_MAX_PTS = 16;

static constexpr int INPUT_LAYER_SIZE =
  // start point, then beziers as control point, end point.
  2 + ROW0_MAX_PTS * (2 + 2) +
  2 + ROW1_MAX_PTS * (2 + 2) +
  2 + ROW2_MAX_PTS * (2 + 2);

static constexpr int OUTPUT_LAYER_SIZE =
  INPUT_LAYER_SIZE +
  // one-hot hint for what letter is this?
  26;

static constexpr int NEIGHBORHOOD = 1;

enum UserRenderStyle : uint32_t {
  RENDERSTYLE_INPUTXY = RENDERSTYLE_USER + 1000,
  RENDERSTYLE_OUTPUTXY = RENDERSTYLE_USER + 1001,
};

// Nominal pixel height of a character when rendering.
// Character can exceed these bounds; it's even normal
// for this to happen width-wise for wide characters.
static constexpr int NOMINAL_CHAR_SIZE = 200;

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

  // Empty, useless stimulation, but can be used to initialize
  // vectors, etc.
  Stimulation() : num_layers(0) {}
  Stimulation(const Stimulation &other) = default;

  int64 Bytes() const {
    int64 ret = sizeof *this;
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
  vector<int> num_nodes;

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

  void NaNCheck(const char *message) const {
    bool has_nans = false;
    vector<int> layer_nans;
    for (const vector<float> &layer : values) {
      int v = 0;
      for (float f : layer) if (std::isnan(f)) v++;
      layer_nans.push_back(v);
      if (v > 0) has_nans = true;
    }
    if (has_nans) {
      string err;
      for (int i = 0; i < layer_nans.size(); i++) {
	err += StringPrintf("stim layer %d. %d/%d values\n",
			    i,
			    layer_nans[i], values[i].size());
      }
      CHECK(false) << "[" << message
		   << "] The stimulation has NaNs :-(\n" << err;
    }
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
  // Empty, useless errors, but can be used to initialize vectors etc.
  Errors() : num_layers(0) {}
  Errors(const Errors &other) = default;

  // Would be nice for these to be const, but then we can't have an
  // assignment operator.
  int num_layers;
  // The first entry here is unused (it's the size of the input layer,
  // which doesn't get errors), but we keep it like this to be
  // consistent with Network and Stimulation.
  vector<int> num_nodes;
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
	MoveMemoryToGPU(cl->context, cl->queue, true,
			&net->layers[layer].indices);
      layers[layer].weights =
	MoveMemoryToGPU(cl->context, cl->queue, false,
			&net->layers[layer].weights);
      layers[layer].biases =
	MoveMemoryToGPU(cl->context, cl->queue, false,
			&net->layers[layer].biases);
    }

    inverted_indices.resize(net->inverted_indices.size());
    for (int layer = 0; layer < net->layers.size(); layer++) {
      inverted_indices[layer].start =
	MoveMemoryToGPUConst(cl->context, cl->queue,
			     net->inverted_indices[layer].start);
      inverted_indices[layer].length =
	MoveMemoryToGPUConst(cl->context, cl->queue,
			     net->inverted_indices[layer].length);
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
    CHECK_SUCCESS(
	clEnqueueReadBuffer(cl->queue, buf, CL_TRUE, 0,
			    sizeof (T) * vec->size(),
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
	  CreateUninitializedGPUMemory<float>(cl->context,
					      net.num_nodes[i + 1]));
    }

    expected =
      CreateUninitializedGPUMemory<float>(cl->context,
					  net.num_nodes[net.num_layers]);
  }

  void LoadInput(const vector<float> &inputs) {
    CHECK_EQ(inputs.size(), net->num_nodes[0]);
    if (CHECK_NANS) { for (float f : inputs) CHECK(!std::isnan(f)); }
    CopyBufferToGPU(cl->queue, inputs, stimulations[0]);
  }

  void LoadExpected(const vector<float> &values) {
    CHECK_EQ(values.size(), net->num_nodes[net->num_layers]);
    if (CHECK_NANS) { for (float f : values) CHECK(!std::isnan(f)); }
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

// TODO PERF: Natively support dense layers
struct ForwardLayerCL {
  explicit ForwardLayerCL(CL *cl, const Network &net) : cl(cl) {
    /*
    std::tie(programs, kernels) =
      MakeTransferKernels(cl, "forwardlayer.cl", "ForwardLayer");
    */

    string base_src = Util::ReadFile("forwardlayer.cl");
    for (int layer = 0; layer < net.layers.size(); layer++) {
      const TransferFunction transfer_function =
	net.layers[layer].transfer_function;
      const int indices_per_node = net.layers[layer].indices_per_node;

      const bool dense = net.layers[layer].type == LAYER_DENSE;
      
      string kernel_src;
      switch (transfer_function) {
      case SIGMOID: kernel_src += SIGMOID_FN; break;
      case RELU: kernel_src += RELU_FN; break;
      case LEAKY_RELU: kernel_src += LEAKY_RELU_FN; break;
      default:
	CHECK(false) << "Invalid transfer function " << transfer_function;
      }

      StringAppendF(&kernel_src, "\n#define INDICES_PER_NODE %d\n",
		    indices_per_node);
	
      kernel_src += base_src;
      auto [program, kernel] =
	cl->BuildOneKernel(kernel_src,
			   dense ?
			   "ForwardLayerDense" :
			   "ForwardLayerSparse");
      // XXX don't save debugging stuff
      layer_kernels.emplace_back(program, kernel,
				 dense, indices_per_node, transfer_function);
    }

  }

  struct ForwardContext {
    ForwardContext(ForwardLayerCL *parent, NetworkGPU *net_gpu, int layer) :
      parent(parent), net_gpu(net_gpu), layer(layer) {
      indices = net_gpu->layers[layer].indices;
      weights = net_gpu->layers[layer].weights;
      biases = net_gpu->layers[layer].biases;
    }

    // TODO: Do we really want to share the same command queue across
    // threads? Presumably clFinish can't tell "this thread's
    // commands" apart from others, so we may be prematurely
    // waiting/running other thread's work.
    void Forward(TrainingRoundGPU *train) {
      ECHECK_LT(layer + 1, train->stimulations.size());

      CL *cl = parent->cl;

      cl_mem src_values = train->stimulations[layer];
      cl_mem dst_values = train->stimulations[layer + 1];

      Network *net = net_gpu->net;

      auto [program, kernel, dense, kernel_ipn, kernel_tf] =
	parent->layer_kernels[layer];
      
      // Sanity check we have the right kernel
      CHECK(dense == (net->layers[layer].type == LAYER_DENSE));
      CHECK(kernel_ipn == net->layers[layer].indices_per_node);
      CHECK(kernel_tf == net->layers[layer].transfer_function);

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	WriteMutexLock ml(&parent->m);

	// TODO PERF: Remove unused args for dense layers?
	
	/*
	CHECK_SUCCESS(clSetKernelArg(kernel, 0, sizeof (cl_int),
				     (void *)&indices_per_node));
	*/
	CHECK_SUCCESS(clSetKernelArg(kernel, 0, sizeof (cl_mem),
				     (void *)&src_values));
	CHECK_SUCCESS(clSetKernelArg(kernel, 1, sizeof (cl_mem),
				     (void *)&indices));
	CHECK_SUCCESS(clSetKernelArg(kernel, 2, sizeof (cl_mem),
				     (void *)&weights));
	CHECK_SUCCESS(clSetKernelArg(kernel, 3, sizeof (cl_mem),
				     (void *)&biases));
	CHECK_SUCCESS(clSetKernelArg(kernel, 4, sizeof (cl_mem),
				     (void *)&dst_values));

	size_t global_work_offset[] = { 0 };
        size_t global_work_size[] = { (size_t)(net->num_nodes[layer + 1]) };
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
    for (auto &[p, k, dense_unused, ipn_unused, tf_unused] : layer_kernels) {
      CHECK_SUCCESS(clReleaseKernel(k));
      CHECK_SUCCESS(clReleaseProgram(p));
    }
  }

  CL *cl = nullptr;
  // Owned. Indexed by layer id.
  std::vector<std::tuple<cl_program, cl_kernel, bool, int, TransferFunction>>
  layer_kernels;

  std::shared_mutex m;
};

// Set the error values; this is almost just a memcpy.
struct SetOutputErrorCL {
  explicit SetOutputErrorCL(CL *cl) : cl(cl) {
    std::tie(programs, kernels) =
      MakeTransferKernels(cl, "setoutputerror.cl", "SetOutputError");
  }

  struct Context {
    Context(SetOutputErrorCL *parent, NetworkGPU *net_gpu) :
      parent(parent), net_gpu(net_gpu) {}

    void SetOutputError(TrainingRoundGPU *train) {
      CL *cl = parent->cl;

      const Network *net = net_gpu->net;
      // Errors are always computed for the output layer, which is the last one.
      const TransferFunction transfer_function =
	net->layers.back().transfer_function;

      cl_kernel kernel = parent->kernels[transfer_function];

      // All three memories here have num_nodes floats.
      int num_nodes = net->num_nodes[net->num_layers];
      cl_mem actual_outputs = train->stimulations.back();
      cl_mem expected = train->expected;
      cl_mem output_error = train->errors.back();

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	WriteMutexLock ml(&parent->m);

	CHECK_SUCCESS(clSetKernelArg(kernel, 0, sizeof (cl_mem),
				     (void *)&actual_outputs));
	CHECK_SUCCESS(clSetKernelArg(kernel, 1, sizeof (cl_mem),
				     (void *)&expected));
	CHECK_SUCCESS(clSetKernelArg(kernel, 2, sizeof (cl_mem),
				     (void *)&output_error));

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
  explicit BackwardLayerCL(CL *cl, const Network &net) : cl(cl) {
    /*
    std::tie(programs, kernels) =
      MakeTransferKernels(cl, "backwardlayer.cl", "BackwardLayer");
    */

    // TODO PERF: Natively support dense layers
    string base_src = Util::ReadFile("backwardlayer.cl");
    for (int src_layer = 0; src_layer < net.layers.size() - 1; src_layer++) {
      int dst_layer = src_layer + 1;
      const TransferFunction transfer_function =
	net.layers[src_layer].transfer_function;
      const int dst_indices_per_node = net.layers[dst_layer].indices_per_node;
      // The dest layer, but num_nodes offset by 1.
      const int dst_num_nodes = net.num_nodes[dst_layer + 1];
      const bool dst_dense = net.layers[dst_layer].type == LAYER_DENSE;
      
      string kernel_src;
      switch (transfer_function) {
      case SIGMOID: kernel_src += SIGMOID_FN; break;
      case RELU: kernel_src += RELU_FN; break;
      case LEAKY_RELU: kernel_src += LEAKY_RELU_FN; break;
      default:
	CHECK(false) << "Invalid transfer function " << transfer_function;
      }

      StringAppendF(&kernel_src,
		    "\n"
		    "#define DST_INDICES_PER_NODE %d\n"
		    "#define DST_NUM_NODES %d\n",
		    dst_indices_per_node,
		    dst_num_nodes);
	
      kernel_src += base_src;
      auto [program, kernel] =
	cl->BuildOneKernel(kernel_src,
			   dst_dense ?
			   "BackwardLayerDense" :
			   "BackwardLayerSparse");
      // XXX don't save debugging stuff
      layer_kernels.emplace_back(program, kernel,
				 dst_dense,
				 dst_indices_per_node, transfer_function);
    }
  }

  struct Context {
    Context(BackwardLayerCL *parent, NetworkGPU *net_gpu, int dst_layer) :
      parent(parent), net_gpu(net_gpu), dst_layer(dst_layer) {

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

      auto [program_, kernel, dst_dense, dst_ipn, tf] =
	parent->layer_kernels[src_layer];
      // Sanity check that this was compiled with the right ipn / tf.
      CHECK(dst_ipn == net->layers[dst_layer].indices_per_node);
      CHECK(tf == net->layers[src_layer].transfer_function);
      CHECK(dst_dense == (net->layers[dst_layer].type == LAYER_DENSE));
      
      cl_mem src_output = train->stimulations[src_layer + 1];
      cl_mem dst_error = train->errors[dst_layer];

      // This is the source layer, but num_nodes is offset by one
      // since it includes the size of the input layer as element 0.
      int src_num_nodes = net->num_nodes[src_layer + 1];
      cl_mem src_error = train->errors[src_layer];

      CHECK_EQ(src_num_nodes, net->inverted_indices[gap].start.size());

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	WriteMutexLock ml(&parent->m);

	/*	
	cl_int dst_indices_per_node = net->layers[dst_layer].indices_per_node;
	CHECK_SUCCESS(clSetKernelArg(kernel, 0, sizeof (cl_int),
				     (void *)&dst_indices_per_node));
	*/
	CHECK_SUCCESS(clSetKernelArg(kernel, 0, sizeof (cl_mem),
				     (void *)&starts));
	CHECK_SUCCESS(clSetKernelArg(kernel, 1, sizeof (cl_mem),
				     (void *)&lengths));
	CHECK_SUCCESS(clSetKernelArg(kernel, 2, sizeof (cl_mem),
				     (void *)&inverted_index));
	CHECK_SUCCESS(clSetKernelArg(kernel, 3, sizeof (cl_mem),
				     (void *)&dst_weights));
	CHECK_SUCCESS(clSetKernelArg(kernel, 4, sizeof (cl_mem),
				     (void *)&src_output));
	CHECK_SUCCESS(clSetKernelArg(kernel, 5, sizeof (cl_mem),
				     (void *)&dst_error));
	CHECK_SUCCESS(clSetKernelArg(kernel, 6, sizeof (cl_mem),
				     (void *)&src_error));

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
    for (auto &[p, k, deleteme0, deleteme1, deleteme2] : layer_kernels) {
      CHECK_SUCCESS(clReleaseKernel(k));
      CHECK_SUCCESS(clReleaseProgram(p));
    }
  }

  CL *cl = nullptr;
  // Indexed by source layer index.
  // Note: Unlike others, these have the layer's parameters
  // baked in.
  std::vector<std::tuple<cl_program, cl_kernel, bool, int, TransferFunction>>
    layer_kernels;

  std::shared_mutex m;
};

struct UpdateWeightsCL {
  explicit UpdateWeightsCL(CL *cl, const Network &net) : cl(cl) {
    // Note that this one doesn't depend on the transfer function/derivative.

    string base_src = Util::ReadFile("updateweights.cl");
    for (int layer = 0; layer < net.layers.size(); layer++) {
      const int indices_per_node = net.layers[layer].indices_per_node;

      const bool dense = net.layers[layer].type == LAYER_DENSE;
      
      string kernel_src;
      StringAppendF(&kernel_src, "\n#define INDICES_PER_NODE %d\n",
		    indices_per_node);
	
      kernel_src += base_src;
      auto [program, kernel] =
	cl->BuildOneKernel(kernel_src,
			   dense ?
			   "UpdateWeightsDense" :
			   "UpdateWeightsSparse");
      // XXX don't save debugging stuff
      layer_kernels.emplace_back(program, kernel,
				 dense, indices_per_node);
    }
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

      auto [program_, kernel, dense, ipn] =
	parent->layer_kernels[layer];
      // Sanity check that this was compiled with the right constants.
      CHECK(ipn == net_gpu->net->layers[layer].indices_per_node);
      CHECK(dense == (net_gpu->net->layers[layer].type == LAYER_DENSE));
      
      const int num_nodes = net_gpu->net->num_nodes[layer + 1];

      CHECK_SUCCESS(
	  clSetKernelArg(kernel, 0, sizeof (cl_float),
			 (void *)&learning_rate));
      /*
      // cl_int indices_per_node = net_gpu->net->layers[layer].indices_per_node;
      CHECK_SUCCESS(
	  clSetKernelArg(kernel, 1, sizeof (cl_int),
			 (void *)&indices_per_node));
      */
      CHECK_SUCCESS(
	  clSetKernelArg(kernel, 1, sizeof (cl_mem),
			 (void *)&layer_error));
      CHECK_SUCCESS(
	  clSetKernelArg(kernel, 2, sizeof (cl_mem),
			 (void *)&layer_indices));
      CHECK_SUCCESS(
	  clSetKernelArg(kernel, 3, sizeof (cl_mem),
			 (void *)&layer_values));
      CHECK_SUCCESS(
	  clSetKernelArg(kernel, 4, sizeof (cl_mem),
			 (void *)&layer_weights));
      CHECK_SUCCESS(
	  clSetKernelArg(kernel, 5, sizeof (cl_mem),
			 (void *)&layer_biases));

      size_t global_work_offset[] = { 0 };
      size_t global_work_size[] = { (size_t)num_nodes };
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
    for (auto &[p, k, dense_unused, ipn_unused] : layer_kernels) {
      CHECK_SUCCESS(clReleaseKernel(k));
      CHECK_SUCCESS(clReleaseProgram(p));
    }
  }

  CL *cl = nullptr;
  // Owned. Indexed by layer id. (is_dense, indices_per_node)
  std::vector<std::tuple<cl_program, cl_kernel, bool, int>>
  layer_kernels;

  std::shared_mutex m;
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
//
// TODO: In addition to the spatial channels, it might be useful to
// have some region that is densely connected. For example, say we
// specify DENSE_PREFIX_ROWS, which are then subtracted from the
// implied "image", and each pixel is assigned indices from the
// neighborhood, then from dense_prefix_rows, then from a gaussian.
//
// TODO: Allow specifying a strategy for index assignment. For chess,
// taking full rows, columns, and diagonals is probably better than
// random gaussians!
//
// TODO: If the number of indices requested is close to the number
// available (or equal to it), then this approach is super inefficient.
// Could instead randomly delete indices. But at least we only do it
// once.
static void MakeIndices(ArcFour *rc, Network *net) {
  static_assert(NEIGHBORHOOD >= 0, "must include the pixel itself.");
  auto OneNode = [](ArcFour *rc, RandomGaussian *gauss,
		    int64 *rejected, int64 *duplicate,
		    int indices_per_node,
		    int src_width, int src_height, int src_channels,
		    int dst_width, int dst_height, int dst_channels,
		    int idx) -> vector<uint32> {

    CHECK(indices_per_node <= src_width * src_height * src_channels) <<
    "Can't get " << indices_per_node
    << " distinct indices from a layer with " <<
    src_width << " x " << src_height << " x " << src_channels <<
    " = " << (src_width * src_height * src_channels) << " sources";

    // Whenever we read the neighborhood, we include all source channels.
    CHECK((NEIGHBORHOOD * 2 + 1) * (NEIGHBORHOOD * 2 + 1) *
	  src_channels <= indices_per_node) <<
    "neighborhood doesn't fit in indices!";
    // Which pixel is this?
    const int dst_nodes_per_row = dst_width * dst_channels;
    [[maybe_unused]]
    const int c = idx % dst_channels;
    const int x = (idx % dst_nodes_per_row) / dst_channels;
    const int y = idx / dst_nodes_per_row;

    const double xf = x / (double)dst_width;
    const double yf = y / (double)dst_height;

    // Use hash set for deduplication; we re-sort for locality of access later.
    unordered_set<int> indices;
    // clips xx,yy if they are out of the image.
    // cc must be a valid channel index.
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

    // Find the closest corresponding pixel in the src layer; add all
    // its channels.
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

  auto OneLayer =
    [&rcs, &OneNode, &net](int layer) {
      if (net->layers[layer].type == LAYER_DENSE) {
	// Dense layers have a predetermined structure.
	const int prev_num_nodes = net->num_nodes[layer];
	const int this_num_nodes = net->num_nodes[layer + 1];
	CHECK(net->layers[layer].indices_per_node == prev_num_nodes);
	vector<uint32> *layer_indices = &net->layers[layer].indices;
	CHECK(layer_indices->size() == this_num_nodes * prev_num_nodes);
	for (int n = 0; n < this_num_nodes; n++) {
	  for (int p = 0; p < prev_num_nodes; p++) {
	    (*layer_indices)[n * prev_num_nodes + p] = p;
	  }
	}
	
      } else {
	// Assign sparse layers randomly.
	CHECK(net->layers[layer].type == LAYER_SPARSE);
	
	const int indices_per_node = net->layers[layer].indices_per_node;
	Printf("Intializing %d indices for layer %d...\n",
	       indices_per_node, layer);
	vector<uint32> *layer_indices = &net->layers[layer].indices;
	CHECK_LT(layer + 1, net->width.size());
	CHECK_LT(layer + 1, net->channels.size());
	CHECK_LT(layer + 1, net->num_nodes.size());
	const int src_width = net->width[layer];
	const int src_channels = net->channels[layer];
	CHECK_EQ(0, net->num_nodes[layer] % (src_width * src_channels));
	const int src_height =
	  net->num_nodes[layer] / (src_width * src_channels);
	const int dst_width = net->width[layer + 1];
	const int dst_channels = net->channels[layer + 1];
	CHECK_EQ(0, net->num_nodes[layer + 1] % (dst_width * dst_channels));
	const int dst_height = net->num_nodes[layer + 1] /
	  (dst_width * dst_channels);
	RandomGaussian gauss{rcs[layer]};
	int64 rejected = 0LL, duplicate = 0LL;
	for (int node_idx = 0;
	     node_idx < dst_height * dst_width * dst_channels;
	     node_idx++) {
	  vector<uint32> indices =
	    OneNode(rcs[layer], &gauss, &rejected, &duplicate,
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
      }
      Printf("... done with layer %d.\n", layer);
    };
      
    
  ParallelComp(net->num_layers, OneLayer, 12);

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
  CHECK_EQ(net->num_layers, net->layers.size());
  ParallelComp(net->num_layers, [rcs, &RandomizeFloats, &net](int layer) {
    // XXX such hacks. How to best initialize?

    /*
    RandomizeFloats(powf(0.025f, (layer / 3.0f) + 1.0), rcs[layer], &net->layers[layer].biases);
    RandomizeFloats(1.0f / (net->layers[layer].indices_per_node * ((layer / 3.0f) + 5)),
		    rcs[layer], &net->layers[layer].weights);
    */

    for (float &f : net->layers[layer].biases) f = 0.0f;
    // RandomizeFloats(0.000025f, rcs[layer], &net->layers[layer].biases);
    // RandomizeFloats(0.025f, rcs[layer], &net->layers[layer].weights);

    // The more indices we have, the smaller initial weights we should
    // use.
    float base_weight = 1.0f;
    float gaussian_width = base_weight /
      (float)net->layers[layer].indices_per_node;
    RandomizeFloats(gaussian_width, rcs[layer], &net->layers[layer].weights);
  }, 12);

  DeleteElements(&rcs);
}


// These must be initialized before starting the UI thread!
static constexpr int NUM_VIDEO_STIMULATIONS = 6;
static constexpr int EXPORT_EVERY = 4;

static constexpr bool DRAW_ERRORS = false;

struct UI {
  UI() {
    current_stimulations.resize(NUM_VIDEO_STIMULATIONS);
    current_errors.resize(NUM_VIDEO_STIMULATIONS);
    current_expected.resize(NUM_VIDEO_STIMULATIONS);
  }

  std::shared_mutex video_export_m;
  int current_round = 0;
  double examples_per_second = 0.0;
  vector<Stimulation> current_stimulations;
  vector<Errors> current_errors;
  vector<vector<float>> current_expected;
  Network *current_network = nullptr;
  bool allow_updates = true;
  bool dirty = true;
  bool take_screenshot = false;
  double current_learning_rate = 0.0;
  double current_total_error = 0.0;

  void SetTakeScreenshot() {
    WriteMutexLock ml(&video_export_m);
    take_screenshot = true;
    dirty = true;
  }
  
  void ExportRound(int r) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      current_round = r;
      // dirty = true;
    }
  }

  void ExportExamplesPerSec(double eps) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      examples_per_second = eps;
      // dirty = true;
    }
  }

  void ExportLearningRate(double rl) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      current_learning_rate = rl;
      // dirty = true;
    }
  }

  void ExportNetworkToVideo(const Network &net) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      if (current_network == nullptr) {
	current_network = Network::Clone(net);
      } else {
	current_network->CopyFrom(net);
      }
      dirty = true;
    }
  }

  void ExportStimulusToVideo(int example_id, const Stimulation &stim) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      CHECK_GE(example_id, 0);
      CHECK_LT(example_id, current_stimulations.size());
      current_stimulations[example_id] = stim;
      dirty = true;
    }
  }

  void ExportExpectedToVideo(int example_id,
			     const vector<float> &expected) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      CHECK_GE(example_id, 0);
      CHECK_LT(example_id, current_expected.size());
      current_expected[example_id] = expected;
      dirty = true;
    }
  }

  void ExportErrorsToVideo(int example_id, const Errors &err) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      CHECK_GE(example_id, 0);
      CHECK_LT(example_id, current_errors.size());
      current_errors[example_id] = err;
      dirty = true;
    }
  }

  void ExportTotalErrorToVideo(double t) {
    WriteMutexLock ml(&video_export_m);
    if (allow_updates) {
      current_total_error = t;
    }
  }

  // TODO: For this drawing stuff, maybe do it in terms of headless
  // ImageRGBA, and then UI thread just blits that stuff to SDL screen.
  // This lets us export training images for movie, and maybe make the
  // rendering code more portable (e.g. could have web-based UI rather
  // than SDL)?

  // Returns unscaled width, height, and scale (= number of screen
  // pixels per layer pixel)
  static std::tuple<int, int, int>
  PixelSize(const Network &net, int layer) {
    auto MakeScale =
      [](int w, int h) {
	int d = std::max(w, h);
	return (d <= 16) ? 3 : (d <= 128) ? 2 : 1;
      };
    switch (net.renderstyle[layer]) {
    default:
    case RENDERSTYLE_RGB: {
      // Could do a hybrid where we use 3 channels per pixel, but still
      // show them all
      int w = net.width[layer], h = net.height[layer];
      return {w, h, MakeScale(w, h)};
    }
    case RENDERSTYLE_FLAT: {
      int w = net.width[layer] * net.channels[layer];
      int h = net.height[layer];
      return {w, h, MakeScale(w, h)};
    }

    case RENDERSTYLE_INPUTXY:
      return {NOMINAL_CHAR_SIZE, NOMINAL_CHAR_SIZE, 1};
    case RENDERSTYLE_OUTPUTXY:
      return {std::max(NOMINAL_CHAR_SIZE, EmbeddedFont::CHAR_WIDTH * 26),
	      NOMINAL_CHAR_SIZE + 1 + EmbeddedFont::CHAR_HEIGHT,
	      1};
    }
  }

  static int ErrorWidth(const Network &net, int layer) {
    switch (net.renderstyle[layer]) {
    default:
      return net.width[layer];
    }
  }

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
	uint8 r = ((x + y) & 1) ? 0xFF : 0x00;
	uint8 g = 0x00;
	uint8 b = ((x + y) & 1) ? 0xFF : 0x00;
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
  }

  // Supposedly SDL prefers this to be called from the main thread.
  void Loop() {
    [[maybe_unused]]
    int mousex = 0, mousey = 0;
    int vlayer = 0;

    for (;;) {
      // int round = ReadWithLock(&video_export_m, &current_round);
      {
	ReadMutexLock ml(&video_export_m);
	if (dirty) {
	  sdlutil::clearsurface(screen, 0x0);
	  const char *paused_msg = allow_updates ? "" : " [^2VIDEO PAUSED]";
	  string menu = StringPrintf(
	      "  round ^3%d ^1|  ^3%0.4f^0 eps    "
	      "^1%.6f^< learning rate   ^1%.6f^< total err%s",
	      current_round,
	      examples_per_second,
	      current_learning_rate,
	      current_total_error,
	      paused_msg);
	  font->draw(2, 2, menu);

	  if (current_network == nullptr) {
	    font->draw(50, 50, "[ ^2No network yet^< ]");
	    SDL_Flip(screen);
	    continue;
	  }

	  int max_width = 0;
	  for (int layer = 0;
	       layer < current_network->num_layers + 1;
	       layer++) {
	    int w = std::get<0>(PixelSize(*current_network, layer));
	    if (DRAW_ERRORS)
	      w += ErrorWidth(*current_network, layer) * 2 + 2;
	    max_width = std::max(w, max_width);
	  }
	  max_width += 4;

	  CHECK(current_stimulations.size() == NUM_VIDEO_STIMULATIONS);
	  CHECK(current_errors.size() == NUM_VIDEO_STIMULATIONS);
	  for (int s = 0; s < NUM_VIDEO_STIMULATIONS; s++) {
	    const Stimulation &stim = current_stimulations[s];
	    const Errors &err = current_errors[s];

	    // Skip if not yet set. These get initialized to empty
	    // sentinels and then updated by the training thread
	    // periodically.
	    if (stim.num_layers == 0 ||
		err.num_layers == 0)
	      continue;

	    CHECK(stim.values.size() == current_network->num_layers + 1);
	    CHECK(stim.values.size() == current_network->renderstyle.size());
	    CHECK(err.error.size() == current_network->num_layers);

	    const int xstart = 4 + s * max_width;
	    if (xstart >= SCREENW)
	      break;


	    int ystart = 24;
	    for (int l = 0; l < stim.values.size(); l++) {

	      switch (current_network->renderstyle[l]) {

	      case RENDERSTYLE_OUTPUTXY: {
		const vector<float> &outs = stim.values[l];
		for (int i = 0; i < 26; i++) {
		  const uint8 v = FloatByte(outs[INPUT_LAYER_SIZE + i]);
		  EmbeddedFont::Blit('A' + i,
				     xstart + EmbeddedFont::CHAR_HEIGHT * i,
				     ystart + NOMINAL_CHAR_SIZE + 1,
				     [this, v](int x, int y) {
				       sdlutil::drawclippixel(
					   screen, x, y,
					   v, v, 0xFF);
				     },
				     [this](int x, int y) {
				       sdlutil::drawclippixel(
					   screen, x, y,
					   0, 0, 0);
				     });
		}
	      }
		// FALLTHROUGH
	      case RENDERSTYLE_INPUTXY: {
		const vector<float> &values = stim.values[l];
		sdlutil::FillRectRGB(screen, xstart, ystart,
				     NOMINAL_CHAR_SIZE,
				     NOMINAL_CHAR_SIZE,
				     40, 40, 40);

		constexpr double sqerr = 1.0f / (NOMINAL_CHAR_SIZE *
						 NOMINAL_CHAR_SIZE);
		
		auto DrawPath = [xstart, ystart, &values](
		    int idx, int num_pts,
		    uint8 r, uint8 g, uint8 b) {
		    float x = values[idx + 0];
		    float y = values[idx + 1];

		    auto Line = [xstart, ystart,
				 r, g, b](float x1, float y1,
					  float x2, float y2) {
			sdlutil::drawclipline(
			    screen,
			    xstart + x1 * NOMINAL_CHAR_SIZE,
			    ystart + y1 * NOMINAL_CHAR_SIZE,
			    xstart + x2 * NOMINAL_CHAR_SIZE,
			    ystart + y2 * NOMINAL_CHAR_SIZE,
			    r, g, b);
		      };
		    
		    for (int i = 0; i < num_pts; i++) {
		      float cx = values[idx + 2 + i * 4 + 0];
		      float cy = values[idx + 2 + i * 4 + 1];
		      float dx = values[idx + 2 + i * 4 + 2];
		      float dy = values[idx + 2 + i * 4 + 3];

		      for (const auto [xx, yy] :
			     TesselateQuadraticBezier<double>(
				 x, y, cx, cy, dx, dy, sqerr)) {
			Line(x, y, xx, yy);
			x = xx;
			y = yy;
		      }
		    }
		  };

		DrawPath(0, ROW0_MAX_PTS, 0xFF, 0xFF, 0x00);
		DrawPath(2 + ROW0_MAX_PTS * 4,
			 ROW1_MAX_PTS, 0x00, 0xFF, 0x00);
		DrawPath(2 + ROW0_MAX_PTS * 4 +
			 2 + ROW1_MAX_PTS * 4,
			 ROW2_MAX_PTS, 0x00, 0xFF, 0xFF);
		
		break;
	      }
	      case RENDERSTYLE_RGB:
		for (int y = 0; y < current_network->height[l]; y++) {
		  int yy = ystart + y;
		  if (yy >= SCREENH) break;

		  for (int x = 0; x < current_network->width[l]; x++) {

		    int xx = xstart + x;
		    if (x >= SCREENW) break;

		    int cidx = y * current_network->width[l] *
		      current_network->channels[l] +
		      x * current_network->channels[l];

		    // XXX! Need to support more than 3 channels for
		    // chess. Color is dubious anyway?
		    switch (current_network->channels[l]) {
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

	      case RENDERSTYLE_FLAT:
		for (int y = 0; y < current_network->height[l]; y++) {
		  const int yy = ystart + y;
		  for (int x = 0;
		       x < current_network->width[l] *
			 current_network->channels[l];
		       x++) {
		    const int xx = xstart + x;
		    if (x >= SCREENW) break;
		    const int cidx =
		      y * current_network->width[l] *
		      current_network->channels[l] + x;
		    const uint8 v = FloatByte(stim.values[l][cidx]);
		    sdlutil::drawpixel(screen, xx, yy, v, v, v);
		  }
		}
		break;
	      }

	      // Now errors.
	      // (XXX Something wrong with this? It draws on top of
	      // the output)
	      if (DRAW_ERRORS && l > 0) {
		const int exstart = xstart +
		  std::get<0>(PixelSize(*current_network, l));
		const vector<float> &errs = err.error[l - 1];
		switch (current_network->renderstyle[l]) {
		default:
		case RENDERSTYLE_FLAT:
		case RENDERSTYLE_RGB:
		  Error2D<2>(errs, exstart, ystart,
			     current_network->width[l] *
			     current_network->channels[l],
			     current_network->height[l]);
		}
	      }

	      ystart += std::get<1>(PixelSize(*current_network, l)) + 4;
	    }


	    if (vlayer >= 0) {
	      double tot = 0.0;
	      int yz = ystart + 4;
	      const vector<float> &vals = stim.values[vlayer];
	      for (int i = 0; i < vals.size(); i++) {
		tot += vals[i];
		if (i < 32) {
		  // Font color.
		  const int c = vals[i] > 0.5f ? 0 : 1;
		  if (vlayer > 0) {
		    const float e = vlayer > 0 ? err.error[vlayer - 1][i] : 0.0;
		    // Font color.
		    const int sign = (e == 0.0) ? 4 : (e < 0.0f) ? 2 : 5;
		    font->draw(xstart, yz,
			       StringPrintf("^%d%.9f ^%d%.12f",
					    c, vals[i], sign, e));
		  } else {
		    font->draw(xstart, yz,
			       StringPrintf("^%d%.9f", c, vals[i]));
		  }
		  yz += FONTHEIGHT;
		}
	      }
	      font->draw(xstart, yz,
			 StringPrintf("[%d] tot: %.9f", vlayer, tot));
	    }
	  }

	  SDL_Flip(screen);

	  if (take_screenshot) {
	    string scfile = StringPrintf("video/train%d.png", current_round);
	    sdlutil::SavePNG(scfile, screen);
	    Printf("Wrote screenshot to %s\n", scfile.c_str());
	    take_screenshot = false;
	  }
	  
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
	  case SDLK_SPACE:
	    {
	      WriteMutexLock ml(&video_export_m);
	      allow_updates = !allow_updates;
	    }
	    break;

	  case SDLK_v:
	    {
	      ReadMutexLock ml(&video_export_m);
	      // Allow vlayer to go to -1 (off), but wrap around
	      // below that.

	      if (current_network != nullptr) {
		vlayer++;
		if (vlayer < -1) {
		  // There are actually num_layers + 1 stimulations.
		  vlayer = current_network->num_layers;
		} else if (vlayer > current_network->num_layers) {
		  vlayer = -1;
		}
		dirty = true;
	      }
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

};


static std::unique_ptr<Network> CreateInitialNetwork(ArcFour *rc) {

  const int num_layers = 4;
  const vector<int> width_config =
    { INPUT_LAYER_SIZE,
      INPUT_LAYER_SIZE,
      INPUT_LAYER_SIZE * 2,
      OUTPUT_LAYER_SIZE,
      OUTPUT_LAYER_SIZE, };

  // If zero, automatically factor to make square-ish.
  const vector<int> height_config =
    { 0, 0, 0, 0, 0, };

  // input layer could maybe be thought of as two-channel (x,y),
  // but output has extra stuff (predicted character). Instead
  // we just think of these as flat and do custom rendering and
  // index assignment.
  const vector<int> channels = { 1, 1, 1, 1, 1, };

  // When zero, create a dense layer.
  const vector<int> indices_per_node_config =
    {
      // First layer dense
      0,
      // Then half
      INPUT_LAYER_SIZE >> 1,
      INPUT_LAYER_SIZE >> 1,      
      INPUT_LAYER_SIZE >> 1,
    };

  const vector<uint32_t> renderstyle = {
    RENDERSTYLE_INPUTXY,
    RENDERSTYLE_FLAT,
    RENDERSTYLE_FLAT,
    RENDERSTYLE_FLAT,
    RENDERSTYLE_OUTPUTXY,
  };

  const vector<TransferFunction> transfer_functions = {
    LEAKY_RELU,
    LEAKY_RELU,
    LEAKY_RELU,
    LEAKY_RELU,
  };

  vector<int> height, width;
  CHECK(height_config.size() == width_config.size());
  for (int i = 0; i < height_config.size(); i++) {
    int w = width_config[i];
    int h = height_config[i];
    CHECK(w > 0);
    CHECK(h >= 0);
    if (h == 0) {
      if (w == 1) {
	width.push_back(1);
	height.push_back(1);
      } else {
	// Try to make a rectangle that's squareish.
	vector<int> factors = Util::Factorize(w);

	CHECK(!factors.empty()) << w << " has no factors??";

	// XXX Does this greedy approach produce good results?
	int ww = factors.back(), hh = 1;
	factors.pop_back();

	for (int f : factors) {
	  if (ww < hh)
	    ww *= f;
	  else
	    hh *= f;
	}

	CHECK(ww * hh == w);
	width.push_back(ww);
	height.push_back(hh);
      }
    } else {
      width.push_back(w);
      height.push_back(h);
    }
  }
  
  // num_nodes = width * height * channels
  // //  indices_per_node = indices_per_channel * channels
  // //  vector<int> indices_per_node;
  vector<int> num_nodes;
  CHECK_EQ(width.size(), height.size());
  CHECK_EQ(width.size(), channels.size());  
  CHECK_EQ(width.size(), renderstyle.size());
  CHECK_EQ(num_layers + 1, height.size());
  CHECK_EQ(num_layers, indices_per_node_config.size());
  CHECK_EQ(num_layers, transfer_functions.size());  
  for (int i = 0; i < num_layers + 1; i++) {
    CHECK(width[i] >= 1);
    CHECK(height[i] >= 1);
    CHECK(channels[i] >= 1);
    num_nodes.push_back(width[i] * height[i] * channels[i]);
  }

  vector<int> indices_per_node;
  vector<LayerType> layer_type;
  for (int i = 0; i < indices_per_node_config.size(); i++) {
    int ipc = indices_per_node_config[i];
    if (ipc == 0) {
      layer_type.push_back(LAYER_DENSE);
      indices_per_node.push_back(width[i] * height[i] * channels[i]);
    } else {
      CHECK(ipc > 0);
      CHECK(ipc <= width[i] * height[i] * channels[i]);
      layer_type.push_back(LAYER_SPARSE);
      indices_per_node.push_back(ipc);
    }
  }

  std::unique_ptr<Network> net =
    std::make_unique<Network>(
	num_nodes, indices_per_node, transfer_functions);
  net->width = width;
  net->height = height;
  net->channels = channels;
  net->renderstyle = renderstyle;

  // XXX should probably be ctor arg?
  CHECK(net->layers.size() == layer_type.size());
  for (int i = 0; i < net->layers.size(); i++) {
    net->layers[i].type = layer_type[i];
  }

  Printf("Randomize weights:\n");
  RandomizeNetwork(rc, net.get());
  Printf("Gen indices:\n");
  MakeIndices(rc, net.get());
  Printf("Invert indices:\n");
  Network::ComputeInvertedIndices(net.get());

  // Should be well-formed now.
  net->StructuralCheck();
  net->NaNCheck("load net.val");

  return net;
}

static UI *ui = nullptr;
static LoadFonts *load_fonts = nullptr;

static void TrainThread() {
  Timer setup_timer;

  CHECK(ui != nullptr) << "Must be created first.";
  CHECK(load_fonts != nullptr) << "Must be created first.";  
  
  // Number of training examples per round of training.
  static constexpr int EXAMPLES_PER_ROUND = 4096;
  // Try to keep twice that in the queue all the time.
  static constexpr int EXAMPLE_QUEUE_TARGET =
    std::max(EXAMPLES_PER_ROUND * 2, 1024);

  // Write a screenshot of the UI (to show training progress for
  // videos, etc.) every time the network does this many rounds.
  static constexpr int SCREENSHOT_ROUND_EVERY = 50;
  
  // On a verbose round, we write a network checkpoint and maybe some
  // other stuff to disk. XXX: Do this based on time, since round
  // speed can vary a lot based on other parameters!
  static constexpr int VERBOSE_ROUND_EVERY = 250;
  
  string start_seed = StringPrintf("%d  %lld", getpid(), (int64)time(nullptr));
  Printf("Start seed: [%s]\n", start_seed.c_str());
  ArcFour rc(start_seed);
  rc.Discard(2000);

  // Load the existing network from disk or create the initial one.
  Timer initialize_network_timer;
  std::unique_ptr<Network> net;

  // Try loading from disk; null on failure.
  net.reset(Network::ReadNetworkBinary("net.val"));

  if (net.get() == nullptr) {
    Printf("Initializing new network...\n");
    net = CreateInitialNetwork(&rc);
    CHECK(net.get() != nullptr);

    Printf("Writing network so we don't have to do that again...\n");
    Network::SaveNetworkBinary(*net, "net.val");
  }

  Printf("Initialized network in %.1fms.\n", initialize_network_timer.MS());

  // Create kernels right away so that we get any compilation errors early.
  ForwardLayerCL forwardlayer{global_cl, *net};
  SetOutputErrorCL setoutputerror{global_cl};
  BackwardLayerCL backwardlayer{global_cl, *net};
  UpdateWeightsCL updateweights{global_cl, *net};
  
  NetworkGPU net_gpu{global_cl, net.get()};

  if (ReadWithLock(&train_should_die_m, &train_should_die))
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
    bool should_die = ReadWithLock(&train_should_die_m, &train_should_die);
    if (should_die) {
      Printf("Train thread signaled death.\n");
      Printf("Saving to net.val...\n");
      Network::SaveNetworkBinary(*net, "net.val");
      // Util::WriteFile("eval/nextframe.txt", StringPrintf("%d\n", eval_frame_num));
    }
    return should_die;
  };

  // Number of threads to allow for simultaneous writing of frames.
  // Asynchronously write_frames{EVAL_ONLY ? 2 : 8};

  if (ShouldDie()) return;


  // (Note that the below actually works while the examples are still
  // being loaded, but maybe overkill since this dataset should load
  // pretty fast? So syncing here.)
  Printf("Waiting for fonts...\n");
  CHECK(load_fonts != nullptr);
  load_fonts->Sync();
  Printf("Fonts all loaded.\n");


  struct TrainingExample {
    vector<float> input;
    vector<float> output;
  };
  // Training examples don't depend on the learning process, so are produced
  // in a separate thread. This mutex protects the deque (only).
  std::shared_mutex training_examples_m;
  // XXX could just be vector, actually?
  deque<TrainingExample> training_examples;

  // Returns true if successful.
  auto PopulateExampleFromFont =
    [](ArcFour *rc, const TTF *ttf, TrainingExample *example) -> bool {

      /*
      static constexpr int INPUT_LAYER_SIZE =
	// start point, then beziers as control point, end point.
	2 + ROW0_MAX_PTS * (2 + 2) +
	2 + ROW1_MAX_PTS * (2 + 2) +
	2 + ROW2_MAX_PTS * (2 + 2);
      */

      const int char_offset = RandTo(rc, 26);
      const int upper_char = 'A' + char_offset;
      const int lower_char = 'a' + char_offset;
      std::vector<TTF::Contour> upper =
	TTF::MakeOnlyBezier(ttf->GetContours(upper_char));
      std::vector<TTF::Contour> lower =
	TTF::MakeOnlyBezier(ttf->GetContours(lower_char));

      // Only room for three contours.
      // XXX: Perhaps we should reject the entire font if it will
      // ever fail these tests; otherwise we are biasing against
      // characters with more contours (e.g. B).
      if (upper.size() > 3 || lower.size() > 3)
	return false;

      // Also don't allow empty characters.
      if (upper.empty() || lower.empty())
	return false;
      
      auto ByPathSizeDesc = [](const Contour &a, const Contour &b) {
	  return b.paths.size() < a.paths.size();
	};

      std::sort(upper.begin(), upper.end(), ByPathSizeDesc);
      std::sort(lower.begin(), lower.end(), ByPathSizeDesc);

      // We need something to put in rows if there are fewer than
      // three contours. We treat this as an empty path starting at 0,0.
      while (upper.size() < 3) {
	TTF::Contour degenerate(0.0f, 0.0f);
	upper.push_back(degenerate);
      }
      while (lower.size() < 3) {
	TTF::Contour degenerate(0.0f, 0.0f);
	lower.push_back(degenerate);
      }

      
      if (upper[0].paths.size() > ROW0_MAX_PTS)
	return false;
      if (upper[1].paths.size() > ROW1_MAX_PTS)
	return false;
      if (upper[2].paths.size() > ROW2_MAX_PTS)
	return false;

      if (lower[0].paths.size() > ROW0_MAX_PTS)
	return false;
      if (lower[1].paths.size() > ROW1_MAX_PTS)
	return false;
      if (lower[2].paths.size() > ROW2_MAX_PTS)
	return false;

      // All right, all is well!

      // TODO: Consider random transform of input data; ideal network
      // should be robust against small translations, scaling, even
      // rotation.
      
      auto PopulateRow = [](std::vector<float> *floats,
			    int row_start,
			    // Not including start position.
			    int row_max_pts,
			    const Contour &contour) {
	constexpr int HDR = 2;
	(*floats)[row_start + 0] = contour.startx;
	(*floats)[row_start + 1] = contour.starty;
	for (int i = 0; i < row_max_pts; i++) {
	  // When we run out of real points, pad with degenerate
	  // zero-length curves at the start point.
	  float cx = i < contour.paths.size() ?
			 contour.paths[i].cx : contour.startx;
	  float cy = i < contour.paths.size() ?
			 contour.paths[i].cy : contour.starty;
	  float x = i < contour.paths.size() ?
			contour.paths[i].x : contour.startx;
	  float y = i < contour.paths.size() ?
			contour.paths[i].y : contour.starty;

	  (*floats)[row_start + HDR + i * 4 + 0] = cx;
	  (*floats)[row_start + HDR + i * 4 + 1] = cy;
	  (*floats)[row_start + HDR + i * 4 + 2] = x;
	  (*floats)[row_start + HDR + i * 4 + 3] = y;
	}

	return row_start + HDR + 4 * row_max_pts;
      };


      // Inputs
      {
	example->input.resize(INPUT_LAYER_SIZE);

	int next_idx = 0;
	next_idx = PopulateRow(&example->input, next_idx, ROW0_MAX_PTS,
			       upper[0]);
	next_idx = PopulateRow(&example->input, next_idx, ROW1_MAX_PTS,
			       upper[1]);
	next_idx = PopulateRow(&example->input, next_idx, ROW2_MAX_PTS,
			       upper[2]);

	CHECK(next_idx == example->input.size()) << next_idx << " vs "
						 << example->input.size();
      }


      // Outputs
      {
	example->output.resize(OUTPUT_LAYER_SIZE);
	int next_idx = 0;
	next_idx = PopulateRow(&example->output, next_idx, ROW0_MAX_PTS,
			       lower[0]);
	next_idx = PopulateRow(&example->output, next_idx, ROW1_MAX_PTS,
			       lower[1]);
	next_idx = PopulateRow(&example->output, next_idx, ROW2_MAX_PTS,
			       lower[2]);

	for (int i = 0; i < 26; i++) {
	  example->output[next_idx] = i == char_offset ? 1.0f : 0.0f;
	  next_idx++;
	}
	
	CHECK(next_idx == example->output.size());
      }

      if (CHECK_NANS) {
	for (float f : example->input) { CHECK(!std::isnan(f)); }
	for (float f : example->output) { CHECK(!std::isnan(f)); }
      }

      return true;
    };

  auto MakeTrainingExamplesThread = [&training_examples_m,
				     &training_examples,
				     &PopulateExampleFromFont]() {
    Printf("Training example thread startup.\n");
    string seed = StringPrintf("make ex %lld", (int64)time(nullptr));
    ArcFour rc(seed);
    rc.Discard(2000);

    for (;;) {
      if (ReadWithLock(&train_should_die_m, &train_should_die)) {
	return;
      }

      training_examples_m.lock_shared();
      // Make sure we have plenty of examples so that learning doesn't stall.
      if (training_examples.size() < EXAMPLE_QUEUE_TARGET) {
	training_examples_m.unlock_shared();

	TrainingExample example;	
	{
	  load_fonts->fonts_m.lock_shared();

	  if (load_fonts->fonts.size() < ENOUGH_FONTS) {
	    load_fonts->fonts_m.unlock_shared();
	    Printf("Not enough training data loaded yet!\n");
	    std::this_thread::sleep_for(1s);
	    continue;
	  } else {
	    const int idx = RandTo(&rc, load_fonts->fonts.size());

	    bool ok = PopulateExampleFromFont(&rc,
					      load_fonts->fonts[idx],
					      &example);
	    load_fonts->fonts_m.unlock_shared();

	    if (ok) {
	      WriteMutexLock ml(&training_examples_m);
	      training_examples.push_back(std::move(example));
	    }

	    // PERF: If not ok, we could mark this as a bad training
	    // example so we don't keep trying (or just remove it from
	    // the vector?)
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
    output_error_ms = 0.0, update_ms = 0.0,
    error_history_ms = 0.0, eval_ms = 0.0;
  Timer total_timer;
  for (int rounds_executed = 0; ; rounds_executed++) {
    Timer round_timer;

    if (ShouldDie()) return;

    while (std::optional<string> excl = GetExclusiveApp ()) {
      Printf("(Sleeping because of exclusive app %s)\n",
	     excl.value().c_str());
      std::this_thread::sleep_for(5000ms);
    }
    
    if (VERBOSE > 2) Printf("\n\n");
    Printf("** NET ROUND %d (%d in this process) **\n",
	   net->rounds, rounds_executed);
    
    // When starting from a fresh network, consider this:

    // XXX: I think the learning rate should maybe depend on the
    // number of examples per round, since we integrate over lots of
    // them. We could end up having a total error for a single node of
    // like +EXAMPLES_PER_ROUND or -EXAMPLES_PER_ROUND, which could
    // yield an unrecoverable-sized update. (Alternatively, we could
    // cap or norm the error values.) We now divide the round learning
    // rate to an example learning rate, below.

    //    const float round_learning_rate =
    //      std::min(0.95, std::max(0.10, 4 * exp(-0.2275 *
    //                         (net->rounds / 100.0 + 1)/3.0)));

    // This one worked well to get chess started, but drops off too
    // soon I think.
    // const float round_learning_rate =
    // std::min(0.10, std::max(0.002, 4 * exp(-0.2275 *
    //     (net->rounds / 100.0 + 1)/3.0)));
    auto Linear =
      [](double start, double end, double round_target, double input) {
	if (input < 0.0) return start;
	if (input > round_target) return end;
	double height = end - start;
	double f = input / round_target;
	return start + f * height;
      };
    const float round_learning_rate =
      Linear(0.10, 0.002, 500000.0, net->rounds);

    // const float round_learning_rate =
    //     std::min(0.125, std::max(0.002, 2 * exp(-0.2275 *
    //                         (net->rounds + 1)/3.0)));

    // const float round_learning_rate =
    // std::min(0.125, std::max(0.002, 2 * exp(-0.2275 *
    //         (net->rounds / 1000.0 + 1)/3.0)));

    // const float round_learning_rate = 0.0025;

    CHECK(!std::isnan(round_learning_rate));
    if (VERBOSE > 2) Printf("Learning rate: %.4f\n", round_learning_rate);

    const float example_learning_rate =
      round_learning_rate / (double)EXAMPLES_PER_ROUND;

    if (ShouldDie()) return;

    bool is_verbose_round =
      0 == ((rounds_executed /* + 1 */) % VERBOSE_ROUND_EVERY);
    if (is_verbose_round) {
      Printf("Writing network:\n");
      net_gpu.ReadFromGPU();
      Network::SaveNetworkBinary(*net, "network-checkpoint.bin");
    }

    if (VERBOSE > 2) Printf("Export network:\n");
    ui->ExportRound(net->rounds);
    ui->ExportLearningRate(round_learning_rate);

    const bool take_screenshot =
      net->rounds % SCREENSHOT_ROUND_EVERY == 0;
    if (rounds_executed % EXPORT_EVERY == 0 ||
	take_screenshot) {
      net_gpu.ReadFromGPU();
      ui->ExportNetworkToVideo(*net);
    }

    if (take_screenshot) {
      ui->SetTakeScreenshot();
    }
    
    if (CHECK_NANS) {
      net_gpu.ReadFromGPU();
      net->NaNCheck("round start");
    }

    Timer setup_timer;
    if (VERBOSE > 2) Printf("Setting up batch:\n");

    vector<TrainingExample> examples;
    examples.reserve(EXAMPLES_PER_ROUND);
    do {
      if (!examples.empty()) {
	if (VERBOSE > 0)
	  Printf("Blocked grabbing examples (still need %d)...\n",
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
    vector<vector<float>> expected =
      Map(examples, [](const TrainingExample &te) {
      return te.output;
    });

    setup_ms += setup_timer.MS();

    CHECK_EQ(examples.size(), expected.size());

    if (false && CHECK_NANS /* && net->rounds == 3 */) {
      // Actually run the forward pass on CPU, trying to find the
      // computation that results in nan...
      net_gpu.ReadFromGPU(); // should already be here, but...
      UnParallelComp(
	  examples.size(),
	  [&net, &examples](int example_idx) {
	    // XXX only run on one example...
	    if (example_idx != 0) return;

	    Stimulation stim{*net};
	    for (int src = 0; src < net->num_layers; src++) {
	      // XXX hard coded to leaky_relu
	      auto Forward =
		[](double potential) -> float {
		  return ((potential < 0.0f) ? potential * 0.01f : potential);
		};

	      const vector<float> &src_values = stim.values[src];
	      vector<float> *dst_values = &stim.values[src + 1];
	      const vector<float> &biases = net->layers[src].biases;
	      const vector<float> &weights = net->layers[src].weights;
	      const vector<uint32> &indices = net->layers[src].indices;
	      const int indices_per_node = net->layers[src].indices_per_node;
	      const int num_nodes = net->num_nodes[src + 1];
	      for (int node_idx = 0; node_idx < num_nodes; node_idx++) {

		// Start with bias.
		double potential = biases[node_idx];
		printf("%d|L %d n %d. bias: %f\n",
		       net->rounds, src, node_idx, potential);
		CHECK(!std::isnan(potential)) << node_idx;
		const int my_weights = node_idx * indices_per_node;
		const int my_indices = node_idx * indices_per_node;
		
		for (int i = 0; i < indices_per_node; i++) {
		  const float w = weights[my_weights + i];
		  int srci = indices[my_indices + i];
		  // XXX check dupes
		  CHECK(srci >= 0 && srci < src_values.size()) << srci;
		  const float v = src_values[srci];
		  // PERF: fma()?
		  CHECK(!std::isnan(w) &&
			!std::isnan(v) &&
			!std::isnan(potential)) <<
		    StringPrintf("L %d, n %d. [%d=%d] %f * %f + %f\n",
				 src,
				 node_idx,
				 i, srci, w, v, potential);
		  potential += w * v;
		}
		CHECK(!std::isnan(potential));
		
		CHECK(node_idx >= 0 && node_idx < dst_values->size());
		float out = Forward(potential);
		printf("    %f -> %f\n", potential, out);
		CHECK(!std::isnan(out)) << potential;
		(*dst_values)[node_idx] = out;
	      }
	    }
	  },
	  16);
    }

    // TODO: may make sense to pipeline this loop somehow, so that we
    // can parallelize CPU/GPU duties?

    // Run a batch of images all the way through. (Each layer requires
    // significant setup.)
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
		       ui->ExportStimulusToVideo(example_idx, stim);
		     }
		   }, 16);
      forward_ms += forward_timer.MS();
      kernel_ms += fc.kernel_ms;
      if (VERBOSE > 2) Printf("\n");
    }

    if (CHECK_NANS) {
      for (int example_idx = 0; example_idx < training.size(); example_idx++) {
	Stimulation stim{*net};
	training[example_idx]->ExportStimulation(&stim);
	stim.NaNCheck("forward pass");
      }
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
      CHECK(values.size() == OUTPUT_LAYER_SIZE) <<
	"Chess-specific check; ok to delete";
      for (int i = 0; i < examples.size(); i++) {
	training[i]->ExportOutput(&values);
	CHECK(examples[i].output.size() == values.size());
	for (int j = 0; j < values.size(); j++) {
	  float d = values[j] - examples[i].output[j];
	  /*
	  if (i == 0) printf("%d. %f want %f = %f\n",
			     j, values[j], examples[i].output[j], d);
	  */
	  total_error += fabs(d);
	}
      }
      ui->ExportTotalErrorToVideo(total_error / (double)examples.size());
    }

    const int num_examples = examples.size();
    // But, don't need to keep this allocated.
    examples.clear();

    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("Error calc.\n");
    Timer output_error_timer;
    ParallelComp(
	num_examples,
	[rounds_executed,
	 &setoutputerror, &net_gpu, &training, &expected](int example_idx) {
	  if (rounds_executed % EXPORT_EVERY == 0 &&
	      example_idx < NUM_VIDEO_STIMULATIONS) {
	    // Copy to screen.
	    ui->ExportExpectedToVideo(example_idx, expected[example_idx]);
	  }
	  
	  // PERF could pipeline this copy earlier
	  training[example_idx]->LoadExpected(expected[example_idx]);
	  SetOutputErrorCL::Context sc{&setoutputerror, &net_gpu};
	  sc.SetOutputError(training[example_idx]);
	  /* Printf("."); */
	}, 16);
    output_error_ms += output_error_timer.MS();
    if (VERBOSE > 2) Printf("\n");

    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("Backwards:\n");
    // Also serial, but in reverse.
    Timer backward_timer;
    // We do NOT propagate errors to the input layer, so dst is
    // strictly greater than 0.
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
      for (int example_idx = 0;
	   example_idx < NUM_VIDEO_STIMULATIONS;
	   example_idx++) {
	Errors err{*net};
	training[example_idx]->ExportErrors(&err);
	ui->ExportErrorsToVideo(example_idx, err);
      }
    }

    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("Update weights:\n");
    Timer update_timer;

    // Don't parallelize! These are all writing to the same network
    // weights. Each call is parallelized, though.
    for (int layer = 0; layer < net->num_layers; layer++) {
      UpdateWeightsCL::Context uc{&updateweights, &net_gpu, layer};

      // PERF Faster to try to run these in parallel (maybe
      // parallelizing memory traffic with kernel execution -- but we
      // can't run the kernels at the same time).
      for (int example = 0; example < num_examples; example++) {
	uc.Update(example_learning_rate, training[example], layer);
      }

      // Now we leave the network on the GPU, and the version in the
      // Network object will be out of date. But flush the command
      // queue. (why? I guess make sure that we're totally done
      // writing since other parts of the code assume concurrent reads
      // are ok?)
      uc.Finish();
      /*
      Printf("[%d/%d] = (%.2f%%) ", layer, net->num_layers, 
             layer * 100.0 / net->num_layers);
      */
    }
    update_ms += update_timer.MS();
    if (VERBOSE > 2) Printf("\n");

    if (CHECK_NANS) {
      net_gpu.ReadFromGPU();
      net->NaNCheck("updated weights");
    }

    if (ShouldDie()) return;

    net->rounds++;
    net->examples += EXAMPLES_PER_ROUND;

    double total_ms = total_timer.MS();
    auto Pct = [total_ms](double d) { return (100.0 * d) / total_ms; };
    double denom = rounds_executed + 1;

    // TODO: Would be nice to average this over the last few rounds.
    double round_eps = EXAMPLES_PER_ROUND / (round_timer.MS() / 1000.0);
    // (EXAMPLES_PER_ROUND * denom) / (total_ms / 1000.0)
    ui->ExportExamplesPerSec(round_eps);
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
	     "%.1fms in updating weights (%.1f%%),\n",
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
	     update_ms / denom, Pct(update_ms));
  }

  Printf(" ** Done. **");

  WriteWithLock(&train_done_m, &train_done, true);
}

// Periodically we check to see if any process name matches something
// in this function. If so, we pause training.
static std::optional<string> GetExclusiveApp() {
  vector<string> procs = Top::Enumerate();

  for (const string &proc : procs) {
    string match = Util::lcase(proc);
    if (match == "spel2.exe") return {proc};
    // Can add more here, including regexes etc...
  }

  return nullopt;
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

  SDL_Surface *icon = SDL_LoadBMP("lowercase-icon.bmp");
  if (icon != nullptr) {
    SDL_WM_SetIcon(icon, nullptr);
  }

  screen = sdlutil::makescreen(SCREENW, SCREENH);
  CHECK(screen);

  font = Font::create(screen,
		      "font.png",
		      FONTCHARS,
		      FONTWIDTH, FONTHEIGHT, FONTSTYLES, 1, 3);
  CHECK(font != nullptr) << "Couldn't load font.";

  global_cl = new CL;

  // Start loading fonts in background.
  load_fonts = new LoadFonts(
      []() { return ReadWithLock(&train_should_die_m, &train_should_die); },
      12,
      MAX_FONTS);

  ui = new UI;
  
  std::thread train_thread(&TrainThread);

  ui->Loop();
  
  Printf("Killing train thread (might need to wait for round to finish)...\n");
  WriteWithLock(&train_should_die_m, &train_should_die, true);
  train_thread.join();

  Printf("Train is dead; now UI exiting.\n");

  SDL_Quit();
  return 0;
}

