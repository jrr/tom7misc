// This code was forked from ../../chess/unblind, which came from
// ../../mtoz, which came from ../../redi, so check that for some
// development history / thoughts.

// Goal here is to see if we could use a neural network (probably
// with some additional data) to generate the wordlist. I have
// no idea what to expect wrt the network size, but:
//  - If we can just completely overfit to the data in a model
//    that's smaller than ~150k (plausible??) we this would be winning.
//  - On the other end of the spectrum, simply producing a good
//    probability density function for the next character in the
//    stream (which can clearly be done with even a tiny model),
//    plus a technique like arithmetic coding, would have a good
//    shot of being better than e.g. arithmetic coding on a
//    frequency table or bigraphs, etc.
//  - The middle ground is probably the most interesting: A model
//    that often makes good predictions, but needs some parallel
//    data stream to "correct" it to the correct wordlist.
//
// First cut is, what kind of error rate can we expect for models
// of different sizes?

#include "SDL.h"
#include "SDL_main.h"
#include "../cc-lib/sdl/sdlutil.h"
#include "../cc-lib/sdl/font.h"

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
#include "../cc-lib/gtl/top_n.h"

#include "network.h"

#include "clutil.h"
#include "timer.h"

#define FONTCHARS " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" /* removed icons */
#define FONTSTYLES 7

template<class K, class C>
inline bool ContainsKey(const C &container, const K &key) {
  return container.find(key) != container.end();
}

// TODO: To threadutil, but note that this is C++17.
struct ReadMutexLock {
  explicit ReadMutexLock(std::shared_mutex *m) : m(m) { m->lock_shared(); }
  ~ReadMutexLock() { m->unlock_shared(); }
  std::shared_mutex *m;
};
// Possible to template this over shared_mutex and mutex without
// requiring an argument?
struct WriteMutexLock {
  explicit WriteMutexLock(std::shared_mutex *m) : m(m) { m->lock(); }
  ~WriteMutexLock() { m->unlock(); }
  std::shared_mutex *m;
};

// Read with the mutex that protects it. T must be copyable,
// obviously!
template<class T>
T SharedReadWithLock(std::shared_mutex *m, const T *t) {
  ReadMutexLock ml(m);
  return *t;
}

// Write with the mutex that protects it. T must be copyable.
template<class T>
void SharedWriteWithLock(std::shared_mutex *m, T *t, const T &val) {
  WriteMutexLock ml(m);
  *t = val;
}

static constexpr int VERBOSE = 1;
// Perform somewhat expensive sanity checking for NaNs.
// (Beware: Also enables some other really expensive diagnostics.)
static constexpr bool CHECK_NANS = false;

using namespace std;

using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;

using uint32 = uint32_t;
using uint64 = uint64_t;

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

enum class RenderStyle {
  RGB,
  FLAT,
  LETTERPREDICTION,
};

// For simplicity, the whole model will be dense. This way we don't
// have to encode the indices, and we don't have to implement a sparse
// forward pass in JS (which is not much harder, but does add a little
// bit of code). The speed of executing the model is irrelevant here.
//
// A major consideration is the encoding of the model itself. A list
// of floats is very inefficient for various reasons, but we could
// start there to see if this is promising. A very nice thing would
// be if all of the coefficients and biases were in an interval
// isomorphic to [0, 127] (or perhaps [0, 127^2]), with the illegal
// ASCII characters notched out, and we could just avoid those weights
// in the training process itself. Seems pretty plausible... we could
// just only allow rationals in like [-2.0, 2.0], evenly spaced among
// the k possible values. It doesn't matter if the code to load the
// float arrays is a little complicated.
// So let's just not worry about that for now.

// Let's say that the output is supposed to rank the 27 (including
// newline) characters in the gamut. Would probably be better to
// use softmax, but I don't have code to train that. So it's just
// like LEAKY_RELU output to 27 cells.
//
// In the input, we want to use some context to predict the next
// character. The network could be "recurrent", where some of its
// output is fed into the next round (this lets it learn how to
// most efficiently encode the context), but I don't have the
// apparatus to train that either. Simplest thing is to use X
// characters from the past to initialize it. We could have
// X * 27 cells (one-hot) or we could have X cells which take
// on values 0/27, 1/27, ... 27/27. I think that the latter is
// normally thought of as a worse approach (now the model would
// need to create a bunch of hyperplanes to distinguish those 27
// cases, and there is no sense in which "a" is more similar to "b"
// than "e", for example (well, except that the dictionary is
// sorted). But it would yield a more compact network, so maybe
// worth trying too. For now, we'll take X * 27 cells, one-hot.
// This also gives us a natural way to start, which is to set all
// of these to zeroes. (OTOH if we find that doesn't work, it
// would be easy to just hard code the beginning of the dictionary
// in JS to warm it up.)

#define RADIX 27
#define INPUT_HISTORY 32

#define OUTPUT_LAYER_SIZE RADIX

struct NetworkConfiguration { 

  const int num_layers = 5;
  const vector<int> width =    { INPUT_HISTORY,
				 11,  9,  9, 7, OUTPUT_LAYER_SIZE, };
  const vector<int> height =   { RADIX,
				 11,  9,  7, 7, 1, };
  const vector<int> channels = { 1,   1,   1,  1, 1, 1, };
  vector<int> indices_per_node =
    {     -1, -1, -1, -1, -1, };

  const vector<TransferFunction> transfer_functions =
    { 
     LEAKY_RELU,
     LEAKY_RELU,
     LEAKY_RELU,
     LEAKY_RELU,
     LEAKY_RELU,
    };

  const vector<RenderStyle> style = {
    RenderStyle::FLAT,
    RenderStyle::FLAT,
    RenderStyle::FLAT,
    RenderStyle::FLAT,
    RenderStyle::FLAT,
    RenderStyle::LETTERPREDICTION,
    // letters...
  };

  
  vector<int> num_nodes;
  NetworkConfiguration() {
    CHECK_EQ(width.size(), height.size());
    CHECK_EQ(width.size(), style.size());
    CHECK_EQ(num_layers + 1, height.size());
    CHECK_EQ(num_layers, indices_per_node.size());
    for (int i = 0; i < indices_per_node.size(); i++) {
      // Implied dense.
      if (indices_per_node[i] == -1) {
	indices_per_node[i] = width[i] * height[i] * channels[i];
      }
    }
    
    for (int i = 0; i < num_layers + 1; i++) {
      CHECK(width[i] >= 1);
      CHECK(height[i] >= 1);
      CHECK(channels[i] >= 1);
      num_nodes.push_back(width[i] * height[i] * channels[i]);
    }
  }
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

      Network *net = net_gpu->net;
      
      // Printf("Setup kernel..\n");

      const TransferFunction transfer_function =
	net->layers[layer].transfer_function;
      cl_kernel kernel = parent->kernels[transfer_function];

      // Can't have multiple threads setting a kernel's argument at one time.
      {
	WriteMutexLock ml(&parent->m);

	cl_int indices_per_node = net->layers[layer].indices_per_node;
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
	  clSetKernelArg(parent->kernel, 0, sizeof (cl_float),
			 (void *)&learning_rate));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 1, sizeof (cl_int),
			 (void *)&indices_per_node));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 2, sizeof (cl_mem),
			 (void *)&layer_error));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 3, sizeof (cl_mem),
			 (void *)&layer_indices));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 4, sizeof (cl_mem),
			 (void *)&layer_values));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 5, sizeof (cl_mem),
			 (void *)&layer_weights));
      CHECK_SUCCESS(
	  clSetKernelArg(parent->kernel, 6, sizeof (cl_mem),
			 (void *)&layer_biases));

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

  #define NEIGHBORHOOD 1
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

  ParallelComp(net->num_layers,
	       [&width, &channels, &rcs, &OneNode, &net](int layer) {
    int indices_per_node = net->layers[layer].indices_per_node;
    if (indices_per_node == -1) {
      indices_per_node = net->num_nodes[layer];
    }
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
    RandomizeFloats(0.025f, rcs[layer], &net->layers[layer].weights);
  }, 12);

  DeleteElements(&rcs);
}


// These must be initialized before starting the UI thread!
static constexpr int NUM_VIDEO_STIMULATIONS = 5;
static constexpr int EXPORT_EVERY = 8;
static constexpr int LOG_EXPORT_EVERY = 25;
// static constexpr int EXPORT_EVERY = 1;
static std::shared_mutex video_export_m;
static int current_round = 0;
static double examples_per_second = 0.0;
static vector<Stimulation> current_stimulations;
static vector<Errors> current_errors;
static vector<vector<float>> current_expected;
static Network *current_network = nullptr;
static bool allow_updates = true;
static bool dirty = true;
static double current_learning_rate = 0.0;
static double current_avg_error = 0.0;

static void ExportRound(int r) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    current_round = r;
    // dirty = true;
  }
}

static void ExportExamplesPerSec(double eps) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    examples_per_second = eps;
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
    if (current_network == nullptr) {
      current_network = Network::Clone(net);
    } else {
      current_network->CopyFrom(net);
    }
    dirty = true;
  }
}

static void ExportStimulusToVideo(int example_id, const Stimulation &stim) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK_GE(example_id, 0);
    CHECK_LT(example_id, current_stimulations.size());
    current_stimulations[example_id] = stim;
    dirty = true;
  }
}

static void ExportExpectedToVideo(int example_id,
				  const vector<float> &expected) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK_GE(example_id, 0);
    CHECK_LT(example_id, current_expected.size());
    current_expected[example_id] = expected;
    dirty = true;
  }
}

static void ExportErrorsToVideo(int example_id, const Errors &err) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    CHECK_GE(example_id, 0);
    CHECK_LT(example_id, current_errors.size());
    current_errors[example_id] = err;
    dirty = true;
  }
}

static void ExportAvgErrorToVideo(double t) {
  WriteMutexLock ml(&video_export_m);
  if (allow_updates) {
    current_avg_error = t;
  }
}

#define SHOWBEST 5

// Returns unscaled width, height, and scale (= number of screen
// pixels per layer pixel)
static std::tuple<int, int, int> PixelSize(const NetworkConfiguration &config, int layer) {
  auto MakeScale =
    [](int w, int h) {
      int d = std::max(w, h);
      return (d <= 16) ? 3 : (d <= 128) ? 2 : 1;
    };
  switch (config.style[layer]) {
  default:
  case RenderStyle::RGB: {
    // Could do a hybrid where we use 3 channels per pixel, but still show them all
    int w = config.width[layer], h = config.height[layer];
    return {w, h, MakeScale(w, h)};
  }
  case RenderStyle::FLAT: {
    int w = config.width[layer] * config.channels[layer];
    int h = config.height[layer];
    return {w, h, MakeScale(w, h)};
  }
  case RenderStyle::LETTERPREDICTION: {
    int w = FONTWIDTH * (INPUT_HISTORY + 6);
    int h = FONTHEIGHT * SHOWBEST;
    return {w, h, 1};
  }
  }
}

static int ErrorWidth(const NetworkConfiguration &config, int layer) {
  switch (config.style[layer]) {
  case RenderStyle::LETTERPREDICTION:
    return 0;
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


// Note, writes \n as |
static char CharOf(int i) {
  if (i == 26) return '|';
  else return 'a' + i;
}

static int SymbolOf(char c) {
  if (c == '\n') {
    return 26;
  } else if (c >= 'a' && c <= 'z') {
    return c - 'a';
  } else {
    CHECK(false) << "Bad character " << (int)c;
    return 0;
  }
}


static void UIThread() {
  // TODO: Eliminate it, or make it like ui_configuration...
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
	string menu = StringPrintf(
	    "  round ^3%d ^1|  ^3%0.4f^0 eps    "
	    "^1%.6f^< learning rate   ^1%.6f^< avg batch err%s",
	    current_round,
	    examples_per_second,
	    current_learning_rate,
	    current_avg_error,
	    paused_msg);
	font->draw(2, 2, menu);
	
	if (current_network == nullptr) {
	  font->draw(50, 50, "[ ^2No network yet^< ]");
	  SDL_Flip(screen);
	  continue;
	}

	// Enough room for the text value/error table.
	int max_width = 29 * FONTWIDTH;
	
	for (int layer = 0; layer < config.num_layers + 1; layer++) {
	  int w, h, s;
	  std::tie(w, h, s) = PixelSize(config, layer);
	  int ww = w * s;
	  if (DRAW_ERRORS)
	    ww += ErrorWidth(config, layer) * 2 + 2;
	  max_width = std::max(ww, max_width);
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
	  
	  CHECK(stim.values.size() == config.num_layers + 1);
	  CHECK(stim.values.size() == config.style.size());
	  CHECK(err.error.size() == config.num_layers);
	  
	  const int xstart = 4 + s * max_width;
	  if (xstart >= SCREENW)
	    break;


	  // For the 0th stimulation, draw the word..
	  string input;
	  CHECK(stim.values[0].size() == INPUT_HISTORY * RADIX);
	  for (int i = 0; i < INPUT_HISTORY; i++) {
	    // Get char that's 1
	    for (int r = 0; r < RADIX; r++) {
	      if (stim.values[0][i * RADIX + r] > 0.5f) {
		input += CharOf(r);
	      }
	    }
	  }
	  // input += "_";
	  font->draw(xstart, 18, input);
	  
	  int ystart = 32;
	  for (int l = 0; l < stim.values.size(); l++) {

	    switch (config.style[l]) {
	    case RenderStyle::LETTERPREDICTION: {
	      const vector<float> &output = stim.values[l];
	      CHECK_EQ(output.size(), RADIX);

	      auto Compare = [](const pair<int, float> &a,
				const pair<int, float> &b) {
			       return a.second > b.second;
			     };
	      
	      gtl::TopN<std::pair<int, float>, decltype(Compare)>
		topn(SHOWBEST, Compare);
	      
	      for (int i = 0; i < RADIX; i++) {
		topn.push(make_pair((int)i, output[i]));
	      }

	      std::unique_ptr<vector<pair<int, float>>> results;
	      results.reset(topn.Extract());
	      int yy = ystart;
	      for (const auto &p : *results) {
		// TODO: Color based on correctness, although
		// this needs errors or other info...
		int predcolor = 0;
		int scorecolor = 4;
		font->draw(xstart, yy,
			   StringPrintf("^1%s^%d%c ^%d%.3f",
					input.c_str(),
					predcolor,
					CharOf(p.first),
					scorecolor,
					p.second));
		yy += FONTHEIGHT;
	      }
	      
	      break;
	    }
	    case RenderStyle::RGB:
	      for (int y = 0; y < config.height[l]; y++) {
		int yy = ystart + y;
		if (yy >= SCREENH) break;

		for (int x = 0; x < config.width[l]; x++) {
		  
		  int xx = xstart + x;
		  if (x >= SCREENW) break;
		
		  int cidx =
		    y * config.width[l] * config.channels[l] +
		    x * config.channels[l];

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

	    case RenderStyle::FLAT: {
	      int scale = std::get<2>(PixelSize(config, l));
	      for (int y = 0; y < config.height[l]; y++) {
		const int yy = ystart + (y * scale);
		for (int x = 0; x < config.width[l] * config.channels[l]; x++) {
		  const int xx = xstart + (x * scale);
		  if (x >= SCREENW) break;
		  const int cidx = y * config.width[l] * config.channels[l] + x;
		  const uint8 v = FloatByte(stim.values[l][cidx]);

		  // PERF!
		  for (int sy = 0; sy < scale; sy++) {
		    for (int sx = 0; sx < scale; sx++) {
		      sdlutil::drawpixel(screen,
					 xx + sx, yy + sy, v, v, v);
		    }
		  }
		}
	      }
	      break;
	    }
	    }
	    

	    int w, h, s;
	    std::tie(w, h, s) = PixelSize(config, l);
	    int ww = w * s;
	    
	    // Now errors.
	    if (DRAW_ERRORS && l > 0) {
	      const int exstart = xstart + ww;
	      const vector<float> &errs = err.error[l - 1];
	      switch (config.style[l]) {
	      case RenderStyle::LETTERPREDICTION:
		// nothing..?
		break;
	      case RenderStyle::FLAT:
	      case RenderStyle::RGB:
		Error2D<2>(errs, exstart, ystart,
			   config.width[l] * config.channels[l],
			   config.height[l]);
	      }
	    }

	    ystart += h * s + 4;
	  }


	  if (vlayer >= 0) {
	    double tot = 0.0;
	    int yz = ystart + 4;
	    const vector<float> &vals = stim.values[vlayer];
	    for (int i = 0; i < vals.size(); i++) {
	      tot += vals[i];
	      if (i < 48) {
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
	    font->draw(xstart, yz, StringPrintf("[%d] tot: %.9f", vlayer, tot));
	  }
	}

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

static void TrainThread() {
  Timer setup_timer; 

  string dict_ascii = Util::ReadFile("../wordlist.asc");
  vector<int> dict27;
  dict27.reserve(dict_ascii.size());

  int exports_until_log = 1; // LOG_EXPORT_EVERY;
  

  // PERF: Can probably drop the last \n.
  for (char c : dict_ascii) {
    if (c == '\n') {
      dict27.push_back(26);
    } else if (c >= 'a' && c <= 'z') {
      dict27.push_back(c - 'a');
    } else {
      fprintf(stderr, "Skipped weird char %02x\n", c);
    }
  }

  fprintf(stderr, "Dict length: %lld\n", (int64)dict27.size());
  fflush(stderr);
  
  static constexpr int CL_PARALLELISM = 2;

  // Number of training examples per round of training.
  // XXX This is probably still way too low. These are very small for chess (a
  // stimulation just needs the node activation values, so it's much smaller than
  // the network itself, which has to store weights for each incoming edge).
  static constexpr int EXAMPLES_PER_ROUND = 2048; // 4096;
  static constexpr int EXAMPLE_QUEUE_TARGET = std::max(EXAMPLES_PER_ROUND * 2, 1024);
  // On a verbose round, we write a network checkpoint and maybe some
  // other stuff to disk. XXX: Do this based on time, since rounds speed can vary
  // a lot based on other parameters!
  static constexpr int VERBOSE_ROUND_EVERY = 500;

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
  Printf("Try loading network...\n");
  std::unique_ptr<Network> net{Network::ReadNetworkBinary("net.val")};
  Printf("... done\n");
  
  if (net.get() == nullptr) {
    Printf("Initializing new network...\n");
    NetworkConfiguration nc;
    Printf("New Network...\n");
    net.reset(new Network(nc.num_nodes, nc.indices_per_node, nc.transfer_functions));
    net->width = nc.width;
    net->height = nc.height;
    net->channels = nc.channels;
    Printf("Randomize weights:\n");
    RandomizeNetwork(&rc, net.get());
    Printf("Gen indices:\n");
    MakeIndices(nc.width, nc.channels, &rc, net.get());
    Printf("Invert indices:\n");
    Network::ComputeInvertedIndices(net.get());
    Network::CheckInvertedIndices(*net);

    Printf("Writing network so we don't have to do that again...\n");
    Network::SaveNetworkBinary(*net, "net.val");
  }

  Printf("Initialized network in %.1fms.\n", initialize_network_timer.MS());

  NetworkGPU net_gpu{global_cl, net.get()};

  if (SharedReadWithLock(&train_should_die_m, &train_should_die))
    return;

  Printf("Network uses %.2fKB of storage (without overhead).\n",
	 net->Bytes() / 1024.0);
  {
    Stimulation tmp(*net);
    int64 stim_bytes = tmp.Bytes();
    Printf("A stimulation is %.2fkB, so for %d examples we need %.2fMB\n",
	   stim_bytes / 1024.0, EXAMPLES_PER_ROUND,
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
      Network::SaveNetworkBinary(*net, "net.val");
    }
    return should_die;
  };

  if (ShouldDie()) return;
  
  struct TrainingExample {
    vector<float> input;
    vector<float> output;
  };
  // Training examples don't depend on the learning process, so are produced
  // in a separate thread. This mutex protects the deque (only).
  // This has got to be overkill... there's almost nothing involved in
  // preparing these examples.
 
  std::shared_mutex training_examples_m;
  // XXX could just be vector, actually?
  deque<TrainingExample> training_examples;

  // Predict the char at dict[pos].
  // pos must be at least INPUT_HISTORY, since we need to read
  // that many characters before as the input layer.
  auto PopulateExampleFromPos =
    [&dict27](int pos, TrainingExample *example) {
      example->input.resize(RADIX * INPUT_HISTORY, 0.0f);
      for (int i = 0; i < INPUT_HISTORY; i++) {
	int x = (pos - INPUT_HISTORY) + i;
	ECHECK(x >= 0 && x < dict27.size());
	int s = dict27[x];
	ECHECK((RADIX * i) + s < example->input.size());
	example->input[RADIX * i + s] = 1.0f;
      }

      example->output.resize(RADIX, 0.0f);
      ECHECK(pos >= 0 && pos < dict27.size());
      int expected_s = dict27[pos];
      ECHECK(expected_s >= 0 && expected_s < example->output.size());
      example->output[expected_s] = 1.0f;
    };

  auto MakeTrainingExamplesThread = [&dict27,
				     &training_examples_m,
				     &training_examples,
				     &PopulateExampleFromPos]() {
    Printf("Training example thread startup.\n");
    string seed = StringPrintf("make ex %lld", (int64)time(nullptr));
    ArcFour rc(seed);
    rc.Discard(2000);
    
    for (;;) {
      if (SharedReadWithLock(&train_should_die_m, &train_should_die)) {
	return;
      }
      
      training_examples_m.lock_shared();
      // Make sure we have plenty of examples so that learning doesn't stall.
      if (training_examples.size() < EXAMPLE_QUEUE_TARGET) {
	training_examples_m.unlock_shared();

	const int idx = INPUT_HISTORY +
	  RandTo(&rc, dict27.size() - INPUT_HISTORY);
	TrainingExample example;
	PopulateExampleFromPos(idx, &example);
	{
	  WriteMutexLock ml(&training_examples_m);
	  training_examples.push_back(std::move(example));
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
    
    // const float round_learning_rate =
    // std::min(0.10, std::max(0.002, 4 * exp(-0.2275 * (net->rounds / 100.0 + 1)/3.0)));
    auto Linear =
      [](double start, double end, double round_target, double input) {
	if (input < 0.0) return start;
	if (input > round_target) return end;
	double height = end - start;
	double f = input / round_target;
	return start + f * height;
      };
      const float round_learning_rate = Linear(1.0, 0.002, 500000.0, net->rounds);
      
    CHECK(!std::isnan(round_learning_rate));
    if (VERBOSE > 2) Printf("Learning rate: %.4f\n", round_learning_rate);

    const float example_learning_rate = round_learning_rate / (double)EXAMPLES_PER_ROUND;
    
    if (ShouldDie()) return;

    bool is_verbose_round = 0 == ((rounds_executed /* + 1 */) % VERBOSE_ROUND_EVERY);
    if (is_verbose_round) {
      Printf("Writing network:\n");
      net_gpu.ReadFromGPU();
      Network::SaveNetworkBinary(*net, "network-checkpoint.bin");
    }

    if (VERBOSE > 2) Printf("Export network:\n");
    ExportRound(net->rounds);
    ExportLearningRate(round_learning_rate);
    if (rounds_executed % EXPORT_EVERY == 0) {
      net_gpu.ReadFromGPU();
      ExportNetworkToVideo(*net);
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
    vector<vector<float>> expected =
      Map(examples, [](const TrainingExample &te) { return te.output; });

    setup_ms += setup_timer.MS();

    CHECK_EQ(examples.size(), expected.size());

    // TODO: may make sense to pipeline this loop somehow, so that we
    // can parallelize CPU/GPU duties?

    // Run a batch of images all the way through. (Each layer requires
    // significant setup.)
    if (VERBOSE > 2) Printf("Creating stimulations...\n");
    Timer stimulation_init_timer;

    if (VERBOSE > 2) Printf("Setting input layer of Stimulations...\n");
    // These are just memory copies; easy to do in parallel.
    CHECK_EQ(examples.size(), training.size());
    UnParallelComp(examples.size(),
		 [&examples, &training](int i) {
		   training[i]->LoadInput(examples[i].input);
		 }, CL_PARALLELISM);
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
      UnParallelComp(examples.size(),
		   [&net, rounds_executed, num_examples = examples.size(),
		    &fc, &training](int example_idx) {
		     fc.Forward(training[example_idx]);

		     if (rounds_executed % EXPORT_EVERY == 0 &&
			 example_idx < NUM_VIDEO_STIMULATIONS) {
		       // XXX this uses unintialized/stale memory btw
		       Stimulation stim{*net};
		       training[example_idx]->ExportStimulation(&stim);
		       // Copy to screen.
		       ExportStimulusToVideo(example_idx, stim);
		     }
		   }, CL_PARALLELISM);
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
      CHECK(values.size() == OUTPUT_LAYER_SIZE) << "Chess-specific check; ok to delete";
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
      double avg_error = total_error / (double)examples.size();
      ExportAvgErrorToVideo(avg_error);

      exports_until_log--;
      if (exports_until_log <= 0) {

	// Compute full loss on actual input.
	struct Acc {
	  int num = 0;
	  int exact = 0;
	  int depth_loss = 0;
	};
	auto Plus = [](Acc a, Acc b) {
		      a.num += b.num;
		      a.exact += b.exact;
		      a.depth_loss += b.depth_loss;
		      return a;
		    };

	auto AccErrors =
	  [&dict27, &net](int idx, Acc *acc) {
	    // Index of the expected output character.
	    idx += INPUT_HISTORY;
	    
	    // PERF could share this stim across threads...
	    Stimulation stim{*net};
	    // This is already initialized to zero.
	    vector<float> *input = &stim.values[0];
	    ECHECK_EQ(input->size(), RADIX * INPUT_HISTORY);
	    for (int i = 0; i < INPUT_HISTORY; i++) {
	      int x = (idx - INPUT_HISTORY) + i;
	      ECHECK(x >= 0 && x < dict27.size());
	      int s = dict27[x];
	      ECHECK((RADIX * i) + s < input->size());
	      (*input)[RADIX * i + s] = 1.0f;
	    }

	    ForwardStimulation(*net, &stim);

	    const vector<float> &output = stim.values.back();
	    ECHECK_EQ(output.size(), RADIX);

	    const int expected_s = dict27[idx];
	    const float expected_score = output[expected_s];
	    // How many items had better score?
	    int depth = 0;
	    for (int i = 0; i < RADIX; i++) {
	      if (i != expected_s && output[i] >= expected_score) {
		depth++;
	      }
	    }

	    acc->num++;
	    if (depth == 0) acc->exact++;
	    acc->depth_loss += depth;
	  };

	Timer cpu_eval;
	fprintf(stderr, "CPU eval on %lld inputs\n", (int)dict27.size());
	fflush(stderr);
	Acc total = ParallelAccumulate(dict27.size() - INPUT_HISTORY,
				       Acc{},
				       Plus,
				       AccErrors,
				       16);
	double cpu_sec = cpu_eval.MS() / 1000.0;
	fprintf(stderr, "Finished in %.1f sec (%.1f/sec)\n"
		"         %d exact (%.2f%%), %d depth loss (%.4f avg)\n",
		cpu_sec, total.num / cpu_sec,
		total.exact, total.exact / (double)total.num,
		total.depth_loss, total.depth_loss / (double)total.num);
	fflush(stderr);
	
	FILE *log = fopen("train-log.tsv", "ab");
	if (log) {
	  fprintf(log, "%lld %lld %.6f %.6f %d %d\n",
		  net->rounds, net->examples,
		  round_learning_rate, avg_error,
		  total.exact, total.depth_loss);
	  fclose(log);
	}
	exports_until_log = LOG_EXPORT_EVERY;
      }
    }
    
    const int num_examples = examples.size();
    // But, don't need to keep this allocated.
    examples.clear();

    if (ShouldDie()) return;
    if (VERBOSE > 2) Printf("Error calc.\n");
    Timer output_error_timer;
    UnParallelComp(num_examples,
		 [rounds_executed,
		  &setoutputerror, &net_gpu,
		  &training, &expected](int example_idx) {
		   if (rounds_executed % EXPORT_EVERY == 0 &&
		       example_idx < NUM_VIDEO_STIMULATIONS) {
		     // Copy to screen.
		     ExportExpectedToVideo(example_idx, expected[example_idx]);
		   }

		   // PERF could pipeline this copy earlier
		   training[example_idx]->LoadExpected(expected[example_idx]);
		   SetOutputErrorCL::Context sc{&setoutputerror, &net_gpu};
		   sc.SetOutputError(training[example_idx]);
		   /* Printf("."); */
		 }, CL_PARALLELISM);
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

      UnParallelComp(num_examples,
		   [num_examples, &training, &bc](int example) {
		     bc.Backward(training[example]);
		     /*
		     if (example % 10 == 0) {
		       Printf("[%d/%d] (%.2f%%) ", example, (int)num_examples,
			      100.0 * example / num_examples);
		     }
		     */
		   }, CL_PARALLELISM);
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
      Printf("[%d/%d] = (%.2f%%) ", layer, net->num_layers, layer * 100.0 / net->num_layers);
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
    ExportExamplesPerSec(round_eps);
    if (true || VERBOSE > 1)
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

  SDL_Surface *icon = SDL_LoadBMP("unblind-icon.bmp");
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

  {
    // XXX Maybe UIThread should be an object, then...
    // printf("Allocating video network/stimulations/data...");
    WriteMutexLock ml(&video_export_m);
    current_stimulations.resize(NUM_VIDEO_STIMULATIONS);
    current_errors.resize(NUM_VIDEO_STIMULATIONS);
    current_expected.resize(NUM_VIDEO_STIMULATIONS);
    // printf("OK.\n");
  }
  
  std::thread train_thread(&TrainThread);

  UIThread();

  Printf("Killing train thread (might need to wait for round to finish)...\n");
  SharedWriteWithLock(&train_should_die_m, &train_should_die, true);
  train_thread.join();

  Printf("Train is dead; now UI exiting.\n");

  SDL_Quit();
  return 0;
}

