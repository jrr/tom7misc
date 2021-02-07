
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

#include "threadutil.h"
#include "randutil.h"
#include "image.h"

#include "clutil.h"
#include "timer.h"

using namespace std;
using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;
using uint32 = uint32_t;
using uint64 = uint64_t;

// Thread-safe, so shared between train and ui threads.
static CL *global_cl = nullptr;

std::shared_mutex print_mutex;
#define Printf(fmt, ...) do {                           \
    WriteMutexLock Printf_ml(&print_mutex);             \
    printf(fmt, ##__VA_ARGS__);                         \
    fflush(stdout);                                     \
  } while (0);

template<class C>
static void DeleteElements(C *cont) {
  for (auto &elt : *cont) {
    delete elt;
  }
  cont->clear();
}

int main(int argc, char **argv) {

  // Test inline assembly of PTX instructions.
  // This kernel is from the NVIDIA sample SDK.
  // %%laneid is a special register, giving the lane id within the current warp;
  // this is a CUDA-specific concept.
  // docs.nvidia.com/cuda/parallel-thread-execution/index.html#instruction-statements
  
  const string kernel_src = R"(
__kernel void GetLaneId(__global int *buffer, int length)
{
    int idx = get_global_id(0);
    if (idx < length)
    {
        unsigned int laneid;
        asm("mov.u32 %0, %%laneid;" : "=r"(laneid));
        buffer[idx] = laneid;
    }
}
)";

  global_cl = new CL;

  static constexpr int WIDTH = 1920;
  static constexpr int HEIGHT = 1080;

  vector<uint32> vec(WIDTH * HEIGHT, 0u);
    
  cl_mem buffer =
    MoveMemoryToGPU(global_cl->context, global_cl->queue, false, &vec);
  clFinish(global_cl->queue);

  auto [program, kernel] = 
    global_cl->BuildOneKernel(kernel_src, "GetLaneId");

  cl_int length = WIDTH * HEIGHT;

  CHECK_SUCCESS(clSetKernelArg(kernel, 0, sizeof (cl_mem),
                               (void *)&buffer));
  CHECK_SUCCESS(clSetKernelArg(kernel, 1, sizeof (cl_int),
                               (void *)&length));

  size_t global_work_offset[] = { 0 };
  size_t global_work_size[] = { (size_t)(WIDTH * HEIGHT) };

  CHECK_SUCCESS(clEnqueueNDRangeKernel(global_cl->queue, kernel,
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
  clFinish(global_cl->queue);

  CopyBufferFromGPUTo(global_cl->queue, buffer, &vec);
  clFinish(global_cl->queue);
  
  ImageRGBA img(WIDTH, HEIGHT);
  for (int y = 0; y < HEIGHT; y++) {
    for (int x = 0; x < WIDTH; x++) {
      int idx = y * WIDTH + x;
      uint32 lane = vec[idx];
      // These are expected to be small ints, to shift them
      // out of the alpha component and into brighter regions.
      img.SetPixel32(x, y, (lane << 10) | 0xFF);
    }
  }
  img.Save("ptxtest.png");
  
  clReleaseKernel(kernel);
  clReleaseProgram(program);

  return 0;
}
