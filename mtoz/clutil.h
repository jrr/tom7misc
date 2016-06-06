
#ifndef __CLUTIL_H
#define __CLUTIL_H

#include <CL/cl.h>
#include <string>
#include <vector>

using namespace std;

using uint8 = uint8_t;
// Better compatibility with CL.
using uchar = uint8_t;

using uint64 = uint64_t;

#define CHECK_SUCCESS(e) \
  do { int ret = (e); if (ret != CL_SUCCESS) { \
      fprintf(stderr, "Not successful with code %d (%s).\n", ret, CL::ErrorString(ret)); \
  abort(); } } while (0)

// Boilerplate. There should probably just be one of these per program.
struct CL {
  CL();

  static const char *ErrorString(cl_int err);
  
  cl_command_queue NewCommandQueue(bool out_of_order = true) {
    return clCreateCommandQueue(context, devices[0], 
				out_of_order ? CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE : 0,
				nullptr);
  }

  pair<cl_program, cl_kernel> BuildOneKernel(const string &kernel_src,
					     const string &function_name);

  ~CL() {
    printf("Destroying CL.\n");
    CHECK_SUCCESS(clReleaseCommandQueue(queue));
    CHECK_SUCCESS(clReleaseContext(context));
    free(devices);
    printf("OK.\n");
  }

  cl_uint num_devices = 0;
  cl_device_id *devices = nullptr;
  cl_context context;
  cl_command_queue queue;
};

// Shares with the host memory and we don't control when it gets copied. This is
// quite inefficient.
template<class T>
static cl_mem BufferFromVector(cl_context context, bool readonly, vector<T> *v) {
  return clCreateBuffer(context, 
			(readonly ? CL_MEM_READ_ONLY : 0) | 
			CL_MEM_USE_HOST_PTR,
			sizeof (T) * v->size(),
			(void *) v->data(),
			nullptr);
}

// Creates a new buffer on the GPU and copies the memory there. They do not alias.
// Note that the command queue is not flushed, so you should not touch the source
// memory until it is.
template<class T>
static cl_mem MoveMemoryToGPU(cl_context context, cl_command_queue cmd,
			      bool readonly, vector<T> *v) {
  cl_mem buf = clCreateBuffer(context, 
			      (readonly ? CL_MEM_READ_ONLY : 0),
			      sizeof (T) * v->size(),
			      nullptr,
			      nullptr);
  clEnqueueWriteBuffer(cmd, buf, CL_TRUE, 0, 
		       sizeof (T) * v->size(), v->data(), 0, nullptr, nullptr);
  return buf;
}

// Same, but with a constant vector. Implies read-only.
template<class T>
static cl_mem MoveMemoryToGPUConst(cl_context context, cl_command_queue cmd,
				   const vector<T> &v) {
  cl_mem buf = clCreateBuffer(context, 
			      CL_MEM_READ_ONLY,
			      sizeof (T) * v.size(),
			      nullptr,
			      nullptr);
  clEnqueueWriteBuffer(cmd, buf, CL_TRUE, 0, 
		       sizeof (T) * v.size(), v.data(), 0, nullptr, nullptr);
  return buf;
}

template<class T>
static cl_mem CreateUninitializedGPUMemory(cl_context context, int n) {
  return clCreateBuffer(context, 0, sizeof (T) * n, nullptr, nullptr);
}

template<class T>
static vector<T> CopyBufferFromGPU(cl_command_queue cmd, cl_mem buf, int n) {
  vector<T> vec;
  vec.resize(n);
  CHECK_SUCCESS(clEnqueueReadBuffer(cmd, buf, CL_TRUE, 0, sizeof (T) * n,
				    vec.data(),
				    // No wait-list or event.
				    0, nullptr,
				    nullptr));
  clFinish(cmd);
  return vec;
}

// Assumes the vector already has the correct size.
template<class T>
static void CopyBufferFromGPUTo(cl_command_queue cmd, cl_mem buf, vector<T> *vec) {
  CHECK_SUCCESS(clEnqueueReadBuffer(cmd, buf, CL_TRUE, 0, sizeof (T) * vec->size(),
				    vec->data(),
				    // No wait-list or event.
				    0, nullptr,
				    nullptr));
  clFinish(cmd);
}

#endif
