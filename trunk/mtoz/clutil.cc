#include "clutil.h"

#include "timer.h"
#include <CL/cl.h>
#include "base/logging.h"

CL::CL() {
  cl_uint num_platforms;
  cl_platform_id platform = nullptr;
  CHECK(CL_SUCCESS == clGetPlatformIDs(0, nullptr, &num_platforms));
  fprintf(stderr, "CL:: Number of platforms: %d.\n", num_platforms);
  
  // Choose the first platform that has a GPU.
  int chosen_platform_id = -1;
  if (num_platforms > 0) {
    cl_platform_id *platforms =
      (cl_platform_id *)malloc(num_platforms * sizeof (cl_platform_id));
    CHECK(CL_SUCCESS == clGetPlatformIDs(num_platforms, platforms, nullptr));
    for (int i = 0; i < num_platforms; i++) {
      struct Prop {
	cl_platform_info key;
	size_t size;
	char *value;
      };
      Prop props[] = {
	{CL_PLATFORM_PROFILE, 0, nullptr},
	{CL_PLATFORM_VERSION, 0, nullptr},
	{CL_PLATFORM_NAME, 0, nullptr},
	{CL_PLATFORM_VENDOR, 0, nullptr},
	{CL_PLATFORM_EXTENSIONS, 0, nullptr},
      };
      for (Prop &prop : props) {
	clGetPlatformInfo(
	    platforms[i], prop.key, 0, nullptr, &prop.size);
	prop.value = (char *)malloc(prop.size + 1);
	clGetPlatformInfo(
	    platforms[i], prop.key, prop.size, prop.value, nullptr);
      }

      fprintf(stderr,
	      "% 4d. %s (%s):\n"
	      "      %s; %s\n", 
	      i, props[2].value, props[0].value,
	      props[1].value, props[3].value, props[4].value);

      cl_uint platform_devices = 0;
      if (CL_SUCCESS == clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU,
				       0, nullptr, &platform_devices)) {
	fprintf(stderr,
		"      Number of GPUs: %d\n", (int)platform_devices);
	if (chosen_platform_id == -1)
	  chosen_platform_id = i;
      } else {
	fprintf(stderr,
		"      NO GPU devices.\n");
      }

      for (Prop &prop : props) free(prop.value);
    }

    if (chosen_platform_id == -1) {
      fprintf(stderr, "There were no platforms with GPUs. Aborting.\n");
      abort();
    }

    fprintf(stderr, "Using platform %d.\n", chosen_platform_id);
    platform = platforms[chosen_platform_id];
    free(platforms);
  }

  // Get the GPU device.
  CHECK(CL_SUCCESS == clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU,
				     0, nullptr, &num_devices));
  CHECK(num_devices > 0) << "Platform should only be selected if it "
    "reports having devices??";

  devices = (cl_device_id *)malloc(num_devices * sizeof (cl_device_id));
  CHECK(CL_SUCCESS == clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU,
				     num_devices, devices, nullptr));

  context = clCreateContext(nullptr, 1, devices, nullptr, nullptr, nullptr);

  
  const bool out_of_order = true;
  // TODO: Try device queue?
  const cl_queue_properties props =
    out_of_order ? CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE : 0;
  queue = clCreateCommandQueueWithProperties(
      context, devices[0],
      &props,
      nullptr);
}


pair<cl_program, cl_kernel> CL::BuildOneKernel(const string &kernel_src,
					       const string &function_name) {
  Timer gpu_compile;
  const char *sources[] = { kernel_src.c_str() };
  size_t source_size[] = { kernel_src.size() };
  cl_program program = clCreateProgramWithSource(context, 1, sources, source_size, nullptr);
  if (CL_SUCCESS != clBuildProgram(program, 1, devices, nullptr, nullptr, nullptr)) {
    size_t blsize;

    CHECK(CL_SUCCESS == clGetProgramBuildInfo(program, devices[0], 
					      CL_PROGRAM_BUILD_LOG, 0, nullptr, &blsize));
    char *build_log = (char *)malloc(blsize + 1);
    CHECK(CL_SUCCESS == clGetProgramBuildInfo(program, devices[0], 
					      CL_PROGRAM_BUILD_LOG, blsize, build_log, nullptr));
    build_log[blsize] = 0;
    printf("Failed to compile:\n %s", build_log);
    exit(-1);
  }

  cl_kernel kernel = clCreateKernel(program, function_name.c_str(), nullptr);
  fprintf(stderr, "Compiled %s in %.1fms.\n", function_name.c_str(), gpu_compile.MS());
  return make_pair(program, kernel);
}


// static
const char *CL::ErrorString(cl_int err) {
  switch(err) {
#define MAKE_ERROR(e) case e: return #e ;
  MAKE_ERROR(CL_SUCCESS);
  MAKE_ERROR(CL_DEVICE_NOT_FOUND);
  MAKE_ERROR(CL_DEVICE_NOT_AVAILABLE);
  MAKE_ERROR(CL_COMPILER_NOT_AVAILABLE);
  MAKE_ERROR(CL_MEM_OBJECT_ALLOCATION_FAILURE);
  MAKE_ERROR(CL_OUT_OF_RESOURCES);
  MAKE_ERROR(CL_OUT_OF_HOST_MEMORY);
  MAKE_ERROR(CL_PROFILING_INFO_NOT_AVAILABLE);
  MAKE_ERROR(CL_MEM_COPY_OVERLAP);
  MAKE_ERROR(CL_IMAGE_FORMAT_MISMATCH);
  MAKE_ERROR(CL_IMAGE_FORMAT_NOT_SUPPORTED);
  MAKE_ERROR(CL_BUILD_PROGRAM_FAILURE);
  MAKE_ERROR(CL_MAP_FAILURE);
  MAKE_ERROR(CL_MISALIGNED_SUB_BUFFER_OFFSET);
  MAKE_ERROR(CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST);
  MAKE_ERROR(CL_COMPILE_PROGRAM_FAILURE);
  MAKE_ERROR(CL_LINKER_NOT_AVAILABLE);
  MAKE_ERROR(CL_LINK_PROGRAM_FAILURE);
  MAKE_ERROR(CL_DEVICE_PARTITION_FAILED);
  MAKE_ERROR(CL_KERNEL_ARG_INFO_NOT_AVAILABLE);
  MAKE_ERROR(CL_INVALID_VALUE);
  MAKE_ERROR(CL_INVALID_DEVICE_TYPE);
  MAKE_ERROR(CL_INVALID_PLATFORM);
  MAKE_ERROR(CL_INVALID_DEVICE);
  MAKE_ERROR(CL_INVALID_CONTEXT);
  MAKE_ERROR(CL_INVALID_QUEUE_PROPERTIES);
  MAKE_ERROR(CL_INVALID_COMMAND_QUEUE);
  MAKE_ERROR(CL_INVALID_HOST_PTR);
  MAKE_ERROR(CL_INVALID_MEM_OBJECT);
  MAKE_ERROR(CL_INVALID_IMAGE_FORMAT_DESCRIPTOR);
  MAKE_ERROR(CL_INVALID_IMAGE_SIZE);
  MAKE_ERROR(CL_INVALID_SAMPLER);
  MAKE_ERROR(CL_INVALID_BINARY);
  MAKE_ERROR(CL_INVALID_BUILD_OPTIONS);
  MAKE_ERROR(CL_INVALID_PROGRAM);
  MAKE_ERROR(CL_INVALID_PROGRAM_EXECUTABLE);
  MAKE_ERROR(CL_INVALID_KERNEL_NAME);
  MAKE_ERROR(CL_INVALID_KERNEL_DEFINITION);
  MAKE_ERROR(CL_INVALID_KERNEL);
  MAKE_ERROR(CL_INVALID_ARG_INDEX);
  MAKE_ERROR(CL_INVALID_ARG_VALUE);
  MAKE_ERROR(CL_INVALID_ARG_SIZE);
  MAKE_ERROR(CL_INVALID_KERNEL_ARGS);
  MAKE_ERROR(CL_INVALID_WORK_DIMENSION);
  MAKE_ERROR(CL_INVALID_WORK_GROUP_SIZE);
  MAKE_ERROR(CL_INVALID_WORK_ITEM_SIZE);
  MAKE_ERROR(CL_INVALID_GLOBAL_OFFSET);
  MAKE_ERROR(CL_INVALID_EVENT_WAIT_LIST);
  MAKE_ERROR(CL_INVALID_EVENT);
  MAKE_ERROR(CL_INVALID_OPERATION);
  MAKE_ERROR(CL_INVALID_GL_OBJECT);
  MAKE_ERROR(CL_INVALID_BUFFER_SIZE);
  MAKE_ERROR(CL_INVALID_MIP_LEVEL);
  MAKE_ERROR(CL_INVALID_GLOBAL_WORK_SIZE);
  MAKE_ERROR(CL_INVALID_PROPERTY);
  MAKE_ERROR(CL_INVALID_IMAGE_DESCRIPTOR);
  MAKE_ERROR(CL_INVALID_COMPILER_OPTIONS);
  MAKE_ERROR(CL_INVALID_LINKER_OPTIONS);
  MAKE_ERROR(CL_INVALID_DEVICE_PARTITION_COUNT);
  default: return "??";
  }
}
