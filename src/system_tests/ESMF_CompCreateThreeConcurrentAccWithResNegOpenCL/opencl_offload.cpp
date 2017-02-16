/*
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!-------------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#define MAX_PLATFORMS 5
#define MAX_DEVICES 100

/* FIXME: We might want to use ESMF error codes here - eventually */
#define SUCCESS 0
#define FAIL -1

extern "C"{
	int OpenCLScaleVec(int vdeviceid);
}

/* Kernel source  - divides all numbers by 10.0 */
const char *DivideByTenGPUSource = "\n" \
	"__kernel void dividebyten(                                             \n" \
	"   __global float* input,                                              \n" \
	"   __global float* output,                                             \n" \
	"   const unsigned int count)                                           \n" \
	"{                                                                      \n" \
	"   int i = get_global_id(0);                                           \n" \
	"   if(i < count)                                                       \n" \
	"       output[i] = input[i] / 10.0 ;									\n" \
	"}                                                                      \n" \
	"\n";

void init_input(float *input_array, size_t sz)
{
	for(int i=0; i<sz; i++){
		input_array[i] = 10.0;
	}
}

void validate_result(float *output_array, size_t sz){
	for(int i=0; i<sz; i++){
		if(output_array[i] != 1.0){
			printf("Result Error at %d\n", i);
			return;
		}
	}
	printf("Result Verified - PASS\n");
}

const int MAX_DATA_SZ = 100;
int offload_and_run(cl_platform_id *platforms, cl_uint platformCount, int vdeviceid)
{
    cl_int ret;
	int i;
    //cl_device_id device_id;
    cl_device_id devices[MAX_DEVICES];
    cl_uint deviceCount = 0;
    cl_context context;
    cl_command_queue commands;
    cl_program program;
    cl_kernel kernel;

    cl_mem input;
    cl_mem output;
	float input_array[MAX_DATA_SZ];
	float output_array[MAX_DATA_SZ];

	size_t global_sz, local_sz;

	// Initialize input array
	init_input(input_array, MAX_DATA_SZ);
	
    bool gpu_found = false;

    for (i = 0; i < platformCount; i++) {
		ret = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, MAX_DEVICES, devices, &deviceCount);
		if(ret == CL_SUCCESS){
            gpu_found = true;
            break;
        }
        else{
			printf("WARNING: Cannot find a GPU on platform %d (ret = %d)\n", i, ret);
        }
    }
    if(gpu_found == false){
        printf("WARNING: Could not find a GPU, retrying with finding a CPU instead\n");
        bool cpu_found = false;
        for (i = 0; i < platformCount; i++) {
            ret = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_CPU, MAX_DEVICES, devices, &deviceCount);
            if(ret == CL_SUCCESS){
                cpu_found = true;
                break;
            }
            else{
                printf("WARNING: Cannot find a CPU on platform %d (ret = %d)\n", i, ret);
            }
        }
        if(cpu_found == false){
            printf("Fatal Error: Could not find a GPU or a CPU to run kernel code-- Aborting\n");
            return FAIL;
        }
    }

  if(vdeviceid > deviceCount){
      printf("Fatal Error: The requested virtual device id (%d) is greater than number of devices (%d) available in the platform\n", vdeviceid, deviceCount);
      return FAIL;
  }

	// Choose the virtual device requested to run the kernel
	cl_device_id device_id = devices[vdeviceid];

	// Find the vendor device id
	// FIXME: Need to find a way to uniquely identify a device
	// device vendor id is an id for the vendor not the device
	/*
	cl_uint vendor_device_id;
	ret = clGetDeviceInfo(device_id, CL_DEVICE_VENDOR_ID, sizeof(vendor_device_id),
				&vendor_device_id, NULL);
	if(ret != CL_SUCCESS){
		printf("WARNING: Unable to get vendor device id for compute device\n");
	}
	else{
		printf("Choosing device %llu (vendor device id) as the compute device\n", (unsigned long long) vendor_device_id);
	}
	*/
	// Create a compute context on the first device (GPU or CPU) found
	context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &ret);
	if(!context){
		printf("Error creating context for the first compute device (ret = %d)\n", ret);
		return FAIL;
	}

	// Create a command queue for the device
	commands = clCreateCommandQueue(context, device_id, 0, &ret);
	if(!commands){
		printf("Error creating a command queue for the chosen device (ret = %d)\n", ret);
		clReleaseContext(context);
		return FAIL;
	}

	// Create the kernel program from source buffer
	program = clCreateProgramWithSource(context, 1,
				(const char **)&DivideByTenGPUSource, NULL, &ret);
	if(!program){
		printf("Error creating compute program from source (ret = %d)\n", ret);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}	

	// Build compute program executable
	ret = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
	if(ret != CL_SUCCESS){
		printf("Error building kernel source (ret = %d)\n", ret);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}

	// Create the compute kernel from the built source
	kernel = clCreateKernel(program, "dividebyten", &ret);
	if(ret != CL_SUCCESS){
		printf("Error creating kernel from the built source (ret = %d)\n", ret);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}

	// Create input/output buffers in device
	input = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float) * MAX_DATA_SZ,
				NULL, &ret);
	output = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(float) * MAX_DATA_SZ,
				NULL, &ret);
	if(!input || !output){
		printf("Error creating input/output buffers on device (ret = %d)\n", ret);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}

	// Write our data - input_array to device memory (input)
	ret = clEnqueueWriteBuffer(commands, input, CL_TRUE, 0,
			sizeof(float) * MAX_DATA_SZ, input_array, 0, NULL, NULL);
	if(ret != CL_SUCCESS){
		printf("Error writing input array to device memory (ret = %d)\n", ret);
		clReleaseMemObject(input);
		clReleaseMemObject(output);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}

	// Set the arguments for the compute kernel
	int count = MAX_DATA_SZ;
	ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), &input);
	if(ret == CL_SUCCESS){
		ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), &output);
		if(ret == CL_SUCCESS){
			ret = clSetKernelArg(kernel, 2, sizeof(unsigned int), &count);
		}
	}
	if(ret != CL_SUCCESS){
		printf("Error setting args for compute kernel (ret = %d)\n", ret);
		clReleaseMemObject(input);
		clReleaseMemObject(output);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}

	// Get max work group size for the device
	ret = clGetKernelWorkGroupInfo(kernel, device_id, CL_KERNEL_WORK_GROUP_SIZE,
			sizeof(local_sz), &local_sz, NULL);
	if(ret != CL_SUCCESS){
		printf("Error getting work group size for device (ret = %d)\n", ret);
		clReleaseMemObject(input);
		clReleaseMemObject(output);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}
			
	global_sz = count; // MAX_DATA_SZ
	if(local_sz > global_sz){
		local_sz = global_sz;
	}
	// Enqueue kernel
	ret = clEnqueueNDRangeKernel(commands, kernel, 1, NULL, &global_sz, &local_sz,
			0, NULL, NULL);
	if(ret != CL_SUCCESS){
		printf("Error enqueing kernel (ret = %d)\n", ret);
		clReleaseMemObject(input);
		clReleaseMemObject(output);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}

	printf("Enqueued compute kernel to device, waiting for kernel to finish\n");
	// Wait for the commands to finish
	clFinish(commands);
	printf("Finished executing Kernel\n");

	// Read the result into output_array
	ret = clEnqueueReadBuffer(commands, output, CL_TRUE, 0, sizeof(float) * MAX_DATA_SZ,
			output_array, 0, NULL, NULL);
	if(ret != CL_SUCCESS){
		printf("Error reading result from device (ret = %d)\n", ret);
		clReleaseMemObject(input);
		clReleaseMemObject(output);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(commands);
		clReleaseContext(context);
		return FAIL;
	}

	printf("Finished reading result from device, now validating result...\n");
	// Validate result
	validate_result(output_array, MAX_DATA_SZ);
	
	// Free all OpenCL handles and stuff
	clReleaseMemObject(input);
	clReleaseMemObject(output);
	clReleaseKernel(kernel);
	clReleaseProgram(program);
	clReleaseCommandQueue(commands);
	clReleaseContext(context);
	return SUCCESS;
}

int OpenCLScaleVec(int vdeviceid)
{
    int i, j, retval;
    cl_int ret;
    cl_uint platformCount;
    cl_platform_id *platforms=NULL;

    // get platform count
    ret = clGetPlatformIDs(MAX_PLATFORMS, NULL, &platformCount);
    if(ret != CL_SUCCESS){
        printf("Error trying to get platform ID count\n");
        exit(FAIL);
    }

    if(platformCount == 0){
        printf("Cannot find any supported platforms, Exiting...\n");
		return(FAIL);
    }

    // get all platforms
    platforms = (cl_platform_id*) malloc(sizeof(cl_platform_id) * platformCount);
    if(platforms == NULL){
        printf("Malloc failed !\n");
        return(FAIL);
    }

    ret = clGetPlatformIDs(platformCount, platforms, NULL);
    if(ret != CL_SUCCESS){
        printf("Error trying to get platform IDs\n");
		free(platforms);
        return(FAIL);
    }

	retval = offload_and_run(platforms, platformCount, vdeviceid);
	if(retval != SUCCESS){
		printf("Error offloading and running kernel on device\n");
		free(platforms);
		return(FAIL);
	}

    free(platforms);
	return SUCCESS;
}
