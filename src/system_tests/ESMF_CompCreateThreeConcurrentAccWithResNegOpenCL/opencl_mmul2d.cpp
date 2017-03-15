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
#include <assert.h>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include "ser_mmul2d.h"

#define MAX_PLATFORMS 5
#define MAX_DEVICES 100

/* FIXME: We might want to use ESMF error codes here - eventually */
#define SUCCESS 0
#define FAIL -1

extern "C"{
    int opencl_mmul2d(int vdeviceid,
                        float *a, float*b, float *c,
                        size_t m, size_t k, size_t n);
}

/* Kernel source  - divides all numbers by 10.0 */
const char *mmul2d_src = "\n" \
	"__kernel void mmul2d(                                          \n" \
	"   const __global float* a,                                    \n" \
	"   const __global float* b,                                    \n" \
	"   __global float* c,                                          \n" \
	"   const size_t m,                                              \n" \
	"   const size_t k,                                              \n" \
	"   const size_t n)                                              \n" \
	"{                                                               \n" \
	"   int i = get_global_id(0);                                    \n" \
	"   int j = get_global_id(1);                                    \n" \
	"   float asum = 0.0;                                            \n" \
	"   for(int l=0; l<k; l++){                                      \n" \
	"       asum += a[m*l+i] + b[k*j+l];                             \n" \
	"   }                                                            \n" \
	"   c[m*j+i] = asum;                                             \n" \
	"}                                                               \n" \
	"\n";

void init_input(float *input_array, size_t sz)
{
	for(int i=0; i<sz; i++){
		input_array[i] = 10.0;
	}
}

void validate_result(float *a, float *b, float *c, size_t m, size_t k, size_t n){
    float sc[m * n];
    int i, j, l;
    int ret;
    
    ret = smmul2d(a, b, sc, m, k, n);
    if(ret != 0){
        printf("Error running serial version of algo\n");
        return;
    }
    
    printf("\n");
    for(int i=0; i<m*n; i++){
        //printf(" %.6f, ", sc[i]);
        if(sc[i] != c[i]){
            printf("\nError while validating result at idx=%d, val = %.6f, exp_val = %.6f\n",
                    i, c[i], sc[i]);
            return;
        }
    }
    printf("\n");
    printf("Result Verified - PASS\n");
}

const int THREAD_BLOCK_SZ=32;
int offload_and_run(cl_platform_id *platforms, cl_uint platformCount, int vdeviceid,
        float *a, float *b, float *c, size_t m, size_t k, size_t n)
{
    cl_int ret;
	int i;
    //cl_device_id device_id;
    cl_device_id devices[MAX_DEVICES];
    cl_uint deviceCount = 0;
    cl_context context;
    cl_command_queue cmdq;
    cl_program program;
    cl_kernel kernel;

    cl_mem wbuf_a, wbuf_b, wbuf_c;
    size_t a_sz = m * k;
    size_t b_sz = k * n;
    size_t c_sz = m * n;

    assert(a && b && c);
    assert((m > 0) && (k > 0) && (n > 0));

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
	cmdq = clCreateCommandQueue(context, device_id, 0, &ret);
	if(!cmdq){
		printf("Error creating a command queue for the chosen device (ret = %d)\n", ret);
		clReleaseContext(context);
		return FAIL;
	}

	// Create the kernel program from source buffer
	program = clCreateProgramWithSource(context, 1,
				(const char **)&mmul2d_src, NULL, &ret);
	if(!program){
		printf("Error creating compute program from source (ret = %d)\n", ret);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}	

	// Build compute program executable
	ret = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
	if(ret != CL_SUCCESS){
		printf("Error building kernel source (ret = %d)\n", ret);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

	// Create the compute kernel from the built source
	kernel = clCreateKernel(program, "mmul2d", &ret);
	if(ret != CL_SUCCESS){
		printf("Error creating kernel from the built source (ret = %d)\n", ret);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

	// Create input/output buffers in device
	wbuf_a = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float) * a_sz,
				NULL, &ret);
	wbuf_b = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float) * b_sz,
				NULL, &ret);
	wbuf_c = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(float) * c_sz,
				NULL, &ret);
	if(!wbuf_a || !wbuf_b || !wbuf_c){
		printf("Error creating input/output buffers on device (ret = %d)\n", ret);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

	// Write our data - input_array to device memory (input)
	ret = clEnqueueWriteBuffer(cmdq, wbuf_a, CL_TRUE, 0,
			sizeof(float) * a_sz, a, 0, NULL, NULL);
    if(ret == CL_SUCCESS){
        ret = clEnqueueWriteBuffer(cmdq, wbuf_b, CL_TRUE, 0,
                sizeof(float) * b_sz, b, 0, NULL, NULL);
        if(ret == CL_SUCCESS){
            ret = clEnqueueWriteBuffer(cmdq, wbuf_c, CL_TRUE, 0,
                    sizeof(float) * c_sz, c, 0, NULL, NULL);
        }
    }
	if(ret != CL_SUCCESS){
		printf("Error writing input/output arrays to device memory (ret = %d)\n", ret);
		clReleaseMemObject(wbuf_a);
		clReleaseMemObject(wbuf_b);
		clReleaseMemObject(wbuf_c);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

	// Set the arguments for the compute kernel
	ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), &wbuf_a);
	if(ret == CL_SUCCESS){
		ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), &wbuf_b);
		if(ret == CL_SUCCESS){
            ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), &wbuf_c);
            if(ret == CL_SUCCESS){
                ret = clSetKernelArg(kernel, 3, sizeof(size_t), &m);
                if(ret == CL_SUCCESS){
                    ret = clSetKernelArg(kernel, 4, sizeof(size_t), &k);
                    if(ret == CL_SUCCESS){
                        ret = clSetKernelArg(kernel, 5, sizeof(size_t), &n);
                    }
                }
            }
		}
	}
	if(ret != CL_SUCCESS){
		printf("Error setting args for compute kernel (ret = %d)\n", ret);
		clReleaseMemObject(wbuf_a);
		clReleaseMemObject(wbuf_b);
		clReleaseMemObject(wbuf_c);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

    size_t global_sz[] = {m, n};
    size_t local_sz[] = {THREAD_BLOCK_SZ, THREAD_BLOCK_SZ};
    size_t max_work_group_sz = 0;
    
	// Get max work group size for the device
	ret = clGetKernelWorkGroupInfo(kernel, device_id, CL_KERNEL_WORK_GROUP_SIZE,
			sizeof(max_work_group_sz), &max_work_group_sz, NULL);
	if(ret != CL_SUCCESS){
		printf("Error getting work group size for device (ret = %d)\n", ret);
		clReleaseMemObject(wbuf_a);
		clReleaseMemObject(wbuf_b);
		clReleaseMemObject(wbuf_c);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

    if(max_work_group_sz < THREAD_BLOCK_SZ * THREAD_BLOCK_SZ){
        local_sz[0] = max_work_group_sz/2;
        local_sz[1] = max_work_group_sz/2;
        printf("WARNING: Resetting local_sz to %ld x %ld\n",local_sz[0], local_sz[1]);
    }
			
	// Enqueue kernel
	ret = clEnqueueNDRangeKernel(cmdq, kernel, 2, NULL, global_sz, local_sz,
			0, NULL, NULL);
	if(ret != CL_SUCCESS){
		printf("Error enqueing kernel (ret = %d)\n", ret);
		clReleaseMemObject(wbuf_a);
		clReleaseMemObject(wbuf_b);
		clReleaseMemObject(wbuf_c);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

	printf("Enqueued compute kernel to device, waiting for kernel to finish\n");
	// Wait for the cmdq to finish
	clFinish(cmdq);
	printf("Finished executing Kernel\n");

	// Read the result into output_array
	ret = clEnqueueReadBuffer(cmdq, wbuf_c, CL_TRUE, 0, sizeof(float) * c_sz,
			c, 0, NULL, NULL);
	if(ret != CL_SUCCESS){
		printf("Error reading result from device (ret = %d)\n", ret);
		clReleaseMemObject(wbuf_a);
		clReleaseMemObject(wbuf_b);
		clReleaseMemObject(wbuf_c);
		clReleaseKernel(kernel);
		clReleaseKernel(kernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(cmdq);
		clReleaseContext(context);
		return FAIL;
	}

	printf("Finished reading result from device, now validating result...\n");
	// Validate result
	//validate_result(output_array, MAX_DATA_SZ);
	validate_result(a, b, c, m, k, n);
	
	// Free all OpenCL handles and stuff
  clReleaseMemObject(wbuf_a);
  clReleaseMemObject(wbuf_b);
  clReleaseMemObject(wbuf_c);
	clReleaseKernel(kernel);
	clReleaseProgram(program);
	clReleaseCommandQueue(cmdq);
	clReleaseContext(context);
	return SUCCESS;
}

int opencl_mmul2d(int vdeviceid,
                    float *a, float *b, float *c,
                    size_t m, size_t k, size_t n)
{
    int i, j, retval;
    cl_int ret;
    cl_uint platformCount;
    cl_platform_id *platforms=NULL;

    if((a == NULL) || (b == NULL) || (c == NULL)){
        printf("Invalid arrays (NULL pointers) passed as args\n");
        return FAIL;
    }

    if((m <= 0) || (k <= 0) || (n <= 0)){
        printf("Invalid array dims (<= 0)\n");
        return FAIL;
    }

    if( (m % THREAD_BLOCK_SZ != 0) ||
        (k % THREAD_BLOCK_SZ != 0) ||
        (n % THREAD_BLOCK_SZ != 0) ){
        printf("Array dims need to be multiples of %d\n", THREAD_BLOCK_SZ);
        printf("m = %ld, k = %ld, n = %ld\n", m, k, n);
        return FAIL;
    }

    printf("a[0] = %.10f, b[0] = %0.10f\n", a[0], b[0]);
    printf("a[1] = %.6f, b[1] = %0.6f\n", a[1], b[1]);

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

	retval = offload_and_run(platforms, platformCount, vdeviceid,
                            a, b, c, m, k, n);
	if(retval != SUCCESS){
		printf("Error offloading and running kernel on device\n");
		free(platforms);
		return(FAIL);
	}

    free(platforms);
	return SUCCESS;
}
