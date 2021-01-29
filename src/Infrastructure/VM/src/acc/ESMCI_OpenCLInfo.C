// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#define MAX_PLATFORMS 10
#define MAX_DEVICES 100
#define VERBOSITY 0

static int get_num_devices(cl_platform_id *platforms, cl_uint platformCount)
{
  int i, j;
  cl_int ret;
  char* info=NULL;
  size_t infoSize;

  // for each platform print all device attributes
  for (i = 0; i < platformCount; i++) {
    cl_device_id devices[MAX_DEVICES];
    cl_uint deviceCount = 0;
    ret = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, MAX_DEVICES, devices, &deviceCount);
    if(ret != CL_SUCCESS){
#if (VERBOSITY > 9)
      printf("WARNING: Error trying to find GPU devices on platform %d, skipping...\n", i);
#endif
    }
    else{
      if(deviceCount == 0){
#if (VERBOSITY > 9)
        printf("Could not find any supported GPU devices on platform %d, skipping...\n", i);
#endif
      }
      else{
        /* FIXME: We return number of devices from the 1st platform available.
         * - is this the expected behavior?
         */
        return (static_cast<int>(deviceCount));
      }
    }
    ret = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ACCELERATOR, MAX_DEVICES, devices, &deviceCount);
    if(ret != CL_SUCCESS){
#if (VERBOSITY > 9)
      printf("WARNING: Error trying to find MIC devices on platform %d, skipping...\n", i);
#endif
    }
    else{
      if(deviceCount == 0){
#if (VERBOSITY > 9)
        printf("Could not find any supported MIC devices on platform %d, skipping...\n", i);
#endif
      }
      else{
        return (static_cast<int>(deviceCount));
      }
    }
  }
  return 0;
}

namespace ESMCI{

int VMAccFwGetNumDevices(void )
{
  int i, j;
  cl_int ret;
  cl_uint platformCount;
  cl_platform_id *platforms=NULL;
  int ndevices = 0;

  // get platform count
  ret = clGetPlatformIDs(MAX_PLATFORMS, NULL, &platformCount);
  if(ret != CL_SUCCESS){
    fprintf(stderr, "(OpenCL Framework): Error trying to get Platform ID count (ret=%d)\n", ret);
    return(-1);
  }

  if(platformCount == 0){
#if (VERBOSITY > 9)
    printf("Cannot find any supported platforms...\n");
#endif
    return(0);
  }

  // get all platforms
  platforms = (cl_platform_id*) malloc(sizeof(cl_platform_id) * platformCount);
  if(platforms == NULL){
    fprintf(stderr, "(OpenCL Framework): Error allocating memory for platforms, malloc failed!\n");
    return(-1);
  }

  ret = clGetPlatformIDs(platformCount, platforms, NULL);
  if(ret != CL_SUCCESS){
    fprintf(stderr, "(OpenCL Framework): Error trying to get Platform IDs (ret=%d)\n", ret);
    return(-1);
  }

  ndevices = get_num_devices(platforms, platformCount);

#if (VERBOSITY > 9)
  printf("Found [%d] accelerator devices\n",ndevices);
#endif

  free(platforms);
  return ndevices;
}

}
