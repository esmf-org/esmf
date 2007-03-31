// $Id: ESMC_F90Interface.h,v 1.6 2007/03/31 05:51:27 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Interface C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_F90INTERFACE_H
 #define ESMC_F90INTERFACE_H

//-------------------------------------------------------------------------
//BOP
//
// !DESCRIPTION:
// This file contains types and macros that are used on the C++ side to
// deal with the C++-to-F90 and F90-to-C++ interface.
//-------------------------------------------------------------------------
//EOP

//-------------------------------------------------------------------------
// Class that helps with [optinal] F90 array arguments on the interface
//-------------------------------------------------------------------------

class ESMC_InterfaceInt{
  public: // this thin class is public to make it's usage uncomplicated
    int *array;
    int dimCount;
    int extent[7];    // size 7 reflects the Fortran limit
  public:
    ESMC_InterfaceInt(void);   // native constructor
    ESMC_InterfaceInt(int *arrayArg, int dimArg, int *lenArg);//n. cnstr.
    ~ESMC_InterfaceInt(void);  // native destructor
};

//-------------------------------------------------------------------------
// Struct that is used in C++ classes to interface with deep F90 classes,
// e.g. for the C++-to-F90 interface
//-------------------------------------------------------------------------

typedef struct{
  void *memoryHolder[8];  // reserve 8 times the space of a void pointer
                          // this value has been determined empirically to work
                          // on the supported platforms.
}ESMC_F90ClassHolder;


//-------------------------------------------------------------------------
// Macros that are used in the C glue code for F90-to-C++
//-------------------------------------------------------------------------

// Pass not-present F90 optional argument through to C++ as NULL, otherwise
// leave unchanged.
#define ESMC_NOT_PRESENT_FILTER(arg) \
                           (!ESMC_PRESENT(arg) ? ESMC_NULL_POINTER : (arg))

// Macros for dynamicaly allocated (deep) objects to detect NULL object
// pointers being passed to C++.  This prevents the otherwise subsequent
// method invocations and member dereferences on non-existent objects,
// thereby preventing crashing or other corruptive behavior.

#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
  #define ESMF_CHECK_POINTER(ptr, status) \
            if ((ptr) == ESMC_NULL_POINTER) { \
              if (ESMC_PRESENT(status)) *(status) = ESMF_FAILURE; \
              return; \
            }

  #define ESMF_CHECK_BINARY_OPERATOR_POINTERS(ptr1, ptr2, result) \
          if ((ptr1) == ESMC_NULL_POINTER || (ptr2) == ESMC_NULL_POINTER) { \
            *(result) = ESMF_FALSE; \
            return; \
          }
#else
  // TODO: delete this else clause when F95 initializers universally supported
  #define ESMF_CHECK_POINTER(ptr, status)
  #define ESMF_CHECK_BINARY_OPERATOR_POINTERS(ptr1, ptr2, result)
#endif

 #endif // ESMC_INTERFACE_H
