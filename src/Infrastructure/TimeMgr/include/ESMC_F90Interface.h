// $Id: ESMC_F90Interface.h,v 1.1 2004/02/18 01:40:59 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMF Interface C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_INTERFACE_H
 #define ESMC_INTERFACE_H

//-------------------------------------------------------------------------
//BOP
//
// !DESCRIPTION:
// These macros are used in the F90-to-C++ interface's C-glue-code for
// dynamicaly allocated (deep) objects to detect NULL object pointers being
// passed to C++.  This prevents the otherwise subsequent method invocations
// and member dereferences on non-existent objects, thereby preventing crashing
// or other corruptive behavior.
//-------------------------------------------------------------------------
//EOP

#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
  #define ESMF_CHECK_POINTER(ptr, status) \
            if ((ptr) == ESMC_NULL_POINTER) { \
              if ((void*) (status) != (void*) ESMC_NULL_POINTER && \
                  (void*) (status) != (void*) ESMC_BAD_POINTER) { \
                *(status) = ESMF_FAILURE; \
              } \
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
