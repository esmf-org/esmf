// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_DynamicMask_H
#define ESMC_DynamicMask_H

//-----------------------------------------------------------------------------
// ESMC_DynamicMask - dynamic masking object
//
// The code in this file defines the public C DynamicMask interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_DynamicMask.C}
// contains the definitions (full code bodies) for the DynamicMask methods.
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  // Allocate enough memory to store members in the Fortran side.
  // Adjust if members are added, rounding up to multiples of 64 byte
  char shallowMem[1024];
}ESMC_DynamicMask;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_DynamicMaskPredefinedSetR8R8R8 - Set R8R8R8 dynamic mask
//
// !INTERFACE:
int ESMC_DynamicMaskPredefinedSetR8R8R8(
  ESMC_DynamicMask *DynamicMask,         // inout
  enum  ESMC_DynamicMaskPredef_Flag predefFlag,  // in
  int *handleAllElements,
  ESMC_R8 *dynamicSrcMaskValue,
  ESMC_R8 *dynamicDstMaskValue
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Set R8R8R8 dynamic mask with predfined dynamic mask routine.
//
//  The arguments are:
//  \begin{description}
//  \item[DynamicMask]
//    The {\tt ESMC\_DynamicMask} to set.
//  \item[predefFlag]
//    Predefined dynamic mask routine.
//  \item[handleAllElements]
//    Make all elements available to the dynamic mask routine.
//  \item[dynamicSrcMaskValue]
//    Source element mask value.
//  \item[dynamicDstMaskValue]
//    Destination element mask value.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_DynamicMaskPredefinedSetR8R8R8V - Set R8R8R8V dynamic mask
//
// !INTERFACE:
int ESMC_DynamicMaskPredefinedSetR8R8R8V(
  ESMC_DynamicMask *DynamicMask,         // inout
  enum  ESMC_DynamicMaskPredef_Flag predefFlag,  // in
  int *handleAllElements,
  ESMC_R8 *dynamicSrcMaskValue,
  ESMC_R8 *dynamicDstMaskValue
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Set R8R8R8V dynamic mask with predfined dynamic mask routine.
//
//  The arguments are:
//  \begin{description}
//  \item[DynamicMask]
//    The {\tt ESMC\_DynamicMask} to set.
//  \item[predefFlag]
//    Predefined dynamic mask routine.
//  \item[handleAllElements]
//    Make all elements available to the dynamic mask routine.
//  \item[dynamicSrcMaskValue]
//    Source element mask value.
//  \item[dynamicDstMaskValue]
//    Destination element mask value.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_DynamicMaskPredefinedSetR4R8R4 - Set R4R8R4 dynamic mask
//
// !INTERFACE:
int ESMC_DynamicMaskPredefinedSetR4R8R4(
  ESMC_DynamicMask *DynamicMask,         // inout
  enum  ESMC_DynamicMaskPredef_Flag predefFlag,  // in
  int *handleAllElements,
  ESMC_R4 *dynamicSrcMaskValue,
  ESMC_R4 *dynamicDstMaskValue
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Set R4R8R4 dynamic mask with predfined dynamic mask routine.
//
//  The arguments are:
//  \begin{description}
//  \item[DynamicMask]
//    The {\tt ESMC\_DynamicMask} to set.
//  \item[predefFlag]
//    Predefined dynamic mask routine.
//  \item[handleAllElements]
//    Make all elements available to the dynamic mask routine.
//  \item[dynamicSrcMaskValue]
//    Source element mask value.
//  \item[dynamicDstMaskValue]
//    Destination element mask value.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_DynamicMaskPredefinedSetR4R8R4V - Set R4R8R4V dynamic mask
//
// !INTERFACE:
int ESMC_DynamicMaskPredefinedSetR4R8R4V(
  ESMC_DynamicMask *DynamicMask,         // inout
  enum  ESMC_DynamicMaskPredef_Flag predefFlag,  // in
  int *handleAllElements,
  ESMC_R4 *dynamicSrcMaskValue,
  ESMC_R4 *dynamicDstMaskValue
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Set R4R8R4V dynamic mask with predfined dynamic mask routine.
//
//  The arguments are:
//  \begin{description}
//  \item[DynamicMask]
//    The {\tt ESMC\_DynamicMask} to set.
//  \item[predefFlag]
//    Predefined dynamic mask routine.
//  \item[handleAllElements]
//    Make all elements available to the dynamic mask routine.
//  \item[dynamicSrcMaskValue]
//    Source element mask value.
//  \item[dynamicDstMaskValue]
//    Destination element mask value.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_DynamicMask_H
