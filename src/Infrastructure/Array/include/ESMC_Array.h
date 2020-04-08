// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research, 
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

#ifndef ESMC_Array_H
#define ESMC_Array_H

//-----------------------------------------------------------------------------
// ESMC_Array - Public C interface to the ESMF Array class
//
// The code in this file defines the public C Array class and declares method 
// signatures (prototypes).  The companion file {\tt ESMC\_Array.C} contains
// the definitions (full code bodies) for the Array methods.
//-----------------------------------------------------------------------------


#include "ESMC_ArraySpec.h"
#include "ESMC_DistGrid.h"
#include "ESMC_Interface.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Array;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ArrayCreate - Create an Array
//
// !INTERFACE:
ESMC_Array ESMC_ArrayCreate(
  ESMC_ArraySpec arrayspec,   // in
  ESMC_DistGrid distgrid,     // in
  const char* name,           // in
  int *rc                     // out
);
// !RETURN VALUE:
//  Newly created ESMC_Array object.
//
// !DESCRIPTION:
//
//  Create an {\tt ESMC\_Array} object.
//
//  The arguments are:
//  \begin{description}
//  \item[arrayspec]
//    {\tt ESMC\_ArraySpec} object containing the type/kind/rank information.
//  \item[distgrid]
//    {\tt ESMC\_DistGrid} object that describes how the Array is decomposed and
//    distributed over DEs. The dimCount of distgrid must be smaller or equal
//    to the rank specified in arrayspec, otherwise a runtime ESMF error will be
//    raised.
//  \item[{[name]}]
//    The name for the Array object. If not specified, i.e. NULL,
//    a default unique name will be generated: "ArrayNNN" where NNN
//    is a unique sequence number from 001 to 999.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ArrayDestroy - Destroy an Array
//
// !INTERFACE:
int ESMC_ArrayDestroy(
  ESMC_Array *array           // inout
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Destroy an {\tt ESMC\_Array} object.
//
//  The arguments are:
//  \begin{description}
//  \item[array] 
//    {\tt ESMC\_Array} object to be destroyed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ArrayGetName - Get the name of an Array
//
// !INTERFACE:
const char *ESMC_ArrayGetName(
  ESMC_Array array,           // in
  int *rc                     // out
);
// !RETURN VALUE:
//  Pointer to the Array name string.
//
// !DESCRIPTION:
//
//  Get the name of the specified {\tt ESMC\_Array} object.
//
//  The arguments are:
//  \begin{description}
//  \item[array] 
//    {\tt ESMC\_Array} object to be queried.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ArrayGetPtr - Get pointer to Array data.
//
// !INTERFACE:
void *ESMC_ArrayGetPtr(
  ESMC_Array array,           // in
  int localDe,                // in
  int *rc                     // out
);
// !RETURN VALUE:
//  Pointer to the Array data.
//
// !DESCRIPTION:
//
//  Get pointer to the data of the specified {\tt ESMC\_Array} object.
//
//  The arguments are:
//  \begin{description}
//  \item[array] 
//    {\tt ESMC\_Array} object to be queried.
//  \item[localDe] 
//    Local De for which to data pointer is queried.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ArrayPrint - Print an Array
//
// !INTERFACE:
int ESMC_ArrayPrint(
  ESMC_Array array            // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Print internal information of the specified {\tt ESMC\_Array} object.
//
//  The arguments are:
//  \begin{description}
//  \item[array] 
//    {\tt ESMC\_Array} object to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


int ESMC_ArraySetLWidth(ESMC_Array array,
  ESMC_InterArrayInt computationalLWidthArg);

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Array_H
