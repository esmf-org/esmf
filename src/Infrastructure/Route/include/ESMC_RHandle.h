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

#ifndef ESMC_RHandle_H
#define ESMC_RHandle_H

//-----------------------------------------------------------------------------
// ESMC_RouteHandle - Public C interface to the ESMF RouteHandle class
//
// The code in this file defines the public C RouteHandle class and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_RHandle.C}
// contains the definitions (full code bodies) for the RouteHandle methods.
//-----------------------------------------------------------------------------


#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_RouteHandle;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_RouteHandleCreateFromFile - Create a RouteHandle
//
// !INTERFACE:
ESMC_RouteHandle ESMC_RouteHandleCreateFromFile(
  char *filename,
  int *rc
);
// !RETURN VALUE:
//  ESMC_RouteHandle
//
// !DESCRIPTION:
//
//  Create an {\tt ESMC\_RouteHandle} object.
//
//  The arguments are:
//  \begin{description}
//  \item[filename] 
//    The file that describes the {\tt ESMC\_RouteHandle} object.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_RouteHandlePrint - Print a RouteHandle
//
// !INTERFACE:
int ESMC_RouteHandlePrint(
  ESMC_RouteHandle rh            // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Print internal information of the specified {\tt ESMC\_RouteHandle} object.
//
//  The arguments are:
//  \begin{description}
//  \item[rh] 
//    {\tt ESMC\_RouteHandle} object to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_RouteHandleWrite - Write a RouteHandle to file
//
// !INTERFACE:
int ESMC_RouteHandleWrite(
  ESMC_RouteHandle rh,          // in
  char *filename                // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Write {\tt ESMC\_RouteHandle} object to file to save regrid information for
//  fast input for regridding operations during runtime.
//
//  The arguments are:
//  \begin{description}
//  \item[rh] 
//    {\tt ESMC\_RouteHandle} object to be printed.
//  \item[filename] 
//    The name of the file for writing the {\tt ESMC\_RouteHandle} object.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_RHandle_H
