// $Id: ESMC_Init.h,v 1.18.4.1 2010/02/05 20:04:35 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements constants and macros for the C Init Code.
//
// !USES:
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// prevent this file from being read more than once
#ifndef ESMC_Init_H
#define ESMC_Init_H

#include "ESMC_Arg.h"

// identifier list for optional ESMC arguments
enum {
  ESMCI_InitArgDefaultConfigFilenameID   = ESMCI_ArgBaseID
};

// prototypes for C routines

#ifdef __cplusplus
extern "C" {
#endif
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Initialize - Initialize the ESMF Framework
//
// !INTERFACE:
  int ESMC_Initialize(
//
// !RETURN VALUE:
//  int return code
//
// !ARGUMENTS:
    int *rc,        // return code
    ...);           // optional arguments
#define ESMC_InitArgDefaultConfigFilename(ARG)  \
ESMCI_Arg(ESMCI_InitArgDefaultConfigFilenameID,ARG)
//  
// !DESCRIPTION:
//  Initialize the ESMF.  This method must be called before
//  any other ESMF methods are used.  The method contains a
//  barrier before returning, ensuring that all processes
//  made it successfully through initialization.
//
//  Typically {\tt ESMC\_Initialize()} will call {\tt MPI\_Init()} 
//  internally unless MPI has been initialized by the user code before
//  initializing the framework. If the MPI initialization is left to
//  {\tt ESMC\_Initialize()} it inherits all of the MPI implementation 
//  dependent limitations of what may or may not be done before 
//  {\tt MPI\_Init()}. For instance, it is unsafe for some MPI implementations,
//  such as MPICH, to do IO before the MPI environment is initialized. Please
//  consult the documentation of your MPI implementation for details.
//
//  Before exiting the application
//  the user must call {\tt ESMC\_Finalize()} to release resources 
//  and clean up the ESMF gracefully.
//
//  The arguments are:
//  \begin{description}
//  \item [{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \item [{[defaultConfigFilename]}]
//    Name of the default configuration file for the entire application.
//  \end{description}
//EOP
//-----------------------------------------------------------------------------
#ifdef __cplusplus
} // extern "C"
#endif

  
#ifdef __cplusplus
extern "C" {
#endif
  int ESMC_Finalize(void);
#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Init_H
