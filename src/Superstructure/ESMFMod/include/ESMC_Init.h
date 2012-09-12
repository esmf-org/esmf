// $Id: ESMC_Init.h,v 1.31 2012/09/12 17:05:07 w6ws Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
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
  ESMCI_InitArgDefaultConfigFilenameID   = ESMCI_ArgBaseID,
  ESMCI_InitArgLogKindFlagID             = ESMCI_ArgBaseID + 1
};

// prototypes for C routines

#ifdef __cplusplus
extern "C" {
#endif
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_Initialize - Initialize ESMF
//
// !INTERFACE:
  int ESMC_Initialize(
    int *rc,        // return code
    ...);           // optional arguments
#define ESMC_InitArgDefaultConfigFilename(ARG)  \
ESMCI_Arg(ESMCI_InitArgDefaultConfigFilenameID,ARG)
#define ESMC_InitArgLogKindFlag(ARG)  \
ESMCI_Arg(ESMCI_InitArgLogKindFlagID,ARG)

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
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
//  Optional arguments are recognised.  For example to turn off logging
//  so that no log files would be created, the {\tt ESMC\_Initialize()} call
//  would be coded as:
//
//    ESMC\_Initialize (\&rc,
//      ESMC\_InitArgLogKindFlag(ESMC\_LOGKIND\_NONE),
//      ESMC\_ArgLast);
//
//  Before exiting the application
//  the user must call {\tt ESMC\_Finalize()} to release resources 
//  and clean up the ESMF gracefully.
//
//  The arguments are:
//  \begin{description}
//  \item [{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//    {\tt NULL} may be passed when the return code is not desired.
//  \item [{[defaultConfigFilename]}]
//    Name of the default configuration file for the entire application.
//  \item [{[LogKindFlag]}]
//    Sets the default Log Type to be used by ESMF Log Manager.
//    This flag is documented in section \ref{const:clogkindflag}.
//    If not specified, defaults to {\tt ESMC\_LOGKIND\_MULTI}.
//  \item [ESMC\_ArgLast]
//    The {\tt ESMC\_ArgLast} macro is always required as a final argument
//    to indicate the end of the optional argument list.  This must be provided
//    even when there are no optional arguments.
//  \end{description}
//EOP
//-----------------------------------------------------------------------------
#ifdef __cplusplus
} // extern "C"
#endif

  
#ifdef __cplusplus
extern "C" {
#endif
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_Finalize - Finalize the ESMF Framework
//
// !INTERFACE:
  int ESMC_Finalize(void);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
// This must be called once on each PET before the application exits to
// allow ESMF to flush buffers, close open connections, and release
// internal resources cleanly.
//EOP
#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Init_H
