// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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

#ifndef ESMC_VM_H
#define ESMC_VM_H

//-----------------------------------------------------------------------------
// ESMC_VM - Public C interface to the ESMF VM class
//
// The code in this file defines the public C VM class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_VM.C} contains
// the definitions (full code bodies) for the VM methods.
//-----------------------------------------------------------------------------

#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif

#include <mpi.h>

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_VM;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_VMGet - Get VM internals
//
// !INTERFACE:
int ESMC_VMGet(
  ESMC_VM vm,                   // in
  int *localPet,                // out
  int *petCount,                // out
  int *peCount,                 // out
  MPI_Comm *mpiCommunicator,    // out
  int *pthreadsEnabledFlag,     // out
  int *openMPEnabledFlag        // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Get internal information about the specified {\tt ESMC\_VM} object.
//
//  The arguments are:
//  \begin{description}
//  \item[vm] 
//    Queried {\tt ESMC\_VM} object.
//  \item[{[localPet]}] 
//    Upon return this holds the id of the PET that issued this call.
//  \item[{[petCount]}]
//    Upon return this holds the number of PETs in the specified {\tt ESMC\_VM}
//    object.
//  \item[{[peCount]}]
//    Upon return this holds the number of PEs referenced by the specified
//    {\tt ESMC\_VM} object.
//  \item[{[mpiCommunicator]}]
//    Upon return this holds the MPI intra-communicator used by the 
//    specified {\tt ESMC\_VM} object. This communicator may be used for
//    user-level MPI communications. It is recommended that the user
//    duplicates the communicator via {\tt MPI\_Comm\_Dup()} in order to
//    prevent any interference with ESMF communications.
//  \item[{[pthreadsEnabledFlag]}]
//    A return value of '1' indicates that the ESMF library was compiled with
//    Pthreads enabled. A return value of '0' indicates that Pthreads are
//    disabled in the ESMF library.
//  \item[{[openMPEnabledFlag]}]
//    A return value of '1' indicates that the ESMF library was compiled with
//    OpenMP enabled. A return value of '0' indicates that OpenMP is
//    disabled in the ESMF library.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_VMGetCurrent - Get current VM
//
// !INTERFACE:
ESMC_VM ESMC_VMGetCurrent(
  int *rc                     // out
);
// !RETURN VALUE:
//  VM object of the current execution context.
//
// !DESCRIPTION:
//
// \begin{sloppypar}
//  Get the {\tt ESMC\_VM} object of the current execution context. Calling
//  {\tt ESMC\_VMGetCurrent()} within an ESMF Component, will return the
//  same VM object as {\tt ESMC\_GridCompGet(..., vm=vm, ...)} or
//  {\tt ESMC\_CplCompGet(..., vm=vm, ...)}. 
// \end{sloppypar}
//
//  The main purpose of providing {\tt ESMC\_VMGetCurrent()} is to simplify ESMF
//  adoption in legacy code. Specifically, code that uses {\tt MPI\_COMM\_WORLD}
//  deep within its calling tree can easily be modified to use the correct MPI
//  communicator of the current ESMF execution context. The advantage is that
//  these modifications are very local, and do not require wide reaching
//  interface changes in the legacy code to pass down the ESMF component object,
//  or the MPI communicator.
//
//  The use of {\tt ESMC\_VMGetCurrent()} is strongly discouraged in newly
//  written Component code. Instead, the ESMF Component object should be used as
//  the appropriate container of ESMF context information. This object should be
//  passed between the subroutines of a Component, and be queried for any
//  Component specific information.
//
//  Outside of a Component context, i.e. within the driver context, the call
//  to {\tt ESMC\_VMGetCurrent()} is identical to {\tt ESMC\_VMGetGlobal()}.
//  \newline
//
//  The arguments are:
//  \begin{description}
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_VMGetGlobal - Get global VM
//
// !INTERFACE:
ESMC_VM ESMC_VMGetGlobal(
  int *rc                     // out
);
// !RETURN VALUE:
//  VM object of the global execution context.
//
// !DESCRIPTION:
//
//  Get the global {\tt ESMC\_VM} object. This is the VM object
//  that is created during {\tt ESMC\_Initialize()} and is the ultimate
//  parent of all VM objects in an ESMF application. It is identical to the VM
//  object returned by {\tt ESMC\_Initialize(..., vm=vm, ...)}.
//
//  The {\tt ESMC\_VMGetGlobal()} call provides access to information about the
//  global execution context via the global VM. This call is necessary because
//  ESMF does not create a global ESMF Component during
//  {\tt ESMC\_Initialize()} that could be queried for information about
//  the global execution context of an ESMF application.
//  
//  Usage of {\tt ESMC\_VMGetGlobal()} from within Component code is
//  strongly discouraged. ESMF Components should only access their own VM
//  objects through Component methods. Global information, if required by
//  the Component user code, should be passed down to the Component from the 
//  driver through the Component calling interface.
//  \newline
//
//  The arguments are:
//  \begin{description}
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_VMPrint - Print a VM
//
// !INTERFACE:
int ESMC_VMPrint(
  ESMC_VM vm                // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Print internal information of the specified {\tt ESMC\_VM} object.
//
//  The arguments are:
//  \begin{description}
//  \item[vm] 
//    {\tt ESMC\_VM} object to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_VM_H
