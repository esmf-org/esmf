// $Id: ESMC_VM.h,v 1.52.2.1 2010/02/05 20:01:32 svasquez Exp $
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

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_VM_H
#define ESMC_VM_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_VM - Public C interface to the ESMF VM class
//
// !DESCRIPTION:
//
// The code in this file defines the public C VM class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_VM.C} contains
// the definitions (full code bodies) for the VM methods.
//
//EOPI
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
int ESMC_VMPrint(ESMC_VM vm);
ESMC_VM ESMC_VMGetGlobal(int *rc);
ESMC_VM ESMC_VMGetCurrent(int *rc);
int ESMC_VMGet(ESMC_VM vm, int *localPet, int *petCount, int *peCount,
  MPI_Comm *mpiCommunicator, int *pthreadsEnabledFlag, int *openMPEnabledFlag);

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_VM_H
