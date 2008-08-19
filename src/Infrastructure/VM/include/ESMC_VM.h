// $Id: ESMC_VM.h,v 1.43 2008/08/19 22:52:43 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
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


#include "ESMC_Interface.h"

extern "C" {

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_VM;

// Class API
int ESMC_VMPrint(ESMC_VM vm);
ESMC_VM ESMC_VMGetGlobal(int *rc);
ESMC_VM ESMC_VMGetCurrent(int *rc);
int ESMC_VMGet(ESMC_VM *vm, int *localPet, int *petCount,
               int *peCount, int *mpiCommunicator,
               int *supportPthreadsFlag,
               int * supportOpenMPFlag);

}; // extern "C"


#endif  // ESMC_VM_H
