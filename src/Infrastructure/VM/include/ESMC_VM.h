// $Id: ESMC_VM.h,v 1.9 2004/05/21 05:04:12 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC_VM include file for C++

// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_VM_H
#define ESMC_VM_H

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_VM - virtual machine
//
// !DESCRIPTION:
//
// The code in this file defines two C++ classes {\tt ESMC\_VM} and 
// {\tt ESMC\_VMPlan}, their members and method signatures (prototypes).
// The companion file {\tt ESMC\_VM.C} contains the full code (bodies) 
// for the methods.
//
// Currently class {\tt ESMC\_VM} is derived from base class {\tt vmachine}
// and class {\tt ESMC\_VMPlan} is derived from base class {\tt vmplan}. There
// are only very few new features that the derived classes add to their base
// classes, thus most of the implementing code is located in {vmachine.C}.
//
///EOP
//-------------------------------------------------------------------------

#include <ESMC_Base.h>  
#include <vmachine.h>   // inherit from vmachine class

class ESMC_VM;
class ESMF_VMPlan;

// class definition
class ESMC_VM : public vmachine {   // inherits from vmachine class
  // This is the ESMF derived virtual machine class.
  public:
    // Get method that supports the F90 optional arguments interface
    int ESMC_VMGet(
      int          *localPet,       // out - id of local PET
      int          *petCount,       // out - number of PETs
      int          *peCount,        // out - number of PEs
      int          *mpiCommunicator,// out - MPI Intracommunicator for VM
      ESMC_Logical *okOpenMpFlag);  // out - flag whether user-level OpenMP o.k.
    // GetPET method that supports the F90 optional arguments interface
    int ESMC_VMGetPET(
      int pet,            // in  - id of specified PET
      int *peCount,       // out - number of PEs for specified PET
      int *ssiId,         // out - ssiid for specified PET
      int *threadCount,   // out - number of treads in thread group with PET
      int *threadId);     // out - thread id for specified PET
};// end class ESMC_VM

// external ESMC_VM methods:
ESMC_VM *ESMC_VMInitialize(int *rc);  // Initialize global vmachine
void     ESMC_VMFinalize(int *rc);    // Shut down and clean up global vmachine
ESMC_VM *ESMC_VMGetGlobal(int *rc);   // Return pointer to global VM

// class definition
class ESMC_VMPlan : public vmplan {   // inherits from vmplan class
  public:
    int nspawn;           // number of PETs this PET will spawn
    ESMC_VM **myvms;      // pointer array of ESMC_VM instances for this PET
    vmachine **myvmachs;  // pointer array of vmachine instances for this PET
};// end class ESMC_VMPlan

#endif  // ESMC_VM_H
