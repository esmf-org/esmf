// $Id: ESMC_VM.h,v 1.4 2004/03/22 14:55:44 theurich Exp $
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
      ESMF_KIND_I4 *mypet,          // out - petid
      ESMF_KIND_I4 *npets,          // out - number of PETs
      ESMF_KIND_I4 *npes,           // out - number of PEs
      int          *mpic,           // out - MPI Intracommunicator for VM
      ESMC_Logical *ok_openmp);     // out - user-level OpenMP o.k.?
    // GetPET method that supports the F90 optional arguments interface
    int ESMC_VMGetPET(
      ESMF_KIND_I4 *petid,          // in  - petid for this PET
      ESMF_KIND_I4 *npes,           // out - number of PEs for this PET
      ESMF_KIND_I4 *ssiid,          // out - ssid for this PET
      ESMF_KIND_I4 *nthreads,       // out - number of treads in group with PET
      ESMF_KIND_I4 *tid);           // out - thread id for this PET
    // ESMC_VMFillFromVmachine allows to "copy" a vmachine object into a 
    // ESMC_VM object. This is necessary because calling vmachine_enter on 
    // a ESMC_VM object returns the child vmachine object and NOT an ESMC_VM
    // object!
    void ESMC_VMFillFromVmachine(vmachine &vmach);
};// end class ESMC_VM

// external ESMC_VM methods:
ESMC_VM *ESMC_VMInitialize(int *rc);  // Initialize global vmachine
void ESMC_VMFinalize(int *rc);        // Shut down and clean up global vmachine


// class definition
class ESMC_VMPlan : public vmplan {   // inherits from vmplan class
};// end class ESMC_VMPlan

#endif  // ESMC_VM_H
