// $Id: ESMC_VM.h,v 1.39.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMC_VM - virtual machine
//
// !DESCRIPTION:
//
// The code in this file defines two C++ classes {\tt ESMC\_VM} and 
// {\tt ESMC\_VMPlan}, their members and method signatures (prototypes).
// The companion file {\tt ESMC\_VM.C} contains the full code (bodies) 
// for the methods.
//
// Currently class {\tt ESMC\_VM} is derived from base class {\tt ESMC_VMK}
// and class {\tt ESMC\_VMPlan} is derived from base class {\tt ESMC_VMKPlan}.
// There are only very few new features that the derived classes add to their
// base classes, thus most of the implementing code is located in 
// {\tt ESMC_VMKernel.C}.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_VMKernel.h"    // inherit from ESMC_VMK class

//-------------------------------------------------------------------------

namespace ESMCI {

typedef struct{
  char *vmKey;    // bit-pattern that identifies VM VAS context
  int localID;    // local ID of the VM within VAS context
}VMId;

} // namespace ESMCI

//-------------------------------------------------------------------------

#include "ESMC_Base.h"        // cannot move Base.h before def. of ESMCI::VMId 

//-------------------------------------------------------------------------

namespace ESMCI {

// ESMCI::VMId methods:
VMId VMIdCreate(int *rc);      // allocates memory for vmKey member
void VMIdDestroy(VMId *vmID, int *rc); // frees memory for vmKey memb
void VMIdPrint(VMId *vmID);
ESMC_Logical VMIdCompare(VMId *vmID1, VMId *vmID2);
int VMIdCopy(VMId *vmIDdst, VMId *vmIDsrc);

} // namespace ESMCI


//-------------------------------------------------------------------------


namespace ESMCI {

// classes

class VM;
class VMPlan;

// class definition
class VM : public VMK {   // inherits from ESMCI::VMK class
  // This is the ESMF derived virtual machine class.

  public:
    // initialize(), finalize() and abort() of global VM
    static VM *initialize(MPI_Comm mpiCommunicator, int *rc);
    static void finalize(ESMC_Logical *keepMpiFlag, int *rc);   
    static void abort(int *rc);
    // life cycle methods      
    void *startup(class VMPlan *vmp, void *(fctp)(void *, void *), void *cargo,
      int *rc);
    int enter(class VMPlan *vmp, void *info, void *cargo);
    void shutdown(class VMPlan *vmp, void *info, int *rc);
    // get()
    int getPETMatchPET(int pet, VM &vmMatch, int *petMatchCount,
      int *petMatchList, int len_petMatchList);
    VMId *getVMId(int *rc) const;   // Return VMId of the VM context.
    static void getArgs(int *argc, char ***argv, int *rc);  // command line args
    static VM *getGlobal(int *rc);      // global VM
    static VM *getCurrent(int *rc);     // current VM
    static VMId *getCurrentID(int *rc); // VMId of current VM
    // misc.
    int print() const;
    int validate() const;
    int sendVMId(VMId *vmid, int dest);
    int recvVMId(VMId *vmid, int source);
};  // class VM


// class definition
class VMPlan : public VMKPlan {   // inherits from ESMCI::VMKPlan

  public:
    int nspawn;     // number of PETs this PET will spawn
    VM **myvms;     // pointer array of VM instances for this PET
    VMK **myvmachs; // pointer array of VMK instances for this PET
};  // class VMPlan

} // namespace ESMCI

#endif  // ESMC_VM_H
