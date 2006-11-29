// $Id: ESMC_VM.h,v 1.33 2006/11/29 22:52:38 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

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
// Currently class {\tt ESMC\_VM} is derived from base class {\tt ESMC_VMK}
// and class {\tt ESMC\_VMPlan} is derived from base class {\tt ESMC_VMKPlan}.
// There are only very few new features that the derived classes add to their
// base classes, thus most of the implementing code is located in 
// {\tt ESMC_VMKernel.C}.
//
///EOP
//-------------------------------------------------------------------------

#include "ESMC_VMKernel.h"    // inherit from ESMC_VMK class


typedef struct{
  char *vmKey;    // bit-pattern that identifies VM VAS context
  int localID;    // local ID of the VM within VAS context
}ESMC_VMId;

#include "ESMC_Base.h"        // cannot move Base.h before def. of ESMC_VMId 

// external ESMC_VMId methods:
ESMC_VMId ESMC_VMIdCreate(int *rc);      // allocates memory for vmKey member
void ESMC_VMIdDestroy(ESMC_VMId *vmID, int *rc); // frees memory for vmKey memb
void ESMC_VMIdPrint(ESMC_VMId *vmID);
ESMC_Logical ESMC_VMIdCompare(ESMC_VMId *vmID1, ESMC_VMId *vmID2);
int ESMC_VMIdCopy(ESMC_VMId *vmIDdst, ESMC_VMId *vmIDsrc);


class ESMC_VM;
class ESMF_VMPlan;


// class definition
class ESMC_VM : public ESMC_VMK {   // inherits from ESMC_VMK class
  // This is the ESMF derived virtual machine class.
  public:
    void *ESMC_VMStartup(class ESMC_VMPlan *vmp, void *(fctp)(void *, void *),
      void *cargo, int *rc);
    int ESMC_VMEnter(class ESMC_VMPlan *vmp, void *info, void *cargo);
    int ESMC_VMGet(
      // Get method that supports the F90 optional arguments interface
      int          *localPet,       // out - id of local PET
      int          *petCount,       // out - number of PETs
      int          *peCount,        // out - number of PEs
      MPI_Comm     *mpiCommunicator,// out - MPI Intracommunicator for VM
      ESMC_Logical *okOpenMpFlag);  // out - flag whether user-level OpenMP o.k.
    int ESMC_VMGetPETLocalInfo(
      // GetPETLocalInfo method that supports the F90 optional args interface
      int pet,            // in  - id of specified PET
      int *peCount,       // out - number of PEs for specified PET
      int *ssiId,         // out - ssiid for specified PET
      int *threadCount,   // out - number of treads in thread group with PET
      int *threadId,      // out - thread id for specified PET
      int *vas);          // out - virtual address space of the specified PET
    int ESMC_VMGetPETMatchPET(
      // match PET in current VM against PETs of another VM
      int pet,                      // in  - id of specified PET
      ESMC_VM &vmMatch,             // in  - vm to match against
      int *petMatchCount,           // out - number of matching PETs in vmMatch
      int *petMatchList,            // out - list of matching PETs in vmMatch
      int len_petMatchList);        // in  - size of petMatchList
    ESMC_VMId *ESMC_VMGetVMId(int *rc);   // Return VMId of the VM context.
    int ESMC_VMSendVMId(ESMC_VMId *vmid, int dest);
    int ESMC_VMRecvVMId(ESMC_VMId *vmid, int source);
    void ESMC_VMPrint(int *rc=NULL);
};// end class ESMC_VM

// external ESMC_VM methods:
void     ESMC_VMGetArgs(int *argc, char ***argv, int *rc);  // Command line args
ESMC_VM *ESMC_VMGetGlobal(int *rc);   // Return pointer to global VM
ESMC_VM *ESMC_VMGetCurrent(int *rc);  // Return pointer to VM of current context
ESMC_VMId *ESMC_VMGetCurrentID(int *rc);// Return ID of the current VM context.
ESMC_VM *ESMC_VMInitialize(MPI_Comm mpiCommunicator, int *rc);  // Initialize
                                                                // global
                                                                // ESMC_VMK
void     ESMC_VMFinalize(ESMC_Logical *keepMpiFlag, int *rc);   // Shut down and
                                                                // clean up
                                                                // global
                                                                // ESMC_VMK
void     ESMC_VMAbort(int *rc);       // Abort and clean up global ESMC_VMK
    

// class definition
class ESMC_VMPlan : public ESMC_VMKPlan {   // inherits from ESMC_VMKPlan class
  public:
    int nspawn;           // number of PETs this PET will spawn
    ESMC_VM **myvms;      // pointer array of ESMC_VM instances for this PET
    ESMC_VMK **myvmachs;  // pointer array of ESMC_VMK instances for this PET
};// end class ESMC_VMPlan


#endif  // ESMC_VM_H
