// $Id: ESMC_VM.C,v 1.15 2004/06/21 18:25:18 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC VM method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the methods of two C++ classes 
// {\tt ESMC\_VM} and {\tt ESMC\_VMPlan} which are defined in the companion 
// file {\tt ESMC\_VM.C}.
//
// Currently class {\tt ESMC\_VM} is derived from base class {\tt vmachine}
// and class {\tt ESMC\_VMPlan} is derived from base class {\tt vmplan}. There
// are only very few new features that the derived classes add to their base
// classes, thus most of the implementing code is located in {\tt vmachine.C}.
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include <ESMC_Start.h>
#include <ESMC_Base.h>  

// associated class definition file
#include <ESMC_VM.h>

// LogErr
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_VM.C,v 1.15 2004/06/21 18:25:18 theurich Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Module variable pointing to the global default VM
// The global VM will be initialized in call ESMC_VMInitialize() and wrapped up
// calling ESMC_VMFinalize(). 
static ESMC_VM *GlobalVM = NULL;  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// This section includes all the VM routines
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMGetGlobal - Get Global VM
//
// !INTERFACE:
ESMC_VM *ESMC_VMGetGlobal(
//
// !RETURN VALUE:
//    Pointer to global VM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//   Get the global default {\tt ESMC\_VM} object. This is the {\tt ESMC\_VM}
//   object that was created during {\tt ESMC\_Initialize()} and is the ultimate
//   parent of all {\tt ESMC\_VM} objects in an ESMF application.
//
//EOP
//-----------------------------------------------------------------------------
  *rc = ESMF_SUCCESS;
  return GlobalVM;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMGet
//
// !INTERFACE:
int ESMC_VM::ESMC_VMGet(
//
// !RETURN VALUE:
//    Error return code
//
// !ARGUMENTS:
//
  int          *localPet,       // out - id of local PET
  int          *petCount,       // out - number of PETs
  int          *peCount,        // out - number of PEs
  int          *mpiCommunicator,// out - MPI Intracommunicator for VM
  ESMC_Logical *okOpenMpFlag){  // out - flag whether user-level OpenMP o.k.
//
// !DESCRIPTION:
//   Get internal information about the specified {\tt ESMC\_VM} object.
//
//EOP
//-----------------------------------------------------------------------------
  if (localPet != ESMC_NULL_POINTER)
    *localPet = this->vmachine_mypet();
  if (petCount != ESMC_NULL_POINTER)
    *petCount = this->vmachine_npets();
  if (peCount != ESMC_NULL_POINTER){
    int npets = this->vmachine_npets();
    *peCount = 0; // reset
    for (int i=0; i<npets; i++)
      *peCount += this->vmachine_ncpet(i);
  }
  if (mpiCommunicator != ESMC_NULL_POINTER){
    // TODO: Ensure that all of the MPI implementations have this MPI-2
    // function implemented. If not then deal with those cases by static 
    // type cast (int). LAM has it and needs the MPI-2 interlanguage cast.
    *mpiCommunicator = (int) MPI_Comm_c2f(this->vmachine_mpi_comm());
  }
  if (okOpenMpFlag != ESMC_NULL_POINTER)
    *okOpenMpFlag = ESMF_TRUE;    // TODO: Determine this at compile time...
  return ESMF_SUCCESS;            // TODO: Do some real error handling here...
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMGetPETLocalInfo
//
// !INTERFACE:
int ESMC_VM::ESMC_VMGetPETLocalInfo(
//
// !RETURN VALUE:
//    Error return code
//
// !ARGUMENTS:
//
  int pet,            // in  - id of specified PET
  int *peCount,       // out - number of PEs for specified PET
  int *ssiId,         // out - ssiid for specified PET
  int *threadCount,   // out - number of treads in thread group with PET
  int *threadId){     // out - thread id for specified PET
//
// !DESCRIPTION:
//   Get internal information about the specified PET within the specified
//   {\tt ESMF\_VM} object.
//
//EOP
//-----------------------------------------------------------------------------
  if (peCount != ESMC_NULL_POINTER)
    *peCount = this->vmachine_ncpet(pet);
  if (ssiId != ESMC_NULL_POINTER)
    *ssiId = this->vmachine_ssiid(pet);
  if (threadCount != ESMC_NULL_POINTER)
    *threadCount = this->vmachine_nthreads(pet);
  if (threadId != ESMC_NULL_POINTER)
    *threadId = this->vmachine_tid(pet);
  return ESMF_SUCCESS;            // TODO: Do some real error handling here...
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMInitialize
//
// !INTERFACE:
ESMC_VM *ESMC_VMInitialize(
//
// !RETURN VALUE:
//    ESMC_VM* to GlobalVM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//    Initialize the global virtual machine as an all MPI VM and return a 
//    handle to the local instance of the GlobalVM.
//
//EOP
//-----------------------------------------------------------------------------
  GlobalVM = new ESMC_VM;
  GlobalVM->vmachine_init();      // set up default vmachine (all MPI)
  *rc = ESMF_SUCCESS;             // TODO: Do some real error handling here...
  return GlobalVM;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMFinalize
//
// !INTERFACE:
void ESMC_VMFinalize(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//    Finalize the global virtual machine referenced by GlobalVM
//
//EOP
//-----------------------------------------------------------------------------
  GlobalVM->vmachine_finalize();
  *rc = ESMF_SUCCESS;             // TODO: Do some real error handling here...
}
//-----------------------------------------------------------------------------
