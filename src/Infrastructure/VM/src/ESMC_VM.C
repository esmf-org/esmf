// $Id: ESMC_VM.C,v 1.9 2004/05/19 02:15:36 theurich Exp $
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
// classes, thus most of the implementing code is located in {vmachine.C}.
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include <iostream.h>  // cout
#include <ESMC_Start.h>
#include <ESMC_Base.h>  

// associated class definition file
#include <ESMC_VM.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_VM.C,v 1.9 2004/05/19 02:15:36 theurich Exp $";
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
  GlobalVM->vmachine_init();             // set up default vmachine (all MPI)
  *rc = ESMF_SUCCESS;
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
  *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMGetGlobal
//
// !INTERFACE:
ESMC_VM *ESMC_VMGetGlobal(
//
// !RETURN VALUE:
//    pointer to global VM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//    Return pointer to global VM
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
//    int error return code
//
// !ARGUMENTS:
//
  ESMF_KIND_I4 *mypet,          // out - petid
  ESMF_KIND_I4 *npets,          // out - number of PETs
  ESMF_KIND_I4 *npes,           // out - number of PEs
  int          *mpic,           // out - MPI Intracommunicator for VM
  ESMC_Logical *ok_openmp){     // out - indicate whether user-level OpenMP o.k.
//
// !DESCRIPTION:
//    Get information about a VM object
//
//EOP
//-----------------------------------------------------------------------------
  if (mypet != ESMC_NULL_POINTER)
    *mypet = this->vmachine_mypet();
  if (npets != ESMC_NULL_POINTER)
    *npets = this->vmachine_npets();
  if (npes != ESMC_NULL_POINTER){
    int npets = this->vmachine_npets();
    *npes = 0; // reset
    for (int i=0; i<npets; i++)
      *npes += this->vmachine_ncpet(i);
  }
  if (mpic != ESMC_NULL_POINTER){
    // TODO:  Note:  on SunOS, MPI_Comm is defined as pointer to a structure,
    //        so mpic should only be used as a reference container not to be
    //        changed by the user.
    //*mpic = (int) this->vmachine_mpi_comm();
    // TODO: Ensure that all of the MPI implementations have this MPI-2
    // function implemented. If not then deal with those cases by static 
    // type cast (as above). LAM has it and needs the MPI-2 interlanguage cast.
    *mpic = (int) MPI_Comm_c2f(this->vmachine_mpi_comm());
  }
  if (ok_openmp != ESMC_NULL_POINTER)
    *ok_openmp = ESMF_TRUE;   // TODO: Determine this at compile time...
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMGetPET
//
// !INTERFACE:
int ESMC_VM::ESMC_VMGetPET(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMF_KIND_I4 *petid,          // in  - petid for this PET
  ESMF_KIND_I4 *npes,           // out - number of PEs for this PET
  ESMF_KIND_I4 *ssiid,          // out - ssiid for this PET
  ESMF_KIND_I4 *nthreads,       // out - number of treads in group with PET
  ESMF_KIND_I4 *tid){           // out - thread id for this PET
//
// !DESCRIPTION:
//    Get PET specific information about a VM object
//
//EOP
//-----------------------------------------------------------------------------
  if (npes != ESMC_NULL_POINTER)
    *npes = this->vmachine_ncpet(*petid);
  if (ssiid != ESMC_NULL_POINTER)
    *ssiid = this->vmachine_ssiid(*petid);
  if (nthreads != ESMC_NULL_POINTER)
    *nthreads = this->vmachine_nthreads(*petid);
  if (tid != ESMC_NULL_POINTER)
    *tid = this->vmachine_tid(*petid);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------
