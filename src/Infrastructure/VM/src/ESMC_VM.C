// $Id: ESMC_VM.C,v 1.25 2005/01/13 18:30:14 theurich Exp $
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
// Currently class {\tt ESMC\_VM} is derived from base class {\tt ESMC_VMK}
// and class {\tt ESMC\_VMPlan} is derived from base class {\tt ESMC_VMKPlan}.
// There are only very few new features that the derived classes add to their
// base classes, thus most of the implementing code is located in 
// {\tt ESMC_VMKernel.C}.
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include <pthread.h>

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
 static const char *const version = "$Id: ESMC_VM.C,v 1.25 2005/01/13 18:30:14 theurich Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Module variable pointing to the global default VM
// The global VM will be initialized in call ESMC_VMInitialize() and wrapped up
// calling ESMC_VMFinalize(). 
static ESMC_VM *GlobalVM = NULL;  
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Module arrays to hold association between tid <-> vm <-> vmID
#define ESMC_VM_MAXTIDS 1000
static pthread_t matchArray_tid[ESMC_VM_MAXTIDS];
static ESMC_VM *matchArray_vm[ESMC_VM_MAXTIDS];
static ESMC_VMId matchArray_vmID[ESMC_VM_MAXTIDS];
static int vmKeyWidth = 0;        // in units of 8-bit chars
static int vmKeyOff = 0;          // extra bits in last char
static int matchArray_count = 0;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// This section includes all the VM routines
//
//-----------------------------------------------------------------------------


static int ESMC_VMKeyCompare(char *vmKey1, char *vmKey2){
  int i;
  for (i=0; i<vmKeyWidth; i++)
    if (vmKey1[i] != vmKey2[i]) break;
  if (i==vmKeyWidth) return ESMF_TRUE;
  return ESMF_FALSE;
}




//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMStartup
//
// !INTERFACE:
void *ESMC_VM::ESMC_VMStartup(
//
// !RETURN VALUE:
//    void* to info structure
//
// !ARGUMENTS:
//
  class ESMC_VMPlan *vmp,         // plan for this child VM
  void *(fctp)(void *, void *),   // fnction pointer to 1st stage callback
  void *cargo){                   // pointer to cargo structure for in/out data
//
// !DESCRIPTION:
//    Startup a new child VM according to plan.
//
//EOP
//-----------------------------------------------------------------------------
  // startup the VM
  void *info = vmk_startup(static_cast<ESMC_VMKPlan *>(vmp), fctp, NULL);
  
  // now take care of book keeping for ESMF...
  int i, pid, m, n;
  int localrc;
  int matchArray_count_old = matchArray_count;
  // enter information for all threads of this new VM into the matchArray
  // TODO: make this thread-safe for when we allow ESMF-threading!
  for (int j=0; j<vmp->nspawn; j++){
    matchArray_tid[matchArray_count]  = vmp->myvms[j]->vmk_mypthid(); // pthid
    matchArray_vm[matchArray_count]   = vmp->myvms[j];

    matchArray_vmID[matchArray_count] = ESMC_VMIdCreate(&localrc);  // new VMId
    for (i=0; i<vmp->myvms[j]->vmk_npets(); i++){
      // loop through all the pets in the VM
      pid = vmp->myvms[j]->vmk_pid(i);
      m = pid / 8;
      n = pid % 8;
      matchArray_vmID[matchArray_count].vmKey[m] |= 0x80>>n;  // set the bits
    }
    matchArray_vmID[matchArray_count].localID = 0;  // assume this is first
    for (i=matchArray_count_old-1; i>=0; i--){
      if (ESMC_VMKeyCompare(matchArray_vmID[i].vmKey,
        matchArray_vmID[matchArray_count].vmKey) == ESMF_TRUE){
        matchArray_vmID[matchArray_count].localID = 
          matchArray_vmID[i].localID + 1; // overwrite the previous default
        break;
      }
    }

    ++matchArray_count;         // done
    
  }
  
  // return pointer to info structure
  return info;
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
  MPI_Comm     *mpiCommunicator,// out - MPI Intracommunicator for VM
  ESMC_Logical *okOpenMpFlag){  // out - flag whether user-level OpenMP o.k.
//
// !DESCRIPTION:
//   Get internal information about the specified {\tt ESMC\_VM} object.
//
//EOP
//-----------------------------------------------------------------------------
  if (localPet != ESMC_NULL_POINTER)
    *localPet = this->vmk_mypet();
  if (petCount != ESMC_NULL_POINTER)
    *petCount = this->vmk_npets();
  if (peCount != ESMC_NULL_POINTER){
    int npets = this->vmk_npets();
    *peCount = 0; // reset
    for (int i=0; i<npets; i++)
      *peCount += this->vmk_ncpet(i);
  }
  if (mpiCommunicator != ESMC_NULL_POINTER){
    *mpiCommunicator = this->vmk_mpi_comm();
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
    *peCount = this->vmk_ncpet(pet);
  if (ssiId != ESMC_NULL_POINTER)
    *ssiId = this->vmk_ssiid(pet);
  if (threadCount != ESMC_NULL_POINTER)
    *threadCount = this->vmk_nthreads(pet);
  if (threadId != ESMC_NULL_POINTER)
    *threadId = this->vmk_tid(pet);
  return ESMF_SUCCESS;            // TODO: Do some real error handling here...
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetPETMatchPET()"
//BOP
// !IROUTINE:  ESMC_VMGetPETMatchPET
//
// !INTERFACE:
int ESMC_VM::ESMC_VMGetPETMatchPET(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int pet,                      // in  - id of specified PET
  ESMC_VM &vmMatch,             // in  - vm to match against
  int *petMatchCount,           // out - number of matching PETs in vmMatch
  int *petMatchList,            // out - list of matching PETs in vmMatch
  int len_petMatchList          // in  - size of petMatchList
  ){              
//
// !DESCRIPTION:
//    Match pet in the current VM object against the PETs in the 
//    provided vmMatch VM. Return number of matched PETs and a list of the
//    matching pet id's that operate in the same virtual address space as
//    the specified pet.
//
//EOP
//-----------------------------------------------------------------------------
  int npets = vmMatch.vmk_npets();  // maximum number of PETs in vmMatch
  int *tempMatchList = new int[npets];
  int tempMatchCount = 0;
  int comparePID = pid[pet];        // this is pet's virtual address space id
  int j=0;
  for (int i=0; i<npets; i++)
    if (vmMatch.vmk_pid(i) == comparePID){
      tempMatchList[j] = i;
      ++j;
    }
  // now j is equal to the number of PETs in vmMatch which operate in the 
  // same virtual address space as pet in the current VM
  if (petMatchCount != ESMC_NULL_POINTER)
    *petMatchCount = j;
  if (len_petMatchList >= j)
    for (int i=0; i<j; i++)
      petMatchList[i] = tempMatchList[i];
  delete [] tempMatchList;
  return ESMF_SUCCESS;
}
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
// !IROUTINE:  ESMC_VMGetCurrent - Get Current VM
//
// !INTERFACE:
ESMC_VM *ESMC_VMGetCurrent(
//
// !RETURN VALUE:
//    Pointer to current VM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//   Get the {\tt ESMC\_VM} object of the current context.
//
//EOP
//-----------------------------------------------------------------------------
  *rc = ESMF_FAILURE; // assume failure
  pthread_t mytid = pthread_self();
  int i;
  for (i=0; i<matchArray_count; i++)
    if (matchArray_tid[i] == mytid) break;
  if (i == matchArray_count)
    return NULL;
  // found a match
  *rc = ESMF_SUCCESS;
  return matchArray_vm[i];
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
  GlobalVM->vmk_init();      // set up default ESMC_VMK (all MPI)
  *rc = ESMF_SUCCESS;        // TODO: Do some real error handling here...
  
  matchArray_count = 0;       // reset
  matchArray_tid[matchArray_count]  = pthread_self();
  matchArray_vm[matchArray_count]   = GlobalVM;
  
  // set vmID
  vmKeyWidth = GlobalVM->vmk_npets()/8;
  vmKeyOff   = GlobalVM->vmk_npets()%8;
  if (vmKeyOff){
    ++vmKeyWidth;               // correction for extra bits
    vmKeyOff = 8 - vmKeyOff;    // number of extra bits in last char
  }
//printf("gjt in ESMC_VMInitialize, vmKeyWidth = %d\n", vmKeyWidth);
  int localrc;
  matchArray_vmID[matchArray_count] = ESMC_VMIdCreate(&localrc);
  for (int i=0; i<vmKeyWidth; i++)
    matchArray_vmID[matchArray_count].vmKey[i] = 0xff;  // globalVM in all VASs
  matchArray_vmID[matchArray_count].vmKey[vmKeyWidth-1] =
    matchArray_vmID[matchArray_count].vmKey[vmKeyWidth-1]<<vmKeyOff; // shift
  matchArray_vmID[matchArray_count].localID = 0;        // globalVM is first

  ++matchArray_count;         // done

                             // totalview cannot handle events during the init
                             // call - it freezes or crashes or ignores input.
  GlobalVM->vmk_barrier();   // so for now, wait for everyone to init.

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
  GlobalVM->vmk_finalize();
  matchArray_count = 0;
  *rc = ESMF_SUCCESS;             // TODO: Do some real error handling here...
}
//-----------------------------------------------------------------------------




// - - brand new stuff for ESMC_VMId - -


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_VMGetCurrentID - Get ID of current VM
//
// !INTERFACE:
ESMC_VMId *ESMC_VMGetCurrentID(
//
// !RETURN VALUE:
//    ID of current VM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//   Get the ID of the current {\tt ESMC\_VM} object.
//
//EOP
//-----------------------------------------------------------------------------
  *rc = ESMF_FAILURE; // assume failure
  pthread_t mytid = pthread_self();
  int i;
  for (i=0; i<matchArray_count; i++)
    if (matchArray_tid[i] == mytid) break;
  if (i == matchArray_count)
    return NULL;
  // found a match
  *rc = ESMF_SUCCESS;
  return &matchArray_vmID[i];
}
//-----------------------------------------------------------------------------


void ESMC_VMIdPrint(ESMC_VMId *vmID){
  printf("ESMC_VMIdPrint:\n");
  if (vmID==NULL){
    printf("this is not a valid VMId!\n");
    return;
  }
  printf("vmID located at: %p\n", vmID);
  if (vmID->vmKey==NULL){
    printf("this VMId does not contain a valid vmKey!\n");
    return;
  }
  printf("  vmKey=0x");
  int bitmap=0;
  int k=0;
  for (int i=0; i<vmKeyWidth; i++){
    bitmap |= vmID->vmKey[i];
    bitmap = bitmap << 8;
    ++k;
    if (k==4){
      printf("%X", bitmap);
      bitmap=0;
      k=0;
    }
  }
  if (k!=0){
    bitmap = bitmap << (3-k)*8;
    printf("%X\n", bitmap);
  }
  printf("  localID: %d\n", vmID->localID);
  
}


ESMC_VMId ESMC_VMIdCreate(int *rc){
  // allocates memory for vmKey member
  ESMC_VMId vmID;    // temporary stack variable
  vmID.vmKey = new char[vmKeyWidth];
  for (int i=0; i<vmKeyWidth; i++)
    vmID.vmKey[i] = 0x00;  // zero out all bits
  vmID.localID = 0;   // reset
  *rc = ESMF_SUCCESS;
  return vmID;
}


void ESMC_VMIdDestroy(ESMC_VMId *vmID, int *rc){
  // frees memory for vmKey member
  if (vmID->vmKey){
    delete [] vmID->vmKey;
    vmID->vmKey = NULL;
  }
  *rc = ESMF_SUCCESS;
}



void ESMC_VM::ESMC_VMSendVMId(ESMC_VMId *vmID, int dest){
  vmk_send(vmID->vmKey, vmKeyWidth, dest);
  vmk_send(&(vmID->localID), sizeof(int), dest);
}

void ESMC_VM::ESMC_VMRecvVMId(ESMC_VMId *vmID, int source){
  vmk_recv(vmID->vmKey, vmKeyWidth, source);
  vmk_send(&(vmID->localID), sizeof(int), source);
}



