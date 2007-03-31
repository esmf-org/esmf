// $Id: ESMC_VM.C,v 1.49 2007/03/31 05:51:28 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC VM method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the methods of two C++ classes 
// {\tt ESMC\_VM} and {\tt ESMC\_VMPlan} which are defined in the companion 
// file {\tt ESMC\_VM.h}.
//
// Class {\tt ESMC\_VM} is derived from base class {\tt ESMC_VMK}
// and class {\tt ESMC\_VMPlan} is derived from base class {\tt ESMC_VMKPlan}.
// Both base classes are defined in {\tt ESMC_VMKernel.h} and implemented in
// source file {\tt ESMC_VMKernel.C}.
// There a couple of new features that the derived classes add to their
// base classes. This is the content of this source file. Furthermore, this
// source file contains the implementation of a global association list for VMs
// used in ESMF.
//
//-----------------------------------------------------------------------------

// include higher level, 3rd party or system headers
#include "ESMF_Pthread.h"

// include ESMF headers
#include "ESMC_Start.h"
#include "ESMC_Base.h" 

// include associated class definition
#include "ESMC_VM.h"

// LogErr
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_VM.C,v 1.49 2007/03/31 05:51:28 cdeluca Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Module variable pointing to the global default VM
// The global VM will be initialized in call ESMC_VMInitialize() and wrapped up
// calling ESMC_VMFinalize(). 
static ESMC_VM *GlobalVM = NULL;  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Module arrays to hold association table between tid <-> vm <-> vmID
#define ESMC_VM_MATCHTABLEMAX 10000  // maximum number of entries in table
static pthread_t matchTable_tid[ESMC_VM_MATCHTABLEMAX];
static ESMC_VM *matchTable_vm[ESMC_VM_MATCHTABLEMAX];
static ESMC_VMId matchTable_vmID[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet static pthread_t *matchTable_tid;
//gjtNotYet static ESMC_VM **matchTable_vm;
//gjtNotYet static ESMC_VMId *matchTable_vmID;
static int vmKeyWidth = 0;      // width in units of 8-bit chars
static int vmKeyOff = 0;        // extra bits in last char (bits to be ignored)
static int matchTableBound = 0; // upper bound of currently filled entries
static int matchTableIndex = 0; // process wide index for non-thread based VMs
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// static module functions
//
//-----------------------------------------------------------------------------

static ESMC_Logical ESMC_VMKeyCompare(char *vmKey1, char *vmKey2){
  int i;
  for (i=0; i<vmKeyWidth; i++)
    if (vmKey1[i] != vmKey2[i]) break;
  if (i==vmKeyWidth) return ESMF_TRUE;
  return ESMF_FALSE;
}


//-----------------------------------------------------------------------------
//
// external VMId functions
//
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMIdCreate()"
//BOPI
// !IROUTINE:  ESMC_VMIdCreate
//
// !INTERFACE:
ESMC_VMId ESMC_VMIdCreate(
//
// !RETURN VALUE:
//    Created {\tt ESMC\_VMId} object.
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//    Allocate memory for a new {\tt ESMC\_VMId} object and reset members.
//
//EOPI
//-----------------------------------------------------------------------------
  // allocates memory for vmKey member
  ESMC_VMId vmID;    // temporary stack variable
  vmID.vmKey = new char[vmKeyWidth];
  for (int i=0; i<vmKeyWidth; i++)
    vmID.vmKey[i] = 0x00;  // zero out all bits
  vmID.localID = 0;   // reset
  *rc = ESMF_SUCCESS;
  return vmID;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMIdDestroy()"
//BOPI
// !IROUTINE:  ESMC_VMIdDestroy
//
// !INTERFACE:
void ESMC_VMIdDestroy(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ESMC_VMId *vmID, 
  int *rc
  ){
//
// !DESCRIPTION:
//    Free memory for a previously created {\tt ESMC\_VMId} object.
//
//EOPI
//-----------------------------------------------------------------------------
  *rc = ESMF_FAILURE;         // assume failure
  if (vmID==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmID", ESMC_LOG_ERROR);
    return; // bail out
  }
  if (vmID->vmKey==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmID->vmKey", ESMC_LOG_ERROR);
    return; // bail out
  }
  // frees memory for vmKey member
  if (vmID->vmKey){
    delete [] vmID->vmKey;
    vmID->vmKey = NULL;
  }
  *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMIdCompare()"
//BOPI
// !IROUTINE:  ESMC_VMIdCompare
//
// !INTERFACE:
ESMC_Logical ESMC_VMIdCompare(
//
// !RETURN VALUE:
//    {\tt ESMC\_Logical} indicating result of comparison. 
//
// !ARGUMENTS:
//
  ESMC_VMId *vmID1, 
  ESMC_VMId *vmID2
  ){
//
// !DESCRIPTION:
//    Compare two {\tt ESMC\_VMId} objects.
//
//EOPI
//-----------------------------------------------------------------------------
  if (vmID1==NULL || vmID2==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmIDs", ESMC_LOG_ERROR);
    return ESMF_FALSE;    // bail out
  }
  if (vmID1->localID != vmID2->localID) return ESMF_FALSE;
  return ESMC_VMKeyCompare(vmID1->vmKey, vmID2->vmKey);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMIdCopy()"
//BOPI
// !IROUTINE:  ESMC_VMIdCopy
//
// !INTERFACE:
int ESMC_VMIdCopy(
//
// !RETURN VALUE:
//    Error code. 
//
// !ARGUMENTS:
//
  ESMC_VMId *vmIDdst, 
  ESMC_VMId *vmIDsrc
  ){
//
// !DESCRIPTION:
//    Copy {\tt ESMC\_VMId} object.
//
//EOPI
//-----------------------------------------------------------------------------
  if (vmIDdst==NULL || vmIDsrc==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmIDs", ESMC_LOG_ERROR);
    return ESMF_FAILURE;    // bail out
  }
  for (int i=0; i<vmKeyWidth; i++)
    vmIDdst->vmKey[i] = vmIDsrc->vmKey[i];
  vmIDdst->localID = vmIDsrc->localID;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMIdPrint()"
//BOPI
// !IROUTINE:  ESMC_VMIdPrint
//
// !INTERFACE:
void ESMC_VMIdPrint(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ESMC_VMId *vmID
  ){
//
// !DESCRIPTION:
//    Print an {\tt ESMC\_VMId} object.
//
//EOPI
//-----------------------------------------------------------------------------
  printf("ESMC_VMIdPrint:\n");
  if (vmID==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmID", ESMC_LOG_ERROR);
    return; // bail out
  }
  printf("vmID located at: %p\n", vmID);
  if (vmID->vmKey==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmID->vmKey", ESMC_LOG_ERROR);
    return; // bail out
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
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// public ESMC_VM methods
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMStartup()"
//BOPI
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
  void *(fctp)(void *, void *),   // function pointer to 1st stage callback
  void *cargo,                    // pointer to cargo structure for in/out data
  int *rc){                       // error return code
//
// !DESCRIPTION:
//    Startup a new child VM according to plan.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  // Startup the VM
  void *info = vmk_startup(static_cast<ESMC_VMKPlan *>(vmp), fctp, cargo,
    &localrc);
  // The return code set by vmk_startup indicates failure in pthread_create() 
  // [if pthread_create() is used -- which depends on the VMKPlan].
  // Translate vmk_startup() error code into ESMF error code.
  if (localrc){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_SYS, " - VMKernel could not "
    "create additional pthreads! Please check stack limit.", rc);
    return NULL;  // bail out if pthreads could not be created
  }

  // For new VM context take care of VMId book keeping for ESMF...
  if (!(vmp->parentVMflag) && vmp->nspawn>0){
    // Only do this if this is really a new VM context (not the parent's)
    // and the local PET spawns any child PETs.
    int i, vas, m, n;
    // TODO: make this section thread-safe for when we allow ESMF-threading!
    // TODO: the issue is that multiple threads within the same VAS may access
    // TODO: the same matchTable instance at the same time (read & write access)

    // The VMId is that same for all PETs spawned by local PET
    ESMC_VMId vmID = ESMC_VMIdCreate(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return NULL;  // bail out on error
    // vmKey part of the vmID gets set to the appropriate bit pattern:
    // ->  set the bit in vmKey for each VAS in which this VM exists <-
    for (i=0; i<vmp->myvms[0]->vmk_npets(); i++){
      // loop through all the pets in the VM
      vas = vmp->myvms[0]->vmk_vas(i);
      m = vas / 8;
      n = vas % 8;
      vmID.vmKey[m] |= 0x80>>n;  // set the bits
    }
    vmID.localID = 0;  // reset localID
    // Search through the valid entries in the matchTable,
    // consider the localIDs of all entries with the same vmKey
    // and determine a localID for the current VM that uniquely identifies it.
    // At the same time construct a list of empty entries to be used further
    // down.
    int *emptyList = new int[matchTableBound];  // can't be larger than that
    int emptyListBound = 0; // reset
    for (i=0; i<matchTableBound; i++){
      if (matchTable_vm[i]!=NULL){
        // This is a valid entry to consider
        if (ESMC_VMKeyCompare(matchTable_vmID[i].vmKey, vmID.vmKey)
          == ESMF_TRUE){
          if (matchTable_vmID[i].localID > vmID.localID)
            vmID.localID = matchTable_vmID[i].localID;
        }
      }else{
        // This is an empty entry that can be used further down
        emptyList[emptyListBound] = i;
        ++emptyListBound;
      }
    }
    ++(vmID.localID); // Make this localID unique to all other entries
    
    // Enter information for each spawned PET of this new VM into the matchTable
    for (int j=0; j<vmp->nspawn; j++){
      // Each thread spawned by this PET gets its own entry because it will
      // have a unique tid which will be used to associate the thread with 
      // this entry in the matchTable.
      int index;
      if (emptyListBound){
        // There are still empty entries in the matchTable below matchTableBound
        --emptyListBound;
        index = emptyList[emptyListBound];
      }else{
        // No more empty entries below matchTableBound -> up matchTableBound
        if (matchTableBound < ESMC_VM_MATCHTABLEMAX){
          // Still space in the matchTable
          index = matchTableBound;
          ++matchTableBound;
        }else{
          // No more space left -> fatal error!
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_MEM,
            " - VM ran out of matchTable space.", rc);
          return NULL;  // bail out on error
        }
      }
      matchTable_tid[index]  = vmp->myvms[j]->vmk_mypthid();  // pthid
      matchTable_vm[index]   = vmp->myvms[j];                 // ptr to this VM
      matchTable_vmID[index] = vmID;                          // vmID
    }
    delete [] emptyList;
  }
  
  if (rc) *rc = ESMF_SUCCESS; // Return successfully
  return info;  // Return pointer to info structure
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMShutdown()"
//BOPI
// !IROUTINE:  ESMC_VMShutdown
//
// !INTERFACE:
void ESMC_VM::ESMC_VMShutdown(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
  class ESMC_VMPlan *vmp,         // plan for this child VM
  void *info,                     // info structure
  int *rc){                       // error return code
//
// !DESCRIPTION:
//    Startup a new child VM according to plan.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  vmk_shutdown(static_cast<ESMC_VMKPlan *>(vmp), info);

  // For each locally spawned PET mark the matchTable entry invalid
  if (!(vmp->parentVMflag)){
    // This is really a separate VM context (not the parent's)
    for (int j=0; j<vmp->nspawn; j++){
      int i;
      for (i=0; i<matchTableBound; i++)
        if (matchTable_vm[i]==vmp->myvms[j]) break;
      if (i < matchTableBound){
        // found matching entry in the matchTable
        matchTable_vm[i] = NULL;  // mark this entry invalid
        ESMC_VMIdDestroy(&(matchTable_vmID[i]), &localrc);
      }else{
        // matchTable must be corrupted
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_MEMC,
          " - VM matchTable corrupted.", rc);
        return; // bail out on error
      }
    }
  }

  if (rc) *rc = ESMF_SUCCESS; // Return successfully
}
  
  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMEnter()"
//BOPI
// !IROUTINE:  ESMC_VMEnter
//
// !INTERFACE:
int ESMC_VM::ESMC_VMEnter(
//
// !RETURN VALUE:
//    Error return code
//
// !ARGUMENTS:
//
  class ESMC_VMPlan *vmp,         // plan for this child VM
  void *info,                     // info structure
  void *cargo                     // pointer to cargo structure for in/out data
  ){
//
// !DESCRIPTION:
//    Enter a child VM.
//
//EOPI
//-----------------------------------------------------------------------------
  int matchTableIndex_old;
  if(vmp->nothreadflag){
    // take care of book keeping for ESMF...
    matchTableIndex_old = matchTableIndex;
    int i;
    for (i=0; i<matchTableBound; i++)
      if (matchTable_vm[i]==vmp->myvms[0]) break;
    if(i < matchTableBound){
      // found matching entry in the matchTable
      matchTableIndex = i;
    }else{
      // TODO: this branch needs to be treated as an error because
      //       the matchTable must be corrupted if no match can be found!
    }
  }

  // enter the VM
  vmk_enter(static_cast<ESMC_VMKPlan *>(vmp), info, cargo);

  if(vmp->nothreadflag){
    // restore book keeping for ESMF...
    matchTableIndex = matchTableIndex_old;
  }
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGet()"
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
//   Get internal information about the {\tt ESMC\_VM} object.
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
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetPETLocalInfo()"
//BOPI
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
  int *threadId,      // out - thread id for specified PET
  int *vas){          // out - virtual address space of the specified PET
//
// !DESCRIPTION:
//   Get internal information about the specified PET within the {\tt ESMF\_VM}
//   object.
//
//EOPI
//-----------------------------------------------------------------------------
  if (peCount != ESMC_NULL_POINTER)
    *peCount = this->vmk_ncpet(pet);
  if (ssiId != ESMC_NULL_POINTER)
    *ssiId = this->vmk_ssiid(pet);
  if (threadCount != ESMC_NULL_POINTER)
    *threadCount = this->vmk_nthreads(pet);
  if (threadId != ESMC_NULL_POINTER)
    *threadId = this->vmk_tid(pet);
  if (vas != ESMC_NULL_POINTER)
    *vas = this->vmk_vas(pet);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetPETMatchPET()"
//BOPI
// !IROUTINE:  ESMC_VMGetPETMatchPET
//
// !INTERFACE:
int ESMC_VM::ESMC_VMGetPETMatchPET(
//
// !RETURN VALUE:
//    Error return code
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
//    Match PET in the current VM object against the PETs in the 
//    provided vmMatch VM. Return number of matched PETs and a list of the
//    matching PET id's that operate in the same virtual address space as
//    the specified PET.
//    Returns ESMF_FAILURE if {\tt petMatchList} was provided (i.e. not equal
//    to {\tt ESMC_NULL_POINTER) but {\tt len_petMatchList} < number of matching
//    PETs found. Otherwise returns ESMF_SUCCESS (even if no matching PETs were
//    found!).
//
//EOPI
//-----------------------------------------------------------------------------
  int npets = vmMatch.vmk_npets();  // maximum number of PETs in vmMatch
  int *tempMatchList = new int[npets];
  int vasCompare = pid[pet];        // this is pet's virtual address space id
  int j=0;
  for (int i=0; i<npets; i++)
    if (vmMatch.vmk_vas(i) == vasCompare){
      tempMatchList[j] = i;
      ++j;
    }
  // now j is equal to the number of PETs in vmMatch which operate in the 
  // same virtual address space as pet in the current VM
  if (petMatchCount != ESMC_NULL_POINTER)
    *petMatchCount = j;
  if (petMatchList != ESMC_NULL_POINTER){
    if (len_petMatchList >= j)
      for (int i=0; i<j; i++)
        petMatchList[i] = tempMatchList[i];
    else{
      ESMC_LogDefault.ESMC_LogWrite("provided petMatchList too small",
        ESMC_LOG_ERROR);
      return ESMF_FAILURE;
    }
  }
  delete [] tempMatchList;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetVMId()"
//BOPI
// !IROUTINE:  ESMC_VMGetVMId - Get ID of VM object
//
// !INTERFACE:
ESMC_VMId *ESMC_VM::ESMC_VMGetVMId(
//
// !RETURN VALUE:
//    ID of VM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//   Get the ID of the {\tt ESMC\_VM} object.
//
//EOPI
//-----------------------------------------------------------------------------
  *rc = ESMF_FAILURE; // assume failure
  pthread_t mytid = vmk_mypthid();
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.ESMC_LogWrite("could not determine VMId",
        ESMC_LOG_ERROR);
      return NULL;  // bail out
    }
  }
  // found a match
  *rc = ESMF_SUCCESS;
  return &matchTable_vmID[i];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMSendVMId()"
//BOPI
// !IROUTINE:  ESMC_VMSendVMId
//
// !INTERFACE:
int ESMC_VM::ESMC_VMSendVMId(
//
// !RETURN VALUE:
//    Error return code
//
// !ARGUMENTS:
//
  ESMC_VMId *vmID,              // in  - ESMC_VMId to be send
  int dest                      // in  - destination PET
  ){
//
// !DESCRIPTION:
//    Send {\tt ESMC\_VMId} to another PET.
//
//EOPI
//-----------------------------------------------------------------------------
  if (vmID==ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmID", ESMC_LOG_ERROR);
    return ESMF_FAILURE;
  }
  vmk_send(vmID->vmKey, vmKeyWidth, dest);
  vmk_send(&(vmID->localID), sizeof(int), dest);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMRecvVMId()"
//BOPI
// !IROUTINE:  ESMC_VMRecvVMId
//
// !INTERFACE:
int ESMC_VM::ESMC_VMRecvVMId(
//
// !RETURN VALUE:
//    Error return code
//
// !ARGUMENTS:
//
  ESMC_VMId *vmID,              // out - ESMC_VMId to be received
  int source                    // in  - source PET
  ){
//
// !DESCRIPTION:
//    Receive {\tt ESMC\_VMId} from another PET.
//
//EOPI
//-----------------------------------------------------------------------------
  if (vmID==ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogWrite("invalid vmID", ESMC_LOG_ERROR);
    return ESMF_FAILURE;
  }
  vmk_recv(vmID->vmKey, vmKeyWidth, source);
  vmk_recv(&(vmID->localID), sizeof(int), source);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMPrint()"
//BOP
// !IROUTINE:  ESMC_VMPrint
//
// !INTERFACE:
void ESMC_VM::ESMC_VMPrint(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
    int *rc       // return code
  ){
//
// !DESCRIPTION:
//    Print {\tt ESMC\_VM} object and its {\\tt ESMC_VMId}
//
//EOP
//-----------------------------------------------------------------------------
  int localrc=ESMC_RC_NOT_IMPL;
  printf("=== <ESMC_VMPrint> =============================\n");
  ESMC_VMId *vmid = ESMC_VMGetVMId(&localrc);
  ESMC_VMIdPrint(vmid);
  vmk_print();
  printf("=== </ESMC_VMPrint> ============================\n");
  if (rc) *rc=localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// external VM functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetArgs()"
//BOPI
// !IROUTINE:  ESMC_VMGetArgs - Get command line arguments
//
// !INTERFACE:
void ESMC_VMGetArgs(
//
// !RETURN VALUE:
//    Get command line arguments
//
// !ARGUMENTS:
//
  int *argc,      // count of command line arguments
  char ***argv,   // command line argument strings
  int *rc){       // return code
//
// !DESCRIPTION:
//   Get the command line arguments
//
//EOPI
//-----------------------------------------------------------------------------
  *rc = ESMF_FAILURE; // assume failure
  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid GlobalVM", ESMC_LOG_ERROR);
    return; // bail out
  }
  *argc = GlobalVM->argc;
  *argv = GlobalVM->argv;
  // success
  *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetGlobal()"
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
  *rc = ESMF_FAILURE; // assume failure
  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid GlobalVM", ESMC_LOG_ERROR);
    return NULL; // bail out
  }
  *rc = ESMF_SUCCESS;
  return GlobalVM;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetCurrent()"
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
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.ESMC_LogWrite("could not determine current VM",
        ESMC_LOG_ERROR);
      return NULL;  // bail out
    }
  }
  // found a match
  *rc = ESMF_SUCCESS;
  return matchTable_vm[i];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetCurrentID()"
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
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.ESMC_LogWrite("could not determine current VMId",
        ESMC_LOG_ERROR);
      return NULL;  // bail out
    }
  }
  // found a match
  *rc = ESMF_SUCCESS;
  return &matchTable_vmID[i];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMInitialize()"
//BOPI
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
  MPI_Comm mpiCommunicator,
  int *rc){   // return code
//
// !DESCRIPTION:
//    Initialize the global virtual machine as an all MPI VM and return a 
//    handle to the local instance of the GlobalVM.
//
//EOPI
//-----------------------------------------------------------------------------
  *rc = ESMF_FAILURE;         // assume failure
  GlobalVM = new ESMC_VM;
  GlobalVM->vmk_init(mpiCommunicator);  // set up default ESMC_VMK (all MPI)
  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogWrite("vmk_init returned invalid GlobalVM",
      ESMC_LOG_ERROR);
    return NULL; // bail out
  }
  
  // allocate the VM association table
//gjtNotYet  matchTable_tid = new pthread_t[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet  matchTable_vm = new ESMC_VM*[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet  matchTable_vmID = new ESMC_VMId[ESMC_VM_MATCHTABLEMAX];

  matchTableBound = 0;       // reset
  matchTable_tid[matchTableBound]  = pthread_self();
  matchTable_vm[matchTableBound]   = GlobalVM;
  
  // set vmID
  vmKeyWidth = GlobalVM->vmk_npets()/8;
  vmKeyOff   = GlobalVM->vmk_npets()%8;
  if (vmKeyOff){
    ++vmKeyWidth;               // correction for extra bits
    vmKeyOff = 8 - vmKeyOff;    // number of extra bits in last char
  }
//printf("gjt in ESMC_VMInitialize, vmKeyWidth = %d\n", vmKeyWidth);
  int localrc;
  matchTable_vmID[matchTableBound] = ESMC_VMIdCreate(&localrc);
  for (int i=0; i<vmKeyWidth; i++)
    matchTable_vmID[matchTableBound].vmKey[i] = 0xff;  // globalVM in all VASs
  matchTable_vmID[matchTableBound].vmKey[vmKeyWidth-1] =
    matchTable_vmID[matchTableBound].vmKey[vmKeyWidth-1]<<vmKeyOff; // shift
  matchTable_vmID[matchTableBound].localID = 0;        // globalVM is first

  ++matchTableBound;         // done

                             // totalview cannot handle events during the init
                             // call - it freezes or crashes or ignores input.
  GlobalVM->vmk_barrier();   // so for now, wait for everyone to init.

  *rc = ESMF_SUCCESS;         // success at last

  return GlobalVM;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMFinalize()"
//BOPI
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
  ESMC_Logical *keepMpiFlag,
  int *rc){   // return code
//
// !DESCRIPTION:
//    Finalize the global virtual machine referenced by GlobalVM
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  *rc = ESMF_FAILURE;         // assume failure
  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid GlobalVM", ESMC_LOG_ERROR);
    return; // bail out
  }
  int finalizeMpi = 1;  // set
  if (keepMpiFlag){
    if (*keepMpiFlag==ESMF_TRUE) finalizeMpi = 0; // reset
  }
  GlobalVM->vmk_finalize(finalizeMpi);
  matchTableBound = 0;
  // delete the VM association table
  ESMC_VMIdDestroy(&(matchTable_vmID[0]), &localrc);
//gjtNotYet  delete [] matchTable_tid;
//gjtNotYet  delete [] matchTable_vm;
//gjtNotYet  delete [] matchTable_vmID;
  *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMAbort()"
//BOPI
// !IROUTINE:  ESMC_VMAbort
//
// !INTERFACE:
void ESMC_VMAbort(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//    Abort the global virtual machine referenced by GlobalVM
//
//EOPI
//-----------------------------------------------------------------------------
  *rc = ESMF_FAILURE;         // assume failure
  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogWrite("invalid GlobalVM", ESMC_LOG_ERROR);
    return; // bail out
  }
  GlobalVM->vmk_abort();
  matchTableBound = 0;
  *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------
