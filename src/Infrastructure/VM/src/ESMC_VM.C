// $Id: ESMC_VM.C,v 1.55.2.4 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_VM.C"
//==============================================================================
//
// ESMC VM method implementation (body) file
//
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

// include associated header file
#include "ESMC_VM.h"

// include higher level, 3rd party or system headers
#include "ESMF_Pthread.h"

// include ESMF headers
#include "ESMC_Start.h"
#include "ESMC_Base.h" 

// LogErr
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_VM.C,v 1.55.2.4 2009/01/21 21:25:24 cdeluca Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
// Module variable pointing to the global default VM
// The global VM will be initialized in call ESMC_VMInitialize() and wrapped up
// calling ESMC_VMFinalize(). 
static VM *GlobalVM = NULL;  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Module arrays to hold association table between tid <-> vm <-> vmID
#define ESMC_VM_MATCHTABLEMAX 10000  // maximum number of entries in table
static pthread_t matchTable_tid[ESMC_VM_MATCHTABLEMAX];
static VM *matchTable_vm[ESMC_VM_MATCHTABLEMAX];
static VMId matchTable_vmID[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet static pthread_t *matchTable_tid;
//gjtNotYet static ESMC_VM **matchTable_vm;
//gjtNotYet static VMId *matchTable_vmID;
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

static ESMC_Logical VMKeyCompare(char *vmKey1, char *vmKey2){
  int i;
  for (i=0; i<vmKeyWidth; i++)
    if (vmKey1[i] != vmKey2[i]) break;
  if (i==vmKeyWidth) return ESMF_TRUE;
  return ESMF_FALSE;
}


//-----------------------------------------------------------------------------
//
// VMId functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdCreate()"
//BOPI
// !IROUTINE:  ESMCI::VMIdCreate
//
// !INTERFACE:
VMId VMIdCreate(
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
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // allocates memory for vmKey member
  VMId vmID;    // temporary stack variable
  vmID.vmKey = new char[vmKeyWidth];
  for (int i=0; i<vmKeyWidth; i++)
    vmID.vmKey[i] = 0x00;  // zero out all bits
  vmID.localID = 0;   // reset

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return vmID;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdDestroy()"
//BOPI
// !IROUTINE:  ESMCI::VMIdDestroy
//
// !INTERFACE:
void VMIdDestroy(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  VMId *vmID, 
  int *rc
  ){
//
// !DESCRIPTION:
//    Free memory for a previously created {\tt ESMC\_VMId} object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  if (vmID==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmID", rc);
    return; // bail out
  }
  if (vmID->vmKey==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmID->vmKey", rc);
    return; // bail out
  }
  // free memory for vmKey member
  if (vmID->vmKey){
    delete [] vmID->vmKey;
    vmID->vmKey = NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdCompare()"
//BOPI
// !IROUTINE:  ESMCI::VMIdCompare
//
// !INTERFACE:
ESMC_Logical VMIdCompare(
//
// !RETURN VALUE:
//    {\tt ESMC\_Logical} indicating result of comparison. 
//
// !ARGUMENTS:
//
  VMId *vmID1, 
  VMId *vmID2
  ){
//
// !DESCRIPTION:
//    Compare two {\tt ESMC\_VMId} objects.
//
//EOPI
//-----------------------------------------------------------------------------
  if (vmID1==NULL || vmID2==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmIDs", NULL);
    return ESMF_FALSE;    // bail out
  }
  if (vmID1->localID != vmID2->localID) return ESMF_FALSE;
  return VMKeyCompare(vmID1->vmKey, vmID2->vmKey);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdCopy()"
//BOPI
// !IROUTINE:  ESMCI::VMIdCopy
//
// !INTERFACE:
int VMIdCopy(
//
// !RETURN VALUE:
//    int return code 
//
// !ARGUMENTS:
//
  VMId *vmIDdst, 
  VMId *vmIDsrc
  ){
//
// !DESCRIPTION:
//    Copy {\tt ESMC\_VMId} object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (vmIDdst==NULL || vmIDsrc==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmIDs", &rc);
    return rc;    // bail out
  }
  for (int i=0; i<vmKeyWidth; i++)
    vmIDdst->vmKey[i] = vmIDsrc->vmKey[i];
  vmIDdst->localID = vmIDsrc->localID;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdPrint()"
//BOPI
// !IROUTINE:  ESMCI::VMIdPrint
//
// !INTERFACE:
void VMIdPrint(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  VMId *vmID
  ){
//
// !DESCRIPTION:
//    Print an {\tt ESMC\_VMId} object.
//
//EOPI
//-----------------------------------------------------------------------------
  printf("ESMCI::VMIdPrint:\n");
  if (vmID==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmID", NULL);
    return; // bail out
  }
  printf("vmID located at: %p\n", vmID);
  if (vmID->vmKey==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmID->vmKey", NULL);
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
// public VM methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::startup()"
//BOPI
// !IROUTINE:  ESMCI::VM::startup
//
// !INTERFACE:
void *VM::startup(
//
// !RETURN VALUE:
//    void * to info structure
//
// !ARGUMENTS:
//
  class VMPlan *vmp,              // plan for this child VM
  void *(fctp)(void *, void *),   // function pointer to 1st stage callback
  void *cargo,                    // pointer to cargo structure for in/out data
  int *rc){                       // return code
//
// !DESCRIPTION:
//    Startup a new child VM according to plan.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // Startup the VM
  void *info = VMK::startup(static_cast<VMKPlan *>(vmp), fctp, cargo, &localrc);
  // The return code set by startup() indicates failure in pthread_create() 
  // [if pthread_create() is used -- which depends on the VMKPlan].
  // Translate startup() error code into ESMF error code.
  if (localrc){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_SYS, " - VMKernel could not "
    "create additional pthreads! Please check stack limit.", rc);
    return NULL;  // bail out if pthreads could not be created
  }

  // For new VM context take care of VMId book keeping for ESMF...
  if (!(vmp->parentVMflag) && vmp->nspawn>0){
    // Only do this if this is really a new VM context (not the parent's)
    // and the local PET spawns any child PETs.

    //TODO: Make this section thread-safe for when we allow creation of child
    //TODO: components out of ESMF-multithreading parent components!
    //TODO: The issue is that multiple threads within the same VAS may access
    //TODO: the same matchTable instance at the same time (read & write access)
    //TODO: There is no problem with the current implementation creating
    //TODO: a multi-threaded child component out of a single-threaded
    //TODO: parent component.

    // The VMId is that same for all PETs spawned by local PET
    VMId vmID = VMIdCreate(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return NULL;  // bail out on error
    // vmKey part of the vmID gets set to the appropriate bit pattern:
    // ->  set the bit in vmKey for each VAS in which this VM exists <-
    for (int i=0; i<vmp->myvms[0]->getNpets(); i++){
      // loop through all the pets in the VM
      int vas = vmp->myvms[0]->getVas(i);
      int m = vas / 8;
      int n = vas % 8;
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
    for (int i=0; i<matchTableBound; i++){
      if (matchTable_vm[i]!=NULL){
        // This is a valid entry to consider
        if (VMKeyCompare(matchTable_vmID[i].vmKey, vmID.vmKey) == ESMF_TRUE){
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
      matchTable_tid[index]  = vmp->myvms[j]->getMypthid();   // pthid
      matchTable_vm[index]   = vmp->myvms[j];                 // ptr to this VM
      matchTable_vmID[index] = VMIdCreate(&localrc);          // vmID
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
        return NULL;  // bail out on error
      VMIdCopy(&(matchTable_vmID[index]), &vmID);             // deep copy
    }
    delete [] emptyList;
    VMIdDestroy(&vmID, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return NULL;  // bail out on error
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return info;  // Return pointer to info structure
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::shutdown()"
//BOPI
// !IROUTINE:  ESMCI::VM::shutdown
//
// !INTERFACE:
void VM::shutdown(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
  class VMPlan *vmp,              // plan for this child VM
  void *info,                     // info structure
  int *rc){                       // return code
//
// !DESCRIPTION:
//    Startup a new child VM according to plan.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  VMK::shutdown(static_cast<VMKPlan *>(vmp), info);

  // For each locally spawned PET mark the matchTable entry invalid
  if (!(vmp->parentVMflag)){
    // This is really a separate VM context (not the parent's)
    
    //TODO: Make this section thread-safe for when we allow creation of child
    //TODO: components out of ESMF-multithreading parent components!
    //TODO: The issue is that multiple threads within the same VAS may access
    //TODO: the same matchTable instance at the same time (read & write access)
    //TODO: There is no problem with the current implementation creating
    //TODO: a multi-threaded child component out of a single-threaded
    //TODO: parent component.
    
    for (int j=0; j<vmp->nspawn; j++){
      int i;
      for (i=0; i<matchTableBound; i++)
        if (matchTable_vm[i]==vmp->myvms[j]) break;
      if (i < matchTableBound){
        // found matching entry in the matchTable
        matchTable_vm[i] = NULL;  // mark this entry invalid
        VMIdDestroy(&(matchTable_vmID[i]), &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          rc)) return;
      }else{
        // matchTable must be corrupted
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_MEMC,
          " - VM matchTable corrupted.", rc);
        return;
      }
    }
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
  
  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::enter()"
//BOPI
// !IROUTINE:  ESMCI::VM::enter
//
// !INTERFACE:
int VM::enter(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  class VMPlan *vmp,              // plan for this child VM
  void *info,                     // info structure
  void *cargo                     // pointer to cargo structure for in/out data
  ){
//
// !DESCRIPTION:
//    Enter a child VM.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

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

  // enter the VMK
  VMK::enter(static_cast<VMKPlan *>(vmp), info, cargo);

  if(vmp->nothreadflag){
    // restore book keeping for ESMF...
    matchTableIndex = matchTableIndex_old;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getPETMatchPET()"
//BOPI
// !IROUTINE:  ESMCI::VM::getPETMatchPET
//
// !INTERFACE:
int VM::getPETMatchPET(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int pet,                      // in  - id of specified PET
  VM &vmMatch,                  // in  - vm to match against
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
//    Returns ESMC_RC_ARG_SIZE if {\tt petMatchList} was provided 
//    (i.e. not equal to {\tt ESMC_NULL_POINTER) but 
//    {\tt len_petMatchList} < number of matching PETs found.
//    Otherwise returns ESMF_SUCCESS (even if no matching PETs were found!).
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int npets = vmMatch.getNpets();  // maximum number of PETs in vmMatch
  int *tempMatchList = new int[npets];
  int vasCompare = pid[pet];        // this is pet's virtual address space id
  int j=0;
  for (int i=0; i<npets; i++)
    if (vmMatch.getVas(i) == vasCompare){
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- Provided petMatchList too small", &rc);
      return rc;
    }
  }
  delete [] tempMatchList;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getVMId()"
//BOPI
// !IROUTINE:  ESMCI::VM::getVMId - Get ID of VM object
//
// !INTERFACE:
VMId *VM::getVMId(
//
// !RETURN VALUE:
//    ID of VM
//
// !ARGUMENTS:
//
  int *rc) const{   // return code
//
// !DESCRIPTION:
//   Get the ID of the {\tt ESMC\_VM} object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  pthread_t mytid = getLocalPthreadId();
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine VMId", rc);
      return NULL;
    }
  }
  // found a match
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &matchTable_vmID[i];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::sendVMId()"
//BOPI
// !IROUTINE:  ESMCI::VM::sendVMId
//
// !INTERFACE:
int VM::sendVMId(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VMId *vmID,                   // in  - VMId to be send
  int dest                      // in  - destination PET
  ){
//
// !DESCRIPTION:
//    Send {\tt ESMCI::VMId} to another PET.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (vmID==ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid VMId", &rc);
    return rc;
  }
  send(vmID->vmKey, vmKeyWidth, dest);
  send(&(vmID->localID), sizeof(int), dest);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::recvVMId()"
//BOPI
// !IROUTINE:  ESMCI::VM::recvVMId
//
// !INTERFACE:
int VM::recvVMId(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VMId *vmID,                   // out - VMId to be received
  int source                    // in  - source PET
  ){
//
// !DESCRIPTION:
//    Receive {\tt ESMCI::VMId} from another PET.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (vmID==ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid VMId", &rc);
    return rc;
  }
  recv(vmID->vmKey, vmKeyWidth, source);
  recv(&(vmID->localID), sizeof(int), source);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::print()"
//BOPI
// !IROUTINE:  ESMCI::VM::print
//
// !INTERFACE:
int VM::print() const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of VM object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      " - 'this' pointer is NULL.", &rc);
    return rc;
  }
  // print info about the ESMCI::VM object
  printf("--- ESMCI::VM::print() start ---\n");
  VMId *vmid = getVMId(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
    &rc)) return rc;
  VMIdPrint(vmid);
  VMK::print();
  printf("--- ESMCI::VM::print() end ---\n");

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::validate()"
//BOPI
// !IROUTINE:  ESMCI::VM::validate
//
// !INTERFACE:
int VM::validate()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Validate details of VM object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check against NULL pointer
  if (this == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      " - 'this' pointer is NULL.", &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// static VM methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getArgs()"
//BOPI
// !IROUTINE:  ESMCI::VM::getArgs - Get command line arguments
//
// !INTERFACE:
void VM::getArgs(
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
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", rc);
    return;
  }
  *argc = GlobalVM->argc;
  *argv = GlobalVM->argv;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getGlobal()"
//BOPI
// !IROUTINE:  ESMCI::VM::getGlobal - Get Global VM
//
// !INTERFACE:
VM *VM::getGlobal(
//
// !RETURN VALUE:
//    Pointer to global VM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//   Get the global default {\tt ESMCI::VM} object. This is the {\tt ESMCI::VM}
//   object that was created during {\tt ESMC\_Initialize()} and is the ultimate
//   parent of all {\tt ESMCI::VM} objects in an ESMF application.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", rc);
    return NULL;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return GlobalVM;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getCurrent()"
//BOPI
// !IROUTINE:  ESMCI::VM::getCurrent - Get Current VM
//
// !INTERFACE:
VM *VM::getCurrent(
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
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  pthread_t mytid = pthread_self();
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", rc);
      return NULL;
    }
  }
  // found a match
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return matchTable_vm[i];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getCurrentID()"
//BOPI
// !IROUTINE:  ESMCI::VM::getCurrentID - Get ID of current VM
//
// !INTERFACE:
VMId *VM::getCurrentID(
//
// !RETURN VALUE:
//    ID of current VM
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//   Get the ID of the current {\tt ESMCI::VM} object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  pthread_t mytid = pthread_self();
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", rc);
      return NULL;
    }
  }
  // found a match

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &matchTable_vmID[i];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::initialize()"
//BOPI
// !IROUTINE:  ESMCI::VM::initialize
//
// !INTERFACE:
VM *VM::initialize(
//
// !RETURN VALUE:
//    VM * to GlobalVM
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
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  GlobalVM = new VM;
  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- GlobalVM allocation failure", rc);
    return NULL;
  }
  GlobalVM->VMK::init(mpiCommunicator);  // set up default VMK (all MPI)
  
  // allocate the VM association table
//gjtNotYet  matchTable_tid = new pthread_t[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet  matchTable_vm = new ESMCI::VM*[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet  matchTable_vmID = new ESMCI::VMId[ESMC_VM_MATCHTABLEMAX];

  matchTableBound = 0;       // reset
  matchTable_tid[matchTableBound]  = pthread_self();
  matchTable_vm[matchTableBound]   = GlobalVM;
  
  // set vmID
  vmKeyWidth = GlobalVM->getNpets()/8;
  vmKeyOff   = GlobalVM->getNpets()%8;
  if (vmKeyOff){
    ++vmKeyWidth;               // correction for extra bits
    vmKeyOff = 8 - vmKeyOff;    // number of extra bits in last char
  }
//printf("gjt in ESMC_VMInitialize, vmKeyWidth = %d\n", vmKeyWidth);
  int localrc;
  matchTable_vmID[matchTableBound] = ESMCI::VMIdCreate(&localrc);
  for (int i=0; i<vmKeyWidth; i++)
    matchTable_vmID[matchTableBound].vmKey[i] = 0xff;  // globalVM in all VASs
  matchTable_vmID[matchTableBound].vmKey[vmKeyWidth-1] =
    matchTable_vmID[matchTableBound].vmKey[vmKeyWidth-1]<<vmKeyOff; // shift
  matchTable_vmID[matchTableBound].localID = 0;        // globalVM is first

  ++matchTableBound;    // done

                        // totalview cannot handle events during the init
                        // call - it freezes or crashes or ignores input.
  GlobalVM->barrier();  // so for now, wait for everyone to init.

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return GlobalVM;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::finalize()"
//BOPI
// !IROUTINE:  ESMCI::VM::finalize
//
// !INTERFACE:
void VM::finalize(
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
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", rc);
    return;
  }
  int finalizeMpi = 1;  // set
  if (keepMpiFlag){
    if (*keepMpiFlag==ESMF_TRUE) finalizeMpi = 0; // reset
  }
  GlobalVM->VMK::finalize(finalizeMpi);
  delete GlobalVM;
  GlobalVM=NULL;

  // clean-up matchTable
  matchTableBound = 0;
  // delete the VM association table
  VMIdDestroy(&(matchTable_vmID[0]), &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return;
//gjtNotYet  delete [] matchTable_tid;
//gjtNotYet  delete [] matchTable_vm;
//gjtNotYet  delete [] matchTable_vmID;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::abort()"
//BOPI
// !IROUTINE:  ESMCI::VM::abort
//
// !INTERFACE:
void VM::abort(
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
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  if (GlobalVM==NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", rc);
    return;
  }
  GlobalVM->VMK::abort();
  matchTableBound = 0;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

} // namespace ESMCI
