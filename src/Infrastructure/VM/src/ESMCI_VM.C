// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_VM.C"
//==============================================================================
#define GARBAGE_COLLECTION_LOG_off
//==============================================================================
//
// VM class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the methods of two C++ classes
// {\tt ESMCI::VM} and {\tt ESMCI::VMPlan} which are defined in the companion
// file {\tt ESMCI::VM.h}.
//
// Class {\tt ESMCI::VM} is derived from base class {\tt ESMCI::VMK}
// and {\tt ESMCI::VMPlan} is derived from base class {\tt ESMCI::VMKPlan}.
// Both base classes are defined in {\tt ESMCI\_VMKernel.h} and implemented in
// source file {\tt ESMCI\_VMKernel.C}.
// There a couple of new features that the derived classes add to their
// base classes. This is the content of this source file. Furthermore, this
// source file contains the implementation of a global association list for VMs
// used in ESMF.
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_VM.h"

// include higher level, 3rd party or system headers
#include <vector>
#include <string>
#include <cstdlib>
#if (defined ESMF_OS_Linux || defined ESMF_OS_Unicos)
#include <malloc.h>
#endif
#include "ESMF_Pthread.h"
#include "ESMCI_IO_Handler.h"

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_Base.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"

using std::string;
using std::vector;


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//==============================================================================
// prototypes for Fortran interface routines called by C++ code below
extern "C" {
  void FTN_X(f_esmf_fortranudtpointercopy)(void *dst, void *src);
  void FTN_X(f_esmf_fortranudtpointercompare)(void *ptr1, void *ptr2, int *flag);
  void FTN_X(f_esmf_fieldcollectgarbage)(void *fobject, int *localrc);
  void FTN_X(f_esmf_fbundlecollectgarbage)(void *fobject, int *localrc);
  void FTN_X(f_esmf_geombasecollectgarbage)(void *fobject, int *localrc);
  void FTN_X(f_esmf_xgridgeombasecolgarbage)(void *fobject, int *localrc);
  void FTN_X(f_esmf_locstreamcollectgarbage)(void *fobject, int *localrc);
  void FTN_X(f_esmf_statecollectgarbage)(void *fobject, int *localrc);
  void FTN_X(f_esmf_compcollectgarbage1)(void *fobject, int *localrc);
  void FTN_X(f_esmf_compcollectgarbage2)(void *fobject, int *localrc);
}
//==============================================================================

namespace ESMCI {

//-----------------------------------------------------------------------------
// Module variable pointing to the global default VM
// The global VM will be initialized in call ESMC_VMInitialize() and wrapped up
// calling ESMC_VMFinalize().
static VM *GlobalVM = NULL;
//-----------------------------------------------------------------------------


struct FortranObject{
  F90ClassHolder fobject;
  int objectID;
};

//-----------------------------------------------------------------------------
// Module arrays to hold association table between tid <-> vm <-> vmID
#define ESMC_VM_MATCHTABLEMAX 10000  // maximum number of entries in table
static esmf_pthread_t matchTable_tid[ESMC_VM_MATCHTABLEMAX];
static VM *matchTable_vm[ESMC_VM_MATCHTABLEMAX];
static VMId matchTable_vmID[ESMC_VM_MATCHTABLEMAX];
static int matchTable_BaseIDCount[ESMC_VM_MATCHTABLEMAX];
static vector<ESMC_Base *> matchTable_Objects[ESMC_VM_MATCHTABLEMAX];
static vector<FortranObject> matchTable_FObjects[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet static esmf_pthread_t *matchTable_tid;
//gjtNotYet static ESMC_VM **matchTable_vm;
//gjtNotYet static VMId *matchTable_vmID;
static int vmKeyWidth = 0;      // width in units of 8-bit chars
static int vmKeyOff = 0;        // extra bits in last char (bits to be ignored)
static int matchTableBound = 0; // upper bound of currently filled entries
static int matchTableIndex = 0; // process wide index for non-thread based VMs
// ESMF runtime environment variables
static vector<string> esmfRuntimeEnv;
static vector<string> esmfRuntimeEnvValue;
// ESMF Initialized/Finalized status
static bool esmfInitialized = false;
static bool esmfFinalized = false;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// static module functions
//
//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdKeyCompare()"
static bool VMKeyCompare(unsigned char *vmKey1, unsigned char *vmKey2){
  int i;
#if 0
  std::cout << ESMC_METHOD << ": entered" << std::endl;
#endif
  for (i=0; i<vmKeyWidth; i++)
    if (vmKey1[i] != vmKey2[i]){
#if 0
      std::cout << ESMC_METHOD << "loop broke with i = " << i
        << ", vmKeyWidth = " << vmKeyWidth << std::endl;
#endif
      break;
    }
  if (i==vmKeyWidth) return true;
#if 0
  else
    std::cout << ESMC_METHOD  << ": vmKey1[" << i << "] = " << vmKey1[i]
        << " != vmKey2[" << i << "] = " << vmKey2[i] << std::endl;
#endif
  return false;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::procParseLine()"
int procParseLine(char* line){
  int i = strlen(line);
  while (*line < '0' || *line > '9') line++;
  line[i-3] = '\0';
  i = atoi(line);
  return i;
}


//-----------------------------------------------------------------------------
//
// VMId functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::create()"
//BOPI
// !IROUTINE:  ESMCI::VMId::create
//
// !INTERFACE:
int VMId::create() {
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Allocate space and initialize VMId internals.
//
//EOPI
//-----------------------------------------------------------------------------
  vmKey = new unsigned char[vmKeyWidth];
  for (int i=0; i<vmKeyWidth; i++)
    vmKey[i] = 0x00;  // zero out all bits
  localID = 0;        // reset

  // return successfully
  int rc = ESMF_SUCCESS;
  return rc;

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::destroy()"
//BOPI
// !IROUTINE:  ESMCI::VMId::destroy
//
// !INTERFACE:
int VMId::destroy() {
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Deallocate VMId internals.
//
//EOPI
//-----------------------------------------------------------------------------
  // free memory for vmKey member
  if (vmKey){
    delete [] vmKey;
    vmKey = NULL;
  }

  // return successfully
  int rc = ESMF_SUCCESS;
  return rc;

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::VMId::deserialize
//
// !INTERFACE:
int VMId::deserialize(const char *buffer, int *offset, bool offsetonly) {
//
// !RETURN VALUE:
//    int return code
//
// !DESCRIPTION:
//    Deserialize a buffer into a VMId object.  Assumes vmKey has been
//    been pre-allocated.
//
//EOPI
//-----------------------------------------------------------------------------

  char *cp;
  int *ip;

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment

  ip = (int *)(buffer + *offset);
  int keywidth = *ip++;
  if (!offsetonly)
    localID = *ip;
  ip++;
  cp = (char *)ip;
  if (!offsetonly) {
    if (vmKey) {
      memcpy (vmKey, cp, keywidth);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Null vmKey encountered when deserializing a VMId object",
          ESMC_CONTEXT, &localrc);
      return localrc;
    }
  }
  cp += keywidth;

  // update offset to point to past the current obj
  *offset = (cp - buffer);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::get()"
//BOPI
// !IROUTINE:  ESMCI::VMId::get
//
// !RETURN VALUE:
//    int return code
//
// !INTERFACE:
int VMId::get(
//
// !RETURN VALUE:
//    int return code
//
//
// !ARGUMENTS:
//
  int  *localID,
  char *key,
  int   key_len
  ){
//
// !DESCRIPTION:
//    Get the elements of a {\tt ESMC\_VMId} object.
//
//    This method is primarily intended for use by VM unit tests.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  *localID = this->localID;
  if (key_len < ESMCI::vmKeyWidth) {
    localrc = ESMC_RC_ARG_SIZE;
    return localrc;
  }
  for (int i=0; i<vmKeyWidth; i++){
    key[i] = this->vmKey[i];
  }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::set()"
//BOPI
// !IROUTINE:  ESMCI::VMId::set
//
// !INTERFACE:
int VMId::set(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int   localID,
  const char *key,
  int   key_len
  ){
//
// !DESCRIPTION:
//    Set the elements of an existing {\tt ESMC\_VMId} object.
//
//    This method is primarily intended for use by VM unit tests.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  this->localID = localID;
  if (key_len < ESMCI::vmKeyWidth) {
    localrc = ESMC_RC_ARG_SIZE;
    return localrc;
  }
  for (int i=0; i<vmKeyWidth; i++){
    this->vmKey[i] = key[i];
  }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::log()"
//BOPI
// !IROUTINE:  ESMCI::VMId::log
//
// !INTERFACE:
void VMId::log(
//
// !ARGUMENTS:
//
  std::string prefix
  )const{
//
// !DESCRIPTION:
//   Log the VMId.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  char digits[64];
  char msg[256];
  std::stringstream info;
  info << "  vmKeyWidth = " << vmKeyWidth;
  sprintf(msg, "%s - VMId: %s", prefix.c_str(), info.str().c_str());
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "  vmKey=0x";
  int bitmap=0;
  int k=0;
  for (int i=0; i<vmKeyWidth; i++){
    bitmap |= vmKey[i];
    bitmap = bitmap << 8;
    ++k;
    if (k==4){
      sprintf(digits, "%X", bitmap);
      info << digits;
      bitmap=0;
      k=0;
    }
  }
  if (k!=0){
    bitmap = bitmap << (3-k)*8;
    sprintf(digits, "%X", bitmap);
    info << digits;
  }
  sprintf(msg, "%s - VMId: %s", prefix.c_str(), info.str().c_str());
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "  localID = " << localID;
  sprintf(msg, "%s - VMId: %s", prefix.c_str(), info.str().c_str());
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::print()"
//BOPI
// !IROUTINE:  ESMCI::VMId::print
//
// !INTERFACE:
int VMId::print() const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of VMId object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // print info about the ESMCI::VM object
  std::cout << "--- ESMCI::VMId::print() start ---" << std::endl;
  std::cout << "  vmKeyWidth = " << vmKeyWidth << std::endl;
  printf("  vmKey=0x");
  int bitmap=0;
  int k=0;
  for (int i=0; i<vmKeyWidth; i++){
    bitmap |= vmKey[i];
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
  std::cout << "  localID = " << localID << std::endl;
  std::cout << "--- ESMCI::VMId::print() end ---" << std::endl;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMId::serialize()"
//BOPI
// !IROUTINE:  ESMCI::VMId::serialize
//
// !INTERFACE:
int VMId::serialize(
    const char *buffer,   // in - base address of the serialization buffer
    int *length,          // in - length of the serialization buffer
    int *offset,          // inout - offset for serialization of this object
    const ESMC_InquireFlag &inquireflag) {  // in update offset only flag
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Turn info in a VMId object into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------

  char *cp;
  int *ip;

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment

  int fixedpart = 2*sizeof (int) + vmKeyWidth;
  if (inquireflag == ESMF_INQUIREONLY) {
    *offset += fixedpart;
  } else {
    if ((*length - *offset) < fixedpart) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to serialize a VMId object",
          ESMC_CONTEXT, &localrc);
      return localrc;
    }
    ip = (int *)(buffer + *offset);
    *ip++ = vmKeyWidth;
    *ip++ = localID;
    cp = (char *) ip;
    if (vmKey == NULL) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Null vmKey when serializing a VMId object",
          ESMC_CONTEXT, &localrc);
      return localrc;
    }
    memcpy (cp, vmKey, vmKeyWidth);
    cp += vmKeyWidth;

    // update offset to point to past the current obj
    *offset = (cp - buffer);
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
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
  vmID.vmKey = new unsigned char[vmKeyWidth];
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmID", ESMC_CONTEXT, rc);
    return; // bail out
  }
  if (vmID->vmKey==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmID->vmKey", ESMC_CONTEXT, rc);
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
bool VMIdCompare(
//
// !RETURN VALUE:
//    bool indicating result of comparison.
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
#if 0
  std::cout << ESMC_METHOD << ": entered" << std::endl;
#endif
  if (vmID1==NULL || vmID2==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmIDs", ESMC_CONTEXT, NULL);
    return false;    // bail out
  }
  if (vmID1->localID != vmID2->localID){
#if 0
    std::cout << ESMC_METHOD << ": localID " << vmID1->localID << " != " << vmID2->localID << std::endl;
#endif
    return false;
  }
  bool match = VMKeyCompare(vmID1->vmKey, vmID2->vmKey);
#if 0
  if (!match) std::cout << ESMC_METHOD << ": keys don't compare" << std::endl;
#endif
  return match;
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmIDs", ESMC_CONTEXT, &rc);
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
#define ESMC_METHOD "ESMCI::VMIdGet()"
//BOPI
// !IROUTINE:  ESMCI::VMIdGet
//
// !INTERFACE:
void VMIdGet(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  VMId *vmID,
  int  *localID,
  char *key,
  int   key_len,
  int  *rc
  ){
//
// !DESCRIPTION:
//    Get the elements of a {\tt ESMC\_VMId} object.
//
//    This method is primarily intended for use by VM unit tests.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;
  *localID = vmID->localID;
  if (key_len < ESMCI::vmKeyWidth) {
    if (rc != NULL) *rc = ESMC_RC_ARG_SIZE;
    return;
  }
  for (int i=0; i<vmKeyWidth; i++){
    key[i] = vmID->vmKey[i];
  }
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdSet()"
//BOPI
// !IROUTINE:  ESMCI::VMIdSet
//
// !INTERFACE:
void VMIdSet(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  VMId *vmID,
  int   localID,
  char *key,
  int   key_len,
  int  *rc
  ){
//
// !DESCRIPTION:
//    Set the elements of an existing {\tt ESMC\_VMId} object.
//
//    This method is primarily intended for use by VM unit tests.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;
  vmID->localID = localID;
  if (key_len < ESMCI::vmKeyWidth) {
    if (rc != NULL) *rc = ESMC_RC_ARG_SIZE;
    return;
  }
  for (int i=0; i<vmKeyWidth; i++){
    vmID->vmKey[i] = key[i];
  }
  if (rc!=NULL) *rc = ESMF_SUCCESS;
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_SYS, " - VMKernel could not "
    "create additional pthreads! Please check stack limit.", ESMC_CONTEXT, rc);
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
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc))
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
        if (VMKeyCompare(matchTable_vmID[i].vmKey, vmID.vmKey)){
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
          ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM,
            " - VM ran out of matchTable space.", ESMC_CONTEXT, rc);
          return NULL;  // bail out on error
        }
      }
      matchTable_tid[index]  = vmp->myvms[j]->getMypthid(); // pthid
      matchTable_vm[index]   = vmp->myvms[j];               // ptr to this VM
      matchTable_vmID[index] = VMIdCreate(&localrc);        // vmID
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return NULL;  // bail out on error
      matchTable_BaseIDCount[index] = 0;                    // reset
      matchTable_Objects[index].reserve(1000);              // start w/ 1000 obj
      matchTable_FObjects[index].reserve(1000);             // start w/ 1000 obj
      VMIdCopy(&(matchTable_vmID[index]), &vmID);           // deep copy
    }
    delete [] emptyList;
    VMIdDestroy(&vmID, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc))
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
//    Shut down a child VM according to plan.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

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
        // destroy VMId object
        VMIdDestroy(&(matchTable_vmID[i]), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return;
        // automatic garbage collection of ESMF objects
        try{
          // The following loop deallocates deep Fortran ESMF objects
          for (int k=matchTable_FObjects[i].size()-1; k>=0; k--){
            if (matchTable_FObjects[i][k].objectID == ESMC_ID_FIELD.objectID){
              FTN_X(f_esmf_fieldcollectgarbage)
                (&(matchTable_FObjects[i][k].fobject),&localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_FIELDBUNDLE.objectID){
              FTN_X(f_esmf_fbundlecollectgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_GEOMBASE.objectID){
              FTN_X(f_esmf_geombasecollectgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_XGRIDGEOMBASE.objectID){
              FTN_X(f_esmf_xgridgeombasecolgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_LOCSTREAM.objectID){
              FTN_X(f_esmf_locstreamcollectgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_STATE.objectID){
              FTN_X(f_esmf_statecollectgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_COMPONENT.objectID){
              FTN_X(f_esmf_compcollectgarbage1)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }
          }
          // second time through the list fully shuts down components
          // the call to f_esmf_compcollectgarbage2() may be collective on some
          // MPI implementations and therefore must be done in a second loop
          // in order to allow the first loop to perform any inter component
          // wrap up communication
          for (int k=matchTable_FObjects[i].size()-1; k>=0; k--){
            if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_COMPONENT.objectID){
              FTN_X(f_esmf_compcollectgarbage2)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
            }
            matchTable_FObjects[i].pop_back();
          }
          if (matchTable_FObjects[i].size() > 0)
            std::cout << "Failure in ESMF Automatic Garbage Collection line: "
              << __LINE__ << std::endl;
          // swap() trick with a temporary to free vector's memory
          std::vector<FortranObject>().swap(matchTable_FObjects[i]);
          // The following loop deletes deep C++ ESMF objects derived from
          // Base class. For deep Fortran classes it deletes the Base member.
          for (int k=matchTable_Objects[i].size()-1; k>=0; k--){
#ifdef GARBAGE_COLLECTION_LOG_on
            std::stringstream debugmsg;
            debugmsg << "ESMF Automatic Garbage Collection: delete: "
              << matchTable_Objects[i][k]->ESMC_BaseGetClassName() << " : "
              << matchTable_Objects[i][k]->ESMC_BaseGetName();
            ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
#endif
            delete matchTable_Objects[i][k];  // delete ESMF object, incl. Base
            matchTable_Objects[i].pop_back();
          }
          if (matchTable_Objects[i].size() > 0)
            std::cout << "Failure in ESMF Automatic Garbage Collection line: "
              << __LINE__ << std::endl;
          // swap() trick with a temporary to free vector's memory
          std::vector<ESMC_Base *>().swap(matchTable_Objects[i]);
        }catch(int catchrc){
          // catch standard ESMF return code
          ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, rc);
          return;
        }catch(...){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            "- Caught exception", ESMC_CONTEXT, rc);
          return;
        }
      }else{
        // matchTable must be corrupted
        ESMC_LogDefault.MsgFoundError(ESMC_RC_MEMC,
          " - VM matchTable corrupted.", ESMC_CONTEXT, rc);
        return;
      }
    }
  }

  VMK::shutdown(static_cast<VMKPlan *>(vmp), info);

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
    if (vmp->nspawn > 0){
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
//    (i.e. not equal to {\tt ESMC\_NULL\_POINTER) but
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- Provided petMatchList too small", ESMC_CONTEXT, &rc);
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

  int i = matchTableIndex;
  for (i=0; i<matchTableBound; i++)
    if (matchTable_vm[i] == this) break;
  if (i == matchTableBound){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Could not determine VMId", ESMC_CONTEXT, rc);
    return NULL;
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
  VMId *vmID,                   // in  - VMId to be sent
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid VMId", ESMC_CONTEXT, &rc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid VMId", ESMC_CONTEXT, &rc);
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
#define ESMC_METHOD "ESMCI::VM::alltoallvVMId()"
//BOPI
// !IROUTINE:  ESMCI::VM::alltoallvVMId
//
// !INTERFACE:
int VM::alltoallvVMId(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VMId **sendvmID,              // in  - VMIds to send
  int *sendcounts,              // in  - VMId send counts
  int *sendoffsets,             // in  - send offsets
  VMId **recvvmID,              // out - VMIds to receive
  int *recvcounts,              // out - VMId receive counts
  int *recvoffsets              // out - receive offsets
  ){
//
// !DESCRIPTION:
//    All to all communication {\tt ESMCI::VMId}.  Assumes that the receive VMId
// array has been pre-allocated and initialized.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  int localrc;

  int petCount = GlobalVM->getNpets();
  int send_sum=0, recv_sum=0;
  for (int i=0; i<petCount; i++) {
    send_sum += sendcounts[i];
    recv_sum += recvcounts[i];
  }

  // communicate vmKeys
  char *send_vmkeys = new char[send_sum];
  for (int key=0; key<send_sum; key++) {
    for (int i=0; i<vmKeyWidth; i++) {
      send_vmkeys[i + key*vmKeyWidth] = sendvmID[key]->vmKey[i];
// std::cout << ESMC_METHOD << ": sendvmID[" << key << "]->vmKey[" << i;
// std::cout << "] = " << (int)sendvmID[key]->vmKey[i] << std::endl;
    }
  }
  char *recv_vmkeys = new char[recv_sum];
  memset (recv_vmkeys, 0, recv_sum);
  localrc=alltoallv(
      send_vmkeys, sendcounts, sendoffsets,
      recv_vmkeys, recvcounts, recvoffsets,
      vmBYTE);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    delete [] send_vmkeys;
    delete [] recv_vmkeys;
    return rc;
  }
  for (int key=0; key<recv_sum; key++) {
    for (int i=0; i<vmKeyWidth; i++) {
      recvvmID[key]->vmKey[i] = recv_vmkeys[i + key*vmKeyWidth];
// std::cout << ESMC_METHOD << ": recvvmID[" << key << "]->vmKey[" << i;
// std::cout << "] = " << (int)recvvmID[key]->vmKey[i] << std::endl;
    }
  }
  delete [] send_vmkeys;
  delete [] recv_vmkeys;

  // communicate localIDs
  int *send_ids = new int[send_sum];
  for (int i=0; i<send_sum; i++) {
    send_ids[i] = sendvmID[i]->localID;
  }
  int *recv_ids = new int[recv_sum];
  localrc=alltoallv(
      send_ids, sendcounts, sendoffsets,
      recv_ids, recvcounts, recvoffsets,
      vmI4);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    delete [] send_ids;
    delete [] recv_ids;
    return rc;
  }
  for (int i=0; i<recv_sum; i++) {
    recvvmID[i]->localID = recv_ids[i];
  }
  delete [] send_ids;
  delete [] recv_ids;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::allgathervVMId()"
//BOPI
// !IROUTINE:  ESMCI::VM::allgathervVMId
//
// !INTERFACE:
int VM::allgathervVMId(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VMId **sendvmid,               // in - VMIds
  int sendcount,                 // in  - VMId count
  VMId **recvvmid,               // out - VMIds
  int *recvcounts,               // in  - VMId count
  int *recvoffsets               // in  - VMId offsets
  ){
//
// !DESCRIPTION:
//    AllGatherV {\tt ESMCI::VMId}.  Assumes that the receive VMId
// array has been pre-allocated and initialized.
//
//EOPI
//-----------------------------------------------------------------------------
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  if (sendvmid==ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid VMId", ESMC_CONTEXT, &rc);
    return rc;
  }
  for (int i=0; i<sendcount; i++) {
    if (sendvmid[i]==ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Invalid VMId", ESMC_CONTEXT, &rc);
      return rc;
    }
  }
  int petCount = GlobalVM->getNpets();
  int mypet    = GlobalVM->getMypet();
  if (sendcount != recvcounts[mypet]){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- non-matching send/recv count", ESMC_CONTEXT, &rc);
    return rc;
  }

  // TODO: Convert this to a real AllGatherV

  // Each PET copies its send data into its receive area, then broadcast
  for (int i=0; i<recvcounts[mypet]; i++) {
    int localrc = VMIdCopy (recvvmid[recvoffsets[mypet]+i], sendvmid[i]);
    if (localrc != ESMF_SUCCESS){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Bad VMIdCopy", ESMC_CONTEXT, &rc);
      return rc;
    }
  }
  for (int root=0; root<petCount; root++) {
    bcastVMId(recvvmid+recvoffsets[root], recvcounts[root], root);
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::bcastVMId()"
//BOPI
// !IROUTINE:  ESMCI::VM::bcastVMId
//
// !INTERFACE:
int VM::bcastVMId(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VMId **vmID,                  // in/out - VMId
  int count,                    // in  - VMId count
  int root                      // in  - root PET
  ){
//
// !DESCRIPTION:
//    Broadcast {\tt ESMCI::VMId}.  Assumes that the receive VMId
// array has been pre-allocated and initialized.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  for (int i=0; i<count; i++) {
    if (vmID[i]==ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Invalid VMId", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // broadcast vmKeys
  char *local_vmkeys = new char[count*vmKeyWidth];
  for (int key=0; key<count; key++) {
    for (int i=0; i<vmKeyWidth; i++) {
      local_vmkeys[i + key*vmKeyWidth] = vmID[key]->vmKey[i];
    }
  }
  broadcast(local_vmkeys, count*vmKeyWidth, root);
  for (int key=0; key<count; key++) {
    for (int i=0; i<vmKeyWidth; i++) {
      vmID[key]->vmKey[i] = local_vmkeys[i + key*vmKeyWidth];
    }
  }
  delete[] local_vmkeys;

  // broadcast localIDs
  int *local_ids = new int[count];
  for (int i=0; i<count; i++) {
    local_ids[i] = vmID[i]->localID;
  }
  broadcast(local_ids, count*sizeof(int), root);
  for (int i=0; i<count; i++) {
    vmID[i]->localID = local_ids[i];
  }
  delete[] local_ids;

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

  // print info about the ESMCI::VM object
  printf("--- ESMCI::VM::print() start ---\n");
  VMId *vmid = getVMId(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  vmid->print();
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

  //TODO: complete this method

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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", ESMC_CONTEXT, rc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", ESMC_CONTEXT, rc);
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

  esmf_pthread_t mytid;
#ifndef ESMF_NO_PTHREADS
  mytid = pthread_self();
#else
  mytid = 0;
#endif
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", ESMC_CONTEXT, rc);
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

  esmf_pthread_t mytid;
#ifndef ESMF_NO_PTHREADS
  mytid = pthread_self();
#else
  mytid = 0;
#endif

  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", ESMC_CONTEXT, rc);
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
#define ESMC_METHOD "ESMCI::VM::getCurrentGarbageInfo()"
//BOPI
// !IROUTINE:  ESMCI::VM::getCurrentGarbageInfo - Get garbage info for Current VM
//
// !INTERFACE:
void VM::getCurrentGarbageInfo(
//
// !ARGUMENTS:
//
  int *fobjCount,     // number of Fortran objects registered
  int *objCount){     // total number of objects registered (Fortran + C++)
//
// !DESCRIPTION:
//   Get the garbage info of the current context.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  esmf_pthread_t mytid;
#ifndef ESMF_NO_PTHREADS
  mytid = pthread_self();
#else
  mytid = 0;
#endif
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", ESMC_CONTEXT, &rc);
      throw rc;
    }
  }
  // found a match

  *fobjCount = matchTable_FObjects[i].size();
  *objCount = matchTable_Objects[i].size();

  // return successfully
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::logCurrentGarbageInfo()"
//BOPI
// !IROUTINE:  ESMCI::VM::logCurrentGarbageInfo - Log garbage info of current VM
//
// !INTERFACE:
void VM::logCurrentGarbageInfo(
//
// !ARGUMENTS:
//
  std::string prefix
  ){
//
// !DESCRIPTION:
//   Log the garbage collection information of the current context.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  esmf_pthread_t mytid;
#ifndef ESMF_NO_PTHREADS
  mytid = pthread_self();
#else
  mytid = 0;
#endif
  int i = matchTableIndex;
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", ESMC_CONTEXT, &rc);
      throw rc;
    }
  }
  // found a match

  char msg[512];
  sprintf(msg, "%s - CurrGarbInfo: Fortran objs=%lu", prefix.c_str(),
    matchTable_FObjects[i].size());
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  for (unsigned j=0; j<matchTable_FObjects[i].size(); j++){
    sprintf(msg, "%s - CurrGarbInfo: fortran objs[%d]: %d", prefix.c_str(), j,
      matchTable_FObjects[i][j].objectID);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  }
  sprintf(msg, "%s - CurrGarbInfo: Base objs=%lu", prefix.c_str(),
    matchTable_Objects[i].size());
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  for (unsigned j=0; j<matchTable_Objects[i].size(); j++){
    const char *proxyString;
    proxyString="actual object";
    if (matchTable_Objects[i][j]->ESMC_BaseGetProxyFlag()==ESMF_PROXYYES)
      proxyString="proxy object";
    sprintf(msg, "%s - CurrGarbInfo: base objs[%d]: %p : %s : %s : %d ; %s",
      prefix.c_str(), j, matchTable_Objects[i][j],
      matchTable_Objects[i][j]->ESMC_BaseGetClassName(),
      matchTable_Objects[i][j]->ESMC_BaseGetName(),
      matchTable_Objects[i][j]->ESMC_BaseGetID(), proxyString);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  }

  // return successfully
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getMemInfo()"
//BOPI
// !IROUTINE:  ESMCI::VM::getMemInfo - Get memory info
//
// !INTERFACE:
void VM::getMemInfo(
//
// !ARGUMENTS:
//
  int *virtMemPet,    // virtual memory used by this PET in KB
  int *physMemPet     // physical memory used by this PET in KB
  ){
//
// !DESCRIPTION:
//   Get the memory information of the local PET.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  *virtMemPet = *physMemPet = -1; // initialize

#if (defined ESMF_OS_Linux || defined ESMF_OS_Unicos)
  // must lock/unlock for thread-safety
  VM *vm = getCurrent();
  vm->lock();
  FILE* file = fopen("/proc/self/status", "r");
  char line[128];
  while (fgets(line, 128, file) != NULL){
    if (strncmp(line, "VmSize:", 7) == 0){
      *virtMemPet = procParseLine(line);
      if (*physMemPet!=-1) break;
    }
    if (strncmp(line, "VmRSS:", 6) == 0){
      *physMemPet = procParseLine(line);
      if (*virtMemPet!=-1) break;
    }
  }
  fclose(file);
  vm->unlock();
#endif

  // return successfully
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::logMemInfo()"
//BOPI
// !IROUTINE:  ESMCI::VM::logMemInfo - Log memory info
//
// !INTERFACE:
void VM::logMemInfo(
//
// !ARGUMENTS:
//
  std::string prefix,
  ESMCI::LogErr *log
  ){
//
// !DESCRIPTION:
//   Log the memory information of the local PET.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

#if (defined ESMF_OS_Linux || defined ESMF_OS_Unicos)
  // must lock/unlock for thread-safety
  VM *vm = getCurrent();
  vm->lock();
  // access /proc/self
  FILE* file = fopen("/proc/self/status", "r");
  char line[128];
  char msg[256];
  while (fgets(line, 128, file) != NULL){
    if (strncmp(line, "Vm", 2) == 0){
      int len = strlen(line);
      line[len-1] = '\0'; // replace the newline with null
      sprintf(msg, "%s - MemInfo: \t%s", prefix.c_str(), line);
      log->Write(msg, ESMC_LOGMSG_INFO);
    }
  }
  fclose(file);
  // access mallinfo
  std::stringstream info;
  struct mallinfo m = mallinfo();
  info << "Non-mmapped space allocated (bytes):       \t" << m.arena;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "Space allocated in mmapped regions (bytes):\t" << m.hblkhd;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "Maximum total allocated space (bytes):     \t" << m.usmblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "Space in freed fastbin blocks (bytes):     \t" << m.fsmblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "Total allocated space (bytes):             \t" << m.uordblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "Total free space (bytes):                  \t" << m.fordblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  info.str(""); // clear info
  info << "Top-most, releasable space (bytes):        \t" << m.keepcost;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  long total = 0; // init
  if (m.hblkhd>=0 && m.uordblks>=0){
    total = (long)m.hblkhd+(long)m.uordblks;
    total /= (long)1024;  // scale to KiB
  }
  info.str(""); // clear info
  info << "Total space in use, mmap + non-mmap (KiB): \t" << total;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  // access through malloc_stats()
  FILE *stderrOrig = stderr;
  char *buf;
  size_t len;
  stderr = open_memstream(&buf, &len);
  malloc_stats();
  fflush(stderr);
  std::string malloc_stats_output;
  if (buf){
    malloc_stats_output = string(buf, buf+len);
    free(buf);
  }
  stderr = stderrOrig;
  size_t pos = malloc_stats_output.rfind("system bytes     =");
  pos += 18;
  long system = strtol(malloc_stats_output.c_str()+pos, NULL, 10);
  system /= (long)1024;  // scale to KiB
  info.str(""); // clear info
  info << "Total space held (mmap + non-mmap) (KiB):  \t" << system;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  pos = malloc_stats_output.rfind("in use bytes     =");
  pos += 18;
  long in_use = strtol(malloc_stats_output.c_str()+pos, NULL, 10);
  in_use /= (long)1024;  // scale to KiB
  info.str(""); // clear info
  info << "Total space used (mmap + non-mmap) (KiB):  \t" << in_use;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  // output the wtime since execution start
  double wt;
  ESMCI::VMK::wtime(&wt);
  info.str(""); // clear info
  info << "Wall-clock time since execution start (s): \t" << wt;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, ESMC_LOGMSG_INFO);
  // unlock again
  vm->unlock();
#endif

  // return successfully
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getBaseIDAndInc()"
//BOPI
// !IROUTINE:  ESMCI::VM::getBaseIDAndInc - Get BaseID and increment counter
//
// !INTERFACE:
int VM::getBaseIDAndInc(
//
// !RETURN VALUE:
//    current BaseID count
//
// !ARGUMENTS:
//
  VMId *vmID){   // identifying vmID
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int i;
  for (i=0; i<matchTableBound; i++)
    if (VMIdCompare(vmID, &(matchTable_vmID[i]))) break;
  if (i == matchTableBound)
    return -1;  // no match found -> return invalid count

  // match found
  int count = matchTable_BaseIDCount[i];
  matchTable_BaseIDCount[i] = count + 1;  // increment
  return count; // return count before increment
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::addObject()"
//BOPI
// !IROUTINE:  ESMCI::VM::addObject - Add object to table for garbage collection
//
// !INTERFACE:
void VM::addObject(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//
  ESMC_Base *object,
  VMId *vmID){   // identifying vmID
//
// !DESCRIPTION:
//    Add object to matchTable_Objects list for this VM. Objects in this
//    list will be delete during VM shutdown and finalize. This implements
//    automatic garbage collection of ESMF objects on the Component scope.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  int i;
  for (i=0; i<matchTableBound; i++)
    if (VMIdCompare(vmID, &(matchTable_vmID[i]))) break;
  if (i == matchTableBound){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Could not find table entry for current VM", ESMC_CONTEXT, &rc);
    throw rc;
  }

  // match found, proceed

  // must lock/unlock for thread-safe access to std::vector
  VM *vm = getCurrent();
  vm->lock();
  matchTable_Objects[i].push_back(object);
  vm->unlock();
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::rmObject()"
//BOPI
// !IROUTINE:  ESMCI::VM::rmObject - Remove object from table for garbage collection
//
// !INTERFACE:
void VM::rmObject(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//
  ESMC_Base *object){   // object to be removed
//
// !DESCRIPTION:
//    Remove object from matchTable_Objects list for current VM.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  // find current VM index
  esmf_pthread_t mytid;
#ifndef ESMF_NO_PTHREADS
  mytid = pthread_self();
#else
  mytid = 0;
#endif
  int i = matchTableIndex;  // correct index if non-threaded VM
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", ESMC_CONTEXT, &rc);
      throw rc;
    }
  }

  // found a match
  // proceed to remove object from this VM's garbage collection table

  // must lock/unlock for thread-safe access to std::vector
  VM *vm = getCurrent();
  vm->lock();
  for (vector<ESMC_Base *>::iterator
    it = matchTable_Objects[i].begin();
    it != matchTable_Objects[i].end(); ++it){
    if (*it == object){
      matchTable_Objects[i].erase(it);  // erase the object entry
      break;
    }
  }
  vm->unlock();
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::addFObject()"
//BOPI
// !IROUTINE:  ESMCI::VM::addFObject - Add Fortran object to table for garb col.
//
// !INTERFACE:
void VM::addFObject(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//
  void **fobject,
  int objectID,
  VMId *vmID){   // identifying vmID
//
// !DESCRIPTION:
//    Add Fortran object to matchTable_FObjects list for this VM. Objects in
//    list will be delete during VM shutdown and finalize. This implements
//    automatic garbage collection of ESMF objects on the Component scope.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  int i;
  for (i=0; i<matchTableBound; i++)
    if (VMIdCompare(vmID, &(matchTable_vmID[i]))) break;
  if (i == matchTableBound){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Could not find table entry for current VM", ESMC_CONTEXT, &rc);
    throw rc;
  }

  // match found

  // must lock/unlock for thread-safe access to std::vector
  VM *vm = getCurrent();
  vm->lock();
  int size = matchTable_FObjects[i].size();
  matchTable_FObjects[i].resize(size+1);  // add element to FObjects list
  void *fobjectElement = (void *)&(matchTable_FObjects[i][size].fobject);

  FTN_X(f_esmf_fortranudtpointercopy)(fobjectElement, (void *)fobject);

  matchTable_FObjects[i][size].objectID = objectID;
  vm->unlock();
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getObject()"
//BOPI
// !IROUTINE:  ESMCI::VM::getObject - Find and return an ESMF object.
//
// !INTERFACE:
void VM::getObject(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//
  void **fobject,     // out - alias to object
  int objectID,       // in - identifying ID
  VMId *vmID,         // in - identifying vmID
  const string &name, // in - identifying object name
  ESMC_ProxyFlag proxyflag,  // in - proxy/non-proxy flag
  bool *object_found, // out - true if found, false if not
  int *rc) {
//
// !DESCRIPTION:
//    Find and return a object in matchTable_FObjects list for a
//    given ID/vmId.
//
//    If proxyflag is ESMF_PROXYYES, only match proxies.  Likewise, if
//    ESMF_PROXYNO, only match non-proxies.  ESMF_PROXYANY matches any.
//
//EOPI
//-----------------------------------------------------------------------------

  const bool debug = false;

  // initialize return code; assume routine not implemented
  *rc = ESMC_RC_NOT_IMPL;   // final return code

  switch (proxyflag) {
    case ESMF_PROXYYES:
    case ESMF_PROXYNO:
    case ESMF_PROXYANY:
      break;
    default: {
      *rc = ESMC_RC_ARG_BAD;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "- Bad proxyflag value", ESMC_CONTEXT, rc);
      return;
    }
  }

  if (debug) {
    std::stringstream msg;
    msg << "looking for object ID: " << objectID;
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
  }
  *fobject = NULL;          // assume not found
  *object_found = false;

  // find VMId

  bool vmid_found = false;
  int i;
  for (i=0; i<matchTableBound; i++) {
    if (VMIdCompare(vmID, &matchTable_vmID[i])) {
      vmid_found = true;
      break;
    }
  }
  if (debug) {
    std::stringstream msg;
    msg << "VMid " << (vmid_found ? "":"not ") << "found";
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
  }
  if (!vmid_found){
    *rc = ESMF_SUCCESS;
    return;
  }

  // Search for and validate ID

  // must lock/unlock for thread-safe access to std::vector
  ESMC_Base *fobject_temp;
  bool id_found = false;
  VM *vm = getCurrent();
  vm->lock();
  for (unsigned it=0; it<matchTable_Objects[i].size(); ++it){

    fobject_temp = matchTable_Objects[i][it];
    int ID = (fobject_temp)->ESMC_BaseGetID();
    if (debug) {
      std::stringstream msg;
      msg << "comparing ID " << ID << " to object ID " << objectID;
      ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    }
    if (ID != objectID)
      continue;


    if (debug)
      ESMC_LogDefault.Write("validating baseStatus and Status", ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    ESMC_Status baseStatus = (fobject_temp)->ESMC_BaseGetBaseStatus();
    ESMC_Status     Status = (fobject_temp)->ESMC_BaseGetStatus();
    if ((baseStatus != ESMF_STATUS_READY) || (Status != ESMF_STATUS_READY))
      continue;

    ESMC_ProxyFlag fobject_proxy = (fobject_temp)->ESMC_BaseGetProxyFlag();
    if (debug) {
      std::stringstream msg;
      msg << "validating proxyflag " << proxyflag << " to " <<
          fobject_proxy;
      ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    }
    switch (proxyflag) {
      case ESMF_PROXYYES: {
        id_found = fobject_proxy == ESMF_PROXYYES;
        if (debug)
          ESMC_LogDefault.Write("PROXYYES", ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        break;
      }
      case ESMF_PROXYNO: {
        id_found = fobject_proxy == ESMF_PROXYNO;
        if (debug)
          ESMC_LogDefault.Write("PROXYNO", ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        break;
      }
      case ESMF_PROXYANY:
        id_found = true;
    }
    if (debug) {
      std::stringstream msg;
      msg << "object " << name << (id_found ? " ":" not ") << "validated";
      ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    }
    if (id_found) break;
  }  // end of ID search loop

  // Compare name

  // TODO: In theory, VMId/ID should be sufficient to distinguish an object and the objects
  // name shouldn't be needed.  However some tests (e.g., in ESMF_TransferGridSTest) have
  // shown that further qualification is needed.  This needs to be investigated.

  if (id_found) {
    char *fobject_name = (fobject_temp)->ESMC_BaseGetName();
    if (debug) {
      std::stringstream msg;
      msg << "comparing requested name: " << name << " to: " <<
          fobject_name;
      ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    }
    if (name == fobject_name) {
      *fobject = fobject_temp;
      // TODO: Bump Base refCount?  Gerhard says not yet.
      *object_found = true;
    }
    if (debug) {
      std::stringstream msg;
      msg << "object " << name << (*object_found ? " ":" not ") << "found";
      ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    }
  }

  vm->unlock();
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::rmFObject()"
//BOPI
// !IROUTINE:  ESMCI::VM::rmFObject - Remove Fortran object from table for garbage collection
//
// !INTERFACE:
void VM::rmFObject(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//
  void **fobject){   // object to be removed
//
// !DESCRIPTION:
//    Remove Fortran object from matchTable_Objects list for current VM.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  // find current VM index
  esmf_pthread_t mytid;
#ifndef ESMF_NO_PTHREADS
  mytid = pthread_self();
#else
  mytid = 0;
#endif
  int i = matchTableIndex;  // correct index if non-threaded VM
  if (matchTable_tid[i] != mytid){
    for (i=0; i<matchTableBound; i++)
      if (matchTable_tid[i] == mytid) break;
    if (i == matchTableBound){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Could not determine current VM", ESMC_CONTEXT, &rc);
      throw rc;
    }
  }

  // found a match
  // proceed to remove object from this VM's garbage collection table

  // must lock/unlock for thread-safe access to std::vector
  VM *vm = getCurrent();
  vm->lock();
  for (vector<FortranObject>::iterator
    it = matchTable_FObjects[i].begin();
    it != matchTable_FObjects[i].end(); ++it){

    void *fobjectElement = (void *)&(it->fobject);

    int flag;
    FTN_X(f_esmf_fortranudtpointercompare)(fobjectElement, (void *)fobject, &flag);

    if (flag){
      matchTable_FObjects[i].erase(it);  // erase the object entry
      break;
    }
  }
  vm->unlock();
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::printMatchTable()"
//BOPI
// !IROUTINE:  ESMCI::VM::printMatchTable - Print the current match table
//
// !INTERFACE:
void VM::printMatchTable(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//
  void){
//
// !DESCRIPTION:
//    Print the current match table.
//
//EOPI
//-----------------------------------------------------------------------------
  printf("--- ESMCI::VM::printMatchTable() start ---\n");
  printf("matchTableBound = %d\n", matchTableBound);
  for (int i=0; i<matchTableBound; i++) {
    printf("matchTable_tid[%d] = %lu\n", i, matchTable_tid[i]);
    (&matchTable_vmID[i])->print();
  }
  printf("--- ESMCI::VM::printMatchTable() end ---\n");
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::getenv()"
//BOPI
// !IROUTINE:  ESMCI::VM::getenv - get environment variable
// !INTERFACE:
char const *VM::getenv(
//
// !RETURN VALUE:
//    pointer to value or NULL
//
// !ARGUMENTS:
//
  char const *name){
//
// !DESCRIPTION:
//    Access environment variables in the global VM object
//
//EOPI
//-----------------------------------------------------------------------------
  int count = esmfRuntimeEnv.size();
  int i;
  for (i=0; i<count; i++)
    if (!esmfRuntimeEnv[i].compare(name)) break;
  if (i == count)
    return NULL;  // no match found, bail out

  // match found
  return esmfRuntimeEnvValue[i].c_str();
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- GlobalVM allocation failure", ESMC_CONTEXT, rc);
    return NULL;
  }
  GlobalVM->VMK::init(mpiCommunicator);  // set up default VMK (all MPI)

  // allocate the VM association table
//gjtNotYet  matchTable_tid = new esmf_pthread_t[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet  matchTable_vm = new ESMCI::VM*[ESMC_VM_MATCHTABLEMAX];
//gjtNotYet  matchTable_vmID = new ESMCI::VMId[ESMC_VM_MATCHTABLEMAX];

  matchTableBound = 0;       // reset
#ifndef ESMF_NO_PTHREADS
  matchTable_tid[matchTableBound]  = pthread_self();
#else
  matchTable_tid[matchTableBound]  = 0;
#endif
  matchTable_vm[matchTableBound]   = GlobalVM;
  matchTable_BaseIDCount[matchTableBound] = 0;        // reset
  matchTable_Objects[matchTableBound].reserve(1000);  // start w/ 1000 obj
  matchTable_FObjects[matchTableBound].reserve(1000); // start w/ 1000 obj

  // obtain ESMF runtime environment
  if (GlobalVM->getLocalPet() == 0){
    char const *esmfRuntimeVarName = "ESMF_RUNTIME_COMPLIANCECHECK";
    char const *esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_COMPLIANCEICOBJECT";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_COMPLIANCEICREGISTER";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_TRACE";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_TRACE_PETLIST";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_TRACE_COMPONENT";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_TRACE_CLOCK";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_TRACE_FLUSH";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_PROFILE";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_PROFILE_PETLIST";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }
    esmfRuntimeVarName = "ESMF_RUNTIME_PROFILE_OUTPUT";
    esmfRuntimeVarValue = std::getenv(esmfRuntimeVarName);
    if (esmfRuntimeVarValue){
      esmfRuntimeEnv.push_back(esmfRuntimeVarName);
      esmfRuntimeEnvValue.push_back(esmfRuntimeVarValue);
    }

    int count = esmfRuntimeEnv.size();
    GlobalVM->broadcast(&count, sizeof(int), 0);
    int *length = new int[2];
    for (int i=0; i<count; i++){
      length[0] = esmfRuntimeEnv[i].length();
      length[1] = esmfRuntimeEnvValue[i].length();
      GlobalVM->broadcast(length, 2*sizeof(int), 0);
      GlobalVM->broadcast((void *)esmfRuntimeEnv[i].c_str(),
        length[0]*sizeof(char), 0);
      GlobalVM->broadcast((void *)esmfRuntimeEnvValue[i].c_str(),
        length[1]*sizeof(char), 0);
    }
    delete [] length;
  }else{
    int count;
    GlobalVM->broadcast(&count, sizeof(int), 0);
    int *length = new int[2];
    for (int i=0; i<count; i++){
      GlobalVM->broadcast(length, 2*sizeof(int), 0);
      char *temp = new char[length[0]+1];
      GlobalVM->broadcast((void *)temp, length[0]*sizeof(char), 0);
      temp[length[0]] = '\0'; // terminate C style string
      esmfRuntimeEnv.push_back(temp);
      delete [] temp;
      temp = new char[length[1]+1];
      GlobalVM->broadcast((void *)temp, length[1]*sizeof(char), 0);
      temp[length[1]] = '\0'; // terminate C style string
      esmfRuntimeEnvValue.push_back(temp);
      delete [] temp;
    }
    delete [] length;
  }

  // set vmID
  vmKeyWidth = GlobalVM->getNpets()/8;
  vmKeyOff   = GlobalVM->getNpets()%8;
  if (vmKeyOff){
    ++vmKeyWidth;               // correction for extra bits
    vmKeyOff = 8 - vmKeyOff;    // number of extra bits in last char
  }
#ifdef DEBUG
  printf("ESMC_VMInitialize, vmKeyWidth=%d vmKeyOff=%d\n", vmKeyWidth, vmKeyOff);
#endif
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

  // set the global initialized state
  esmfInitialized = true;

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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", ESMC_CONTEXT, rc);
    return;
  }

  // delete the VM association table
  VMIdDestroy(&(matchTable_vmID[0]), &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc))
    return;
  // automatic garbage collection of ESMF objects
  try{
    // We need to make sure any open files and streams are closed.
    // Also, resources such as cached I/O communication patterns are deleted.
    IO_Handler::finalize(&localrc);
    if (localrc != ESMF_SUCCESS)
      std::cout << "IO_Handler::finalize returned " << localrc << std::endl;
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) {
      return;
    }
    // The following loop deallocates deep Fortran ESMF objects
    for (int k=matchTable_FObjects[0].size()-1; k>=0; k--){
      if (matchTable_FObjects[0][k].objectID == ESMC_ID_FIELD.objectID){
        FTN_X(f_esmf_fieldcollectgarbage)(&(matchTable_FObjects[0][k].fobject),
          &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return;
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_FIELDBUNDLE.objectID){
        FTN_X(f_esmf_fbundlecollectgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_GEOMBASE.objectID){
        FTN_X(f_esmf_geombasecollectgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_XGRIDGEOMBASE.objectID){
        FTN_X(f_esmf_xgridgeombasecolgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_LOCSTREAM.objectID){
        FTN_X(f_esmf_locstreamcollectgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_STATE.objectID){
        FTN_X(f_esmf_statecollectgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_COMPONENT.objectID){
        FTN_X(f_esmf_compcollectgarbage1)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
      }
    }
    // second time through the list fully shuts down components
    // the call to f_esmf_compcollectgarbage2() may be collective on some
    // MPI implementations and therefore must be done in a second loop
    // in order to allow the first loop to perform any inter component
    // wrap up communication
    for (int k=matchTable_FObjects[0].size()-1; k>=0; k--){
      if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_COMPONENT.objectID){
        FTN_X(f_esmf_compcollectgarbage2)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
      }
      matchTable_FObjects[0].pop_back();
    }
    if (matchTable_FObjects[0].size() > 0)
      std::cout << "Failure in ESMF Automatic Garbage Collection line: "
        << __LINE__ << std::endl;
    // swap() trick with a temporary to free vector's memory
    std::vector<FortranObject>().swap(matchTable_FObjects[0]);
    // The following loop deletes deep C++ ESMF objects derived from
    // Base class. For deep Fortran classes it deletes the Base member.
    for (int k=matchTable_Objects[0].size()-1; k>=0; k--){
#ifdef GARBAGE_COLLECTION_LOG_on
      std::stringstream debugmsg;
      debugmsg << "ESMF Automatic Garbage Collection: delete: "
        << matchTable_Objects[0][k]->ESMC_BaseGetClassName() << " : "
        << matchTable_Objects[0][k]->ESMC_BaseGetName();
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
#endif
      delete matchTable_Objects[0][k];  // delete ESMF object, incl. Base
      matchTable_Objects[0].pop_back();
    }
    if (matchTable_Objects[0].size() > 0)
      std::cout << "Failure in ESMF Automatic Garbage Collection line: "
        << __LINE__ << std::endl;
    // swap() trick with a temporary to free vector's memory
    std::vector<ESMC_Base *>().swap(matchTable_Objects[0]);
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc);
    return;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }

  // clean-up matchTable
  matchTableBound = 0;

//gjtNotYet  delete [] matchTable_tid;
//gjtNotYet  delete [] matchTable_vm;
//gjtNotYet  delete [] matchTable_vmID;

  int finalizeMpi = 1;  // set
  if (keepMpiFlag){
    if (*keepMpiFlag==ESMF_TRUE) finalizeMpi = 0; // reset
  }
  GlobalVM->VMK::finalize(finalizeMpi);
  delete GlobalVM;
  GlobalVM=NULL;

  // set the global finalized state
  esmfFinalized = true;

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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Invalid GlobalVM", ESMC_CONTEXT, rc);
    return;
  }
  GlobalVM->VMK::abort();
  matchTableBound = 0;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::isInitialized()"
//BOPI
// !IROUTINE:  ESMCI::VM::isInitialized
//
// !INTERFACE:
bool VM::isInitialized(
//
// !RETURN VALUE:
//    true/false indicating initialized status
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//    Query ESMF initialized status.
//
//EOPI
//-----------------------------------------------------------------------------
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return esmfInitialized;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::isFinalized()"
//BOPI
// !IROUTINE:  ESMCI::VM::isFinalized
//
// !INTERFACE:
bool VM::isFinalized(
//
// !RETURN VALUE:
//    true/false indicating finalized status
//
// !ARGUMENTS:
//
  int *rc){   // return code
//
// !DESCRIPTION:
//    Query ESMF finalized status.
//
//EOPI
//-----------------------------------------------------------------------------
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return esmfFinalized;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::timerLog()"
//BOPI
// !IROUTINE:  ESMCI::VM::timerLog
//
// !INTERFACE:
void VM::timerLog(
//
// !RETURN VALUE:
//    void
//
// !ARGUMENTS:
//
  std::string timer){
//
// !DESCRIPTION:
//    Log the timer information to the default log
//
//EOPI
//-----------------------------------------------------------------------------
  std::stringstream timerMsg;
  std::map<std::string, VMTimer>::iterator t = timers.find(timer);
  timerMsg << "Timer '" << timer << "' accumulated time: "
    << t->second.taccu << " seconds in " << t->second.iters << " iterations.";
  ESMC_LogDefault.Write(timerMsg.str(), ESMC_LOGMSG_INFO);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::MPIError()"
//BOPI
// !IROUTINE:  ESMCI::VM::MPIError - Check for MPI error and log if true
//
// !INTERFACE:
bool VM::MPIError(
//
// !ARGUMENTS:
//
  int mpiErrorToCheck,
  int LINE, 
  const std::string &FILE,
  const std::string &method,
  int *rcToReturn){
//
// !DESCRIPTION:
//   Check for MPI error and log if an error is found
//
//EOPI
//-----------------------------------------------------------------------------
  if (mpiErrorToCheck != MPI_SUCCESS){
    char mpierr[MPI_MAX_ERROR_STRING];
    int resultlen;
    MPI_Error_string(mpiErrorToCheck, mpierr, &resultlen);
    char msg[20+resultlen];
    sprintf(msg, "Caught MPI error: %s", mpierr);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, msg,
      LINE, FILE, method, rcToReturn);
    return true;
  }
  return false;
}
//-----------------------------------------------------------------------------

} // namespace ESMCI
