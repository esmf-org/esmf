// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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
#define TRANSLATE_VMID_LOG_off
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
#include <map>
#include <vector>
#include <string>
#include <cstdlib>
#if (defined ESMF_OS_Linux || defined ESMF_OS_Unicos)
#include <malloc.h>
#include <execinfo.h>
#endif
#if (defined ESMF_OS_Darwin)
#include <mach/mach.h>
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
using std::map;


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
#define ESMC_METHOD "ESMCI::VMKeyCompare()"
static bool VMKeyCompare(unsigned char *vmKey1, unsigned char *vmKey2){
  int i;
  for (i=0; i<vmKeyWidth; i++)
    if (vmKey1[i] != vmKey2[i]){
      break;
    }
  if (i==vmKeyWidth) return true;
  return false;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMKeyLessThan()"
static bool VMKeyLessThan(unsigned char *vmKey1, unsigned char *vmKey2){
  // notice that left most VAS bits set leads to less than condition
  // this is so that where this is used in a sort, the desired order is 
  // achieved
  int i;
  for (i=0; i<vmKeyWidth; i++)
    if (vmKey1[i] != vmKey2[i]){
      break;
    }
  if (i==vmKeyWidth) return false;  // case of equality
  // now compare the first byte where a difference was found
  if ((int)vmKey1[i] > (int)vmKey2[i]) return true;  // left most bits set in vmKey1, not vmKey2
  return false;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMKeyFirstBitFromLeft()"
static unsigned VMKeyFirstBitFromLeft(unsigned char *vmKey){
  // a value returned of vmKeyWidth * 8 indicates that no bit was set
  int i;
  for (i=0; i<vmKeyWidth; i++)
    if (vmKey[i] > 0){
      break;
    }
  if (i==vmKeyWidth) return (unsigned) vmKeyWidth * 8; // indicating no bit set
  unsigned offSet = (unsigned)i * 8;
  unsigned char testByte = 128; // first bit from left set
  unsigned index;
  unsigned upper = 8;
  if (i==vmKeyWidth-1) upper -= vmKeyOff;
  for (index=0; index<upper; index++){
    if (vmKey[i] & testByte) break;
    testByte = testByte >> 1;  // shift the set bit one to the right
  }
  return offSet+index;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMKeyVasList()"
static void VMKeyVasList(unsigned char *vmKey, vector<unsigned> &vasList){
  vasList.clear();  // clear the vector
  for (int i=0; i<vmKeyWidth; i++){
    if (vmKey[i] > 0){
      // there is at least one bit set in this byte
      unsigned char testByte = 128; // first bit from left set
      unsigned upper = 8;
      if (i==vmKeyWidth-1) upper -= vmKeyOff;
      for (unsigned index=0; index<upper; index++){
#if 0
{
  std::stringstream msg;
  msg << "VMKeyVasList i="<<i<<" index="<<index<<" vmKey[i]="<<(int)vmKey[i]
    <<" testByte="<<(int)testByte<<" (vmKey[i] & testByte)="<< (vmKey[i] & testByte);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
}
#endif    
        if (vmKey[i] & testByte){
          // index byte is set
          vasList.push_back(i*8+index);
        }
        testByte = testByte >> 1;  // shift the set bit one to the right
      }
    }
  }
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
  if (!offsetonly)
    localID = *ip;
  ip++;
  cp = (char *)ip;
  if (!offsetonly) {
    if (vmKey) {
      memcpy (vmKey, cp, vmKeyWidth);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Null vmKey encountered when deserializing a VMId object",
          ESMC_CONTEXT, &localrc);
      return localrc;
    }
  }
  cp += vmKeyWidth;

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
  if (key_len < vmKeyWidth) {
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
  if (key_len < vmKeyWidth) {
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
  std::string prefix,
  ESMC_LogMsgType_Flag msgType
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
  info << "  vmKeyWidth (bytes) = " << vmKeyWidth
    <<" vmKeyOff (invalid bits end of last byte) = " << vmKeyOff;
  sprintf(msg, "%s - VMId: %s", prefix.c_str(), info.str().c_str());
  ESMC_LogDefault.Write(msg, msgType);
  info.str(""); // clear info
  info << "  vmKey=0x";
  int bitmap=0;
  int k=0;
  for (int i=0; i<vmKeyWidth; i++){
    bitmap |= vmKey[i];
    ++k;
    if (k<4){
      bitmap = bitmap << 8;
    }else{
      sprintf(digits, "%08X", bitmap);
      info << digits;
      bitmap=0;
      k=0;
    }
  }
  if (k!=0){
    bitmap = bitmap << (3-k)*8;
    sprintf(digits, "%08X", bitmap);
    info << digits;
  }
  sprintf(msg, "%s - VMId: %s", prefix.c_str(), info.str().c_str());
  ESMC_LogDefault.Write(msg, msgType);
  info.str(""); // clear info
  info << "  localID = " << localID;
  sprintf(msg, "%s - VMId: %s", prefix.c_str(), info.str().c_str());
  ESMC_LogDefault.Write(msg, msgType);
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
    ++k;
    if (k<4){
      bitmap = bitmap << 8;
    }else{
      printf("%08X", bitmap);
      bitmap=0;
      k=0;
    }
  }
  if (k!=0){
    bitmap = bitmap << (3-k)*8;
    printf("%08X\n", bitmap);
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

  int fixedpart = sizeof(int) + vmKeyWidth;
  if (inquireflag != ESMF_INQUIREONLY){
    if ((*length - *offset) < fixedpart) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "Buffer too short to serialize a VMId object",
        ESMC_CONTEXT, &localrc);
      return localrc;
    }
  }
  
  ip = (int *)(buffer + *offset);
  if (inquireflag != ESMF_INQUIREONLY)
    *ip++ = localID;
  else
    ip += 1;
  
  cp = (char *) ip;
  if (inquireflag != ESMF_INQUIREONLY){
    if (vmKey == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "Null vmKey when serializing a VMId object",
      ESMC_CONTEXT, &localrc);
      return localrc;
    }
    memcpy (cp, vmKey, vmKeyWidth);
  }
  cp += vmKeyWidth;

  // update offset to point to past the current obj
  *offset = (cp - buffer);

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
  const VMId *vmID1,
  const VMId *vmID2
  ){
//
// !DESCRIPTION:
//    Compare two {\tt ESMC\_VMId} objects.
//
//EOPI
//-----------------------------------------------------------------------------
  if (vmID1==NULL || vmID2==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmIDs", ESMC_CONTEXT, NULL);
    return false;    // bail out
  }
  if (vmID1->localID != vmID2->localID){
    return false;
  }
  return VMKeyCompare(vmID1->vmKey, vmID2->vmKey);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VMIdLessThan()"
//BOPI
// !IROUTINE:  ESMCI::VMIdLessThan
//
// !INTERFACE:
bool VMIdLessThan(
//
// !RETURN VALUE:
//    bool indicating vmID1 < vmID2
//
// !ARGUMENTS:
//
  const VMId *vmID1,
  const VMId *vmID2
  ){
//
// !DESCRIPTION:
//    Compare two {\tt ESMC\_VMId} objects with less than.
//
//EOPI
//-----------------------------------------------------------------------------
  if (vmID1==NULL || vmID2==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Invalid vmIDs", ESMC_CONTEXT, NULL);
    return false;    // bail out
  }
  if (VMKeyCompare(vmID1->vmKey, vmID2->vmKey)){
    // keys identical, look at localID part
    return (vmID1->localID < vmID2->localID);
  }
  return VMKeyLessThan(vmID1->vmKey, vmID2->vmKey);
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
#ifdef GARBAGE_COLLECTION_LOG_on
            std::stringstream debugmsg;
            debugmsg << "ESMF Automatic Garbage Collection: FObject delete: "
              << *(void **)&(matchTable_FObjects[i][k].fobject);
            ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
#endif
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
#if 0
//gjt: Disable cleaning up GeomBase from the garbage collection level because
//gjt: each Field actually owns its own instance of the GeomBase. The way
//gjt: FieldDestruct() currently works, it looks at the GeomBase, and if that
//gjt: were already destroyed, it causes issues. In the long run this might
//gjt: be better implemented via a protection against accessing GeomBase from
//gjt: FieldDestruct() in that case. But then it is important that all of the
//gjt: Geom types correctly implement their destructor. Right now the Mesh
//gjt: destructur is not doing all that is needed, and so the Mesh cleanup
//ght: actually does depend on the Field garbage collection.
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_GEOMBASE.objectID){
              FTN_X(f_esmf_geombasecollectgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
#endif
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_XGRIDGEOMBASE.objectID){
              FTN_X(f_esmf_xgridgeombasecolgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
#if 0
//gjt: Disable cleaning up LocStream from the garbage collection level because
//gjt: it leads to issues when a field tries to destroy it later.
//gjt: This is going to be tricky to resolve with Fortran implemented classes.
//gjt: For now rather have small memory leaks than invalid memory access.
            }else if (matchTable_FObjects[i][k].objectID ==
              ESMC_ID_LOCSTREAM.objectID){
              FTN_X(f_esmf_locstreamcollectgarbage)(
                &(matchTable_FObjects[i][k].fobject), &localrc);
              if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                ESMC_CONTEXT, rc))
                return;
#endif
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
              << matchTable_Objects[i][k]->ESMC_BaseGetName() << " : "
              << matchTable_Objects[i][k];
            ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
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

  int petCount = getPetCount();
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
  int petCount = getPetCount();
  int localPet = getLocalPet();
  if (sendcount != recvcounts[localPet]){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- non-matching send/recv count", ESMC_CONTEXT, &rc);
    return rc;
  }

  // TODO: Convert this to a real AllGatherV

  // Each PET copies its send data into its receive area, then broadcast
  for (int i=0; i<recvcounts[localPet]; i++) {
    int localrc = VMIdCopy(recvvmid[recvoffsets[localPet]+i], sendvmid[i]);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
  }
  for (int root=0; root<petCount; root++) {
    int localrc = bcastVMId(recvvmid+recvoffsets[root], recvcounts[root], root);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
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
#define ESMC_METHOD "ESMCI::VM::translateVMId()"
//BOPI
// !IROUTINE:  ESMCI::VM::translateVMId
//
// !INTERFACE:
int VM::translateVMId(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VMId **vmIDs,                       // in  - VMId
  ESMCI::InterArray<int> *ids,        // out - ids
  ESMCI::InterArray<int> *rootVmIds,  // out - indices of vmIds local PET is root
  int *rootVmIdCount                  // out - number of vmIds local PET is root
  ){
//
// !DESCRIPTION:
//    Construct globally unique integer ids for the {\tt ESMCI::VMId} elements.
// The number of elements in the vmIDs, and ids arrays must be identical. There
// is no check of this condition possible on this level, and it is an assumption
// made to be true!
//    The rootVmIds object is created and filled with indices pointing into the
// local vmIDs array for those entries for which the local PET is root.
//    The algorithm makes the following assumptions about the vmIDs passed into
// the PETs across the current VM:
//  1. Each VMId object must be present on all the PETs of the current VM that
//     are associated with the VAS bits set in the VMId key.
//  2. VMId objects that have VAS bits set outside what is covered by the PETs
//     of the current VM are supported.
//  3. VMId objects must NOT be present on any PET associated with a VAS for
//     which the corresponding VMId key bit is not set. (E.g. proxy objects
//     would bring in such VMIds.)
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  if (ids->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
      "ids must enter with one dimension",
      ESMC_CONTEXT, &rc);
    return rc;
  }
  
  int elementCount = ids->extent[0];
  int *idsArray = ids->array;
  int petCount = getPetCount();
  int localPet = getLocalPet();
  MPI_Comm mpiComm = getMpi_c();
  MPI_Group mpiGroup;
  MPI_Comm_group(mpiComm, &mpiGroup);
  
  *rootVmIdCount = 0; // initialize
  int *rootVmIdsArray = rootVmIds->array;

  if (elementCount > 0){
    // there is work to be done...

    struct Helper1{
      VMId *vmID;
      int index;
      int id;
      bool operator==(const Helper1 &cmp)const{
        return VMIdCompare(vmID,cmp.vmID);
      }
      bool operator<(const Helper1 &cmp)const{
        return VMIdLessThan(vmID,cmp.vmID);
      }
      bool vmKeyEqual(const Helper1 &cmp)const{
        return VMKeyCompare(vmID->vmKey,cmp.vmID->vmKey);
      }
      void getVasList(vector<unsigned> &vasList)const{
        VMKeyVasList(vmID->vmKey, vasList);
      }
    };
    
    // set up helper1 vector and sort
    vector<Helper1> helper1(elementCount);
    for (int i=0; i<elementCount; i++){
      helper1[i].vmID = vmIDs[i];
      helper1[i].index = i;
    }
    sort(helper1.begin(), helper1.end());

#ifdef TRANSLATE_VMID_LOG_on
    // development log
    for (unsigned i=0; i<helper1.size(); i++){
      std::stringstream prefix;
      prefix << "sorted helper1[" << i << "]";
      helper1[i].vmID->log(prefix.str());
    }
#endif

    // record the unique helper1 index in idsArray for later back reference
    int ii = 0; // init
    idsArray[helper1[0].index] = ii; // spin up
    for (unsigned i=1; i<helper1.size(); i++){
      if (!(helper1[i] == helper1[i-1])) ++ii;
      idsArray[helper1[i].index] = ii;
    }
    
    // cut out duplicate elements from sorted helper1 vector
    helper1.erase(unique(helper1.begin(),helper1.end()),helper1.end());

#ifdef TRANSLATE_VMID_LOG_on
    // development log
    for (unsigned i=0; i<helper1.size(); i++){
      std::stringstream prefix;
      prefix << "unique helper1[" << i << "]";
      helper1[i].vmID->log(prefix.str());
    }
#endif

    struct Helper2{
      unsigned indexH1; // index into helper1 vector
      unsigned count;   // number of different localIDs within the same vmKey
      MPI_Comm subComm; // communicator across participating PETs as per vmKey
      int rootPet;      // the root PET for this vmKey within current VM
      int subRootPet;   // the root PET for this vmKey within subComm
      Helper2(int index){
        indexH1 = index;
        count = 1;
        subComm = MPI_COMM_NULL;
        rootPet = -1;
        subRootPet = -1;
      }
    };
    
    // setup helper2 to only reference elements in helper1 with unique vmKey
    vector<Helper2> helper2;
    helper2.reserve(helper1.size());  // maximum possible size
    helper2.push_back(Helper2(0));
    for (unsigned i=1; i<helper1.size(); i++){
      if (helper1[i].vmKeyEqual(helper1[i-1])){
        // same vmKey -> count this to previous helper2 element
        helper2.back().count++;
      }else{
        // new vmKey -> add a new helper2 element
        helper2.push_back(Helper2(i));
      }
    }

#ifdef TRANSLATE_VMID_LOG_on
    // development log
    for (unsigned i=0; i<helper2.size(); i++){
      std::stringstream msg;
      msg << "helper2[" << i << "] - indexH1=" << helper2[i].indexH1
        << " - count=" << helper2[i].count;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

    map<unsigned,unsigned> vasToPetMap;
    // The global VAS index is equal to the rank in the MPI_COMM_WORLD. It also
    // corresponds to the bits in order of the vmKey. This is mapped against the
    // local PET index.
    for (int i=0; i<petCount; i++)
      vasToPetMap[getVas(i)] = i;

#ifdef TRANSLATE_VMID_LOG_on
    // development log
    map<unsigned,unsigned>::iterator it;
    for (it=vasToPetMap.begin(); it!=vasToPetMap.end(); ++it){
      std::stringstream msg;
      msg << "vasToPetMap - vas=" << it->first
        << " - pet=" << it->second;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

    // determine rootPet for each entry in helper2, sum up totalLocalIds, and
    // create mpiComm to handle the respective vmKey PET subspace
    int totalLocalIds = 0; // init
    for (unsigned i=0; i<helper2.size(); i++){
      vector<unsigned> vasList;
      helper1[helper2[i].indexH1].getVasList(vasList);
#ifdef TRANSLATE_VMID_LOG_on
      // development log
      for (unsigned k=0; k<vasList.size(); k++){
        std::stringstream msg;
        msg << "vasList[" << k << "]=" << vasList[k];
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
#endif
      vector<int> petList;
      petList.reserve(vasList.size());  // only edge cases might have less PETs
      unsigned kk = 0;
      for (unsigned k=0; k<vasList.size(); k++){
        if (vasToPetMap.find(vasList[k]) != vasToPetMap.end()){
          // the VAS exists in the current VM
          if (kk==0){
            // first active PET that is found handling a VAS becomes rootPet
            helper2[i].rootPet = vasToPetMap[vasList[k]];
            if (helper2[i].rootPet == localPet){
              totalLocalIds += helper2[i].count;
            }
          }
          // add to petList
          petList.push_back(vasToPetMap[vasList[k]]);
          if (petList[kk]==helper2[i].rootPet)
            helper2[i].subRootPet=kk;
#ifdef TRANSLATE_VMID_LOG_on
          // development log
          std::stringstream msg;
          msg << "petList["<<kk<<"]=" << petList[kk]
            << " vasList[k=" << k << "]=" << vasList[k];
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          // end development log
#endif
          ++kk;
        }
      }
      MPI_Group subGroup;
      MPI_Group_incl(mpiGroup, petList.size(), &(petList[0]), &subGroup);
      MPI_Comm_create_group(mpiComm, subGroup, 99, &(helper2[i].subComm));
      if (helper2[i].subComm == MPI_COMM_NULL){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "This VMId's key does not have this PET's VAS bit set. Unsupported!",
          ESMC_CONTEXT, &rc);
          return rc;
      }
      // clean-up
      MPI_Group_free(&subGroup);
    }

    // deal with rootVmIds
    *rootVmIdCount = totalLocalIds; // return this value to caller
    int j=0;
    for (unsigned i=0; i<helper2.size(); i++){
      if (helper2[i].rootPet == localPet){
        // rootPet for this id  -> fill into rootVmIdsArray
        for (unsigned k=0; k<helper2[i].count; k++){
          rootVmIdsArray[j++]=helper1[helper2[i].indexH1+k].index;
        }
      }
    }

#ifdef TRANSLATE_VMID_LOG_on
    // development log
    for (unsigned i=0; i<helper2.size(); i++){
      std::stringstream msg;
      msg << "helper2[" << i << "] - indexH1=" << helper2[i].indexH1
        << " - count=" << helper2[i].count 
        << " - rootPet=" << helper2[i].rootPet;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
    {
      std::stringstream msg;
      msg << "totalLocalIds=" << totalLocalIds;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

    // AllGather() the totalLocalIds
    vector<int> totalLocalIdsList(petCount);
    allgather(&totalLocalIds, &(totalLocalIdsList[0]), sizeof(int));

#ifdef TRANSLATE_VMID_LOG_on
    // development log
    for (unsigned i=0; i<totalLocalIdsList.size(); i++){
      std::stringstream msg;
      msg << "totalLocalIdsList[" << i << "]=" << totalLocalIdsList[i];
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

    // determine beginning value of localId
    unsigned localId = 0;
    for (int i=0; i<localPet; i++){
      localId += totalLocalIdsList[i];
    }

#ifdef TRANSLATE_VMID_LOG_on
    // development log
    {
      std::stringstream msg;
      msg << "localId=" << localId;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

    // determine globally unique integer indices for all the entries in helper2
    for (unsigned i=0; i<helper2.size(); i++){
      unsigned localIdTemp;
      if (helper2[i].rootPet == localPet){
        // rootPet for this id
        localIdTemp = localId;
        localId += helper2[i].count;
      }
      // broadcast localIdTemp from root to all other participating PETs
      MPI_Bcast(&localIdTemp, sizeof(unsigned), MPI_BYTE, helper2[i].subRootPet,
        helper2[i].subComm);
      // all PETs can fill in the globally unique integer id
      for (unsigned k=0; k<helper2[i].count; k++){
        helper1[helper2[i].indexH1+k].id = localIdTemp+k;
      }
      // clean-up
      MPI_Comm_free(&(helper2[i].subComm));
    }
    
    // finish up by filling the globally unique integer id into the idsArray
    for (int i=0; i<elementCount; i++){
      idsArray[i] = helper1[idsArray[i]].id;
    }
    
  } // elementCount > 0
  
  // clean-up
  MPI_Group_free(&mpiGroup);
  
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
#define ESMC_METHOD "ESMCI::VM::logGarbageInfo()"
//BOPI
// !IROUTINE:  ESMCI::VM::logGarbageInfo - Log garbage info of current VM
//
// !INTERFACE:
void VM::logGarbageInfo(
//
// !ARGUMENTS:
//
  std::string prefix,
  bool current,
  ESMC_LogMsgType_Flag msgType
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
  // found the current VM
  
  int ic=i;
  int lb=0;
  int ub=matchTableBound;
  if (current){
    lb = ic;
    ub = ic+1;
  }

  char msg[512];
  for (int i=lb; i<ub; i++){
    if (i==ic)
      sprintf(msg, "%s - GarbInfo: VM matchTableIndex=%i"
        " ***current VM context****", prefix.c_str(), i);
    else
      sprintf(msg, "%s - GarbInfo: VM matchTableIndex=%i", prefix.c_str(), i);
    ESMC_LogDefault.Write(msg, msgType);
    sprintf(msg, "%s - GarbInfo: Fortran objs=%lu", prefix.c_str(),
      matchTable_FObjects[i].size());
    ESMC_LogDefault.Write(msg, msgType);
    for (unsigned j=0; j<matchTable_FObjects[i].size(); j++){
      void *basePtr = NULL;
      if (matchTable_FObjects[i][j].objectID != ESMC_ID_GEOMBASE.objectID)
        basePtr = **(void ***)(&matchTable_FObjects[i][j].fobject);
      sprintf(msg, "%s - GarbInfo: fortran objs[%d]: %s %p - %p",
        prefix.c_str(), j,
        ESMC_ObjectID_Name(matchTable_FObjects[i][j].objectID),
        *(void **)(&matchTable_FObjects[i][j].fobject), basePtr);
      ESMC_LogDefault.Write(msg, msgType);
    }
    sprintf(msg, "%s - GarbInfo: Base objs=%lu", prefix.c_str(),
      matchTable_Objects[i].size());
    ESMC_LogDefault.Write(msg, msgType);
    for (unsigned j=0; j<matchTable_Objects[i].size(); j++){
      const char *proxyString;
      proxyString="actual object";
      if (matchTable_Objects[i][j]->ESMC_BaseGetProxyFlag()==ESMF_PROXYYES)
        proxyString="proxy object";
      sprintf(msg, "%s - GarbInfo: base objs[%d]: %s : %p : %s : %d ; %s",
        prefix.c_str(), j, matchTable_Objects[i][j]->ESMC_BaseGetClassName(),
        matchTable_Objects[i][j],
        matchTable_Objects[i][j]->ESMC_BaseGetName(),
        matchTable_Objects[i][j]->ESMC_BaseGetID(), proxyString);
      ESMC_LogDefault.Write(msg, msgType);
    }
  }

  // return successfully
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::logBacktrace()"
//BOPI
// !IROUTINE:  ESMCI::VM::logBacktrace - Log backtrace
//
// !INTERFACE:
void VM::logBacktrace(
//
// !ARGUMENTS:
//
  std::string prefix,
  ESMC_LogMsgType_Flag msgType
  ){
//
// !DESCRIPTION:
//   Log the backtrace of the current call stack.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

#if (defined ESMF_OS_Linux || defined ESMF_OS_Unicos)
  const int size=1000;
  void *buffer[size];
  int count = backtrace(buffer, size);
  char **symbols = backtrace_symbols(buffer, count);
  for (int i=0; i<count; i++){
    std::stringstream info;
    info << prefix << " - Backtrace: " << symbols[i];
    ESMC_LogDefault.Write(info, msgType);
  }
  free(symbols);
#endif

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
  ESMC_LogMsgType_Flag msgType,
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
      log->Write(msg, msgType);
    }
  }
  fclose(file);
  // access mallinfo
  std::stringstream info;
  struct mallinfo m = mallinfo();
  info << "Non-mmapped space allocated (bytes):       \t" << m.arena;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  info.str(""); // clear info
  info << "Space allocated in mmapped regions (bytes):\t" << m.hblkhd;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  info.str(""); // clear info
  info << "Maximum total allocated space (bytes):     \t" << m.usmblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  info.str(""); // clear info
  info << "Space in freed fastbin blocks (bytes):     \t" << m.fsmblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  info.str(""); // clear info
  info << "Total allocated space (bytes):             \t" << m.uordblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  info.str(""); // clear info
  info << "Total free space (bytes):                  \t" << m.fordblks;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  info.str(""); // clear info
  info << "Top-most, releasable space (bytes):        \t" << m.keepcost;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  long total = 0; // init
  if (m.hblkhd>=0 && m.uordblks>=0){
    total = (long)m.hblkhd+(long)m.uordblks;
    total /= (long)1024;  // scale to KiB
  }
  info.str(""); // clear info
  info << "Total space in use, mmap + non-mmap (KiB): \t" << total;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
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
  log->Write(msg, msgType);
  pos = malloc_stats_output.rfind("in use bytes     =");
  pos += 18;
  long in_use = strtol(malloc_stats_output.c_str()+pos, NULL, 10);
  in_use /= (long)1024;  // scale to KiB
  info.str(""); // clear info
  info << "Total space used (mmap + non-mmap) (KiB):  \t" << in_use;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  // output the wtime since execution start
  double wt;
  ESMCI::VMK::wtime(&wt);
  info.str(""); // clear info
  info << "Wall-clock time since execution start (s): \t" << wt;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);
  // unlock again
  vm->unlock();
#endif
#if (defined ESMF_OS_Darwin)
  // must lock/unlock for thread-safety
  VM *vm = getCurrent();
  vm->lock();

  // string storage
  char msg[256];

  // Get memory
  task_vm_info_data_t mem_info;
  mach_msg_type_number_t size = TASK_VM_INFO_COUNT;
  kern_return_t kerr = task_info(mach_task_self(),
                                 TASK_VM_INFO,
                                 (task_info_t)&mem_info,
                                 &size);
  if( kerr == KERN_SUCCESS ) {
    sprintf(msg, "%s - MemInfo: VmRSS:                       \t%d (bytes)",prefix.c_str(),mem_info.resident_size);
    log->Write(msg, msgType);
    sprintf(msg, "%s - MemInfo: VmHWM:                       \t%d (bytes)",prefix.c_str(),mem_info.resident_size_peak);
    log->Write(msg, msgType);
    sprintf(msg, "%s - MemInfo: Total allocated space (bytes): \t%d",prefix.c_str(),mem_info.virtual_size);
    log->Write(msg, msgType);

    // Other memory info that might be useful at some point
#if 0
    sprintf(msg, "%s - MemInfo: v_size:                       \t%d",prefix.c_str(),mem_info.virtual_size);
    log->Write(msg, msgType);

    sprintf(msg, "%s - MemInfo: r_size:                       \t%d",prefix.c_str(),mem_info.resident_size);
    log->Write(msg, msgType);

    sprintf(msg, "%s - MemInfo: internal:                       \t%d",prefix.c_str(),mem_info.internal);
    log->Write(msg, msgType);

    sprintf(msg, "%s - MemInfo: external:                       \t%d",prefix.c_str(),mem_info.external);
    log->Write(msg, msgType);

    sprintf(msg, "%s - MemInfo: resusable:                       \t%d",prefix.c_str(),mem_info.reusable);
    log->Write(msg, msgType);

    sprintf(msg, "%s - MemInfo: compressed:                       \t%d",prefix.c_str(),mem_info.compressed);
    log->Write(msg, msgType);

    sprintf(msg, "%s - MemInfo: phys_footprint:                       \t%d",prefix.c_str(),mem_info.phys_footprint);
    log->Write(msg, msgType);
#endif

  }


  // output the wtime since execution start
  std::stringstream info;
  double wt;
  ESMCI::VMK::wtime(&wt);
  info.str(""); // clear info
  info << "Wall-clock time since execution start (s): \t" << wt;
  sprintf(msg, "%s - MemInfo: %s", prefix.c_str(), info.str().c_str());
  log->Write(msg, msgType);

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

#ifdef GARBAGE_COLLECTION_LOG_on
  std::stringstream msg;
  msg << "VM::addObject() object added: " << object;
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
  //logBacktrace("VM::addObject()", ESMC_LOGMSG_DEBUG);  // enable to pin down specific caller
#endif

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

  //gjt: Disabled the following block of code because we allow object sharing
  //gjt: across VMs. Therefore garbage collection functionality like removing
  //gjt: an object must loop through _all_ of the VMs.
#if 0
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
#endif

  // must lock/unlock for thread-safe access to std::vector
  VM *vm = getCurrent();
  vm->lock();
  for (int i=0; i<matchTableBound; i++){  //gjt: loop through all of the VMs
  for (vector<ESMC_Base *>::iterator
    it = matchTable_Objects[i].begin();
    it != matchTable_Objects[i].end(); ++it){
    if (*it == object){
      matchTable_Objects[i].erase(it);  // erase the object entry
#ifdef GARBAGE_COLLECTION_LOG_on
    std::stringstream msg;
    msg << "VM::rmObject() object removed: " << object;
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    //logBacktrace("VM::rmObject()", ESMC_LOGMSG_DEBUG);  // enable to pin down specific caller
#endif
      break;
    }
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
  
#ifdef GARBAGE_COLLECTION_LOG_on
  std::stringstream msg;
  msg << "VM::addFObject() object added: " <<
    string(ESMC_ObjectID_Name(objectID)) << " " << *(void **)fobject << " - " <<
    **(void ***)fobject;
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
  //logBacktrace("VM::addFObject()", ESMC_LOGMSG_DEBUG);  // enable to pin down specific caller
#endif

  vm->unlock();
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

  //gjt: Disabled the following block of code because we allow object sharing
  //gjt: across VMs. Therefore garbage collection functionality like removing
  //gjt: an object must loop through _all_ of the VMs.
#if 0
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
#endif

  // must lock/unlock for thread-safe access to std::vector
  VM *vm = getCurrent();
  vm->lock();
  for (int i=0; i<matchTableBound; i++){  //gjt: loop through all of the VMs
  for (vector<FortranObject>::iterator
    it = matchTable_FObjects[i].begin();
    it != matchTable_FObjects[i].end(); ++it){

    void *fobjectElement = (void *)&(it->fobject);

    int flag;
    FTN_X(f_esmf_fortranudtpointercompare)(fobjectElement, (void *)fobject, &flag);

    if (flag){
      matchTable_FObjects[i].erase(it);  // erase the object entry
#ifdef GARBAGE_COLLECTION_LOG_on
      std::stringstream msg;
      msg << "VM::rmFObject() object removed: " << *(void **)fobject << " - " <<
        **(void ***)fobject;
      ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
      //logBacktrace("VM::rmFObject()", ESMC_LOGMSG_DEBUG);  // enable to pin down specific caller
#endif
      break;
    }
  }
  }

  vm->unlock();
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::VM::validObject()"
//BOPI
// !IROUTINE:  ESMCI::VM::validObject - Check if an object is valid in garbage collection
//
// !INTERFACE:
bool VM::validObject(
//
// !RETURN VALUE:
//    true/false
//
// !ARGUMENTS:
//
  ESMC_Base *object){   // object to be checked
//
// !DESCRIPTION:
//    Check if an object is valid in garbage collection
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  // must lock/unlock for thread-safe access to std::vector
  bool valid = false;
  VM *vm = getCurrent();
  vm->lock();
  for (int i=0; i<matchTableBound; i++){
  for (vector<ESMC_Base *>::iterator
    it = matchTable_Objects[i].begin();
    it != matchTable_Objects[i].end(); ++it){
    if (*it == object){
      valid = true;
      break;
    }
  }
  }
  vm->unlock();
  return valid;
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
#if 0
//gjt: Disable cleaning up GeomBase from the garbage collection level because
//gjt: each Field actually owns its own instance of the GeomBase. The way
//gjt: FieldDestruct() currently works, it looks at the GeomBase, and if that
//gjt: were already destroyed, it causes issues. In the long run this might
//gjt: be better implemented via a protection against accessing GeomBase from
//gjt: FieldDestruct() in that case. But then it is important that all of the
//gjt: Geom types correctly implement their destructor. Right now the Mesh
//gjt: destructur is not doing all that is needed, and so the Mesh cleanup
//ght: actually does depend on the Field garbage collection.
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_GEOMBASE.objectID){
        FTN_X(f_esmf_geombasecollectgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
#endif
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_XGRIDGEOMBASE.objectID){
        FTN_X(f_esmf_xgridgeombasecolgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
#if 0
//gjt: Disable cleaning up LocStream from the garbage collection level because
//gjt: it leads to issues when a field tries to destroy it later.
//gjt: This is going to be tricky to resolve with Fortran implemented classes.
//gjt: For now rather have small memory leaks than invalid memory access.
      }else if (matchTable_FObjects[0][k].objectID ==
        ESMC_ID_LOCSTREAM.objectID){
        FTN_X(f_esmf_locstreamcollectgarbage)(
          &(matchTable_FObjects[0][k].fobject), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc))
          return;
#endif
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
        << matchTable_Objects[0][k]->ESMC_BaseGetName() << " : "
        << matchTable_Objects[0][k];
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
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
  std::string timer,
  ESMC_LogMsgType_Flag msgType
  ){
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
  ESMC_LogDefault.Write(timerMsg.str(), msgType);
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
