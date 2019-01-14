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
#define ESMC_FILENAME "ESMCI_ArrayBundle.C"
//==============================================================================
#define AB_REDISTSTORE_LOG_off
//==============================================================================
//
// ArrayBundle class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ArrayBundle methods declared
// in the companion file ESMCI_ArrayBundle.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_ArrayBundle.h"

// include higher level, 3rd party or system headers
#include <cstdio>
#include <cstring>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_Container.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_IO.h"

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


namespace ESMCI {

//-----------------------------------------------------------------------------
//
// constructor and destruct()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::ArrayBundle()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::ArrayBundle    - constructor
//
// !INTERFACE:
ArrayBundle::ArrayBundle(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  Array **arrayList,                  // (in)
  int arrayCount,                     // (in)
  bool multi,                         // (in)
  bool relaxed                        // (in)
  ){
//
// !DESCRIPTION:
//    Construct the internal structure of an ESMCI::ArrayBundle object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{  
    
    // fill in the ArrayBundle object
    for (int i=0; i<arrayCount; i++){
#if 0
      ESMC_LogDefault.Write(arrayList[i]->getName(), ESMC_LOGMSG_INFO);
#endif
      arrayContainer.add(string(arrayList[i]->getName()), arrayList[i],
        multi, relaxed);
    }
    
    arrayCreator = false; // Array objects were provided externally
  
    // invalidate the name for this ArrayBundle object in the Base class
    ESMC_BaseSetName(NULL, "ArrayBundle");
   
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    throw rc;  // bail out with exception
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    throw rc;  // bail out with exception
  }
  
  // return successfully
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::destruct()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::destruct
//
// !INTERFACE:
int ArrayBundle::destruct(bool followCreator){
//
// TODO: The followCreator flag is only needed until we have reference counting.
// TODO: For now followCreator, which by default is true, will be coming in as
// TODO: false when calling through the native destructor. This prevents
// TODO: sequence problems during automatic garbage collection unit reference
// TODO: counting comes in to solve this problem in the final manner.
//
// !DESCRIPTION:
//    Destruct the internal structure of an ESMCI::ArrayBundle object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    // garbage collection
    if (arrayCreator && followCreator){
      vector<Array *> arrayVector;
      getVector(arrayVector);
      for (int i=0; i<getCount(); i++)
        Array::destroy(&arrayVector[i]);
    }
    // swap() trick with an empty temporary to free container's memory
    //TODO: Activate the swap trick below if there is ever suspicion that the
    //TODO: container member inside of a destroyed ArrayBundle is holding on
    //TODO: memory its memory.
    //Container<std::string, Array *>().swap(arrayContainer);
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::create()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::create
//
// !INTERFACE:
ArrayBundle *ArrayBundle::create(
//
// !RETURN VALUE:
//    ArrayBundle * to newly allocated ArrayBundle
//
// !ARGUMENTS:
//
  Array **arrayList,                  // (in)
  int arrayCount,                     // (in)
  bool multi,                         // (in)
  bool relaxed,                       // (in)
  int *rc                             // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ArrayBundle} object from list of Arrays.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  ArrayBundle *arraybundle;
  try{
  
  // check the input and get the information together to call creator

  // call class constructor
  try{
    arraybundle = new ArrayBundle(arrayList, arrayCount, multi, relaxed);
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return ESMC_NULL_POINTER; // bail out
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::ArrayBundle.", ESMC_CONTEXT,
      rc);  
    return ESMC_NULL_POINTER; // bail out
  }
  
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER; // bail out
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arraybundle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::destroy()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::destroy
//
// !INTERFACE:
int ArrayBundle::destroy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle **arraybundle,    // in - ArrayBundle to destroy
  bool noGarbage){              // in - remove from garbage collection
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // return with errors for NULL pointer
  if (arraybundle == ESMC_NULL_POINTER || *arraybundle == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to ArrayBundle", ESMC_CONTEXT, &rc);
    return rc;
  }

  // destruct ArrayBundle object
  localrc = (*arraybundle)->destruct();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // mark as invalid object
  (*arraybundle)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  
  // optionally delete the complete object and remove from garbage collection
  if (noGarbage){
    VM::rmObject(*arraybundle); // remove object from garbage collection
    delete (*arraybundle);      // completely delete the object, free heap
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// read and write
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::read()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::read
//
// !INTERFACE:
int ArrayBundle::read(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  const std::string &file,      // in    - name of file being read
  bool *singleFile,             // in    - All arrays from single file if true
  int   *timeslice,             // in    - timeslice option
  ESMC_IOFmt_Flag *iofmt        // in    - I/O format flag
  ){
//
// !DESCRIPTION:
//   Read Array data to an ArrayBundle object from file(s).
//   For this API to be functional, the environment variable {\tt ESMF\_PIO} 
//   should be set to "internal" when the ESMF library is built.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  ESMC_IOFmt_Flag localiofmt;
  bool localsingleFile;                   // For default handling

  // Check the required parameters
  if (file.empty()) {
    ESMC_LogDefault.Write("filename argument required",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    return ESMF_RC_ARG_BAD;
  }
  // Set optional parameters which are not optional at next layer
  if ((ESMC_IOFmt_Flag *)NULL != iofmt) {
    localiofmt = *iofmt;
  } else {
    localiofmt = ESMF_IOFMT_NETCDF;
  }

  IO *newIO = IO::create(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  if ((bool *)NULL == singleFile) {
    localsingleFile = false;
  } else {
    localsingleFile = *singleFile;
  }

  // From here out, we need to be sure to clean up before returning
  if (localsingleFile) {
    Container<std::string, Array *>::iterator it;
    for (it = arrayContainer.begin();
         it != arrayContainer.end(); ++it) {
      if (ESMF_SUCCESS == localrc) {
        localrc = newIO->addArray(it->second->second);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc);
      }
    }
    if (ESMF_SUCCESS == localrc) {
      // Call the IO read function
      localrc = newIO->read(file, localiofmt, timeslice);
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc);
    }
  } else {
    Container<std::string, Array *>::iterator it;
    int i = 0;
    for (it = arrayContainer.begin();
         it != arrayContainer.end(); ++it) {
      if (ESMF_SUCCESS == localrc) {
        localrc = newIO->addArray(it->second->second);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc);
      }
      if (ESMF_SUCCESS == localrc) {
        stringstream filename;
        filename << file << std::fixed << std::setw(3) << std::setfill('0') << i;
        // Call the IO read function
        localrc = newIO->read(filename.str(), localiofmt, timeslice);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc);
        newIO->clear();
      }
      i++;
    }
  }

  // cleanup
  IO::destroy(&newIO);
  newIO = (IO *)NULL;

  // return
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::write()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::write
//
// !INTERFACE:
int ArrayBundle::write(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  const std::string &file,        // in    - name of file being read
  const std::string &convention,  // in    - Attribute convention
  const std::string &purpose,     // in    - Attribute purpose
  bool *singleFile,               // in    - All arrays to single file if true
  bool *overwrite,                // in    - OK to overwrite fields if true
  ESMC_FileStatus_Flag *status,   // in    - file status flag
  int   *timeslice,               // in    - timeslice option
  ESMC_IOFmt_Flag *iofmt          // in    - I/O format flag
  ){
//
// !DESCRIPTION:
//   Write the Arrays into a file. For this API to be functional,
//   the environment variable {\tt ESMF\_PIO} should be set to "internal"
//   when the ESMF library is built.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  ESMC_IOFmt_Flag localiofmt;             // For default handling
  bool localoverwrite;                    // For default handling
  bool localsingleFile;                   // For default handling
  ESMC_FileStatus_Flag localstatus;       // For default handling

  // Check the required parameters
  if (file.empty()) {
    ESMC_LogDefault.Write("filename argument required",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    return ESMF_RC_ARG_BAD;
  }

  IO *newIO = IO::create(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // Handle format default
  if ((ESMC_IOFmt_Flag *)NULL == iofmt) {
    localiofmt = ESMF_IOFMT_NETCDF;
  } else {
    localiofmt = *iofmt;
  }
  // Handle overwrite default
  if ((bool *)NULL == overwrite) {
    localoverwrite = false;
  } else {
    localoverwrite = *overwrite;
  }
  // Handle singleFile default
  if ((bool *)NULL == singleFile) {
    localsingleFile = false;
  } else {
    localsingleFile = *singleFile;
  }
  // Handle status default
  if ((ESMC_FileStatus_Flag *)NULL == status) {
    localstatus = ESMC_FILESTATUS_UNKNOWN;
  } else {
    localstatus = *status;
  }

  // From here out, we need to be sure to clean up before returning
  if (localsingleFile) {
    Container<std::string, Array *>::iterator it;
    for (it = arrayContainer.begin();
         it != arrayContainer.end(); ++it) {
      if (ESMF_SUCCESS == localrc) {
        localrc = newIO->addArray(it->second->second);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc);
      }
    }
    if (ESMF_SUCCESS == localrc) {
      // Call the IO write function
      localrc = newIO->write(file, localiofmt, localoverwrite,
                             localstatus, timeslice);
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc);
    }
  } else {
    Container<std::string, Array *>::iterator it;
    int i = 0;
    for (it = arrayContainer.begin();
         it != arrayContainer.end(); ++it) {
      if (ESMF_SUCCESS == localrc) {
        localrc = newIO->addArray(it->second->second);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc);
      }
      if (ESMF_SUCCESS == localrc) {
        stringstream filename;
        filename << file << std::fixed << std::setw(3) << std::setfill('0') << i;
        // Call the IO write function
        localrc = newIO->write(filename.str(), localiofmt, localoverwrite,
          localstatus, timeslice);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc);
        newIO->clear();
      }
      i++;
    }
  }

  // cleanup
  IO::destroy(&newIO);
  newIO = (IO *)NULL;

  // return
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// misc.
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::print()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::print
//
// !INTERFACE:
int ArrayBundle::print()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of ArrayBundle object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to ArrayBundle", ESMC_CONTEXT, &rc);
    return rc;
  }

  // print info about the ESMCI::ArrayBundle object
  printf("--- ESMCI::ArrayBundle::print start ---\n");
  printf("ArrayBundle: %s\n", getName());
  printf("arrayCount = %d\n", getCount());
  arrayContainer.print();
  printf("--- ESMCI::ArrayBundle::print end ---\n");
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// comms
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::haloStore()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::haloStore
//
// !INTERFACE:
int ArrayBundle::haloStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *arraybundle,           // inout - ArrayBundle to be haloed
  RouteHandle **routehandle,          // inout - handle to precomputed comm
  ESMC_HaloStartRegionFlag halostartregionflag, // in - start of halo region
  InterArray<int> *haloLDepth,        // in    - lower corner halo depth
  InterArray<int> *haloUDepth         // in    - upper corner halo depth
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for ArrayBundle halo 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  int petCount = vm->getPetCount();
  int localPet = vm->getLocalPet();

  try{
    // every Pet must provide arraybundle
    if (arraybundle == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to arraybundle", ESMC_CONTEXT, &rc);
      return rc;
    }
    int arrayCount = arraybundle->getCount();
    vector<int> arrayCountList(petCount);
    vm->allgather(&arrayCount, &(arrayCountList[0]), sizeof(int));
    arrayCount = *min_element(arrayCountList.begin(), arrayCountList.end());
    if (arrayCount != 
      *max_element(arrayCountList.begin(), arrayCountList.end())){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "arraybundle argument contains different number"
        " of Arrays on different PETs", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (arrayCount == 0){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "arraybundle argument contains no Arrays", ESMC_CONTEXT, &rc);
      return rc;
    }
    // construct local matchList
    vector<int> matchList(arrayCount);
    vector<Array *> arrayVector;
    arraybundle->getVector(arrayVector, ESMC_ITEMORDER_ADDORDER);
    for (int i=0; i<arrayCount; i++){
      matchList[i] = i; // initialize
      Array *array = arrayVector[i];
      // search if there was an earlier entry that is compatible
      for (int j=i-1; j>=0; j--){
        bool match = array->isRHCompatible(arrayVector[j], &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        if (match){
          // found match
          matchList[i] = matchList[j];
          break;
        }
      }
    }
    // communicate to construct global matchList
    vector<int> matchPetList(petCount);
    for (int i=0; i<arrayCount; i++){
      vm->allgather(&(matchList[i]), &(matchPetList[0]), sizeof(int));
      int match = *min_element(matchPetList.begin(), matchPetList.end());
      if (match == *max_element(matchPetList.begin(), matchPetList.end()))
        matchList[i] = match;        
      else
        matchList[i] = i;
    }
    // create and initialize the RouteHandle
    *routehandle = RouteHandle::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    localrc = (*routehandle)->setType(ESMC_ARRAYBUNDLEXXE);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    // allocate XXE and attach to RouteHandle
    XXE *xxe;
    try{
      xxe = new XXE(vm, 100, 10, 1000);
    }catch (...){
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
      return rc;
    }
    localrc = (*routehandle)->setStorage(xxe);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    // use Array::haloStore() to determine the required XXE streams
    int rraShift = 0; // reset
    int vectorLengthShift = 0;  // reset
    vector<XXE *> xxeSub(arrayCount);
    for (int i=0; i<arrayCount; i++){
      Array *array = arrayVector[i];
      if (matchList[i] < i){
        // Array matches previous Array in ArrayBundle
//        printf("localPet=%d, Array #%d does not require precompute, "
//          "reuse xxe from #%d\n", localPet, i, matchList[i]);
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[matchList[i]], rraShift,
          vectorLengthShift);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
      }else{
        // Array does _not_ match any previous Array in ArrayBundle
//        printf("localPet=%d, Array #%d requires precompute\n",
//          localPet, i);
        RouteHandle *rh;
        localrc = Array::haloStore(array, &rh, halostartregionflag,
          haloLDepth, haloUDepth);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // get a handle on the XXE stored in rh
        xxeSub[i] = (XXE *)rh->getStorage();
        // delete the temporary routehandle w/o deleting the xxeSub
        localrc = rh->setType(ESMC_UNINITIALIZEDHANDLE);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        localrc = RouteHandle::destroy(rh);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[i], rraShift,
          vectorLengthShift);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // keep track of xxeSub for xxe garbage collection
        localrc = xxe->storeXxeSub(xxeSub[i]);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      }
      rraShift += 2 * array->getDELayout()->getLocalDeCount();
      ++vectorLengthShift;
    }
    //TODO: consider calling an XXE optimization method here that could
    //TODO: re-arrange what is in all of the sub XXE streams for performance opt
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return, don't set success, this is multi exit method
  return rc;
}
//-----------------------------------------------------------------------------
    
    //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::halo()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::halo
//
// !INTERFACE:
int ArrayBundle::halo(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *arraybundle,             // inout - ArrayBundle to be haloed
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  bool checkflag                        // in    - ESMF_FALSE: (def.) bas. chcks
                                        //         ESMF_TRUE: full input check
  ){    
//
// !DESCRIPTION:
//    Execute an ArrayBundle halo operation
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // implemented via sparseMatMul
  localrc = sparseMatMul(arraybundle, arraybundle, routehandle,
    ESMC_REGION_SELECT, NULL, 0, checkflag, true);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::haloRelease()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::haloRelease
//
// !INTERFACE:
int ArrayBundle::haloRelease(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  RouteHandle *routehandle        // inout -
  ){    
//
// !DESCRIPTION:
//    Release information for an ArrayBundle halo
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // implemented via sparseMatMul
  localrc = sparseMatMulRelease(routehandle);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::redistStore()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::redistStore
//
// !INTERFACE:
int ArrayBundle::redistStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,            // in    - source ArrayBundle
  ArrayBundle *dstArraybundle,            // in    - destination ArrayBundle
  RouteHandle **routehandle,              // inout - handle to precomputed comm
  InterArray<int> *srcToDstTransposeMap,  // in    - mapping src -> dst dims
  ESMC_TypeKind_Flag typekindFactor,      // in    - typekind of factor
  void *factor                            // in    - redist factor
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for redistribution
//  from srcArrayBundle to dstArrayBundle.
//
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  int petCount = vm->getPetCount();
  int localPet = vm->getLocalPet();

  try{
    // every Pet must provide srcArraybundle and dstArraybundle
    if (srcArraybundle == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to srcArraybundle", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (dstArraybundle == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to dstArraybundle", ESMC_CONTEXT, &rc);
      return rc;
    }
    int arrayCount = srcArraybundle->getCount();
    if (arrayCount != dstArraybundle->getCount()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "srcArraybundle and dstArraybundle contain different number"
        " of Arrays", ESMC_CONTEXT, &rc);
      return rc;
    }
    vector<int> arrayCountList(petCount);
    vm->allgather(&arrayCount, &(arrayCountList[0]), sizeof(int));
    arrayCount = *min_element(arrayCountList.begin(), arrayCountList.end());
    if (arrayCount != 
      *max_element(arrayCountList.begin(), arrayCountList.end())){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "srcArraybundle and dstArraybundle arguments contain different number"
        " of Arrays on different PETs", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (arrayCount == 0){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "srcArraybundle and dstArraybundle arguments contain no Arrays",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    // construct local matchList
    vector<int> matchList(arrayCount);
    vector<Array *> srcArrayVector;
    vector<Array *> dstArrayVector;
    srcArraybundle->getVector(srcArrayVector, ESMC_ITEMORDER_ADDORDER);
    dstArraybundle->getVector(dstArrayVector, ESMC_ITEMORDER_ADDORDER);
    for (int i=0; i<arrayCount; i++){
      matchList[i] = i; // initialize
      Array *srcArray = srcArrayVector[i];
      Array *dstArray = dstArrayVector[i];
      // search if there was an earlier entry that is compatible
      for (int j=i-1; j>=0; j--){
        bool srcMatch = srcArray->isRHCompatible(srcArrayVector[j], &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        bool dstMatch = dstArray->isRHCompatible(dstArrayVector[j], &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        if (srcMatch && dstMatch){
          // found match
          matchList[i] = matchList[j];
          break;
        }
      }
    }
    // communicate to construct global matchList
    vector<int> matchPetList(petCount);
    for (int i=0; i<arrayCount; i++){
      vm->allgather(&(matchList[i]), &(matchPetList[0]), sizeof(int));
      int match = *min_element(matchPetList.begin(), matchPetList.end());
      if (match == *max_element(matchPetList.begin(), matchPetList.end()))
        matchList[i] = match;        
      else
        matchList[i] = i;
    }
    // create and initialize the RouteHandle
    *routehandle = RouteHandle::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    localrc = (*routehandle)->setType(ESMC_ARRAYBUNDLEXXE);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    // allocate XXE and attach to RouteHandle
    XXE *xxe;
    try{
      xxe = new XXE(vm, 100, 10, 1000);
    }catch (...){
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
      return rc;
    }
    localrc = (*routehandle)->setStorage(xxe);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    // use Array::redistStore() to determine the required XXE streams
    int rraShift = 0; // reset
    int vectorLengthShift = 0;  // reset
    vector<XXE *> xxeSub(arrayCount);
    for (int i=0; i<arrayCount; i++){
      Array *srcArray = srcArrayVector[i];
      Array *dstArray = dstArrayVector[i];
      if (matchList[i] < i){
        // src/dst Array pair matches previous pair in ArrayBundle
#ifdef AB_REDISTSTORE_LOG_on
        {
          std::stringstream msg;
          msg << "AB_REDISTSTORE_LOG:" << __LINE__ << " pair #" << i <<
            " does NOT require precompute! Found match with pair #" << 
            matchList[i];
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
        }
#endif
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[matchList[i]], rraShift,
          vectorLengthShift);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
      }else{
        // src/dst Array pair does _not_ match any previous pair in ArrayBundle
#ifdef AB_REDISTSTORE_LOG_on
        {
          std::stringstream msg;
          msg << "AB_REDISTSTORE_LOG:" << __LINE__ << " pair #" << i <<
            " DOES require precompute!";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
        }
#endif
        RouteHandle *rh;
        localrc = Array::redistStore(srcArray, dstArray, &rh,
          srcToDstTransposeMap, typekindFactor, factor);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // get a handle on the XXE stored in rh
        xxeSub[i] = (XXE *)rh->getStorage();
        // delete the temporary routehandle w/o deleting the xxeSub
        localrc = rh->setType(ESMC_UNINITIALIZEDHANDLE);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        localrc = RouteHandle::destroy(rh);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[i], rraShift,
          vectorLengthShift);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // keep track of xxeSub for xxe garbage collection
        localrc = xxe->storeXxeSub(xxeSub[i]);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      }
      rraShift += srcArray->getDELayout()->getLocalDeCount()
        + dstArray->getDELayout()->getLocalDeCount();
      ++vectorLengthShift;
    }
    //TODO: consider calling an XXE optimization method here that could
    //TODO: re-arrange what is in all of the sub XXE streams for performance opt
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return, don't set success, this is multi exit method
  return rc;
}
//-----------------------------------------------------------------------------
    
    //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::redist()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::redist
//
// !INTERFACE:
int ArrayBundle::redist(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,          // in    - source ArrayBundle
  ArrayBundle *dstArraybundle,          // inout - destination ArrayBundle
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  bool checkflag                        // in    - ESMF_FALSE: (def.) bas. chcks
                                        //         ESMF_TRUE: full input check
  ){    
//
// !DESCRIPTION:
//    Execute an ArrayBundle redistribution operation
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // implemented via sparseMatMul
  localrc = sparseMatMul(srcArraybundle, dstArraybundle, routehandle,
    ESMC_REGION_SELECT, NULL, 0, checkflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::redistRelease()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::redistRelease
//
// !INTERFACE:
int ArrayBundle::redistRelease(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  RouteHandle *routehandle        // inout -
  ){    
//
// !DESCRIPTION:
//    Release information for an ArrayBundle redistribution
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // implemented via sparseMatMul
  localrc = sparseMatMulRelease(routehandle);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::sparseMatMulStore()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::sparseMatMulStore
//
// !INTERFACE:
int ArrayBundle::sparseMatMulStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,        // in    - source ArrayBundle
  ArrayBundle *dstArraybundle,        // in    - destination ArrayBundle
  RouteHandle **routehandle,          // inout - handle to precomputed comm
  vector<SparseMatrix<ESMC_I4,ESMC_I4> > &sparseMatrix, // in - sparse matrix
  InterArray<int> *srcTermProcessing  // inout - srcTermProcessing parameters
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for sparse matrix multiplication
//  from srcArrayBundle to dstArrayBundle.
//
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  int petCount = vm->getPetCount();
  int localPet = vm->getLocalPet();

  try{
    // every Pet must provide srcArraybundle and dstArraybundle
    if (srcArraybundle == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to srcArraybundle", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (dstArraybundle == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to dstArraybundle", ESMC_CONTEXT, &rc);
      return rc;
    }
    int arrayCount = srcArraybundle->getCount();
    if (arrayCount != dstArraybundle->getCount()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "srcArraybundle and dstArraybundle contain different number"
        " of Arrays", ESMC_CONTEXT, &rc);
      return rc;
    }
    vector<int> arrayCountList(petCount);
    vm->allgather(&arrayCount, &(arrayCountList[0]), sizeof(int));
    arrayCount = *min_element(arrayCountList.begin(), arrayCountList.end());
    if (arrayCount != 
      *max_element(arrayCountList.begin(), arrayCountList.end())){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "srcArraybundle and dstArraybundle arguments contain different number"
        " of Arrays on different PETs", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (arrayCount == 0){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "srcArraybundle and dstArraybundle arguments contain no Arrays",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    // check if srcTermProcessing argument is valid
    vector<int*> srcTermProcParameters(arrayCount);
    if (!present(srcTermProcessing)){
      // srcTermProcessing argument is not present
      for (int i=0; i<arrayCount; i++)
        srcTermProcParameters[i] = NULL;  // invalidate each parameter
    }else{
      // srcTermProcessing argument is present
      if (srcTermProcessing->extent[0]==arrayCount){
        // same number of elements as there are arrays in the bundles
        for (int i=0; i<arrayCount; i++)
          srcTermProcParameters[i] = &(srcTermProcessing->array[i]);
      }else if (srcTermProcessing->extent[0]==1){
        // single element in srcTermProcessing but more arrays in bundles
        if (srcTermProcessing->array[0] < 0){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "Single srcTermProcessing parameter must not be negative to "
            "apply for all arrays in bundle.",
            ESMC_CONTEXT, &rc);
          return rc;
        }
        for (int i=0; i<arrayCount; i++)
          srcTermProcParameters[i] = &(srcTermProcessing->array[0]);
      }else{
        // all other conditions are error conditions
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "Number of elements in srcTermProcessing must match number of "
          "arrays in bundles, or be 1.",
          ESMC_CONTEXT, &rc);
        return rc;
      }
    }
    // construct local matchList
    vector<int> matchList(arrayCount);
    vector<Array *> srcArrayVector;
    vector<Array *> dstArrayVector;
    srcArraybundle->getVector(srcArrayVector, ESMC_ITEMORDER_ADDORDER);
    dstArraybundle->getVector(dstArrayVector, ESMC_ITEMORDER_ADDORDER);    
    for (int i=0; i<arrayCount; i++){
      matchList[i] = i; // initialize
      Array *srcArray = srcArrayVector[i];
      Array *dstArray = dstArrayVector[i];
      // search if there was an earlier entry that is compatible
      for (int j=i-1; j>=0; j--){
        bool srcMatch = srcArray->isRHCompatible(srcArrayVector[j], &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        bool dstMatch = dstArray->isRHCompatible(dstArrayVector[j], &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        if (srcMatch && dstMatch){
          // found match
          matchList[i] = matchList[j];
          break;
        }
      }
    }
    // communicate to construct global matchList
    vector<int> matchPetList(petCount);
    for (int i=0; i<arrayCount; i++){
      vm->allgather(&(matchList[i]), &(matchPetList[0]), sizeof(int));
      int match = *min_element(matchPetList.begin(), matchPetList.end());
      if (match == *max_element(matchPetList.begin(), matchPetList.end()))
        matchList[i] = match;        
      else
        matchList[i] = i;
    }
    // create and initialize the RouteHandle
    *routehandle = RouteHandle::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    localrc = (*routehandle)->setType(ESMC_ARRAYBUNDLEXXE);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    // allocate XXE and attach to RouteHandle
    XXE *xxe;
    try{
      xxe = new XXE(vm, 100, 10, 1000);
    }catch (...){
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
      return rc;
    }
    localrc = (*routehandle)->setStorage(xxe);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;
    // use Array::sparseMatMulStore() to determine the required XXE streams
    int rraShift = 0; // reset
    int vectorLengthShift = 0;  // reset
    vector<XXE *> xxeSub(arrayCount);
    for (int i=0; i<arrayCount; i++){
      Array *srcArray = srcArrayVector[i];
      Array *dstArray = dstArrayVector[i];
      if (matchList[i] < i){
        // src/dst Array pair matches previous pair in ArrayBundle
//        printf("localPet=%d, src/dst pair #%d does not require precompute\n",
//          localPet, i);
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[matchList[i]], rraShift,
          vectorLengthShift);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // also supply the value of matching srcTermProcessing element back
        if (srcTermProcParameters[i] && srcTermProcParameters[matchList[i]])
          *srcTermProcParameters[i] = *srcTermProcParameters[matchList[i]];
      }else{
        // src/dst Array pair does _not_ match any previous pair in ArrayBundle
//        printf("localPet=%d, src/dst pair #%d requires precompute\n",
//          localPet, i);
        RouteHandle *rh;
        localrc = Array::sparseMatMulStore(srcArray, dstArray, &rh, 
          sparseMatrix, false, false, srcTermProcParameters[i]);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // get a handle on the XXE stored in rh
        xxeSub[i] = (XXE *)rh->getStorage();
        // delete the temporary routehandle w/o deleting the xxeSub
        localrc = rh->setType(ESMC_UNINITIALIZEDHANDLE);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        localrc = RouteHandle::destroy(rh);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[i], rraShift,
          vectorLengthShift);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // keep track of xxeSub for xxe garbage collection
        localrc = xxe->storeXxeSub(xxeSub[i]);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      }
      rraShift += srcArray->getDELayout()->getLocalDeCount()
        + dstArray->getDELayout()->getLocalDeCount();
      ++vectorLengthShift;
    }
    //TODO: consider calling an XXE optimization method here that could
    //TODO: re-arrange what is in all of the sub XXE streams for performance opt
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return, don't set success, this is multi exit method
  return rc;
}
//-----------------------------------------------------------------------------
    
    //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::sparseMatMul()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::sparseMatMul
//
// !INTERFACE:
int ArrayBundle::sparseMatMul(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,          // in    - source ArrayBundle
  ArrayBundle *dstArraybundle,          // inout - destination ArrayBundle
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  ESMC_Region_Flag zeroflag,            // in    - ESMC_REGION_TOTAL:
                                        //          -> zero out total region
                                        //         ESMC_REGION_SELECT:
                                        //          -> zero out target points
                                        //         ESMC_REGION_EMPTY:
                                        //          -> don't zero out any points
  ESMC_TermOrder_Flag *termorderflag,   // in    - ESMC_TERMORDER_FREE
                                        //         -> free partial sum order
                                        //       - ESMC_TERMORDER_SRCPET
                                        //         -> PET then seq index order
                                        //       - ESMC_TERMORDER_SRCSEQ
                                        //         -> strict src seq index order
  int termorderflag_len,                //       - elements in termorderflag
  bool checkflag,                       // in    - ESMF_FALSE: (def.) bas. chcks
                                        //         ESMF_TRUE: full input check
  bool haloFlag                         // in    - support halo conditions
  ){    
//
// !DESCRIPTION:
//    Execute an ArrayBundle sparse matrix multiplication operation
//
//EOPI
//-----------------------------------------------------------------------------
#define SMMINFO_off
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{
    // determine rhType
    RouteHandleType rhType = (*routehandle)->getType();
        
    Array *srcArray = NULL;
    Array *dstArray = NULL;
    vector<Array *> srcArrayVector;
    vector<Array *> dstArrayVector;
    srcArraybundle->getVector(srcArrayVector, ESMC_ITEMORDER_ADDORDER);
    dstArraybundle->getVector(dstArrayVector, ESMC_ITEMORDER_ADDORDER);
    
    // prepare termOrders vector
    vector<ESMC_TermOrder_Flag> termOrders;
    int count=0;  // reset
    if (srcArraybundle != NULL){
      count = srcArraybundle->getCount();
    }else if (dstArraybundle != NULL){
      count = dstArraybundle->getCount();
    }
    if (termorderflag_len == 0 || termorderflag == NULL){
      // set the default for all Array pairs
      for (int i=0; i<count; i++)
        termOrders.push_back(ESMC_TERMORDER_FREE);
    }else{
      if (termorderflag_len == 1){
        // set the single provided order for all Array pairs
        for (int i=0; i<count; i++)
          termOrders.push_back(*termorderflag);
      }else if(termorderflag_len == count){
        // copy the provided entries over
        for (int i=0; i<count; i++)
          termOrders.push_back(termorderflag[i]);
      }else{
        // inconsistency detected
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "incorrect number of elements provided in the termorderflag.", 
          ESMC_CONTEXT, &rc);
        return rc;  // bail out
      }
    }

    // process according to the different routehandle types        
    if (rhType == ESMC_ARRAYXXE){
      // apply same routehandle to each src/dst Array pair
      if (srcArraybundle != NULL && dstArraybundle != NULL){
        if (srcArraybundle->getCount() != dstArraybundle->getCount()){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "srcArraybundle and dstArraybundle contain different number"
            " of Arrays", ESMC_CONTEXT, &rc);
          return rc;
        }
        for (int i=0; i<srcArraybundle->getCount(); i++){
          srcArray = srcArrayVector[i];
          dstArray = dstArrayVector[i];
          localrc = Array::sparseMatMul(srcArray, dstArray, routehandle,
            ESMF_COMM_BLOCKING, NULL, NULL, zeroflag, termOrders[i],
            checkflag, haloFlag);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
      }else if (srcArraybundle != NULL){
        for (int i=0; i<srcArraybundle->getCount(); i++){
          srcArray = srcArrayVector[i];
          localrc = Array::sparseMatMul(srcArray, dstArray, routehandle,
            ESMF_COMM_BLOCKING, NULL, NULL, zeroflag, termOrders[i],
            checkflag, haloFlag);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
      }else if (dstArraybundle != NULL){
        for (int i=0; i<dstArraybundle->getCount(); i++){
          dstArray = dstArrayVector[i];
          localrc = Array::sparseMatMul(srcArray, dstArray, routehandle,
            ESMF_COMM_BLOCKING, NULL, NULL, zeroflag, termOrders[i],
            checkflag, haloFlag);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
      }
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }else if(rhType == ESMC_ARRAYBUNDLEXXE){
      // prepare for relative run-time addressing (RRA)
      vector<char *> rraList;
      vector<int> vectorLength;
      vector<int> srcLocalDeCountList;
      vector<XXE::SuperVectP> superVectPList;
      rraList.reserve(100);             // optimize performance
      vectorLength.reserve(100);        // optimize performance
      srcLocalDeCountList.reserve(100); // optimize performance
      superVectPList.reserve(100);      // optimize performance
      
      if (srcArraybundle != NULL && dstArraybundle != NULL){
        if (srcArraybundle->getCount() != dstArraybundle->getCount()){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "srcArraybundle and dstArraybundle contain different number"
            " of Arrays", ESMC_CONTEXT, &rc);
          return rc;
        }
      }

      // get a handle on the XXE stored in routehandle
      XXE *xxe = (XXE *)(*routehandle)->getStorage();
      
      if (srcArraybundle != NULL || dstArraybundle != NULL){
        int k=0;  // init
        for (int i=0; i<count; i++){
          if (srcArraybundle != NULL){
            srcArray = srcArrayVector[i];
            void **larrayBaseAddrList = srcArray->getLarrayBaseAddrList();
            for (int j=0; j<srcArray->getDELayout()->getLocalDeCount(); j++){
              char *rraElement = (char *)larrayBaseAddrList[j];
              rraList.push_back(rraElement);
            }
          }
          if (dstArraybundle != NULL){
            dstArray = dstArrayVector[i];
            void **larrayBaseAddrList = dstArray->getLarrayBaseAddrList();
            for (int j=0; j<dstArray->getDELayout()->getLocalDeCount(); j++){
              char *rraElement = (char *)larrayBaseAddrList[j];
              rraList.push_back(rraElement);
            }
          }
          // see if xxe sub element indicates okay for super-vectorization
          bool superVectorOkay = xxe->getNextSubSuperVectorOkay(&k);
          int vectorL = 0;  // initialize
          // src-side super vectorization
          int srcLocalDeCount = 0;
          if (srcArraybundle != NULL)
            srcLocalDeCount = srcArray->getDELayout()->getLocalDeCount();
          int *srcSuperVecSizeUnd = new int[3];   // undistributed: r, s, t
          int **srcSuperVecSizeDis = new int*[2]; // distributed: i, j
          srcSuperVecSizeDis[0] = new int[srcLocalDeCount];
          srcSuperVecSizeDis[1] = new int[srcLocalDeCount];
          Array::superVecParam(srcArray, srcLocalDeCount, superVectorOkay,
            srcSuperVecSizeUnd, srcSuperVecSizeDis, vectorL);
          // dst-side super vectorization
          int dstLocalDeCount = 0;
          if (dstArraybundle != NULL)
            dstLocalDeCount = dstArray->getDELayout()->getLocalDeCount();
          int *dstSuperVecSizeUnd = new int[3];   // undistributed: r, s, t
          int **dstSuperVecSizeDis = new int*[2]; // distributed: i, j
          dstSuperVecSizeDis[0] = new int[dstLocalDeCount];
          dstSuperVecSizeDis[1] = new int[dstLocalDeCount];
          Array::superVecParam(dstArray, dstLocalDeCount, superVectorOkay,
            dstSuperVecSizeUnd, dstSuperVecSizeDis, vectorL);
          XXE::SuperVectP superVectP;
          superVectP.srcSuperVecSize_r = srcSuperVecSizeUnd[0];
          superVectP.srcSuperVecSize_s = srcSuperVecSizeUnd[1];
          superVectP.srcSuperVecSize_t = srcSuperVecSizeUnd[2];
          superVectP.srcSuperVecSize_i = srcSuperVecSizeDis[0];
          superVectP.srcSuperVecSize_j = srcSuperVecSizeDis[1];
          superVectP.dstSuperVecSize_r = dstSuperVecSizeUnd[0];
          superVectP.dstSuperVecSize_s = dstSuperVecSizeUnd[1];
          superVectP.dstSuperVecSize_t = dstSuperVecSizeUnd[2];
          superVectP.dstSuperVecSize_i = dstSuperVecSizeDis[0];
          superVectP.dstSuperVecSize_j = dstSuperVecSizeDis[1];
          delete [] srcSuperVecSizeUnd;
          delete [] srcSuperVecSizeDis;
          delete [] dstSuperVecSizeUnd;
          delete [] dstSuperVecSizeDis;
          // push info into the vectors
          vectorLength.push_back(vectorL);
          srcLocalDeCountList.push_back(srcLocalDeCount);
          superVectPList.push_back(superVectP);
        }
      }
      int rraCount = rraList.size();
      // set filterBitField  
      int filterBitField = 0x0; // init. to execute _all_ operations in XXE
      if (count == 0){
        // use SRCPET as default setting
        filterBitField |= XXE::filterBitNbTestFinish; // set NbTestFinish filter
        filterBitField |= XXE::filterBitCancel;       // set Cancel filter    
        filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
#ifdef SMMINFO_on
        ESMC_LogDefault.Write("AB/SMM exec: TERMORDER_SRCPET (default)",
          ESMC_LOGMSG_INFO);
#endif
      }else{
        if (termOrders[0] == ESMC_TERMORDER_SRCSEQ){
          filterBitField |= XXE::filterBitNbWaitFinish; // set NbWaitFinish filter
          filterBitField |= XXE::filterBitNbTestFinish; // set NbTestFinish filter
          filterBitField |= XXE::filterBitCancel;       // set Cancel filter
#ifdef SMMINFO_on
        ESMC_LogDefault.Write("AB/SMM exec: TERMORDER_SRCSEQ",
          ESMC_LOGMSG_INFO);
#endif
        }else if (termOrders[0] == ESMC_TERMORDER_SRCPET){
          filterBitField |= XXE::filterBitNbTestFinish; // set NbTestFinish filter
          filterBitField |= XXE::filterBitCancel;       // set Cancel filter    
          filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
#ifdef SMMINFO_on
          ESMC_LogDefault.Write("AB/SMM exec: TERMORDER_SRCPET",
            ESMC_LOGMSG_INFO);
#endif
        }else if (termOrders[0] == ESMC_TERMORDER_FREE){
          // not safe to use FREE for AB routehandle -> use TERMORDER_SRCPET
          // settings here...
//          filterBitField |= XXE::filterBitNbWaitFinish; // set NbWaitFinish filter
//          filterBitField |= XXE::filterBitCancel;       // set Cancel filter    
//          filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
          filterBitField |= XXE::filterBitNbTestFinish; // set NbTestFinish filter
          filterBitField |= XXE::filterBitCancel;       // set Cancel filter    
          filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
#ifdef SMMINFO_on
          ESMC_LogDefault.Write("AB/SMM exec: TERMORDER_FREE -> TERMORDER_SRCPET",
            ESMC_LOGMSG_INFO);
#endif
        }
      }
      if (zeroflag!=ESMC_REGION_TOTAL)
        filterBitField |= XXE::filterBitRegionTotalZero;  // filter reg. total zero
      if (zeroflag!=ESMC_REGION_SELECT)
        filterBitField |= XXE::filterBitRegionSelectZero; // filter reg. select zero
      // execute XXE stream
      localrc = xxe->exec(rraCount, &(rraList[0]), &(vectorLength[0]), 
        filterBitField, NULL, NULL, NULL, -1, -1,
        // following are super-vectorization parameters
        &(srcLocalDeCountList[0]), &(superVectPList[0]));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
      // garbage collection
      for (unsigned i=0; i<superVectPList.size(); i++){
        delete [] superVectPList[i].srcSuperVecSize_i;
        delete [] superVectPList[i].srcSuperVecSize_j;
        delete [] superVectPList[i].dstSuperVecSize_i;
        delete [] superVectPList[i].dstSuperVecSize_j;
      }
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }else{
      // unimplemented branch
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "only ESMC_ARRAYXXE and ESMC_ARRAYBUNDLEXXE are supported",
        ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return, don't set success, this is multi exit method
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::sparseMatMulRelease()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::sparseMatMulRelease
//
// !INTERFACE:
int ArrayBundle::sparseMatMulRelease(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  RouteHandle *routehandle        // inout -
  ){    
//
// !DESCRIPTION:
//    Release information for an ArrayBundle sparse matrix multiplication
//    operation.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{
  
  // get XXE from routehandle
  XXE *xxe = (XXE *)routehandle->getStorage();
  
#define XXEPROFILEPRINT___disable
#ifdef XXEPROFILEPRINT
    // print XXE stream profile
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    int localPet = vm->getLocalPet();
    int petCount = vm->getPetCount();
    char file[160];
    sprintf(file, "asmmprofile.%05d", localPet);
    FILE *fp = fopen(file, "a");
    fprintf(fp, "\n=================================================="
      "==============================\n");
    fprintf(fp, "=================================================="
      "==============================\n\n");
    for (int pet=0; pet<petCount; pet++){
      if (pet==localPet){
        localrc = xxe->printProfile(fp);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
      }
      vm->barrier();
    }
    fclose(fp);
#endif
  
  // delete xxe
  delete xxe;

  // mark storage pointer in RouteHandle as invalid  
  routehandle->setStorage(NULL);
  
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// serialize() and deserialize()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::serialize()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::serialize - Turn ArrayBundle into byte stream
//
// !INTERFACE:
int ArrayBundle::serialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length
  int *offset,           // inout - original offset
  const ESMC_AttReconcileFlag &attreconflag,   // in - attreconcile flag
  const ESMC_InquireFlag &inquireflag) const { // in - inquireflag
//
// !DESCRIPTION:
//    Turn info in ArrayBundle class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Prepare pointer variables of different types
  char *cp;
  int *ip;
  int r;

  // Check if buffer has enough free memory to hold object
  if ((inquireflag != ESMF_INQUIREONLY) && (*length - *offset) <
    (int)sizeof(ArrayBundle)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add an ArrayBundle object", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Serialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc =
    ESMC_Base::ESMC_Serialize(buffer,length,offset,attreconflag,inquireflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // Serialize the ArrayBundle with all its Arrays
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ip = (int *)(buffer + *offset);
  if (inquireflag != ESMF_INQUIREONLY)
    *ip++ = getCount();
  else
    ip++;

  cp = (char *)ip;
  *offset = (cp - buffer);
  vector<Array *> arrayVector;
  getVector(arrayVector, ESMC_ITEMORDER_ADDORDER);
  for (int i=0; i<getCount(); i++){
    localrc =
      arrayVector[i]->serialize(buffer,length,offset,attreconflag,inquireflag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::deserialize - Turn byte strm into ArrayBundle
//
// !INTERFACE:
int ArrayBundle::deserialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // in - byte stream to read
  int *offset,           // inout - original offset
  const ESMC_AttReconcileFlag &attreconflag) {  // in - attreconcile flag
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Prepare pointer variables of different types
  char *cp;
  int *ip;
  int r;
  
  // Deserialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc = ESMC_Base::ESMC_Deserialize(buffer,offset,attreconflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // Deserialize the ArrayBundle with all its Arrays
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ip = (int *)(buffer + *offset);
  int arrayCount = *ip++;
  cp = (char *)ip;
  *offset = (cp - buffer);
  for (int i=0; i<arrayCount; i++){
    Array *array = new Array(-1); // prevent baseID counter increment
    array->deserialize(buffer,offset,attreconflag);
    arrayContainer.add(string(array->getName()), array, true);
  }
  arrayCreator = true;  // deserialize creates local Array objects
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


} // namespace ESMCI
