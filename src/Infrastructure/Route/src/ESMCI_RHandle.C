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
#define ESMC_FILENAME "ESMCI_RHandle.C"
//==============================================================================
#define RH_CREATE_MEMLOG_off
//==============================================================================
//
// RouteHandle class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ RouteHandle methods declared
// in the companion file ESMCI_RHandle.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_RHandle.h"

// include higher level, 3rd party or system headers
#include <cerrno>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <sstream>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_Array.h"
#include "ESMCI_ArrayBundle.h"

// LogErr headers
#include "ESMCI_LogErr.h"                  // for LogErr

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = 
  "$Id$";
//-----------------------------------------------------------------------------


namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::create()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::create - Create a new RouteHandle
//
// !INTERFACE:
RouteHandle *RouteHandle::create(
//
// !RETURN VALUE:
//  pointer to newly allocated RouteHandle
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Allocate memory for a new RouteHandle object and initialize it. 
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  RouteHandle *routehandle = NULL;
  try{

    // new object
    routehandle = new RouteHandle;

    // construct initial internals
    localrc = routehandle->construct();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      throw localrc;
    }
    
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    if (routehandle)
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::RouteHandle.", ESMC_CONTEXT, 
      rc);  
    if (routehandle)
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return routehandle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::create()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::create - Create a new RouteHandle from RH
//
// !INTERFACE:
RouteHandle *RouteHandle::create(
//
// !RETURN VALUE:
//  pointer to newly allocated RouteHandle
//
// !ARGUMENTS:
    RouteHandle *rh,                // in  - routehandle to copy from
    InterArray<int> *originPetList, // in  - petList of ncoming RH
    InterArray<int> *targetPetList, // in  - petList of newly created RH
    int *rc) {                      // out - return code
//
// !DESCRIPTION:
//  Allocate memory for a new RouteHandle object and initialize it.
//  Then copy internals of incoming 'rh' into the newly create object.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  RouteHandle *routehandle = NULL;
  try{
    
    // sanity check the incoming petList arguments
    int sizePetList = 0;  // default
    if (present(originPetList))
      sizePetList = originPetList->extent[0];
    int sizeTargetPetList = 0;  // default
    if (present(targetPetList))
      sizeTargetPetList = targetPetList->extent[0];
    if (sizePetList != sizeTargetPetList){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "Both petList arguments must specify the same number of PETs",
        ESMC_CONTEXT, &localrc);
      throw localrc;  // bail out with exception
    }
    bool petMapping = false;  // default
    if (sizePetList>0)
      petMapping = true;
    
    // construct mapping vectors
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) throw localrc;
    int petCount = vm->getPetCount();
    vector<int> originToTargetMap(petCount,-1);  // initialize to -1
    vector<int> targetToOriginMap(petCount,-1);  // initialize to -1
    if (petMapping){
      for (int i=0; i<sizePetList; i++){
        int originPet=originPetList->array[i];
        int targetPet=targetPetList->array[i];
        if (originPet<0 || originPet>=petCount){
          // this PET is out of bounds
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "PETs in originPetList must be between 0 and petCount-1",
            ESMC_CONTEXT, &localrc);
          throw localrc;  // bail out with exception
        }
        if (targetPet<0 || targetPet>=petCount){
          // this PET is out of bounds
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "PETs in targetPetList must be between 0 and petCount-1",
            ESMC_CONTEXT, &localrc);
          throw localrc;  // bail out with exception
        }
        // set up originToTargetMap
        if (originToTargetMap[originPet] != -1){
          // this same PET was already in the petList
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "There must be no duplicate PETs in the originPetList",
            ESMC_CONTEXT, &localrc);
          throw localrc;  // bail out with exception
        }
        originToTargetMap[originPet] = targetPet;
        // set up targetToOriginMap
        if (targetToOriginMap[targetPet] != -1){
          // this same PET was already in the petList
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "There must be no duplicate PETs in the targetPetList",
            ESMC_CONTEXT, &localrc);
          throw localrc;  // bail out with exception
        }
        targetToOriginMap[targetPet] = originPet;
      }
    }
    
    // new RH object
    routehandle = new RouteHandle;

    // construct initial internals
    localrc = routehandle->construct();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      throw localrc;
    }
    
    // copy the information from the incoming RH
    // keep htype the same
    routehandle->htype = rh->htype;
    
    // get to the XXE object
    XXE *xxe = (XXE *)rh->getStorage();
    
    // streamify the XXE object
    stringstream *xxeStreami = new stringstream;  // explicit mem management
    xxe->streamify(*xxeStreami);
    
#ifdef RH_CREATE_MEMLOG_on
  VM::logMemInfo(std::string(ESMC_METHOD": right after creating xxeStreami"));
#endif
  
    if (petMapping){
      
      // Need to do the streami shuffle from origin -> target PETs
  
      // copy the contents of xxeStreami into a contiguous string
      string sendStreamiStr = xxeStreami->str();
      string::size_type sendStreamiSize = sendStreamiStr.size();
      // delete xxeStreami since it is going to be replaced
      delete xxeStreami;
      xxeStreami=NULL;  // make invalid streami identifiable
    
#ifdef RH_CREATE_MEMLOG_on
  VM::logMemInfo(std::string(ESMC_METHOD": right after creating sendStreamiStr"));
#endif

      // exchange the streami between PETs
      int localPet = vm->getLocalPet();
      bool iAmOrigin = false;
      if (originToTargetMap[localPet]>-1)
        iAmOrigin = true;
      bool iAmTarget = false;
      if (targetToOriginMap[localPet]>-1)
        iAmTarget = true;
      // prepare to receive size of streami from origin
      string::size_type recvStreamiSize = 0;  // initialize
      VMK::commhandle *recvCommH = new VMK::commhandle;
      if (iAmTarget){
        // post non-blocking receive of size of streami from origin PET
        localrc = vm->recv(&recvStreamiSize, sizeof(recvStreamiSize),
          targetToOriginMap[localPet], &recvCommH);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)){
          throw localrc;
        }
      }
      // prepare to send size of streami to target
      if (iAmOrigin){
        // send size of the local streami to the target PET
        localrc = vm->send(&sendStreamiSize, sizeof(sendStreamiSize),
          originToTargetMap[localPet]);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)){
          throw localrc;
        }
      }
      // wait for receive
      if (iAmTarget){
        // wait for the non-blocking receive to finish
        localrc = vm->commwait(&recvCommH);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)){
          throw localrc;
        }
      }
    
      // prepare to receive streami from origin
      char *recvMsg = new char[recvStreamiSize];
      if (iAmTarget){
        // post non-blocking receive of streami from origin PET
        localrc = vm->recv(recvMsg, recvStreamiSize,
          targetToOriginMap[localPet], &recvCommH);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)){
          throw localrc;
        }
      }
      // prepare to send streami to target
      char const *sendMsg = sendStreamiStr.data();
      if (iAmOrigin){
        // send size of the local streami to the target PET
        localrc = vm->send(sendMsg, sendStreamiSize,
          originToTargetMap[localPet]);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)){
          throw localrc;
        }
      }
      // wait for receive
      if (iAmTarget){
        // wait for the non-blocking receive to finish
        localrc = vm->commwait(&recvCommH);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)){
          throw localrc;
        }
      }
    
#ifdef RH_CREATE_MEMLOG_on
  VM::logMemInfo(std::string(ESMC_METHOD": right after receiving recvMsg"));
#endif
      
      if (iAmTarget){
        // recreate xxeStreami and fill with recvMsg
        xxeStreami = new stringstream;
        xxeStreami->str(string(recvMsg, recvStreamiSize));
      }
      // collect garbage
      delete recvCommH;
      delete [] recvMsg;
    }
    
    // construct a new XXE object from streamified form
    XXE *xxeNew;
    if (xxeStreami){
      // a valid streami is present on this PET
      if (petMapping)
        xxeNew = new XXE(*xxeStreami, &originToTargetMap);
      else
        xxeNew = new XXE(*xxeStreami);
    }else{
      // a valid streami is NOT present on this PET
      xxeNew = new XXE(vm, 0, 0, 0); // noop on this PET
    }
    
    // collect garbage
    delete xxeStreami;
    
    // store the new XXE object in RH
    routehandle->setStorage(xxeNew);
    // do NOT copy any of the other members!
    
#ifdef RH_CREATE_MEMLOG_on
  VM::logMemInfo(std::string(ESMC_METHOD": right after creating xxeNew"));
#endif

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    if (routehandle)
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::RouteHandle.", ESMC_CONTEXT, 
      rc);  
    if (routehandle)
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return routehandle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::create()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::create - Create a new RouteHandle from file
//
// !INTERFACE:
RouteHandle *RouteHandle::create(
//
// !RETURN VALUE:
//  pointer to newly allocated RouteHandle
//
// !ARGUMENTS:
    const std::string &file,        // in  - name of file read in
    int *rc) {                      // out - return code
//
// !DESCRIPTION:
//  Allocate memory for a new RouteHandle object and initialize it.
//  Then read the RouteHandle from file.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  RouteHandle *routehandle = NULL;
  try{
    // access the current VM
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) throw localrc;
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    MPI_Comm comm = vm->getMpi_c();
    
    // open the file
#ifdef ESMF_MPIUNI
    FILE *fp=fopen(file.c_str(), "rb");
    if (!fp) {
      string msg = file + ": " + strerror (errno);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN, msg,
          ESMC_CONTEXT,
          &localrc);
      throw ESMC_RC_FILE_OPEN;
    }
#else
    MPI_File fh;
    localrc = MPI_File_open(comm, (char*)file.c_str(), 
      MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw ESMC_RC_FILE_OPEN;
#endif

    // read the header start
    char header[30];
    sprintf(header, "ESMF_RouteHandle file v%04d", 1); // version 1
    char headerIn[30];
#ifdef ESMF_MPIUNI
    fread(headerIn, strlen(header), sizeof(char), fp);
#else
    localrc = MPI_File_read(fh, headerIn, strlen(header), MPI_CHAR,
      MPI_STATUS_IGNORE);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    if (strncmp(headerIn, header, strlen(header)) != 0){
      // did not find the expected header start
      std::string msg = std::string("Unknown ESMF_RouteHandle file header: ") + headerIn;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED, msg,
        ESMC_CONTEXT,
        &localrc);
      throw localrc;
    }
    
    // read and check petCount
    int petCountIn;
#ifdef ESMF_MPIUNI
    fread(&petCountIn, 1, sizeof(int), fp);
#else
    localrc = MPI_File_read(fh, &petCountIn, 1, MPI_INT, MPI_STATUS_IGNORE);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    if (petCountIn != petCount){
      // did not find the expected petCount
      stringstream msg;
      msg << "The petCount of the reading context is " << petCount <<
        ", and must match the petCount in the RouteHandle file: " <<
        petCountIn;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED, msg.str(), ESMC_CONTEXT,
        &localrc);
      throw localrc;
    }

    // new RH object
    routehandle = new RouteHandle;

    // construct initial internals
    localrc = routehandle->construct();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      throw localrc;
    }

    // set the htype
#ifdef ESMF_MPIUNI
    fread(&(routehandle->htype), 1, sizeof(int), fp);
#else
    localrc = MPI_File_read(fh, &(routehandle->htype), 1, MPI_INT,
      MPI_STATUS_IGNORE);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    
#ifdef ESMF_MPIUNI
    // for mpiuni, read streamiSize instead of displacment
    unsigned long size;
    fread(&size, 1, sizeof(unsigned long), fp);
#else
    // each PET reads its local displacement
    unsigned long disp;
#define BUG_MPI_SEEK_CUR
#ifdef BUG_MPI_SEEK_CUR
    // some MPI implementations have a bug wrt MPI_SEEK_CUR in MPI_File_seek()
    // work around this by using MPI_SEEK_SET instead.
    MPI_Offset currOffset;
    localrc = MPI_File_get_position(fh, &currOffset);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
    localrc = MPI_File_seek(fh, currOffset+localPet*sizeof(disp), MPI_SEEK_SET);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#else
    // without the bug wrt MPI_SEEK_CUR, the code is more straight forward
    localrc = MPI_File_seek(fh, localPet*sizeof(disp), MPI_SEEK_CUR);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    localrc = MPI_File_read(fh, &disp, 1, MPI_UNSIGNED_LONG, MPI_STATUS_IGNORE);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
    unsigned long size;
    if (localPet<petCount-1){
      localrc = MPI_File_read(fh, &size, 1, MPI_UNSIGNED_LONG,
        MPI_STATUS_IGNORE);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
    }else{
      MPI_Offset sizeHelp;
      localrc = MPI_File_get_size(fh, &sizeHelp);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      size = (unsigned long)sizeHelp;
    }
    size -= disp;
    
    // set the PET specific view
    localrc = MPI_File_set_view(fh, disp, MPI_BYTE, MPI_BYTE, (char*)"native",
      MPI_INFO_NULL);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    
    // read the streami string from file and close
    char *readMsg = new char[size];
#ifdef ESMF_MPIUNI
    fread(readMsg, size, sizeof(char), fp);
    fclose(fp);
#else
    localrc = MPI_File_read(fh, readMsg, size, MPI_BYTE, MPI_STATUS_IGNORE);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
    localrc = MPI_File_close(&fh);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    
    // setup streami from string
    stringstream *xxeStreami = new stringstream;  // explicit mem management
    xxeStreami->str(string(readMsg, size));
    delete [] readMsg;
    
    // construct a new XXE object from streamified form
    XXE *xxeNew;
    if (xxeStreami){
      // a valid streami is present on this PET
      xxeNew = new XXE(*xxeStreami);
    }else{
      //TODO: flag this as an error
    }
    
    // collect garbage
    delete xxeStreami;
    
    // store the new XXE object in RH
    routehandle->setStorage(xxeNew);

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    if (routehandle)
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::RouteHandle.", ESMC_CONTEXT, 
      rc);  
    if (routehandle)
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }


  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return routehandle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::destroy()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::destroy - free a RouteHandle
//
// !INTERFACE:
int RouteHandle::destroy(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    RouteHandle *routehandle,   // in - RouteHandle object to destroy
    bool noGarbage){            // in - remove from garbage collection
//
// !DESCRIPTION:
//  ESMF routine which destroys a RouteHandle object.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // return with errors for NULL pointer
  if (routehandle == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to RouteHandle", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // destruct RouteHandle object
  localrc = routehandle->destruct();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // mark as invalid object
  routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  
  // optionally delete the complete object and remove from garbage collection
  if (noGarbage){
    VM::rmObject(routehandle); // remove object from garbage collection
    delete routehandle;        // completely delete the object, free heap
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::construct()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::construct - fill a RouteHandle
//
// !INTERFACE:
int RouteHandle::construct(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    void){ 
// 
// !DESCRIPTION: 
//  ESMF routine which fills in the contents of an already
//  allocated RouteHandle object
//
//EOP
//-----------------------------------------------------------------------------
  htype = ESMC_UNINITIALIZEDHANDLE;

  for (int i=0; i<RHSTORAGECOUNT; i++)
    storage[i] = NULL;

  srcArray = NULL;
  dstArray = NULL;
  asPtr = NULL;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::destruct()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::destruct - release RouteHandle resources
//
// !INTERFACE:
int RouteHandle::destruct(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  void){
//
// !DESCRIPTION:
//  Deallocates any space allocated by ESMF_RouteHandleConstruct.
//
//EOP
//-----------------------------------------------------------------------------
  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    switch (htype){
    case ESMC_ARRAYXXE:
      ESMCI::Array::sparseMatMulRelease(this);
      break;
    case ESMC_ARRAYBUNDLEXXE:
      ESMCI::ArrayBundle::sparseMatMulRelease(this);
      break;
    default:
      break;
    }
    // remove any additional storage from routehandle
    //TODO: specific to vector<int>* storage, will break for anything else!
    for (int i=1; i<RHSTORAGECOUNT; i++){
      std::vector<int> *tmp = (std::vector<int> *)getStorage(i);
      if (tmp){
        delete tmp;
        setStorage(NULL, i);
      }
    }
    // handle attached state pointer
    if (asPtr){
      delete [] asPtr;
      asPtr = NULL;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::validate()"
//BOP
// !IROUTINE: ESMCI::RouteHandle::validate - validate a RouteHandle
//
// !INTERFACE:
int RouteHandle::validate(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  )const{
//
// !DESCRIPTION:
//  Validates that a RouteHandle is internally consistent.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::print()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::print - print contents of a RouteHandle
//
// !INTERFACE:
int RouteHandle::print(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  )const{
//
// !DESCRIPTION:
//  Print information about a RouteHandle.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{
  
    // get XXE from routehandle
    XXE *xxe = (XXE *)getStorage();
    
    if (xxe == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc);
      return rc;
    }
  
    // write XXE stream profile to file
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    int localPet = vm->getLocalPet();
    int petCount = vm->getPetCount();

#if 0
    char file[160];
    sprintf(file, "xxeprofile.%05d", localPet);
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
    
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
    // -------------------------------------------------------------------------
#define PRINTCOMMMATRIX
#ifdef PRINTCOMMMATRIX
    // get the communication matrix from routehandle
    std::vector<int> *commMatrixDstPet       =(std::vector<int> *)getStorage(1);
    std::vector<int> *commMatrixDstDataCount =(std::vector<int> *)getStorage(2);
    std::vector<int> *commMatrixSrcPet       =(std::vector<int> *)getStorage(3);
    std::vector<int> *commMatrixSrcDataCount =(std::vector<int> *)getStorage(4);
    
    if (commMatrixDstPet == NULL
      || commMatrixDstDataCount == NULL
      || commMatrixSrcPet == NULL
      || commMatrixSrcDataCount == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc);
      return rc;
    }
  
    // construct the communication matrix as an ESMF Array
    ArraySpec as;
    as.set(2, ESMC_TYPEKIND_I4);
    std::vector<int> minIndexV;
    minIndexV.push_back(1);
    minIndexV.push_back(1);
    InterArray<int> minIndex(minIndexV);
    std::vector<int> maxIndexV;
    maxIndexV.push_back(petCount);
    maxIndexV.push_back(petCount);
    InterArray<int> maxIndex(maxIndexV);
    DELayout *delayout = NULL;
    DistGrid *dg = DistGrid::create(&minIndex, &maxIndex, NULL, NULL, 0, NULL,
      NULL, NULL, NULL, NULL, delayout, NULL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    Array *sendMsgArray = Array::create(&as, dg,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    Array *sendDataArray = Array::create(&as, dg,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    Array *recvMsgArray = Array::create(&as, dg,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    Array *recvDataArray = Array::create(&as, dg,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
    LocalArray **localarrayList;
    int *entry;

    // fill the Array with comm matrix values
    localarrayList = sendMsgArray->getLocalarrayList();
    entry = (int *)localarrayList[0]->getBaseAddr();
    for (int i=0; i<petCount; i++)
      entry[i] = 0; // reset
    for (unsigned i=0; i<(*commMatrixDstPet).size(); i++){
      int pet = (*commMatrixDstPet)[i];
      ++entry[pet];
    }
    localrc = sendMsgArray->setName("sendMsgCount_src-dst");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // fill the Array with comm matrix values
    localarrayList = sendDataArray->getLocalarrayList();
    entry = (int *)localarrayList[0]->getBaseAddr();
    for (int i=0; i<petCount; i++)
      entry[i] = 0; // reset
    for (unsigned i=0; i<(*commMatrixDstPet).size(); i++){
      int pet = (*commMatrixDstPet)[i];
      entry[pet] += (*commMatrixDstDataCount)[i];
    }
    localrc = sendDataArray->setName("sendTotalSize_src-dst");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
    // fill the Array with comm matrix values
    localarrayList = recvMsgArray->getLocalarrayList();
    entry = (int *)localarrayList[0]->getBaseAddr();
    for (int i=0; i<petCount; i++)
      entry[i] = 0; // reset
    for (unsigned i=0; i<(*commMatrixSrcPet).size(); i++){
      int pet = (*commMatrixSrcPet)[i];
      ++entry[pet];
    }
    localrc = recvMsgArray->setName("recvMsgCount_dst-src");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // fill the Array with comm matrix values
    localarrayList = recvDataArray->getLocalarrayList();
    entry = (int *)localarrayList[0]->getBaseAddr();
    for (int i=0; i<petCount; i++)
      entry[i] = 0; // reset
    for (unsigned i=0; i<(*commMatrixSrcPet).size(); i++){
      int pet = (*commMatrixSrcPet)[i];
      entry[pet] += (*commMatrixSrcDataCount)[i];
    }
    localrc = recvDataArray->setName("recvTotalSize_dst-src");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
    // write the Arrays to file -- using Array::write()
    bool overwrite = false;
    ESMC_FileStatus_Flag status = ESMC_FILESTATUS_REPLACE;
    localrc = sendMsgArray->write("commMatrix.nc", "", "", "",
      &overwrite, &status, NULL, NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    status = ESMC_FILESTATUS_OLD;
    localrc = sendDataArray->write("commMatrix.nc", "", "", "",
      &overwrite, &status, NULL, NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = recvMsgArray->write("commMatrix.nc", "", "", "",
      &overwrite, &status, NULL, NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = recvDataArray->write("commMatrix.nc", "", "", "",
      &overwrite, &status, NULL, NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
    // destroy the Arrays
    localrc = Array::destroy(&sendMsgArray, true);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = Array::destroy(&sendDataArray, true);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = Array::destroy(&recvMsgArray, true);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = Array::destroy(&recvDataArray, true);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = DistGrid::destroy(&dg, true);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

#endif  // PRINTCOMMMATRIX
#endif  // PIO, etc.
    
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::write()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::write - write RouteHandle to file
//
// !INTERFACE:
int RouteHandle::write(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  const std::string &file         // in    - name of file being written
  )const{
//
// !DESCRIPTION:
//  Write RouteHandle to file.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{
    // access the current VM
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) throw rc;
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    MPI_Comm comm = vm->getMpi_c();
    
    // access the XXE as a stream
    XXE *xxe = (XXE *)getStorage();
    stringstream *xxeStreami = new stringstream;  // explicit mem management
    xxe->streamify(*xxeStreami);
    // copy the contents of xxeStreami into a contiguous string
    string writeStreamiStr(xxeStreami->str());
    unsigned long writeStreamiSize = (unsigned long)writeStreamiStr.size();
    delete xxeStreami;  // garbage collection
    
    // open the file
#ifdef ESMF_MPIUNI
    FILE *fp=fopen(file.c_str(), "wb");
#else
    MPI_File fh;
    localrc = MPI_File_open(comm, (char*)file.c_str(), 
      MPI_MODE_WRONLY | MPI_MODE_CREATE, MPI_INFO_NULL, &fh);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
    // make sure that if file existed before, size is reset
    localrc = MPI_File_set_size(fh, 0);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
    localrc = MPI_Barrier(comm);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
  
    // write the file header
#ifdef ESMF_MPIUNI
#else    
    unsigned long *displacements; // only root will use this
    MPI_Offset headDisp;  // only root will use this
#endif
    if (localPet==0){
      char header[30];
      sprintf(header, "ESMF_RouteHandle file v%04d", 1); // version 1
#ifdef ESMF_MPIUNI
      fwrite(header, strlen(header), sizeof(char), fp);
      fwrite(&petCount, 1, sizeof(int), fp);
      fwrite(&htype, 1, sizeof(int), fp);
#else
      localrc = MPI_File_write(fh, header, strlen(header), MPI_CHAR,
        MPI_STATUS_IGNORE);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      localrc = MPI_File_write(fh, &petCount, 1, MPI_INT, MPI_STATUS_IGNORE);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      localrc = MPI_File_write(fh, (void*)&htype, 1, MPI_INT, MPI_STATUS_IGNORE);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
      
      // later versions may add header info here
      
#ifdef ESMF_MPIUNI
#else      
      // query the current file position (where displacements will be written)
      MPI_Offset offsetKeep;
      localrc = MPI_File_get_position(fh, &offsetKeep);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      // write petCount many dummy displacements to finish header
      displacements = new unsigned long[petCount];
      memset(displacements, 0, petCount*sizeof(unsigned long)); // fill w/ zero
      localrc = MPI_File_write(fh, displacements, petCount, MPI_UNSIGNED_LONG,
        MPI_STATUS_IGNORE);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      // query the current file position (right after the header)
      MPI_Offset offset;
      localrc = MPI_File_get_position(fh, &offset);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      // convert into a byte displacement
      localrc = MPI_File_get_byte_offset(fh, offset, &headDisp);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      // on root add the header displacment to the writeStreamiSize for all
      writeStreamiSize += headDisp;
      // reset root position back to where to write displacements
      localrc = MPI_File_seek(fh, offsetKeep, MPI_SEEK_SET);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    }
 
#ifdef ESMF_MPIUNI
    // for mpiuni, write streamiSize instead of displacment
    fwrite(&writeStreamiSize, 1, sizeof(unsigned long), fp);
#else
    // scan across PETs to determine the local displacement (with header)
    unsigned long disp;
    MPI_Scan(&writeStreamiSize, &disp, 1, MPI_UNSIGNED_LONG, MPI_SUM, comm);
    if (localPet==0) writeStreamiSize -= headDisp; // correct back
    disp -= writeStreamiSize;  // correct displacement from the inclusive scan

    // gather all the displamements on the root PET for writing into the header
    MPI_Gather(&disp, 1, MPI_UNSIGNED_LONG, displacements, 1, MPI_UNSIGNED_LONG,
      0, comm);
    
    if (localPet==0){
      // write the actual displacments into the header
      localrc = MPI_File_write(fh, displacements, petCount, MPI_UNSIGNED_LONG,
        MPI_STATUS_IGNORE);
      if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
      delete [] displacements;  // garbage collection
    }
    
    localrc = MPI_File_set_view(fh, disp, MPI_BYTE, MPI_BYTE, (char*)"native",
      MPI_INFO_NULL);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    
    // write the actual stream and close
#ifdef ESMF_MPIUNI
    fwrite(writeStreamiStr.data(), writeStreamiSize, sizeof(char), fp);
    fclose(fp);
#else
    localrc = MPI_File_write(fh, (void*)writeStreamiStr.data(),
      writeStreamiSize, MPI_BYTE, MPI_STATUS_IGNORE);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
    localrc = MPI_File_close(&fh);
    if (VM::MPIError(localrc, ESMC_CONTEXT)) throw localrc;
#endif
    
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::optimize()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::optimize - optimize for communication pattern
//
// !INTERFACE:
int RouteHandle::optimize(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  )const{
//
// !DESCRIPTION:
//  Optimize for the communication pattern stored in the RouteHandle.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{

#if 0
    ESMC_LogDefault.Write("Entering RouteHandle::optimize()", ESMC_LOGMSG_DEBUG,
      ESMC_CONTEXT);
#endif

    // get the communication matrix from routehandle
    std::vector<int> *commMatrixDstPet       =(std::vector<int> *)getStorage(1);
    std::vector<int> *commMatrixDstDataCount =(std::vector<int> *)getStorage(2);
    std::vector<int> *commMatrixSrcPet       =(std::vector<int> *)getStorage(3);
    std::vector<int> *commMatrixSrcDataCount =(std::vector<int> *)getStorage(4);
    
    if (commMatrixDstPet == NULL
      || commMatrixDstDataCount == NULL
      || commMatrixSrcPet == NULL
      || commMatrixSrcDataCount == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc);
      return rc;
    }
  
    
        
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::isCompatible()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::isCompatible - RouteHandle is compatible with the src/dst arrays
//
// !INTERFACE:
bool RouteHandle::isCompatible(
//
// !RETURN VALUE:
//  true if compatible, false if not compatible
//
// !ARGUMENTS:
    Array *srcArrayArg,
    Array *dstArrayArg,
    int *rc                                       // (out) return code
  )const{
//
// !DESCRIPTION:
//  Check whether the routehandle object is compatible with the specified
//  srcArray -> dstArray arguments.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  //TODO: for now only check srcMatch, because of the specific application
  //TODO: where I need this for, the dstArray is a derivative of the srcArray,
  //TODO: so if the srcArray matches the dstArray will, too. Plus the dstArray
  //TODO: happens to be a temporary array that will not be valid when I come 
  //TODO: back to check anyway! All this needs to be fixed by introducing
  //TODO: persistent Array finger prints.
  //TODO: The other thing that needs to be considered when implementing real
  //TODO: fingerprinting here is that RHs also function for a large class of
  //TODO: compatible Arrays. This is especially true now that 
  //TODO: super-vectorization is implemented!
  bool srcMatch = Array::matchBool(srcArrayArg, srcArray, &localrc);
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);

#if 0
  std::stringstream debugmsg;
  debugmsg << "RouteHandle::isCompatible(), srcMatch=" << srcMatch;
  ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
#endif

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return srcMatch;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
