// $Id: ESMCI_AttributeUpdate.C,v 1.20.2.1 2010/02/05 19:53:40 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMCI_AttributeUpdate.C"

// Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include "ESMC_Start.h"
#include "ESMCI_Attribute.h"
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_AttributeUpdate.C,v 1.20.2.1 2010/02/05 19:53:40 svasquez Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

// class wide keySize
static const int keySize = 4*sizeof(int) + 2*sizeof(bool) + 1;

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// UPDATE ROUTINES:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdate"
//BOPI
// !IROUTINE:  AttributeUpdate - update an {\tt Attribute}
//
// !INTERFACE:
      int Attribute::AttributeUpdate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      VM *vm,                    // the VM
      const vector<ESMC_I4> &roots) {      // the rootList
//
// !DESCRIPTION:
//    Update an {\tt Attribute} hierarchy with a later version of itself.  
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
  char *recvBuf, *sendBuf;
  int offset = 0;
  int length = 0;
  int recvlength = 0;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to an Attribute", &localrc);
    return ESMF_FAILURE;
  }
    
  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  
  // make the roots and nonroots vectors
  vector<ESMC_I4> nonroots;
  vector<ESMC_I4>::const_iterator it;
  for(i=0; i<petCount; ++i) {
    it = find(roots.begin(), roots.end(), i);
    if(it == roots.end()) nonroots.push_back(i);
  }
    
  // find out if update is necessary
  localrc = AttributeUpdateNeeded(vm, length, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.Write(
                  "AttributeUpdateNeeded failed", ESMC_LOG_INFO);
    return ESMF_SUCCESS;
  }
   
  // find out if I am a root
  it = find(nonroots.begin(), nonroots.end(), localPet);
  
  // allocate buffers
  recvBuf = NULL; sendBuf = NULL;
  recvBuf = new char[length];
  sendBuf = new char[length];
  if (!recvBuf) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                  "Failed allocating buffer", &localrc);
    return localrc;
  }
  if (!sendBuf) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                  "Failed allocating buffer", &localrc);
    return localrc;
  }
 
  // I am a root, create buffer
  if (it == nonroots.end()) {
    localrc = AttributeUpdateBufSend(sendBuf, localPet, &offset, &length);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
  }

  // roots send to nonroots
  localrc = AttributeUpdateComm(vm, offset, &recvlength, sendBuf, recvBuf, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateComm failed", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
    
  // I am a nonroot, unpack buffer
  if (it != nonroots.end()) {
      localrc = AttributeUpdateBufRecv(recvBuf, localPet, &offset, recvlength);
      if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
  }

  // all set flags to false
  localrc = AttributeUpdateReset();
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateReset failed", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  
  delete [] recvBuf;
  delete [] sendBuf;

  return ESMF_SUCCESS;

 } // end AttributeUpdate
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateBufRecv"
//BOPI
// !IROUTINE:  AttributeUpdateBufRecv - unpack the serialized Attribute updates
//
// !INTERFACE:
      int Attribute::AttributeUpdateBufRecv(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *recvBuf,                   // buffer with packed updates
      int localPet,                   // localPet for printing
      int *offset,                     // current position in the buffer
      const int &length)  {                 // length of buffer
//
// !DESCRIPTION:
//    Serialize the updates to an {\tt Attribute} hierarchy.  
//    Expected to be called internally.
//
//EOPI

  int localrc, nbytes, loffset;
  Attribute *attr;
  unsigned int i;
  char *thiskey, *distkey;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  thiskey = NULL; distkey = NULL;
  thiskey = new char[keySize];
  if (!thiskey) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                  "Failed allocating key", &localrc);
    return localrc;
  }
  distkey = new char[keySize];
  if (!distkey) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                  "Failed allocating key", &localrc);
    delete [] thiskey;
    return localrc;
  }
    
  // make key
  localrc = AttributeUpdateKeyCreate(thiskey);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey failed", &localrc);
    delete [] thiskey;
    return ESMF_FAILURE;
  }

  // unpack next key
  memcpy(distkey, recvBuf+(*offset), keySize);

  // compare keys
  if (AttributeUpdateKeyCompare(thiskey, distkey) == false) {
  /*  
    printf("DeleteMe!!!\n");
  if (localPet == 4) {
    printf("%d  %s  %s  %s  %d  -  %d  %s  %s  %s  %d\n", 
          (*(reinterpret_cast<int*> (thiskey+0))),
          thiskey+4,
          (*(reinterpret_cast<bool*> (thiskey+5))) ? "true" : "false",
          (*(reinterpret_cast<bool*> (thiskey+6))) ? "true" : "false",
          (*(reinterpret_cast<int*> (thiskey+7))),
          (*(reinterpret_cast<int*> (distkey+0))),
          distkey+4,
          (*(reinterpret_cast<bool*> (distkey+5))) ? "true" : "false",
          (*(reinterpret_cast<bool*> (distkey+6))) ? "true" : "false",
          (*(reinterpret_cast<int*> (distkey+7))));
    }
    */
    if (attrPackHead == ESMF_TRUE) {
      delete [] thiskey;
      delete [] distkey;
      return ESMC_ATTUPDATERM_ATTPACK;
    }
    else if (attrPack == ESMF_TRUE) {
      delete [] thiskey;
      delete [] distkey;
      return ESMC_ATTUPDATERM_ATTPACKATT;
    }
    else {
      delete [] thiskey;
      delete [] distkey;
      return ESMC_ATTUPDATERM_ATTRIBUTE;
    }
  }

  // get key info
  bool valueChange = (*(reinterpret_cast<bool*> (distkey+sizeof(int)+1)));
  bool strctChange = (*(reinterpret_cast<bool*> (distkey+sizeof(int)+1+sizeof(bool))));
  int attrChange = (*(reinterpret_cast<int*> (distkey+sizeof(int)+1+(2*sizeof(bool)))));
  int packChange = (*(reinterpret_cast<int*> (distkey+sizeof(int)+1+(2*sizeof(bool))+sizeof(int))));

  // now update offset
  *offset += keySize;

  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;  
  
  // check if buffer has been overwritten
  if (length < *offset){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Overread the buffer", &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }
  
  // if value change, unpack into temp and set
  if (valueChange) {
    for (i=0; i<attrChange; ++i) {
      attr = NULL;
      attr = new Attribute(ESMF_FALSE);
      if (!attr) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Failed allocating Attribute", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return localrc;
      }
      attr->setBase(attrBase);
      attr->parent = this;
      attr->ESMC_Deserialize(recvBuf,offset);
      localrc = AttributeCopy(*attr);
      // can delete this one and not call reset because this is a value copy
      delete attr;
    }
  }

  // if struct change, traverse and build
  if (strctChange) {
    for (i=0; i<attrChange; ++i) {
      attr = NULL;
      attr = new Attribute(ESMF_FALSE);
      if (!attr) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Failed allocating Attribute", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return localrc;
      }
      attr->setBase(attrBase);
      attr->parent = this;
      attr->ESMC_Deserialize(recvBuf,offset);
      localrc = AttributeSet(attr);
    }
    for (i=0; i<packChange; ++i) {
      attr = NULL;
      attr = new Attribute("42","42","42","42");
      if (!attr) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Failed allocating Attribute", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return localrc;
      }
      attr->setBase(attrBase);
      attr->parent = this;
      attr->ESMC_Deserialize(recvBuf,offset);
      localrc = AttPackSet(attr);
    }
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed adding attribute", &localrc);
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }
    localrc = attr->AttributeUpdateReset();
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed resetting", &localrc);
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }
  }
  
  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;

  // check if buffer has been overwritten
  if (length < *offset){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Overread the buffer", &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }
      
  // recurse through the Attribute hierarchy
  for (i=0; i<attrChange; ++i) {
    localrc = attrList.at(i)->AttributeUpdateBufRecv(recvBuf,localPet,offset,length);
    if (localrc == ESMC_ATTUPDATERM_ATTRIBUTE) 
      localrc = AttributeRemove(attrList.at(i)->attrName);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufRecv failed removing attr", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
  }
  for (i=0; i<packChange; ++i) {
    localrc = packList.at(i)->AttributeUpdateBufRecv(recvBuf,localPet,offset,length);
    if (localrc == ESMC_ATTUPDATERM_ATTPACKATT)
      localrc = AttPackRemoveAttribute(packList.at(i)->attrName, packList.at(i)->attrConvention, 
        packList.at(i)->attrPurpose, packList.at(i)->attrObject);
    else if (localrc == ESMC_ATTUPDATERM_ATTPACK) {
      localrc = AttPackRemove(packList.at(i)->attrConvention, 
        packList.at(i)->attrPurpose, packList.at(i)->attrObject);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed removing attr", &localrc);
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }
    }
    else if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed in recursion", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
    }
  }
  for (i=0; i<linkList.size(); ++i) {
    localrc = linkList.at(i)->AttributeUpdateBufRecv(recvBuf,localPet,offset,length);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "AttributeUpdate not enabled for object hierarchy changes", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
  }
    
  // check if buffer has been overwritten
  if (length < *offset){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Overread the buffer", &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }
    
  delete [] thiskey;
  delete [] distkey;
    
  return ESMF_SUCCESS;
  
  } // end AttributeUpdateBufRecv
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateBufSend"
//BOPI
// !IROUTINE:  AttributeUpdateBufSend - serialize the Attribute updates
//
// !INTERFACE:
      int Attribute::AttributeUpdateBufSend(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *sendBuf,                   // buffer to pack updates
      int localPet,
      int *offset,                     // current position in the buffer
      int *length) const {            // length of buffer
//
// !DESCRIPTION:
//    Deserialize the updates to an {\tt Attribute} hierarchy.  
//    Expected to be called internally.
//
//EOPI

  int localrc, nbytes;
  unsigned int i, j;
  Attribute *attr;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  // check (length - offset) > sizeof(Attribute) + keylength
    
  // make key
  char *key;
  key = NULL;
  key = new char[keySize];
  if (!key) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                  "Failed allocating key", &localrc);
    return localrc;
  }
  localrc = AttributeUpdateKeyCreate(key);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey failed", &localrc);
    delete [] key;
    return ESMF_FAILURE;
  }
    
  // add this key to the buffer
  memcpy(sendBuf+(*offset),key,keySize);
  *offset += keySize;

  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;

  /*if (localPet == 0) {
  printf("%d  %s  %s  %d  -   %d  %s  %s  %d\n", 
                      (*(reinterpret_cast<int*> (key+0))),
                      (*(reinterpret_cast<bool*> (key+4))) ? "true" : "false",
                      (*(reinterpret_cast<bool*> (key+5))) ? "true" : "false",
                      (*(reinterpret_cast<int*> (key+6))),
                      (*(reinterpret_cast<int*> (sendBuf+(*offset)-keySize-(8-nbytes)+0))),
                      (*(reinterpret_cast<bool*> (sendBuf+(*offset)-keySize-(8-nbytes)+4))) ? "true" : "false",
                      (*(reinterpret_cast<bool*> (sendBuf+(*offset)-keySize-(8-nbytes)+5))) ? "true" : "false",
                      (*(reinterpret_cast<int*> (sendBuf+(*offset)-keySize-(8-nbytes)+6))));
}*/
    
  // if value changes
  for (i=0; i<attrList.size(); ++i) { 
    if (attrList.at(i)->valueChange == ESMF_TRUE) {
      localrc = attrList.at(i)->ESMC_Serialize(sendBuf,length,offset,ESMF_NOINQUIRE);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed Serialize", &localrc);
        delete [] key;
        return ESMF_FAILURE;
      }
    }
  }

  // if struct changes
  for (i=0; i<attrList.size(); ++i) {
    if (attrList.at(i)->valueChange == ESMF_TRUE ||
      attrList.at(i)->structChange == ESMF_TRUE) {
      localrc = attrList.at(i)->AttributeUpdateReset();
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufSend failed resetting", &localrc);
        delete [] key;
        return ESMF_FAILURE;
      }
      localrc = attrList.at(i)->ESMC_Serialize(sendBuf,length,offset,ESMF_NOINQUIRE);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed Serialize", &localrc);
        delete [] key;
        return ESMF_FAILURE;
      }
    }
  }
  for (i=0; i<packList.size(); i++) {
    if (packList.at(i)->structChange == ESMF_TRUE) {
      localrc = packList.at(i)->AttributeUpdateReset();
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufSend failed resetting", &localrc);
        delete [] key;
        return ESMF_FAILURE;
      }
      localrc = packList.at(i)->ESMC_Serialize(sendBuf,length,offset,ESMF_NOINQUIRE);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed Serialize", &localrc);
        delete [] key;
        return ESMF_FAILURE;
      }
    }
  }
    
  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;
  
  // recurse through the Attribute hierarchy
  for (i=0; i<linkList.size(); i++) {
    localrc = linkList.at(i)->AttributeUpdateBufSend(sendBuf,localPet,offset,length);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed", &localrc);
      delete [] key;
      return ESMF_FAILURE;
    }
  }
  
  // check if buffer has been overwritten
  if (*length < *offset){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Buffer too short to add an Attribute hierarchy", &localrc);
    delete [] key;
    return localrc;
  }
  
  delete [] key;
  
  return ESMF_SUCCESS;
  
  } // end AttributeUpdateBufSend
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateChanges"
//BOPI
// !IROUTINE:  AttributeUpdateChanges - look for changes in Attribute hierachy
//
// !INTERFACE:
      int Attribute::AttributeUpdateChanges(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int *linkChanges,            // link changes flag
      int *structChanges,          // structural changes flag
      int *valueChanges,           // value changes flag
      int *numKeys) const{        // number of keys flag
//
// !DESCRIPTION:
//    Search an {\tt Attribute} hierarchy for changes to be updated.  
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i,j;
  Attribute *attr;
  
  attr = NULL;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  ++(*numKeys);
  
  for (i=0; i<attrList.size(); ++i) {
    ++(*numKeys);
    if (attrList.at(i)->linkChange == ESMF_TRUE) ++(*linkChanges);
    if (attrList.at(i)->structChange == ESMF_TRUE) {
      if (attrList.at(i)->attrRoot == ESMF_FALSE && 
        attrList.at(i)->attrPackHead == ESMF_FALSE) ++(*structChanges); }
    if (attrList.at(i)->valueChange == ESMF_TRUE) ++(*valueChanges);
  }

  for (i=0; i<packList.size(); ++i)
    localrc = packList.at(i)->AttributeUpdateChanges(linkChanges,
      structChanges, valueChanges, numKeys);
  
  for(i=0; i<linkList.size(); ++i)
    localrc = linkList.at(i)->AttributeUpdateChanges(linkChanges,
      structChanges,valueChanges,numKeys);
  
  return ESMF_SUCCESS;
  
  } // end AttributeUpdateChanges
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateComm"
//BOPI
// !IROUTINE:  AttributeUpdateComm - {\tt Attribute} update comm pattern
//
// !INTERFACE:
      int Attribute::AttributeUpdateComm(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      VM *vm,                    // the VM
      int sendBufSize,                       // size of sendBuf
      int *recvBufSize,                       // size of recvBuf
      char *sendBuf,                         // sendBuf
      char *recvBuf,                         // recvBuf
      const vector<ESMC_I4> &roots,          // roots vector
      const vector<ESMC_I4> &nonroots) const {     // nonroots vector
//
// !DESCRIPTION:
//    Update an {\tt Attribute} hierarchy with a later version of itself.  
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
  int handshake = 42;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  // prepare for comms
  VMK::commhandle **commh = new VMK::commhandle*;
  
  // am I a root?
  vector<ESMC_I4>::const_iterator itSend, itNR, itR;
  itSend = find(roots.begin(), roots.end(), localPet);
      
  /*printf("PET%d - nonroots.size() = %d, roots.size()= %d\n",localPet,
          nonroots.size(), roots.size());*/
  
  int ceilID=ceil(static_cast<double> (nonroots.size())/static_cast<double> (roots.size()));
  int floorID=floor(static_cast<double> (nonroots.size())/static_cast<double> (roots.size()));
    
  // No, I am receiving
  if (itSend == roots.end()) {
    // find this PET in nonroots, bail if it is not present
    itNR = find(nonroots.begin(), nonroots.end(), localPet);
    if (itNR == nonroots.end()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      " - AttributeUpdate - PET not in nonroots vector", &localrc);
      delete commh;
      return ESMF_FAILURE;
    }

    int indRecv = 0;
    // find the index of roots from which to receive
    if (roots.size() < nonroots.size())
      indRecv=fmod(static_cast<double> (*itNR),static_cast<double> (ceilID));
    else
      indRecv = distance(nonroots.begin(), find(nonroots.begin(), nonroots.end(), localPet));
    
    // receive with message=0 and status=NULL for now
    *commh = NULL;
    vm->recv(recvBufSize, sizeof(int), roots[indRecv], commh);
    vm->send(&handshake, sizeof(int), roots[indRecv]);
    vm->commqueuewait();
    *commh = NULL;
    vm->recv(recvBuf, *recvBufSize, roots[indRecv], commh);
    vm->send(&handshake, sizeof(int), roots[indRecv]);
    // now we all wait for all comm calls to complete
    vm->commqueuewait();
    /*printf("\n\nI am PET #%d, I received message \"%s\" from PET #%d\n\n",
            localPet, recvBuf, roots[indRecv]);*/
  }
    
  // Yes, I am sending
  else if (itSend != roots.end()) {
  
    // find this PET in roots, bail if it is not present
    itR = find(roots.begin(), roots.end(), localPet);
    if (itR == roots.end()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      " - AttributeUpdate - PET not in roots vector", &localrc);
      delete commh;
      return ESMF_FAILURE;
    }
    
    for (i=0; i<ceilID; ++i) {
      // find the index of nonroots to which to send
      int indSend = distance(roots.begin(), itR) + i*floorID;
      
      if (indSend < nonroots.size()) {
        // send with message=0 and status=NULL for now
        /*printf("\n\nI am PET #%d, I am sending message \"%s\" to PET #%d\n\n",
                localPet, sendBuf, nonroots[indSend]);*/
        vm->recv(&handshake, sizeof(int), nonroots[indSend]);
        vm->send(&sendBufSize, sizeof(sendBufSize), nonroots[indSend]);
        vm->recv(&handshake, sizeof(int), nonroots[indSend]);
        vm->send(sendBuf, sendBufSize, nonroots[indSend]);
      }
    }
  }
    
  // Dunno who I am, bailing
  else {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
    " - AttributeUpdate - PET unidentified", &localrc);
    delete commh;
    return ESMF_FAILURE;
  }
    
  delete commh;
  return ESMF_SUCCESS;
  
  } // end AttributeUpdateComm
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateKeyCompare"
//BOPI
// !IROUTINE:  AttributeUpdateKeyCompare - Compare Attribute keys
//
// !INTERFACE:
      bool Attribute::AttributeUpdateKeyCompare(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *key1,              // one key
      char *key2) const {      // other key
//
// !DESCRIPTION:
//    Compare Attribute keys.
//    Expected to be called internally.
//
//EOPI
  
  int localrc;
  bool result = true;
  int o = 0;

  if ((*(reinterpret_cast<int*> (key1+o))) != (*(reinterpret_cast<int*> (key2+o)))) {
    result = false;}
  o += sizeof(int);
  
  if (key1[o] != key2[o]) {
    result = false;}
  o += 1;
  
  if (o > keySize) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateKeyCompare key buffer is misaligned", &localrc);
    return ESMF_FAILURE;
  }
  
  return result;
    
  } // end AttributeUpdateKeyCompare
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateKeyCreate"
//BOPI
// !IROUTINE:  AttributeUpdateKeyCreate - Make the key for this Attribute
//
// !INTERFACE:
      int Attribute::AttributeUpdateKeyCreate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *key) const { // this key
//
// !DESCRIPTION:
//    Make the key for this Attribute.
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
  char *xorkey;
  xorkey = new char[1];
  *xorkey = 'a';
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  int offset = 0;

  // copy the baseID
  // FIXME: this is a temporary fix for the Field to Grid Attribute links
  if (attrBase->classID == 42)
    *(reinterpret_cast<int*> (key+offset)) = 42;

  else *(reinterpret_cast<int*> (key+offset)) = attrBase->ESMC_BaseGetID();
  offset += sizeof(int);

  // xor the name into key
  if (!attrName.empty()) {
  for (i=0; i<attrName.size(); ++i)
    *xorkey = *xorkey ^ attrName[i];}
  
  // xor the convention into key
  if (!attrConvention.empty()) {
  for (i=0; i<attrConvention.size(); ++i)
    *xorkey = *xorkey ^ attrConvention[i];}
  
  // xor the purpose into key
  if (!attrPurpose.empty()) {
  for (i=0; i<attrPurpose.size(); ++i)
    *xorkey = *xorkey ^ attrPurpose[i];}
    
  memcpy((key+offset),xorkey,1);
  offset += 1;
   
  // now the value and struct changes booleans
  bool trueval = true;
  bool falseval = false;
  if (valueChange == ESMF_TRUE)
    *(reinterpret_cast<bool*> (key+offset)) = trueval;
  else 
    *(reinterpret_cast<bool*> (key+offset)) = falseval;
  offset += sizeof(bool);
  if (structChange == ESMF_TRUE)
    *(reinterpret_cast<bool*> (key+offset)) = trueval;
  else 
    *(reinterpret_cast<bool*> (key+offset)) = falseval;
  offset += sizeof(bool);
  
  // now the number of struct changes on this attribute
  int attrChanges = 0;
  int packChanges = 0;
  if (structChange == ESMF_TRUE) {
    for (i=0; i<attrList.size(); ++i) {
      if (attrList.at(i)->structChange == ESMF_TRUE)
        ++attrChanges;
    }
    for (i=0; i<packList.size(); ++i) {
      if (packList.at(i)->structChange == ESMF_TRUE) {
        ++packChanges;}
    }
  }
  *(reinterpret_cast<int*> (key+offset)) = attrChanges;
  offset += sizeof(int);
  *(reinterpret_cast<int*> (key+offset)) = packChanges;
  offset += sizeof(int);
  if ( offset > keySize) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey key buffer is misaligned", &localrc);
    delete [] xorkey;
    return ESMF_FAILURE;
  }

  delete [] xorkey;
  return ESMF_SUCCESS;
  
  } // end AttributeUpdateKeyCreate
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateNeeded"
//BOPI
// !IROUTINE:  AttributeUpdateNeeded - Determine if update is needed
//
// !INTERFACE:
      int Attribute::AttributeUpdateNeeded(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      VM *vm,                                     // the VM
      int &bufSize,                               // size of buffer to return
      const vector<ESMC_I4> &roots,               // roots vector
      const vector<ESMC_I4> &nonroots) const {    // nonroots vector
//
// !DESCRIPTION:
//    Determine if AttributeUpdate() is needed. 
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // formalities
  int length, offset, recvlength;
  int linkChangesOut, realChangesOut, numKeysOut;
  vector<ESMC_I4>::const_iterator it;
  char *recvBuf, *sendBuf;
  length = 3*sizeof(int);
  
  offset = 0;
  recvlength = 0;
  linkChangesOut = 0;
  realChangesOut = 0;
  numKeysOut = 0;
  
  recvBuf = NULL; sendBuf = NULL;
  recvBuf = new char[length];
  sendBuf = new char[length];
  if (!recvBuf) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                  "Failed allocating buffer", &localrc);
    return localrc;
  }
  if (!sendBuf) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                  "Failed allocating buffer", &localrc);
    return localrc;
  }

  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();

  // identify myself
  it = find(nonroots.begin(), nonroots.end(), localPet);
  
  // I am a root
  if (it == nonroots.end()) {
    int linkChanges = 0;
    int structChanges = 0;
    int valueChanges = 0;
    int numKeys = 0;

    // look for changes
    localrc = AttributeUpdateChanges(&linkChanges, &structChanges, 
      &valueChanges, &numKeys);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateChanges failed", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
    
    // create buffer
    int realChanges = structChanges+valueChanges;
    (*(reinterpret_cast<int*> (sendBuf+offset)))=linkChanges;
    offset += sizeof(int);
    (*(reinterpret_cast<int*> (sendBuf+offset)))=realChanges;
    offset += sizeof(int);
    (*(reinterpret_cast<int*> (sendBuf+offset)))=numKeys;
    offset += sizeof(int);
    if (offset != length) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded -  writing buffer failed", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
    
    // set Out values
    linkChangesOut = linkChanges;
    realChangesOut = realChanges;
    numKeysOut = numKeys;
  }
  
  // call AttributeUpdateComm with changes as sendBuf and recvBuf
  localrc = AttributeUpdateComm(vm, length, &recvlength, 
        sendBuf, recvBuf, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateComm failed", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  
  // I am a nonroot
  if (it != nonroots.end()) {
  
    // unpack the buffer
    if (recvlength != length) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded - recvBuf not correct size", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
    linkChangesOut = (*(reinterpret_cast<int*> (recvBuf+offset)));
    offset += sizeof(int);
    realChangesOut = (*(reinterpret_cast<int*> (recvBuf+offset)));
    offset += sizeof(int);
    numKeysOut = (*(reinterpret_cast<int*> (recvBuf+offset)));
    offset += sizeof(int);
    if (offset != length) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded -  reading buffer failed", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }  
  }
  
  // if link changes, we bail and recommend StateReconcile
  if (linkChangesOut > 0) {
    ESMC_LogDefault.Write(
                  "Attribute link changes made, call ESMF_StateReconcile() first", ESMC_LOG_INFO);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  // if no changes, nothing to do
  else if (realChangesOut == 0) {
    ESMC_LogDefault.Write(
                  "There is nothing to update, gracefully exiting.", ESMC_LOG_INFO);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  // if funny values, we bail
  else if (realChangesOut < 0 || numKeysOut <= 0) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded something is amiss", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  
  // set return value of buffer size
  bufSize = realChangesOut*sizeof(Attribute) + numKeysOut*keySize;
  
  /*printf("linkChanges = %d, realChangesOut = %d, numKeysOut = %d bufSize = %d\n", 
    linkChangesOut, realChangesOut, numKeysOut, bufSize);*/
  
  delete [] recvBuf;
  delete [] sendBuf;

  return ESMF_SUCCESS;
  
  } // end AttributeUpdateNeeded
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateRemove"
//BOPI
// !IROUTINE:  AttributeUpdateRemove - Remove an Attribute in Update
//
// !INTERFACE:
      int Attribute::AttributeUpdateRemove(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        int attrNum) {          // number of attribute to remove
//
// !DESCRIPTION:
//    Remove an Attribute in {\tt ESMCI\_AttibuteUpdate()}.
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  delete attrList.at(attrNum);
  attrList.erase(attrList.begin() + attrNum);
  structChange = ESMF_TRUE;

  return ESMF_SUCCESS;
  
  } // end AttributeUpdateRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateReset"
//BOPI
// !IROUTINE:  AttributeUpdateReset - Reset flags for AttributeUpdate
//
// !INTERFACE:
      int Attribute::AttributeUpdateReset(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        ) { 
//
// !DESCRIPTION:
//    Reset the flags for AttributeUpdate().
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  linkChange = ESMF_FALSE;
  structChange = ESMF_FALSE;
  valueChange = ESMF_FALSE;

  for(i=0; i<attrList.size(); ++i)
    localrc = attrList.at(i)->AttributeUpdateReset();

  for(i=0; i<packList.size(); ++i)
    localrc = packList.at(i)->AttributeUpdateReset();

  for(i=0; i<linkList.size(); ++i)
    localrc = linkList.at(i)->AttributeUpdateReset();

  return ESMF_SUCCESS;
  
  } // end AttributeUpdateReset

} // namespace ESMCI
