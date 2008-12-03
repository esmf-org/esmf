// $Id: ESMCI_AttributeUpdate.C,v 1.2 2008/12/03 17:48:36 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMCI_Attribute.C"

// ESMCI_Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMCI_Attribute} methods declared
// in the companion file ESMCI_Attribute.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include "ESMC_Start.h"
#include "ESMCI_Attribute.h"
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_AttributeUpdate.C,v 1.2 2008/12/03 17:48:36 rokuingh Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

// class wide keySize
static const int keySize = 2*sizeof(int) + 2*sizeof(bool) + 1;

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// UPDATE ROUTINES:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdate"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdate - update an {\tt ESMCI_Attribute}
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      VM *vm,                    // the VM
      const vector<ESMC_I4> &roots) {      // the rootList
//
// !DESCRIPTION:
//    Update an {\tt ESMCI_Attribute} hierarchy with a later version of itself.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
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
  localrc = ESMCI_AttributeUpdateNeeded(vm, length, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.Write(
                  "AttributeUpdateNeeded failed", ESMC_LOG_INFO);
    return ESMF_SUCCESS;
  }
 
  // find out if I am a root
  it = find(nonroots.begin(), nonroots.end(), localPet);
  
  // allocate buffers
  recvBuf = new char[length];
  sendBuf = new char[length];
  
  // I am a root, create buffer
  if (it == nonroots.end()) {
    localrc = ESMCI_AttributeUpdateBufSend(sendBuf, localPet, &offset, &length);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
  }
//printf("offset = %d\n", offset);
  // roots send to nonroots
  // recvBuf = new char[20];  sendBuf = new char[20];  sendBuf = "Hello World!";
  localrc = ESMCI_AttributeUpdateComm(vm, offset, &recvlength, sendBuf, recvBuf, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateComm failed", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
//printf("recvlength = %d\n", recvlength);
  // I am a nonroot, unpack buffer
  if (it != nonroots.end()) {
      localrc = ESMCI_AttributeUpdateBufRecv(recvBuf, localPet, &offset, recvlength);
      if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed", &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
  }
//printf("offset = %d\n", offset);
  // all set flags to false
  localrc = ESMCI_AttributeUpdateReset();
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateReset failed", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }

  delete [] recvBuf;
  delete [] sendBuf;

  return ESMF_SUCCESS;

 } // end ESMCI_AttributeUpdate
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateBufRecv"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateBufRecv - unpack the serialized Attribute updates
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateBufRecv(
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
//    Serialize the updates to an {\tt ESMCI_Attribute} hierarchy.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc, nbytes, loffset;
  ESMCI_Attribute *attr;
  unsigned int i;
  char *thiskey, *distkey;
  thiskey = new char[keySize];
  distkey = new char[keySize];
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // make key
  localrc = ESMCI_AttributeUpdateKeyCreate(thiskey);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey failed", &localrc);
    delete [] thiskey;
    return ESMF_FAILURE;
  }

  // unpack next key
  memcpy(distkey, recvBuf+(*offset), keySize);

  // compare keys
  if (ESMCI_AttributeUpdateKeyCompare(thiskey, distkey) == false) {
    //printf("DeleteMe!!!\n");
    return 42;
  }

  // now update offset
  *offset += keySize;

  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;  
  
  // check if buffer has been overwritten
  if (length < *offset){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "Overread the buffer", &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }
  
  /*if (localPet == 4) {
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
    }*/

  // if struct change, unpack numAttrs into temp and add
  int sChange = sizeof(int)+1+sizeof(bool);
  int vChange = sizeof(int)+1;
  if ((*(reinterpret_cast<bool*> (distkey+sChange))) == true) {
    int nChange = sizeof(int)+1+(sizeof(bool)*2);
    int numChanges = (*(reinterpret_cast<int*> (distkey+nChange)));
    for (i=0; i<numChanges; ++i) {
      attr = new ESMCI_Attribute(ESMF_FALSE);
      attr->setBase(attrBase);
      attr->ESMC_Deserialize(recvBuf,offset);
      localrc = ESMCI_AttributeSet(attr);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed adding attribute", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
      localrc = attr->ESMCI_AttributeUpdateReset();
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed resetting", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
    }
  }
  
  // else if value change, unpack into temp and set
  else if ((*(reinterpret_cast<bool*> (distkey+vChange))) == true) {
    attr = new ESMCI_Attribute(ESMF_FALSE);
    attr->ESMC_Deserialize(recvBuf,offset);
    *this = *attr;
    // can delete this one and not call reset because this is a leaf
    delete attr;
  }

  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;

  // check if buffer has been overwritten
  if (length < *offset){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "Overread the buffer", &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }
      
  // recurse through the Attribute hierarchy
  for (i=0; i<attrCount; ++i) {
    localrc = attrList[i]->ESMCI_AttributeUpdateBufRecv(recvBuf,localPet,offset,length);
    if (localrc == 42) {
      localrc = ESMCI_AttributeUpdateRemove(i);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed removing attr", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
      --i;
    }
    else if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed in recursion", &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
    }
  }
    
  // check if buffer has been overwritten
  if (length < *offset){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "Overread the buffer", &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }
    
  delete [] thiskey;
  delete [] distkey;
    
  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateBufRecv
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateBufSend"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateBufSend - serialize the Attribute updates
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateBufSend(
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
//    Deserialize the updates to an {\tt ESMCI_Attribute} hierarchy.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc, nbytes;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  // check (length - offset) > sizeof(ESMCI_Attribute) + keylength
  
  // make key
  char *key;
  key = new char[keySize];
  localrc = ESMCI_AttributeUpdateKeyCreate(key);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
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

/*  if (localPet == 0) {
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
  
  // if struct changes
  if (structChange == ESMF_TRUE) {
    for (i=0; i<attrCount; ++i) {
      if (attrList[i]->structChange == ESMF_TRUE) {
        localrc = attrList[i]->ESMCI_AttributeUpdateReset();
        if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed resetting", &localrc);
          delete [] key;
          return ESMF_FAILURE;
        }
        localrc = attrList[i]->ESMC_Serialize(sendBuf,length,offset);
        if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed Serialize", &localrc);
          delete [] key;
          return ESMF_FAILURE;
        }
      }
    }
  }
  
  // if value changes 
  else if (valueChange == ESMF_TRUE) {
    localrc = ESMC_Serialize(sendBuf,length,offset);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed Serialize", &localrc);
      delete [] key;
      return ESMF_FAILURE;
    }
  }
  
  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;

  // recurse through the Attribute hierarchy
  for (i=0; i<attrCount; i++) {
    localrc = attrList[i]->ESMCI_AttributeUpdateBufSend(sendBuf,localPet,offset,length);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufSend failed", &localrc);
      delete [] key;
      return ESMF_FAILURE;
    }
  }
  
  // check if buffer has been overwritten
  if (*length < *offset){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "Buffer too short to add an Attribute hierarchy", &localrc);
    delete [] key;
    return localrc;
  }
  
  delete [] key;
  
  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateBufSend
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateChanges"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateChanges - look for changes in Attribute hierachy
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateChanges(
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
//    Search an {\tt ESMCI_Attribute} hierarchy for changes to be updated.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  ++(*numKeys);
  
  if (linkChange == ESMF_TRUE) ++(*linkChanges);
  if (structChange == ESMF_TRUE) {
    if (attrRoot == ESMF_FALSE && attrPackHead == ESMF_FALSE) ++(*structChanges); }
  if (valueChange == ESMF_TRUE) ++(*valueChanges);

  for(i=0; i<attrCount; ++i) {
    localrc = attrList[i]->ESMCI_AttributeUpdateChanges(linkChanges,
      structChanges,valueChanges,numKeys);
  }
  
  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateChanges
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateComm"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateComm - {\tt ESMCI_Attribute} update comm pattern
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateComm(
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
//    Update an {\tt ESMCI_Attribute} hierarchy with a later version of itself.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - AttributeUpdate - PET not in nonroots vector", &localrc);
      return ESMF_FAILURE;
    }

    int indRecv = 0;
    // find the index of roots from which to receive
    if (roots.size() < nonroots.size())
      indRecv=fmod(static_cast<double> (*itNR),static_cast<double> (ceilID));
    else
      indRecv = distance(nonroots.begin(), find(nonroots.begin(), nonroots.end(), localPet));
    
    // receive with message=0 and status=NULL for now
    vm->recv(recvBufSize, sizeof(int), roots[indRecv], 0);
    vm->recv(recvBuf, *recvBufSize, roots[indRecv], 0);
    /*printf("\n\nI am PET #%d, I received message \"%s\" from PET #%d\n\n",
            localPet, recvBuf, roots[indRecv]);*/
  }
    
  // Yes, I am sending
  else if (itSend != roots.end()) {
  
    // find this PET in roots, bail if it is not present
    itR = find(roots.begin(), roots.end(), localPet);
    if (itR == roots.end()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - AttributeUpdate - PET not in roots vector", &localrc);
      return ESMF_FAILURE;
    }
    
    for (i=0; i<ceilID; ++i) {
      // find the index of nonroots to which to send
      int indSend = distance(roots.begin(), itR) + i*floorID;
      
      if (indSend < nonroots.size()) {
        // send with message=0 and status=NULL for now
        /*printf("\n\nI am PET #%d, I am sending message \"%s\" to PET #%d\n\n",
                localPet, sendBuf, nonroots[indSend]);*/
        vm->send(&sendBufSize, sizeof(sendBufSize), nonroots[indSend],0);
        vm->send(sendBuf, sendBufSize, nonroots[indSend],0);
      }
    }
  }
    
  // Dunno who I am, bailing
  else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
    " - AttributeUpdate - PET unidentified", &localrc);
    return ESMF_FAILURE;
  }
  
  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateComm
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateKeyCompare"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateKeyCompare - Compare Attribute keys
//
// !INTERFACE:
      bool ESMCI_Attribute::ESMCI_AttributeUpdateKeyCompare(
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
  
/*  if ((*(reinterpret_cast<bool*> (key1+o))) != (*(reinterpret_cast<bool*> (key2+o)))) {
    result = false;}
  o += sizeof(bool);

  if ((*(reinterpret_cast<bool*> (key1+o))) != (*(reinterpret_cast<bool*> (key2+o)))) {
    result = false;}
  o += sizeof(bool);*/

  if (o > keySize) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateKeyCompare key buffer is misaligned", &localrc);
    return ESMF_FAILURE;
  }
  
  return result;
    
  } // end ESMCI_AttributeUpdateKeyCompare
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateKeyCreate"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateKeyCreate - Make the key for this Attribute
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateKeyCreate(
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

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  unsigned int i;
  char *xorkey;
  xorkey = new char[1];
  *xorkey = 'a';
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  int offset = 0;
 
  // copy the baseID
  *(reinterpret_cast<int*> (key+offset)) = attrBase->ESMC_BaseGetID();
  offset += sizeof(int);
  if ( offset > 4) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey key buffer is misaligned", &localrc);
    delete [] xorkey;
    return ESMF_FAILURE;
  }

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
  if ( offset > 5) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey key buffer is misaligned", &localrc);
    delete [] xorkey;
    return ESMF_FAILURE;
  }
   
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
  if ( offset > 7) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey key buffer is misaligned", &localrc);
    delete [] xorkey;
    return ESMF_FAILURE;
  }
  
  // now the number of struct changes on this attribute
  int numChanges = 0;
  if (structChange == ESMF_TRUE) {
    for (i=0; i<attrCount; ++i) {
      if (attrList[i]->structChange == ESMF_TRUE) {
        ++numChanges;}
    }
  }
  *(reinterpret_cast<int*> (key+offset)) = numChanges;
  offset += sizeof(int);
  if ( offset > keySize) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey key buffer is misaligned", &localrc);
    delete [] xorkey;
    return ESMF_FAILURE;
  }

  delete [] xorkey;
  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateKeyCreate
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateNeeded"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateNeeded - Determine if update is needed
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateNeeded(
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
//    Determine if ESMCI_AttributeUpdate() is needed. 
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
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
  recvBuf = new char[length];
  sendBuf = new char[length];
  
  offset = 0;
  recvlength = 0;
  linkChangesOut = 0;
  realChangesOut = 0;
  numKeysOut = 0;
  
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
    localrc = ESMCI_AttributeUpdateChanges(&linkChanges, &structChanges, 
      &valueChanges, &numKeys);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
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
  
  // call ESMCI_AttributeUpdateComm with changes as sendBuf and recvBuf
  localrc = ESMCI_AttributeUpdateComm(vm, length, &recvlength, 
        sendBuf, recvBuf, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateComm failed", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  
  // I am a nonroot
  if (it != nonroots.end()) {
  
    // unpack the buffer
    if (recvlength != length) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded something is amiss", &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  
  // set return value of buffer size
  bufSize = realChangesOut*sizeof(ESMCI_Attribute) + numKeysOut*keySize;
  
  /*printf("linkChanges = %d, realChangesOut = %d, numKeysOut = %d bufSize = %d\n", 
    linkChangesOut, realChangesOut, numKeysOut, bufSize);*/
  
  delete [] recvBuf;
  delete [] sendBuf;

  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateNeeded
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateRemove"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateRemove - Remove an Attribute in Update
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateRemove(
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

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  ESMCI_Attribute *attr;
  attr = attrList[attrNum];
  
  attr->~ESMCI_Attribute();
  attrList.erase(attrList.begin() + attrNum);
  attrCount--;
  structChange = ESMF_TRUE;

  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeUpdateReset"
//BOPI
// !IROUTINE:  ESMCI_AttributeUpdateReset - Reset flags for AttributeUpdate
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeUpdateReset(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        ) { 
//
// !DESCRIPTION:
//    Reset the flags for ESMCI_AttributeUpdate().
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  linkChange = ESMF_FALSE;
  structChange = ESMF_FALSE;
  valueChange = ESMF_FALSE;

  for(i=0; i<attrCount; ++i) {
    localrc = attrList[i]->ESMCI_AttributeUpdateReset();
  }

  return ESMF_SUCCESS;
  
  } // end ESMCI_AttributeUpdateReset

} // namespace ESMCI
