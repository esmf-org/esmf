// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_AttributeUpdate.C"

//#define DEBUG_PRINT_RUN
//#define DEBUG_PRINT_INIT_FINAL

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
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include "ESMCI_Macros.h"
#include "ESMCI_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"

using namespace std;

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

// class wide keySize
static const int keySize = 4*sizeof(int) + 1;

#ifdef DEBUG_PRINT_RUN
  static const int strsize = ESMF_MAXSTR*10;
  char msg[strsize];
  char filename[ESMF_MAXSTR];
  ofstream fp;
#endif

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
      VM *vm,                        // the VM
      const vector<ESMC_I4> &roots,  // the rootList
      bool reconcile) {              // reconcile flag
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to an Attribute", ESMC_CONTEXT, &localrc);
    return ESMF_FAILURE;
  }
    
  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

#ifdef DEBUG_PRINT_INIT_FINAL
  char dbgname[ESMF_MAXSTR];
  char *pifname = this->getBase()->ESMC_Base::ESMC_BaseGetName();
  sprintf(dbgname, "A%d-%s-0.dbg", localPet, pifname);
  ESMC_Print(true, dbgname, true);
#endif

  // make the roots and nonroots vectors
  vector<ESMC_I4> nonroots;
  vector<ESMC_I4>::const_iterator it;
  for(i=0; i<petCount; ++i) {
    it = find(roots.begin(), roots.end(), i);
    if(it == roots.end()) nonroots.push_back(i);
  }

#ifdef DEBUG_PRINT_RUN
  char *dbrname = this->getBase()->ESMC_Base::ESMC_BaseGetName();
  sprintf(filename, "A%d-%s-Update.dbg", localPet, dbrname);
  fp.open(filename, ofstream::out | ofstream::trunc);

  sprintf(msg, "P%d - rootList = [", localPet);
  for (unsigned int i=0; i<roots.size(); ++i) sprintf(msg + strlen(msg), "%d, ", roots.at(i));
  sprintf(msg + strlen(msg), "]\n");
  attprint(msg, strsize, true, fp);

  sprintf(msg, "P%d - nonrootList = [", localPet);
  for (unsigned int i=0; i<nonroots.size(); ++i) sprintf(msg + strlen(msg), "%d, ", nonroots.at(i));
  sprintf(msg + strlen(msg), "]\n");
  attprint(msg, strsize, true, fp);
#endif

  // find out if update is necessary
  localrc = AttributeUpdateNeeded(vm, length, roots, nonroots, reconcile);
  if (localrc != ESMF_SUCCESS || petCount == 1) return ESMF_SUCCESS;

  // find out if I am a root
  it = find(nonroots.begin(), nonroots.end(), localPet);
  
  // allocate buffers
  recvBuf = NULL; sendBuf = NULL;
  recvBuf = new char[length];
  sendBuf = new char[length];

  // I am a root, create buffer
  if (it == nonroots.end()) {
    if (reconcile) this->ESMC_Serialize(sendBuf, &length, &offset, ESMF_NOINQUIRE);
    else {
      localrc = AttributeUpdateBufSend(sendBuf, localPet, &offset, &length);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                    "AttributeUpdateBufSend failed", ESMC_CONTEXT, &localrc);
        delete [] recvBuf;
        delete [] sendBuf;
        return ESMF_FAILURE;
      }
    }
  }

  // roots send to nonroots
  localrc = AttributeUpdateComm(vm, offset, &recvlength, sendBuf, recvBuf, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateComm failed", ESMC_CONTEXT, &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }

  // I am a nonroot, unpack buffer
  if (it != nonroots.end()) {
    if (reconcile) {
      // save the Base
      ESMC_Base *temp_base = attrBase;
      // clean tree
      this->clean();
      // reset the base
      setBase(temp_base);
      // now rebuild
      this->ESMC_Deserialize(recvBuf, &offset);
    }
    else {
      localrc = AttributeUpdateBufRecv(recvBuf, localPet, &offset, recvlength);
      if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed", ESMC_CONTEXT, &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
      }
    }
  }

  // all set flags to false
  // RLO: All reset calls removed April 2014 because they are now seen as an
  // optimization that is causing failures in the MultiReconcile and ClosedLoop
  // tests of AttributeUpdate
  // TODO: September 2016, this should be reenabled at least for the case where
  //   reconcile == true on objects that can't be affected by the multireconcile
  //   or closedloop failures (maybe components), but I'm not yet sure how..
  /*localrc = AttributeUpdateReset();
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateReset failed", ESMC_CONTEXT, &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }*/

#ifdef DEBUG_PRINT_RUN
  fp.close();
#endif

#ifdef DEBUG_PRINT_INIT_FINAL
  sprintf(dbgname, "A%d-%s-1.dbg", localPet, pifname);
  ESMC_Print(true, dbgname, true);
#endif

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
  distkey = new char[keySize];

  // make key
  localrc = AttributeUpdateKeyCreate(thiskey);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey failed", ESMC_CONTEXT, &localrc);
    delete [] thiskey;
    delete [] distkey;
    return ESMF_FAILURE;
  }
 
  // unpack next key
  memcpy(distkey, recvBuf+(*offset), keySize);
  *offset += keySize;

#ifdef DEBUG_PRINT_RUN
  sprintf(msg,
      "\nName = %s, Convention = %s, Purpose = %s\n",
      attrName.c_str(), attrConvention.c_str(), attrPurpose.c_str());
  attprint(msg, strsize, true, fp);
  // key = ID, xor [name, conv, purp], value changes, struct changes, pack changes
  char msg[ESMF_MAXSTR*10];
  sprintf(msg, "(This)   key: %s, BaseID: %d, attrRoot: %d\n"
               "    Changes: (value) %d (struct) %d (pack) %d\n"
               "    Sizes  : (attr) %d (pack) %d (link) %d\n"
               "(Source) key: %s, BaseID: %d\n"
               "    Changes: (value) %d (struct) %d (pack) %d\n",
          thiskey+4,
          (*(reinterpret_cast<int*> (thiskey+0))),
          attrRoot,
          (*(reinterpret_cast<int*> (thiskey+5))),
          (*(reinterpret_cast<int*> (thiskey+9))),
          (*(reinterpret_cast<int*> (thiskey+13))),
          attrList.size(), packList.size(), linkList.size(),
          distkey+4,
          (*(reinterpret_cast<int*> (distkey+0))),
          (*(reinterpret_cast<int*> (distkey+5))),
          (*(reinterpret_cast<int*> (distkey+9))),
          (*(reinterpret_cast<int*> (distkey+13))));
  attprint(msg, strsize, true, fp);
#endif

  if (!AttributeUpdateKeyCompare(thiskey, distkey)) {
#ifdef DEBUG_PRINT_RUN
  sprintf(msg, "!!!NO MATCH!!!\n");
  attprint(msg, strsize, true, fp);
#endif

    // first two blocks for handling of non-ordered containers
    if (attrUpdateDone == ESMF_TRUE) {
      delete [] thiskey;
      delete [] distkey;
      *offset -= keySize;
      return ESMF_SUCCESS;
    }
    else if (attrRoot == ESMF_TRUE) {
      delete [] thiskey;
      delete [] distkey;
      *offset -= keySize;
      return ESMC_ATTUPDATERM_HOOKANDCONTINUE;
    }
    else if (attrPackHead == ESMF_TRUE) {
      delete [] thiskey;
      delete [] distkey;
      *offset -= keySize;
      return ESMC_ATTUPDATERM_ATTPACK;
    }
    else if (attrPack == ESMF_TRUE) {
      delete [] thiskey;
      delete [] distkey;
      *offset -= keySize;
      return ESMC_ATTUPDATERM_ATTPACKATT;
    }
    else {
      delete [] thiskey;
      delete [] distkey;
      *offset -= keySize;
      return ESMC_ATTUPDATERM_ATTRIBUTE;
    }
  }
#ifdef DEBUG_PRINT_RUN
  else { 
  sprintf(msg, "!!!MATCH!!!\n");
  attprint(msg, strsize, true, fp);
  }
#endif

  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;  

  // make sure the buffer has not been overwritten
  if (length < *offset){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "BufRead overread the buffer", ESMC_CONTEXT, &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }

  // get key info
  int valueChange = (*(reinterpret_cast<int*> (distkey+sizeof(int)+1)));
  int attrChange = (*(reinterpret_cast<int*> (distkey+sizeof(int)+1+(sizeof(int)))));
  int packChange = (*(reinterpret_cast<int*> (distkey+sizeof(int)+1+(2*sizeof(int)))));

  // if attr struct change, unpack and add to end of list
  for (i=0; i<attrChange; ++i) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    attr->setBase(attrBase);
    attr->parent = this;
    attr->ESMC_Deserialize(recvBuf,offset);
    localrc = AttributeSet(attr);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed AttributeSet",
                   ESMC_CONTEXT, &localrc);
      delete attr;
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "  - added Attribute: %s\n", attr->attrName.c_str());
    attprint(msg, strsize, true, fp);
#endif
    // RLO: All reset calls removed April 2014 because they are now seen as an
    // optimization that is causing failures in the MultiReconcile and ClosedLoop
    // tests of AttributeUpdate
    /*localrc = attr->AttributeUpdateReset();
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed resetting",
                   ESMC_CONTEXT, &localrc);
      delete attr;
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }*/
  }

  // if value change, unpack into temp, find matching attr, and copy
  for (i=0; i<valueChange; ++i) {

    // create attr from serialized list
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    attr->setBase(attrBase);
    attr->parent = this;
    attr->ESMC_Deserialize(recvBuf,offset);

    // find the right Attribute to modify
    char *srckey, *dstkey;
    srckey = new char[keySize]; 
    dstkey = new char[keySize]; 
    localrc = attr->AttributeUpdateKeyCreate(srckey);
    unsigned int j;
    for (j=0; j<attrList.size(); ++j) {
      localrc = attrList.at(j)->AttributeUpdateKeyCreate(dstkey);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufRecv failed AttributeUpdateKeyCreate",
                ESMC_CONTEXT, &localrc);
        delete [] thiskey;
        delete [] distkey;
        delete attr;
        delete [] srckey;
        delete [] dstkey;
        return ESMF_FAILURE;
      }
      if (AttributeUpdateKeyCompare(srckey, dstkey)) break;
    }

    // copy serialized attribute from buffer to attr
    localrc = attrList.at(j)->AttributeCopy(*attr);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufRecv failed AttributeCopy",
                 ESMC_CONTEXT, &localrc);
      delete [] thiskey;
      delete [] distkey;
      delete attr;
      delete [] srckey;
      delete [] dstkey;
      return ESMF_FAILURE;
    }

    // reset the Update parameters
    // RLO: All reset calls removed April 2014 because they are now seen as an
    // optimization that is causing failures in the MultiReconcile and ClosedLoop
    // tests of AttributeUpdate
    /*localrc = attrList.at(j)->AttributeUpdateReset();
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufRecv failed resetting",
                 ESMC_CONTEXT, &localrc);
      delete [] thiskey;
      delete [] distkey;
      delete attr;
      delete [] srckey;
      delete [] dstkey;
      return ESMF_FAILURE;
    }*/

#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "  - replaced Attribute: %s\n", attr->attrName.c_str());
    attprint(msg, strsize, true, fp);
#endif

    // can delete this one and not call reset because this is a value copy
    delete attr;
    delete [] srckey;
    delete [] dstkey;
  }

  // if pack struct change, unpack and add to end of list
  for (i=0; i<packChange; ++i) {
    attr = NULL;
    attr = new Attribute("42","42","42","42");
    attr->setBase(attrBase);
    attr->parent = this;
    attr->ESMC_Deserialize(recvBuf,offset);
    localrc = AttPackSet(attr);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed AttPackSet",
                   ESMC_CONTEXT, &localrc);
      delete attr;
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }
    // RLO: All reset calls removed April 2014 because they are now seen as an
    // optimization that is causing failures in the MultiReconcile and ClosedLoop
    // tests of AttributeUpdate
    /*localrc = attr->AttributeUpdateReset();
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed resetting",
                   ESMC_CONTEXT, &localrc);
      delete attr;
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }*/
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "  - added AttPack: %s\n", attr->attrName.c_str());
    attprint(msg, strsize, true, fp);
#endif

  }

  // recurse through the Attribute hierarchy
  for (i=0; i<attrList.size(); ++i) {
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "  - recursing attrList to Attribute: %s\n",
            attrList.at(i)->attrName.c_str());
    attprint(msg, strsize, true, fp);
#endif
    localrc = attrList.at(i)->AttributeUpdateBufRecv(recvBuf,localPet,offset,length);
    if (localrc == ESMC_ATTUPDATERM_ATTRIBUTE) {
#ifdef DEBUG_PRINT_RUN
      sprintf(msg, "  - removing Attribute: %s\n",
              attrList.at(i)->attrName.c_str());
      attprint(msg, strsize, true, fp);
#endif
      localrc = AttributeRemove(attrList.at(i)->attrName);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufRecv failed AttributeRemove",
                 ESMC_CONTEXT, &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
      --i;
    }
    else if (localrc == ESMC_ATTUPDATERM_ATTPACKATT) {
      localrc = AttributeRemove(attrList.at(i)->attrName);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed AttPackRemoveAttribute",
                 ESMC_CONTEXT, &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
      --i;
    }
    else if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                "AttributeUpdateBufRecv failed recursing attrList",
               ESMC_CONTEXT, &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
    }
  }

  // recurse through the Attribute Package hierarchy
  for (i=0; i<packList.size(); ++i) {
    string attPackInstanceName;
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "  - recursing packList to AttPack: %s\n",
            packList.at(i)->attrName.c_str());
    attprint(msg, strsize, true, fp);
#endif
    localrc = packList.at(i)->AttributeUpdateBufRecv(recvBuf,localPet,offset,length);
    if (localrc == ESMC_ATTUPDATERM_ATTPACK) {
 #ifdef DEBUG_PRINT_RUN
      sprintf(msg, "  - removing AttPack: %s\n",
              packList.at(i)->attrName.c_str());
      attprint(msg, strsize, true, fp);
 #endif
     localrc = AttPackRemove(packList.at(i));
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed AttPackRemove",
                   ESMC_CONTEXT, &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
      --i;
    }
    else if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateBufRecv failed recursing packList",
                   ESMC_CONTEXT, &localrc);
      delete [] thiskey;
      delete [] distkey;
      return ESMF_FAILURE;
    }
  }

  // recurse the linkList
  int hook_index = 0;
  bool hook = true;
  // handling for unordered containers
  while (hook) {
    hook = false; // reset hook to start off with a clean slate
    for (i=hook_index; i<linkList.size(); ++i) {
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "  - recursing linkList to root of class: %d\n",
            linkList.at(i)->attrBase->classID);
    attprint(msg, strsize, true, fp);
#endif
      localrc = linkList.at(i)->AttributeUpdateBufRecv(recvBuf,localPet,offset,length);
#ifdef DEBUG_PRINT_RUN
      if (localrc == ESMC_ATTUPDATERM_HOOKANDCONTINUE) 
        sprintf(msg,
          "  - returned ESMC_ATTUPDATERM_HOOKANDCONTINUE\n",
          localPet);
      else if (localrc == ESMC_ATTUPDATERM_ATTPACK) 
        sprintf(msg,
          "  - returned ESMC_ATTUPDATERM_ATTPACK\n",
          localPet);
      else if (localrc == ESMC_ATTUPDATERM_ATTRIBUTE) 
        sprintf(msg,
          "  - returned ESMC_ATTUPDATERM_ATTRIBUTE\n",
          localPet);
      else if (localrc == ESMC_ATTUPDATERM_ATTPACKATT) 
        sprintf(msg,
            "  - returned ESMC_ATTUPDATERM_ATTPACKATT\n",
            localPet);
      else
        sprintf(msg,
            "returned %d from linkList traversal\n", localrc);
      attprint(msg, strsize, true, fp);
#endif
      // handling for unordered containers
      if (localrc == ESMC_ATTUPDATERM_HOOKANDCONTINUE) {
        if (!hook) {
          hook = true;
          hook_index = i;
        }
      }
      else if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "AttributeUpdateBufRecv failed recursing linkList",
           ESMC_CONTEXT, &localrc);
        delete [] thiskey;
        delete [] distkey;
        return ESMF_FAILURE;
      }
    }  // for loop
  }  // while loop - hack for non-ordered containers
    
  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;  

  // make sure the buffer has not been overwritten
  if (length < *offset) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "BufRead overread the buffer", ESMC_CONTEXT, &localrc);
    delete [] thiskey;
    delete [] distkey;
    return localrc;
  }
    
  delete [] thiskey;
  delete [] distkey;

  attrUpdateDone = ESMF_TRUE;
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

  localrc = AttributeUpdateKeyCreate(key);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateMakeKey failed", ESMC_CONTEXT, &localrc);
    delete [] key;
    return ESMF_FAILURE;
  }

  // add this key to the buffer
  memcpy(sendBuf+(*offset),key,keySize);
  *offset += keySize;

  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;

  // make sure the buffer has not been overwritten
  if (*length < *offset){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "BufferSend overread the buffer", ESMC_CONTEXT, &localrc);
    delete [] key;
    return ESMF_FAILURE;
  }

#ifdef DEBUG_PRINT_RUN
 // compare keys
  sprintf(msg,
    "\nName = %s, Convention = %s, Purpose = %s\n",
    attrName.c_str(), attrConvention.c_str(), attrPurpose.c_str());
  attprint(msg, strsize, true, fp);
  // key = ID, xor [name, conv, purp], value changes, struct changes, pack changes
  sprintf(msg, "key: %s, BaseID: %d, attrRoot: %d\n"
               "    Changes: (value) %d (struct) %d (pack) %d\n"
               "    Sizes  : (attr) %d (pack) %d (link) %d\n",
                      key+4,
                      (*(reinterpret_cast<int*> (key+0))),
                      attrRoot,
                      (*(reinterpret_cast<int*> (key+5))),
                      (*(reinterpret_cast<int*> (key+9))),
                      (*(reinterpret_cast<int*> (key+13))),
                      attrList.size(), packList.size(), linkList.size());
  attprint(msg, strsize, true, fp);
#endif

  // get key info
  int valueChanges = (*(reinterpret_cast<int*> (key+sizeof(int)+1)));
  int structChanges = (*(reinterpret_cast<int*> (key+sizeof(int)+1+sizeof(int))));
  int packChanges = (*(reinterpret_cast<int*> (key+sizeof(int)+1+(2*sizeof(int)))));

  // if struct changes
  if (structChanges > 0) {
    int structcount = 0;
    for (i=0; i<attrList.size(); ++i) {
      if (attrList.at(i)->structChange == ESMF_TRUE) {
        // RLO: All reset calls removed April 2014 because they are now seen as an
        // optimization that is causing failures in the MultiReconcile and ClosedLoop
        // tests of AttributeUpdate
        /*localrc = attrList.at(i)->AttributeUpdateReset();
        if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
             "AttributeUpdateBufSend failed resetting", ESMC_CONTEXT, &localrc);
          delete [] key;
          return ESMF_FAILURE;
        }*/
        localrc = attrList.at(i)->ESMC_Serialize(sendBuf,length,offset,ESMF_NOINQUIRE);
        if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "AttributeUpdateBufSend failed Serialize", ESMC_CONTEXT, &localrc);
          delete [] key;
          return ESMF_FAILURE;
        }
        ++structcount;
      }
    }
    if (structChanges != structcount) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
         "BufferSend mismatch on structChanges count", ESMC_CONTEXT, &localrc);
      delete [] key;
      return ESMF_FAILURE;
    }
  }

  // if value changes
  if (valueChanges > 0) {
    int valcount = 0;
    for (i=0; i<attrList.size(); ++i) { 
      if (attrList.at(i)->valueChange == ESMF_TRUE) {
        // RLO: All reset calls removed April 2014 because they are now seen as an
        // optimization that is causing failures in the MultiReconcile and ClosedLoop
        // tests of AttributeUpdate
        /*localrc = attrList.at(i)->AttributeUpdateReset();
        if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                    "AttributeUpdateBufSend failed resetting",
                    ESMC_CONTEXT, &localrc);
          delete [] key;
          return ESMF_FAILURE;
        }*/
        localrc = attrList.at(i)->ESMC_Serialize(sendBuf,length,offset,ESMF_NOINQUIRE);
        if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
              "AttributeUpdateBufSend failed Serialize", ESMC_CONTEXT, &localrc);
          delete [] key;
          return ESMF_FAILURE;
        }
        ++valcount;
      }
    }
    if (valueChanges != valcount) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "BufferSend mismatch on valueChanges count", ESMC_CONTEXT, &localrc);
      delete [] key;
      return ESMF_FAILURE;
    }
  }

  // if pack changes
  if (packChanges > 0) {
  int packcount = 0;
  for (i=0; i<packList.size(); i++) {
    if (packList.at(i)->structChange == ESMF_TRUE) {
      // RLO: All reset calls removed April 2014 because they are now seen as an
      // optimization that is causing failures in the MultiReconcile and ClosedLoop
      // tests of AttributeUpdate
      /*localrc = packList.at(i)->AttributeUpdateReset();
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "AttributeUpdateBufSend failed resetting", ESMC_CONTEXT, &localrc);
        delete [] key;
        return ESMF_FAILURE;
      }*/
      localrc = packList.at(i)->ESMC_Serialize(sendBuf,length,offset,ESMF_NOINQUIRE);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "AttributeUpdateBufSend failed Serialize", ESMC_CONTEXT, &localrc);
        delete [] key;
        return ESMF_FAILURE;
      }
      ++packcount;
    }
  }
  if (packChanges != packcount) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
             "BufferSend mismatch on packChanges count", ESMC_CONTEXT, &localrc);
    delete [] key;
    return ESMF_FAILURE;
  }
  }

  // recurse through the Attribute hierarchy
  for (i=0; i<attrList.size(); i++) {
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "attrList recursion %d of %d\n",
        localPet, i, attrList.size());
    attprint(msg, strsize, true, fp);
#endif
    localrc = attrList.at(i)->AttributeUpdateBufSend(sendBuf,localPet,offset,length);
  }

  // recurse through the Attribute hierarchy
  for (i=0; i<packList.size(); i++) {
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "packList recursion %d of %d\n",
        localPet, i, packList.size());
    attprint(msg, strsize, true, fp);
#endif
    localrc = packList.at(i)->AttributeUpdateBufSend(sendBuf,localPet,offset,length);
  }

  // recurse through the Attribute hierarchy
  for (i=0; i<linkList.size(); i++) {
#ifdef DEBUG_PRINT_RUN
    sprintf(msg, "linkList recursion %d of %d\n",
        localPet, i, linkList.size());
    attprint(msg, strsize, true, fp);
#endif
    localrc = linkList.at(i)->AttributeUpdateBufSend(sendBuf,localPet,offset,length);
  }
  // make sure offset is aligned correctly
  nbytes=(*offset)%8;
  if (nbytes!=0) *offset += 8-nbytes;
 
  // check if buffer has been overwritten
  if (*length < *offset){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add an Attribute hierarchy", ESMC_CONTEXT, &localrc);
    delete [] key;
    return localrc;
  }
  
  delete [] key;

  return ESMF_SUCCESS;
  
  } // end AttributeUpdateBufSend
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateTreeChanges"
//BOPI
// !IROUTINE:  AttributeUpdateTreeChanges - look for all changes in Attribute hierarchy
//
// !INTERFACE:
      int Attribute::AttributeUpdateTreeChanges(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int *linkChanges,            // link changes flag
      int *structChanges,          // structural changes flag
      int *valueChanges,           // value changes flag
      int *deleteChanges,          // value changes flag
      int *numKeys) const{         // number of keys flag
//
// !DESCRIPTION:
//    Search an {\tt Attribute} hierarchy for changes to be updated.  
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
    
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  ++(*numKeys);

  int packAttCount = 1;
  if ((attrPackHead == ESMF_TRUE) && (structChange == ESMF_TRUE))
    localrc = AttributeUpdateCountPackage(&packAttCount);

  if (linkChange == ESMF_TRUE) ++(*linkChanges);
  if (structChange == ESMF_TRUE) 
    *structChanges += packAttCount;
  if (valueChange == ESMF_TRUE) ++(*valueChanges);
  if (deleteChange == ESMF_TRUE) ++(*deleteChanges);
 
  for (i=0; i<attrList.size(); ++i)
    localrc = attrList.at(i)->AttributeUpdateTreeChanges(linkChanges,
      structChanges, valueChanges, deleteChanges, numKeys);

  for (i=0; i<packList.size(); ++i)
    localrc = packList.at(i)->AttributeUpdateTreeChanges(linkChanges,
      structChanges, valueChanges, deleteChanges, numKeys);
  
  for(i=0; i<linkList.size(); ++i)
    localrc = linkList.at(i)->AttributeUpdateTreeChanges(linkChanges,
      structChanges, valueChanges, deleteChanges, numKeys);
  
  return ESMF_SUCCESS;
  
  } // end AttributeUpdateTreeChanges
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeUpdateCountPackage"
//BOPI
// !IROUTINE:  AttributeUpdateCountPackage - count the Attributes in a package
//
// !INTERFACE:
      int Attribute::AttributeUpdateCountPackage(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int *packAttCount) const{         // number of attributes in package
//
// !DESCRIPTION:
//    Count the Attributes in a package.  
//    Expected to be called internally.
//
//EOPI

  int localrc;
  unsigned int i;
      
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  ++(*packAttCount);
 
  for (i=0; i<attrList.size(); ++i)
    localrc = attrList.at(i)->AttributeUpdateCountPackage(packAttCount);

  for (i=0; i<packList.size(); ++i)
    localrc = packList.at(i)->AttributeUpdateCountPackage(packAttCount);
    
  return ESMF_SUCCESS;
  
  } // end AttributeUpdateCountPackage
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

  int ceilID=ceil(static_cast<double> (nonroots.size())/static_cast<double> (roots.size()));
    
  // No, I am receiving
  if (itSend == roots.end()) {
    // find this PET in nonroots, bail if it is not present
    itNR = find(nonroots.begin(), nonroots.end(), localPet);
    if (itNR == nonroots.end()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - AttributeUpdate - PET not in nonroots vector", ESMC_CONTEXT, &localrc);
      delete commh;
      return ESMF_FAILURE;
    }

  int floorID=floor(static_cast<double> (distance(nonroots.begin(), itNR))/static_cast<double> (roots.size()));

    int indRecv = 0;
    // find the index of roots from which to receive
    indRecv = distance(nonroots.begin(), itNR) - floorID*roots.size();
    
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

#ifdef DEBUG_PRINT_RUN      
sprintf(msg, "\nP%d RECEIVE \"%s\" from P%d\n",
            localPet, recvBuf, roots[indRecv]);
attprint(msg, strsize, true, fp);
#endif
  }
    
  // Yes, I am sending
  else if (itSend != roots.end()) {
  
    // find this PET in roots, bail if it is not present
    itR = find(roots.begin(), roots.end(), localPet);
    if (itR == roots.end()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - AttributeUpdate - PET not in roots vector", ESMC_CONTEXT, &localrc);
      delete commh;
      return ESMF_FAILURE;
    }
    
    for (i=0; i<ceilID; ++i) {
      // find the index of nonroots to which to send
      int indSend = distance(roots.begin(), itR) + i*roots.size();
      
      if (indSend < nonroots.size()) {
        // send with message=0 and status=NULL for now
#ifdef DEBUG_PRINT_RUN      
sprintf(msg, "\nP%d, SEND \"%s\" to P%d\n",
        localPet, sendBuf, nonroots[indSend]);
attprint(msg, strsize, true, fp);
#endif
        vm->recv(&handshake, sizeof(int), nonroots[indSend]);
        vm->send(&sendBufSize, sizeof(sendBufSize), nonroots[indSend]);
        vm->recv(&handshake, sizeof(int), nonroots[indSend]);
        vm->send(sendBuf, sendBufSize, nonroots[indSend]);
      }
    }
  }
    
  // Dunno who I am, bailing
  else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
    " - AttributeUpdate - PET unidentified", ESMC_CONTEXT, &localrc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
       "AttributeUpdateKeyCompare key buffer is misaligned", ESMC_CONTEXT, &localrc);
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
  // FIXME: this is a temporary fix for the Field to Grid Attribute link
  if (attrBase->classID == 42)
    *(reinterpret_cast<int*> (key+offset)) = 42;
  else *(reinterpret_cast<int*> (key+offset)) = attrBase->ESMC_BaseGetID();
  offset += sizeof(int);

  // xor the name into key
  if (!attrName.empty() && attrPackHead != ESMF_TRUE) {
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
   
  // now the number of struct changes on this attribute
  int valueChanges = 0;
  int attrChanges = 0;
  int packChanges = 0;

  // AttributeUpdateChanges
    for (i=0; i<attrList.size(); ++i) {
      if (attrList.at(i)->structChange == ESMF_TRUE)
        ++attrChanges;
      if (attrList.at(i)->valueChange == ESMF_TRUE)
        ++valueChanges;
    }
    for (i=0; i<packList.size(); ++i) {
      if (packList.at(i)->structChange == ESMF_TRUE)
        ++packChanges;
    }

  *(reinterpret_cast<int*> (key+offset)) = valueChanges;
  offset += sizeof(int);
  *(reinterpret_cast<int*> (key+offset)) = attrChanges;
  offset += sizeof(int);
  *(reinterpret_cast<int*> (key+offset)) = packChanges;
  offset += sizeof(int);
  if ( offset > keySize) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "AttributeUpdateMakeKey key buffer is misaligned", ESMC_CONTEXT, &localrc);
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
      VM *vm,                           // the VM
      int &bufSize,                     // size of buffer to return
      const vector<ESMC_I4> &roots,     // roots vector
      const vector<ESMC_I4> &nonroots,  // nonroots vector
      bool reconcile) const {           // reconcile flag
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

  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();

  // identify myself
  it = find(nonroots.begin(), nonroots.end(), localPet);
  
  // I am a root
  if (it == nonroots.end()) {
    if (reconcile) {
      // set the first position of sendBuf to the size of offset from serialize
      this->ESMC_Serialize(sendBuf, &length, &offset, ESMF_INQUIREONLY);
      (*(reinterpret_cast<int*> (sendBuf)))=offset;
      realChangesOut = 1;
      numKeysOut = 1;
      bufSize = offset;
    } else {
      int linkChanges = 0;
      int structChanges = 0;
      int valueChanges = 0;
      int deleteChanges = 0;
      int numKeys = 0;

      // look for changes
      localrc = AttributeUpdateTreeChanges(&linkChanges, &structChanges,
        &valueChanges, &deleteChanges, &numKeys);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "AttributeUpdateNeeded failed AttributeUpdateTreeChanges", ESMC_CONTEXT, &localrc);
        delete [] recvBuf;
        delete [] sendBuf;
        return ESMF_FAILURE;
      }

      // create buffer
      int realChanges = structChanges + valueChanges + deleteChanges;
      (*(reinterpret_cast<int*> (sendBuf+offset)))=linkChanges;
      offset += sizeof(int);
      (*(reinterpret_cast<int*> (sendBuf+offset)))=realChanges;
      offset += sizeof(int);
      (*(reinterpret_cast<int*> (sendBuf+offset)))=numKeys;
      offset += sizeof(int);
      if (offset != length) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                    "AttributeUpdateNeeded -  writing buffer failed", ESMC_CONTEXT, &localrc);
        delete [] recvBuf;
        delete [] sendBuf;
        return ESMF_FAILURE;
      }

      // set Out values
      linkChangesOut = linkChanges;
      realChangesOut = realChanges;
      numKeysOut = numKeys;
    }
  }
  
  // call AttributeUpdateComm with changes as sendBuf and recvBuf
  localrc = AttributeUpdateComm(vm, length, &recvlength, 
        sendBuf, recvBuf, roots, nonroots);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded failed AttributeUpdateComm", ESMC_CONTEXT, &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  
  // I am a nonroot
  if (it != nonroots.end()) {
    // unpack the buffer
    if (recvlength != length) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded - recvBuf not correct size", ESMC_CONTEXT, &localrc);
      delete [] recvBuf;
      delete [] sendBuf;
      return ESMF_FAILURE;
    }
    if (reconcile) {
      // set realChanges to 1 so routine doesn't bail and pull out buffer size
      realChangesOut = 1;
      numKeysOut = 1;
      bufSize = (*(reinterpret_cast<int*> (recvBuf)));
    } else {
      linkChangesOut = (*(reinterpret_cast<int*> (recvBuf+offset)));
      offset += sizeof(int);
      realChangesOut = (*(reinterpret_cast<int*> (recvBuf+offset)));
      offset += sizeof(int);
      numKeysOut = (*(reinterpret_cast<int*> (recvBuf+offset)));
      offset += sizeof(int);
      if (offset != length) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                    "AttributeUpdateNeeded -  reading buffer failed", ESMC_CONTEXT, &localrc);
        delete [] recvBuf;
        delete [] sendBuf;
        return ESMF_FAILURE;
      }
    }
  }

  // if link changes, we bail and recommend StateReconcile
  if (linkChangesOut > 0) {
    ESMC_LogDefault.Write(
      "Attribute link changes made, call ESMF_StateReconcile() first",
      ESMC_LOGMSG_INFO, ESMC_CONTEXT);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  // if no changes, nothing to do
  else if (realChangesOut == 0) {
    ESMC_LogDefault.Write(
      "There is nothing to update, gracefully exiting.", ESMC_LOGMSG_INFO,
      ESMC_CONTEXT);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  // if funny values, we bail
  else if (realChangesOut < 0 || numKeysOut <= 0) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeUpdateNeeded something is amiss", ESMC_CONTEXT, &localrc);
    delete [] recvBuf;
    delete [] sendBuf;
    return ESMF_FAILURE;
  }
  
  // set return value of buffer size
  // gjt: make sure to take 8-byte alignment into account
  int keySize8 = keySize/8 * 8;
  if (keySize%8) keySize8 += 8; 

  if (!reconcile)
    bufSize = (realChangesOut)*sizeof(Attribute) + numKeysOut*keySize8;

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
  deleteChange = ESMF_TRUE;

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
  deleteChange = ESMF_FALSE;

  for(i=0; i<attrList.size(); ++i)
    localrc = attrList.at(i)->AttributeUpdateReset();

  for(i=0; i<packList.size(); ++i)
    localrc = packList.at(i)->AttributeUpdateReset();

  for(i=0; i<linkList.size(); ++i)
    localrc = linkList.at(i)->AttributeUpdateReset();

  return ESMF_SUCCESS;
  
  } // end AttributeUpdateReset

} // namespace ESMCI
