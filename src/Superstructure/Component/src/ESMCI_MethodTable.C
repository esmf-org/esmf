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
#define ESMC_FILENAME "ESMCI_MethodTable.C"
//==============================================================================
//
// ESMCI MethodTable implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt MethodTable} methods
// declared in the companion file {\tt ESMCI\_MethodTable.h}.
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_MethodTable.h"

// insert higher level, 3rd party or system includes
#include <string>
#include <sstream>
#ifndef ESMF_NO_DLFCN
#include <dlfcn.h>
#endif

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

//==============================================================================
//==============================================================================
// MethodTable class implementation
//==============================================================================
//==============================================================================

extern "C" {

  // call to native class constructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtablecreate"
  void FTN_X(c_esmc_methodtablecreate)(ESMCI::MethodTable **ptr, int *rc){
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    (*ptr) = new ESMCI::MethodTable;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("MethodTable allocation", ESMC_CONTEXT,
        rc);
      return;
    }
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // call to native class destructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtabledestroy"
  void FTN_X(c_esmc_methodtabledestroy)(ESMCI::MethodTable **ptr, int *rc){
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("MethodTable deallocation", ESMC_CONTEXT,
        rc);
      return;
    }
    delete (*ptr);
    *ptr = NULL;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableadd"
  void FTN_X(c_esmc_methodtableadd)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, void *pointer, int *rc,
    ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      localrc = (*ptr)->add(label, pointer);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableaddshobj"
  void FTN_X(c_esmc_methodtableaddshobj)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, char const *nameArg,
    char const *sharedObjArg,
    int *rc, ESMCI_FortranStrLenArg labelLen, ESMCI_FortranStrLenArg nameLen,
    ESMCI_FortranStrLenArg sharedObjLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      std::string name(nameArg, nameLen);
      name.resize(name.find_last_not_of(" ")+1);
      std::string sharedObj(sharedObjArg, sharedObjLen);
      sharedObj.resize(sharedObj.find_last_not_of(" ")+1);
      localrc = (*ptr)->add(label, name, sharedObj);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableaddrep"
  void FTN_X(c_esmc_methodtableaddrep)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, void *pointer, int *rc,
    ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      localrc = (*ptr)->addreplace(label, pointer);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableaddrepshobj"
  void FTN_X(c_esmc_methodtableaddrepshobj)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, char const *nameArg,
    char const *sharedObjArg,
    int *rc, ESMCI_FortranStrLenArg labelLen, ESMCI_FortranStrLenArg nameLen,
    ESMCI_FortranStrLenArg sharedObjLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      std::string name(nameArg, nameLen);
      name.resize(name.find_last_not_of(" ")+1);
      std::string sharedObj(sharedObjArg, sharedObjLen);
      sharedObj.resize(sharedObj.find_last_not_of(" ")+1);
      localrc = (*ptr)->addreplace(label, name, sharedObj);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableget"
  void FTN_X(c_esmc_methodtableget)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, ESMC_Logical *isPresent, int *rc,
    ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      if ((*ptr)->isPresent(label))
        *isPresent = ESMF_TRUE;
      else
        *isPresent = ESMF_FALSE;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableremove"
  void FTN_X(c_esmc_methodtableremove)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, int *rc, ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      localrc = (*ptr)->remove(label);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableexecute"
  void FTN_X(c_esmc_methodtableexecute)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, void *object, int *userRc, int *rc,
    ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      localrc = (*ptr)->execute(label, object, userRc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableexecuteef"
  void FTN_X(c_esmc_methodtableexecuteef)(ESMCI::MethodTable **ptr,
    char const *labelArg, int *index, void *object, ESMC_Logical *existflag,
    int *userRc, int *rc,
    ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      bool existing;
      std::string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      if (index){
        std::stringstream indexString;
        indexString << "::ESMF::index::" << *index;
        label += indexString.str();
      }
      localrc = (*ptr)->execute(label, object, userRc, &existing);
      if (existing)
        *existflag = ESMF_TRUE;
      else
        *existflag = ESMF_FALSE;
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

} // extern "C"

// -----------------------------------------------------------------------------

namespace ESMCI {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodElement::execute()"
  int MethodElement::execute(void *object, int *userRc)const{
    int rc = ESMC_RC_NOT_IMPL;
    if (pointer){
      typedef void (*FuncP)(void *, int *);
      FuncP vf = (FuncP)pointer;
      (*vf)(object, userRc);
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "invalid function pointer", ESMC_CONTEXT, &rc);
      return rc;

    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodElement::resolve()"
  int MethodElement::resolve(void){
    int rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB,
      "System does not support dynamic loading.", ESMC_CONTEXT, &rc);
    return rc;
#else
    void *lib;
    if (shobj.length()>0)
      lib = dlopen(shobj.c_str(), RTLD_LAZY);
    else
      lib = dlopen(NULL, RTLD_LAZY);  // search in executable
    if (lib == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "shared object not found", ESMC_CONTEXT, &rc);
      return rc;
    }
    pointer = (void *)dlsym(lib, name.c_str());
    if (pointer == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "named routine not found", ESMC_CONTEXT, &rc);
      return rc;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
#endif
  }

// -----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::add()"
  int MethodTable::add(std::string labelArg, void *pointer){
    int rc = ESMC_RC_NOT_IMPL;
    typename MethodTable::const_iterator it = lower_bound(labelArg);
    if (it != end() && it->first == labelArg){
      // key already exists -> error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "method with identical label already exists", ESMC_CONTEXT, &rc);
      return rc;
    }else{
      // this is a new key
      insert(it, std::pair<std::string,MethodElement>
        (labelArg, MethodElement(pointer)));
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::add()"
  int MethodTable::add(std::string labelArg, std::string name,
    std::string sharedObj){
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    typename MethodTable::const_iterator it = lower_bound(labelArg);
    if (it != end() && it->first == labelArg){
      // key already exists -> error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "method with identical label already exists", ESMC_CONTEXT, &rc);
      return rc;
    }else{
      // this is a new key
      typename MethodTable::iterator itt
        = insert(it, std::pair<std::string,MethodElement>
        (labelArg, MethodElement(name, sharedObj)));
      localrc = itt->second.resolve();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::addreplace()"
  int MethodTable::addreplace(std::string labelArg, void *pointer){
    int rc = ESMC_RC_NOT_IMPL;
    typename MethodTable::iterator it = lower_bound(labelArg);
    if (it != end() && it->first == labelArg){
      // key already exists -> simply replace
      it->second = MethodElement(pointer);
    }else{
      // this is a new key
      insert(it, std::pair<std::string,MethodElement>
        (labelArg, MethodElement(pointer)));
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::addreplace()"
  int MethodTable::addreplace(std::string labelArg, std::string name,
    std::string sharedObj){
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    typename MethodTable::iterator it = lower_bound(labelArg);
    if (it != end() && it->first == labelArg){
      // key already exists -> simply replace
      it->second = MethodElement(name, sharedObj);
      localrc = it->second.resolve();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
    }else{
      // this is a new key
      typename MethodTable::iterator itt
        = insert(it, std::pair<std::string,MethodElement>
        (labelArg, MethodElement(name, sharedObj)));
      localrc = itt->second.resolve();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::remove()"
  int MethodTable::remove(std::string labelArg){
    int rc = ESMC_RC_NOT_IMPL;
    int erased = erase(labelArg);
    if (erased == 0){
      // entry was not present -> return with error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "method not found in method table", ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::isPresent()"
  bool MethodTable::isPresent(std::string labelArg){
    bool found = true;
    if (find(labelArg) == end())
      found = false;
    return found;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::execute()"
  int MethodTable::execute(std::string labelArg, void *object, int *userRc,
    bool *existflag){
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    typename MethodTable::iterator it = find(labelArg);
    if (find(labelArg) == end()){
      // entry does not exist
      if (existflag){
        *existflag = false;
        if (userRc) *userRc = ESMF_SUCCESS;
        // return successfully
        rc = ESMF_SUCCESS;
        return rc;
      }else{
        // return with error
        std::stringstream msg;
        msg << "method not found in method table: " << labelArg;
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, msg, ESMC_CONTEXT, &rc);
        return rc;
      }
    }
    // entry does exist -> execute
    if (existflag) *existflag = true;
    localrc = it->second.execute(object, userRc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc; // bail out
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

} // namespace ESMCI

