// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
      ESMC_LogDefault.MsgAllocError("- MethodTable allocation", ESMC_CONTEXT, 
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
      ESMC_LogDefault.MsgAllocError("- MethodTable deallocation", ESMC_CONTEXT,
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
        "- corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
//    rc)) 
//      return;
    // debugging---------
    
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
        "- corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
//    rc)) 
//      return;
    // debugging---------
    
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
        "- corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
//    rc)) 
//     return;
    // debugging---------
    
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
        "- corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
//    rc)) 
//      return;
    // debugging---------
    
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
        "- corrupt label string", ESMC_CONTEXT, rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
//    rc)) 
//      return;
    // debugging---------
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

} // extern "C"



namespace ESMCI {
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodElement::print()"
  int MethodElement::print(void)const{
    int rc = ESMC_RC_NOT_IMPL;
    printf("%s\n", label.c_str());
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodElement::execute()"
  int MethodElement::execute(void *object, int *userRc){
    int rc = ESMC_RC_NOT_IMPL;
    if (pointer){
      typedef void (*FuncP)(void *, int *);
      FuncP vf = (FuncP)pointer;
      (*vf)(object, userRc);
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        " - invalid function pointer", ESMC_CONTEXT, &rc);
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
      "- System does not support dynamic loading.", ESMC_CONTEXT, &rc);
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
        "- named routine not found", ESMC_CONTEXT, &rc);
      return rc;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
#endif
  }
  
// -----------------------------------------------------------------------------
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::print()"
  int MethodTable::print(void)const{
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    MethodElement *element = table; // initialize
    while (element){
      localrc = element->print();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
      element = element->nextElement;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::add()"
  int MethodTable::add(std::string labelArg, void *pointer){
    int rc = ESMC_RC_NOT_IMPL;
    if (table){
      MethodElement *element = table; // initialize
      MethodElement *prev;
      while (element){
        if (element->label == labelArg){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
            "- method with identical label already exists", ESMC_CONTEXT, &rc);
          return rc;
        }
        prev = element;
        element = element->nextElement;
      }
      prev->nextElement = new MethodElement(labelArg, pointer);
    }else{
      table = new MethodElement(labelArg, pointer);
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
    MethodElement *element = table; // initialize
    if (table){
      MethodElement *prev;
      while (element){
        if (element->label == labelArg){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
            "- method with identical label already exists", ESMC_CONTEXT, &rc);
          return rc;
        }
        prev = element;
        element = element->nextElement;
      }
      prev->nextElement = new MethodElement(labelArg, name, sharedObj);
      element = prev->nextElement;
    }else{
      table = new MethodElement(labelArg, name, sharedObj);
      element = table;
    }
    localrc = element->resolve();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc; // bail out
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::remove()"
  int MethodTable::remove(std::string labelArg){
    int rc = ESMC_RC_NOT_IMPL;
    if (table){
      MethodElement *element = table; // initialize
      MethodElement *prev = table;  // initialize
      while (element){
        if (element->label == labelArg){
          if (element == table)
            table = element->nextElement;
          else
            prev->nextElement = element->nextElement;
          delete element;
          // return successfully
          rc = ESMF_SUCCESS;
          return rc;
        }
        prev = element;
        element = element->nextElement;
      }
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- method not found in method table", ESMC_CONTEXT, &rc);
      return rc;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- empty method table", ESMC_CONTEXT, &rc);
      return rc;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::execute()"
  int MethodTable::execute(std::string labelArg, void *object, int *userRc,
    bool *existflag){
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    if (table){
      MethodElement *element = table; // initialize
      while (element){
        if (element->label == labelArg){
          if (existflag) *existflag = true;
          localrc = element->execute(object, userRc);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc; // bail out
          // return successfully
          rc = ESMF_SUCCESS;
          return rc;
        }
        element = element->nextElement;
      }
      if (existflag){
        *existflag = false;
        if (userRc) *userRc = ESMF_SUCCESS;
      }else{
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
          "- method not found in method table", ESMC_CONTEXT, &rc);
        return rc;
      }
    }else{
      if (existflag){
        *existflag = false;
        if (userRc) *userRc = ESMF_SUCCESS;
      }else{
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
          "- empty method table", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
} // namespace ESMCI

