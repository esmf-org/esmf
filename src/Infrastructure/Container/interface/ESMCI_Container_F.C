// $Id: ESMCI_Container_F.C,v 1.12 2011/05/10 01:27:10 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Container_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMCI_LogMacros.inc"
#include "ESMCI_F90Interface.h"
#include "ESMCI_Field.h"

#include "ESMCI_Container.h"

#include <string>
#include <vector>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Container} class functions.
//
//EOP
//-------------------------------------------------------------------------

// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:
        
  void FTN(c_esmc_containeradd)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *itemName, ESMCI::F90ClassHolder *f90p, 
    ESMC_Logical *relaxedflag, int *rc, ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containeradd()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    bool relaxed;
    if (*relaxedflag == ESMF_TRUE)
      relaxed=true;
    else
      relaxed=false;

    // call into C++
    try{
      
      (*ptr)->add(std::string(itemName,nlen), *f90p, relaxed);
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containeraddreplace)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *itemName, ESMCI::F90ClassHolder *f90p, 
    int *rc, ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containeraddreplace()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{
      
      (*ptr)->addReplace(std::string(itemName,nlen), *f90p);
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containercreate)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containercreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // call into C++
    try{
      
      *ptr = new ESMCI::Container<std::string, ESMCI::F90ClassHolder>;
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containerdestroy)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containerdestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{
      
      delete *ptr;
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergetcount)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    int *count, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergetcount()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      *count = (*ptr)->size();
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergarbageon)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergarbageon()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      (*ptr)->garbageOn();
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergarbageoff)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergarbageoff()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      (*ptr)->garbageOff();
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergarbageclear)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergarbageclear()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      (*ptr)->garbageClear();
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergarbagecount)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    int *count, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergarbagecount()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      *count = (*ptr)->garbageCount();
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergetfield)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *fieldName, ESMCI::F90ClassHolder *f90p, int *rc, 
    ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergetfield()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      ESMCI::Field field = (*ptr)->get(std::string(fieldName, nlen));
      
      // cast C++ Field object into Fortran
      localrc = field.castToFortran(f90p);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergetispresent)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *itemName, ESMC_Logical *isPresent, int *rc,
    ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergetispresent()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      if ((*ptr)->isPresent(std::string(itemName, nlen)))
        *isPresent = ESMF_TRUE;
      else
        *isPresent = ESMF_FALSE;

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergetvector)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    std::vector<ESMCI::F90ClassHolder> **vector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergetvector()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      // construct persistent vector object
      *vector = new std::vector<ESMCI::F90ClassHolder>;
      // query the C++ layer
      (*ptr)->getVector(**vector);
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergarbageget)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    std::vector<ESMCI::F90ClassHolder> **vector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergarbageget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      // construct persistent vector object
      *vector = new std::vector<ESMCI::F90ClassHolder>;
      // query the C++ layer
      (*ptr)->garbageGet(**vector);
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containergetvectoritem)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    std::vector<ESMCI::F90ClassHolder> **vector, int *item, 
    ESMCI::F90ClassHolder *f90p, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergetvectoritem()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      // query the C++ layer
      ESMCI::Field field = (**vector)[*item];
      
      // cast C++ Field object into Fortran
      localrc = field.castToFortran(f90p);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return;
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containerreleasevector)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    std::vector<ESMCI::F90ClassHolder> **vector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containerreleasevector()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{

      if (*vector != NULL){
        // release persistent vector object
        delete *vector;
      }
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containerremove)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *itemName, ESMC_Logical *relaxedflag, int *rc,
    ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containerremove()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    bool relaxed;
    if (*relaxedflag == ESMF_TRUE)
      relaxed=true;
    else
      relaxed=false;

    // call into C++
    try{

      (*ptr)->remove(std::string(itemName, nlen), relaxed);
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containerreplace)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *itemName, ESMCI::F90ClassHolder *f90p, 
    ESMC_Logical *relaxedflag, int *rc, ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containerreplace()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    bool relaxed;
    if (*relaxedflag == ESMF_TRUE)
      relaxed=true;
    else
      relaxed=false;

    // call into C++
    try{
      
      (*ptr)->replace(std::string(itemName,nlen), *f90p, relaxed);
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  //-------------------------------------------------------------------------

  void FTN(c_esmc_containerprint)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containerprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // call into C++
    try{
      
      (*ptr)->print();
      
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

}
