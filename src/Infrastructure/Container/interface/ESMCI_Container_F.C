// $Id: ESMCI_Container_F.C,v 1.4 2011/04/19 00:16:16 theurich Exp $
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
    char const *fieldName, ESMCI::F90ClassHolder *f90p, int *rc, 
    ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containeradd()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    try{
      
      // call into C++
      (*ptr)->add(std::string(fieldName,nlen),*f90p);
      
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
    
    try{
      
      // call into C++
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

    try{
      
      // call into C++
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

  void FTN(c_esmc_containergetfield)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *fieldName, ESMCI::F90ClassHolder *f90p, int *rc, 
    ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containergetfield()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    try{

      // query the C++ layer
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

  void FTN(c_esmc_containerremove)
    (ESMCI::Container<std::string, ESMCI::F90ClassHolder> **ptr, 
    char const *itemName, int *rc, ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_containerremove()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    try{

      // call the C++ layer
      (*ptr)->remove(std::string(itemName, nlen), true);
      
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

    try{
      
      // call into C++
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
