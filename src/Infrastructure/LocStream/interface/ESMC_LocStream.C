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
#define ESMC_FILENAME "ESMC_LocStream.C"
//==============================================================================
//
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// This file contains the C interfaces' code for the {\tt LocStream} class
// functions.
//
//EOP
//------------------------------------------------------------------------------
// INCLUDES
#include "ESMC_LocStream.h"

#include "ESMCI_LocStream.h"
#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"

using namespace ESMCI;

extern "C" {

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocStreamCreateLocal()"
  ESMC_LocStream ESMC_LocStreamCreateLocal(int ls_size, enum ESMC_IndexFlag *indexflag,
                                           enum ESMC_CoordSys_Flag *coordSys, int *rc){
    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    ESMC_LocStream locstream;

    locstream.ptr = reinterpret_cast<void *>(ESMCI::LocStream::create(ls_size,
                                             indexflag, coordSys, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc)){
      locstream.ptr = NULL;
      return locstream;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return locstream;
  }

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocStreamGetBounds()"
  int ESMC_LocStreamGetBounds(ESMC_LocStream locstream,
                        int localDe,
                        int *cLBound,
                        int *cUBound){

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;   // final return code

    // typecase into ESMCI type
    ESMCI::LocStream *locstreamp = reinterpret_cast<ESMCI::LocStream *>(locstream.ptr);

    // Invoke the C++ interface
    localrc = ESMCI::LocStream::getbounds(locstreamp, localDe, cLBound, cUBound);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &rc)) return rc;  // bail out

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocStreamAddKeyAlloc()"
  int ESMC_LocStreamAddKeyAlloc(ESMC_LocStream locstream,
                                const char *keyName,
                                enum ESMC_TypeKind_Flag *keyTypeKind){

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;   // final return code

    // typecase into ESMCI type
    ESMCI::LocStream *locstreamp = reinterpret_cast<ESMCI::LocStream *>(locstream.ptr);

    // Invoke the C++ interface
    localrc = ESMCI::LocStream::addKeyAlloc(locstreamp, keyName, keyTypeKind);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &rc)) return rc;  // bail out

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocStreamGetKeyPtr()"
  void *ESMC_LocStreamGetKeyPtr(ESMC_LocStream locstream, const char *keyName,
                               int localDe, int *rc){

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

    ESMC_Array array = ESMC_LocStreamGetKeyArray(locstream, keyName, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      rc)) return NULL;  // bail out

    void *ptr = ESMC_ArrayGetPtr(array, localDe, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      rc)) return NULL;  // bail out

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return ptr;
  }


//--------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocStreamGetKeyArray()"
  ESMC_Array ESMC_LocStreamGetKeyArray(ESMC_LocStream locstream, const char *keyName, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // typecase into ESMCI type
    ESMCI::LocStream *locstreamp = reinterpret_cast<ESMCI::LocStream *>(locstream.ptr);

    // Invoke the C++ interface
    ESMC_Array array = locstreamp->getKeyArray(keyName, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      rc)) return array;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return array;
  }


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocStreamDestroy()"
  int ESMC_LocStreamDestroy(ESMC_LocStream *locstream){
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    // typecase into ESMCI type
    ESMCI::LocStream *locstreamp = reinterpret_cast<ESMCI::LocStream *>(locstream->ptr);

    // Invoke the C++ interface
    localrc = ESMCI::LocStream::destroy(locstreamp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &rc)) return rc;

    // invalidate pointer
    locstream->ptr = NULL;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  //--------------------------------------------------------------------------


}
