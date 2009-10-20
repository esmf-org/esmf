// $Id: ESMC_Field.C,v 1.16 2009/10/20 18:45:46 feiliu Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Field.C"
//==============================================================================
//
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// This file contains the C interfaces' code for the {\tt Field} class functions.
//
//EOP
//------------------------------------------------------------------------------
// INCLUDES

#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMC_Field.h"
#include "ESMCI_Field.h"
#include "ESMCI_F90Interface.h"
#include "ESMC_Interface.h"

using namespace ESMCI;

extern "C" {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreate()"
ESMC_Field ESMC_FieldCreate(ESMC_Mesh mesh, ESMC_ArraySpec arrayspec, ESMC_InterfaceInt gridToFieldMap, 
    ESMC_InterfaceInt ungriddedLBound, ESMC_InterfaceInt ungriddedUBound, const char *name, int *rc){
    int localrc;

    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    localrc = ESMF_RC_NOT_IMPL;

    ESMC_Field field = NULL;  // initialize

    // Invoque the C++ interface
    field = reinterpret_cast<void *>(ESMCI::Field::create(mesh, arrayspec, gridToFieldMap, ungriddedLBound, ungriddedUBound, name, &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return field; // bail out

    if(rc) *rc = localrc;
    return field;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldGet()"
int ESMC_FieldGet(ESMC_Field field, ESMC_Mesh *mesh, int *rc){

    int localrc;
   
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field);

    // Invoque the C++ interface
    localrc = fieldp->getMesh(mesh);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return localrc;

    // invalidate pointer
    field = NULL;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return localrc;

}


//--------------------------------------------------------------------------
// !BOP
// !IROUTINE: ESMC_FieldDestroy
//
// !EOP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldDestroy()"
int ESMC_FieldDestroy(ESMC_Field field, int *rc){
    int localrc;
   
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::Field *fieldp = reinterpret_cast<ESMCI::Field *>(field);

    // Invoque the C++ interface
    localrc = ESMCI::Field::destroy(fieldp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return localrc;

    // invalidate pointer
    field = NULL;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return localrc;

}

}
