// $Id: ESMC_Field.C,v 1.14 2009/10/12 16:53:34 feiliu Exp $
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

#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMC_Field.h"
#include "ESMCI_F90Interface.h"

using namespace ESMCI;

extern "C" {
#include <string.h>
// Prototypes of the Fortran interface functions.
void FTN(f_esmf_fieldcreate)(ESMC_Field *fieldp, void *mesh_pointer, ESMC_ArraySpec *arrayspec, 
    int *gridToFieldMap, int *len1, 
    int *ungriddedLBound, int *len2,
    int *ungriddedUBound, int *len3,
    char *name, 
    int *rc,
    ESMCI_FortranStrLenArg nlen);

void FTN(f_esmf_fieldget)(ESMC_Field *field, void *mesh_pointer, int *rc);

void FTN(f_esmf_fielddestroy)(ESMC_Field *field, int *rc);

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreate()"
ESMC_Field ESMC_FieldCreate(ESMC_Mesh *mesh, ESMC_ArraySpec *arrayspec, ESMC_InterfaceInt gridToFieldMap, 
    ESMC_InterfaceInt ungriddedLBound, ESMC_InterfaceInt ungriddedUBound, const char *name, int *rc){
  int localrc;

  // Initialize return code. Assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  ESMCI::InterfaceInt *gtfm = (ESMCI::InterfaceInt *)(gridToFieldMap.ptr);
  ESMCI::InterfaceInt *uglb = (ESMCI::InterfaceInt *)(ungriddedLBound.ptr);
  ESMCI::InterfaceInt *ugub = (ESMCI::InterfaceInt *)(ungriddedUBound.ptr);

  ESMC_Field field;
  memset(reinterpret_cast<void *>(&field), 0, sizeof(ESMC_Field));

  if(gtfm->dimCount != 1){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
       "- gridToFieldMap array must be of rank 1", rc);
     return field;
  }
  if(uglb->dimCount != 1){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
       "- ungriddedLBound array must be of rank 1", rc);
     return field;
  }
  if(ugub->dimCount != 1){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
       "- ungriddedUBound array must be of rank 1", rc);
     return field;
  }

  int slen = strlen(name);
  char * fName = new char[slen];
  localrc = ESMC_CtoF90string(name, fName, slen);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) {
      delete[] fName;
      return field;
  }

  FTN(f_esmf_fieldcreate)(&field, mesh->ptr, arrayspec, gtfm->array, &gtfm->extent[0], uglb->array, &uglb->extent[0], ugub->array, &ugub->extent[0], fName, &localrc, slen);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) {
      return field;
  }

  delete fName;

  if (rc) *rc = localrc;

  return field;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldGet()"
int ESMC_FieldGet(ESMC_Field *field, ESMC_Mesh *mesh, int *rc){

    int localrc;

    // Initialize return code. Assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;
    if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

    mesh->ptr = malloc(sizeof(void *));
    FTN(f_esmf_fieldget)(field, mesh->ptr, &localrc);
  
    if (rc) *rc = localrc;
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
int ESMC_FieldDestroy(ESMC_Field *field, int *rc){
   int localrc;
   // Initialize return code. Assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
   if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;
   
   FTN(f_esmf_fielddestroy)(field, &localrc);

   localrc = ESMF_SUCCESS;

   if (rc) *rc = localrc;
   return localrc;

} // ESMC_FieldDestroy

} // extern "C"
