// $Id: ESMC_Field.C,v 1.12 2009/09/21 20:38:31 feiliu Exp $
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

using namespace ESMCI;

extern "C" {
// Prototypes of the Fortran interface functions.
void FTN(f_esmf_fieldcreate)(ESMC_Field *fieldp, ESMC_Mesh *mesh, ESMC_ArraySpec *arrayspec, int *gridToFieldMap, 
    int *ungriddedLBound, int *ungriddedUBound, int *rc);

void FTN(f_esmf_fieldget)(ESMC_Field *field, ESMC_Mesh *mesh, int *rc);

void FTN(f_esmf_fielddestroy)(ESMC_Field *field, int *rc);

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldCreate()"
ESMC_Field ESMC_FieldCreate(ESMC_Mesh *mesh, ESMC_ArraySpec *arrayspec, int *gridToFieldMap, 
    int *ungriddedLBound, int *ungriddedUBound, const char *name, int *rc){
  int localrc;

  // Initialize return code. Assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  ESMC_Field field;

  FTN(f_esmf_fieldcreate)(&field, mesh, arrayspec, gridToFieldMap, ungriddedLBound, ungriddedUBound, rc);

  if (rc) *rc = localrc;

  return field;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_FieldGet()"
int ESMC_FieldGet(ESMC_Field *field, ESMC_Mesh *mesh, int *rc){

    return 0;
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
   
   FTN(f_esmf_fielddestroy)(field, rc);

   localrc = ESMF_SUCCESS;

   return localrc;

} // ESMC_FieldDestroy

} // extern "C"
