//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_Field_h
#define ESMC_Field_h

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Field - Public C interface to the ESMF Field class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Field class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Field.C} contains
// the definitions (full code bodies) for the Field methods.
//
//EOP
//-----------------------------------------------------------------------------


#if defined (__cplusplus)
extern "C" {
#endif

#include "ESMC_Mesh.h"
#include "ESMC_ArraySpec.h"

typedef struct {
  void * ptr;
  long long isinit;
} ESMC_Field;

ESMC_Field ESMC_FieldCreate(ESMC_Mesh *mesh, ESMC_ArraySpec *arrayspec, int *gridToFieldMap, int *ungriddedLBound, int *ungriddedUBound, const char *name, int *rc);

int ESMC_FieldGet(ESMC_Field *field, ESMC_Mesh *mesh, int *rc);

int ESMC_FieldDestroy(ESMC_Field *field, int *rc);

#if defined (__cplusplus)
} // extern "C"
#endif

#endif
