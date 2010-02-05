// $Id: ESMC_Field.h,v 1.16.2.1 2010/02/05 19:55:44 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

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
#include "ESMC_Interface.h"

// Class declaration type
typedef void * ESMC_Field;

// Class API
ESMC_Field ESMC_FieldCreate(ESMC_Mesh mesh, ESMC_ArraySpec arrayspec,
  ESMC_InterfaceInt gridToFieldMap, ESMC_InterfaceInt ungriddedLBound,
  ESMC_InterfaceInt ungriddedUBound, const char *name, int *rc);

int ESMC_FieldDestroy(ESMC_Field *field);

int ESMC_FieldPrint(ESMC_Field field);

ESMC_Mesh ESMC_FieldGetMesh(ESMC_Field field, int *rc);


#if defined (__cplusplus)
} // extern "C"
#endif

#endif
