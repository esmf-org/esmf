
// $Id: ESMCI_Field.h,v 1.2.2.1 2010/02/05 19:55:41 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_Field_H
#define ESMCI_Field_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Field - one line general Fieldment about this class
//
// !DESCRIPTION:
//
// The code in this file defines the ESMC Field class prototypes for the
// fortran interface routines. The companion file ESMC\_Field_C.F90  contains
// the definitions (full code bodies) for the interface routines.
//
//EOP 
//

//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMC_Mesh.h"
#include "ESMC_ArraySpec.h"
#include "ESMCI_F90Interface.h"
#include "ESMC_Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// C++ Field class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
namespace ESMCI{

  class Field{
    // pointer to fortran derived type
    F90ClassHolder fortranclass;
    // methods
   public:
    static Field* create(ESMC_Mesh mesh, ESMC_ArraySpec arrayspec,
      ESMC_InterfaceInt gridToFieldMap, ESMC_InterfaceInt ungriddedLBound,
      ESMC_InterfaceInt ungriddedUBound, const char *name, int *rc); 
    static int destroy(Field *field);
    int print();
    ESMC_Mesh getMesh(int *rc);
  }; 
}

#endif  // ESMCI_Field_H
