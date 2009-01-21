// $Id: ESMCI_State.h,v 1.15 2009/01/21 21:38:02 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_State_H
#define ESMCI_State_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_State - one line general Statement about this class
//
// !DESCRIPTION:
//
// The code in this file defines the ESMC State class prototypes for the
// fortran interface routines. The companion file ESMC\_State_C.F90  contains
// the definitions (full code bodies) for the interface routines.
//
// 
//

//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMCI_Array.h"
#include "ESMCI_F90Interface.h"


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// C++ State class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
namespace ESMCI{
  class State{
    // pointer to fortran derived type
    F90ClassHolder fortranclass;
    // methods
    public:
    static State* create(char* name, int *rc);
    int addArray(Array *array);
    int print();
    int getArray(char* name, Array **array);
    static int destroy(State *state);
  }; // class State
};// namespace ESMCI


#endif  // ESMCI_State_H
