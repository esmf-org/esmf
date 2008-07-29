// $Id: ESMCI_State.h,v 1.12 2008/07/29 01:34:57 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF State C declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
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
#include "ESMC_Start.h"
#include "ESMCI_LogErr.h"


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// C++ State class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
   namespace ESMCI{
   class State{

     // pointer to fortran derived type
     ESMC_F90ClassHolder* f90this;

     public:
     static State* create(char* name, int *rc);
     int addArray( Array *array);
     int print();
     int getArray(char* name, Array **array);
     int destroy();
   }; // class State
   };// namespace ESMCI

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// prototypes for the fortran interface routines.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
extern "C" {

  void FTN(f_esmf_statecreate)(ESMCI::State* state, char* statename, int* rc,
				ESMCI_FortranStrLenArg nlen);

  void FTN(f_esmf_stateaddarray)(ESMCI::State* state, ESMCI::Array** array, 
                                 int* rc);

  void FTN(f_esmf_stateprint)(ESMCI::State* state, int* rc);

  void FTN(f_esmf_stategetarray)(ESMCI::State* state, char* name, 
                                 ESMCI::Array** array, int* rc, 
                                 ESMCI_FortranStrLenArg nlen);

  void FTN(f_esmf_statedestroy)(ESMCI::State* state, int* rc);

 }; // end prototypes for fortran interface

 #endif  // ESMC_State_H
