// $Id: ESMCI_State.h,v 1.7 2008/03/31 22:25:25 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
#include "ESMC_LogErr.h"


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
     int addArray(State *state, Array *array);
     int getArray(State *state, char* name, Array **array);
     int destroy(State* state);
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

  void FTN(f_esmf_stateaddarray)(ESMCI::State* state, ESMCI::Array* array, 
                                 int* rc);

  void FTN(f_esmf_stategetarray)(ESMCI::State* state, char* name, 
                                 ESMCI::Array** array, int* rc, 
                                 ESMCI_FortranStrLenArg nlen);

  void FTN(f_esmf_statedestroy)(ESMCI::State* state, int* rc);

#if 0
      TODO: finish these prototypes
      void FTN(f_esmf_statecreate)(char *statename, statetype, compname,
            bundles, fields, arrays, nestedstates, names, itemcount, int *rc);
      void FTN(f_esmf_statedestroy)(ESMC_State *state, int *rc);
      void FTN(f_esmf_stateaddbundle)(ESMC_State *state, ESMC_Bundle *bundle, int *rc);
      void FTN(f_esmf_stateaddfield)(ESMC_State *state, ESMC_Field *field, int *rc);
      void FTN(f_esmf_stateaddarray)(ESMC_State *state, ESMC_Array *array, int *rc);
      void FTN(f_esmf_stateaddstate)(ESMC_State *state, nestedstate, int *rc);
      void FTN(f_esmf_stateadddataname)(ESMC_State *state, char *name, int *rc);
      void FTN(f_esmf_stategetinfo)(ESMC_State *state, char *statename, 
                          statetype, char *compname,
                          int *itemcount, char *itemnames, objtypes, int *rc);
      void FTN(f_esmf_stategetname)(ESMC_State *state, char *statename, int *rc);
      void FTN(f_esmf_stateisneeded)(ESMC_State *state, char *dataname, int *rc);
      void FTN(f_esmf_stategetneeded)(ESMC_State *state, char *dataname, 
                                 needed, int *rc);
      void FTN(f_esmf_statesetneeded)(ESMC_State *state, char *dataname, needed, int *rc);
      void FTN(f_esmf_stategetbundle)(ESMC_State *state, char *name, ESMC_Bundle *bundle, int *rc);
      void FTN(f_esmf_stategetfield)(ESMC_State *state, char *name, ESMC_Field *field, int *rc);
      void FTN(f_esmf_stategetarray)(ESMC_State *state, char *name, ESMC_Array *array, int *rc);
      void FTN(f_esmf_stategetstate)(ESMC_State *state, char *name, nestedstate, int *rc);
      void FTN(f_esmf_statewriterestart)(ESMC_State *state, iospec, int *rc);
      void FTN(f_esmf_statereadrestart)(char *name, iospec, int *rc);
      void FTN(f_esmf_statevalidate)(ESMC_State *state, char *options, int *rc);
      void FTN(f_esmf_stateprint)(ESMC_State *state, char *options, int *rc);
#endif
 }; // end prototypes for fortran interface

 #endif  // ESMC_State_H
