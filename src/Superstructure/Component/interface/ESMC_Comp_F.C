// $Id: ESMC_Comp_F.C,v 1.8 2003/02/27 21:28:26 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//==============================================================================
//
//==============================================================================
//
// This file contains Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_Comp.h"
#include "ESMC_FTable.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The {\tt Component} implementation language is Fortran 90, but the
// routines which register function and data addresses to be used later
// in callback code must be implemented in C++.  These routines here
// allow the F90 to call the C++ support routines.
//
// For the general C++ interfaces to the public entry points, see
// the file {\tt ESMF_Comp_C.F90}.
//
//
//EOP


// these interface subroutine names MUST be in lower case
extern "C" {

     // these functions have no leading c_ and are ESMF and not ESMC because 
     // they're to be called directly by F90 user code.  making them different
     // seems like an invitation to errors; also if they don't have the
     // leading c_ then we may run into naming conflicts with the real
     // C++ code which does the work (which was why that prefix was added 
     // in the first place).
     //
     // also note they CANNOT have prototypes in fortran because the routine 
     // types and data types are private/different for each call so there
     // is no correct prototype syntax which will work.
     //
     // and finally, note that they have an extra level of indirection,
     // because the first arg is actually being called with a component
     // pointer - and after one dereference we are at the component derived
     // type.  the second dereference finds the ftable pointer which must
     // be the first entry in the comp derived type.

     void FTN(esmf_compregister)(void *ptr, int (*func)(), int *status) {
         int rc, funcrc;
         int *tablerc = new int;
         void *f90comp = ptr;
         ESMC_FTable *tabptr = **(ESMC_FTable***)ptr;
         
         rc = (tabptr)->ESMC_FTableExtend(8, 0); // room for 8 funcs, 0 data
         if (rc != ESMF_SUCCESS) {
             *status = rc;
             return;
         }

         rc = (tabptr)->ESMC_FTableSetFuncPtr("register", (void *)func, 
                              FT_VOIDPINTP, (void *)f90comp, (void *)tablerc);
         if (rc != ESMF_SUCCESS) {
             *status = rc;
             return;
         }

         rc = (tabptr)->ESMC_FTableCallVFuncPtr("register", &funcrc);
         if (rc != ESMF_SUCCESS) {
             *status = rc;
             return;
         }

         if (funcrc != ESMF_SUCCESS) {
             *status = funcrc;
             return;
         }
         *status = ESMF_SUCCESS;
         return;
      
         // TODO:  is it possible to make this simpler and call it directly:
         // rc = (*func)(comp, func_rc);
         // *status = ESMF_SUCCESS;
         // return; 
     }

     // TODO: type used to be a string, which is more general, but i'm having
     // problems passing strings between F90 routines directly to C w/o
     // prototypes (which is necessary since func can't be a typed variable)
     void FTN(esmf_compsetroutine)(void *ptr, int *type,
                                     int *phase, void *func, int *status) {
         char *name;
         int *tablerc = new int;
         void *f90comp = ptr;
         ESMC_FTable *tabptr = **(ESMC_FTable***)ptr;

         switch(*type) {
           case ESMF_INIT:   name = "init";  break;
           case ESMF_RUN:    name = "run";   break;
           case ESMF_FINAL:  name = "final"; break;
           default:
             printf("compsetroutine: unrecognized routine type %d\n", *type);
             *status = ESMF_FAILURE;
             return;
         }
         //printf("SetRoutine: setting function name = '%s'\n", name);
         if (*phase == 1)
             *status = (tabptr)->ESMC_FTableSetFuncPtr(name, func, 
                              FT_VOIDPINTP, (void *)f90comp, (void *)tablerc);
         else {
             char *tbuf = new char[strlen(name) + 4];
             sprintf(tbuf, "%s%02d", name, *phase);
             *status = (tabptr)->ESMC_FTableSetFuncPtr(tbuf, func, 
                              FT_VOIDPINTP, (void *)f90comp, (void *)tablerc);
         }
     }

     // TODO: ditto as above
     void FTN(esmf_compsetdataptr)(ESMC_FTable ***ptr, int *type,
                                                 void *datap, int *status) {
         char *name = "localdata";
         *status = (**ptr)->ESMC_FTableSetDataPtr(name, datap, DT_VOIDP);
     }

};


