// $Id: ESMC_FTable_F.C,v 1.1 2003/02/27 21:28:26 nscollins Exp $
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
//
//EOP


// these interface subroutine names MUST be in lower case
extern "C" {

     // no need for explicit create methods - call the native class 
     // constructor and destructor methods directly.
     void FTN(c_esmc_ftablecreate)(ESMC_FTable **ptr, int *status) {
         (*ptr) = new ESMC_FTable;
         (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_ftabledestroy)(ESMC_FTable **ptr, int *status) {
         delete (*ptr);
         *ptr = 0;
         (*status) = ESMF_SUCCESS;
     }
  
     // call a function 
     void FTN(c_esmc_ftablecallroutine)(ESMC_FTable **ptr, int *type,
                                                                 int *status) {
         int funcrc;
         char *name;

         // TODO: two return codes here - one is whether we could find
         // the right function to call; the other is the actual return code
         // from the user function itself.
         switch(*type) {
           case ESMF_INIT:   name = "init";  break;
           case ESMF_RUN:    name = "run";   break;
           case ESMF_FINAL:  name = "final"; break;
           default:
             printf("ftablecallroutine, unrecognized routine type %d\n", *type);
             *status = ESMF_FAILURE;
             return;
         }

         *status = (*ptr)->ESMC_FTableCallVFuncPtr(name, &funcrc);
     }

     // get and set routines for both function and data pointers.
     // index them by name.
     void FTN(c_esmc_ftablegetroutine)(ESMC_FTable **ptr, int *type, 
                                 void **func, enum ftype *ftype, int *status) {
         char *name;

         switch(*type) {
           case ESMF_INIT:   name = "init";  break;
           case ESMF_RUN:    name = "run";   break;
           case ESMF_FINAL:  name = "final"; break;
           default:
             printf("ftablegetroutine: unrecognized routine type %d\n", *type);
             *status = ESMF_FAILURE;
             return;
         }

         *status = (*ptr)->ESMC_FTableGetFuncPtr(name, func, ftype);
     }

     void FTN(c_esmc_ftablegetdataptr)(ESMC_FTable **ptr, int *type,
                                 void **data, enum dtype *dtype, int *status) {
         char *name;

         switch(*type) {
           default:
             name = "localdata";
         }

         *status = (*ptr)->ESMC_FTableGetDataPtr(name, data, dtype);
     }

     void FTN(c_esmc_ftablesetroutine)(ESMC_FTable **ptr, int *type,
                                  void *func, enum ftype *ftype, int *status) {
         char *name;

         switch(*type) {
           case ESMF_INIT:   name = "init";  break;
           case ESMF_RUN:    name = "run";   break;
           case ESMF_FINAL:  name = "final"; break;
           default:
             printf("ftablesetroutine: unrecognized routine type %d\n", *type);
             *status = ESMF_FAILURE;
             return;
         }

         *status = (*ptr)->ESMC_FTableSetFuncPtr(name, func, *ftype);
     }

     void FTN(c_esmc_ftablesetdataptr)(ESMC_FTable **ptr, int *type,
                                  void *data, enum dtype *dtype, int *status) {
         char *name;

         switch(*type) {
           default:
             name = "localdata";
         }

         *status = (*ptr)->ESMC_FTableSetDataPtr(name, data, *dtype);
     }

};


