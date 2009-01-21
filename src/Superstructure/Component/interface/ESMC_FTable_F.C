// $Id: ESMC_FTable_F.C,v 1.23.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_Comp.h"
#include "ESMC_FTable.h"

#include "ESMC_VM.h"

#include "trim.h"
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
         *status = ESMC_RC_NOT_IMPL;

         (*ptr) = new ESMC_FTable;
         (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_ftabledestroy)(ESMC_FTable **ptr, int *status) {
         *status = ESMC_RC_NOT_IMPL;
         delete (*ptr);
         *ptr = 0;
         *status = ESMF_SUCCESS;
     }
  
     // call a function 
     void FTN(c_esmc_ftablecallentrypoint)(ESMC_FTable **ptr, char *type, 
                                        int *phase, int *status, int slen) {
         int funcrc;
         char *name;
         int localrc = ESMC_RC_NOT_IMPL;
         *status = ESMC_RC_NOT_IMPL;

         newtrim(type, slen, phase, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         // TODO: two return codes here - one is whether we could find
         // the right function to call; the other is the actual return code
         // from the user function itself.

         localrc = (*ptr)->ESMC_FTableCallVFuncPtr(name, &funcrc);

         if (status) {
             if (localrc != ESMF_SUCCESS)
                 *status = localrc;
             else if (funcrc != ESMF_SUCCESS)
                 *status = funcrc;
             else
                 *status = ESMF_SUCCESS;
	}
     
         delete[] name;
     }

     // get and set routines for both function and data pointers.
     // index them by name.
     void FTN(c_esmc_ftablegetentrypoint)(ESMC_FTable **ptr, char *type, 
                       void **func, enum ftype *ftype, int *status, int slen) {
         char *name;
         *status = ESMC_RC_NOT_IMPL;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableGetFuncPtr(name, func, ftype);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetentrypoint)(ESMC_FTable **ptr, char *type,
                                           void *func, int *status, int slen) {
         char *name;
         *status = ESMC_RC_NOT_IMPL;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableSetFuncPtr(name, func);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetargs)(ESMC_FTable **ptr, char *type,
                            int *acount, void **alist, int *status, int slen) {
         char *name;

         *status = ESMC_RC_NOT_IMPL;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableSetFuncArgs(name, *acount, alist);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetstateargs)(ESMC_FTable **ptr, char *type,
                         int *phase, void *comp, 
                         void *importState, void *exportState,
	                 void *clock, int *status, int slen) {

         char *fname;
         int acount = 5;
         void *alist[5];

         *status = ESMC_RC_NOT_IMPL;

         newtrim(type, slen, phase, NULL, &fname);
         //printf("after newtrim, name = '%s'\n", fname);

         alist[0] = (void *)comp;
         alist[1] = (void *)importState;
         alist[2] = (void *)exportState;
         alist[3] = (void *)clock;
         alist[4] = (void *)status;

         *status = (*ptr)->ESMC_FTableSetFuncArgs(fname, acount, alist);

         delete[] fname;
     }


     void FTN(c_esmc_ftablesetioargs)(ESMC_FTable **ptr, char *type,
                                       int *phase, void *comp, 
                                       void *iospec, void *clock, 
                                       int *status, int slen) {

         char *fname;
         int acount = 4;
         void *alist[4];

         newtrim(type, slen, phase, NULL, &fname);
         //printf("after newtrim, name = '%s'\n", fname);

         alist[0] = (void *)comp;
         alist[1] = (void *)iospec;
         alist[2] = (void *)clock;
         alist[3] = (void *)status;

         *status = (*ptr)->ESMC_FTableSetFuncArgs(fname, acount, alist);

         delete[] fname;
     }


     void FTN(c_esmc_ftablesetinternalstate)(ESMC_FTable ***ptr, char *type,
                        void **data, enum dtype *dtype, int *status, int slen) {
         char *name;

         *status = ESMC_RC_NOT_IMPL;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (**ptr)->ESMC_FTableSetDataPtr(name, data, *dtype);

         delete[] name;
     }

     void FTN(c_esmc_ftablegetinternalstate)(ESMC_FTable ***ptr, char *type,
                       void **data, enum dtype *dtype, int *status, int slen) {
         char *name;

         *status = ESMC_RC_NOT_IMPL;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (**ptr)->ESMC_FTableGetDataPtr(name, data, dtype);

         delete[] name;
     }
     
};


