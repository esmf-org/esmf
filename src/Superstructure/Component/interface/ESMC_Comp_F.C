// $Id: ESMC_Comp_F.C,v 1.22 2004/04/13 17:30:46 nscollins Exp $
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


static void ESMC_SetTypedEP(void *ptr, char *tname, int slen, int *phase, 
                        int nstate, enum ftype ftype, void *func, int *status) {
     char *name;
     int *tablerc = new int;
     void *f90comp = ptr;
     ESMC_FTable *tabptr = **(ESMC_FTable***)ptr;

     newtrim(tname, slen, phase, &nstate, &name);
         
     //printf("SetTypedEP: setting function name = '%s'\n", name);
     if (ftype == FT_VOIDPINTP)
         *status = (tabptr)->ESMC_FTableSetFuncPtr(name, func, f90comp, tablerc);
     else
         *status = (tabptr)->ESMC_FTableSetFuncPtr(name, func, ftype);

     delete[] name;
}

static void ESMC_GetDP(ESMC_FTable ***ptr, void **datap, int *status) {
    char *name = "localdata";
    enum dtype dtype;

    *status = (**ptr)->ESMC_FTableGetDataPtr(name, datap, &dtype);
}

static void ESMC_SetDP(ESMC_FTable ***ptr, void **datap, int *status) {
    char *name = "localdata";
    enum dtype dtype = DT_VOIDP;

    *status = (**ptr)->ESMC_FTableSetDataPtr(name, *datap, dtype);
}

extern "C" void ESMC_SetServ(void *ptr, int (*func)(), int *status) {
     int rc, funcrc;
     int *tablerc = new int;
     void *f90comp = ptr;
     ESMC_FTable *tabptr = **(ESMC_FTable***)ptr;
     
     // TODO: shouldn't need to expand the table here - should be buried
     // inside ftable code.
     rc = (tabptr)->ESMC_FTableExtend(8, 2); // room for 8 funcs, 2 data
     if (rc != ESMF_SUCCESS) {
         *status = rc;
         return;
     }

     rc = (tabptr)->ESMC_FTableSetFuncPtr("register", (void *)func, 
                                                           f90comp, tablerc);
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
  
     // TODO:  see if it is possible to make this simpler and call directly:
     // rc = (*func)(comp, func_rc);
     // *status = ESMF_SUCCESS;
     // return; 
}


// these interface subroutine names MUST be in lower case
extern "C" {

     // these functions have no leading c_ and are ESMF and not ESMC because 
     // they're intended to be called directly by F90 user code.  
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

     // ---------- Set Services ---------------
     void FTN(esmf_gridcompsetservices)(void *ptr, int (*func)(), int *status) {
         ESMC_SetServ(ptr, func, status);
     }
     void FTN(esmf_cplcompsetservices)(void *ptr, int (*func)(), int *status) {
         ESMC_SetServ(ptr, func, status);
     }

     void FTN(esmf_usercompsetservices)(void *ptr, int (*func)(), int *status) {
         ESMC_SetServ(ptr, func, status);
     }

     // ---------- Set Entry Point ---------------
     void FTN(esmf_gridcompsetentrypoint)(void *ptr, char *tname,
                               void *func, int *phase, int *status, int slen) {
        //ESMC_SetTypedEP(ptr, tname, slen, phase, 1, FT_COMP1STAT, func, status);
        ESMC_SetTypedEP(ptr, tname, slen, phase, 0, FT_COMP2STAT, func, status);
     }
     void FTN(esmf_cplcompsetentrypoint)(void *ptr, char *tname,
                               void *func, int *phase, int *status, int slen) {
        //ESMC_SetTypedEP(ptr, tname, slen, phase, 1, FT_COMP1STAT, func, status);
        ESMC_SetTypedEP(ptr, tname, slen, phase, 0, FT_COMP2STAT, func, status);
     }

     void FTN(esmf_usercompsetentrypoint)(void *ptr, char *tname,
                             void *func, int *phase, int *status, int slen) {
       ESMC_SetTypedEP(ptr, tname, slen, phase, 0, FT_VOIDPINTP, func,  status);
     }


     // ---------- Set Internal State ---------------
     void FTN(esmf_gridcompsetinternalstate)(ESMC_FTable ***ptr, 
                                                 void **datap, int *status) {
         ESMC_SetDP(ptr, datap, status);
     }
     void FTN(esmf_cplcompsetinternalstate)(ESMC_FTable ***ptr, 
                                                 void **datap, int *status) {
         ESMC_SetDP(ptr, datap, status);
     }

     void FTN(esmf_usercompsetinternalstate)(ESMC_FTable ***ptr, char *name, 
                                         void **datap, int *status, int slen) {
         char *tbuf; 
         enum dtype dtype = DT_VOIDP;

         newtrim(name, slen, NULL, NULL, &tbuf);
         //printf("after newtrim, name = '%s'\n", tbuf);

         *status = (**ptr)->ESMC_FTableSetDataPtr(tbuf, *datap, dtype);
  
         delete[] tbuf;
     }

     // ---------- Get Internal State ---------------
     void FTN(esmf_gridcompgetinternalstate)(ESMC_FTable ***ptr, 
                                                 void **datap, int *status) {
         ESMC_GetDP(ptr, datap, status);
     }
     void FTN(esmf_cplcompgetinternalstate)(ESMC_FTable ***ptr, 
                                                 void **datap, int *status) {
         ESMC_GetDP(ptr, datap, status);
     }

     void FTN(esmf_usercompgetinternalstate)(ESMC_FTable ***ptr, char *name,
                                         void **datap, int *status, int slen) {
         char *tbuf; 
         enum dtype dtype;

         newtrim(name, slen, NULL, NULL, &tbuf);
         //printf("after newtrim, name = '%s'\n", tbuf);

         *status = (**ptr)->ESMC_FTableGetDataPtr(tbuf, datap, &dtype);
  
         delete[] tbuf;
     }

}


// these interface subroutine names MUST be in lower case
extern "C" {

  void FTN(c_esmc_compwait)(
    ESMC_VM **ptr_vm_parent,  // p2 to the parent VM
    ESMC_VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
    void **vm_info,           // p2 to member which holds info
    void **vm_cargo,          // p2 to member which holds cargo
    int *status) {            // return error code in status

    // Things get a little confusing here with pointers, so I will define
    // some temp. variables that make matters a little clearer I hope:
    ESMC_VM &vm_parent = **ptr_vm_parent;     // reference to parent VM
    ESMC_VMPlan &vmplan = **ptr_vmplan;       // reference to VMPlan

    // Now call the vmachine_exit function which will block respective PETs
    vm_parent.vmachine_exit(vmplan, *vm_info);
    cargotype *cargo = (cargotype *)*vm_cargo;
    delete cargo;

    *status = ESMF_SUCCESS;
  }

}
