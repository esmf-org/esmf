// $Id: ESMC_Comp_F.C,v 1.27 2004/09/17 13:56:47 theurich Exp $
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
#define ESMF_FILENAME "ESMC_Comp_F.C"
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"
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


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SetTypedEP"
static void ESMC_SetTypedEP(void *ptr, char *tname, int slen, int *phase, 
                        int nstate, enum ftype ftype, void *func, int *status) {
     char *name;
     int *tablerc = new int;
     int localrc;
     void *f90comp = ptr;
     ESMC_FTable *tabptr;

     //printf("ptr = 0x%08x\n", (unsigned long)ptr);
     //printf("*ptr = 0x%08x\n", (unsigned long)(*(int*)ptr));
     if ((ptr == ESMC_NULL_POINTER) || ((*(void**)ptr) == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
        return;
     }

     tabptr = **(ESMC_FTable***)ptr;
     //printf("tabptr = 0x%08x\n", (unsigned long)(tabptr));
     newtrim(tname, slen, phase, &nstate, &name);
         
     //printf("SetTypedEP: setting function name = '%s'\n", name);
     if (ftype == FT_VOIDPINTP)
         localrc = (tabptr)->ESMC_FTableSetFuncPtr(name, func, f90comp, tablerc);
     else
         localrc = (tabptr)->ESMC_FTableSetFuncPtr(name, func, ftype);

     if (status) *status = localrc;
     delete[] name;
     delete tablerc;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GetDP"
static void ESMC_GetDP(ESMC_FTable ***ptr, void **datap, int *status) {
    char *name = "localdata";
    enum dtype dtype;
    int localrc;

     //printf("ptr = 0x%08x\n", (unsigned long)ptr);
     //printf("*ptr = 0x%08x\n", (unsigned long)(*(int*)ptr));
    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
       return;
    }

    localrc = (**ptr)->ESMC_FTableGetDataPtr(name, datap, &dtype);
    if (status) *status = localrc;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SetDP"
static void ESMC_SetDP(ESMC_FTable ***ptr, void **datap, int *status) {
    char *name = "localdata";
    enum dtype dtype = DT_VOIDP;
    int localrc;

     //printf("ptr = 0x%08x\n", (unsigned long)ptr);
     //printf("*ptr = 0x%08x\n", (unsigned long)(*(int*)ptr));
    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
        return;
    }

    localrc = (**ptr)->ESMC_FTableSetDataPtr(name, *datap, dtype);
    if (status) *status = localrc;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SetServ"
extern "C" void ESMC_SetServ(void *ptr, int (*func)(), int *status) {
     int localrc, funcrc;
     int *tablerc = new int;
     void *f90comp = ptr;
     ESMC_FTable *tabptr;
     

     //printf("ptr = 0x%08x\n", (unsigned long)ptr);
     //printf("*ptr = 0x%08x\n", (unsigned long)(*(int*)ptr));
     //if ((ptr == ESMC_NULL_POINTER)) {
     if ((ptr == ESMC_NULL_POINTER) || ((*(void**)ptr) == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
        return;
     }
     tabptr = **(ESMC_FTable***)ptr;
     //printf("tabptr = 0x%08x\n", (unsigned long)(tabptr));

     // TODO: shouldn't need to expand the table here - should be buried
     // inside ftable code.
     localrc = (tabptr)->ESMC_FTableExtend(8, 2); // room for 8 funcs, 2 data
     if (localrc != ESMF_SUCCESS) {
         if (status) *status = localrc;
         return;
     }

     localrc = (tabptr)->ESMC_FTableSetFuncPtr("register", (void *)func, 
                                                           f90comp, tablerc);
     if (localrc != ESMF_SUCCESS) {
         if (status) *status = localrc;
         return;
     }

     localrc = (tabptr)->ESMC_FTableCallVFuncPtr("register", &funcrc);
     if (localrc != ESMF_SUCCESS) {
         if (status) *status = localrc;
         return;
     }

     if (funcrc != ESMF_SUCCESS) {
         if (status) *status = funcrc;
         return;
     }
     if (status) *status = ESMF_SUCCESS;
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

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMF_UserCompSetInternalState"
     void FTN(esmf_usercompsetinternalstate)(ESMC_FTable ***ptr, char *name, 
                                         void **datap, int *status, int slen) {
         char *tbuf; 
         enum dtype dtype = DT_VOIDP;
         int localrc;

         if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
            return;
         }

         newtrim(name, slen, NULL, NULL, &tbuf);
         //printf("after newtrim, name = '%s'\n", tbuf);

         localrc = (**ptr)->ESMC_FTableSetDataPtr(tbuf, *datap, dtype);
  
         delete[] tbuf;
         if (status) *status = localrc;
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

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMF_UserCompGetInternalState"
     void FTN(esmf_usercompgetinternalstate)(ESMC_FTable ***ptr, char *name,
                                         void **datap, int *status, int slen) {
         char *tbuf; 
         enum dtype dtype;
         int localrc;

         if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
            return;
         }

         newtrim(name, slen, NULL, NULL, &tbuf);
         //printf("after newtrim, name = '%s'\n", tbuf);

         localrc = (**ptr)->ESMC_FTableGetDataPtr(tbuf, datap, &dtype);
  
         delete[] tbuf;
         if (status) *status = localrc;
     }

}


// these interface subroutine names MUST be in lower case
extern "C" {

  void FTN(c_esmc_compwait)(
    ESMC_VM **ptr_vm_parent,  // p2 to the parent VM
    ESMC_VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
    void **vm_info,           // p2 to member which holds info
    void **vm_cargo,          // p2 to member which holds cargo
    int *callrc,              // return code of the user component method
    int *status) {            // return error code in status

    // Things get a little confusing here with pointers, so I will define
    // some temp. variables that make matters a little clearer I hope:
    ESMC_VM &vm_parent = **ptr_vm_parent;     // reference to parent VM
    ESMC_VMPlan &vmplan = **ptr_vmplan;       // reference to VMPlan

    // Now call the vmachine_exit function which will block respective PETs
    vm_parent.vmachine_exit(vmplan, *vm_info);
    cargotype *cargo = (cargotype *)*vm_cargo;
    *callrc = cargo->rc;
    delete cargo;

    if (status) *status = ESMF_SUCCESS;
  }

}
