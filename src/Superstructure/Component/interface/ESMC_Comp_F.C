// $Id: ESMC_Comp_F.C,v 1.32.2.1 2005/02/09 22:23:51 theurich Exp $
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


extern "C" {
static void *ESMC_FTableCallEntryPointVMHop(void *vm, void *cargo);
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SetServ"
extern "C" void ESMC_SetServ(void *ptr, int (*func)(), int *status) {
     int localrc, funcrc;
     int *tablerc = new int;
     ESMC_Comp *f90comp = (ESMC_Comp *)ptr;
     ESMC_FTable *tabptr;
     
     if (status) *status = ESMF_SUCCESS;  // assume success 'till problems found

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

     // time to startup the VM for this component...     
     ESMC_VM *vm_parent;
     FTN(f_esmf_compgetvmparent)(f90comp, &vm_parent, &localrc); //get vm_parent
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
       status)) return;
     ESMC_VMPlan *vmplan_p;
     FTN(f_esmf_compgetvmplan)(f90comp, &vmplan_p, &localrc);    //get vmplan_p
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
       status)) return;
     // parent VM and plan for child can now be used to startup the child VM
     void *vm_info = vm_parent->ESMC_VMStartup(vmplan_p,
       ESMC_FTableCallEntryPointVMHop, NULL, &localrc);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
       status)) return;
     // keep vm_info in a safe place (in parent component) 'till it's used again
     FTN(f_esmf_compsetvminfo)(f90comp, &vm_info, &localrc);
     if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
       status)) return;
     // ...now the component's VM is started up and placed on hold.
     
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


// VM-enabled CallBack loop     
extern "C" {

     
static void *ESMC_FTableCallEntryPointVMHop(void *vm, void *cargo){
  // This routine is the first level that gets instantiated in new VM
  // The first argument must be of type (void *) and points to a derived
  // ESMC_VMK class object.
  
  // pull out info from cargo
  char *name = ((cargotype *)cargo)->name;
  ESMC_FTable *ftable = ((cargotype *)cargo)->ftable;  // pointer to ftable
  
  // Need to call a special call function which adds the VM to the interface
  int funcrc, localrc;
  
//  fprintf(stderr, "gjt I am in ESMC_FTableCallEntryPointVMHop\n");
  
  localrc = ftable->ESMC_FTableCallVFuncPtr(name, (ESMC_VM*)vm, &funcrc);
  
  // TODO: Here I need to communicate between all child PET's to find out if
  // any failed in the call to user supplied component method
  
  // put the return code into cargo   
  ((cargotype *)cargo)->rc = funcrc;    // TODO cargo is shared between threads
  
  return NULL;
}

// call a function through VM
void FTN(c_esmc_ftablecallentrypointvm)(
  ESMC_VM **ptr_vm_parent,  // p2 to the parent VM
  ESMC_VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
  void **vm_info,           // p2 to member which holds info returned by enter
  void **vm_cargo,          // p2 to member which holds cargo
  ESMC_FTable **ptr,        // p2 to the ftable of this component
  char *type,               // string holding type of called entry point
  int *phase,               // phase selector
  int *status,              // return error code in status
  int slen) {               // additional F90 argument associated with type
       
  // local variables
  int funcrc;               // function return value
  int localrc;              // local return value
  char *name;               // trimmed type string

  newtrim(type, slen, phase, NULL, &name);

  // TODO: two return codes here - one is whether we could find
  // the right function to call; the other is the actual return code
  // from the user function itself.

  // Things get a little confusing here with pointers, so I will define
  // some temp. variables that make matters a little clearer I hope:
  ESMC_VM *vm_parent = *ptr_vm_parent;      // pointer to parent VM
  ESMC_VMPlan *vmplan = *ptr_vmplan;        // pointer to VMPlan
  ESMC_FTable *ftable = *ptr;               // pointer to function table
         
  cargotype *cargo = new cargotype;
  strcpy(cargo->name, name);   // copy trimmed type string
  cargo->ftable = ftable;      // pointer to function table
  cargo->rc = ESMF_SUCCESS;    // initialize return code to SUCCESS for all PETs
  *vm_cargo=(void*)cargo;      // store pointer to the cargo structure
         
//fprintf(stderr, "gjt cargo after new: %p\n", cargo);
  
  vm_parent->vmk_enter(static_cast<ESMC_VMKPlan *>(vmplan), *vm_info,
    (void*)cargo);
  
  delete[] name;
  *status = ESMF_SUCCESS;
}

  void FTN(c_esmc_compwait)(
    ESMC_VM **ptr_vm_parent,  // p2 to the parent VM
    ESMC_VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
    void **vm_info,           // p2 to member which holds info
    void **vm_cargo,          // p2 to member which holds cargo
    int *callrc,              // return code of the user component method
    int *status) {            // return error code in status

    // Things get a little confusing here with pointers, so I will define
    // some temp. variables that make matters a little clearer I hope:
    ESMC_VM *vm_parent = *ptr_vm_parent;      // pointer to parent VM
    ESMC_VMPlan *vmplan = *ptr_vmplan;        // pointer to VMPlan

    // Now call the vmk_exit function which will block respective PETs
//fprintf(stderr, "gjt debug: c_esmc_compwait, vm_parent=%p\n", vm_parent);
//fprintf(stderr, "gjt debug: c_esmc_compwait, vm_info=%p\n", *vm_info);
//fprintf(stderr, "gjt debug: c_esmc_compwait, vm_plan=%p\n", vmplan);
    
//    vm_parent->vmk_print();
//    vmplan->vmkplan_print();
    
    
//    fprintf(stderr, "gjt debug: i am in c_esmc_compwait\n");

    vm_parent->vmk_exit(static_cast<ESMC_VMKPlan *>(vmplan), *vm_info);
    cargotype *cargo = (cargotype *)*vm_cargo;
    if (cargo == NULL) {
//fprintf(stderr, "nsc cargo before delete was null\n");
        if (status) *status = ESMF_FAILURE;
        return;
    }
    *callrc = cargo->rc;
//fprintf(stderr, "gjt cargo before delete: %p\n", cargo);
    delete cargo;

    if (status) *status = ESMF_SUCCESS;
  }

}



