// $Id: ESMC_FTable_F.C,v 1.15 2004/04/19 20:23:23 theurich Exp $
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
#include "ESMC_Array.h"

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
         (*ptr) = new ESMC_FTable;
         (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_ftabledestroy)(ESMC_FTable **ptr, int *status) {
         delete (*ptr);
         *ptr = 0;
         *status = ESMF_SUCCESS;
     }
  
     // call a function 
     void FTN(c_esmc_ftablecallentrypoint)(ESMC_FTable **ptr, char *type, 
                                        int *phase, int *status, int slen) {
         int funcrc;
         int localrc;
         char *name;

         newtrim(type, slen, phase, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         // TODO: two return codes here - one is whether we could find
         // the right function to call; the other is the actual return code
         // from the user function itself.

         localrc = (*ptr)->ESMC_FTableCallVFuncPtr(name, &funcrc);

         *status = funcrc;

         delete[] name;
     }

     // get and set routines for both function and data pointers.
     // index them by name.
     void FTN(c_esmc_ftablegetentrypoint)(ESMC_FTable **ptr, char *type, 
                       void **func, enum ftype *ftype, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableGetFuncPtr(name, func, ftype);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetentrypoint)(ESMC_FTable **ptr, char *type,
                                           void *func, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableSetFuncPtr(name, func);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetargs)(ESMC_FTable **ptr, char *type,
                            int *acount, void **alist, int *status, int slen) {
         char *name;

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
                        void *data, enum dtype *dtype, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (**ptr)->ESMC_FTableSetDataPtr(name, data, *dtype);

         delete[] name;
     }

     void FTN(c_esmc_ftablegetinternalstate)(ESMC_FTable ***ptr, char *type,
                       void **data, enum dtype *dtype, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (**ptr)->ESMC_FTableGetDataPtr(name, data, dtype);

         delete[] name;
     }
     
   
// VM-enabled CallBack loop     
     
static void *ESMC_FTableCallEntryPointVMHop(void *vm, void *cargo){
  // This routine is the first level that gets instantiated in new VM
  // The first argument must be of type (void *) and points to a derived
  // vmachine class object.
  
  // pull out info from cargo
  char *name = ((cargotype *)cargo)->name;
  ESMC_FTable &ftable = *((cargotype *)cargo)->ftable;  // reference to ftable
  
  // Need to call a special call function which adds the VM to the interface
  int funcrc, localrc;
  localrc = ftable.ESMC_FTableCallVFuncPtr(name, (ESMC_VM*)vm, &funcrc);
  
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
  ESMC_VM &vm_parent = **ptr_vm_parent;     // reference to parent VM
  ESMC_VMPlan &vmplan = **ptr_vmplan;       // reference to VMPlan
  ESMC_FTable &ftable = **ptr;              // reference to function table
         
  cargotype *cargo = new cargotype;
  strcpy(cargo->name, name);   // copy trimmed type string
  cargo->ftable = &ftable;     // reference to function table
  *vm_cargo=(void*)cargo;      // store pointer to the cargo structure
         
  *vm_info = vm_parent.vmachine_enter(vmplan, ESMC_FTableCallEntryPointVMHop,
    (void*)cargo);
  
  delete[] name;
  *status = ESMF_SUCCESS;
}

};


