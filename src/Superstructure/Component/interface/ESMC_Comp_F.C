// $Id: ESMC_Comp_F.C,v 1.17 2004/03/08 16:03:25 nscollins Exp $
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

#ifdef ESMF_ENABLE_VM
#include "ESMC_VM.h"
#endif

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

// some slight of hand to generate fortran reference documentation
// when the code is actually in C++
//
//BOP
// !IROUTINE: ESMF_GridCompSetServices - Register Component interface routines
//
// !INTERFACE:
//      subroutine ESMF_GridCompSetServices(component, subroutineName, rc)
//
// !ARGUMENTS:
//      type(ESMF_GridComp) :: component
//      subroutine :: subroutineName
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Call a gridded {\tt ESMF\_Component}'s SetServices registration routine.  
//  The parent Component must first create a subcomponent and then
//  call this routine, giving it the component derived type
//  returned from the create, plus the public, well-known, subroutine name
//  that is the registration routine for that component.  This name must be
//  documented by the subcomponent provider.
//
//  After this subroutine returns, the framework now knows how to call
//  the Initialize, Run, and Finalize routines for the subcomponents.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_GridCompCreate} call.
//   
//   \item[subroutineName]
//    The public name of the Component's {\tt SetServices} call.  
//    A component writer must provide this information.
//   
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP
//BOP
//
// !IROUTINE: ESMF_CplCompSetServices - Register Component interface routines
//
// !INTERFACE:
//      subroutine ESMF_CplCompSetServices(component, subroutineName, rc)
//
// !ARGUMENTS:
//      type(ESMF_CplComp), intent(inout) :: component
//      subroutine, intent(in) :: subroutineName
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Call a Coupler {\tt ESMF\_Component}'s SetServices registration routine.  
//  The parent Component must first create a subcomponent and then
//  call this routine, giving it the component derived type
//  returned from the create, plus the public, well-known, subroutine name
//  that is the registration routine for that component.  This name must be
//  documented by the subcomponent provider.
//
//  After this subroutine returns, the framework now knows how to call
//  the Initialize, Run, and Finalize routines for the subcomponents.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_CplCompCreate} call.
//   
//   \item[subroutineName]
//    The public name of the Component's {\tt SetServices} call.  
//    A component writer must provide this information.
//   
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP

//BOP
// !IROUTINE: ESMF_GridCompSetEntryPoint - Set name of component subroutines
//
// !INTERFACE:
//      subroutine ESMF_GridCompSetEntryPoint(component, subroutineType, &
//                                            subroutineName, phase, rc)
//
// !ARGUMENTS:
//      type(ESMF_GridComp), intent(inout) :: component
//      character(len=*), intent(in) :: subroutineType
//      subroutine, intent(in) :: subroutineName
//      integer, intent(in) :: phase
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Intended to be called by a component during the Registration process.
//  A component calls SetEntryPoint for each of the predefined Init,
//  Run, and Finalize routines, to assocate the internal subroutine to be
//  called for each function.  If multiple phases for Init, Run, or Finalize
//  are needed, this can be called with phase numbers.
//
//  After this subroutine returns, the framework now knows how to call
//  the Initialize, Run, and Finalize routines for the subcomponents.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_GridCompCreate} call.
//   
//   \item[subroutineType]
//    One of a set of predefined subroutine types - e.g. {\tt ESMF\_SETINIT}, 
//    {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}.
//
//   \item[subroutineName]
//    The name of the Component's subroutine to be associated with the
//    subroutineType (e.g. Initialize).   This subroutine does not have to be
//    public to the module.
//   
//   \item[{[phase]}] 
//    For Components which need to initialize or run or finalize in mutiple
//    phases, the phase number which corresponds to this subroutine name.
//    For single phase subroutines, either omit this argument, or use the
//    parameter {\tt ESMF\_SINGLEPHASE}.   The Component writer must document
//    the requirements of the Component for how and when the multiple phases
//    are expected to be called.
//
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP
//BOP
// !IROUTINE: ESMF_CplCompSetEntryPoint - Set name of component subroutines
//
// !INTERFACE:
//      subroutine ESMF_CplCompSetEntryPoint(component, subroutineType, &
//                                            subroutineName, phase, rc)
//
// !ARGUMENTS:
//      type(ESMF_CplComp), intent(inout) :: component
//      character(len=*), intent(in) :: subroutineType
//      subroutine, intent(in) :: subroutineName
//      integer, intent(in) :: phase
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Intended to be called by a component during the Registration process.
//  A component calls SetEntryPoint for each of the predefined Init,
//  Run, and Finalize routines, to assocate the internal subroutine to be
//  called for each function.  If multiple phases for Init, Run, or Finalize
//  are needed, this can be called with phase numbers.
//
//  After this subroutine returns, the framework now knows how to call
//  the Initialize, Run, and Finalize routines for the subcomponents.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_CplCompCreate} call.
//   
//   \item[subroutineType]
//    One of a set of predefined subroutine types - e.g. {\tt ESMF\_SETINIT}, 
//    {\tt ESMF\_SETRUN}, {\tt ESMF\_SETFINAL}.
//
//   \item[subroutineName]
//    The name of the Component's subroutine to be associated with the
//    subroutineType (e.g. Initialize).   This subroutine does not have to be
//    public to the module.
//   
//   \item[{[phase]}] 
//    For Components which need to initialize or run or finalize in mutiple
//    phases, the phase number which corresponds to this subroutine name.
//    For single phase subroutines, either omit this argument, or use the
//    parameter {\tt ESMF\_SINGLEPHASE}.   The Component writer must document
//    the requirements of the Component for how and when the multiple phases
//    are expected to be called.
//
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP

//BOP
// !IROUTINE: ESMF_GridCompSetInternalState - Set private data block pointer
//
// !INTERFACE:
//      subroutine ESMF_GridCompSetInternalState(component, dataPointer, rc)
//
// !ARGUMENTS:
//      type(ESMF_GridComp), intent(inout) :: component
//      type(any), pointer, intent(in) :: dataPointer
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Available to be called by a component at any time, but expected to be
//  most useful when called during the Registration process, or Initialization.
//  Since Init, Run, and Finalize must be separate subroutines, data that
//  they need to share in common can either be module global data, or can
//  be allocated in a private data block, and the address of that block
//  can be registered with the framework and retrieved by subsequent calls.
//  When running multiple instantiations of a component, for example during
//  ensemble runs, it may be simpler to maintain private data specific to 
//  each run with private data blocks.  A corresponding {\tt GetInternalState}
//  call retrieves the data pointer.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_GridCompCreate} call.
//   
//   \item[dataPointer]
//    A pointer to the private data block, wrapped in a derived type which
//    contains only a pointer to the block.  This level of indirection is
//    needed to reliably set and retrieve the data block no matter which
//    architecture or compiler is used.  
//
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP
//BOP
// !IROUTINE: ESMF_CplCompSetInternalState - Set private data block pointer
//
// !INTERFACE:
//      subroutine ESMF_CplCompSetInternalState(component, dataPointer, rc)
//
// !ARGUMENTS:
//      type(ESMF_CplComp), intent(inout) :: component
//      type(any), pointer, intent(in) :: dataPointer
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Available to be called by a component at any time, but expected to be
//  most useful when called during the Registration process, or Initialization.
//  Since Init, Run, and Finalize must be separate subroutines, data that
//  they need to share in common can either be module global data, or can
//  be allocated in a private data block, and the address of that block
//  can be registered with the framework and retrieved by subsequent calls.
//  When running multiple instantiations of a component, for example during
//  ensemble runs, it may be simpler to maintain private data specific to 
//  each run with private data blocks.  A corresponding {\tt GetInternalState}
//  call retrieves the data pointer.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_CplCompCreate} call.
//   
//   \item[dataPointer]
//    A pointer to the private data block, wrapped in a derived type which
//    contains only a pointer to the block.  This level of indirection is
//    needed to reliably set and retrieve the data block no matter which
//    architecture or compiler is used.  
//
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP

//BOP
// !IROUTINE: ESMF_GridCompGetInternalState - Get private data block pointer
//
// !INTERFACE:
//      subroutine ESMF_GridCompGetInternalState(component, dataPointer, rc)
//
// !ARGUMENTS:
//      type(ESMF_GridComp), intent(inout) :: component
//      type(any), pointer, intent(in) :: dataPointer
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Available to be called by a component at any time after 
//  {\tt SetInternalState} has been called.
//  Since Init, Run, and Finalize must be separate subroutines, data that
//  they need to share in common can either be module global data, or can
//  be allocated in a private data block, and the address of that block
//  can be registered with the framework and retrieved by this call.
//  When running multiple instantiations of a component, for example during
//  ensemble runs, it may be simpler to maintain private data specific to 
//  each run with private data blocks.  A corresponding {\tt SetInternalState}
//  call sets the data pointer to this block, and this call retrieves the 
//  data pointer.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_GridCompCreate} call.
//   
//   \item[dataPointer]
//    A derived type, containing only a pointer to the private data block.
//    The framework will fill in the block and when this call returns the
//    pointer is set to the same address set during 
//    {\tt ESMF\_GridSetInternalState}.
//    This level of indirection is needed to reliably set and retrieve 
//    the data block no matter which architecture or compiler is used.  
//
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP
//BOP
// !IROUTINE: ESMF_CplCompGetInternalState - Get private data block pointer
//
// !INTERFACE:
//      subroutine ESMF_CplCompGetInternalState(component, dataPointer, rc)
//
// !ARGUMENTS:
//      type(ESMF_CplComp), intent(inout) :: component
//      type(any), pointer, intent(in) :: dataPointer
//      integer, intent(out), optional :: rc
//
// !DESCRIPTION:
//  Available to be called by a component at any time after 
//  {\tt SetInternalState} has been called.
//  Since Init, Run, and Finalize must be separate subroutines, data that
//  they need to share in common can either be module global data, or can
//  be allocated in a private data block, and the address of that block
//  can be registered with the framework and retrieved by this call.
//  When running multiple instantiations of a component, for example during
//  ensemble runs, it may be simpler to maintain private data specific to 
//  each run with private data blocks.  A corresponding {\tt SetInternalState}
//  call sets the data pointer to this block, and this call retrieves the 
//  data pointer.
//    
//  The arguments are:
//  \begin{description}
//   
//   \item[component]
//    Component object returned from a {\tt ESMF\_CplCompCreate} call.
//   
//   \item[dataPointer]
//    A derived type, containing only a pointer to the private data block.
//    The framework will fill in the block and when this call returns the
//    pointer is set to the same address set during 
//    {\tt ESMF\_CplSetInternalState}.
//    This level of indirection is needed to reliably set and retrieve 
//    the data block no matter which architecture or compiler is used.  
//
//   \item[{[rc]}] 
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   
//   \end{description}
//
//EOP

// now, the actual code called directly by the users fortran code.

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


#ifdef ESMF_ENABLE_VM
// these interface subroutine names MUST be in lower case
extern "C" {

  void FTN(c_esmc_compreturn)(
    ESMC_VM **ptr_vm_parent,  // p2 to the parent VM
    ESMC_VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
    void **vm_info,           // p2 to member which holds info
    void **vm_cargo,          // p2 to member which holds cargo
    int *status){             // return error code in status
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
#endif
