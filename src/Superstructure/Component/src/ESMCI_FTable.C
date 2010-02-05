// $Id: ESMCI_FTable.C,v 1.33.2.1 2010/02/05 20:03:59 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_FTable.C"
//==============================================================================
//
// ESMCI Function table implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Function Table} methods 
// declared in the companion file {\tt ESMCI\_FTable.h}.  
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_FTable.h"

// insert higher level, 3rd party or system includes
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#ifndef ESMF_NO_DLFCN
#include <dlfcn.h>
#endif

// LogErr
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"

// include ESMF headers
#include "ESMCI_Comp.h"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_FTable.C,v 1.33.2.1 2010/02/05 20:03:59 svasquez Exp $";
//-----------------------------------------------------------------------------


//==============================================================================
// prototypes for Fortran interface routines called by C++ code below
extern "C" {
  void FTN(f_esmf_compsetvminfo)(ESMCI::Comp *compp, void *vm_info, int *rc);
  void FTN(f_esmf_compgetvminfo)(ESMCI::Comp *compp, void *vm_info, int *rc);
  void FTN(f_esmf_compgetvmparent)(ESMCI::Comp *compp, ESMCI::VM **vmparent, 
    int *rc);
  void FTN(f_esmf_compgetvmplan)(ESMCI::Comp *compp, ESMCI::VMPlan **vmplan, 
    int *rc);
  void FTN(f_esmf_compinsertvm)(ESMCI::Comp *compp, void *vm, int *rc);
  void FTN(f_esmf_compgetctype)(ESMCI::Comp *compp, ESMCI::CompType *ctype,
    int *rc);
  void FTN(f_esmf_compreplicate)(ESMCI::Comp *compp, ESMCI::Comp *compp_src,
    void *vm, int *rc);
  void FTN(f_esmf_comprefcopy)(ESMCI::Comp *compp, ESMCI::Comp *compp_src,
    int *rc);
  void FTN(f_esmf_compdelete)(ESMCI::Comp *compp, int *rc);
  
  void FTN(f_esmf_fortranudtpointersize)(int *size);
  void FTN(f_esmf_fortranudtpointercopy)(void *dst, void *src);
}
//==============================================================================



//==============================================================================
// FTable interfaces to be called from Fortran side (ESMF_Comp.F90)
//
// these interface subroutine names MUST be in lower case
//
extern "C" {

  // call to native class constructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablecreate"
  void FTN(c_esmc_ftablecreate)(ESMCI::FTable **ptr, int *rc) {
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    (*ptr) = new ESMCI::FTable;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- Ftable allocation", rc);  
      return;
    }
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // call to native class destructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftabledestroy"
  void FTN(c_esmc_ftabledestroy)(ESMCI::FTable **ptr, int *rc) {
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- Ftable deallocation", rc);  
      return;
    }
    delete (*ptr);
    *ptr = NULL;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
  // set arguments for standard Component methods
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablesetstateargs"
  void FTN(c_esmc_ftablesetstateargs)(ESMCI::FTable **ptr, 
    enum ESMCI::method *method, int *phase, void *comp, void *importState,
    void *exportState, void *clock, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    char const *methodString;
    switch(*method){
    case ESMCI::SETINIT:
      methodString = "Initialize";
      break;
    case ESMCI::SETRUN:
      methodString = "Run";
      break;
    case ESMCI::SETFINAL:
      methodString = "Finalize";
      break;
    case ESMCI::SETWRITERESTART:
      methodString = "WriteRestart";
      break;
    case ESMCI::SETREADRESTART:
      methodString = "ReadRestart";
      break;
    case ESMCI::SETREGISTER:
      methodString = "Register";
      break;
    default:
      break;
    }
    
    int slen = strlen(methodString);
    char *fname;
    ESMCI::FTable::newtrim(methodString, slen, phase, NULL, &fname);
    //printf("after newtrim, name = '%s'\n", fname);

    void *alist[4];
    alist[0] = (void *)comp;
    alist[1] = (void *)importState;
    alist[2] = (void *)exportState;
    alist[3] = (void *)clock;

    ESMCI::FTable *ftable = *ptr; // incoming FTable

    // only if the incoming FTable contains PET-local component copies
    for (int i=0; i<ftable->componentcount; i++){
      ESMCI::Comp *comp = ftable->component + i;      // component copy
      ESMCI::FTable *ft = **(ESMCI::FTable ***)comp;  // assoc. FTable
      localrc = ft->setFuncArgs(fname, 4, alist);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
        return;
    }

    delete[] fname;  // delete memory that "newtrim" allocated above

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // set the InternalState in FTable
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablesetinternalstate"
  void FTN(c_esmc_ftablesetinternalstate)(ESMCI::FTable ***ptr,
    char const *type, void **data, enum ESMCI::dtype *dtype, int *rc, int slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    char *name;
    ESMCI::FTable::newtrim(type, slen, NULL, NULL, &name);
    //printf("after newtrim, name = '%s'\n", name);

    localrc = (**ptr)->setDataPtr(name, data, *dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;

    delete[] name;  // delete memory that "newtrim" allocated above

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // get the InternalState from FTable
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablegetinternalstate"
  void FTN(c_esmc_ftablegetinternalstate)(ESMCI::FTable ***ptr,
    char const *type, void **data, enum ESMCI::dtype *dtype, int *rc, int slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    char *name;
    ESMCI::FTable::newtrim(type, slen, NULL, NULL, &name);
    //printf("after newtrim, name = '%s'\n", name);

    localrc = (**ptr)->getDataPtr(name, data, dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;

    delete[] name;  // delete memory that "newtrim" allocated above

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setvm"
  void FTN(c_esmc_setvm)(void *ptr, void (*func)(), int *userRc, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setVM(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setvmshobj"
  void FTN(c_esmc_setvmshobj)(void *ptr, char const *routineArg, 
    char const *sharedObjArg, int *userRc, int *rc, 
    ESMCI_FortranStrLenArg rlen, ESMCI_FortranStrLenArg llen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, 
      "- System does not support dynamic loading.", rc);
    return;
#else
    void *lib;
    if (llen>0){
      string sharedObj(sharedObjArg, llen);
      sharedObj.resize(sharedObj.find_last_not_of(" ")+1);
      lib = dlopen(sharedObj.c_str(), RTLD_LAZY);
    }else
      lib = dlopen(NULL, RTLD_LAZY);  // search in executable
    if (lib == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "shared object not found", rc);
      return;
    }
    string routine(routineArg, rlen);
    routine.resize(routine.find_last_not_of(" ")+1);
    void (*func)() = (void (*)())dlsym(lib, routine.c_str());
    if ((void *)func == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "routine not found", rc);
      return;
    }
    ESMCI::FTable::setVM(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
#endif
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservices"
  void FTN(c_esmc_setservices)(void *ptr, void (*func)(), int *userRc, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setServices(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservicesshobj"
  void FTN(c_esmc_setservicesshobj)(void *ptr, char const *routineArg, 
    char const *sharedObjArg, int *userRc, int *rc, 
    ESMCI_FortranStrLenArg rlen, ESMCI_FortranStrLenArg llen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, 
      "- System does not support dynamic loading.", rc);
    return;
#else
    void *lib;
    if (llen>0){
      string sharedObj(sharedObjArg, llen);
      sharedObj.resize(sharedObj.find_last_not_of(" ")+1);
      lib = dlopen(sharedObj.c_str(), RTLD_LAZY);
    }else
      lib = dlopen(NULL, RTLD_LAZY);  // search in executable
    if (lib == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "shared object not found", rc);
      return;
    }
    string routine(routineArg, rlen);
    routine.resize(routine.find_last_not_of(" ")+1);
    void (*func)() = (void (*)())dlsym(lib, routine.c_str());
    if ((void *)func == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "routine not found", rc);
      return;
    }
    ESMCI::FTable::setServices(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
#endif
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setentrypoint"
  void FTN(c_esmc_setentrypoint)(void *ptr, enum ESMCI::method *method,
    void *func, int *phase, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    
    char const *methodString;
    switch(*method){
    case ESMCI::SETINIT:
      methodString = "Initialize";
      break;
    case ESMCI::SETRUN:
      methodString = "Run";
      break;
    case ESMCI::SETFINAL:
      methodString = "Finalize";
      break;
    case ESMCI::SETWRITERESTART:
      methodString = "WriteRestart";
      break;
    case ESMCI::SETREADRESTART:
      methodString = "ReadRestart";
      break;
    case ESMCI::SETREGISTER:
      methodString = "Register";
      break;
    default:
      break;
    }
    
    int slen = strlen(methodString);
    char *fname;
    ESMCI::FTable::newtrim(methodString, slen, phase, NULL, &fname);
         
    ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
    localrc = (tabptr)->setFuncPtr(fname, func, ESMCI::FT_VOIDP4INTP);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    
    delete[] fname;  // delete memory that "newtrim" allocated above
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
} // extern "C"
//==============================================================================


//==============================================================================
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
//
// these interface subroutine names MUST be in lower case
extern "C" {

  // ---------- GridComp ---------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_gridcompsetinternalstate"
  void FTN(esmf_gridcompsetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_gridcompgetinternalstate"
  void FTN(esmf_gridcompgetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::getDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // ---------- CplComp ---------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_cplcompsetinternalstate"
  void FTN(esmf_cplcompsetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_cplcompgetinternalstate"
  void FTN(esmf_cplcompgetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::getDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // ---------- UserComp ---------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompsetvm"
  void FTN(esmf_usercompsetvm)(void *ptr, void (*func)(), int *userRc,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setVM(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompsetservices"
  void FTN(esmf_usercompsetservices)(void *ptr, void (*func)(), int *userRc,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setServices(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompsetinternalstate"
  void FTN(esmf_usercompsetinternalstate)(ESMCI::FTable ***ptr,
    char const *name, void **datap, int *rc, int slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "null pointer found", rc);
      return;
    }

    char *tbuf; 
    ESMCI::FTable::newtrim(name, slen, NULL, NULL, &tbuf);
    //printf("after newtrim, name = '%s'\n", tbuf);

    enum ESMCI::dtype dtype = ESMCI::DT_FORTRAN_UDT_POINTER;
    localrc = (**ptr)->setDataPtr(tbuf, datap, dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
  
    delete[] tbuf;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompgetinternalstate"
  void FTN(esmf_usercompgetinternalstate)(ESMCI::FTable ***ptr,
    char const *name, void **datap, int *rc, int slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "null pointer found", rc);
      return;
    }

    char *tbuf; 
    ESMCI::FTable::newtrim(name, slen, NULL, NULL, &tbuf);
    //printf("after newtrim, name = '%s'\n", tbuf);

    enum ESMCI::dtype dtype;
    localrc = (**ptr)->getDataPtr(tbuf, datap, &dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
  
    delete[] tbuf;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

} // extern "C"
//==============================================================================


//==============================================================================
// VM-enabled CallBack loop     
extern "C" {
     
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_FTableCallEntryPointVMHop"
static void *ESMCI_FTableCallEntryPointVMHop(void *vm, void *cargoCast){
  // This routine is the first level that gets instantiated in new VM
  // The first argument must be of type (void *) and points to a derived
  // ESMCI::VMK class object. The second argument is also of type (void *)
  // and points to a cargotype structure.
  
  // pull out info from cargo
  ESMCI::cargotype *cargo = (ESMCI::cargotype *)cargoCast;
  char *name = cargo->name;               // name of callback
  ESMCI::FTable *ftable = cargo->ftable;  // ptr to ftable
  
  int esmfrc;           // ESMF return code of ESMCI::FTable::callVFuncPtr()
  int userrc = -99999;  // user return code from the registered component method

  // prepare return code members in cargo
  int mypet = ((ESMCI::VM*)vm)->getMypet();
  int mynthreads = ((ESMCI::VM*)vm)->getNthreads(mypet);
  int mytid = ((ESMCI::VM*)vm)->getTid(mypet);
  if (mytid==0){
    // master thread -> allocate return code members in cargo for all threads
    if (cargo->rcCount != mynthreads){
      delete [] cargo->esmfrc;
      delete [] cargo->userrc;
      cargo->esmfrc = new int[mynthreads];
      cargo->userrc = new int[mynthreads];
      cargo->rcCount = mynthreads;
    }
  }
  ((ESMCI::VM*)vm)->threadbarrier();  // synchronize all threads in local group
  
  // call into user code through ESMF function table...
  int localrc = ftable->callVFuncPtr(name, (ESMCI::VM*)vm, &userrc);
  // ...back from user code

  // put the return codes into cargo 
  cargo->userrc[mytid] = userrc;
  
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &esmfrc)){
    cargo->esmfrc[mytid] = esmfrc;
    return NULL;
  }
  
  // return successfully
  cargo->esmfrc[mytid] = ESMF_SUCCESS;
  return NULL;
}

// call a function through VM
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablecallentrypointvm"
void FTN(c_esmc_ftablecallentrypointvm)(
  ESMCI::VM **ptr_vm_parent,  // p2 to the parent VM
  ESMCI::VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
  void **vm_info,             // p2 to member which holds info returned by enter
  void **vm_cargo,            // p2 to member which holds cargo
  ESMCI::FTable **ptr,        // p2 to the ftable of this component
  enum ESMCI::method *method, // method type
  int *phase,                 // phase selector
  int *rc                     // return code
  ){
       
  // local variables
  int localrc;              // local return code
  char *name;               // trimmed type string

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  char const *methodString;
  switch(*method){
  case ESMCI::SETINIT:
    methodString = "Initialize";
    break;
  case ESMCI::SETRUN:
    methodString = "Run";
    break;
  case ESMCI::SETFINAL:
    methodString = "Finalize";
    break;
  case ESMCI::SETWRITERESTART:
    methodString = "WriteRestart";
    break;
  case ESMCI::SETREADRESTART:
    methodString = "ReadRestart";
    break;
  case ESMCI::SETREGISTER:
    methodString = "Register";
    break;
  default:
    break;
  }

  int slen = strlen(methodString);
  ESMCI::FTable::newtrim(methodString, slen, phase, NULL, &name);

  // Things get a little confusing here with pointers, so I will define
  // some temp. variables that make matters a little clearer I hope:
  ESMCI::VM *vm_parent = *ptr_vm_parent;      // pointer to parent VM
  ESMCI::VMPlan *vmplan = *ptr_vmplan;        // pointer to VMPlan
  ESMCI::FTable *ftable = *ptr;               // pointer to function table
         
  ESMCI::cargotype *cargo = new ESMCI::cargotype;
  strcpy(cargo->name, name);    // copy trimmed type string
  cargo->ftable = ftable;       // pointer to function table
  cargo->rcCount = 1;           // default
  cargo->esmfrc = new int[1];
  cargo->userrc = new int[1];
  cargo->esmfrc[0] = ESMF_SUCCESS; // initialize return code
  cargo->userrc[0] = 0;            // initialize user return code
  *vm_cargo=(void*)cargo;       // store pointer to the cargo structure
  delete[] name;  // delete memory that "newtrim" allocated above

  // enter the child VM -> resurface in ESMCI_FTableCallEntryPointVMHop()
  localrc = vm_parent->enter(vmplan, *vm_info, (void*)cargo);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
    return;
        
  // ... if the child VM uses threads (multi-threading or single-threading) 
  // then this parent PET continues running concurrently to the child PET in the
  // same VAS! In that case the return codes in cargo are not valid here!
  // The status returned by VMEnter() indicates that success of entering the
  // child VM, not failure or success of the callback.
  // The return code of the callback code will be valid in all cases (threading
  // or no threading) _after_ VMK::exit() returns.
  
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_compwait"
void FTN(c_esmc_compwait)(
  ESMCI::VM **ptr_vm_parent,  // p2 to the parent VM
  ESMCI::VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
  void **vm_info,             // p2 to member which holds info
  void **vm_cargo,            // p2 to member which holds cargo
  int *userrc,                // return code of the user component method
  int *rc){                   // esmf internal return error code

  // initialize the return codes
  int localrc = ESMC_RC_NOT_IMPL;
  if (rc) *rc = ESMC_RC_NOT_SET; // return code of ESMF callback code

  // Things get a little confusing here with pointers, so I will define
  // some temp. variables that make matters a little clearer I hope:
  ESMCI::VM *vm_parent = *ptr_vm_parent;        // pointer to parent VM
  ESMCI::VMPlan *vmplan = *ptr_vmplan;          // pointer to VMPlan
  ESMCI::cargotype *cargo = (ESMCI::cargotype *)*vm_cargo;  // pointer to cargo
  
  // Now call the vmk_exit function which will block respective PETs
  vm_parent->exit(static_cast<ESMCI::VMKPlan *>(vmplan), *vm_info);
  
  // return with errors if there is no cargo to obtain error codes
  if (cargo == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - No cargo structure to obtain error codes", rc);
    return;
  }
  
  // obtain return codes out of cargo
  //TODO: deal with multiple return codes coming back for multi-threaded VMs
  localrc = cargo->esmfrc[0];
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
    return;
  if (userrc) *userrc = cargo->userrc[0];
  
  // delete cargo structure
  delete [] cargo->esmfrc;
  delete [] cargo->userrc;
  delete cargo;
  
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
  
} // extern "C"
//==============================================================================



//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section implements the FTable class
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::getDP()"
void FTable::getDP(FTable ***ptr, void **datap, int *rc){
    char const *name = "localdata";
    enum dtype dtype;
    int localrc;

     // Initialize return code; assume routine not implemented
     if (rc) *rc = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

     //printf("ptr = 0x%08x\n", (ESMC_POINTER)ptr);
     //printf("*ptr = 0x%08x\n", (ESMC_POINTER)(*(int*)ptr));
    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", rc);
       return;
    }

    localrc = (**ptr)->getDataPtr(name, datap, &dtype);
    if (rc) *rc = localrc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setDP()"
void FTable::setDP(FTable ***ptr, void **datap, int *rc){
    char const *name = "localdata";
    enum dtype dtype = DT_FORTRAN_UDT_POINTER;
    int localrc;

     // Initialize return code; assume routine not implemented
     if (rc) *rc = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

     //printf("ptr = 0x%08x\n", (ESMC_POINTER)ptr);
     //printf("*ptr = 0x%08x\n", (ESMC_POINTER)(*(int*)ptr));
    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", rc);
        return;
    }

    localrc = (**ptr)->setDataPtr(name, datap, dtype);
    if (rc) *rc = localrc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::getDataPtr()"
//BOPI
// !IROUTINE:  getDataPtr - get data pointer from name
//
// !INTERFACE:
int FTable::getDataPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char const *namep,     // in, data name
      void **datap,          // out, data address
      enum dtype *dtype) {   // out, data type
//
// !DESCRIPTION:
//    Returns the named data pointer
//
//EOPI
//-----------------------------------------------------------------------------
    int i;

    for (i=datacount-1; i>=0; i--) {    // go backwards for: "last in first out"
      if (strcmp(namep, data[i].dataname)) continue;

      *dtype = data[i].dtype;
      
      if (*dtype == DT_VOIDP){
        *datap = data[i].dataptr;
      }else if (*dtype == DT_FORTRAN_UDT_POINTER){
        FTN(f_esmf_fortranudtpointercopy)((void *)datap, data[i].dataptr);
      }

      return ESMF_SUCCESS;
    }

    return ESMF_FAILURE;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setDataPtr()"
//BOPI
// !IROUTINE:  setDataPtr - set data pointer
//
// !INTERFACE:
int FTable::setDataPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char const *namep,     // in, data name
      void **datap,          // in, data address
      enum dtype dtype) {    // in, data type
//
// !DESCRIPTION:
//    Sets the named data pointer
//
//EOPI
//-----------------------------------------------------------------------------
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    if (datacount >= dataalloc){
      data =
        (datainfo *)realloc((void *)data, (datacount+4) * sizeof(datainfo));
      dataalloc = datacount+4;
    }
    data[datacount].dataname = new char[strlen(namep)+1];
    strcpy(data[datacount].dataname, namep);
    data[datacount].dtype = dtype;
    
    if (dtype == DT_VOIDP){
      data[datacount].dataptr = *datap;
    }else if (dtype == DT_FORTRAN_UDT_POINTER){
      int datumSize;  // upper limit of (UDT, pointer) size
      FTN(f_esmf_fortranudtpointersize)(&datumSize);
      data[datacount].dataptr = (void *)new char[datumSize];
      FTN(f_esmf_fortranudtpointercopy)(data[datacount].dataptr, (void *)datap);
    }
   
    datacount++;

    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Ftable::setServices"
void FTable::setServices(void *ptr, void (*func)(), int *userRc, int *rc) {
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // Check input
  if ((ptr == ESMC_NULL_POINTER) || ((*(void**)ptr) == ESMC_NULL_POINTER)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", rc);
    return;
  }
  
  // TODO: shouldn't need to expand the table here - should be done inside
  // FTable code on demand.
  ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
  localrc = (tabptr)->extend(8, 2); // room for 8 funcs, 2 data
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
    return;

  // Set callback function and arguments
  ESMCI::Comp *f90comp = (ESMCI::Comp *)ptr;
  localrc = (tabptr)->setFuncPtr("Register", (void *)func, f90comp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
    return;

  // time to startup the VM for this component (if not already started)...
  ESMCI::VM *vm_parent;
  FTN(f_esmf_compgetvmparent)(f90comp, &vm_parent, &localrc); //get vm_parent
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return;
  ESMCI::VMPlan *vmplan_p;
  FTN(f_esmf_compgetvmplan)(f90comp, &vmplan_p, &localrc);    //get vmplan_p
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return;
  void *vm_info;
  FTN(f_esmf_compgetvminfo)(f90comp, &vm_info, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return;
  if (vm_info==NULL){
    // VM for this component has not been started yet
    vm_info = vm_parent->startup(vmplan_p,
      ESMCI_FTableCallEntryPointVMHop, NULL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return;
    // keep vm_info in a safe place (in parent component) 'till it's used again
    FTN(f_esmf_compsetvminfo)(f90comp, &vm_info, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return;
  }
  // ...now the component's VM is started up and placed on hold.
  
  // call into register routine using the component's VM
  void *vm_cargo;
  enum method reg = SETREGISTER;
  FTN(c_esmc_ftablecallentrypointvm)(&vm_parent, &vmplan_p, &vm_info,
    &vm_cargo, &tabptr, &reg, NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return;
  
  // wait for the register routine to return
  FTN(c_esmc_compwait)(&vm_parent, &vmplan_p, &vm_info, &vm_cargo, userRc,
    &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) return;
  
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Ftable::setVM"
void FTable::setVM(void *ptr, void (*func)(), int *userRc, int *rc) {
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // Check input
  if ((ptr == ESMC_NULL_POINTER) || ((*(void**)ptr) == ESMC_NULL_POINTER)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", rc);
    return;
  }

  // TODO: shouldn't need to expand the table here - should be done inside
  // FTable code on demand.
  ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
  localrc = (tabptr)->extend(8, 2); // room for 8 funcs, 2 data
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
    return;

  // Set callback function and arguments
  ESMCI::Comp *f90comp = (ESMCI::Comp *)ptr;
  localrc = (tabptr)->setFuncPtr("setVM", (void *)func, f90comp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
    return;

  // Call into user code callback function
  localrc = (tabptr)->callVFuncPtr("setVM", NULL, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
    return;

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::getEntry()"
//BOPI
// !IROUTINE:  getEntry - get FTable entry from name
//
// !INTERFACE:
int FTable::getEntry(
//
// !RETURN VALUE:
//    entry index, or -1 if not found.
//
// !ARGUMENTS:
  char const *name,      // in, function name
  int *rc) {             // out, return code
//
// !DESCRIPTION:
//    Returns the named entry
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize rc and localrc ; assume functions not implemented
  if (*rc) *rc = ESMC_RC_NOT_IMPL;

  // Check input
  if (name == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", rc);
    return -1;  // indicate entry not found
  }

  int i;
  for (i=0; i<funccount; i++) {
    if (!strcmp(name, funcs[i].funcname))
      break;
  }
  if (i==funccount) i=-1; // indicate entry not found
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return i;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncPtr()"
//BOPI
// !IROUTINE:  setFuncPtr - set function pointer, no extra args
//
// !INTERFACE:
int FTable::setFuncPtr(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    char const *name,      // in, function name
    void *func) {          // in, function address
//
// !DESCRIPTION:
//    Sets the named function pointer
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // Check input
  if (name == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }
  if (func == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }

  // look for the name already existing in the table.  if found
  // replace it.  otherwise add it to the end.
  int i;
  for (i=0; i<funccount; i++)
    if (!strcmp(name, funcs[i].funcname)) break;

  // we found the function, or we got to the end of the table.
  // either way we are ready to add it.

  int thisfunc = i;

  // extend the table if needed
  if (thisfunc >= funcalloc){
    funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
    funcalloc = thisfunc+4;
  }
  funcs[thisfunc].funcptr = func;
  funcs[thisfunc].ftype = FT_NULL;
  // do these only if not replacing an existing entry.
  if (thisfunc == funccount){
    funcs[thisfunc].funcname = new char[strlen(name)+1];
    strcpy(funcs[thisfunc].funcname, name);
    funccount++;
  }
   
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncPtr()"
//BOPI
// !IROUTINE:  setFuncPtr - set function pointer, type; no args yet.
//
// !INTERFACE:
int FTable::setFuncPtr(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    char const *name,      // in, function name
    void *func,            // in, function address
    enum ftype ftype) {    // in, function type
//
// !DESCRIPTION:
//    Sets the named function pointer and type, but specifies no argument 
//    values.  Before this can be called successfully, the user must call
//    FTable::setFuncArgs to fill in the argument list.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // Check input
  if (name == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }
  if (func == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }

  // look for the name already existing in the table.  if found
  // replace it.  otherwise add it to the end.
  int i;
  for (i=0; i<funccount; i++)
    if (!strcmp(name, funcs[i].funcname)) break;

  // we found the function, or we got to the end of the table.
  // either way we are ready to add it.

  int thisfunc = i;

  // extend the table if needed
  if (thisfunc >= funcalloc){
    funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
    funcalloc = thisfunc+4;
  }
  funcs[thisfunc].funcptr = func;
  funcs[thisfunc].ftype = ftype;
  // do these only if not replacing an existing entry.
  if (thisfunc == funccount) {
    funcs[thisfunc].funcname = new char[strlen(name)+1];
    strcpy(funcs[thisfunc].funcname, name);
    funccount++;
  }
   
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncPtr()"
//BOPI
// !IROUTINE:  setFuncPtr - set voidp, intp specifically
//
// !INTERFACE:
int FTable::setFuncPtr(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    char const *name,     // in, function name
    void *func,           // in, function address
    void *arg){           // in, void *
//
// !DESCRIPTION:
//    Sets the named function pointer and arg.  This is a common case
//    so it has it's own interface.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // Check input
  if (name == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }
  if (func == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }
  if (arg == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }

  // look for the name already existing in the table.  if found
  // replace it.  otherwise add it to the end.
  int i;
  for (i=0; i<funccount; i++)
    if (!strcmp(name, funcs[i].funcname)) break;

  // we found the function, or we got to the end of the table.
  // either way we are ready to add it.

  int thisfunc = i;

  // extend the table if needed
  if (thisfunc >= funcalloc) {
    funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
    funcalloc = thisfunc+4;
  }
  funcs[thisfunc].funcptr = func;
  funcs[thisfunc].ftype = FT_VOIDP1INTP;
  funcs[thisfunc].funcarg[0] = arg;
  // do these only if not replacing an existing entry.
  if (thisfunc == funccount) {
    funcs[thisfunc].funcname = new char[strlen(name)+1];
    strcpy(funcs[thisfunc].funcname, name);
    funccount++;
  }
   
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncArgs()"
//BOPI
// !IROUTINE:  setFuncArgs - set arglist for existing function
//
// !INTERFACE:
int FTable::setFuncArgs(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    char const *name,      // in, function name
    int acount,            // in, count of args
    void **arglist) {      // in, address of arg list
//
// !DESCRIPTION:
//    Sets the named function args.  The function must already exist.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;
    
  // find the "name" entry
  int i = getEntry(name, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc; // bail out
  if (i == -1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
    "unknown function name", &rc);
    return rc; // bail out
  }

  // Check arglist argument
  if (arglist == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", &rc);
    return rc;
  }

  // fill in arguments
  for(int j=0; j<acount; j++)
    funcs[i].funcarg[j] = arglist[j];

  // return successfully
  return rc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::callVFuncPtr()"
//BOPI
// !IROUTINE:  callVFuncPtr - call a function w/ proper args
//
// !INTERFACE:
int FTable::callVFuncPtr(
//
// !RETURN VALUE:
//    integer return code
//
// !ARGUMENTS:
  char const *name,     // in, function name
  VM *vm_pointer,       // in, optional, pointer to this PET's VM instance
  int *userrc) {        // out, optional, function return
//
// !DESCRIPTION:
//    Calls the named function pointer
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // try to find "name" entry in single FTable instance on parent PET
  int i = getEntry(name, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc; // bail out
  
  Comp *comp;       // pointer to PET-local component
  funcinfo *func;   // pointer to PET-local function entry
  
  // optionally insert vm and replicate Component object, according to situation
  if (vm_pointer){
    // vm_pointer was provided -> use to set VM in Component object
    VM *vmm = vm_pointer;
    VM **vm = &vmm;
    int mypet = vm_pointer->getMypet();
    int mynthreads = vm_pointer->getNthreads(mypet);
    int mytid = vm_pointer->getTid(mypet);
    if (componentcount==0){
      // first time Component is entering its VM -> replicate Comp
      if (i == -1){
        // single FTable instance on parent PET must have contained name
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "unknown function name", &rc);
        return rc; // bail out
      }
      func = funcs + i; // determine PET-local function entry
      vm_pointer->threadbarrier();  // wait for all the threads to have entered
      if (mytid==0){
        // master thread allocates component list in FTable
        component = new Comp[mynthreads];
        componentcount = mynthreads;
        vm_pointer->threadbarrier();  // synchronize with slave threads
        comp = component;   // determine PET-local component
        // make a copy of the component reference
        FTN(f_esmf_comprefcopy)(comp, (Comp *)(func->funcarg[0]), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
          return rc; // bail out
        // insert child VM
        FTN(f_esmf_compinsertvm)(comp, vm, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
          return rc; // bail out
      }else{
        // slave thread -> replicate component structure, insert child VM
        vm_pointer->threadbarrier();  // synchronize with master thread
        // replicate Component from the parent PET w/ private FTable, insert VM
        comp = component + mytid; // determine PET-local component
        FTN(f_esmf_compreplicate)(comp, (Comp *)(func->funcarg[0]), vm,
          &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
          return rc; // bail out
      }
    }else if (i>-1){
      // not first time entry, but found name in FTable
      // -> must be SetServices _again_
      func = funcs + i; // determine PET-local function entry
      comp = component + mytid; // determine PET-local component
    }else if (componentcount == mynthreads){
      // subsequent time Component is entering its VM -> obtain comp
      comp = component + mytid; // determine PET-local component
      FTable *ftable = **(FTable ***)comp;  // determine comp's FTable instance
      int j = ftable->getEntry(name, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
        return rc; // bail out
      if (j == -1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "unknown function name", &rc);
        return rc; // bail out
      }
      func = ftable->funcs + j; // determine PET-local function entry
    }else{
      // fatal inconsistent situation
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "fatal inconsistency between Component and VM", &rc);
      return rc;
    }
  }else{
    // vm_pointer was not provided -> simple call-back without VM insertion
    func = funcs + i; // determine PET-local function entry
    comp = (Comp *)(func->funcarg[0]);
  }

  // call-back into user code
  switch (func->ftype){
    case FT_VOIDP1INTP: {
      //printf("calling out of case FT_VOIDP1INTP\n");
      if (userrc) func->funcintarg = *userrc;
      VoidP1IntPFunc vf = (VoidP1IntPFunc)func->funcptr;
      (*vf)((void *)comp, &(func->funcintarg));
      if (userrc) *userrc = func->funcintarg;
      break;
    }
    case FT_VOIDP4INTP: {
      //printf("calling out of case FT_VOIDP4INTP\n");
      if (userrc) func->funcintarg = *userrc;
      VoidP4IntPFunc vf = (VoidP4IntPFunc)func->funcptr;
      (*vf)(comp, func->funcarg[1], func->funcarg[2],
        func->funcarg[3], &(func->funcintarg));
      if (userrc) *userrc = func->funcintarg;
      break;
    }
    default:
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "unknown function type", &rc);
      return rc;
  }

  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::extend()"
//BOPI
// !IROUTINE:  extend - make space for additional functions/data
//
// !INTERFACE:
int FTable::extend(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int nfuncp,     // in, number of functions which will be added
      int ndatap) {   // in, number of data pointers which will be added
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // TODO: allocate space for N items, rounded up?
    if (nfuncp > funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, nfuncp * sizeof(funcinfo));
        funcalloc = nfuncp; 
    }
    if (ndatap > dataalloc) {
        data = (datainfo *)realloc((void *)data, ndatap * sizeof(datainfo));
        dataalloc = ndatap;
    }

    //printf("TableExtend called, sizeof(funcinfo)=%d, sizeof(datainfo)=%d\n",
    //                            sizeof(funcinfo), sizeof(datainfo));
    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::query()"
//BOPI
// !IROUTINE:  query - return count of functions/data
//
// !INTERFACE:
int FTable::query(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nfuncp,     // out, number of functions which will be added
      int *ndatap) {   // out, number of data pointers which will be added
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // fill in values
    *nfuncp = funccount;
    *ndatap = datacount;

    //printf("TableQuery method called \n");
    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::validate()"
//BOPI
// !IROUTINE:  validate - internal consistency check for a Component
//
// !INTERFACE:
int FTable::validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char const *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Component is internally consistent.
//      Returns error code if problems are found.  Base class method.
//
//EOPI
//-----------------------------------------------------------------------------
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return ESMC_RC_NOT_IMPL;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::print()"
//BOPI
// !IROUTINE:  print - print contents of a Component
//
// !INTERFACE:
int FTable::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char const *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Component.  The options control the
//      type of information and level of detail.  Base class method.
//
//EOPI
//-----------------------------------------------------------------------------
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return rc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable()"
//BOPI
// !IROUTINE:  FTable - native C++ constructor
//
// !INTERFACE:
FTable::FTable(
//
// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
    void){
//
// !DESCRIPTION:
//  Native constructor.
//
//EOPI
//-----------------------------------------------------------------------------
  //printf("in ftable constructor\n");
  funccount = 0;
  funcalloc = 0;
  funcs = NULL;
  datacount = 0;
  dataalloc = 0; 
  data = NULL;
  componentcount = 0;
  component = NULL;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMCI::FTable()"
//BOPI
// !IROUTINE:  ~FTable - native C++ destructor
//
// !INTERFACE:
FTable::~FTable(void) {
//
// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
//  none
//
// !DESCRIPTION:
//  Native destructor - deleting internal allocations
//
//EOPI
//-----------------------------------------------------------------------------
  for (int i=0; i<funccount; i++)
    funcs[i].~funcinfo();
  free(funcs);
  funccount = 0;
  funcalloc = 0;
  funcs = NULL;
         
  for (int i=0; i<datacount; i++)
    data[i].~datainfo();
  free(data);
  datacount = 0;
  dataalloc = 0; 
  data = NULL;
  
  for (int i=1; i<componentcount; i++){
    // skip i=0 since that is identical to the actual Component object
    Comp *comp = component + i;
    FTable *ftable = **(FTable ***)comp;
    delete ftable;
    ftable = NULL;
    int localrc;
    FTN(f_esmf_compdelete)(comp, &localrc);
  }
  delete component;
  component = NULL;
}
//-----------------------------------------------------------------------------

//==============================================================================
//
// this trim routine does several things:
//
// most importantly, it null terminates a counted-char string passed in
// from fortran.  it's not guarenteed you can write into the N+1th
// character location (if the string is full length in fortran, for example)
// so we're forced to make a copy and copy into it.  this routine allocates,
// so the char string created here MUST be deleted by the caller when finished.
//
// secondly, the phase number (init 1, init 2, etc) is passed in as an int.
// if > 0 this routine turns it into a 2-char string filled with
// leading 0s and tacks it onto the end of the name to make it unique.
//
// and finally, component routines can be called with either a single
// state or a pair of states. we can require that the user specify the
// interface at registration time, or we can decide at run time which
// form was called and dispatch the corresponding entry point.
// for now i'm going to fill both types of component entry points for each
// registration.  i can always remove that code if we force the user
// to specify at registration time what format the states are expected in.
// so if nstate > 0, it gets the same treatment as phase: it's turned into
// a single char string and tacked on the end.
//
// (we'd also at some point like to be able to pass back into fortran
// a really type(State) F90 array - but the bytes on the stack are 
// compiler dependent - we'd have to create the array in fortran and
// save a copy of it to be safe.  that code is *NOT* implemented at this
// point, but i know it would sure seem natural from the user's viewpoint.)
// 

// this is max of 2 char phase + 'P' + 1 char nstate + 'S' + trailing NULL
#define MAXPAD 8

void FTable::newtrim(char const *oldc, int clen, int *phase, int *nstate,
  char **newc) {
  char *cp, *ctmp;
  int hasphase = 0;
  int hasstate = 0;
  char tspace[MAXPAD];
  int pad=2;         // if neither phase nor nstate, still need term NULL

  //printf("in newtrim, oldc = '%s', clen = %d\n", oldc, clen);

  // warning - on the intel compiler, optional args come in
  // as -1, not 0.  check for both before dereferencing.
  if ((phase != NULL) && (phase != (int *)-1) && (*phase > 0))  {
    pad = MAXPAD;
    hasphase++;
  }

  // warning - on the intel compiler, optional args come in
  // as -1, not 0.  check for both before dereferencing.
  // if state > 0, use it to alter the EP name.
  if ((nstate != NULL) && (nstate != (int *)-1) && (*nstate > 0))  {
    pad = MAXPAD;
    hasstate++;
  }

  // make new space and leave room for at least a null terminator, more
  // if it has either phase or num states or both.
  ctmp = new char[clen+pad];
  strncpy(ctmp, oldc, clen);
  ctmp[clen] = '\0';
  for (cp = &ctmp[clen-1]; *cp == ' '; cp--)   // trim() trailing blanks
    *cp = '\0';
  
  // tack on trailing numbers if phase or nstate
  if (hasphase && hasstate) {
    sprintf(tspace, "%02dP%1dS", *phase, *nstate);
    strcat(ctmp, tspace);
  } else if (hasphase) {
    sprintf(tspace, "%02dP", *phase);
    strcat(ctmp, tspace);
  } else if (hasstate) {
    sprintf(tspace, "%1dS", *nstate);
    strcat(ctmp, tspace);
  }

  // set return pointer.  caller MUST free this when finished with it.
  *newc = ctmp;
  //printf("out newtrim, newc = '%s'\n", *newc);
  
  return;
}
//==============================================================================

} // namespace ESMCI



//==============================================================================
//==============================================================================
// MethodTable implementation
//==============================================================================
//==============================================================================

extern "C" {

  // call to native class constructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtablecreate"
  void FTN(c_esmc_methodtablecreate)(ESMCI::MethodTable **ptr, int *rc){
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    (*ptr) = new ESMCI::MethodTable;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- MethodTable allocation", rc);  
      return;
    }
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // call to native class destructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtabledestroy"
  void FTN(c_esmc_methodtabledestroy)(ESMCI::MethodTable **ptr, int *rc){
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- MethodTable deallocation", rc);  
      return;
    }
    delete (*ptr);
    *ptr = NULL;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableadd"
  void FTN(c_esmc_methodtableadd)(ESMCI::MethodTable **ptr,
    char const *labelArg, void *pointer, int *rc,
    ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      localrc = (*ptr)->add(label, pointer);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
        return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- corrupt label string", rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
//      return;
    // debugging---------
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableaddshobj"
  void FTN(c_esmc_methodtableaddshobj)(ESMCI::MethodTable **ptr,
    char const *labelArg, char const *nameArg, char const *sharedObjArg,
    int *rc, ESMCI_FortranStrLenArg labelLen, ESMCI_FortranStrLenArg nameLen,
    ESMCI_FortranStrLenArg sharedObjLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      string label(labelArg, labelLen);
      label.resize(label.find_last_not_of(" ")+1);
      string name(nameArg, nameLen);
      name.resize(name.find_last_not_of(" ")+1);
      string sharedObj(sharedObjArg, sharedObjLen);
      sharedObj.resize(sharedObj.find_last_not_of(" ")+1);
      localrc = (*ptr)->add(label, name, sharedObj);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
        return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- corrupt label string", rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
//      return;
    // debugging---------
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableremove"
  void FTN(c_esmc_methodtableremove)(ESMCI::MethodTable **ptr,
    char const *label, int *rc, ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      string labelArg(label, labelLen);
      labelArg.resize(labelArg.find_last_not_of(" ")+1);
      localrc = (*ptr)->remove(labelArg);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
        return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- corrupt label string", rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
//     return;
    // debugging---------
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_methodtableexecute"
  void FTN(c_esmc_methodtableexecute)(ESMCI::MethodTable **ptr,
    char const *label, void *object, int *userRc, int *rc,
    ESMCI_FortranStrLenArg labelLen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (labelLen>=0){
      string labelArg(label, labelLen);
      labelArg.resize(labelArg.find_last_not_of(" ")+1);
      localrc = (*ptr)->execute(labelArg, object, userRc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
        return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- corrupt label string", rc);
      return;
    }

    // debugging---------
//    localrc = (*ptr)->print();
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
//      return;
    // debugging---------
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

} // extern "C"



namespace ESMCI {
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodElement::print()"
  int MethodElement::print(void)const{
    int rc = ESMC_RC_NOT_IMPL;
    printf("%s\n", label.c_str());
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodElement::execute()"
  int MethodElement::execute(void *object, int *userRc){
    int rc = ESMC_RC_NOT_IMPL;
    if (pointer){
      VoidP1IntPFunc vf = (VoidP1IntPFunc)pointer;
      (*vf)(object, userRc);
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        " - invalid function pointer", &rc);
      return rc;
      
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodElement::resolve()"
  int MethodElement::resolve(void){
    int rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, 
      "- System does not support dynamic loading.", &rc);
    return rc;
#else
    void *lib;
    if (shobj.length()>0){
      lib = dlopen(shobj.c_str(), RTLD_LAZY);
    }else
      lib = dlopen(NULL, RTLD_LAZY);  // search in executable
    if (lib == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "shared object not found", &rc);
      return rc;
    }
    pointer = (void *)dlsym(lib, name.c_str());
    if (pointer == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- named routine not found", &rc);
      return rc;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
#endif
  }
  
// -----------------------------------------------------------------------------
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::print()"
  int MethodTable::print(void)const{
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    MethodElement *element = table; // initialize
    while (element){
      localrc = element->print();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
        return rc; // bail out
      element = element->nextElement;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::add()"
  int MethodTable::add(std::string labelArg, void *pointer){
    int rc = ESMC_RC_NOT_IMPL;
    if (table){
      MethodElement *element = table; // initialize
      MethodElement *prev;
      while (element){
        if (element->label == labelArg){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
            "- method with identical label already exists", &rc);
          return rc;
        }
        prev = element;
        element = element->nextElement;
      }
      prev->nextElement = new MethodElement(labelArg, pointer);
    }else{
      table = new MethodElement(labelArg, pointer);
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::add()"
  int MethodTable::add(std::string labelArg, std::string name,
    std::string sharedObj){
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    MethodElement *element = table; // initialize
    if (table){
      MethodElement *prev;
      while (element){
        if (element->label == labelArg){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
            "- method with identical label already exists", &rc);
          return rc;
        }
        prev = element;
        element = element->nextElement;
      }
      prev->nextElement = new MethodElement(labelArg, name, sharedObj);
      element = prev->nextElement;
    }else{
      table = new MethodElement(labelArg, name, sharedObj);
      element = table;
    }
    localrc = element->resolve();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc; // bail out
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::remove()"
  int MethodTable::remove(std::string labelArg){
    int rc = ESMC_RC_NOT_IMPL;
    if (table){
      MethodElement *element = table; // initialize
      MethodElement *prev = table;  // initialize
      while (element){
        if (element->label == labelArg){
          if (prev == table)
            table = element->nextElement;
          else
            prev->nextElement = element->nextElement;
          delete element;
          // return successfully
          rc = ESMF_SUCCESS;
          return rc;
        }
        prev = element;
        element = element->nextElement;
      }
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- method not found in method table", &rc);
      return rc;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- empty method table", &rc);
      return rc;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodTable::execute()"
  int MethodTable::execute(std::string labelArg, void *object, int *userRc){
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;
    if (table){
      MethodElement *element = table; // initialize
      while (element){
        if (element->label == labelArg){
          localrc = element->execute(object, userRc);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
            return rc; // bail out
          // return successfully
          rc = ESMF_SUCCESS;
          return rc;
        }
        element = element->nextElement;
      }
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- method not found in method table", &rc);
      return rc;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- empty method table", &rc);
      return rc;
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  
} // namespace ESMCI
