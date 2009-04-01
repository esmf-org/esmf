// $Id: ESMCI_FTable.C,v 1.18 2009/04/01 22:28:42 theurich Exp $
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
static const char *const version = "$Id: ESMCI_FTable.C,v 1.18 2009/04/01 22:28:42 theurich Exp $";
//-----------------------------------------------------------------------------


//==============================================================================
// prototypes for fortran interface routines
extern "C" {
  void FTN(f_esmf_compsetvminfo)(ESMCI::Comp *compp, void *vm_info, int *rc);
  void FTN(f_esmf_compgetvminfo)(ESMCI::Comp *compp, void *vm_info, int *rc);
  void FTN(f_esmf_compgetvmparent)(ESMCI::Comp *compp, ESMCI::VM **vmparent, 
    int *rc);
  void FTN(f_esmf_compgetvmplan)(ESMCI::Comp *compp, ESMCI::VMPlan **vmplan, 
    int *rc);
  void FTN(f_esmf_compinsertvm)(ESMCI::Comp *compp, void *vm, int *rc);
  void FTN(f_esmf_compget)(ESMCI::Comp *compp, ESMCI::CompType *ctype, int *rc);
  void FTN(f_esmf_compreplicate)(ESMCI::Comp *compp, ESMCI::Comp *compp_src,
    void *vm, int *rc);
  void FTN(f_esmf_compcopy)(ESMCI::Comp *compp, ESMCI::Comp *compp_src, int     
    *rc);
  void FTN(f_esmf_compdelete)(ESMCI::Comp *compp, int *rc);
};

extern "C"{
  void FTN(f_esmf_fortranudtpointersize)(int *size);
  void FTN(f_esmf_fortranudtpointercopy)(void *dst, void *src);
}
//==============================================================================


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

static void newtrim(char *c, int clen, int *phase, int *nstate, char **newc) {
  char *cp, *ctmp;
  int hasphase = 0;
  int hasstate = 0;
  char tspace[MAXPAD];
  int pad=2;         // if neither phase nor nstate, still need term NULL

  //printf("in newtrim, c = '%s', clen = %d\n", c, clen);

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
  strncpy(ctmp, c, clen);
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


//==============================================================================
// FTable interfaces to be called from Fortran side (ESMF_Comp.F90)
//
// these interface subroutine names MUST be in lower case
//
extern "C" {

  // call to native class constructor
  void FTN(c_esmc_ftablecreate)(ESMCI::FTable **ptr, int *status) {
    *status = ESMC_RC_NOT_IMPL;
    (*ptr) = new ESMCI::FTable;
    (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
  }

  // call to native class destructor
  void FTN(c_esmc_ftabledestroy)(ESMCI::FTable **ptr, int *status) {
    *status = ESMC_RC_NOT_IMPL;
    delete (*ptr);
    *ptr = 0;
    *status = ESMF_SUCCESS;
  }
  
  // call a function through the ftable
  void FTN(c_esmc_ftablecallentrypoint)(ESMCI::FTable **ptr, 
    enum ESMCI::method *method, int *phase, int *status){
    int funcrc;
    char *name;
    int localrc = ESMC_RC_NOT_IMPL;
    *status = ESMC_RC_NOT_IMPL;

    char *methodString;
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
    newtrim(methodString, slen, phase, NULL, &name);
    //printf("after newtrim, name = '%s'\n", name);

    // TODO: two return codes here - one is whether we could find
    // the right function to call; the other is the actual return code
    // from the user function itself.

    localrc = (*ptr)->callVFuncPtr(name, NULL, &funcrc);

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

  // set arguments for standard Component methods
  void FTN(c_esmc_ftablesetstateargs)(ESMCI::FTable **ptr, 
    enum ESMCI::method *method, int *phase, void *comp, void *importState,
    void *exportState, void *clock, int *status){
    char *fname;
    int acount = 5;
    void *alist[5];

    *status = ESMC_RC_NOT_IMPL;

    char *methodString;
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
    newtrim(methodString, slen, phase, NULL, &fname);
    //printf("after newtrim, name = '%s'\n", fname);

    alist[0] = (void *)comp;
    alist[1] = (void *)importState;
    alist[2] = (void *)exportState;
    alist[3] = (void *)clock;
    alist[4] = (void *)status;

    *status = (*ptr)->setFuncArgs(fname, acount, alist);
    delete[] fname;
  }

  // set arguments for RESTART Component methods
  // TODO: treat this as standard Component method
  void FTN(c_esmc_ftablesetioargs)(ESMCI::FTable **ptr,
    enum ESMCI::method *method, int *phase,
    void *comp, void *iospec, void *clock, int *status) {
    char *fname;
    int acount = 4;
    void *alist[4];

    *status = ESMC_RC_NOT_IMPL;

    char *methodString;
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
    newtrim(methodString, slen, phase, NULL, &fname);
    //printf("after newtrim, name = '%s'\n", fname);

    alist[0] = (void *)comp;
    alist[1] = (void *)iospec;
    alist[2] = (void *)clock;
    alist[3] = (void *)status;

    *status = (*ptr)->setFuncArgs(fname, acount, alist);
    delete[] fname;
  }

  void FTN(c_esmc_ftablesetinternalstate)(ESMCI::FTable ***ptr, char *type,
    void **data, enum ESMCI::dtype *dtype, int *status, int slen) {
    char *name;

    *status = ESMC_RC_NOT_IMPL;

    newtrim(type, slen, NULL, NULL, &name);
    //printf("after newtrim, name = '%s'\n", name);

    *status = (**ptr)->setDataPtr(name, data, *dtype);
    delete[] name;
  }

  void FTN(c_esmc_ftablegetinternalstate)(ESMCI::FTable ***ptr, char *type,
    void **data, enum ESMCI::dtype *dtype, int *status, int slen) {
    char *name;

    *status = ESMC_RC_NOT_IMPL;

    newtrim(type, slen, NULL, NULL, &name);
    //printf("after newtrim, name = '%s'\n", name);

    *status = (**ptr)->getDataPtr(name, data, dtype);

    delete[] name;
  }
  
} // extern "C"
//==============================================================================


//==============================================================================
// These functions are being called through the Fortran interface layer
extern "C" {
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setvm"
  void FTN(c_esmc_setvm)(void *ptr, void (*func)(), int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setVM(ptr, func, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setvmshobj"
  void FTN(c_esmc_setvmshobj)(void *ptr, char *routineArg, 
    char *sharedObjArg, int *rc, 
    ESMCI_FortranStrLenArg rlen, ESMCI_FortranStrLenArg llen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, 
      "System does not support dynamic loading.", rc);
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
    ESMCI::FTable::setVM(ptr, func, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    if (rc) *rc = ESMF_SUCCESS;
#endif
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservices"
  void FTN(c_esmc_setservices)(void *ptr, void (*func)(), int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setServices(ptr, func, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservicesshobj"
  void FTN(c_esmc_setservicesshobj)(void *ptr, char *routineArg, 
    char *sharedObjArg, int *rc, 
    ESMCI_FortranStrLenArg rlen, ESMCI_FortranStrLenArg llen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, 
      "System does not support dynamic loading.", rc);
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
    ESMCI::FTable::setServices(ptr, func, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    if (rc) *rc = ESMF_SUCCESS;
#endif
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setentrypoint"
  void FTN(c_esmc_setentrypoint)(void *ptr, enum ESMCI::method *method,
    void *func, int *phase, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    char *methodString;
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
    newtrim(methodString, slen, phase, NULL, &fname);
         
    ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
    localrc = (tabptr)->setFuncPtr(fname, func, ESMCI::FT_COMP2STAT);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) 
      return;
    
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
  void FTN(esmf_gridcompsetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *status){
    ESMCI::FTable::setDP(ptr, datap, status);
  }
  
  void FTN(esmf_gridcompgetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *status){
    ESMCI::FTable::getDP(ptr, datap, status);
  }

  // ---------- CplComp ---------------
  void FTN(esmf_cplcompsetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *status){
    ESMCI::FTable::setDP(ptr, datap, status);
  }

  void FTN(esmf_cplcompgetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *status){
    ESMCI::FTable::getDP(ptr, datap, status);
  }

  // ---------- UserComp ---------------
  void FTN(esmf_usercompsetservices)(void *ptr, void (*func)(), int *status){
    ESMCI::FTable::setServices(ptr, func, status);
  }

  void FTN(esmf_usercompsetvm)(void *ptr, void (*func)(), int *status){
    ESMCI::FTable::setVM(ptr, func, status);
  }

  void FTN(esmf_usercompsetentrypoint)(void *ptr, char *tname, void *func,
    int *phase, int *status, int slen){
    ESMCI::FTable::setTypedEP(ptr, tname, slen, phase, 0, ESMCI::FT_VOIDPINTP,
      func,  status);
  }

  void FTN(esmf_usercompsetinternalstate)(ESMCI::FTable ***ptr, char *name, 
    void **datap, int *status, int slen){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMF_UserCompSetInternalState"
    char *tbuf; 
    enum ESMCI::dtype dtype = ESMCI::DT_FORTRAN_UDT_POINTER;
    int localrc;

    // Initialize return code; assume routine not implemented
    if (status) *status = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "null pointer found", status);
      return;
    }

    newtrim(name, slen, NULL, NULL, &tbuf);
    //printf("after newtrim, name = '%s'\n", tbuf);

    localrc = (**ptr)->setDataPtr(tbuf, datap, dtype);
  
    delete[] tbuf;
    if (status) *status = localrc;
  }

  void FTN(esmf_usercompgetinternalstate)(ESMCI::FTable ***ptr, char *name,
    void **datap, int *status, int slen){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMF_UserCompGetInternalState"
    char *tbuf; 
    enum ESMCI::dtype dtype;
    int localrc;

    // Initialize return code; assume routine not implemented
    if (status) *status = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "null pointer found", status);
      return;
    }

    newtrim(name, slen, NULL, NULL, &tbuf);
    //printf("after newtrim, name = '%s'\n", tbuf);

    localrc = (**ptr)->getDataPtr(tbuf, datap, &dtype);
  
    delete[] tbuf;
    if (status) *status = localrc;
  }

} // extern "C"
//==============================================================================


//==============================================================================
// VM-enabled CallBack loop     
extern "C" {
     
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_FTableCallEntryPointVMHop"
static void *ESMCI_FTableCallEntryPointVMHop(void *vm, void *cargo){
  // This routine is the first level that gets instantiated in new VM
  // The first argument must be of type (void *) and points to a derived
  // ESMCI::VMK class object. The second argument is also of type (void *)
  // and points to a cargotype structure.
  
  // pull out info from cargo
  char *name = ((ESMCI::cargotype *)cargo)->name;         // name of callback
  ESMCI::FTable *ftable = ((ESMCI::cargotype *)cargo)->ftable; // ptr to ftable
  
  int esmfrc;   // ESMF return code of ESMCI::FTableCallVFuncPtr()
  int userrc;   // user return code from the registered component method 
  
  // call into user code through ESMF function table...
  esmfrc = ftable->callVFuncPtr(name, (ESMCI::VM*)vm, &userrc);
  // ...back from user code
  
  // put the return codes into cargo 
  // TODO: If this PET is part of a threadgroup that was spawned out of a
  // single parent PET then return codes must be returned as a single value to
  // the parent in the cargo structure (btw, each child PET that's a thread
  // has the same pointer to cargo. Naturally the above must be done in a
  // threadsafe manner :-). 
  ((ESMCI::cargotype *)cargo)->esmfrc = esmfrc;
  ((ESMCI::cargotype *)cargo)->userrc = userrc;
  
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
  int *status                 // return error code in status
  ){
       
  // local variables
  int localrc;              // local return code
  char *name;               // trimmed type string

  // Initialize return code; assume routine not implemented
  if (status) *status = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  char *methodString;
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
  newtrim(methodString, slen, phase, NULL, &name);

  // Things get a little confusing here with pointers, so I will define
  // some temp. variables that make matters a little clearer I hope:
  ESMCI::VM *vm_parent = *ptr_vm_parent;      // pointer to parent VM
  ESMCI::VMPlan *vmplan = *ptr_vmplan;        // pointer to VMPlan
  ESMCI::FTable *ftable = *ptr;               // pointer to function table
         
  ESMCI::cargotype *cargo = new ESMCI::cargotype;
  strcpy(cargo->name, name);   // copy trimmed type string
  cargo->ftable = ftable;      // pointer to function table
  cargo->esmfrc = ESMF_SUCCESS;// initialize return code to SUCCESS for all PETs
  cargo->userrc = ESMF_SUCCESS;// initialize return code to SUCCESS for all PETs
  *vm_cargo=(void*)cargo;      // store pointer to the cargo structure

  // enter the child VM -> resurface in ESMCI_FTableCallEntryPointVMHop()
  localrc = vm_parent->enter(vmplan, *vm_info, (void*)cargo);
  ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, status);
        
  // ... if the child VM uses threads (multi-threading or single-threading) 
  // then this parent PET continues running concurrently to the child PET in the
  // same VAS! In that case the return codes in cargo are not valid here!
  // The status returned by VMEnter() indicates that success of entering the
  // child VM, not failure or success of the callback.
  // The return code of the callback code will be valid in all cases (threading
  // or no threading) _after_ VMK::exit() returns.
  
  delete[] name;  // delete memory that "newtrim" allocated above
}

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_compwait"
void FTN(c_esmc_compwait)(
  ESMCI::VM **ptr_vm_parent,  // p2 to the parent VM
  ESMCI::VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
  void **vm_info,             // p2 to member which holds info
  void **vm_cargo,            // p2 to member which holds cargo
  int *callrc,                // return code of the user component method
  int *esmfrc) {              // esmf internal return error code

  // Things get a little confusing here with pointers, so I will define
  // some temp. variables that make matters a little clearer I hope:
  ESMCI::VM *vm_parent = *ptr_vm_parent;        // pointer to parent VM
  ESMCI::VMPlan *vmplan = *ptr_vmplan;          // pointer to VMPlan
  ESMCI::cargotype *cargo = (ESMCI::cargotype *)*vm_cargo;  // pointer to cargo
  
  // initialize the return codes
  *esmfrc = ESMC_RC_NOT_SET; // return code of ESMF callback code
  *callrc = ESMC_RC_NOT_SET; // return code of registered user code

  // Now call the vmk_exit function which will block respective PETs
  vm_parent->exit(static_cast<ESMCI::VMKPlan *>(vmplan), *vm_info);
  
  // return with errors if there is no cargo to obtain error codes
  if (cargo == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - No cargo structure to obtain error codes", esmfrc);
    return;
  }
  
  // obtain return codes out of cargo
  *esmfrc = cargo->esmfrc;
  *callrc = cargo->userrc;
  
  // delete cargo structure
  delete cargo;
  
  // error check esmfrc
  if (ESMC_LogDefault.MsgFoundError(*esmfrc,
    " - ESMF internal error during user function callback", NULL)) return;
  
}
  
} // extern "C"
//==============================================================================


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
#define ESMC_METHOD "ESMCI::FTable::SetTypedEP()"
void FTable::setTypedEP(void *ptr, char *tname, int slen, int *phase, 
  int nstate, enum ftype ftype, void *func, int *status) {
     char *name;
     int *tablerc;
     int localrc;
     void *f90comp = ptr;
     FTable *tabptr;

     // Initialize return code; assume routine not implemented
     if (status) *status = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

     //printf("ptr = 0x%08x\n", (ESMC_POINTER)ptr);
     //printf("*ptr = 0x%08x\n", (ESMC_POINTER)(*(int*)ptr));
     if ((ptr == ESMC_NULL_POINTER) || ((*(void**)ptr) == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
        return;
     }

     tabptr = **(FTable***)ptr;
     //printf("tabptr = 0x%08x\n", (ESMC_POINTER)(tabptr));
     newtrim(tname, slen, phase, &nstate, &name);
         
     //printf("SetTypedEP: setting function name = '%s'\n", name);
     if (ftype == FT_VOIDPINTP) {
         // TODO: same as the register routine - you cannot delete tablerc
         // yet - you have to wait until the table is deleted, and then the
         // table does not know which of the stored args can be deleted and
         // which cannot.  maybe the args need to all be allocated and all
         // nuked at table destroy time.
         tablerc = new int;
         localrc = (tabptr)->setFuncPtr(name, func, f90comp, tablerc);
     } else
         localrc = (tabptr)->setFuncPtr(name, func, ftype);

     if (status) *status = localrc;
     delete[] name;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::getDP()"
void FTable::getDP(FTable ***ptr, void **datap, int *status){
    char *name = "localdata";
    enum dtype dtype;
    int localrc;

     // Initialize return code; assume routine not implemented
     if (status) *status = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

     //printf("ptr = 0x%08x\n", (ESMC_POINTER)ptr);
     //printf("*ptr = 0x%08x\n", (ESMC_POINTER)(*(int*)ptr));
    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
       return;
    }

    localrc = (**ptr)->getDataPtr(name, datap, &dtype);
    if (status) *status = localrc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setDP()"
void FTable::setDP(FTable ***ptr, void **datap, int *status){
    char *name = "localdata";
    enum dtype dtype = DT_FORTRAN_UDT_POINTER;
    int localrc;

     // Initialize return code; assume routine not implemented
     if (status) *status = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

     //printf("ptr = 0x%08x\n", (ESMC_POINTER)ptr);
     //printf("*ptr = 0x%08x\n", (ESMC_POINTER)(*(int*)ptr));
    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                                              "null pointer found", status);
        return;
    }

    localrc = (**ptr)->setDataPtr(name, datap, dtype);
    if (status) *status = localrc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Ftable::setServices"
void FTable::setServices(void *ptr, void (*func)(), int *status) {
  int localrc, funcrc;
  int *tablerc;
  ESMCI::Comp *f90comp = (ESMCI::Comp *)ptr;
  ESMCI::FTable *tabptr;
  
  if (status) *status = ESMF_SUCCESS;  // assume success 'till problems found

  //printf("ptr = 0x%08x\n", (ESMC_POINTER)ptr);
  //printf("*ptr = 0x%08x\n", (ESMC_POINTER)(*(int*)ptr));
  //if ((ptr == ESMC_NULL_POINTER)) {
  if ((ptr == ESMC_NULL_POINTER) || ((*(void**)ptr) == ESMC_NULL_POINTER)) {
     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                                           "null pointer found", status);
     return;
  }
  tabptr = **(ESMCI::FTable***)ptr;
  //printf("tabptr = 0x%08x\n", (ESMC_POINTER)(tabptr));

  // TODO: shouldn't need to expand the table here - should be buried
  // inside ftable code.
  localrc = (tabptr)->extend(8, 2); // room for 8 funcs, 2 data
  if (localrc != ESMF_SUCCESS) {
      if (status) *status = localrc;
      return;
  }

  // TODO: this is going to cause a memory leak, because the 'tablerc'
  // variable is actually stored in the jump table as one of the arguments.
  // i believe it is used to call a subroutine which has a return code,
  // but then that code is ignored.  so there are two problems here:
  // how to return the error(s), and how to delete the integer when the
  // table is destroyed.  for now, it is a leak; small, and only shows up
  // when you delete a component which does not happen often, thankfully.
  tablerc = new int;
  localrc = (tabptr)->setFuncPtr("Register", (void *)func, f90comp, tablerc);

  // TODO: decide what to do if tablerc comes back
  // with an error.  for now, ignore it and look at localrc only.
  
  if (localrc != ESMF_SUCCESS) {
      if (status) *status = localrc;
      return;
  }

  // time to startup the VM for this component (if not already started)...
  ESMCI::VM *vm_parent;
  FTN(f_esmf_compgetvmparent)(f90comp, &vm_parent, &localrc); //get vm_parent
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
    status)) return;
  ESMCI::VMPlan *vmplan_p;
  FTN(f_esmf_compgetvmplan)(f90comp, &vmplan_p, &localrc);    //get vmplan_p
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
    status)) return;
  void *vm_info;
  FTN(f_esmf_compgetvminfo)(f90comp, &vm_info, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
    status)) return;
  if (vm_info==NULL){
    // VM for this component has not been started yet
    vm_info = vm_parent->startup(vmplan_p,
      ESMCI_FTableCallEntryPointVMHop, NULL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      status)) return;
    // keep vm_info in a safe place (in parent component) 'till it's used again
    FTN(f_esmf_compsetvminfo)(f90comp, &vm_info, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      status)) return;
  }
  // ...now the component's VM is started up and placed on hold.
  
  // call into register routine using the component's VM
  void *vm_cargo;
  enum method reg = SETREGISTER;
  FTN(c_esmc_ftablecallentrypointvm)(&vm_parent, &vmplan_p, &vm_info,
    &vm_cargo, &tabptr, &reg, NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
    status)) return;
  
  int callrc;
  FTN(c_esmc_compwait)(&vm_parent, &vmplan_p, &vm_info,
    &vm_cargo, &callrc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
    status)) return;
  
  return;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Ftable::setVM"
void FTable::setVM(void *ptr, void (*func)(), int *status) {
  int localrc, funcrc;
  int *tablerc;
  ESMCI::Comp *f90comp = (ESMCI::Comp *)ptr;
  ESMCI::FTable *tabptr;
  
  if (status) *status = ESMF_SUCCESS;  // assume success 'till problems found

  //printf("ptr = 0x%08x\n", (ESMC_POINTER)ptr);
  //printf("*ptr = 0x%08x\n", (ESMC_POINTER)(*(int*)ptr));
  //if ((ptr == ESMC_NULL_POINTER)) {
  if ((ptr == ESMC_NULL_POINTER) || ((*(void**)ptr) == ESMC_NULL_POINTER)) {
     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                                           "null pointer found", status);
     return;
  }
  tabptr = **(ESMCI::FTable***)ptr;
  //printf("tabptr = 0x%08x\n", (ESMC_POINTER)(tabptr));

  // TODO: shouldn't need to expand the table here - should be buried
  // inside ftable code.
  localrc = (tabptr)->extend(8, 2); // room for 8 funcs, 2 data
  if (localrc != ESMF_SUCCESS) {
      if (status) *status = localrc;
      return;
  }

  // TODO: this is going to cause a memory leak, because the 'tablerc'
  // variable is actually stored in the jump table as one of the arguments.
  // i believe it is used to call a subroutine which has a return code,
  // but then that code is ignored.  so there are two problems here:
  // how to return the error(s), and how to delete the integer when the
  // table is destroyed.  for now, it is a leak; small, and only shows up
  // when you delete a component which does not happen often, thankfully.
  tablerc = new int;
  localrc = (tabptr)->setFuncPtr("setVM", (void *)func, f90comp, tablerc);

  // TODO: decide what to do if tablerc comes back
  // with an error.  for now, ignore it and look at localrc only.
  
  if (localrc != ESMF_SUCCESS) {
      if (status) *status = localrc;
      return;
  }

  localrc = (tabptr)->callVFuncPtr("setVM", NULL, &funcrc);
  if (localrc != ESMF_SUCCESS) {
      if (status) *status = localrc;
      return;
  }

  if (funcrc != ESMF_SUCCESS) {
      if (status) *status = funcrc;
      return;
  }

  return;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::extend()"
//BOP
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
//EOP
// !REQUIREMENTS:  

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
//BOP
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
//EOP
// !REQUIREMENTS:  

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
#define ESMC_METHOD "ESMCI::FTable::setFuncPtr()"
//BOP
// !IROUTINE:  setFuncPtr - set function pointer, no extra args
//
// !INTERFACE:
      int FTable::setFuncPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      void *func) {          // in, function address
//
// !DESCRIPTION:
//    Sets the named function pointer
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // extend the table if needed
    if (thisfunc >= funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
        funcalloc = thisfunc+4;
    }
    funcs[thisfunc].funcptr = func;
    funcs[thisfunc].ftype = FT_NULL;
    // do these only if not replacing an existing entry.
    if (thisfunc == funccount) {
        funcs[thisfunc].funcname = new char[strlen(name)+1];
        strcpy(funcs[thisfunc].funcname, name);
        funccount++;
    }
   

    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncPtr()"
//BOP
// !IROUTINE:  setFuncPtr - set function pointer, type; no args yet.
//
// !INTERFACE:
      int FTable::setFuncPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      void *func,            // in, function address
      enum ftype ftype) {    // in, function type
//
// !DESCRIPTION:
//    Sets the named function pointer and type, but specifies no argument 
//    values.  Before this can be called successfully, the user must call
//    FTable::setFuncArgs to fill in the argument list.
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // extend the table if needed
    if (thisfunc >= funcalloc) {
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
   

    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncPtr()"
//BOP
// !IROUTINE:  setFuncPtr - set voidp, intp specifically
//
// !INTERFACE:
      int FTable::setFuncPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      void *func,            // in, function address
      void *arg1,            // in, void *
      int *arg2) {           // in, int *
//
// !DESCRIPTION:
//    Sets the named function pointer and args.  This is a common case
//    so it has it's own interface.
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // The 2nd time I come in, delete the old one
    if (thisfunc < funccount) 
       delete ((int *) funcs[thisfunc].funcarg[1]);
    
    // extend the table if needed
    if (thisfunc >= funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
        funcalloc = thisfunc+4;
    }
    funcs[thisfunc].funcptr = func;
    funcs[thisfunc].ftype = FT_VOIDPINTP;
    funcs[thisfunc].funcarg[0] = arg1;
    funcs[thisfunc].funcarg[1] = (void *)arg2;
    // do these only if not replacing an existing entry.
    if (thisfunc == funccount) {
        funcs[thisfunc].funcname = new char[strlen(name)+1];
        strcpy(funcs[thisfunc].funcname, name);
        funccount++;
    }
   

    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncPtr()"
//BOP
// !IROUTINE:  setFuncPtr - set function pointer, arg list
//
// !INTERFACE:
      int FTable::setFuncPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      void *func,            // in, function address
      enum ftype ftype,      // in, function type
      int acount,            // in, count of args
      void **arglist) {      // in, address of arg list
//
// !DESCRIPTION:
//    Sets the named function pointer and args
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, thisfunc;
    //char msgbuf[ESMF_MAXSTR];

    // look for the name already existing in the table.  if found
    // replace it.  otherwise add it to the end.
    for (i=0; i<funccount; i++) {
        if (!strcmp(name, funcs[i].funcname))
           break;
    }

    // we found the function, or we got to the end of the table.
    // either way we are ready to add it.

    thisfunc = i;

    // extend the table if needed
    if (thisfunc >= funcalloc) {
        funcs = (funcinfo *)realloc((void *)funcs, (thisfunc+4) * sizeof(funcinfo));
        funcalloc = thisfunc+4;
    }
    funcs[thisfunc].funcptr = func;
    funcs[thisfunc].ftype = ftype;
    for(i=0; i<acount; i++)
        funcs[thisfunc].funcarg[i] = arglist[i];
    // do these only if not replacing an existing entry.
    if (thisfunc == funccount) {
        funcs[thisfunc].funcname = new char[strlen(name)+1];
        strcpy(funcs[thisfunc].funcname, name);
        funccount++;
    }
   

    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::setFuncArgs()"
//BOP
// !IROUTINE:  setFuncArgs - set arglist for existing function
//
// !INTERFACE:
      int FTable::setFuncArgs(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *name,            // in, function name
      int acount,            // in, count of args
      void **arglist) {      // in, address of arg list
//
// !DESCRIPTION:
//    Sets the named function args.  The function must already exist.
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int status = ESMC_RC_NOT_IMPL;
    int i, j;
    char msgbuf[ESMF_MAXSTR];

    for (i=0; i<funccount; i++) {
        if (strcmp(name, funcs[i].funcname))
           continue;
   
        for(j=0; j<acount; j++)
            funcs[i].funcarg[j] = arglist[j];
   
        return ESMF_SUCCESS;
    }

   
    sprintf(msgbuf, "Error: function '%s' not found\n", name);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, msgbuf, &status);

    return status;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::getDataPtr()"
//BOP
// !IROUTINE:  getDataPtr - get data pointer from name
//
// !INTERFACE:
      int FTable::getDataPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *namep,           // in, data name
      void **datap,          // out, data address
      enum dtype *dtype) {   // out, data type
//
// !DESCRIPTION:
//    Returns the named data pointer
//
//EOP
// !REQUIREMENTS:  

    int i;

    for (i=0; i<datacount; i++) {
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
//BOP
// !IROUTINE:  setDataPtr - set data pointer
//
// !INTERFACE:
      int FTable::setDataPtr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *namep,           // in, data name
      void **datap,          // in, data address
      enum dtype dtype) {    // in, data type
//
// !DESCRIPTION:
//    Sets the named data pointer
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // TODO: test this code
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
#define ESMC_METHOD "ESMCI::FTable::getEntry()"
//BOP
// !IROUTINE:  getEntry - get FTable entry from name
//
// !INTERFACE:
int FTable::getEntry(
//
// !RETURN VALUE:
//    entry index, or -1 if not found.
//
// !ARGUMENTS:
  char *name,            // in, function name
  int *rc) {             // out, return code
//
// !DESCRIPTION:
//    Returns the named entry
//
//EOP
//-----------------------------------------------------------------------------
  // Initialize rc and localrc ; assume functions not implemented
  if (*rc) *rc = ESMC_RC_NOT_IMPL;

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
#define ESMC_METHOD "ESMCI::FTable::callVFuncPtr()"
//BOP
// !IROUTINE:  callVFuncPtr - call a function w/ proper args
//
// !INTERFACE:
int FTable::callVFuncPtr(
//
// !RETURN VALUE:
//    integer return code
//
// !ARGUMENTS:
  char *name,           // in, function name
  VM *vm_pointer,       // in, pointer to this PET's VM instance
  int *userrc) {        // out, function return
//
// !DESCRIPTION:
//    Calls the named function pointer
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (userrc!=NULL) *userrc = -99999;     // set userrc to something obvious

  // find the "name" entry
  int i = getEntry(name, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &localrc))
    return localrc; // bail out
  if (i == -1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
    "unknown function name", &localrc);
    return localrc; // bail out
  }
  
  // optionally insert vm and replicate Component object, according to situation
  Comp *comp;
  if (vm_pointer){
    // vm_pointer was provided -> use to set VM in Component object
    VM *vmm = vm_pointer;
    VM **vm = &vmm;
    int rrc;
    int mypet = vm_pointer->getMypet();
    if (vm_pointer->getNthreads(mypet) > 1){
      // mypet is part of a thread group
      CompType ctype;
      // Replicate the component object on the heap for this thread
      FTN(f_esmf_compget)((Comp *)funcs[i].funcarg[0], &ctype, &rrc);
      if (ctype == COMPTYPE_GRID || ctype == COMPTYPE_CPL)
        comp = new Comp;
      else
        comp = (Comp *)NULL;
      // replicate the component object for each thread
      FTN(f_esmf_compreplicate)(comp, (Comp *)funcs[i].funcarg[0], vm, &rrc);
    }else{
      // mypet is a single-threaded process within this VM
      // insert the current vm object into component object
      comp = (Comp *)funcs[i].funcarg[0];
      FTN(f_esmf_compinsertvm)(comp, vm, &rrc);
    }
  }else{
    // vm_pointer was not provided -> simple call-back without vm insertion
    comp = (Comp *)funcs[i].funcarg[0];
  }

  // call-back into user code
  switch (funcs[i].ftype){
    case FT_VOIDPINTP: {
//printf("calling out of case FT_VOIDPINTP VM\n");
      VoidPtrIntPtrFunc vf = (VoidPtrIntPtrFunc)funcs[i].funcptr;
      (*vf)((void *)comp, (int *)funcs[i].funcarg[1]);
      *userrc = *(int *)(funcs[i].funcarg[1]);
      break;
    }
    case FT_COMP2STAT: {
//printf("calling out of case FT_COMP2STAT VM\n");
      C2SFunc vf = (C2SFunc)funcs[i].funcptr;
      (*vf)(comp, funcs[i].funcarg[1], funcs[i].funcarg[2],
        funcs[i].funcarg[3], (int *)funcs[i].funcarg[4]);
      *userrc = *(int *)(funcs[i].funcarg[4]);
      break;
    }
    default:
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "unknown function type", &localrc);
      return localrc;
  }
  
  // check if anything needs to be updated after call-back
  if (vm_pointer){
    // vm_pointer was provided -> use to set VM in Component object
    VM *vmm = vm_pointer;
    VM **vm = &vmm;
    int rrc;
    int mypet = vm_pointer->getMypet();
    if (vm_pointer->getNthreads(mypet) > 1){
      // mypet is part of a thread group
      // Update the original with any changes made by the child
      // todo: how can this be done correctly merging all the copies
      // todo: into a single object?
      //FTN(f_esmf_compcopy)((Comp *)funcs[i].funcarg[0],
      //  (Comp *)comp, &rrc);
      // Delete the heap copy of the component object for this thread
      FTN(f_esmf_compdelete)((Comp *)comp, &rrc);
      delete (Comp *)comp;
    }
  }

  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::validate()"
//BOP
// !IROUTINE:  validate - internal consistency check for a Component
//
// !INTERFACE:
      int FTable::validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Component is internally consistent.
//      Returns error code if problems are found.  Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return ESMC_RC_NOT_IMPL;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable::print()"
//BOP
// !IROUTINE:  print - print contents of a Component
//
// !INTERFACE:
      int FTable::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Component.  The options control the
//      type of information and level of detail.  Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return rc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FTable()"
//BOP
// !IROUTINE:  FTable - native C++ constructor
//
// !INTERFACE:
      FTable::FTable(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//   Native constructor.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    //printf("in ftable constructor\n");
    funccount = 0;
    funcalloc = 0;
    funcs = NULL;
    datacount = 0;
    dataalloc = 0; 
    data = NULL;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMCI::FTable()"
//BOP
// !IROUTINE:  ~FTable - native C++ destructor
//
// !INTERFACE:
      FTable::~FTable(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n
  
    int i;

    //printf("in ftable destructor\n");
    //if (funcs) delete[] funcs;
    //if (data) delete[] data;

    for (i=0; i<funccount; i++)
        funcs[i].~funcinfo();
        
    for (i=0; i<datacount; i++)
        data[i].~datainfo();

    if (funcs) free(funcs);
    if (data) free(data);

    funccount = 0;
    funcalloc = 0;
    funcs = 0;
    datacount = 0;
    dataalloc = 0; 
    data = 0;
}
//-----------------------------------------------------------------------------

} // namespace ESMCI
