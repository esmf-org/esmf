// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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

// include ESMF headers
#include "ESMCI_Comp.h"
#include "ESMCI_CompTunnel.h"
#include "ESMCI_LogErr.h"

using std::string;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


//==============================================================================
// prototypes for Fortran interface routines called by C++ code below
//TODO: eventually move these calls into the ESMCI::Comp class
extern "C" {
  void FTN_X(f_esmf_compsetvminfo)(ESMCI::Comp *compp, void *vm_info, int *rc);
  void FTN_X(f_esmf_compresetvmreleased)(ESMCI::Comp *compp, int *rc);
  void FTN_X(f_esmf_compinsertvm)(ESMCI::Comp *compp, void *vm, int *rc);
  void FTN_X(f_esmf_compgetctype)(ESMCI::Comp *compp, ESMCI::CompType *ctype,
    int *rc);
  void FTN_X(f_esmf_compreplicate)(ESMCI::Comp *compp, ESMCI::Comp *compp_src,
    void *vm, int *rc);
  void FTN_X(f_esmf_comprefcopy)(ESMCI::Comp *compp, ESMCI::Comp *compp_src,
    int *rc);
  void FTN_X(f_esmf_compdelete)(ESMCI::Comp *compp, int *rc);
  
  void FTN_X(f_esmf_fortranudtpointersize)(int *size);
  void FTN_X(f_esmf_fortranudtpointercopy)(void *dst, void *src);
  
  void FTN_X(esmf_complianceicregister)(void *comp, int *rc);
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
  void FTN_X(c_esmc_ftablecreate)(ESMCI::FTable **ptr, int *rc) {
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    (*ptr) = new ESMCI::FTable;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- Ftable allocation", ESMC_CONTEXT, rc);  
      return;
    }
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // call to native class destructor
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftabledestroy"
  void FTN_X(c_esmc_ftabledestroy)(ESMCI::FTable **ptr, int *rc) {
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*ptr == NULL){
      ESMC_LogDefault.MsgAllocError("- Ftable deallocation", ESMC_CONTEXT, rc);  
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
  void FTN_X(c_esmc_ftablesetstateargs)(ESMCI::FTable **ptr, 
    enum ESMCI::method *method, int *phase, void *comp,
    ESMCI::State *importState, ESMCI::State *exportState, ESMCI::Clock **clock,
    ESMCI::CompTunnel **compTunnel, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    if (*compTunnel != NULL){
      // this is a dual component which contains a valid actual component object
      // -> set references to the passed in arguments and return
      (*compTunnel)->setMethod(*method);
      (*compTunnel)->setPhase(*phase);
      (*compTunnel)->setImportState(importState);
      (*compTunnel)->setExportState(exportState);
      (*compTunnel)->setClock(clock);
      
    }else{
      // this is not a dual component, thus do the actual ftable encoding

      char const *methodString = ESMCI::FTable::methodString(*method);
    
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
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return;
      }

      delete[] fname;  // delete memory that "newtrim" allocated above
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // set the InternalState in FTable
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablesetinternalstate"
  void FTN_X(c_esmc_ftablesetinternalstate)(ESMCI::FTable ***ptr,
    char const *type, void **data, enum ESMCI::dtype *dtype, int *rc,
    ESMCI_FortranStrLenArg slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    char *name;
    ESMCI::FTable::newtrim(type, slen, NULL, NULL, &name);
    //printf("after newtrim, name = '%s'\n", name);

    localrc = (**ptr)->setDataPtr(name, data, *dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;

    delete[] name;  // delete memory that "newtrim" allocated above

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // get the InternalState from FTable
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablegetinternalstate"
  void FTN_X(c_esmc_ftablegetinternalstate)(ESMCI::FTable ***ptr,
    char const *type, void **data, enum ESMCI::dtype *dtype, int *rc,
      ESMCI_FortranStrLenArg slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    char *name;
    ESMCI::FTable::newtrim(type, slen, NULL, NULL, &name);
    //printf("after newtrim, name = '%s'\n", name);

    localrc = (**ptr)->getDataPtr(name, data, dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;

    delete[] name;  // delete memory that "newtrim" allocated above

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setvm"
  void FTN_X(c_esmc_setvm)(void *ptr, void (*func)(), int *userRc, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setVM(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setvmshobj"
  void FTN_X(c_esmc_setvmshobj)(void *ptr, char const *routineArg, 
    char const *sharedObjArg, int *userRc, int *rc, 
    ESMCI_FortranStrLenArg rlen, ESMCI_FortranStrLenArg llen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, 
      "- System does not support dynamic loading.", ESMC_CONTEXT, rc);
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
        "shared object not found", ESMC_CONTEXT, rc);
      return;
    }
    string routine(routineArg, rlen);
    routine.resize(routine.find_last_not_of(" ")+1);
    void (*func)() = (void (*)())dlsym(lib, routine.c_str());
    if ((void *)func == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "routine not found", ESMC_CONTEXT, rc);
      return;
    }
    ESMCI::FTable::setVM(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
#endif
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservices"
  void FTN_X(c_esmc_setservices)(void *ptr, void (*func)(), int *userRc, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setServices(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservicesshobj"
  void FTN_X(c_esmc_setservicesshobj)(void *ptr, char const *routineArg, 
    char const *sharedObjArg, ESMC_Logical *foundRoutine, int *userRc, int *rc,
    ESMCI_FortranStrLenArg rlen, ESMCI_FortranStrLenArg llen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
#ifdef ESMF_NO_DLFCN
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, 
      "- System does not support dynamic loading.", ESMC_CONTEXT, rc);
    return;
#else
    *foundRoutine = ESMF_FALSE; // initialize
    void *lib;
    if (llen>0){
      string sharedObj(sharedObjArg, llen);
      sharedObj.resize(sharedObj.find_last_not_of(" ")+1);
      lib = dlopen(sharedObj.c_str(), RTLD_LAZY);
    }else
      lib = dlopen(NULL, RTLD_LAZY);  // search in executable
    if (lib == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "shared object not found", ESMC_CONTEXT, rc);
      return;
    }
    string routine(routineArg, rlen);
    routine.resize(routine.find_last_not_of(" ")+1);
    void (*func)() = (void (*)())dlsym(lib, routine.c_str());
    if ((void *)func != NULL){
      // Routine was found
      *foundRoutine = ESMF_TRUE;
      ESMCI::FTable::setServices(ptr, func, userRc, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc)) return;
    }
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
#endif
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservicescomp"
  void FTN_X(c_esmc_setservicescomp)(ESMCI::Comp *dualComp, 
    ESMCI::CompTunnel **compTunnel, ESMCI::Comp *localActualComp, 
    int *localActualCompRootPet, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*compTunnel != NULL){
      delete *compTunnel; // clean-up
      *compTunnel = NULL; // mark clean
    }
    // The compTunnel object must be created on all the PETs that execute
    // this routine, which are potentially all parent PETs, not just those
    // that eventually enter the child VM. This is so that all child component
    // objects that exist on the parent VM with a valid entry of a child VM
    // also have a valid compTunnel member.
    *compTunnel = new ESMCI::CompTunnel(localActualComp,
      *localActualCompRootPet);
    if (*compTunnel == NULL){
      ESMC_LogDefault.MsgAllocError("- CompTunnel allocation", ESMC_CONTEXT, rc);  
      return; // bail out
    }
    // call into setServices with the internal CompTunnel::SetServices wrapper
    int userRc;
    localrc =dualComp->setServices(ESMCI::CompTunnel::setServicesWrap, &userRc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      delete *compTunnel; // clean-up
      *compTunnel = NULL; // mark clean
      return; // bail out
    }
    if (ESMC_LogDefault.MsgFoundError(userRc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      delete *compTunnel; // clean-up
      *compTunnel = NULL; // mark clean
      return; // bail out on userRc b/c setServicesWrap is an internal routine
    }
    
    // Now that everything has returned successfully, mark the tunnel as
    // connected.
    // This must be done up on this level, so that _all_ PETs that call into
    // SetServices() have the tunnel set to connected.
    (*compTunnel)->setConnected(true);
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setservicessock"
  void FTN_X(c_esmc_setservicessock)(ESMCI::Comp *dualComp, 
    ESMCI::CompTunnel **compTunnel, int *port, char const *serverArg,
    int *timeout, int *rc, ESMCI_FortranStrLenArg len){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    if (*compTunnel != NULL){
      delete *compTunnel; // clean-up
      *compTunnel = NULL; // mark clean
    }
    if (*port<1024 || *port>65535){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "- The port argument is outside valid range [1024, 65535]",
        ESMC_CONTEXT, rc);
      return;
    }
    // server name
    string server(serverArg, len);
    // The compTunnel object must be created on all the PETs that execute
    // this routine, which are potentially all parent PETs, not just those
    // that eventually enter the child VM. This is so that all child component
    // objects that exist on the parent VM with a valid entry of a child VM
    // also have a valid compTunnel member.
    *compTunnel = new ESMCI::CompTunnel(*port, server);
    if (*compTunnel == NULL){
      ESMC_LogDefault.MsgAllocError("- CompTunnel allocation", ESMC_CONTEXT, rc);  
      return; // bail out
    }
    (*compTunnel)->setTimeout(*timeout);  // set dual side timeout for setServ.
    // call into setServices with the internal CompTunnel::SetServices wrapper
    int userRc;
    localrc =dualComp->setServices(ESMCI::CompTunnel::setServicesWrap, &userRc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      delete *compTunnel; // clean-up
      *compTunnel = NULL; // mark clean
      return; // bail out
    }
    // CompTunnel::setServicesWrap() is a framework internal method, therefore
    // the code returned in userRc is a framework internal return code and must
    // be treated as such. 
    // Do not filter the RC_TIMEOUT at this level, since a timeout needs to
    // bail out until it gets to the upper ESMF level, right before returning
    // to the user, where the filtering is done according to the presence of
    // a timeoutFlag argument.
    if (ESMC_LogDefault.MsgFoundError(userRc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      delete *compTunnel; // clean-up
      *compTunnel = NULL; // mark clean
      return; // bail out on userRc b/c setServicesWrap is an internal routine
    }
    
    // Now that everything has returned successfully, mark the tunnel as
    // connected.
    // This must be done up on this level, so that _all_ PETs that call into
    // SetServices() have the tunnel set to connected.
    (*compTunnel)->setConnected(true);
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setentrypoint"
  void FTN_X(c_esmc_setentrypoint)(void *ptr, enum ESMCI::method *method,
    void *func, int *phase, int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    
    char const *methodString = ESMCI::FTable::methodString(*method);
    
    int slen = strlen(methodString);
    char *fname;
    ESMCI::FTable::newtrim(methodString, slen, phase, NULL, &fname);
         
    ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
    localrc = (tabptr)->setFuncPtr(fname, func, ESMCI::FT_VOIDP4INTP);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    
    delete[] fname;  // delete memory that "newtrim" allocated above
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getentrypointphasecount"
  void FTN_X(c_esmc_getentrypointphasecount)(void *ptr,
    enum ESMCI::method *method, int *phaseCount, ESMC_Logical *phaseZeroFlag,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    
    char const *methodString = ESMCI::FTable::methodString(*method);
    
    ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
    
    int slen = strlen(methodString);
    int phase = 0;  // initialize
    int i;
    
    *phaseZeroFlag = ESMF_FALSE; // initialize
    
    char *fname;
    ESMCI::FTable::newtrim(methodString, slen, &phase, NULL, &fname);
    i = tabptr->getEntry(fname, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out
    delete[] fname;  // delete memory that "newtrim" allocated above
    if (i != -1)   
      *phaseZeroFlag = ESMF_TRUE;  // set the flag

    do{
      ++phase;
      ESMCI::FTable::newtrim(methodString, slen, &phase, NULL, &fname);
      
      i = tabptr->getEntry(fname, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return; // bail out
      
      delete[] fname;  // delete memory that "newtrim" allocated above
      
    }while (i != -1);
    
    *phaseCount = phase - 1;
    
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
  void FTN_X(esmf_gridcompsetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_gridcompgetinternalstate"
  void FTN_X(esmf_gridcompgetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::getDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // ---------- CplComp ---------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_cplcompsetinternalstate"
  void FTN_X(esmf_cplcompsetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_cplcompgetinternalstate"
  void FTN_X(esmf_cplcompgetinternalstate)(ESMCI::FTable ***ptr, void **datap,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::getDP(ptr, datap, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

  // ---------- UserComp ---------------
#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompsetvm"
  void FTN_X(esmf_usercompsetvm)(void *ptr, void (*func)(), int *userRc,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setVM(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompsetservices"
  void FTN_X(esmf_usercompsetservices)(void *ptr, void (*func)(), int *userRc,
    int *rc){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::FTable::setServices(ptr, func, userRc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompsetinternalstate"
  void FTN_X(esmf_usercompsetinternalstate)(ESMCI::FTable ***ptr,
    char const *name, void **datap, int *rc,
    ESMCI_FortranStrLenArg slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "null pointer found", ESMC_CONTEXT, rc);
      return;
    }

    char *tbuf; 
    ESMCI::FTable::newtrim(name, slen, NULL, NULL, &tbuf);
    //printf("after newtrim, name = '%s'\n", tbuf);

    enum ESMCI::dtype dtype = ESMCI::DT_FORTRAN_UDT_POINTER;
    localrc = (**ptr)->setDataPtr(tbuf, datap, dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
  
    delete[] tbuf;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "esmf_usercompgetinternalstate"
  void FTN_X(esmf_usercompgetinternalstate)(ESMCI::FTable ***ptr,
    char const *name, void **datap, int *rc,
    ESMCI_FortranStrLenArg slen){
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    if ((ptr == ESMC_NULL_POINTER) || (*ptr == ESMC_NULL_POINTER)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
        "null pointer found", ESMC_CONTEXT,rc);
      return;
    }

    char *tbuf; 
    ESMCI::FTable::newtrim(name, slen, NULL, NULL, &tbuf);
    //printf("after newtrim, name = '%s'\n", tbuf);

    enum ESMCI::dtype dtype;
    localrc = (**ptr)->getDataPtr(tbuf, datap, &dtype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
  
    delete[] tbuf;
    
    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
  }

} // extern "C"
//==============================================================================


//==============================================================================
// VM-enabled CallBack loop     
extern "C" {
     
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_FTableCallEntryPointVMHop"
void *ESMCI_FTableCallEntryPointVMHop(void *vm, void *cargoCast){
  // This routine is the first level that gets instantiated in new VM
  // The first argument must be of type (void *) and points to a derived
  // ESMCI::VMK class object. The second argument is also of type (void *)
  // and points to a cargotype structure.
  
  // pull out info from cargo
  ESMCI::cargotype *cargo = (ESMCI::cargotype *)cargoCast;
  char *name = cargo->name;               // name of callback
  ESMCI::FTable *ftable = cargo->ftable;  // ptr to ftable
  
  int localrc;          // local return code
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
  
  // get a pointer to the CompTunnel object
  ESMCI::Comp *f90comp = cargo->f90comp;
  ESMCI::CompTunnel *compTunnel;
  localrc = f90comp->getTunnel(&compTunnel);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &esmfrc)){
    cargo->esmfrc[mytid] = esmfrc;
    return NULL;
  }

  // determine whether this is a dual component that is ready to execute
  bool dualConnected = false;  // initialize
  if (compTunnel) dualConnected = compTunnel->isConnected();
  
  if (dualConnected){
    // this is a dual component with a compTunnel that is connected

    //TODO: check whether "name" is found in dual components ftable. If so then
    //TODO: consider that an override, and execute dual components method
    //TODO: instead
    
    localrc = compTunnel->execute(cargo);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &esmfrc)){
      cargo->esmfrc[mytid] = esmfrc;  // put esmf return code into cargo
      return NULL;
    }
    
    // ...the user return code will not be available until wait() is called
    
  }else{
    // a regular component or a dual component that needs to connect still,
    // use the local ftable for user code or system code callback
    localrc = ftable->callVFuncPtr(name, (ESMCI::VM*)vm, &userrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &esmfrc)){
      cargo->esmfrc[mytid] = esmfrc;  // put esmf return code into cargo
      return NULL;
    }
    
    // ...back from user code
    cargo->userrc[mytid] = userrc;  // put the user return code into cargo 
  }
      
  // return successfully
  cargo->esmfrc[mytid] = ESMF_SUCCESS;
  return NULL;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// call a function through VM
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_ftablecallentrypointvm"
void FTN_X(c_esmc_ftablecallentrypointvm)(
  ESMCI::Comp *f90comp,       // pointer to Fortran component object
  ESMCI::VM **ptr_vm_parent,  // p2 to the parent VM
  ESMCI::VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
  void **vm_info,             // p2 to member which holds info returned by enter
  void **vm_cargo,            // p2 to member which holds cargo
  ESMCI::FTable **ptr,        // p2 to the ftable of this component
  enum ESMCI::method *method, // method type
  int *phase,                 // phase selector
  int *port,                  // port number
  int *timeout,               // time out in seconds
  int *recursionCount,        // keeping track of recursion level of component
  int *rc                     // return code
  ){
       
  // local variables
  int localrc;              // local return code
  char *name;               // trimmed type string
  
  // check to make sure VM has really been started up for this Component
  if (*vm_info == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL, 
      "No VM was started for this Component - missing SetServices() call?",
      ESMC_CONTEXT, rc);
    return; // bail out
  }

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  char const *methodString = ESMCI::FTable::methodString(*method);

  int slen = strlen(methodString);
  ESMCI::FTable::newtrim(methodString, slen, phase, NULL, &name);

  // dereference double pointers to pointers
  ESMCI::VM *vm_parent  = *ptr_vm_parent;     // pointer to parent VM
  ESMCI::VMPlan *vmplan = *ptr_vmplan;        // pointer to VMPlan
  ESMCI::FTable *ftable = *ptr;               // pointer to function table
  
  int i = ftable->getEntry(name, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return; // bail out
  enum ESMCI::method currentMethod = ESMCI::METHOD_NONE;  // default invalid
  if (i > -1)
    currentMethod = ftable->methodFromIndex(i);
  
  // support for recursion _and_ re-entrance for non-blocking mode
  // - recursion:   A component may call into any of its standard methods from 
  //                within the context of already executing a standard method.
  //                There is no practical limit to the recursion depth.
  // - re-entrance: This refers to the situation where there is an outstanding
  //                non-blocking call, but the parent calls into the standard
  //                child component method again. This really only happens
  //                on the dual side of dual-actual pairs connected by a 
  //                component tunnel. The dual side then may re-enter with an
  //                associated wait call.
  bool newCargoFlag = true;   // initialize
  if (recursionCount && *recursionCount==0 && *vm_cargo)
    newCargoFlag = false; // this is not a recursion but a re-entrance
  
  if (newCargoFlag){
    ESMCI::cargotype *cargo = new ESMCI::cargotype;
    strcpy(cargo->name, name);    // copy trimmed type string
    cargo->f90comp = f90comp;     // pointer to Fortran component
    cargo->ftable = ftable;       // pointer to function table
    cargo->rcCount = 1;           // default
    cargo->esmfrc = new int[1];
    cargo->userrc = new int[1];
    cargo->esmfrc[0] = ESMF_SUCCESS;  // initialize return code to SUCCESS
    cargo->userrc[0] = ESMF_SUCCESS;  // initialize user return code to SUCCESS
    cargo->previousCargo = *vm_cargo; // support recursion
    cargo->previousParentFlag = vmplan->parentVMflag;  // support threaded rec.
    cargo->currentMethod = currentMethod;
    if (*method == ESMCI::METHOD_SERVICELOOP){
      // for serviceloop method the currentPhase carries the port argument
      cargo->currentPhase = *port;
    }else{
      // for all other methods the currentPhase is the current phase
      if (phase)
        cargo->currentPhase = *phase;
      else
        cargo->currentPhase = 1;    // default
    }
    
    // store pointer to the cargo structure
    *vm_cargo=(void*)cargo;
  
    if (cargo->previousCargo != NULL){
      // this is a recursive method invocation
      // use the parentVMflag to indicate running in already existing child VM
      vmplan->parentVMflag = 1; 
    }
  }
  delete[] name;  // delete memory that "newtrim" allocated above
  
  // store the current timeout in the cargo structure
  ESMCI::cargotype *cargo = (ESMCI::cargotype *)*vm_cargo;
  if (timeout)
    cargo->timeout = *timeout;
  else
    cargo->timeout = -1;          // indicate invalid timeout

  // increment recursionCount before entering child VM
  if (recursionCount) (*recursionCount)++;

  // enter the child VM -> resurface in ESMCI_FTableCallEntryPointVMHop()
  localrc = vm_parent->enter(vmplan, *vm_info, *vm_cargo);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return; // bail out
        
  // ... if the child VM uses threads (multi-threading or single-threading) 
  // then this parent PET continues running concurrently to the child PET in the
  // same VAS! In that case the return codes in cargo are not valid here!
  // The status returned by VM::enter() indicates that success of entering the
  // child VM, not failure or success of the callback.
  // The return code of the callback code will be valid in all cases (threading
  // or no threading) _after_ VMK::exit() returns.
  
  // decrement recursionCount again
  if (recursionCount) (*recursionCount)--;
  
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_compwait"
void FTN_X(c_esmc_compwait)(
  ESMCI::VM **ptr_vm_parent,  // p2 to the parent VM
  ESMCI::VMPlan **ptr_vmplan, // p2 to the VMPlan for component's VM
  void **vm_info,             // p2 to member which holds info
  void **vm_cargo,            // p2 to member which holds cargo
  int *timeout,               // time out in seconds
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
  
  // return with errors if there is no cargo to obtain error codes
  if (cargo == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - No cargo structure to obtain error codes", ESMC_CONTEXT, rc);
    return;
  }

  // get a pointer to the CompTunnel object
  ESMCI::Comp *f90comp = cargo->f90comp;
  ESMCI::CompTunnel *compTunnel;
  localrc = f90comp->getTunnel(&compTunnel);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

  // determine whether this is a dual component that is ready to execute
  bool dualConnected = false;  // initialize
  if (compTunnel) dualConnected = compTunnel->isConnected();
  
  if (dualConnected){
    // this is a dual component with a compTunnel that is connected
    // -> execute compTunnel::wait() on dual components VM.
    
    if (timeout == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        " - Must provide valid timeout argument", ESMC_CONTEXT, rc);
      return;
    }
    
    ESMCI::State *is = compTunnel->getImportState();
    ESMCI::State *es = compTunnel->getImportState();
    ESMCI::Clock **clock = compTunnel->getClock();

    ESMCI::Clock *clockP = NULL; // default initialize
    if (clock) clockP = *clock;
    
    int userRc = ESMC_RC_NOT_IMPL;
    
    localrc = f90comp->execute(ESMCI::METHOD_WAIT, is, es, clockP, 
      ESMF_NONBLOCKING,       // nonblocking or else endless recursion!!!
      1, *timeout, &userRc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // TODO: in case of threading the userRc in not immediatly available!
    if (ESMC_LogDefault.MsgFoundError(userRc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out too because this is for an internal routine
  }
  
  // Now call the vmk_exit function which will block respective PETs
  vm_parent->exit(static_cast<ESMCI::VMKPlan *>(vmplan), *vm_info);
  
  // obtain return codes out of cargo
  //TODO: deal with multiple return codes coming back for multi-threaded VMs
  localrc = cargo->esmfrc[0];
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  if (userrc) *userrc = cargo->userrc[0];
  
  // delete cargo structure and handle recursion
  delete [] cargo->esmfrc;
  delete [] cargo->userrc;
  *vm_cargo = cargo->previousCargo; // bring back previous cargo structure
  vmplan->parentVMflag = cargo->previousParentFlag;   // previous value
  delete cargo;
  
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------
  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_compget"
void FTN_X(c_esmc_compget)(
  void **vm_cargo,            // p2 to member which holds cargo
  enum ESMCI::method *method, // method type
  int *phase,                 // phase selector
  int *timeout,               // timeout
  int *rc){                   // esmf internal return error code

  // initialize the return codes
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  ESMCI::cargotype *cargo = (ESMCI::cargotype *)*vm_cargo;
  
  if (cargo){
    *method = cargo->currentMethod;
    *phase = cargo->currentPhase;
    *timeout = cargo->timeout;
  }else{
    *method = ESMCI::METHOD_NONE;
    *phase = 0;
    *timeout = 0;
  }

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

  
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, rc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, rc);
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
      FTN_X(f_esmf_fortranudtpointercopy)((void *)datap, data[i].dataptr);
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
    FTN_X(f_esmf_fortranudtpointersize)(&datumSize);
    data[datacount].dataptr = (void *)new char[datumSize];
    FTN_X(f_esmf_fortranudtpointercopy)(data[datacount].dataptr, (void *)datap);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", 
      ESMC_CONTEXT, rc);
    return;
  }
  
  // TODO: shouldn't need to expand the table here - should be done inside
  // FTable code on demand.
  ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
  localrc = (tabptr)->extend(32, 2); // room for 32 funcs, 2 data
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

  // Set callback function and arguments
  ESMCI::Comp *f90comp = (ESMCI::Comp *)ptr;
  localrc = (tabptr)->setFuncPtr(methodString(METHOD_SETSERVICES),
    (void *)func, f90comp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

  // time to startup the VM for this component (if not already started)...
  ESMCI::VM *vm_parent;
  localrc = f90comp->getVmParent(&vm_parent);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  ESMCI::VMPlan *vmplan_p;
  localrc = f90comp->getVmPlan(&vmplan_p);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  void *vm_info;
  localrc = f90comp->getVmInfo(&vm_info);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  if (vm_info==NULL){
    // VM for this component has not been started yet
    vm_info = vm_parent->startup(vmplan_p,
      ESMCI_FTableCallEntryPointVMHop, NULL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // keep vm_info in a safe place (in parent component) 'till it's used again
    FTN_X(f_esmf_compsetvminfo)(f90comp, &vm_info, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
  }
  // ...now the component's VM is started up and placed on hold.
  
  // reset a flag in the component structure
  FTN_X(f_esmf_compresetvmreleased)(f90comp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  
  // call into register routine using the component's VM
  void *vm_cargo = NULL;
  enum method reg = METHOD_SETSERVICES;
  FTN_X(c_esmc_ftablecallentrypointvm)(f90comp, &vm_parent, &vmplan_p, &vm_info,
    &vm_cargo, &tabptr, &reg, NULL, NULL, NULL, NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  
  // wait for the register routine to return
  FTN_X(c_esmc_compwait)(&vm_parent, &vmplan_p, &vm_info, &vm_cargo, NULL,
    userRc, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, rc);
    return;
  }

  // cast into ESMCI::Comp pointer
  ESMCI::Comp *f90comp = (ESMCI::Comp *)ptr;

  // check to see if VM already exists for this Component
  void *vm_info;
  localrc = f90comp->getVmInfo(&vm_info);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;
  if (vm_info!=NULL){
    // VM for this component already exists
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID, "- VM already exists",
      ESMC_CONTEXT, rc);
    return;
  }

  // TODO: shouldn't need to expand the table here - should be done inside
  // FTable code on demand.
  ESMCI::FTable *tabptr = **(ESMCI::FTable***)ptr;
  localrc = (tabptr)->extend(32, 2); // room for 32 funcs, 2 data
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

  // Set callback function and arguments
  localrc = (tabptr)->setFuncPtr("setVM", (void *)func, f90comp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

  // Call into user code callback function
  localrc = (tabptr)->callVFuncPtr("setVM", NULL, userRc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;

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
//  Returns the index into the function table that matches "name". A linear
//  search is used, which is sufficient for typically very small function
//  table sizes.
//  One added feature this look-up routine also provides is a secondary
//  search for the actual method name in case that "name" was _not_ found,
//  _and_ "name" contained the substring "IC" which indicates that this
//  would have been a look-up for an interface component method.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize rc and localrc ; assume functions not implemented
  if (*rc) *rc = ESMC_RC_NOT_IMPL;

  // Check input
  if (name == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, rc);
    return -1;  // indicate entry not found
  }

  int i;
  for (i=0; i<funccount; i++) {
    if (!strcmp(name, funcs[i].funcname))
      break;
  }
  if (i==funccount) i=-1; // indicate entry not found
  
//if (i==-1)
//printf("gjt: failed attempt to look up method: %s\n", name);

  if (i == -1){
    // name was not found in function table -> check if name contains "IC"
    char const *ic = strstr(name, "IC");
    if (ic){
      // this was a failed attempt look up interface component method
      // -> try actual component method instead
      int len = strlen(name);
      char *tempname = new char[len];
      char *b = tempname;
      char const *a = name;
      for (int l=0; l<len+1; l++){
        if (a!=ic && a!=ic+1){
          *b = *a;
          b++;
        }
        a++;
      }
//printf("gjt: failed attempt to look up IC method -> try actual method: %s\n",
//tempname);
      
      for (i=0; i<funccount; i++) {
        if (!strcmp(tempname, funcs[i].funcname))
          break;
      }
      if (i==funccount) i=-1; // indicate entry not found
      delete [] tempname;
//if (i==-1)
//printf("gjt: failed 2nd attempt to look up method: %s\n", name);
    }
  }
  
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found", 
      ESMC_CONTEXT, &rc);
    return rc;
  }
  if (func == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, &rc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, &rc);
    return rc;
  }
  if (func == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, &rc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, &rc);
    return rc;
  }
  if (func == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, &rc);
    return rc;
  }
  if (arg == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, &rc);
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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc; // bail out
  
  if (i == -1){
    char msg[80];
    sprintf(msg, "unknown function name: %s", name);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, msg, ESMC_CONTEXT, &rc);
    return rc; // bail out
  }

  // Check arglist argument
  if (arglist == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, "null pointer found",
      ESMC_CONTEXT, &rc);
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
  int *userrc) {        // out, function return code
//
// !DESCRIPTION:
//    Calls the named function pointer
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // sanity check userrc
  if (!userrc){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- userrc must not be NULL pointer!", ESMC_CONTEXT, &rc);
    return rc; // bail out
  }

  // try to find "name" entry in single FTable instance on parent PET
  int i = getEntry(name, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc; // bail out
  
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
        "unknown function name", ESMC_CONTEXT, &rc);
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
        FTN_X(f_esmf_comprefcopy)(comp, (Comp *)(func->funcarg[0]), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc; // bail out
        // insert child VM
        FTN_X(f_esmf_compinsertvm)(comp, vm, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc; // bail out
      }else{
        // slave thread -> replicate component structure, insert child VM
        vm_pointer->threadbarrier();  // synchronize with master thread
        // replicate Component from the parent PET w/ private FTable, insert VM
        comp = component + mytid; // determine PET-local component
        FTN_X(f_esmf_compreplicate)(comp, (Comp *)(func->funcarg[0]), vm,
          &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc; // bail out
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
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc; // bail out
      if (j == -1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "unknown function name", ESMC_CONTEXT, &rc);
        return rc; // bail out
      }
      func = ftable->funcs + j; // determine PET-local function entry
    }else{
      // fatal inconsistent situation
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "fatal inconsistency between Component and VM", ESMC_CONTEXT, &rc);
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
      VoidP1IntPFunc vf = (VoidP1IntPFunc)func->funcptr;
      (*vf)((void *)comp, userrc);
      // conditionally set entry point for ServiceLoop
      if (!strcmp(name, methodString(METHOD_SETSERVICES))){
        localrc = comp->setEntryPoint(METHOD_SERVICELOOP, ServiceLoop);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc; // bail out
      }      
      // conditionally call into compliance IC for register
      if (!strcmp(name, methodString(METHOD_SETSERVICES))){
        char const *envVar = VM::getenv("ESMF_RUNTIME_COMPLIANCECHECK");
        bool complianceCheckFlag = false;  // default internal compl. check off
        if (envVar != NULL){
          string value(envVar);
          // see if compliance checker should be turned on
          complianceCheckFlag |= value.find("on")!=string::npos;  // turn on
          complianceCheckFlag |= value.find("ON")!=string::npos;  // turn on
        }
        if (complianceCheckFlag){
          int registerIcUserRc;
          
//TODO: for now disable the DLFCN based lookup until we correctly build PIC
//TODO: on all combos for which we have DLFCN enabled
#define ESMF_NO_DLFCNdummy
          
#ifdef ESMF_NO_DLFCNdummy
          FTN_X(esmf_complianceicregister)((void *)comp, &registerIcUserRc);
#else
          
#define QUOTEMACRO_(x) #x
#define QUOTEMACRO(x) QUOTEMACRO_(x)
          
          envVar = VM::getenv("ESMF_RUNTIME_COMPLIANCEICOBJECT");
          void *lib;
          lib = dlopen(envVar, RTLD_LAZY);  // envVar==NULL -> look into exe
          if (lib == NULL){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
              "- shared object not found", ESMC_CONTEXT, &rc);
            return rc;
          }
          envVar = VM::getenv("ESMF_RUNTIME_COMPLIANCEICREGISTER");
          void *pointer;
          if (envVar != NULL)
            pointer = (void *)dlsym(lib, envVar);
          else
            pointer = (void *)dlsym(lib,
              QUOTEMACRO(FTN(esmf_complianceicregister)) );
          if (pointer == NULL){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
              "- compliance IC register routine not found", ESMC_CONTEXT, &rc);
            return rc;
          }
          
          VoidP1IntPFunc vf = (VoidP1IntPFunc)pointer;
          (*vf)((void *)comp, &registerIcUserRc);
#endif
          
          // compliance IC for register is an internal routine -> look at rc
          if (ESMC_LogDefault.MsgFoundError(registerIcUserRc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc; // bail out
        }
      }
      
      break;
    }
    case FT_VOIDP4INTP: {
      //printf("calling out of case FT_VOIDP4INTP\n");
      VoidP4IntPFunc vf = (VoidP4IntPFunc)func->funcptr;
      (*vf)((void *)comp, func->funcarg[1], func->funcarg[2],
        func->funcarg[3], userrc);
      break;
    }
    default:
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "unknown function type", ESMC_CONTEXT, &rc);
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
    FTN_X(f_esmf_compdelete)(comp, &localrc);
  }
  delete [] component;
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
  if ((phase != NULL) && (phase != (int *)-1) && (*phase >= 0))  {
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


//==============================================================================
char const *FTable::methodString(enum ESMCI::method method){
  switch(method){
  case ESMCI::METHOD_NONE:
    return "None";
    break;
  case ESMCI::METHOD_INITIALIZE:
    return "Initialize";
    break;
  case ESMCI::METHOD_RUN:
    return "Run";
    break;
  case ESMCI::METHOD_FINALIZE:
    return "Finalize";
    break;
  case ESMCI::METHOD_WRITERESTART:
    return "WriteRestart";
    break;
  case ESMCI::METHOD_READRESTART:
    return "ReadRestart";
    break;
  case ESMCI::METHOD_SERVICELOOP:
    return "ServiceLoop";
    break;
  case ESMCI::METHOD_INITIALIZEIC:
    return "InitializeIC";
    break;
  case ESMCI::METHOD_RUNIC:
    return "RunIC";
    break;
  case ESMCI::METHOD_FINALIZEIC:
    return "FinalizeIC";
    break;
  case ESMCI::METHOD_WRITERESTARTIC:
    return "WriteRestartIC";
    break;
  case ESMCI::METHOD_READRESTARTIC:
    return "ReadRestartIC";
    break;
  case ESMCI::METHOD_SERVICELOOPIC:
    return "ServiceLoopIC";
    break;
  case ESMCI::METHOD_SETSERVICES:
    return "Register";
    break;
  case ESMCI::METHOD_WAIT:
    return "Wait";
    break;
  default:
    return "Unknown";
    break;
  }
  return NULL;
}
//==============================================================================

//==============================================================================
enum method FTable::methodFromString(char const *methodString){
  if (!strncmp(methodString, "InitializeIC", strlen("InitializeIC")))
    return ESMCI::METHOD_INITIALIZEIC;
  else if (!strncmp(methodString, "RunIC", strlen("RunIC")))
    return ESMCI::METHOD_RUNIC;
  else if (!strncmp(methodString, "FinalizeIC", strlen("FinalizeIC")))
    return ESMCI::METHOD_FINALIZEIC;
  else if (!strncmp(methodString, "WriteRestartIC", strlen("WriteRestartIC")))
    return ESMCI::METHOD_WRITERESTARTIC;
  else if (!strncmp(methodString, "ReadRestartIC", strlen("ReadRestartIC")))
    return ESMCI::METHOD_READRESTARTIC;
  else if (!strncmp(methodString, "Initialize", strlen("Initialize")))
    return ESMCI::METHOD_INITIALIZE;
  else if (!strncmp(methodString, "Run", strlen("Run")))
    return ESMCI::METHOD_RUN;
  else if (!strncmp(methodString, "Finalize", strlen("Finalize")))
    return ESMCI::METHOD_FINALIZE;
  else if (!strncmp(methodString, "WriteRestart", strlen("WriteRestart")))
    return ESMCI::METHOD_WRITERESTART;
  else if (!strncmp(methodString, "ReadRestart", strlen("ReadRestart")))
    return ESMCI::METHOD_READRESTART;
  else if (!strncmp(methodString, "Register", strlen("Register")))
    return ESMCI::METHOD_SETSERVICES;
  return ESMCI::METHOD_NONE;
}
//==============================================================================

//==============================================================================
enum method FTable::methodFromIndex(int i){
  return methodFromString(funcs[i].funcname);
}
//==============================================================================

} // namespace ESMCI
