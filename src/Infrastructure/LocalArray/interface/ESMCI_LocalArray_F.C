// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_LocalArray_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <cstdio>
#include <cstring>

#include "ESMCI_Macros.h"
#include "ESMCI_LocalArray.h"
#include "ESMCI_LogErr.h"

using namespace std;

//------------------------------------------------------------------------------
//BOPI
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which allows F90 to
// call C++ for supporting {\tt ESMC\_LocalArray} class functions.
//
//EOPI

#ifdef ESMC_DATA_ADDR_NEEDS_INDIR
#define XD *
#else
#define XD
#endif

// the interface subroutine names MUST be in lower case
extern "C" {

  void FTN_X(c_esmc_localarraycreatenodata)(ESMCI::LocalArray **ptr, int *rank,
    ESMC_TypeKind_Flag *tk, ESMCI::LocalArrayOrigin *oflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraycreatenodata()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::LocalArray::create(*tk, *rank, *oflag, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_localarraycreatecopy)(ESMCI::LocalArray **ptr, 
    ESMCI::LocalArray **larrayOut, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraycreatecopy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    // call into C++
    *larrayOut = ESMCI::LocalArray::create(*ptr, ESMCI::DATACOPY_VALUE,
      NULL, NULL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_localarraysetinfo)(ESMCI::LocalArray **ptr, 
    struct ESMCI::c_F90ptr *fptr, void XD *base, int *counts, int *lbounds,
    int *ubounds, int *offsets, ESMC_Logical *contig, ESMC_Logical *dealloc,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraysetinfo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    // ESMC_Logical -> bool casting
    bool dflag;
    bool *df = NULL;
    if (dealloc){
      dflag = (*dealloc == ESMF_TRUE) ? true : false;
      df = &dflag;
    }
    bool cflag;
    bool *cf = NULL;
    if (contig){
      cflag = (*contig == ESMF_TRUE) ? true : false;
      cf = &cflag;
    }
    // call into C++
    //TODO: replace this by multiple set() calls.
    localrc = (*ptr)->setInfo(fptr, XD base, counts, lbounds, ubounds, offsets,
      cf, df);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygetcounts)(ESMCI::LocalArray **ptr, int *counts,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygetcounts()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    int rank = (*ptr)->getRank();
    const int *lcounts = (*ptr)->getCounts();
    for (int i=0; i<rank; i++)
      counts[i] = lcounts[i];
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygetlbounds)(ESMCI::LocalArray **ptr, int *lbounds,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygetlbounds()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    int rank = (*ptr)->getRank();
    const int *llbounds = (*ptr)->getLbounds();
    for (int i=0; i<rank; i++)
      lbounds[i] = llbounds[i];
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygetubounds)(ESMCI::LocalArray **ptr, int *ubounds,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygetubounds()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    int rank = (*ptr)->getRank();
    const int *lubounds = (*ptr)->getUbounds();
    for (int i=0; i<rank; i++)
      ubounds[i] = lubounds[i];
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygetrank)(ESMCI::LocalArray **ptr, int *rank,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygetrank()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    *rank = (*ptr)->getRank();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygettypekind)(ESMCI::LocalArray **ptr,
    ESMC_TypeKind_Flag *typekind, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygettypekind()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    *typekind = (*ptr)->getTypeKind();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraydestroy)(ESMCI::LocalArray **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraydestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    localrc = ESMCI::LocalArray::destroy(*ptr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraysetbaseaddr)(ESMCI::LocalArray **ptr, void XD *base,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraysetbaseaddr()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    (*ptr)->setBaseAddr(XD base);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygetbaseaddr)(ESMCI::LocalArray **ptr, void **base,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygetbaseaddr()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    *base = (void *)(*ptr)->getBaseAddr();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraysetfptr)(ESMCI::LocalArray **ptr,
    struct ESMCI::c_F90ptr *fptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraysetfptr()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    localrc = (*ptr)->setFortranDopev(fptr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygetfptr)(ESMCI::LocalArray **ptr,
    struct ESMCI::c_F90ptr *fptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygetfptr()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    localrc = (*ptr)->getFortranDopev(fptr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarrayforcefptr)(ESMCI::LocalArray **ptr, void XD *base,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarrayforcefptr()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    localrc = (*ptr)->forceFortranPtr(XD base);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraysetdealloc)(ESMCI::LocalArray **ptr,
    ESMC_Logical *dealloc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraysetdealloc()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    if (*dealloc == ESMF_TRUE)
      (*ptr)->setDealloc(true);
    else
      (*ptr)->setDealloc(false);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraygetdealloc)(ESMCI::LocalArray **ptr,
    ESMC_Logical *dealloc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraygetdealloc()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    if ((*ptr)->getDealloc())
      *dealloc = ESMF_TRUE;
    else
      *dealloc = ESMF_FALSE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarrayprint)(ESMCI::LocalArray **ptr, char *opts, int *rc,
    ESMCI_FortranStrLenArg clen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarrayprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    // make a local copy because opts may be non-writable or not
    // long enough to add a trailing null safely.
    char *temp = NULL;
    if (opts && (clen > 0)) {
      temp = new char[clen+1];
      strncpy(temp, opts, clen);
      temp[clen] = '\0';
    }
    localrc = (*ptr)->print(temp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    if (temp) delete[] temp;
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_localarraywrite)(ESMCI::LocalArray **ptr, char *opts,
    char *fname, int *rc,
    ESMCI_FortranStrLenArg optlen,
    ESMCI_FortranStrLenArg flen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraywrite()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // check input
    if ((ptr == NULL) || (*ptr == NULL)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to LocalArray object", ESMC_CONTEXT, rc);
      return;
    }
    // make a local copy because opts may be non-writable or not
    // long enough to add a trailing null safely.
    char *opttemp = NULL;
    char *filetemp = NULL;
    if (opts && (optlen > 0)) {
      opttemp = new char[optlen+1];
      strncpy(opttemp, opts, optlen);
      opttemp[optlen] = '\0';
    }
    if (fname && (flen > 0)) {
      filetemp = new char[flen+1];
      strncpy(filetemp, fname, flen);
      filetemp[flen] = '\0';
    }
    localrc = (*ptr)->write(opttemp, filetemp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    if (opttemp) delete[] opttemp;
    if (filetemp) delete[] filetemp;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmf_f90ptrsizeprint)(void *p1, void *p2, int *rank, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmf_f90ptrsizeprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // compare actual size of Fortran dope vector computed at run time,
    // vs. the compile-time memory allocation in the LocalArray object.
    // -> only used in ESMF_F90ArrayPtrUTest.F90 for sanity check
    int rsize = (int)((char*)p2 - (char*)p1);
    int fixed = ESMF_FPTR_BASE_SIZE + ESMF_MAXDIM * ESMF_FPTR_PLUS_RANK;
      
    if (rsize > fixed) {
      printf("dope vector allocation too small: rank %d\n", *rank);
      printf(" full details: \n");
      printf("  rank %d: compiled-in size is %d bytes, run-time computed"
        " size is %d bytes (diff=%d)\n", *rank, fixed, rsize,
        rsize-fixed);
      if (rc!=NULL) *rc = ESMF_FAILURE;
    }else{
      printf("dope vector allocation is sufficient: rank %d %d bytes <= "
        "%d bytes\n", *rank, rsize, fixed);
      if (rc!=NULL) *rc = ESMF_SUCCESS;
    }
  }

}


