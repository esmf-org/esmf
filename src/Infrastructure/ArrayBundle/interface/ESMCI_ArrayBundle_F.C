// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_ArrayBundle_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_RHandle.h"
#include "ESMCI_Array.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_ArrayBundle.h"

#include <vector>
#include <string>

using std::exception;
using std::string;
using std::vector;

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ArrayBundle} class functions.
//
//EOP
//-------------------------------------------------------------------------

// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:

  void FTN_X(c_esmc_arraybundleadd)(ESMCI::ArrayBundle **ptr, 
    ESMCI::Array **arrayList, int *arrayCount,
    ESMC_Logical *multiflag, ESMC_Logical *relaxedflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleadd()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    bool multi;
    if (*multiflag == ESMF_TRUE)
      multi=true;
    else
      multi=false;

    bool relaxed;
    if (*relaxedflag == ESMF_TRUE)
      relaxed=true;
    else
      relaxed=false;

    // call into C++
    try{
      
      for (int i=0; i<*arrayCount; i++){
        // call into C++ layer
        (*ptr)->add(arrayList[i], multi, relaxed);
      }

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_arraybundleaddreplace)(ESMCI::ArrayBundle **ptr, 
    ESMCI::Array **arrayList, int *arrayCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleaddreplace()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    try{
      
      for (int i=0; i<*arrayCount; i++){
        // call into C++ layer
        (*ptr)->addReplace(arrayList[i]);
      }

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_arraybundlecreate)(ESMCI::ArrayBundle **ptr, 
    ESMCI::Array **arrayList, int *arrayCount,
    ESMC_Logical *multiflag, ESMC_Logical *relaxedflag, 
    char *name, int *len_name, int *rc,
    ESMCI_FortranStrLenArg name_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlecreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    bool multi;
    if (*multiflag == ESMF_TRUE)
      multi=true;
    else
      multi=false;

    bool relaxed;
    if (*relaxedflag == ESMF_TRUE)
      relaxed=true;
    else
      relaxed=false;

    // call into C++
    *ptr = ESMCI::ArrayBundle::create(arrayList, *arrayCount, multi, relaxed,
      &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set the name in the ArrayBundle object
    char *cname = ESMC_F90toCstring(name, *len_name);
    if (cname){
      (*ptr)->setName(cname);
      delete [] cname;
    }else if(*len_name){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid string", ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_arraybundledestroy)(ESMCI::ArrayBundle **ptr, 
    ESMC_Logical *noGarbage, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundledestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool noGarbageOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(noGarbage) != ESMC_NULL_POINTER)
      if (*noGarbage == ESMF_TRUE) noGarbageOpt = true;
    // call into C++
    ESMC_LogDefault.MsgFoundError(
      ESMCI::ArrayBundle::destroy(ptr, noGarbageOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_arraybundlegetlist)(ESMCI::ArrayBundle **ptr,
    char *arrayName, int *arrayCount, ESMCI::Array **opt_arrayList, 
    int *len_arrayList, ESMC_ItemOrder_Flag *itemorderflag, int *rc,
    ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlegetlist()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // fill simple return values
    *arrayCount = (*ptr)->getCount(std::string(arrayName, nlen));
    // fill arrayList
    if (*len_arrayList != 0){
      // opt_arrayList was provided
      if (*len_arrayList < *arrayCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- opt_arrayList must provide arrayCount elements", ESMC_CONTEXT, rc);
        return;
      }
      // opt_arrayList has correct number of elements
      vector<ESMCI::Array *> arrayVector;
      (*ptr)->get(std::string(arrayName, nlen), arrayVector, *itemorderflag);
      for (int i=0; i<*arrayCount; i++)
        opt_arrayList[i] = arrayVector[i];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_arraybundlegetlistall)(ESMCI::ArrayBundle **ptr,
    int *arrayCount, ESMCI::Array **opt_arrayList, int *len_arrayList, 
    ESMC_ItemOrder_Flag *itemorderflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlegetlistall()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // fill simple return values
    if (ESMC_NOT_PRESENT_FILTER(arrayCount) != ESMC_NULL_POINTER)
      *arrayCount = (*ptr)->getCount();
    // fill arrayList
    if (*len_arrayList != 0){
      // opt_arrayList was provided
      if (*len_arrayList < (*ptr)->getCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- opt_arrayList must provide arrayCount elements", ESMC_CONTEXT, rc);
        return;
      }
      // opt_arrayList has correct number of elements
      vector<ESMCI::Array *> arrayVector;
      (*ptr)->getVector(arrayVector, *itemorderflag);
      for (int i=0; i<(*ptr)->getCount(); i++)
        opt_arrayList[i] = arrayVector[i];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_arraybundlegetitem)(ESMCI::ArrayBundle **ptr, char *arrayName,
    ESMCI::Array **array, int *rc, ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlegetitem()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    try{

      // query the C++ layer
      *array = (*ptr)->get(std::string(arrayName, nlen));

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_arraybundlegetispresent)(ESMCI::ArrayBundle **ptr,
    char *arrayName, ESMC_Logical *isPresent, int *rc,
    ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlegetispresent()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    try{

      // query the C++ layer
      if ((*ptr)->isPresent(std::string(arrayName, nlen)))
        *isPresent = ESMF_TRUE;
      else
        *isPresent = ESMF_FALSE;

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_arraybundlegetcount)(ESMCI::ArrayBundle **ptr,
    char *arrayName, int *arrayCount, int *rc, ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlegetcount()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    try{

      // query the C++ layer
      *arrayCount = (*ptr)->getCount(std::string(arrayName, nlen));

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_arraybundlehalostore)(ESMCI::ArrayBundle **arraybundle,
    ESMCI::RouteHandle **routehandle,
    ESMC_HaloStartRegionFlag *halostartregionflag,
    ESMCI::InterArray<int> *haloLDepth, ESMCI::InterArray<int> *haloUDepth,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlehalostore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::haloStore(
      *arraybundle, routehandle, *halostartregionflag, haloLDepth,
      haloUDepth),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundlehalo)(ESMCI::ArrayBundle **arraybundle,
    ESMCI::RouteHandle **routehandle, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlehalo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool checkflagOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(checkflag) != ESMC_NULL_POINTER)
      if (*checkflag == ESMF_TRUE) checkflagOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::halo(
      *arraybundle, routehandle, checkflagOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundleprint)(ESMCI::ArrayBundle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->print(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
  }

  void FTN_X(c_esmc_arraybundlewrite)(ESMCI::ArrayBundle **bundle,
                                      const char *file,
                                      const char *conv,
                                      const char *purp,
                                      ESMC_Logical *opt_singleFile,
                                      ESMC_Logical *opt_overwriteflag,
                                      ESMC_FileStatus_Flag *status,
                                      int *timeslice,
                                      ESMC_IOFmt_Flag *iofmt, int *rc,
                                      ESMCI_FortranStrLenArg file_l,
                                      ESMCI_FortranStrLenArg conv_l,
                                      ESMCI_FortranStrLenArg purp_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlewrite()"
    bool singleFile;
    bool overwriteflag;
    // Initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    // The Fortran interface always sets the flags and optional variables
    // except for timeslice. For character variables, create a C++ string.
    // helper variable
    string fileName = string (file, ESMC_F90lentrim (file, file_l));

    string convention, purpose;
    if (conv)
      convention = string (conv, ESMC_F90lentrim (conv, conv_l));
    if (purp)
      purpose    = string (purp, ESMC_F90lentrim (purp, purp_l));

    if (ESMC_NOT_PRESENT_FILTER(opt_singleFile) != ESMC_NULL_POINTER) {
      singleFile = (ESMF_FALSE != *opt_singleFile);
    } else {
      singleFile = true;
    }
    if (ESMC_NOT_PRESENT_FILTER(opt_overwriteflag) != ESMC_NULL_POINTER) {
      overwriteflag = (ESMF_TRUE == *opt_overwriteflag);
    } else {
      overwriteflag = false;
    }
    // Call into the actual C++ method wrapped inside LogErr handling
    localrc = (*bundle)->write(fileName, convention, purpose, &singleFile, &overwriteflag,
                               status, timeslice, iofmt);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundleread)(ESMCI::ArrayBundle **bundle,
    const char *file, ESMC_Logical *opt_singleFile,
    int *timeslice, ESMC_IOFmt_Flag *iofmt, int *rc,
    ESMCI_FortranStrLenArg file_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleread()"
    bool singleFile;
    // Initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    // Create C++ string for string input
    // helper variable
    string fileName = string (file, ESMC_F90lentrim (file, file_l));

    if (ESMC_NOT_PRESENT_FILTER(opt_singleFile) != ESMC_NULL_POINTER) {
      singleFile = (ESMF_FALSE != *opt_singleFile);
    } else {
      singleFile = true;
    }
    // Call into the actual C++ method wrapped inside LogErr handling
    localrc = (*bundle)->read(fileName, &singleFile, timeslice, iofmt);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundlerediststore)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle, 
    ESMCI::InterArray<int> *srcToDstTransposeMap,
    ESMC_TypeKind_Flag *typekind,
    void *factor, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlerediststore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::redistStore(
      *srcArraybundle, *dstArraybundle, routehandle, srcToDstTransposeMap,
      *typekind, factor),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundlerediststorenf)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    ESMCI::InterArray<int> *srcToDstTransposeMap, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlerediststorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::redistStore(
      *srcArraybundle, *dstArraybundle, routehandle, srcToDstTransposeMap),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundleredist)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleredist()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool checkflagOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(checkflag) != ESMC_NULL_POINTER)
      if (*checkflag == ESMF_TRUE) checkflagOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::redist(
      *srcArraybundle, *dstArraybundle, routehandle, checkflagOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundleremove)(ESMCI::ArrayBundle **ptr,
    char *arrayNameList, int *itemCount, ESMC_Logical *multiflag,
    ESMC_Logical *relaxedflag, int *rc, ESMCI_FortranStrLenArg nlen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleremove()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;

    bool multi;
    if (*multiflag == ESMF_TRUE)
      multi=true;
    else
      multi=false;

    bool relaxed;
    if (*relaxedflag == ESMF_TRUE)
      relaxed=true;
    else
      relaxed=false;

    // call into C++
    try{
      
      for (int i=0; i<*itemCount; i++){
        // call into C++ layer
        (*ptr)->remove(std::string(arrayNameList+i*nlen, nlen), multi, relaxed);
      }

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_arraybundlereplace)(ESMCI::ArrayBundle **ptr,
    ESMCI::Array **arrayList, int *itemCount, ESMC_Logical *multiflag, 
    ESMC_Logical *relaxedflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlereplace()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    bool multi;
    if (*multiflag == ESMF_TRUE)
      multi=true;
    else
      multi=false;

    bool relaxed;
    if (*relaxedflag == ESMF_TRUE)
      relaxed=true;
    else
      relaxed=false;
      
    // call into C++
    try{
      
      for (int i=0; i<*itemCount; i++){
        // call into C++ layer
        (*ptr)->replace(arrayList[i], multi, relaxed);
      }

    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN_X(c_esmc_arraybundlesmmstore)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle, 
    ESMC_TypeKind_Flag *typekindFactors, void *factorList, int *factorListCount,
    ESMCI::InterArray<int> *factorIndexList, 
    ESMCI::InterArray<int> *srcTermProcessing, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmmstore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    
    try{
    
    // check argument consistency
    if (*factorListCount > 0){
      // must provide valid factorList and factorIndexList args
      if (!present(factorIndexList)){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
          "- Not a valid pointer to factorIndexList array", ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- factorIndexList array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->extent[0] != 2 && 
        (factorIndexList)->extent[0] != 4){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of factorIndexList array must be of size 2 or 4",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->extent[1] != *factorListCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of factorIndexList does not match factorListCount",
          ESMC_CONTEXT, rc);
        return;
      }
    }
    // prepare SparseMatrix vector
    vector<ESMCI::SparseMatrix<ESMC_I4,ESMC_I4> > sparseMatrix;
    int srcN = (factorIndexList)->extent[0]/2;
    int dstN = (factorIndexList)->extent[0]/2;
    sparseMatrix.push_back(ESMCI::SparseMatrix<ESMC_I4,ESMC_I4>(
      *typekindFactors, factorList,
      *factorListCount, srcN, dstN, (factorIndexList)->array));
    // Call into the actual C++ method wrapped inside LogErr handling
    if (ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::sparseMatMulStore(
      *srcArraybundle, *dstArraybundle, routehandle, sparseMatrix,
      srcTermProcessing),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_arraybundlesmmstorenf)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    ESMCI::InterArray<int> *srcTermProcessing, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmmstorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // prepare empty SparseMatrix vector
    vector<ESMCI::SparseMatrix<ESMC_I4,ESMC_I4> > sparseMatrix;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::sparseMatMulStore(
      *srcArraybundle, *dstArraybundle, routehandle, sparseMatrix,
      srcTermProcessing),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundlesmm)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    ESMC_Region_Flag *zeroflag, ESMC_TermOrder_Flag *termorderflag,
    int *termorderflag_len, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmm()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool checkflagOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(checkflag) != ESMC_NULL_POINTER)
      if (*checkflag == ESMF_TRUE) checkflagOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::sparseMatMul(
      *srcArraybundle, *dstArraybundle, routehandle, *zeroflag, 
      termorderflag, *termorderflag_len, checkflagOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundleserialize)(ESMCI::ArrayBundle **arraybundle, 
    char *buf, int *length, int *offset, ESMC_AttReconcileFlag *attreconflag,
    ESMC_InquireFlag *inquireflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*arraybundle)->serialize(
      buf,length,offset,*attreconflag,*inquireflag),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraybundledeserialize)(ESMCI::ArrayBundle **arraybundle,
    char *buf, int *offset, ESMC_AttReconcileFlag *attreconflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundledeserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    *arraybundle = new ESMCI::ArrayBundle(-1);  // prevent baseID counter incr.
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*arraybundle)->deserialize(
      buf,offset,*attreconflag),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
#undef  ESMC_METHOD
}
