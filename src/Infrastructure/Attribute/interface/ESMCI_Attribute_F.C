// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Attribute method interface (from F90 to C++) file
#define ESMF_FILENAME "ESMCI_Attribute_F.C"

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable 
// interfaces to the C++ Attribute methods.
//
//-----------------------------------------------------------------------------
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include <string.h>
#include <stdlib.h>
#include <algorithm> // std::min()
#include "ESMCI_F90Interface.h"
#include "ESMCI_Macros.h"
#include "ESMCI_Base.h"
#include "ESMCI_LogErr.h"

using std::string;
using std::vector;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes Attribute routine interfaces
//
//

extern "C" {

/*
  void FTN_X(c_esmc_arraycreateallocate)(ESMCI::Array **ptr, 
    ESMCI::ArraySpec *arrayspec, ESMCI::DistGrid **distgrid,
    ESMCI::InterfaceInt **distgridToArrayMap,
    ESMCI::InterfaceInt **computationalEdgeLWidthArg,
    ESMCI::InterfaceInt **computationalEdgeUWidthArg,
    ESMCI::InterfaceInt **computationalLWidthArg,
    ESMCI::InterfaceInt **computationalUWidthArg, 
    ESMCI::InterfaceInt **totalLWidthArg, ESMCI::InterfaceInt **totalUWidthArg,
    ESMC_IndexFlag *indexflag, ESMCI::InterfaceInt **undistLBoundArg,
    ESMCI::InterfaceInt **undistUBoundArg,
    char *name, int *len_name, int *rc,
    ESMCI_FortranStrLenArg name_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreateallocate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::Array::create(arrayspec, *distgrid, *distgridToArrayMap,
      *computationalEdgeLWidthArg, *computationalEdgeUWidthArg,
      *computationalLWidthArg, *computationalUWidthArg, *totalLWidthArg,
      *totalUWidthArg, ESMC_NOT_PRESENT_FILTER(indexflag), NULL,
      *undistLBoundArg, *undistUBoundArg, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set the name in the Array object
    char *cname = ESMC_F90toCstring(name, *len_name);
    if (cname){
      (*ptr)->setName(cname);
      delete [] cname;
    }else if(*len_name){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid string", ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }
*/

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  Attribute object methods
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackget - get an attpack handle
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackget)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackget()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      ESMCI::Attribute **attpack,    // in/out - attpack to return
      int *count,                    // in - number of value(s)
      char *specList,                // in - char string
      int *lens,                     // in - lengths
      ESMC_Logical *present,         // out/out - present flag 
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg slen) { // hidden/in - strlen count for specList
// 
// !DESCRIPTION:
//     Retrieve an attribute package from any Attribute containing object.
//
//EOP

  int status, j;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // allocate space for the array of char*'s and vector of strings
  vector<string> cvalue;
  //cvalue.reserve(*count);
  // hardcode to 4 until we figure out how to deal with arbitrary number of specs
  cvalue.reserve(4);

  // check that valueList is allocated
  if (!specList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList value", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  if (!present) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute present flag", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // loop through valueList allocating space and copying values to cvalue
  j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (j > slen) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad count value", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;

    }

    // copy and convert F90 string to null terminated one
    string temp((&specList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cvalue.push_back(temp);
    j = j + lens[i];
  }

  // TODO: this is just a bandaid for the fact that the attpackinstance name
  //       is not "required" and the C++ routine still requires 4 arguments
  if (*count < 4) {
    string capname;
    cvalue.push_back(capname);
  }
  //TODO: make this more general, for now order is object, convention, purpose, instname
  *attpack = (**base).root.AttPackGet(cvalue[1], cvalue[2], cvalue[0], cvalue[3]);
  if (!(*attpack)) *present = ESMF_FALSE;
  else *present = ESMF_TRUE;

  if (rc) *rc = ESMF_SUCCESS;
  
}  // end c_esmc_attpackget

//BOP
// !IROUTINE:  c_esmc_attpackaddattribute - add an attribute to an attpack
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackaddattribute)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackaddattribute()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
      int *count,                    // in - number of value(s)
      char *specList,                // in - char string
      int *lens,                     // in - lengths
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg nlen,   // hidden/in - strlen count for name
      ESMCI_FortranStrLenArg slen) { // hidden/in - strlen count for specList
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // allocate space for the array of char*'s and vector of strings
  vector<string> cspec;
  cspec.reserve(*count);

  // check that valueList is allocated
  if (!specList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // loop through valueList allocating space and copying values to cvalue
  int j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (j > slen) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&specList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cspec.push_back(temp);
    j = j + lens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.AttPackAddAttribute(cname, cspec[1], cspec[2], cspec[0]);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));

  if (rc) *rc = status;

}  // end c_esmc_attpackaddattribute

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackcreatecustom - Setup the attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackcreatecustom)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackcreatecustom()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      int *count,                    // in - number of value(s)
      char *specList,                // in - char string
      int *lens,                     // in - lengths
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg slen) { // hidden/in - strlen count for specList
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // allocate space for the array of char*'s and vector of strings
  vector<string> cspec;
  cspec.reserve(*count);

  // check that valueList is allocated
  if (!specList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // loop through valueList allocating space and copying values to cvalue
  int j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (j > slen) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&specList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cspec.push_back(temp);
    j = j + lens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.AttPackCreateCustom(cspec[1], cspec[2], cspec[0]);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

  if (rc) *rc = status;

}  // end c_esmc_attpackcreatecustom

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackcreatestandard - Setup the attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackcreatestandard)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackcreatestandard()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      int *count,                    // in - number of value(s)
      char *specList,                // in - char string
      int *lens,                     // in - lengths
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg slen) { // hidden/in - strlen count for specList
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // allocate space for the array of char*'s and vector of strings
  vector<string> cspec;
  cspec.reserve(*count);

  // check that valueList is allocated
  if (!specList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // loop through valueList allocating space and copying values to cvalue
  int j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (j > slen) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&specList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cspec.push_back(temp);
    j = j + lens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.AttPackCreateStandard(cspec[1], cspec[2], cspec[0]);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

  if (rc) *rc = status;

}  // end c_esmc_attpackcreatestandard

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpacknest - Setup the attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpacknest)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpacknest()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,               // in/out - base object
      int *count,                     // in - number of value(s)
      char *specList,                 // in - char string
      int *lens,                      // in - lengths
      int  *nestCount,                // in - number of nested attpacks (child nodes)
      char *nestConvention,           // in - nest convention list
      char *nestPurpose,              // in - nest purpose list
      int  *nestConvLens,             // in - length of each nestConvention
      int  *nestPurpLens,             // in - length of each nestPurpose
      int  *rc,                       // in - return code
      ESMCI_FortranStrLenArg slen,    // hidden/in - strlen count for specList
      ESMCI_FortranStrLenArg nclen,   // hidden/in - strlen count for nestConvention
      ESMCI_FortranStrLenArg nplen) { // hidden/in - strlen count for nestPurpose
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int j, k, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if (!nestCount) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestCount", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!nestConvention) || (nclen <= 0) || (nestConvention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!nestPurpose) || (nplen <= 0) || (nestPurpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!nestConvLens) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvLens", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestPurpLens) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpLens", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // allocate space for the array of char*'s and vector of strings
  vector<string> cspec;
  cspec.reserve(*count);

  // check that valueList is allocated
  if (!specList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // loop through valueList allocating space and copying values to cvalue
  j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (j > slen) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad specList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&specList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cspec.push_back(temp);
    j = j + lens[i];
  }

  // allocate space for vector of strings
  vector<string> cnconv, cnpurp;
  cnconv.reserve(*nestCount);
  cnpurp.reserve(*nestCount);

  // loop through nestConvention, nestPurpose allocating space and copying
  //   values to cnconv, cnpurp
  j = 0;
  k = 0;
  for (unsigned int i=0; i<(*nestCount); i++) {
    if (!(nestConvention[j])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }
    if (!(nestPurpose[k])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 strings to null terminated ones
    string tempc((&nestConvention[j]), nestConvLens[i]);
    string tempp((&nestPurpose[k]), nestPurpLens[i]);
    tempc.resize(tempc.find_last_not_of(" ")+1);
    tempp.resize(tempp.find_last_not_of(" ")+1);
    cnconv.push_back(tempc);
    cnpurp.push_back(tempp);
    j += nestConvLens[i];
    k += nestPurpLens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.AttPackNest(cspec[1], cspec[2], cspec[0],
                                     *nestCount, cnconv, cnpurp);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

  if (rc) *rc = status;

}  // end c_esmc_attpacknest

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackcreatestdnest - Setup a standard nested attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackcreatestdnest)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackcreatestdnest()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *nestConvention,      // in - nest convention list
      char *nestPurpose,         // in - nest purpose list
      int  *nestConvLens,        // in - length of each nestConvention
      int  *nestPurpLens,        // in - length of each nestPurpose
      int  *nestAttPackInstanceCountList, // in - number of desired instances
                                          //   of each (conv,purp) attpack type
      int  *nestCount,           // in - number of nested attpacks (child nodes)
      char *nestAttPackInstanceNameList,  // out - attpack instance name list
      int  *nestAttPackInstanceNameLens,  // inout - length of each inst name
      int  *nestAttPackInstanceNameSize,  // in - number of elements in 
                                     //      attPackInstanceNameList
      int  *nestAttPackInstanceNameCount, // out - number of attpack 
                                          //   instance names
      int  *rc,                  // in - return code
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,// hidden/in - strlen count for object
      ESMCI_FortranStrLenArg nclen,// hidden/in - strlen count for nestConvention
      ESMCI_FortranStrLenArg nplen,// hidden/in - strlen count for nestPurpose
      ESMCI_FortranStrLenArg napinlen) { // hidden/in - strlen count for 
                                         //   nestAttPackInstanceNameList
// 
// !DESCRIPTION:
//     Create a standard nested attpack with a specified number of instances
//     of each attpack type (convention,purpose).  Return a list of their names.
//
//EOP

  int j, k, status;
  int totalInstances;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }


  // simple sanity check before doing any more work
  if ((!nestConvention) || (nclen <= 0) || (nestConvention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!nestPurpose) || (nplen <= 0) || (nestPurpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!nestConvLens) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvLens", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestPurpLens) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpLens", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceCountList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                        "bad attribute nestAttPackInstanceCountList,", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestCount) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                        "bad attribute nestCount,", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "no attribute nestAttPackInstanceNameList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameLens) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestAttPackInstanceNameLens", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameSize) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestAttPackInstanceNameSize", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameCount) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestAttPackInstanceNameCount", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cconv.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // allocate space for vectors of strings and ints
  vector<string> cnconv, cnpurp;
  vector<int> cnapicountlist;
  cnconv.reserve(*nestCount);
  cnpurp.reserve(*nestCount);
  cnapicountlist.reserve(*nestCount);

  // loop through nestConvention, nestPurpose allocating space and copying
  //   values to cnconv, cnpurp
  j = 0;
  k = 0;
  totalInstances = 0;
  for (unsigned int i=0; i<(*nestCount); i++) {
    cnapicountlist.push_back(nestAttPackInstanceCountList[i]);
    totalInstances += nestAttPackInstanceCountList[i];

    if (!(nestConvention[j])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }
    if (!(nestPurpose[k])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 strings to null terminated ones
    string tempc((&nestConvention[j]), nestConvLens[i]);
    string tempp((&nestPurpose[k]), nestPurpLens[i]);
    tempc.resize(tempc.find_last_not_of(" ")+1);
    tempp.resize(tempp.find_last_not_of(" ")+1);
    cnconv.push_back(tempc);
    cnpurp.push_back(tempp);
    j += nestConvLens[i];
    k += nestPurpLens[i];
  }

  // local buffer for returned attpack instance names and count
  vector<string> cnapinamelist;
  cnapinamelist.reserve(totalInstances);
  int cnapinamecount;

  // Create the attribute package on the object
  status = (**base).root.AttPackCreateStandard(cconv, cpurp, cobj,
                                               cnconv, cnpurp, 
                                               cnapicountlist,
                                               *nestCount,
                                               cnapinamelist,
                                               cnapinamecount);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

  // convert attpack instance names to F90
  int namecount = std::min(cnapinamecount, *nestAttPackInstanceNameSize);
  j = 0;
  for (unsigned int i=0; i<namecount; i++) {
    // check if F90 name buffer length is big enough
    if (cnapinamelist[i].length() > nestAttPackInstanceNameLens[i]) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_BUFFER_SHORT,
          "returning attPackInstanceName too long for given F90 name buffer",
           ESMC_CONTEXT, &status);
        if (rc) *rc = status;
        return;
    }

    nestAttPackInstanceNameLens[i] = cnapinamelist[i].length();
    status = ESMC_CtoF90string(cnapinamelist[i].c_str(), 
                               &nestAttPackInstanceNameList[j], 
                               nestAttPackInstanceNameLens[i]);
    if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
    j += nestAttPackInstanceNameLens[i];
  }

  // return number of converted attpack instance names
  *nestAttPackInstanceNameCount = namecount;

  if (rc) *rc = status;

}  // end c_esmc_attpackcreatestdnest

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackremove - Remove the attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackremove)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackremove()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,           // in/out - base object
      ESMCI::Attribute **attpack, // in - attribute package
      int *rc) {                  // in - return code
// 
// !DESCRIPTION:
//    Remove an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // Remove the attribute package from the object.
  status = (**base).root.AttPackRemove(*attpack);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackremove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackremoveattribute - Remove an attribute from an
//                                              attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackremoveattribute)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackremoveattribute()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
      ESMCI::Attribute ** attpack,   // in - attribute package
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//    Remove an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.AttPackRemoveAttribute(cname, *attpack);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackremoveattribute

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackgetcharlist - get attribute from an attpack
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackgetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetcharlist()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
	    ESMCI::Attribute **attpack,    // in - Attribute package
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *count,                    // in - must match actual length
      int *lens,                     // in/out - length of strings
      char *valueList,               // out - character values
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg nlen,   // hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen) { // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status, j;
  unsigned int i,k;
  ESMC_TypeKind_Flag attrTypeKind;
  int* llens;
  int lcount;
  ESMCI::Attribute *attr; 

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // verify the Attribute package
  if (!(*attpack)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NOTALLOC,
                         "failed getting Attribute package", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // get the attribute
  attr = (*attpack)->AttPackGetAttribute(cname);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "This Attribute package does have the specified Attribute", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // RLO - I don't understand this line, but retrieving Attributes from nested
  //       packages does not seem to work without it..
  // set attpack to parent of found attribute
  (*attpack) = attr->AttributeGetParent();

  // get type of the Attribute from the attpack
  status = (*attpack)->AttributeGet(cname, &attrTypeKind, NULL);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
  
  // take this out because an attribute that is not set will not yet have a typekind,
  // so if you are getting an attribute which was not set the call will fail here...
  /*  if (attrTypeKind != *tk) {
  printf("!!!!!!!!!!!!!!!!!\n\n\ntypekind in = %d  -  typekind out = %d\n", *tk, attrTypeKind);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind does not match this Attribute", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }*/

  // we need to get the count first 
  lcount = (*attpack)->AttributeGetItemCount(cname);
  if (lcount > *count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
                         "attribute has more items than array has space", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // set the itemcount out to the itemcount of the attribute
  // remove the count as a return value here, only use as input to check that there are
  // not more items than space in the list
  //*count = lcount;
  
  //   use the count to allocate llens
  llens = new int[lcount];
  
  //  use llens to get the lengths of all items on this attribute
  status = (*attpack)->AttributeGet(cname, llens, lcount);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<lcount; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_BUFFER_SHORT,
                         "Attribute is too long for buffer", ESMC_CONTEXT, &status);
      delete [] llens;
      if (rc) *rc = status;
      return;
    }
    // this was causing problems with the CtoF90string call below - have to pass
    // in the length of the F90 string, not the length of the C string!
    //lens[i] = llens[i];
  }
  
  vector<string> lcvalue;

  // next we get all the strings into the char**
  status = (*attpack)->AttributeGet(cname, &lcvalue);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }
  
  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<lcount; i++) {
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(lcvalue[i].c_str(), 
      &valueList[j], lens[i]);
    if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) {
      delete [] llens;
      return;
    }
    j = j + lens[i];
  }
      
  delete [] llens;
  if (rc) *rc = status;
  
}  // end c_esmc_attpackgetcharlist

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackgetvalue - get attribute from an attpack
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackgetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetvalue()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
	    ESMCI::Attribute **attpack,    // in - attribute package
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *count,                    // in - must match actual length
      void *value,                   // out - value
      int *rc,                       // in/out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Return the (name,value) pair from any object type in the system.
//
//EOP

  int status, attrCount;
  ESMC_TypeKind_Flag attrTk;
  ESMCI::Attribute *attr;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // check the attribute package
  if (!(*attpack)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NOTALLOC,
                    "failed getting attribute package", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // get the attribute
  attr = (*attpack)->AttPackGetAttribute(cname);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "This Attribute package does have the specified Attribute", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // RLO - I don't understand this line, but retrieving Attributes from nested
  //       packages does not seem to work without it..
  // set attpack to parent of found attribute
  (*attpack) = attr->AttributeGetParent();

  // get type of the Attribute from the attpack
  status = (*attpack)->AttributeGet(cname, &attrTk, &attrCount);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;

  if (attrTk != *tk) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "attribute value not expected kind", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (attrCount > *count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
                         "attribute has more items than array has space", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (value) {
    if (attrCount >= 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(attrCount);
        status = (*attpack)->AttributeGet(cname, &attrCount, &temp);
        for (unsigned int i=0; i<attrCount; i++)
          (static_cast<ESMC_I4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(attrCount);
        status = (*attpack)->AttributeGet(cname, &attrCount, &temp);
        for (unsigned int i=0; i<attrCount; i++)
          (static_cast<ESMC_I8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(attrCount);
        status = (*attpack)->AttributeGet(cname, &attrCount, &temp);
        for (unsigned int i=0; i<attrCount; i++)
          (static_cast<ESMC_R4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(attrCount);
        status = (*attpack)->AttributeGet(cname, &attrCount, &temp);
        for (unsigned int i=0; i<attrCount; i++)
          (static_cast<ESMC_R8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(attrCount);
        status = (*attpack)->AttributeGet(cname, &attrCount, &temp);
        for (unsigned int i=0; i<attrCount; i++)
          (static_cast<ESMC_Logical*> (value))[i] = temp[i];
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", ESMC_CONTEXT, &status);
        if (rc) *rc = status;
        return;
      }
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }
  }

  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
	ESMC_CONTEXT, &status)) {if (rc) *rc = status; return;}

  if (rc) *rc = status;

}  // end c_esmc_attpackgetvalue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackgetapinstnames - get attpack instance names
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackgetapinstnames)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetapinstnames()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,                  // in/out - base object
      ESMCI::Attribute **attpack,        // in - attribute package
      char *attPackInstanceNameList,     // out - attpack instance names
      int *attPackInstanceNameLens,      // inout - lengths of attpack inst names
      int *attPackInstanceNameSize,      // in - number of elements in 
                                         //      attPackInstanceNameList
      int *attPackInstanceNameCount,     // out - number of attpack instance names
      int *rc,                           // in - return code
      ESMCI_FortranStrLenArg napinlen) { // hidden/in - strlen count for attPackInstanceNameList
// 
// !DESCRIPTION:
//     Return the attpack instance names for (convention,purpose)
//
//EOP

  int j, k, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // check the attribute package
  if (!(*attpack)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NOTALLOC,
                    "failed getting attribute package", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "no attribute attPackInstanceNameList", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameLens) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute attPackInstanceNameLens", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameSize) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute attPackInstanceNameSize", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameCount) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute attPackInstanceNameCount", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // local buffer for returned attpack instance names and count
  vector<string> capinamelist;
  capinamelist.reserve(*attPackInstanceNameSize);
  int capinamecount;

  // Create the attribute package on the object
  status = (**base).root.AttPackGet((*attpack)->getConvention(), 
                                    (*attpack)->getPurpose(), 
                                    (*attpack)->getObject(),
                                    capinamelist, capinamecount);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

  // check if F90 name buffer size is big enough
  if (capinamecount > *attPackInstanceNameSize) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_BUFFER_SHORT,
        "given F90 buffer size too small for number of returning attPackInstanceNames",
         ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // convert attpack instance names to F90
  j = 0;
  for (unsigned int i=0; i<capinamecount; i++) {
    // check if F90 name buffer length is big enough
    if (capinamelist[i].length() > attPackInstanceNameLens[i]) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_BUFFER_SHORT,
         "returning attPackInstanceName too long for given F90 name buffer len",
           ESMC_CONTEXT, &status);
        if (rc) *rc = status;
        return;
    }

    attPackInstanceNameLens[i] = capinamelist[i].length();
    status = ESMC_CtoF90string(capinamelist[i].c_str(), 
                               &attPackInstanceNameList[j], 
                               attPackInstanceNameLens[i]);
    if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
    j += attPackInstanceNameLens[i];
  }

  // return number of converted attpack instance names
  *attPackInstanceNameCount = capinamecount;

}  // end c_esmc_attpackgetapinstnames

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackispresent - Query for an Attribute package Attribute
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackispresent)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackispresent()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
	  ESMCI::Attribute **attpack,    // in - Attribute package
      ESMC_Logical *present,         // out/out - present flag 
      int *rc,                       // in/out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name

// 
// !DESCRIPTION:
//     Query an Attribute package for the presence of an Attribute.
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  

  // Set the attribute on the object.
  status = (**base).root.AttPackIsPresent(cname, *attpack, present);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  
}  // end c_esmc_attpackispresent
/*
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeMove - Move an attribute between objects
//
// !INTERFACE:
      void FTN_X(c_esmc_attributemove)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributemove()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **source,              // in/out - base object
      ESMC_Base **destination,         // in/out - base object
      int *rc) {                       // in/out - return code
// 
// !DESCRIPTION:
//     Swap the Attribute hierarchy from Base1 to Base2
//
//EOP

  int status;
  
  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!source) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  status = (**destination).root.AttributeMove(&((**source).root));
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeMove
*/
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpacksetcharlist - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpacksetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpacksetcharlist()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *count,                    // in - number of items
      char *valueList,               // in - F90, non-null terminated string
      int *lens,                     // in - length of the char*s
      ESMCI::Attribute **attpack,    // in - attribute package
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg nlen,  // hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen) { // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int j, status;
  ESMCI::Attribute *attr;
  char msgbuf[ESMF_MAXSTR];

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // allocate space for the array of char*'s and vector of strings
  vector<string> cvalue;
  cvalue.reserve(*count);

  // loop through valueList allocating space and copying values to cvalue
  j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (!(valueList[j])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&valueList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cvalue.push_back(temp);
    j = j + lens[i];
  }
  
  // Find the attpack
  if(!(*attpack)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", ESMC_CONTEXT, &status);
    return;
  }
  
  // Find the attribute
  attr = (*attpack)->AttPackGetAttribute(cname);
  if (!attr) {
    sprintf(msgbuf, 
      "This Attribute package does not have an Attribute named %s\n",
       cname.c_str());
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, msgbuf, ESMC_CONTEXT, &status);
    return;
  }
  

  // Set the attribute on the object.
  status = attr->AttrModifyValue(*tk, *count, &cvalue);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpacksetcharlist

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpacksetvalue - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN_X(c_esmc_attpacksetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpacksetvalue()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *count,                    // in - item count
      void *value,                   // in - F90, non-null terminated string
      ESMCI::Attribute **attpack,    // in - attribute package
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int status;
  ESMCI::Attribute *attr;
  char msgbuf[ESMF_MAXSTR];

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  // Check the attpack
  if(!(*attpack)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", ESMC_CONTEXT, &status);
    return;
  }
  
  // Find the attribute
  attr = (*attpack)->AttPackGetAttribute(cname);
  if (!attr) {
    sprintf(msgbuf, 
      "This Attribute package does not have an Attribute named %s\n",
       cname.c_str());
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, msgbuf, ESMC_CONTEXT, &status);
    return;
  }
  
  if (value) {
	if (*count >= 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I4*> (value))[i]);
          status = attr->AttrModifyValue(*tk, *count, &temp);
        /*status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);*/
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I8*> (value))[i]);
          status = attr->AttrModifyValue(*tk, *count, &temp);
        /*status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);*/
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R4*> (value))[i]);
          status = attr->AttrModifyValue(*tk, *count, &temp);
        /*status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);*/
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R8*> (value))[i]);
          status = attr->AttrModifyValue(*tk, *count, &temp);
        /*status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);*/
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_Logical*> (value))[i]);
          status = attr->AttrModifyValue(*tk, *count, &temp);
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", ESMC_CONTEXT, &status);
        if (rc) *rc = status;
        return;
      }
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }
  }

  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
	ESMC_CONTEXT, &status)) {if (rc) *rc = status; return;}

  if (rc) *rc = status;

}  // end c_esmc_attpacksetvalue

//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeCopy - copy an attribute between objects
//
// !INTERFACE:
      void FTN_X(c_esmc_attributecopy)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributecopy()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **source,              // in/out - base object
      ESMC_Base **destination,         // in/out - base object
      ESMC_AttCopyFlag *attcopyflag,   // in - attcopyflag
      ESMC_AttTreeFlag *atttreeflag,   // in - atttreeflag
      int *rc) {                       // in/out - return code
// 
// !DESCRIPTION:
//     Copy the Attribute hierarchy from Base1 to Base2
//
//EOP

  int status;
  
  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!source) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!attcopyflag) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad ESMC_AttCopyFlag", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!atttreeflag) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad ESMC_AttTreeFlag", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (*attcopyflag == ESMF_COPY_VALUE && *atttreeflag == ESMC_ATTTREE_OFF) {
      status = (**destination).root.AttributeCopyValue((**source).root);
      ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  }
  else if (*attcopyflag == ESMF_COPY_REFERENCE) {
      status = (**destination).root.AttributeCopyHybrid((**source).root);
      ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  }
/* RLO - had to disable this because it no longer makes sense with new copy flag
  else if (*attcopyflag == ESMF_COPY_ALIAS) {
      status = (**destination).root.AttributeMove(&((**source).root));
      ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  }
*/
  else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "invalid attribute copy flag combination", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

}  // end c_ESMC_AttributeCopy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetCharList - get attribute list from an ESMF type
//
// !INTERFACE:
      void FTN_X(c_esmc_attributegetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributegetcharlist()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *count,               // in - must match actual length
      int *lens,                // in/out - length of strings
      char *valueList,          // out - character values
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen,   // hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen) { // hidden/in - strlen count for valueList
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status, j;
  unsigned int i,k;
  ESMC_TypeKind_Flag attrTypeKind;
  int *llens;
  int lcount;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
 
  // allocate space for the name
  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // check the typekind, do not return error (default value possible)
  status = (**base).root.AttributeGet(cname, &attrTypeKind, NULL);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
  
// take this out because an attribute that is not set will not yet have a typekind,
// so if you are getting an attribute which was not set the call will fail here...
/*  if (attrTypeKind != *tk) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind is inappropriate for this routine", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }*/

  // get the number of items on the attribute, compare to the buffer size
  lcount = (**base).root.AttributeGetItemCount(cname);
  if (lcount > *count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                         "attribute has more items than array has space", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }
  // now set *count to the actual number of items in the attribute
  // remove the count as a return value here, only use as input to check that there are
  // not more items than space in the list
//  *count = lcount;
  
  //   use the count to allocate llens
  llens = new int[lcount];
  
  //  use llens to get the lengths of all items on this attribute
  status = (**base).root.AttributeGet(cname, llens, lcount);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<lcount; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_BUFFER_SHORT,
                         "Attribute is too long for buffer", ESMC_CONTEXT, &status);
      delete [] llens;
      if (rc) *rc = status;
      return;
    }
    // this was causing problems with the CtoF90string call below - have to pass
    // in the length of the F90 string, not the length of the C string!
    //lens[i] = llens[i];
  }
  
  // allocate all char**s and string vector
  vector<string> cvalue;
  cvalue.reserve(lcount);

  // next we get all the strings into the char**
  status = (**base).root.AttributeGet(cname, &cvalue);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }
  
  printf("~~~~~~~~~~~~~~~~~~ cvalue = %s and size = %d\n", cvalue[0].c_str(), cvalue.size());
  
  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<lcount; i++) {
//printf("lens = %d\n", lens[i]);
//printf("strlen = %d\n", strlen(cvalue[i].c_str()));
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(cvalue[i].c_str(), &valueList[j], lens[i]);
    if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
    }
    j = j + lens[i];
  }
  
  delete [] llens;
  if (rc) *rc = status;
  
}  // end c_ESMC_AttributeGetCharList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetValue - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN_X(c_esmc_attributegetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributegetvalue()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *items,               // in - must match actual length
      void *value,              // out - value
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Return the (name,value) pair from any object type in the system.
//
//EOP

  int status, attrItems;
  ESMC_TypeKind_Flag attrTk;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.AttributeGet(cname, &attrTk, &attrItems);
  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;

  if (attrTk != *tk) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "attribute value not expected kind", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }
  
  if (attrItems > *items) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                         "item count is off", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }
  
  if (value) {
    if (attrItems >= 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(attrItems);
        status = (**base).root.AttributeGet(cname, &attrItems, &temp);
        for (unsigned int i=0; i<attrItems; i++)
          (static_cast<ESMC_I4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(attrItems);
        status = (**base).root.AttributeGet(cname, &attrItems, &temp);
        for (unsigned int i=0; i<attrItems; i++)
          (static_cast<ESMC_I8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(attrItems);
        status = (**base).root.AttributeGet(cname, &attrItems, &temp);
        for (unsigned int i=0; i<attrItems; i++)
          (static_cast<ESMC_R4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(attrItems);
        status = (**base).root.AttributeGet(cname, &attrItems, &temp);
        for (unsigned int i=0; i<attrItems; i++)
          (static_cast<ESMC_R8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(attrItems);
        status = (**base).root.AttributeGet(cname, &attrItems, &temp);
        for (unsigned int i=0; i<attrItems; i++)
          (static_cast<ESMC_Logical*> (value))[i] = temp[i];
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", ESMC_CONTEXT, &status);
        if (rc) *rc = status;
        return;
      }
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }
  }

  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
	ESMC_CONTEXT, &status)) {if (rc) *rc = status; return;}

  if (rc) *rc = status;

}  // end c_ESMC_AttributeGetValue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttpackGetInfoName - get type and number of items in an attpackattr
//
// !INTERFACE:
      void FTN_X(c_esmc_attpackgetinfoname)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetinfoname()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,              // in/out - base object
      char *name,                    // in - F90, non-null terminated string
	  ESMCI::Attribute **attpack,    // in - Attribute package
      ESMC_TypeKind_Flag *tk,        // out - typekind
      int *count,                    // out - item count
      int *rc,                       // in - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the typekind, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int status;
  ESMCI::Attribute *attr;

printf("00000000000000\n");

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
printf("11111111111111111\n");

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
printf("2222222222222222\n");

  if (!tk) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute typekind", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  if (!count) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // get the Attribute package
  if (!(*attpack)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NOTALLOC,
                         "failed getting Attribute package", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // get the attribute
  attr = (*attpack)->AttPackGetAttribute(cname);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, 
      "This Attribute package does have the specified Attribute", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  // RLO - I don't understand this line, but retrieving Attributes from nested
  //       packages does not seem to work without it..
  // set attpack to parent of found attribute
  (*attpack) = attr->AttributeGetParent();

  status = (*attpack)->AttributeGet(cname, tk, count);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttpackGetInfoName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetInfoName - get type and number of items in an attr
//
// !INTERFACE:
      void FTN_X(c_esmc_attributegetinfoname)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributegetinfoname()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // out - typekind
      int *count,               // out - item count
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the typekind, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  if (!tk) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute typekind", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  if (!count) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.AttributeGet(cname, tk, count);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeGetInfoName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetInfoNum - get type and number of items in an attr
//
// !INTERFACE:
      void FTN_X(c_esmc_attributegetinfonum)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributegetinfonum()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int *num,                 // in - attr number
      char *name,               // out - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // out - typekind
      int *count,               // out - item count
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the name, type, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  if (!tk) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute typekind", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  if (!count) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // declare string for name
  string cname;

  status = (**base).root.AttributeGet((*num)-1, &cname, tk, count);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  status = ESMC_CtoF90string(cname.c_str(), name, nlen);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeGetInfoNum


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetCount - get number of attrs
//
// !INTERFACE:
      void FTN_X(c_esmc_attributegetcount)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributegetcount()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int *count,               // out - attribute count
      ESMC_AttGetCountFlag *flag,  // in - attgetcount flag
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//   Return the count of attributes for any object type in the system.
//
//EOP

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  if (!count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (*flag == ESMC_ATTGETCOUNT_ATTRIBUTE)
      *count = (**base).root.AttributeGetCount();
  else if (*flag == ESMC_ATTGETCOUNT_ATTPACK)
      *count = (**base).root.AttributeGetCountPack();
  else if (*flag == ESMC_ATTGETCOUNT_ATTLINK)
      *count = (**base).root.AttributeGetCountLink();
  else if (*flag == ESMC_ATTGETCOUNT_TOTAL)
      *count = (**base).root.AttributeGetCountTotal();
  else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "invalid value for attcountflag", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (count <= 0) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute count", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }
  
  if (rc) *rc = ESMF_SUCCESS;    

}  // end c_ESMC_AttributeGetCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attributeispresent - Query for an Attribute
//
// !INTERFACE:
      void FTN_X(c_esmc_attributeispresent)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributeispresent()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      ESMC_Logical *present,     // out/out - present flag 
      int *rc,                   // in/out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Query for the presence of an Attribute.
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  if (!present) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute present flag", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.AttributeIsPresent(cname, present);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attributeispresent

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeLink - Link an Attribute hierarchy
//
// !INTERFACE:
      void FTN_X(c_esmc_attributelink)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributelink()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **source,       // in/out - base object
      ESMC_Base **destination,  // in/out - base destination object
      ESMC_Logical *linkChange, // in/out - link changes boolean
      int *rc) {                // in/out - return value 
// 
// !DESCRIPTION:
//     Set a link in an attribute hierarchy.
//
//EOP

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!source) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  if (!linkChange) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad linkChange", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // Set the attribute link on the object.
  status = (**source).root.AttributeLink(&(**destination).root, linkChange);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeLink

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeLinkRemove - Remove a link an Attribute hierarchy
//
// !INTERFACE:
      void FTN_X(c_esmc_attributelinkremove)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributelinkremove()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **source,       // in/out - base object
      ESMC_Base **destination,  // in/out - base destination object
      ESMC_Logical *linkChange, // in/out - link changes boolean
      int *rc) {                // in/out - return value 
// 
// !DESCRIPTION:
//     Remove a link in an attribute hierarchy.
//
//EOP

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!source) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  if (!linkChange) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad linkChange", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // Set the attribute link on the object.
  status = (**source).root.AttributeLinkRemove(&(**destination).root, linkChange);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeLinkRemove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attributeremove - Remove the attribute
//
// !INTERFACE:
      void FTN_X(c_esmc_attributeremove)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributeremove()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      int *rc,                   // in - return code     
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//    Remove an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.AttributeRemove(cname);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attributeremove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetCharList - Set String Attribute List on an ESMF type
//
// !INTERFACE:
      void FTN_X(c_esmc_attributesetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributesetcharlist()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *count,               // in - number of value(s)
      char *valueList,          // in - char string
      int *lens,                // in - lengths
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen) { // hidden/in - strlen count for valueList
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//     Character strings have this special version since they come in
//     with an additional hidden length argument.
//
//EOP

  int j, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // allocate space for the array of char*'s and vector of strings
  vector<string> cvalue;
  cvalue.reserve(*count);

  // check that valueList is allocated
  if (!valueList) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // loop through valueList allocating space and copying values to cvalue
  j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (j > vlen) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&valueList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cvalue.push_back(temp);
    j = j + lens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.AttributeSet(cname, cvalue.size(), &cvalue);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeSetCharList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetValue - Set Attribute on an ESMF type
//
// !INTERFACE:
      void FTN_X(c_esmc_attributesetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributesetvalue()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // in - typekind
      int *count,               // in - number of value(s)
      void *value,              // in - any value or list of values
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//     Any type or list of types can be passed except character strings
//     since they come with an additional hidden length argument.
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (value) {
    if (*count >= 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I4*> (value))[i]);
        status = (**base).root.AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I8*> (value))[i]);
        status = (**base).root.AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R4*> (value))[i]);
        status = (**base).root.AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R8*> (value))[i]);
        status = (**base).root.AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_Logical*> (value))[i]);
        status = (**base).root.AttributeSet(cname, *count, &temp);
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", ESMC_CONTEXT, &status);
        if (rc) *rc = status;
        return;
      }
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
    }
  }

  if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
	ESMC_CONTEXT, &status)) {if (rc) *rc = status; return;}
    
  if (rc) *rc = status;

}  // end c_ESMC_AttributeSetValue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetObjsInTree - Set an Attribute on all objects
//                                               in an Attribute hierarchy
//
// !INTERFACE:
      void FTN_X(c_esmc_attributesetobjsintree)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributesetobjsintree()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *object,             // in - F90, object of the Attribute
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind_Flag *tk,        // in - typekind of the Attribute
      int *count,               // in - items
      void *value,              // in - value
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg olen,// hidden/in - strlen count for object
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Change the Attribute values for Attribute <name> on all <object>s.
//
//EOP

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cobject(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cobject.resize(cobject.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobject.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.AttributeSetObjsInTree(cname,cobject,*tk,*count,value);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeSetObjsInTree

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetObjChrInTree - Set a Char Attribute on all
//                                           objects in an Attribute hierarchy
//
// !INTERFACE:
      void FTN_X(c_esmc_attributesetobjchrintree)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributesetobjchrintree()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *object,             // in - F90, object of the Attribute
      char *name,               // in - F90, non-null terminated string
      char *value,              // in - value
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg olen, // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg nlen, // hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen) { // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Change the Attribute values for Attribute <name> on all <object>s.
//
//EOP

  ESMC_TypeKind_Flag tk;
  int count, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if ((!value) || (vlen <= 0) || (value[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute character string value", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cvalue(value, vlen);
  cvalue.resize(cvalue.find_last_not_of(" ")+1);

  if (cvalue.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute character string value conversion",
                          ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // Re-use c_ESMC_AttributeSetObjsInTree() to set the attribute on the object.
  tk = ESMC_TYPEKIND_CHARACTER;
  count = 1;
  FTN_X(c_esmc_attributesetobjsintree)(base, object, name, 
      &tk, &count, (void *)&cvalue, &status, olen, nlen); 
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeSetObjChrInTree

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeUpdate - Update an Attribute
//
// !INTERFACE:
      void FTN_X(c_esmc_attributeupdate)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributeupdate()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      ESMCI::VM **vm,           // in - VM that this Attribute lives on
      int *rootList,            // in - root PET list
      int *count,               // in - count of rootList
      int *rc) {                // in - return code
// 
// !DESCRIPTION:
//     Update an Attribute.
//
//EOP

  int status;
  vector<ESMC_I4> rootListl;
  
  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  //check the VM
  if (*vm == ESMC_NULL_POINTER){
    *vm = ESMCI::VM::getCurrent(&status);
    if (ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
  }

  // make list into a vector
  rootListl.reserve(*count);
  for (unsigned int i=0; i<*count; ++i)
    rootListl.push_back(rootList[i]);

  // Update the Attribute
  status = (**base).root.AttributeUpdate(*vm, rootListl);
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeUpdate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeUpdateReset - Reset flags in an Attribute hierarchy
//
// !INTERFACE:
      void FTN_X(c_esmc_attributeupdatereset)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributeupdatereset()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int *rc) {                // in - return code
// 
// !DESCRIPTION:
//     Reset flags after updating an Attribute hierarchy.
//
//EOP

  int status;
  
  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }
  
  // Update the Attribute
  status = (**base).root.AttributeUpdateReset();
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeUpdate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attributeread - Read the attribute package
//
// !INTERFACE:
void FTN_X(c_esmc_attributeread)(ESMC_Base **base,
                               int *fileNameLen,
                               const char *fileName,
                               int *schemaFileNameLen,
                               const char *schemaFileName,
                               int *status,
                               ESMCI_FortranStrLenArg fnlen,
                               ESMCI_FortranStrLenArg sfnlen) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributeread()"

// TODO convention, purpose, basename?
// TODO match formatting of rest of this file

   ESMF_CHECK_POINTER(*base, status)

   // Read the attributes into the object.
   int rc = (*base)->root.AttributeRead(
                    *fileNameLen, // always present internal arg.
                    ESMC_NOT_PRESENT_FILTER(fileName),
                    *schemaFileNameLen, // always present internal arg.
                    ESMC_NOT_PRESENT_FILTER(schemaFileName));
                    
   if (ESMC_PRESENT(status)) *status = rc;
}

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attributewritetab - Setup the attribute package
//
// !INTERFACE:
void FTN_X(c_esmc_attributewritetab)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributewritetab()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *targetobj,           // in - target object for writing
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,// hidden/in - strlen count for object
      ESMCI_FortranStrLenArg tlen) { // hidden/in - strlen count for target object
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;
  
  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!targetobj) || (tlen <= 0) || (targetobj[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  string ctarobj(targetobj, tlen);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);
  ctarobj.resize(ctarobj.find_last_not_of(" ")+1);

  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (ctarobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // Write the attributes from the object.
  status = (**base).root.AttributeWriteTab(cconv, cpurp, cobj, ctarobj,
    (*base)->ESMC_Base::ESMC_BaseGetName());
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attributewritetab

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attributewritexml - Setup the attribute package
//
// !INTERFACE:
void FTN_X(c_esmc_attributewritexml)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributewritexml()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *targetobj,           // in - target object for writing
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,// hidden/in - strlen count for object
      ESMCI_FortranStrLenArg tlen) { // hidden/in - strlen count for target object
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;
  
  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", ESMC_CONTEXT, &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!targetobj) || (tlen <= 0) || (targetobj[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  string ctarobj(targetobj, tlen);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);
  ctarobj.resize(ctarobj.find_last_not_of(" ")+1);

  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", ESMC_CONTEXT, &status);
    if (rc) *rc = status;
    return;
  }

  if (ctarobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", ESMC_CONTEXT, &status);
      if (rc) *rc = status;
      return;
  }

  // Write the attributes from the object.
  status = (**base).root.AttributeWriteXML(cconv, cpurp, cobj, ctarobj, 
    (*base)->ESMC_Base::ESMC_BaseGetName());
  ESMC_LogDefault.MsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackwritexml

#undef  ESMC_METHOD

} // extern "C"
