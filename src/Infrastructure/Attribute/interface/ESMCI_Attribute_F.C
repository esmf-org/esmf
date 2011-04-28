// $Id: ESMCI_Attribute_F.C,v 1.46 2011/04/28 18:53:29 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
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
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include <algorithm> // std::min()
#include "ESMCI_F90Interface.h"
#include "ESMCI_Macros.h"
#include "ESMCI_Attribute.h"
#include "ESMCI_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"

using std::string;
using std::vector;

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Attribute_F.C,v 1.46 2011/04/28 18:53:29 rokuingh Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes Attribute routine interfaces
//
//

extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  Attribute object methods
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackaddattribute - add an attribute to an attpack
//
// !INTERFACE:
      void FTN(c_esmc_attpackaddattribute)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackaddattribute()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen) { // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.AttPackAddAttribute(cname, cconv, cpurp, cobj);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackaddattribute

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackcreatecustom - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpackcreatecustom)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackcreatecustom()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen) { // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.AttPackCreateCustom(cconv, cpurp, cobj);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackcreatecustom

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackcreatestandard - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpackcreatestandard)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackcreatestandard()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen) { // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Associate a convention, purpose, and object type with an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.AttPackCreateStandard(cconv, cpurp, cobj);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackcreatestandard

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpacknest - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpacknest)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpacknest()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      int  *nestCount,           // in - number of nested attpacks (child nodes)
      char *nestConvention,      // in - nest convention list
      char *nestPurpose,         // in - nest purpose list
      int  *nestConvLens,        // in - length of each nestConvention
      int  *nestPurpLens,        // in - length of each nestPurpose
      int  *rc,                  // in - return code
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,// hidden/in - strlen count for object
      ESMCI_FortranStrLenArg nclen,// hidden/in - strlen count for nestConvention
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestCount) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestCount", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!nestConvention) || (nclen <= 0) || (nestConvention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!nestPurpose) || (nplen <= 0) || (nestPurpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!nestConvLens) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvLens", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestPurpLens) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpLens", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", &status);
      if (rc) *rc = status;
      return;
    }
    if (!(nestPurpose[k])) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", &status);
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
  status = (**base).root.AttPackNest(cconv, cpurp, cobj,
                                     *nestCount, cnconv, cnpurp);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpacknest

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackcreatestdnest - Setup a standard nested attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpackcreatestdnest)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }


  // simple sanity check before doing any more work
  if ((!nestConvention) || (nclen <= 0) || (nestConvention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!nestPurpose) || (nplen <= 0) || (nestPurpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!nestConvLens) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvLens", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestPurpLens) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpLens", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceCountList) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                        "bad attribute nestAttPackInstanceCountList,", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestCount) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                        "bad attribute nestCount,", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameList) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "no attribute nestAttPackInstanceNameList", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameLens) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestAttPackInstanceNameLens", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameSize) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestAttPackInstanceNameSize", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!nestAttPackInstanceNameCount) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestAttPackInstanceNameCount", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestConvention", &status);
      if (rc) *rc = status;
      return;
    }
    if (!(nestPurpose[k])) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute nestPurpose", &status);
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
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

  // convert attpack instance names to F90
  int namecount = std::min(cnapinamecount, *nestAttPackInstanceNameSize);
  j = 0;
  for (unsigned int i=0; i<namecount; i++) {
    // check if F90 name buffer length is big enough
    if (cnapinamelist[i].length() > nestAttPackInstanceNameLens[i]) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_BUFFER_SHORT,
          "returning attPackInstanceName too long for given F90 name buffer",
           &status);
        if (rc) *rc = status;
        return;
    }

    nestAttPackInstanceNameLens[i] = cnapinamelist[i].length();
    status = ESMC_CtoF90string(const_cast<char*>(cnapinamelist[i].c_str()), 
                               &nestAttPackInstanceNameList[j], 
                               nestAttPackInstanceNameLens[i]);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    j += nestAttPackInstanceNameLens[i];
  }

  // return number of converted attpack instance names
  *nestAttPackInstanceNameCount = namecount;

}  // end c_esmc_attpackcreatestdnest

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackremove - Remove the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpackremove)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackremove()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *attPackInstanceName, // in - attpack instance name
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg clen,   // hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,   // hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//    Remove an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // Remove the attribute package from the object.
  status = (**base).root.AttPackRemove(cconv, cpurp, cobj, capname);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackremove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackremoveattribute - Remove an attribute from an
//                                              attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpackremoveattribute)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackremoveattribute()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *attPackInstanceName, // in - attpack instance name
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//    Remove an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // Set the attribute on the object.
  status = (**base).root.AttPackRemoveAttribute(cname, cconv, cpurp, cobj,
                                                capname);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackremoveattribute
/*
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackgetchar - get attribute from an attpack
//
// !INTERFACE:
      void FTN(c_esmc_attpackgetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetchar()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      char *value,              // out - character value
      char *convention,         // in - convention
      char *purpose,            // in - purpose
      char *object,             // in - object
      char *attPackInstanceName,// in - attpack instance name
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen,// hidden/in - strlen count for value
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status;
  ESMC_TypeKind attrTypeKind;
  int slen;              // actual attribute string length
  int *llens;
  ESMCI::Attribute *attpack, *attr; 

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }
  
  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  if (cconv.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  //   use the count to allocate llens
  llens = new int[1];
  
  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // get the Attribute package
  attpack = (**base).root.AttPackGet(cconv, cpurp, cobj, capname);
  if (!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NOTALLOC,
                         "failed getting Attribute package", &status);
    if (rc) *rc = status;
    return;
  }

  // get the attribute
  attr = attpack->AttPackGetAttribute(cname);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "This Attribute package does have the specified Attribute", &status);
    if (rc) *rc = status;
    return;
  }

  // set attpack to parent of found attribute
  attpack = attr->AttributeGetParent();

  // get length of the attribute
  status = attpack->AttributeGet(cname, llens, 1);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) {
    ESMC_LogDefault.Write("failed getting item char* lengths", ESMC_LOG_INFO);
    delete [] llens;
    return;
  }
  
  slen = llens[0];

  // make sure destination will be long enough
  if (slen > vlen) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_BUFFER_SHORT,
                         "Attribute is too long for buffer", &status);
    delete [] llens;
    if (rc) *rc = status;
    return; 
  }

  string cvalue;
  status = attpack->AttributeGet(cname, &cvalue);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) {
    ESMC_LogDefault.Write("failed getting Attribute value", ESMC_LOG_INFO);
    delete [] llens;
    return;
  }

  status = ESMC_CtoF90string(const_cast<char*> (cvalue.c_str()), value, vlen);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

  delete [] llens;
  
}  // end c_esmc_attpackgetchar*/

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackgetcharlist - get attribute from an attpack
//
// !INTERFACE:
      void FTN(c_esmc_attpackgetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetcharlist()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind *tk,        // in - typekind
      int *count,               // in - must match actual length
      int *lens,                // in/out - length of strings
      char *valueList,          // out - character values
      char *convention,         // in - convention
      char *purpose,            // in - purpose
      char *object,             // in - object
      char *attPackInstanceName,// in - attpack instance name
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen,// hidden/in - strlen count for value
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status, j;
  unsigned int i,k;
  ESMC_TypeKind attrTypeKind;
  int* llens;
  int lcount;
  ESMCI::Attribute *attpack, *attr; 

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  if (cconv.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // get the Attribute package
  attpack = (**base).root.AttPackGet(cconv, cpurp, cobj, capname);
  if (!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NOTALLOC,
                         "failed getting Attribute package", &status);
    if (rc) *rc = status;
    return;
  }

  // get the attribute
  attr = attpack->AttPackGetAttribute(cname);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "This Attribute package does have the specified Attribute", &status);
    if (rc) *rc = status;
    return;
  }

  // set attpack to parent of found attribute
  attpack = attr->AttributeGetParent();

  // get type of the Attribute from the attpack
  status = attpack->AttributeGet(cname, &attrTypeKind, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) return;
  
// take this out because an attribute that is not set will not yet have a typekind,
// so if you are getting an attribute which was not set the call will fail here...
/*  if (attrTypeKind != *tk) {
printf("!!!!!!!!!!!!!!!!!\n\n\ntypekind in = %d  -  typekind out = %d\n", *tk, attrTypeKind);
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind does not match this Attribute", &status);
    if (rc) *rc = status;
    return;
  }*/

  // we need to get the count first 
  lcount = attpack->AttributeGetItemCount(cname);
  if (lcount > *count) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                         "attribute has more items than array has space", &status);
    if (rc) *rc = status;
    return;
  }
  // set the itemcount out to the itemcount of the attribute
  *count = lcount;
  
  //   use the count to allocate llens
  llens = new int[lcount];
  
  //  use llens to get the lengths of all items on this attribute
  status = attpack->AttributeGet(cname, llens, lcount);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<lcount; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_BUFFER_SHORT,
                         "Attribute is too long for buffer", &status);
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
  status = attpack->AttributeGet(cname, &lcvalue);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }
  
  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<lcount; i++) {
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(const_cast<char*> (lcvalue[i].c_str()), 
      &valueList[j], lens[i]);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) {
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
      void FTN(c_esmc_attpackgetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetvalue()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_TypeKind *tk,        // in - typekind
      int *count,               // in - must match actual length
      void *value,              // out - value
      char *convention,         // in - convention
      char *purpose,            // in - purpose
      char *object,             // in - object
      char *attPackInstanceName,// in - attpack instance name
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//     Return the (name,value) pair from any object type in the system.
//
//EOP

  int status, attrCount;
  ESMC_TypeKind attrTk;
  ESMCI::Attribute *attpack, *attr;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  if (cconv.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // get the attribute package
  attpack = (**base).root.AttPackGet(cconv, cpurp, cobj, capname);
  if (!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NOTALLOC,
                    "failed getting attribute package", &status);
    if (rc) *rc = status;
    return;
  }

  // get the attribute
  attr = attpack->AttPackGetAttribute(cname);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "This Attribute package does have the specified Attribute", &status);
    if (rc) *rc = status;
    return;
  }

  // set attpack to parent of found attribute
  attpack = attr->AttributeGetParent();

  // get type of the Attribute from the attpack
  status = attpack->AttributeGet(cname, &attrTk, &attrCount);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) return;

  if (attrTk != *tk) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "attribute value not expected kind", &status);
    if (rc) *rc = status;
    return;
  }

  if (attrCount > *count) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                         "attribute has more items than array has space", &status);
    if (rc) *rc = status;
    return;
  }

  if (value) {
/*    if (*count == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = attpack->AttributeGet(cname, (static_cast<ESMC_I4*> (value)));  
      else if (*tk == ESMC_TYPEKIND_I8)
        status = attpack->AttributeGet(cname, (static_cast<ESMC_I8*> (value)));
      else if (*tk == ESMC_TYPEKIND_R4)
        status = attpack->AttributeGet(cname, (static_cast<ESMC_R4*> (value)));
      else if (*tk == ESMC_TYPEKIND_R8)
        status = attpack->AttributeGet(cname, (static_cast<ESMC_R8*> (value)));
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = attpack->AttributeGet(cname, (static_cast<ESMC_Logical*> (value)));
      else {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
    }
    else if (*count > 1) {*/
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        status = attpack->AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_I4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        status = attpack->AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_I8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        status = attpack->AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_R4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        status = attpack->AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_R8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        status = attpack->AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_Logical*> (value))[i] = temp[i];
        temp.clear();
      } else {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
/*    }
    else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", &status);
      if (rc) *rc = status;
      return;
    }*/
  }

  if (rc) *rc = status;

}  // end c_esmc_attpackgetvalue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpackgetapinstnames - get attpack instance names
//
// !INTERFACE:
      void FTN(c_esmc_attpackgetapinstnames)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackgetapinstnames()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *convention,         // in - convention
      char *purpose,            // in - purpose
      char *object,             // in - object
      char *attPackInstanceNameList, // out - attpack instance names
      int *attPackInstanceNameLens,  // inout - lengths of attpack inst names
      int *attPackInstanceNameSize,  // in - number of elements in 
                                     //      attPackInstanceNameList
      int *attPackInstanceNameCount, // out - number of attpack instance names
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg clen, // hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen, // hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen, // hidden/in - strlen count for object
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameList) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "no attribute attPackInstanceNameList", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameLens) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute attPackInstanceNameLens", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameSize) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute attPackInstanceNameSize", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!attPackInstanceNameCount) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute attPackInstanceNameCount", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // local buffer for returned attpack instance names and count
  vector<string> capinamelist;
  capinamelist.reserve(*attPackInstanceNameSize);
  int capinamecount;

  // Create the attribute package on the object
  status = (**base).root.AttPackGet(cconv, cpurp, cobj,
                                    capinamelist, capinamecount);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

  // check if F90 name buffer size is big enough
  if (capinamecount > *attPackInstanceNameSize) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_BUFFER_SHORT,
        "given F90 buffer size too small for number of returning attPackInstanceNames",
         &status);
      if (rc) *rc = status;
      return;
  }

  // convert attpack instance names to F90
  j = 0;
  for (unsigned int i=0; i<capinamecount; i++) {
    // check if F90 name buffer length is big enough
    if (capinamelist[i].length() > attPackInstanceNameLens[i]) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_BUFFER_SHORT,
         "returning attPackInstanceName too long for given F90 name buffer len",
           &status);
        if (rc) *rc = status;
        return;
    }

    attPackInstanceNameLens[i] = capinamelist[i].length();
    status = ESMC_CtoF90string(const_cast<char*>(capinamelist[i].c_str()), 
                               &attPackInstanceNameList[j], 
                               attPackInstanceNameLens[i]);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
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
      void FTN(c_esmc_attpackispresent)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackispresent()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *attPackInstanceName, // in - attpack instance name
      ESMC_Logical *present,     // out/out - present flag 
      int *rc,                   // in/out - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//     Query an Attribute package for the presence of an Attribute.
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // Set the attribute on the object.
  status = (**base).root.AttPackIsPresent(cname, cconv, cpurp, cobj, capname, 
                                          present);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));
  
}  // end c_esmc_attpackispresent
/*
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeMove - Move an attribute between objects
//
// !INTERFACE:
      void FTN(c_esmc_attributemove)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", &status);
    if (rc) *rc = status;    
    return;
  }

  status = (**destination).root.AttributeMove(&((**source).root));
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeMove
*//*
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpacksetchar - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpacksetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpacksetchar()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      char *value,               // in - F90, non-null terminated string
      ESMC_TypeKind *tk,         // in - typekind
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *attPackInstanceName, // in - attpack instance name
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen,// hidden/in - strlen count for value
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if (!value) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!convention) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!purpose) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!object) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cvalue(value, vlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cvalue.resize(cvalue.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", &status);
      if (rc) *rc = status;
      return;
  }

  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // Set the attribute on the object.
  status = (**base).root.AttPackSet(cname, *tk, 1, &cvalue, cconv, cpurp, cobj,
                                    capname);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpacksetchar*/

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpacksetcharlist - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpacksetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpacksetcharlist()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      ESMC_TypeKind *tk,         // in - typekind
      int *count,                 // in - number of items
      char *valueList,               // in - F90, non-null terminated string
      int *lens,                 // in - length of the char*s
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *attPackInstanceName, // in - attpack instance name
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen,// hidden/in - strlen count for value
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int j, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!convention) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!purpose) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!object) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", &status);
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&valueList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    cvalue.push_back(temp);
    j = j + lens[i];
  }
  
  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  // Set the attribute on the object.
  status = (**base).root.AttPackSet(cname, *tk, *count, &cvalue, cconv, cpurp,
                                    cobj, capname);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpacksetcharlist

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attpacksetvalue - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpacksetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpacksetvalue()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      ESMC_TypeKind *tk,         // in - typekind
      int *count,                // in - item count
      void *value,               // in - F90, non-null terminated string
      char *convention,          // in - convention
      char *purpose,             // in - purpose
      char *object,              // in - object type
      char *attPackInstanceName, // in - attpack instance name
      int *rc,                   // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg clen,// hidden/in - strlen count for convention
      ESMCI_FortranStrLenArg plen,// hidden/in - strlen count for purpose
      ESMCI_FortranStrLenArg olen,   // hidden/in - strlen count for object
      ESMCI_FortranStrLenArg alen) { // hidden/in - strlen count for attPackInstanceName
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if (!convention) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!purpose) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if (!object) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cconv(convention, clen);
  string cpurp(purpose, plen);
  string cobj(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cconv.resize(cconv.find_last_not_of(" ")+1);
  cpurp.resize(cpurp.find_last_not_of(" ")+1);
  cobj.resize(cobj.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", &status);
      if (rc) *rc = status;
      return;
  }
  
  // convert optional (char *) arg attPackInstanceName to string
// TODO: is the following line safe for all F90 compilers when passing a 
//       not-present char* attPackInstanceName ?  what is value of alen?
//  string capname((char*)ESMC_NOT_PRESENT_FILTER(attPackInstanceName), alen);
  string capname;
  if (ESMC_NOT_PRESENT_FILTER(attPackInstanceName) != ESMC_NULL_POINTER &&
                                                      alen > 0) {
    capname.assign(attPackInstanceName, 0, alen);
  }
  capname.resize(capname.find_last_not_of(" ")+1);

  if (value) {
/*    if (*count == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = (**base).root.AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_I4*> (value)), cconv, cpurp, cobj, capname);
      else if (*tk == ESMC_TYPEKIND_I8)
        status = (**base).root.AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_I8*> (value)), cconv, cpurp, cobj, capname);
      else if (*tk == ESMC_TYPEKIND_R4)
        status = (**base).root.AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_R4*> (value)), cconv, cpurp, cobj, capname);
      else if (*tk == ESMC_TYPEKIND_R8)
        status = (**base).root.AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_R8*> (value)), cconv, cpurp, cobj, capname);
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = (**base).root.AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_Logical*> (value)), cconv, cpurp, cobj, capname);
      else {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
    }
    else if (*count > 1) {*/
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I4*> (value))[i]);
        status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I8*> (value))[i]);
        status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R4*> (value))[i]);
        status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R8*> (value))[i]);
        status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_Logical*> (value))[i]);
        status = (**base).root.AttPackSet(cname, *tk, *count, &temp,
                        cconv, cpurp, cobj, capname);
        temp.clear();
      } else {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
/*    }
    else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", &status);
      if (rc) *rc = status;
      return;
    }*/
  }

  if (rc) *rc = status;

}  // end c_esmc_attpacksetvalue

//-----------------------------------------------------------------------------


      void FTN(c_esmc_attributeread)(ESMC_Base **base,
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
      void FTN(c_esmc_attributewritetab)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!targetobj) || (tlen <= 0) || (targetobj[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object", &status);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (ctarobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Write the attributes from the object.
  status = (**base).root.AttributeWriteTab(cconv, cpurp, cobj, ctarobj,
    (*base)->ESMC_Base::ESMC_BaseGetName());
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attributewritetab

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attributewritexml - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attributewritexml)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!targetobj) || (tlen <= 0) || (targetobj[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object", &status);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (ctarobj.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Write the attributes from the object.
  status = (**base).root.AttributeWriteXML(cconv, cpurp, cobj, ctarobj, 
    (*base)->ESMC_Base::ESMC_BaseGetName());
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attpackwritexml

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeCopy - copy an attribute between objects
//
// !INTERFACE:
      void FTN(c_esmc_attributecopy)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!attcopyflag) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad ESMC_AttCopyFlag", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!atttreeflag) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad ESMC_AttTreeFlag", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (*attcopyflag == ESMC_ATTCOPY_VALUE && *atttreeflag == ESMC_ATTTREE_OFF) {
      status = (**destination).root.AttributeCopyValue((**source).root);
      ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));
  }
  else if (*attcopyflag == ESMC_ATTCOPY_HYBRID) {
      status = (**destination).root.AttributeCopyHybrid((**source).root);
      ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));
  }
  else if (*attcopyflag == ESMC_ATTCOPY_REFERENCE) {
      status = (**destination).root.AttributeMove(&((**source).root));
      ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc));
  }
  else {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "invalid attribute copy flag combination", &status);
    if (rc) *rc = status;
    return;
  }

}  // end c_ESMC_AttributeCopy
/*
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetChar - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributegetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributegetchar()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      char *value,              // out - character value
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen) { // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status;
  ESMC_TypeKind attrTypeKind;
  int slen;
  int *llens;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  //   use the count to allocate llens
  llens = new int[1];
  
  //  use llens to get the lengths of all items on this attribute
  status = (**base).root.AttributeGet(cname, llens, 1);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) {
    ESMC_LogDefault.Write("failed getting item char* lengths", ESMC_LOG_INFO);
    delete [] llens;
    return;
  }
  slen = llens[0];

  // make sure destination will be long enough
  if (slen > vlen) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_BUFFER_SHORT,
                         "Attribute is too long for buffer", &status);
    delete [] llens;
    if (rc) *rc = status;
    return; 
  }

  string cvalue;
  status = (**base).root.AttributeGet(cname, &cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }

  status = ESMC_CtoF90string(const_cast<char*> (cvalue.c_str()), value, vlen);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));
    
  delete [] llens;
  
}  // end c_ESMC_AttributeGetChar*/

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetCharList - get attribute list from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributegetcharlist)(
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
      ESMC_TypeKind *tk,        // in - typekind
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
  ESMC_TypeKind attrTypeKind;
  int *llens;
  int lcount;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
 
  // allocate space for the name
  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // check the typekind, do not return error (default value possible)
  status = (**base).root.AttributeGet(cname, &attrTypeKind, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) return;
  
// take this out because an attribute that is not set will not yet have a typekind,
// so if you are getting an attribute which was not set the call will fail here...
/*  if (attrTypeKind != *tk) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind is inappropriate for this routine", &status);
    if (rc) *rc = status;
    return;
  }*/

  // get the number of items on the attribute, compare to the buffer size
  lcount = (**base).root.AttributeGetItemCount(cname);
  if (lcount > *count) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                         "attribute has more items than array has space", &status);
    if (rc) *rc = status;
    return;
  }
  // now set *count to the actual number of items in the attribute
  *count = lcount;
  
  //   use the count to allocate llens
  llens = new int[lcount];
  
  //  use llens to get the lengths of all items on this attribute
  status = (**base).root.AttributeGet(cname, llens, lcount);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<lcount; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_BUFFER_SHORT,
                         "Attribute is too long for buffer", &status);
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
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) {
    delete [] llens;
    return;
  }
  
  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<lcount; i++) {
//printf("lens = %d\n", lens[i]);
//printf("strlen = %d\n", strlen(cvalue[i].c_str()));
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(const_cast<char*> (cvalue[i].c_str()), &valueList[j], lens[i]);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) {
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
      void FTN(c_esmc_attributegetvalue)(
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
      ESMC_TypeKind *tk,        // in - typekind
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
  ESMC_TypeKind attrTk;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.AttributeGet(cname, &attrTk, &attrItems);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc))) return;

  if (attrTk != *tk) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "attribute value not expected kind", &status);
    if (rc) *rc = status;
    return;
  }
  
  if (attrItems > *items) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                         "item count is off", &status);
    if (rc) *rc = status;
    return;
  }
  
  if (value) {
/*    if (*items == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = (**base).root.AttributeGet(cname, (static_cast<ESMC_I4*> (value)));  
      else if (*tk == ESMC_TYPEKIND_I8)
        status = (**base).root.AttributeGet(cname, (static_cast<ESMC_I8*> (value)));
      else if (*tk == ESMC_TYPEKIND_R4)
        status = (**base).root.AttributeGet(cname, (static_cast<ESMC_R4*> (value)));
      else if (*tk == ESMC_TYPEKIND_R8)
        status = (**base).root.AttributeGet(cname, (static_cast<ESMC_R8*> (value)));
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = (**base).root.AttributeGet(cname, (static_cast<ESMC_Logical*> (value)));
      else {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
    }
    else if (*items > 1) {*/
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*items);
        status = (**base).root.AttributeGet(cname, items, &temp);
        for (unsigned int i=0; i<*items; i++)
          (static_cast<ESMC_I4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*items);
        status = (**base).root.AttributeGet(cname, items, &temp);
        for (unsigned int i=0; i<*items; i++)
          (static_cast<ESMC_I8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*items);
        status = (**base).root.AttributeGet(cname, items, &temp);
        for (unsigned int i=0; i<*items; i++)
          (static_cast<ESMC_R4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*items);
        status = (**base).root.AttributeGet(cname, items, &temp);
        for (unsigned int i=0; i<*items; i++)
          (static_cast<ESMC_R8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*items);
        status = (**base).root.AttributeGet(cname, items, &temp);
        for (unsigned int i=0; i<*items; i++)
          (static_cast<ESMC_Logical*> (value))[i] = temp[i];
        temp.clear();
      } else {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
/*    }
    else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", &status);
      if (rc) *rc = status;
      return;
    }*/
  }

  if (rc) *rc = status;

}  // end c_ESMC_AttributeGetValue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetInfoName - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmc_attributegetinfoname)(
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
      ESMC_TypeKind *tk,        // out - typekind
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if (!tk) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute typekind", &status);
      if (rc) *rc = status;
      return;
  }

  if (!count) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.AttributeGet(cname, tk, count);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeGetInfoName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetInfoNum - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmc_attributegetinfonum)(
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
      ESMC_TypeKind *tk,        // out - typekind
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name)) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if (!tk) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute typekind", &status);
      if (rc) *rc = status;
      return;
  }

  if (!count) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", &status);
      if (rc) *rc = status;
      return;
  }

  // declare string for name
  string cname;

  status = (**base).root.AttributeGet((*num)-1, &cname, tk, count);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  status = ESMC_CtoF90string(const_cast<char*> (cname.c_str()), name, nlen);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeGetInfoNum


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetCount - get number of attrs
//
// !INTERFACE:
      void FTN(c_esmc_attributegetcount)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  if (!count) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", &status);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "invalid value for attcountflag", &status);
    if (rc) *rc = status;
    return;
  }

  if (count <= 0) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute count", &status);
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
      void FTN(c_esmc_attributeispresent)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (!present) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute present flag", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.AttributeIsPresent(cname, present);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attributeispresent

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeLink - Link an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmc_attributelink)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", &status);
    if (rc) *rc = status;    
    return;
  }

  if (!linkChange) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad linkChange", &status);
    if (rc) *rc = status;    
    return;
  }

  // Set the attribute link on the object.
  status = (**source).root.AttributeLink(&(**destination).root, linkChange);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeLink

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeLinkRemove - Remove a link an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmc_attributelinkremove)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", &status);
    if (rc) *rc = status;    
    return;
  }

  if (!linkChange) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad linkChange", &status);
    if (rc) *rc = status;    
    return;
  }

  // Set the attribute link on the object.
  status = (**source).root.AttributeLinkRemove(&(**destination).root, linkChange);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeLinkRemove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_esmc_attributeremove - Remove the attribute
//
// !INTERFACE:
      void FTN(c_esmc_attributeremove)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.AttributeRemove(cname);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_esmc_attributeremove
/*
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetChar - Set String Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributesetchar()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      char *value,              // in - char string
      int *rc,                  // in - return code
      ESMCI_FortranStrLenArg nlen,// hidden/in - strlen count for name
      ESMCI_FortranStrLenArg vlen) { // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//     Character strings have this special version since they come in
//     with an additional hidden length argument.
//
//EOP

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if (!value) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cvalue(value, vlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cvalue.resize(cvalue.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object
  status = (**base).root.AttributeSet(cname, &cvalue);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeSetChar*/

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetCharList - Set String Attribute List on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetcharlist)(
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
      ESMC_TypeKind *tk,        // in - typekind
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", &status);
      if (rc) *rc = status;
      return;
  }
  
  // allocate space for the array of char*'s and vector of strings
  vector<string> cvalue;
  cvalue.reserve(*count);

  // check that valueList is allocated
  if (!valueList) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
  }

  // loop through valueList allocating space and copying values to cvalue
  j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (j > vlen) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
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
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeSetCharList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetValue - Set Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetvalue)(
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
      ESMC_TypeKind *tk,        // in - typekind
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
    if (rc) *rc = status;
    return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion, name must not be empty", &status);
    if (rc) *rc = status;
    return;
  }

  if (value) {
/*    if (*count == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = (**base).root.AttributeSet(cname, *(static_cast<ESMC_I4*> (value)));  
      else if (*tk == ESMC_TYPEKIND_I8)
        status = (**base).root.AttributeSet(cname, *(static_cast<ESMC_I8*> (value)));
      else if (*tk == ESMC_TYPEKIND_R4)
        status = (**base).root.AttributeSet(cname, *(static_cast<ESMC_R4*> (value)));
      else if (*tk == ESMC_TYPEKIND_R8)
        status = (**base).root.AttributeSet(cname, *(static_cast<ESMC_R8*> (value)));
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = (**base).root.AttributeSet(cname, *(static_cast<ESMC_Logical*> (value)));
      else {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
    }
    else if (*count > 1) { */
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
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE,
                         "typekind was inappropriate for this routine", &status);
        if (rc) *rc = status;
        return;
      }
/*    }
    else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF,
                       "the number of items is inappropriate", &status);
      if (rc) *rc = status;
      return;
    }*/
  }
    
  if (rc) *rc = status;

}  // end c_ESMC_AttributeSetValue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetObjChrInTree - Set a Char Attribute on all
//                                           objects in an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmc_attributesetobjchrintree)(
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

  ESMC_TypeKind tk;
  int count, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if ((!value) || (vlen <= 0) || (value[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute character string value", &status);
      if (rc) *rc = status;
      return;
  }

  string cvalue(value, vlen);
  cvalue.resize(cvalue.find_last_not_of(" ")+1);

  if (cvalue.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute character string value conversion",
                          &status);
      if (rc) *rc = status;
      return;
  }
  
  // Re-use c_ESMC_AttributeSetObjsInTree() to set the attribute on the object.
  tk = ESMC_TYPEKIND_CHARACTER;
  count = 1;
  FTN(c_esmc_attributesetobjsintree)(base, object, name, 
      &tk, &count, (void *)&cvalue, &status, olen, nlen); 
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeSetObjChrInTree
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetObjsInTree - Set an Attribute on all objects
//                                               in an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmc_attributesetobjsintree)(
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
      ESMC_TypeKind *tk,        // in - typekind of the Attribute
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cobject(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cobject.resize(cobject.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobject.empty()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.AttributeSetObjsInTree(cname,cobject,*tk,*count,value);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeSetObjsInTree

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeUpdate - Update an Attribute
//
// !INTERFACE:
      void FTN(c_esmc_attributeupdate)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  //check the VM
  if (*vm == ESMC_NULL_POINTER){
    *vm = ESMCI::VM::getCurrent(&status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }

  // make list into a vector
  rootListl.reserve(*count);
  for (unsigned int i=0; i<*count; ++i)
    rootListl.push_back(rootList[i]);

  // Update the Attribute
  status = (**base).root.AttributeUpdate(*vm, rootListl);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeUpdate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeUpdateReset - Reset flags in an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmc_attributeupdatereset)(
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  // Update the Attribute
  status = (**base).root.AttributeUpdateReset();
  ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMCI_ERR_PASSTHRU,
    ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_AttributeUpdate

#undef  ESMC_METHOD

} // extern "C"
