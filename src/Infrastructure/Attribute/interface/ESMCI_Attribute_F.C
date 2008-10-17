// $Id: ESMCI_Attribute_F.C,v 1.2 2008/10/17 20:07:49 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
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
#include "ESMC_Start.h"
#include "ESMCI_Attribute.h"
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Attribute_F.C,v 1.2 2008/10/17 20:07:49 rokuingh Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes ESMCI_Attribute routine interfaces
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
// !IROUTINE:  c_ESMCI_attpackcreate - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attpackcreate)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpackcreate()"
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
      int nlen,                  // hidden/in - strlen count for name
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen) {                // hidden/in - strlen count for object
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttPackCreate(cname, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackcreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpackremove - Remove the attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attpackremove)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpackremove()"
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
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen) {                // hidden/in - strlen count for object
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttPackRemove(cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed removing attribute package", &status);
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackremove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpackremoveattribute - Remove an attribute from an
//                                              attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attpackremoveattribute)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpackremoveattribute()"
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
      int nlen,                  // hidden/in - strlen count for name
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen) {                // hidden/in - strlen count for object
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cpurp.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttPackRemoveAttribute(cname, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed removing attribute package", &status);
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackremoveattribute

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpackgetchar - get attribute from an attpack
//
// !INTERFACE:
      void FTN(c_esmci_attpackgetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpackgetchar()"
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
      int *rc,                  // in - return code
      int nlen,                 // hidden/in - strlen count for name
      int vlen,                 // hidden/in - strlen count for value
      int clen,                 // hidden/in - strlen count for convention
      int plen,                 // hidden/in - strlen count for purpose
      int olen) {               // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status;
  char msgbuf[ESMF_MAXSTR];
  ESMC_TypeKind attrTypeKind;
  int slen;              // actual attribute string length
  int *llens;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  //   use the count to allocate llens
  llens = new int[1];
  
  //  use llens to get the lengths of all items on this attribute
  status = (**base).root.ESMCI_AttPackGet(cconv, cpurp, cobj)->\
    ESMCI_AttributeGet(cname, llens, 1);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.Write(
                         "failed getting item char* lengths", ESMC_LOG_INFO);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }
  slen = llens[0];

  // make sure destination will be long enough
  if (slen > vlen) {
    sprintf(msgbuf,"attribute %s is %d bytes long, buffer length %d is too short",
      name, slen, vlen);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
    delete [] llens;
    if (rc) *rc = status;
    return; 
  }

  string cvalue;
  status = ((**base).root.ESMCI_AttPackGet(cconv, cpurp, cobj))->\
    ESMCI_AttributeGet(cname, &cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting the attpack attribute value", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }

  status = ESMC_CtoF90string(const_cast<char*> (cvalue.c_str()), value, cvalue.size());
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
  }

  delete [] llens;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackgetchar

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpackgetcharlist - get attribute from an attpack
//
// !INTERFACE:
      void FTN(c_esmci_attpackgetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpackgetcharlist()"
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
      int *rc,                  // in - return code
      int nlen,                 // hidden/in - strlen count for name
      int vlen,                 // hidden/in - strlen count for value
      int clen,                 // hidden/in - strlen count for convention
      int plen,                 // hidden/in - strlen count for purpose
      int olen) {               // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status, j;
  unsigned int i,k;
  char msgbuf[ESMF_MAXSTR];
  ESMC_TypeKind attrTypeKind;
  int* llens;
  int lcount;
  ESMCI::ESMCI_Attribute *attr;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // get the Attribute package
  attr = (**base).root.ESMCI_AttPackGet(cconv, cpurp, cobj);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting Attribute package", &status);
    if (rc) *rc = status;
    return;
  }

  // get type of the Attribute from the attpack, do not return error (default value possible)
  status = attr->ESMCI_AttributeGet(cname, &attrTypeKind, NULL);
  if (status != ESMF_SUCCESS || attrTypeKind != *tk) {
    ESMC_LogDefault.Write(
                          "failed getting typekind - looking for default value",
                          ESMC_LOG_INFO);
    if (rc) *rc = status;
    return;
  }

  // we need to get the count first 
  lcount = attr->ESMCI_AttributeGetItemCount(cname);
  if (lcount > *count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "itemcount-in is less than the itemcount of the attribute", &status);
    if (rc) *rc = status;
    return;
  }
  // set the itemcount out to the itemcount of the attribute
  *count = lcount;
  
  //   use the count to allocate llens
  llens = new int[lcount];
  
  //  use llens to get the lengths of all items on this attribute
  status = attr->ESMCI_AttributeGet(cname, llens, lcount);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting item char* lengths", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<lcount; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      sprintf(msgbuf,"attribute %s item #%d is %d bytes long, buffer length %d is too short",
        cname.c_str(), i+1, lens[i], llens[i]);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
      delete [] llens;
      if (rc) *rc = status;
      return;
    }
    lens[i] = llens[i];
  }
  
  vector<string> lcvalue;

  // next we get all the strings into the char**
  status = attr->ESMCI_AttributeGet(cname, &lcvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }
  
  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<lcount; i++) {
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(const_cast<char*> (lcvalue[i].c_str()), &valueList[j], 
    (lcvalue[i]).size());
    if (status != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
    }
    j = j + lens[i];
  }
      
  delete [] llens;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackgetcharlist

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpackgetvalue - get attribute from an attpack
//
// !INTERFACE:
      void FTN(c_esmci_attpackgetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpackgetvalue()"
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
      int *rc,                  // in - return code
      int nlen,                 // hidden/in - strlen count for name
      int clen,                 // hidden/in - strlen count for convention
      int plen,                 // hidden/in - strlen count for purpose
      int olen) {               // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Return the (name,value) pair from any object type in the system.
//
//EOP

  int status, attrCount;
  ESMC_TypeKind attrTk;
  ESMCI::ESMCI_Attribute *attpack;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  attpack = (**base).root.ESMCI_AttPackGet(cconv, cpurp, cobj);
  if (!attpack) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                    "failed getting attribute package", &status);
    if (rc) *rc = status;
    return;
  }

  status = attpack->ESMCI_AttributeGet(cname, &attrTk, &attrCount);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                    "failed getting attribute type and count", &status);
    if (rc) *rc = status;
    return;
  }

  if (attrTk != *tk) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected kind", &status);
    if (rc) *rc = status;
    return;
  }

  if (attrCount > *count) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute has more items than array has space", &status);
    if (rc) *rc = status;
    return;
  }

  if (value) {
    if (*count == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = attpack->ESMCI_AttributeGet(cname, (static_cast<ESMC_I4*> (value)));  
      else if (*tk == ESMC_TYPEKIND_I8)
        status = attpack->ESMCI_AttributeGet(cname, (static_cast<ESMC_I8*> (value)));
      else if (*tk == ESMC_TYPEKIND_R4)
        status = attpack->ESMCI_AttributeGet(cname, (static_cast<ESMC_R4*> (value)));
      else if (*tk == ESMC_TYPEKIND_R8)
        status = attpack->ESMCI_AttributeGet(cname, (static_cast<ESMC_R8*> (value)));
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = attpack->ESMCI_AttributeGet(cname, (static_cast<ESMC_Logical*> (value)));
      else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
      }
    }
    else if (*count > 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        status = attpack->ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_I4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        status = attpack->ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_I8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        status = attpack->ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_R4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        status = attpack->ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_R8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        status = attpack->ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_Logical*> (value))[i] = temp[i];
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
    }
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackgetvalue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpackispresent - Query for an Attribute package Attribute
//
// !INTERFACE:
      void FTN(c_esmci_attpackispresent)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpackispresent()"
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
      ESMC_Logical *present,     // out/out - present flag 
      int *rc,                   // in/out - return code
      int nlen,                  // hidden/in - strlen count for name
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen) {                // hidden/in - strlen count for object
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttPackIsPresent(cname, cconv, cpurp, cobj, present);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed query for Attribute package Attribute", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackispresent

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpacksetchar - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attpacksetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpacksetchar()"
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
      int *rc,                   // in - return code
      int nlen,                  // hidden/in - strlen count for name
      int vlen,                  // hidden/in - strlen count for value
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen) {                // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if ((!value) || (vlen <= 0) || (value[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cvalue.empty()) {
      ESMC_LogDefault.Write("Attribute has an empty value argument",
                              ESMC_LOG_INFO);
      cvalue = '\0';
  }

  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttPackSet(cname, *tk, 1, &cvalue, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpacksetchar

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpacksetcharlsit - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attpacksetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpacksetcharlist()"
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
      int *rc,                   // in - return code
      int nlen,                  // hidden/in - strlen count for name
      int vlen,                  // hidden/in - strlen count for value
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen) {                // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int j, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // allocate space for the array of char*'s and vector of strings
  vector<string> cvalue;
  cvalue.reserve(*count);

  // loop through valueList allocating space and copying values to cvalue
  j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (!(valueList[j]) || (lens[i] <= 0)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&valueList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    if (temp.empty()) {
      ESMC_LogDefault.Write("Attribute has an empty value argument",
                              ESMC_LOG_INFO);
      temp = '\0';
    }
    cvalue.push_back(temp);
    j = j + lens[i];
  }
  
  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count, &cvalue, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute char* value", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpacksetcharlist

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attpacksetvalue - Set attributes in the attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attpacksetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attpacksetvalue()"
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
      int *rc,                   // in - return code
      int nlen,                  // hidden/in - strlen count for name
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen) {                // hidden/in - strlen count for object
// 
// !DESCRIPTION:
//     Set the convention, purpose, and object type on an attribute package
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cconv.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (value) {
    if (*count == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_I4*> (value)), cconv, cpurp, cobj);  
      else if (*tk == ESMC_TYPEKIND_I8)
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_I8*> (value)), cconv, cpurp, cobj);
      else if (*tk == ESMC_TYPEKIND_R4)
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_R4*> (value)), cconv, cpurp, cobj);
      else if (*tk == ESMC_TYPEKIND_R8)
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_R8*> (value)), cconv, cpurp, cobj);
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count,
          (static_cast<ESMC_Logical*> (value)), cconv, cpurp, cobj);
      else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
      }
    }
    else if (*count > 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I4*> (value))[i]);
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count, &temp, cconv, cpurp, cobj);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I8*> (value))[i]);
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count, &temp, cconv, cpurp, cobj);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R4*> (value))[i]);
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count, &temp, cconv, cpurp, cobj);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R8*> (value))[i]);
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count, &temp, cconv, cpurp, cobj);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_Logical*> (value))[i]);
        status = (**base).root.ESMCI_AttPackSet(cname, *tk, *count, &temp, cconv, cpurp, cobj);
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
    }
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpacksetvalue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attributewritetab - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attributewritetab)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributewritetab()"
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
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen,                  // hidden/in - strlen count for object
      int tlen) {                // hidden/in - strlen count for target object
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!targetobj) || (tlen <= 0) || (targetobj[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (ctarobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttributeWriteTab(cconv, cpurp, cobj, ctarobj,
    (*base)->ESMC_Base::ESMC_BaseGetName());
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed attributewritetab", &status);
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attributewritetab

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attributewritexml - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmci_attributewritexml)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributewritexml()"
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
      int clen,                  // hidden/in - strlen count for convention
      int plen,                  // hidden/in - strlen count for purpose           
      int olen,                  // hidden/in - strlen count for object
      int tlen) {                // hidden/in - strlen count for target object
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!convention) || (clen <= 0) || (convention[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!purpose) || (plen <= 0) || (purpose[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose", &status);
      if (rc) *rc = status;
      return;
  }
  
  // simple sanity check before doing any more work
  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  // simple sanity check before doing any more work
  if ((!targetobj) || (tlen <= 0) || (targetobj[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cpurp.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (cobj.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (ctarobj.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttributeWriteXML(cconv, cpurp, cobj, ctarobj, 
    (*base)->ESMC_Base::ESMC_BaseGetName());
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed attributewritexml", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attpackwritexml

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeCopyAll - copy an attribute hierarchy between objects
//
// !INTERFACE:
      void FTN(c_esmci_attributecopyall)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributecopyall()"
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
//     Copy the Attribute hierarchy from Base1 to Base2
//
//EOP

  int status;
  
  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!source) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad source base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", &status);
    if (rc) *rc = status;    
    return;
  }

  status = (**destination).root.ESMCI_AttributeCopyAll(*source);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed attributecopyall", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeCopyAll

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attributeremove - Remove the attribute
//
// !INTERFACE:
      void FTN(c_esmci_attributeremove)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributeremove()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      int *rc,                   // in - return code     
      int nlen) {                // hidden/in - strlen count for name
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttributeRemove(cname);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed removing the attribute", &status);
    if (rc) *rc = status;
    return;
  }
    
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attributeremove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeGetChar - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmci_attributegetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributegetchar()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      char *value,              // out - character value
      int *rc,                  // in - return code
      int nlen,                 // hidden/in - strlen count for name
      int vlen) {               // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status;
  char msgbuf[ESMF_MAXSTR];
  ESMC_TypeKind attrTypeKind;
  int slen;              // actual attribute string length
  int *llens;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  //   use the count to allocate llens
  llens = new int[1];
  
  //  use llens to get the lengths of all items on this attribute
  status = (**base).root.ESMCI_AttributeGet(cname, llens, 1);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.Write(
                         "failed getting item char* lengths", ESMC_LOG_INFO);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }
  slen = llens[0];

  // make sure destination will be long enough
  if (slen > vlen) {
    sprintf(msgbuf,"attribute %s is %d bytes long, buffer length %d is too short",
      name, slen, vlen);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
    delete [] llens;
    if (rc) *rc = status;
    return; 
  }

  string cvalue;
  status = (**base).root.ESMCI_AttributeGet(cname, &cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }

  status = ESMC_CtoF90string(const_cast<char*> (cvalue.c_str()), value, vlen);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
  }
    
  delete [] llens;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeGetChar

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeGetCharList - get attribute list from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmci_attributegetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributegetcharlist()"
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
      int nlen) {               // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Retrieve a (name,value) pair from any object type in the system.
//
//EOP

  int status, j;
  unsigned int i,k;
  char msgbuf[ESMF_MAXSTR];
  ESMC_TypeKind attrTypeKind;
  int *llens;
  int lcount;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
 
  // allocate space for the name
  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // check the typekind, do not return error (default value possible)
  status = (**base).root.ESMCI_AttributeGet(cname, &attrTypeKind, NULL);
  if (status != ESMF_SUCCESS || attrTypeKind != *tk) {
    ESMC_LogDefault.Write(
                          "failed getting typekind - looking for default value",
                          ESMC_LOG_INFO);
    if (rc) *rc = status;
    return;
  }

  // get the number of items on the attribute, compare to the buffer size
  lcount = (**base).root.ESMCI_AttributeGetItemCount(cname);
  if (lcount > *count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "itemcount-in is less than the itemcount of the attribute", &status);
    if (rc) *rc = status;
    return;
  }
  // now set *count to the actual number of items in the attribute
  *count = lcount;
  
  //   use the count to allocate llens
  llens = new int[lcount];
  
  //  use llens to get the lengths of all items on this attribute
  status = (**base).root.ESMCI_AttributeGet(cname, llens, lcount);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting item char* lengths", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<lcount; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      sprintf(msgbuf,"attribute %s item #%d is %d bytes long, buffer length %d is too short",
        cname.c_str(), i+1, lens[i], llens[i]);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
      delete [] llens;
      if (rc) *rc = status;
      return;
    }
    lens[i] = llens[i];
  }
  
  // allocate all char**s and string vector
  vector<string> cvalue;
  cvalue.reserve(lcount);

  // next we get all the strings into the char**
  status = (**base).root.ESMCI_AttributeGet(cname, &cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
  }
  
  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<lcount; i++) {
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(const_cast<char*> (cvalue[i].c_str()), &valueList[j], lens[i]);
    if (status != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
    delete [] llens;
    if (rc) *rc = status;
    return;
    }
    j = j + lens[i];
  }
  
  delete [] llens;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeGetCharList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeGetValue - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmci_attributegetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributegetvalue()"
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
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Return the (name,value) pair from any object type in the system.
//
//EOP

  int status, attrCount;
  ESMC_TypeKind attrTk;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMCI_AttributeGet(cname, &attrTk, &attrCount);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.Write("failed getting attribute type and count", ESMC_LOG_INFO);
    if (rc) *rc = status;
    return;
  }

  if (attrTk != *tk) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected kind", &status);
    if (rc) *rc = status;
    return;
  }
  
  if (attrCount > *count) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute has more items than array has space", &status);
    if (rc) *rc = status;
    return;
  }
  
  if (value) {
    if (*count == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = (**base).root.ESMCI_AttributeGet(cname, (static_cast<ESMC_I4*> (value)));  
      else if (*tk == ESMC_TYPEKIND_I8)
        status = (**base).root.ESMCI_AttributeGet(cname, (static_cast<ESMC_I8*> (value)));
      else if (*tk == ESMC_TYPEKIND_R4)
        status = (**base).root.ESMCI_AttributeGet(cname, (static_cast<ESMC_R4*> (value)));
      else if (*tk == ESMC_TYPEKIND_R8)
        status = (**base).root.ESMCI_AttributeGet(cname, (static_cast<ESMC_R8*> (value)));
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = (**base).root.ESMCI_AttributeGet(cname, (static_cast<ESMC_Logical*> (value)));
      else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
      }
    }
    else if (*count > 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        status = (**base).root.ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_I4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        status = (**base).root.ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_I8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        status = (**base).root.ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_R4*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        status = (**base).root.ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_R8*> (value))[i] = temp[i];
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        status = (**base).root.ESMCI_AttributeGet(cname, count, &temp);
        for (unsigned int i=0; i<*count; i++)
          (static_cast<ESMC_Logical*> (value))[i] = temp[i];
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
    }
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeGetValue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeGetInfoName - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmci_attributegetinfoname)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributegetinfoname()"
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
      int nlen) {               // hidden/in - strlen count for name
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if (!tk) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute typekind", &status);
      if (rc) *rc = status;
      return;
  }

  if (!count) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMCI_AttributeGet(cname, tk, count);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute info by name", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeGetInfoName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeGetInfoNum - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmci_attributegetinfonum)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributegetinfonum()"
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
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the name, type, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMF_RC_NOT_IMPL;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if (!tk) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute typekind", &status);
      if (rc) *rc = status;
      return;
  }

  if (!count) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed allocating attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMCI_AttributeGet((*num)-1, &cname, tk, count);
  if (status != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute info by num", &status);
      if (rc) *rc = status;
      return;
  }

  status = ESMC_CtoF90string(const_cast<char*> (cname.c_str()), name, nlen);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeGetInfoNum


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeGetCount - get number of attrs
//
// !INTERFACE:
      void FTN(c_esmci_attributegetcount)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributegetcount()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int *count,               // out - attribute count
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//   Return the count of attributes for any object type in the system.
//
//EOP

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  status = ESMF_SUCCESS;

  if (!base) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  if (!count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute count", &status);
    if (rc) *rc = status;
    return;
  }

  *count = (**base).root.ESMCI_AttributeGetCount();
  if (count <= 0) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute count", &status);
    if (rc) *rc = status;
    return;
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeGetCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_attributeispresent - Query for an Attribute
//
// !INTERFACE:
      void FTN(c_esmci_attributeispresent)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributeispresent()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,          // in/out - base object
      char *name,                // in - F90, non-null terminated string
      ESMC_Logical *present,     // out/out - present flag 
      int *rc,                   // in/out - return code
      int nlen) {                // hidden/in - strlen count for name
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (!present) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute present flag", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttributeIsPresent(cname, present);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed query for Attribute", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_attributeispresent

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeSetChar - Set String Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmci_attributesetchar)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributesetchar()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      char *value,              // in - char string
      int *rc,                  // in - return code
      int nlen,                 // hidden/in - strlen count for name
      int vlen) {               // hidden/in - strlen count for value
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if ((!value) || (vlen <= 0) || (value[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cvalue(value, vlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cvalue.resize(cvalue.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cvalue.empty()) {
      ESMC_LogDefault.Write("Attribute has an empty value argument",
                              ESMC_LOG_INFO);
      cvalue = '\0';
  }

  // Set the attribute on the object
  status = (**base).root.ESMCI_AttributeSet(cname, &cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute char value", &status);
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeSetChar

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeSetCharList - Set String Attribute List on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmci_attributesetcharlist)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributesetcharlist()"
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
      int nlen) {               // hidden/in - strlen count for name
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // allocate space for the array of char*'s and vector of strings
  vector<string> cvalue;
  cvalue.reserve(*count);

  // loop through valueList allocating space and copying values to cvalue
  j = 0;
  for (unsigned int i=0; i<(*count); i++) {
    if (!(valueList[j]) || (lens[i] <= 0)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      if (rc) *rc = status;
      return;
    }

    // copy and convert F90 string to null terminated one
    string temp((&valueList[j]), lens[i]);
    temp.resize(temp.find_last_not_of(" ")+1);
    if (temp.empty()) {
      ESMC_LogDefault.Write("Attribute has an empty value argument",
                              ESMC_LOG_INFO);
      temp = '\0';
    }
    cvalue.push_back(temp);
    j = j + lens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttributeSet(cname, cvalue.size(), &cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute char* value", &status);
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeSetCharList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeSetValue - Set Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmci_attributesetvalue)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributesetvalue()"
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
      int nlen) {               // hidden/in - strlen count for name
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
    if (rc) *rc = status;
    return;
  }

  string cname(name, nlen);
  cname.resize(cname.find_last_not_of(" ")+1);
  if (cname.empty()) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
    if (rc) *rc = status;
    return;
  }

  if (value) {
    if (*count == 1) {
      if (*tk == ESMC_TYPEKIND_I4)
        status = (**base).root.ESMCI_AttributeSet(cname, *(static_cast<ESMC_I4*> (value)));  
      else if (*tk == ESMC_TYPEKIND_I8)
        status = (**base).root.ESMCI_AttributeSet(cname, *(static_cast<ESMC_I8*> (value)));
      else if (*tk == ESMC_TYPEKIND_R4)
        status = (**base).root.ESMCI_AttributeSet(cname, *(static_cast<ESMC_R4*> (value)));
      else if (*tk == ESMC_TYPEKIND_R8)
        status = (**base).root.ESMCI_AttributeSet(cname, *(static_cast<ESMC_R8*> (value)));
      else if (*tk == ESMC_TYPEKIND_LOGICAL)
        status = (**base).root.ESMCI_AttributeSet(cname, *(static_cast<ESMC_Logical*> (value)));
      else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
      }
    }
    else if (*count > 1) {
      if (*tk == ESMC_TYPEKIND_I4) {
        vector<ESMC_I4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I4*> (value))[i]);
        status = (**base).root.ESMCI_AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_I8) {
        vector<ESMC_I8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_I8*> (value))[i]);
        status = (**base).root.ESMCI_AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R4) {
        vector<ESMC_R4> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R4*> (value))[i]);
        status = (**base).root.ESMCI_AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_R8) {
        vector<ESMC_R8> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_R8*> (value))[i]);
        status = (**base).root.ESMCI_AttributeSet(cname, *count, &temp);
        temp.clear();
      } else if (*tk == ESMC_TYPEKIND_LOGICAL) {
        vector<ESMC_Logical> temp;
        temp.reserve(*count);
        for (unsigned int i=0; i<*count; i++)
          temp.push_back((static_cast<ESMC_Logical*> (value))[i]);
        status = (**base).root.ESMCI_AttributeSet(cname, *count, &temp);
        temp.clear();
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "typekind was inappropriate for this routine", &status);
      }
    }
  }
    
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeSetValue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeSetLink - Set a link in an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmci_attributesetlink)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributesetlink()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **source,       // in/out - base object
      ESMC_Base **destination,  // in/out - base destination object
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
                         "bad source base", &status);
    if (rc) *rc = status;    
    return;
  }
  
  if (!destination) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad destination base", &status);
    if (rc) *rc = status;    
    return;
  }

  // Set the attribute link on the object.
  status = (**source).root.ESMCI_AttributeSetLink(*destination);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute link", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeSetLink

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMCI_AttributeSetObjsInTree - Set an Attribute on all objects
//                                               in an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmci_attributesetobjsintree)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_attributesetobjsintree()"
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
      int olen,                 // hidden/in - strlen count for object
      int nlen) {               // hidden/in - strlen count for name
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
                         "bad base", &status);
    if (rc) *rc = status;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  if ((!object) || (olen <= 0) || (object[0] == '\0')) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object", &status);
      if (rc) *rc = status;
      return;
  }

  string cname(name, nlen);
  string cobject(object, olen);
  cname.resize(cname.find_last_not_of(" ")+1);
  cobject.resize(cobject.find_last_not_of(" ")+1);

  if (cname.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  if (cobject.empty()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMCI_AttributeSetObjsInTree(cname,cobject,*tk,*count,value);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed call AttributeSetObjsInTree", &status);
  }

  if (rc) *rc = status;
  return;

}  // end c_ESMCI_AttributeSetObjsInTree

#undef  ESMC_METHOD

} // extern "C"
