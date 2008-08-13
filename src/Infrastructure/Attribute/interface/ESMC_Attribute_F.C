// $Id: ESMC_Attribute_F.C,v 1.17 2008/08/13 14:53:22 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Attribute method interface (from F90 to C++) file
#define ESMF_FILENAME "ESMC_Attribute_F.C"

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
#include "ESMC_Attribute.h"
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Attribute_F.C,v 1.17 2008/08/13 14:53:22 rokuingh Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes ESMC_Attribute routine interfaces
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
// !IROUTINE:  c_ESMC_attpackcreate - Setup the attribute package
//
// !INTERFACE:
      void FTN(c_esmc_attpackcreate)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attpackcreate()"
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
  char *cname, *cconv, *cpurp, *cobj;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      delete [] cname;
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      delete [] cname;
      delete [] cconv;
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      delete [] cname;
      delete [] cconv;
      delete [] cpurp;
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttPackCreate(cname, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
  }
  
  delete [] cname;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpackcreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpackremove - Remove the attribute package
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
  char *cname, *cconv, *cpurp, *cobj;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
      delete [] cname;
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
      delete [] cname;
      delete [] cconv;
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      delete [] cname;
      delete [] cconv;
      delete [] cpurp;
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeRemove(cname, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed removing attribute package", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }
  
  delete [] cname;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpackremove

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpackgetchar - get attribute from an attpack
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
  char *cname, *cvalue, *cconv, *cpurp, *cobj;
  int slen;              // actual attribute string length

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cname;
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // get the length and type of the Attribute from the attpack 
  status = ((**base).root.ESMC_AttPackGet(cconv, cpurp, cobj))->\
    ESMC_AttributeGet(cname, &attrTypeKind, &slen, NULL);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.Write(
                          "failed getting typekind - looking for default value",
                          ESMC_LOG_INFO);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  // make sure destination will be long enough
  if (slen > vlen) {
    sprintf(msgbuf,"attribute %s is %d bytes long, buffer length %d is too short",
      name, slen, vlen);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return; 
  }

  cvalue = new char[slen+1];
  if (!cvalue) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value allocation", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  status = ((**base).root.ESMC_AttPackGet(cconv, cpurp, cobj))->\
    ESMC_AttributeGet(cname, cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting the attpack attribute value", &status);
    delete [] cname;
    delete [] cvalue;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  status = ESMC_CtoF90string(cvalue, value, vlen);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
  }
  
  delete [] cname;
  delete [] cvalue;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpackgetchar

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpackgetcharlist - get attribute from an attpack
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
  char *cname, *cconv, *cpurp, *cobj;
  char **lcvalue;
  int* llens;
  int lcount;
  ESMC_Attribute *attr;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cname;
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // get the Attribute package
  attr = (**base).root.ESMC_AttPackGet(cconv, cpurp, cobj);
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting Attribute package", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  // get type of the Attribute from the attpack, do not return error (default value possible)
  status = attr->ESMC_AttributeGet(cname, &attrTypeKind, NULL, NULL);
  if (status != ESMF_SUCCESS || attrTypeKind != *tk) {
    ESMC_LogDefault.Write(
                          "failed getting typekind - looking for default value",
                          ESMC_LOG_INFO);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  // we need to get the count first 
  lcount = attr->ESMC_AttributeGetItemCount(cname);
  if (lcount != *count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "itemcount-in does not match itemcount of attribute", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }
  
  //   use the count to allocate llens
  llens = new int[*count];
  
  //  use llens to get the lengths of all items on this attribute
  status = attr->ESMC_AttributeGet(cname, llens, *count);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting item char* lengths", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    delete [] llens;
    if (rc) *rc = status;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<*count; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      sprintf(msgbuf,"attribute %s item #%d is %d bytes long, buffer length %d is too short",
        cname, i+1, lens[i], llens[i]);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
      delete [] cname;
      delete [] cconv;
      delete [] cpurp;
      delete [] cobj;
      delete [] llens;
      if (rc) *rc = status;
      return;
    }
  }
  
  // allocate all char**s
  lcvalue = new char*[*count];
  for (i=0; i<*count; i++) {
    // allocate space for each char*
    lcvalue[i] = new char[llens[i]+1];
    if (!(lcvalue[i])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value allocation", &status);
      delete [] cname;
      delete [] cconv;
      delete [] cpurp;
      delete [] cobj;
      delete [] llens;
      delete [] lcvalue;
      if (rc) *rc = status;
      return;
    }
  }

  // next we get all the strings into the char**
  status = attr->ESMC_AttributeGet(cname, &attrTypeKind, NULL, lcvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    delete [] llens;
    delete [] lcvalue;
    delete [] lcvalue;
    if (rc) *rc = status;
    return;
  }

  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<*count; i++) {
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(lcvalue[i], &valueList[j], lens[i]);
    if (status != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    delete [] llens;
    delete [] lcvalue;
    if (rc) *rc = status;
    return;
    }
    j = j + lens[i];
  }
      
  delete [] cname;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  delete [] llens;
  delete [] lcvalue;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpackgetcharlist

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpackgetvalue - get attribute from an attpack
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
  char *cname, *cconv, *cpurp, *cobj;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cname;
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  status = ((**base).root.ESMC_AttPackGet(cconv, cpurp, cobj))->\
    ESMC_AttributeGet(cname, &attrTk, &attrCount, NULL);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                    "failed getting attribute type and count", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  if (attrTk != *tk) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected kind", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  if (attrCount != *count) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected count", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    delete [] cobj;
    if (rc) *rc = status;
    return;
  }

  status = ((**base).root.ESMC_AttPackGet(cconv, cpurp, cobj))->ESMC_AttributeGet(cname, NULL, NULL, value);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
  }
  
  delete [] cname;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpackgetvalue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpackispresent - Query for an Attribute package Attribute
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
  char *cname, *cconv, *cpurp, *cobj;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cname;
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttPackIsPresent(cname, cconv, cpurp, cobj, present);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed query for Attribute package Attribute", &status);
  }
  
  delete [] cname;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpackispresent

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpacksetchar - Set attributes in the attribute package
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
  char *cname, *cvalue, *cconv, *cpurp, *cobj;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cvalue = ESMC_F90toCstring(value, vlen);
  if (!cvalue) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cname;
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttPackSet(cname, *tk, 1, cvalue, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
  }
  
  delete [] cname;
  delete [] cvalue;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpacksetchar

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpacksetcharlsit - Set attributes in the attribute package
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
  char *cname, *temp, *cconv, *cpurp, *cobj;
  char **cvalue;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cname;
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // allocate space for the array of char*'s
  cvalue = new char*[(*count)];

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
    temp = ESMC_F90toCstring((&valueList[j]), lens[i]);
    if (!(temp)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
      delete [] cname;
      delete [] cconv;
      delete [] cpurp;
      delete [] cobj;
      delete [] cvalue;
      delete [] cvalue;
      if (rc) *rc = status;
      return;
    }
    cvalue[i] = new char[lens[i]];
    strcpy(cvalue[i], temp);
    j = j + lens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttPackSet(cname, *tk, *count, cvalue, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute char* value", &status);
  }
  
  delete [] cname;
  delete [] temp;
  delete [] cvalue;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpacksetcharlist

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attpacksetvalue - Set attributes in the attribute package
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
  char *cname, *cconv, *cpurp, *cobj;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cname;
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cname;
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttPackSet(cname, *tk, *count, value, cconv, cpurp, cobj);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
  }

  delete [] cname;
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpacksetvalue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attributewritetab - Setup the attribute package
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
  char *cconv, *cpurp, *cobj, *ctarobj;
  int temp = 0;
  
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

  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  ctarobj = ESMC_F90toCstring(targetobj, tlen);
  if (!ctarobj) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", &status);
      delete [] cconv;
      delete [] cpurp;
      delete [] cobj;
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeWriteTab(cconv, cpurp, cobj, ctarobj,
    (*base)->ESMC_Base::ESMC_BaseGetName(), temp);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed attributewritetab", &status);
  }

  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  delete [] ctarobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attributewritetab

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attributewritexml - Setup the attribute package
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
  char *cconv, *cpurp, *cobj, *ctarobj;
  
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

  // copy and convert F90 string to null terminated one
  cconv = ESMC_F90toCstring(convention, clen);
  if (!cconv) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute convention conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cpurp = ESMC_F90toCstring(purpose, plen);
  if (!cpurp) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute purpose conversion", &status);
    delete [] cconv;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  cobj = ESMC_F90toCstring(object, olen);
  if (!cobj) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
    delete [] cconv;
    delete [] cpurp;
    if (rc) *rc = status;
    return;
  }

  // copy and convert F90 string to null terminated one
  ctarobj = ESMC_F90toCstring(targetobj, tlen);
  if (!ctarobj) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute target object conversion", &status);
      delete [] cconv;
      delete [] cpurp;
      delete [] cobj;
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeWriteXML(cconv, cpurp, cobj, ctarobj, 
    (*base)->ESMC_Base::ESMC_BaseGetName());
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed attributewritexml", &status);
  }
  
  delete [] cconv;
  delete [] cpurp;
  delete [] cobj;
  delete [] ctarobj;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attpackwritexml

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeCopyAll - copy an attribute hierarchy between objects
//
// !INTERFACE:
      void FTN(c_esmc_attributecopyall)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributecopyall()"
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
  ESMC_Attribute *temp;
  
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

  status = (**destination).root.ESMC_AttributeCopyAll(*source);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed attributecopyall", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_AttributeCopyAll

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attributeremove - Remove the attribute
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
      int nlen) {                // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//    Remove an attribute package
//
//EOP

  int status;
  char *cname;

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
  
  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeRemove(cname);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed removing the attribute", &status);
    if (rc) *rc = status;
    delete [] cname;
    return;
  }
  
  delete [] cname;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attributeremove

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
  char *cname, *cvalue;
  int slen;              // actual attribute string length

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMC_AttributeGet(cname, &attrTypeKind, &slen, NULL);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.Write(
                          "failed getting typekind - looking for default value",
                          ESMC_LOG_INFO);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // make sure destination will be long enough
  if (slen > vlen) {
    sprintf(msgbuf,"attribute %s is %d bytes long, buffer length %d is too short",
      name, slen, vlen);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
    delete [] cname;
    if (rc) *rc = status;
    return; 
  }

  cvalue = new char[slen+1];
  if (!cvalue) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value allocation", &status);
      delete [] cname;
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMC_AttributeGet(cname, cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
    delete [] cname;
    delete [] cvalue;
    if (rc) *rc = status;
    return;
  }

  status = ESMC_CtoF90string(cvalue, value, vlen);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
  }
  
  delete [] cname;
  delete [] cvalue;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_AttributeGetChar

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
  char *cname;
  char **lcvalue;
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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  // check the typekind, do not return error (default value possible)
  status = (**base).root.ESMC_AttributeGet(cname, &attrTypeKind, NULL, NULL);
  if (status != ESMF_SUCCESS || attrTypeKind != *tk) {
    ESMC_LogDefault.Write(
                          "failed getting typekind - looking for default value",
                          ESMC_LOG_INFO);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  // we need to get the count first 
  lcount = (**base).root.ESMC_AttributeGetItemCount(cname);
  if (lcount != *count) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "itemcount-in does not match itemcount of attribute", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }
  
  //   use the count to allocate llens
  llens = new int[*count];
  
  //  use llens to get the lengths of all items on this attribute
  status = (**base).root.ESMC_AttributeGet(cname, llens, *count);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting item char* lengths", &status);
    delete [] cname;
    delete [] llens;
    if (rc) *rc = status;
    return;
  }

  //  check the llens against the supplied lens to make sure buffer is large enough
  for (i=0; i<*count; i++) {
    // make sure destination will be long enough
    if (lens[i] < llens[i]) {
      sprintf(msgbuf,"attribute %s item #%d is %d bytes long, buffer length %d is too short",
        name, i+1, lens[i], llens[i]);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         msgbuf, &status);
      delete [] cname;
      delete [] llens;
      if (rc) *rc = status;
      return;
    }
  }
  
  // allocate all char**s
  lcvalue = new char*[*count];
  for (i=0; i<*count; i++) {
    // allocate space for each char*
    lcvalue[i] = new char[llens[i]+1];
    if (!(lcvalue[i])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value allocation", &status);
      delete [] cname;
      delete [] llens;
      delete [] lcvalue;
      if (rc) *rc = status;
      return;
    }
  }

  // next we get all the strings into the char**
  status = (**base).root.ESMC_AttributeGet(cname, &attrTypeKind, NULL, lcvalue);
  //status = (**base).root.ESMC_AttributeGet(cname, tk, NULL, lcvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
    delete [] cname;
    delete [] llens;
    delete [] lcvalue;
    if (rc) *rc = status;
    return;
  }
  
  // finally we convert them all to f90 and pack them into char*
  j = 0;
  for (i=0; i<*count; i++) {
    // convert strings to F90 using F90 length
    status = ESMC_CtoF90string(lcvalue[i], &valueList[j], lens[i]);
    if (status != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
    delete [] cname;
    delete [] llens;
    delete [] lcvalue;
    if (rc) *rc = status;
    return;
    }
    j = j + lens[i];
  }
  
  delete [] cname;
  delete [] llens;
  delete [] lcvalue;
  
  if (rc) *rc = status;
  return;

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
  char *cname;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMC_AttributeGet(cname, &attrTk, &attrCount, NULL);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.Write("failed getting attribute type and count", ESMC_LOG_INFO);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  if (attrTk != *tk) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected kind", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }
  
  if (attrCount != *count) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected count", &status);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }
  
  status = (**base).root.ESMC_AttributeGet(cname, NULL, NULL, value);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute value", &status);
  }
  
  delete [] cname;
  
  if (rc) *rc = status;
  return;

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
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the typekind, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int status;
  char *cname;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMC_AttributeGet(cname, tk, count, NULL);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute info by name", &status);
  }
  
  delete [] cname;
  
  if (rc) *rc = status;
  return;

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
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the name, type, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int status;
  char *cname;

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

  cname = new char[ESMF_MAXSTR];
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed allocating attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  status = (**base).root.ESMC_AttributeGet((*num)-1, cname, tk, count, NULL);
  if (status != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute info by num", &status);
      delete [] cname;
      if (rc) *rc = status;
      return;
  }

  status = ESMC_CtoF90string(cname, name, nlen);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
  }
  
  delete [] cname;
  
  if (rc) *rc = status;
  return;

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

  *count = (**base).root.ESMC_AttributeGetCount();
  if (count <= 0) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed getting attribute count", &status);
    if (rc) *rc = status;
    return;
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_AttributeGetCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_attributeispresent - Query for an Attribute
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
      int nlen) {                // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Query for the presence of an Attribute.
//
//EOP

  int status;
  char *cname;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeIsPresent(cname, present);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed query for Attribute", &status);
  }
  
  delete [] cname;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_attributeispresent

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
  char *cname, *cvalue;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cvalue = ESMC_F90toCstring(value, vlen);
  if (!cvalue) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
      delete [] cname;
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeSet(cname, cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute char value", &status);
  }

  delete [] cname;
  delete [] cvalue;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_AttributeSetChar

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
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//     Character strings have this special version since they come in
//     with an additional hidden length argument.
//
//EOP

  int j, status;
  char *cname, *temp;
  char **cvalue;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // allocate space for the array of char*'s
  cvalue = new char*[(*count)];

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
    temp = ESMC_F90toCstring((&valueList[j]), lens[i]);
    if (!(temp)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value conversion", &status);
      delete [] cname;
      delete [] cvalue;
      if (rc) *rc = status;
      return;
    }
    cvalue[i] = new char[lens[i]];
    strcpy(cvalue[i], temp);
    j = j + lens[i];
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeSet(cname, *tk, *count, cvalue);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute char* value", &status);
  }

  delete [] cname;
  delete [] temp;
  delete [] cvalue;
  
  if (rc) *rc = status;
  return;

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
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//     Any type or list of types can be passed except character strings
//     since they come with an additional hidden length argument.
//
//EOP

  int status;
  char *cname;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
    if (rc) *rc = status;
    return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeSet(cname, *tk, *count, value);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute value", &status);
  }
  
  delete [] cname;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_AttributeSetValue

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetLink - Set a link in an Attribute hierarchy
//
// !INTERFACE:
      void FTN(c_esmc_attributesetlink)(
//
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_attributesetlink()"
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
  status = (**source).root.ESMC_AttributeSetLink(*destination);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed setting attribute link", &status);
  }
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_AttributeSetLink

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
      int olen,                 // hidden/in - strlen count for object
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Change the Attribute values for Attribute <name> on all <object>s.
//
//EOP

  int i, status;
  char *cobject, *cname;

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

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name conversion", &status);
      if (rc) *rc = status;
      return;
  }
  
  // copy and convert F90 string to null terminated one
  cobject = ESMC_F90toCstring(object, olen);
  if (!cobject) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute object conversion", &status);
      delete [] cname;
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  status = (**base).root.ESMC_AttributeSetObjsInTree(cobject,cname,*tk,*count,value);
  if (status != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "failed call AttributeSetObjsInTree", &status);
  }

  delete [] cname;
  delete [] cobject;
  
  if (rc) *rc = status;
  return;

}  // end c_ESMC_AttributeSetObjsInTree

#undef  ESMC_METHOD

} // extern "C"
