// $Id: ESMC_Base_F.C,v 1.48.2.5 2009/01/21 21:25:19 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Base method interface (from F90 to C++) file
#define ESMF_FILENAME "ESMC_Base_F.C"

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable 
// interfaces to the C++ Base methods.
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"  // will this work?

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base_F.C,v 1.48.2.5 2009/01/21 21:25:19 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes ESMC_Base routine interfaces
//
//

extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  Base object methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseCreate - create and initialize a new Base object 
//
// !INTERFACE:
      void FTN(c_esmc_basecreate)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *superclass,         // in - F90, non-null terminated string
      char *name,               // in (opt) - F90, non-null terminated string
      int *nattrs,              // in - number of initial attributes to alloc
      int *rc,                  // out - return code
      int sclen,                // hidden/in - strlen count for superclass
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Create a new Base object.
//
//EOP

  int status;
  char *cname = NULL;
  char *scname = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // copy and convert F90 strings to null terminated ones
  if (superclass && (sclen > 0) && (superclass[0] != '\0')) {
      scname = ESMC_F90toCstring(superclass, sclen);
      if (!scname) {
           ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
          if (rc) *rc = status;
          return;
      }
  }
  if (name && (nlen > 0) && (name[0] != '\0')) {
      cname = ESMC_F90toCstring(name, nlen);
      if (!cname) {
          delete [] scname;
          if (rc) *rc = status;
          return;
      }
  } 

  (*base) = new ESMC_Base(scname, cname, *nattrs);
  if (*base != NULL)
      *rc = ESMF_SUCCESS;
  else
      *rc = ESMF_FAILURE;

  if (scname) delete [] scname;
  if (cname)  delete [] cname;
  return;

}  // end c_ESMC_BaseCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseDestroy - release resources from a Base object
//
// !INTERFACE:
      void FTN(c_esmc_basedestroy)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Free resources associated with a base object.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (base && *base)
      delete (*base);

  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BasePrint - print Base object 
//
// !INTERFACE:
      void FTN(c_esmc_baseprint)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *opts,               // in - F90, non-null terminated string
      int *rc,                  // out - return code
      int nlen) {               // hidden/in - strlen count for options
// 
// !DESCRIPTION:
//     Print the contents of a base object.
//
//EOP

  char *copts = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.ESMC_LogWrite("Base object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
    // for Print, it's not a failure for an uninit object to be printed
  }

  // copy and convert F90 string to null terminated one
  if (opts && (nlen > 0) && (opts[0] != '\0')) {
      copts = ESMC_F90toCstring(opts, nlen);
      if (!copts) {
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  *rc = (*base)->ESMC_Print(copts);

  if (copts)
      delete [] copts;
  return;

}  // end c_ESMC_BasePrint


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseSerialize - Serialize Base object 
//
// !INTERFACE:
      void FTN(c_esmc_baseserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *buf,                // in/out - really a byte stream
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a base object.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;


  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.ESMC_LogWrite("Base object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*base)->ESMC_Serialize(buf, length, offset);

  return;

}  // end c_ESMC_BaseSerialize


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseDeserialize - Deserialize Base object 
//
// !INTERFACE:
      void FTN(c_esmc_basedeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *buf,                // in/out - really a byte stream
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a base object.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  *base = new ESMC_Base;
  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.ESMC_LogWrite("Base object error", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *rc = (*base)->ESMC_Deserialize(buf, offset);

  return;

}  // end c_ESMC_BaseDeserialize


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseValidate - print Base object 
//
// !INTERFACE:
      void FTN(c_esmc_basevalidate)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *opts,               // in - F90, non-null terminated string
      int *rc,                  // out - return code
      int nlen) {               // hidden/in - strlen count for options
// 
// !DESCRIPTION:
//     Validate the contents of a base object.
//
//EOP

  char *copts = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.ESMC_LogWrite("Base object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  // copy and convert F90 string to null terminated one
  if (opts && (nlen > 0) && (opts[0] != '\0')) {
      copts = ESMC_F90toCstring(opts, nlen);
      if (!copts) {
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  *rc = (*base)->ESMC_Validate(copts);

  if (copts)
      delete [] copts;
  return;

}  // end c_ESMC_BaseValidate


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetName - return the object name to a Fortran caller
//
// !INTERFACE:
      void FTN(c_esmc_getname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // out - F90, non-null terminated string
      int *rc,                  // out - return code
      int nlen) {               // hidden/in - max strlen count for name
// 
// !DESCRIPTION:
//     return the name to a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  if (ESMF_MAXSTR < nlen){
    strncpy(name, (*base)->ESMC_BaseGetF90Name(), ESMF_MAXSTR);
    memset(name+ESMF_MAXSTR, ' ', nlen-ESMF_MAXSTR);  // fill rest with spaces
  }else{
    strncpy(name, (*base)->ESMC_BaseGetF90Name(), nlen);
  }

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_GetName


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_SetName - set the object name from an F90 caller
//
// !INTERFACE:
      void FTN(c_esmc_setname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *classname,          // in - F90, non-null terminated string
      char *objname,            // in - F90, non-null terminated string
      int *rc,                  // out - return code
      int clen,                 // hidden/in - max strlen count for classname
      int olen) {               // hidden/in - max strlen count for objname
// 
// !DESCRIPTION:
//     set the name from an F90 caller.
//
//EOPI

  char *oname = NULL;
  char *cname = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }
 
  if (classname && (clen > 0) && (classname[0] != '\0')) {
      // copy and convert F90 string to null terminated one
      cname = ESMC_F90toCstring(classname, clen);
      if (!cname) {
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  if (objname && (olen > 0) && (objname[0] != '\0')) {
      // copy and convert F90 string to null terminated one
      oname = ESMC_F90toCstring(objname, olen);
      if (!oname) {
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  (*rc) = (*base)->ESMC_BaseSetName(oname, cname);

  delete [] oname;
  delete [] cname; 

  return;

}  // end c_ESMC_SetName


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetClassName - return the object name to a Fortran caller
//
// !INTERFACE:
      void FTN(c_esmc_getclassname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *classname,          // out - Fortran, non-null terminated string
      int *rc,                  // out - return code
      int nlen) {               // hidden/in - max strlen count for name
// 
// !DESCRIPTION:
//     return the name to a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *rc = (*base)->ESMC_BaseGetF90ClassName(classname, nlen);

  return;

}  // end c_ESMC_GetClassName



//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetID - return the object id to the caller
//
// !INTERFACE:
      void FTN(c_esmc_getid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      int *id,                  // out - Fortran, integer address
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     return the object ID to a Fortran caller.
//
//EOPI

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base || !id) {
    printf("in c_ESMC_GetID, base or id bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *id = (*base)->ESMC_BaseGetID();

  return;

}  // end c_ESMC_GetID



//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_SetID - set an object id 
//
// !INTERFACE:
      void FTN(c_esmc_setid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      int *id,                  // in - Fortran, integer address
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     set an object ID from a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base || !id) {
    printf("in c_ESMC_SetID, base or id bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  (*base)->ESMC_BaseSetID(*id);

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_SetID



//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetVMId - return the object's VMId to the caller
//
// !INTERFACE:
      void FTN(c_esmc_getvmid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      ESMCI::VMId **vmid,       // out - Fortran, ESMF_VMId
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     return the object's VMId to a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    printf("in c_ESMC_GetVMId, base is bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *vmid = (*base)->ESMC_BaseGetVMId();

  return;

}  // end c_ESMC_GetVMId


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_SetVMId - allocate space and set the object's VMId 
//
// !INTERFACE:
      void FTN(c_esmc_setvmid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      ESMCI::VMId **vmid,       // in - Fortran, ESMF_VMId
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     allocate space and set the object's VMId.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    printf("in c_ESMC_SetVMId, base is bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  (*base)->ESMC_BaseSetVMId(*vmid);
  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_SetVMId




//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Attribute methods
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseAttSetValue - Set Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_baseattsetvalue)(
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
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      if (rc) *rc = status;
      return;
  }

  // Set the attribute on the object.
  *rc = (*base)->ESMC_BaseAttSet(cname, *tk, *count, value);

  delete [] cname;
  return;

}  // end c_ESMC_BaseAttSetValue


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseAttSetChar - Set String Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_baseattsetchar)(
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
    if (rc) *rc = ESMF_FAILURE;    
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      //printf("ESMF_BaseAttSet: bad attribute name\n");
      if (rc) *rc = status;
      return;
  }

  if ((!value) || (vlen <= 0) || (value[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      //printf("ESMF_BaseAttSet: bad attribute value\n");
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  cvalue = ESMC_F90toCstring(value, vlen);
  if (!cvalue) {
      *rc = ESMF_FAILURE;
      return;
  }

  // Set the attribute on the object.
  *rc = (*base)->ESMC_BaseAttSet(cname, cvalue);

  delete [] cname;
  delete [] cvalue;
  return;

}  // end c_ESMC_BaseAttSetChar

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseAttGetValue - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_baseattgetvalue)(
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
    *rc = ESMF_FAILURE;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      //printf("ESMF_BaseAttSet: bad attribute name\n");
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      *rc = ESMF_FAILURE;
      return;
  }

  status = (*base)->ESMC_BaseAttGet(cname, &attrTk, &attrCount, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status,
                         "failed getting attribute type and count", &status)) {
    //printf("ESMF_BaseAttGetValue: failed getting attribute info\n");
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  if (attrTk != *tk) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected kind", &status);
    //printf("attribute %s not expected kind %s, actually kind %d\n", 
    //       name, ESMC_TypeKindString(*tk), ESMC_TypeKindString(attrTk));
    delete [] cname;
    if (rc) *rc = status;
    return;
  }
  if (attrCount != *count) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected count", &status);
    //printf("expected count %d does not match actual count %d\n", 
    //           *count, attrCount);
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  status = (*base)->ESMC_BaseAttGet(cname, NULL, NULL, value);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status,
                         "failed getting attribute value", &status);
  delete [] cname;
  if (rc) *rc = status;

  return;

}  // end c_ESMC_BaseAttGetValue


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseAttGetChar - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_baseattgetchar)(
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

  int i;
  ESMC_TypeKind attrTypeKind;
  char *cname, *cvalue;
  int slen;              // actual attribute string length

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      printf("ESMF_BaseAttGet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      *rc = ESMF_FAILURE;
      return;
  }

  *rc = (*base)->ESMC_BaseAttGet(cname, &attrTypeKind, &slen, NULL);
  if (*rc != ESMF_SUCCESS) {
    delete [] cname;
    return;
  }

// TODO: re-enable the following error checking when ESMF_TYPEKIND_CHARACTER
// becomes available
  
//  if (attrTypeKind != ESMF_TYPEKIND_CHARACTER) {
      // TODO: this needs to sprintf into a buffer to format up the error msg
//      printf("ESMF_BaseAttGet: attribute %s not type character\n", name);
//      delete [] cname;
//      *rc = ESMF_FAILURE;
//      return; 
//  }

  // make sure destination will be long enough
  if (slen > vlen) {
    printf("ESMF_BaseAttGet: attribute %s is %d bytes long, buffer length "
      "%d is too short", name, slen, vlen);
    delete [] cname;
    *rc = ESMF_FAILURE;
    return; 
  }

  cvalue = new char[slen+1];

  *rc = (*base)->ESMC_BaseAttGet(cname, cvalue);
  if (*rc != ESMF_SUCCESS) {
    delete [] cname;
    delete [] cvalue;
    return;
  }

  *rc = ESMC_CtoF90string(cvalue, value, vlen);

  delete [] cname;
  delete [] cvalue;
  return;

}  // end c_ESMC_BaseAttGetChar


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseAttGetAttrInfoName - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmc_baseattgetattrinfoname)(
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

  char *cname;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    *rc = ESMF_FAILURE;    
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      printf("ESMF_BaseAttGetValue: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!tk) {
      printf("ESMF_BaseAttGetValue: bad attribute typekind argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!count) {
      printf("ESMF_BaseAttGetValue: bad attribute count argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      *rc = ESMF_FAILURE;
      return;
  }

  *rc = (*base)->ESMC_BaseAttGet(cname, tk, count, NULL);

  delete [] cname;
  return;

}  // end c_ESMC_BaseAttGetAttrInfoName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseAttGetAttrInfoNum - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmc_baseattgetattrinfonum)(
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

  char *cname;

// Initialize return code; assume routine not implemented
if (rc) *rc = ESMF_RC_NOT_IMPL;

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  if (!name) {
      printf("ESMF_BaseAttGetValue: bad attribute name argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!tk) {
      printf("ESMF_BaseAttGetValue: bad attribute typekind argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!count) {
      printf("ESMF_BaseAttGetValue: bad attribute count argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  cname = new char[ESMF_MAXSTR];

  *rc = (*base)->ESMC_BaseAttGet((*num)-1, cname, tk, count, NULL);
  if (*rc != ESMF_SUCCESS) {
      delete [] cname;
      return;
  }

  *rc = ESMC_CtoF90string(cname, name, nlen);
  
  delete [] cname;
  return;

}  // end c_ESMC_BaseAttGetAttrInfoNum


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseAttGetCount - get number of attrs
//
// !INTERFACE:
      void FTN(c_esmc_baseattgetcount)(
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
  status = ESMC_RC_NOT_IMPL;

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  if (!count) {
      printf("ESMF_BaseAttGetValue: bad attribute count argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  *count = (*base)->ESMC_BaseAttGetCount();

  *rc = (count == 0) ? ESMF_FAILURE : ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseAttGetCount


} // extern "C"
