// $Id: ESMC_Base_F.C,v 1.24 2004/11/03 00:11:53 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"  // will this work?

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base_F.C,v 1.24 2004/11/03 00:11:53 nscollins Exp $";
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

  int i, status;
  char *cname = NULL;
  char *scname = NULL;

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

  int i, status;
  char *copts = NULL;

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

  int i, status;
  char *copts = NULL;

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
//BOP
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
//EOP

  int i, status;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  memcpy(name, (*base)->ESMC_BaseGetF90Name(), nlen);

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_GetName


//-----------------------------------------------------------------------------
//BOP
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
//EOP

  int i, status;
  char *oname = NULL;
  char *cname = NULL;

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
//BOP
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
//EOP

  int i, status;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *rc = (*base)->ESMC_BaseGetF90ClassName(classname, nlen);

  return;

}  // end c_ESMC_GetClassName



//-----------------------------------------------------------------------------
//BOP
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
//EOP

  int i, status;

  if (!base || !id) {
    printf("in c_ESMC_GetID, base or id bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *id = (*base)->ESMC_BaseGetID();

  return;

}  // end c_ESMC_GetID



//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Attribute methods
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetValue - Set Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetvalue)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_DataType *dt,        // in - data type, any but character 
      ESMC_DataKind *dk,        // in - data kind for int/real
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

  int i, status;
  char *cname;

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
  *rc = (*base)->ESMC_AttributeSet(cname, *dt, *dk, *count, value);

  delete [] cname;
  return;

}  // end c_ESMC_AttributeSetValue


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSetChar - Set String Attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetchar)(
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

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  // simple sanity checks before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      //printf("ESMF_AttributeSet: bad attribute name\n");
      if (rc) *rc = status;
      return;
  }

  if ((!value) || (vlen <= 0) || (value[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute value", &status);
      //printf("ESMF_AttributeSet: bad attribute value\n");
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
  *rc = (*base)->ESMC_AttributeSet(cname, cvalue);

  delete [] cname;
  delete [] cvalue;
  return;

}  // end c_ESMC_AttributeSetChar

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetValue - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributegetvalue)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_DataType *dt,        // in - data type expected to be returned
      ESMC_DataKind *dk,        // in - expected data kind for int/real 
      int *count,               // in - must match actual length
      void *value,              // out - value
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Return the (name,value) pair from any object type in the system.
//
//EOP

  int i, status, attrCount;
  ESMC_DataType attrDt;
  ESMC_DataKind attrDk;
  char *cname;

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
      //printf("ESMF_AttributeSet: bad attribute name\n");
      if (rc) *rc = status;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      *rc = ESMF_FAILURE;
      return;
  }

  status = (*base)->ESMC_AttributeGet(cname, &attrDt, &attrDk, &attrCount, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status,
                         "failed getting attribute type and count", &status)) {
    //printf("ESMF_AttributeGetValue: failed getting attribute info\n");
    delete [] cname;
    if (rc) *rc = status;
    return;
  }

  if (attrDt != *dt) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected type", &status);
    //printf("attribute %s not expected type %s, actually type %d\n", 
    //       name, ESMC_DataTypeString(*dt), ESMC_DataTypeString(attrDt));
    delete [] cname;
    if (rc) *rc = status;
    return;
  }
  if (attrDk != *dk) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_ARG_INCOMP,
                         "attribute value not expected kind", &status);
    //printf("attribute %s not expected kind %s, actually kind %d\n", 
    //       name, ESMC_DataKindString(*dk), ESMC_DataKindString(attrDk));
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

  status = (*base)->ESMC_AttributeGet(cname, NULL, NULL, NULL, value);
  ESMC_LogDefault.ESMC_LogMsgFoundError(status,
                         "failed getting attribute value", &status);
  delete [] cname;
  if (rc) *rc = status;

  return;

}  // end c_ESMC_AttributeGetValue


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetChar - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributegetchar)(
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

  int i, status;
  ESMC_DataType attrDt;
  char *cname, *cvalue;
  int slen;              // actual attribute string length

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      *rc = ESMF_FAILURE;
      return;
  }

  *rc = (*base)->ESMC_AttributeGet(cname, &attrDt, NULL, &slen, NULL);
  if (*rc != ESMF_SUCCESS) {
    delete [] cname;
    return;
  }

  if (attrDt != ESMF_DATA_CHARACTER) {
      // TODO: this needs to sprintf into a buffer to format up the error msg
      printf("ESMF_AttributeGet: attribute %s not type character\n", name);
      delete [] cname;
      *rc = ESMF_FAILURE;
      return; 
  }

  // make sure destination will be long enough
  if (slen > vlen) {
      printf("ESMF_AttributeGet: attribute %s is %d bytes long, buffer length %s is too short", 
                                  name, slen, vlen);
      delete [] cname;
      *rc = ESMF_FAILURE;
      return; 
  }

  cvalue = new char[slen+1];

  *rc = (*base)->ESMC_AttributeGet(cname, cvalue);
  if (*rc != ESMF_SUCCESS) {
    delete [] cname;
    delete [] cvalue;
    return;
  }

  *rc = ESMC_CtoF90string(cvalue, value, vlen);

  delete [] cname;
  delete [] cvalue;
  return;

}  // end c_ESMC_AttributeGetChar


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetAttrInfoName - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmc_attributegetattrinfoname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_DataType *dt,        // out - data type (int, float, etc)
      ESMC_DataKind *dk,        // out - data kind (*4, *8)
      int *count,               // out - item count
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the type, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int i, status, attrCount;
  char *cname;

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  // simple sanity check before doing any more work
  if ((!name) || (nlen <= 0) || (name[0] == '\0')) {
      printf("ESMF_AttributeGetValue: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }
  if (!dt) {
      printf("ESMF_AttributeGetValue: bad attribute datatype argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!dk) {
      printf("ESMF_AttributeGetValue: bad attribute datakind argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!count) {
      printf("ESMF_AttributeGetValue: bad attribute count argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  cname = ESMC_F90toCstring(name, nlen);
  if (!cname) {
      *rc = ESMF_FAILURE;
      return;
  }

  *rc = (*base)->ESMC_AttributeGet(cname, dt, dk, count, NULL);

  delete [] cname;
  return;

}  // end c_ESMC_AttributeGetAttrInfoName

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetAttrInfoNum - get type and number of items in an attr
//
// !INTERFACE:
      void FTN(c_esmc_attributegetattrinfonum)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int num,                  // in - attr number
      char *name,               // out - F90, non-null terminated string
      ESMC_DataType *dt,        // out - data type (int, float)
      ESMC_DataKind *dk,        // out - data kind (*4, *8)
      int *count,               // out - item count
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//   Return the name, type, count of items in the (name,value) pair from any 
//   object type in the system.
//
//EOP

  int i, status;
  char *cname;

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  if (!name) {
      printf("ESMF_AttributeGetValue: bad attribute name argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!dt) {
      printf("ESMF_AttributeGetValue: bad attribute datatype argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!dk) {
      printf("ESMF_AttributeGetValue: bad attribute datakind argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  if (!count) {
      printf("ESMF_AttributeGetValue: bad attribute count argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  cname = new char[ESMF_MAXSTR];

  *rc = (*base)->ESMC_AttributeGet(num, cname, dt, dk, count, NULL);
  if (*rc != ESMF_SUCCESS) {
      delete [] cname;
      return;
  }

  *rc = ESMC_CtoF90string(cname, name, nlen);
  
  delete [] cname;
  return;

}  // end c_ESMC_AttributeGetAttrInfoNum


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGetCount - get number of attrs
//
// !INTERFACE:
      void FTN(c_esmc_attributegetcount)(
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

  if (!base) {
    *rc = ESMF_FAILURE;
    return;
  }

  if (!count) {
      printf("ESMF_AttributeGetValue: bad attribute count argument\n");
      *rc = ESMF_FAILURE;
      return;
  }

  *count = (*base)->ESMC_AttributeGetCount();

  *rc = (count == 0) ? ESMF_FAILURE : ESMF_SUCCESS;
  return;

}  // end c_ESMC_AttributeGetCount


#if 0
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetbyNumber - get an ESMF object's attribute by number
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetbyNumber(
// 
// !RETURN VALUE:
//    int return code.  name must point to existing space long enough to
//    hold up to ESMF_MAXSTR bytes.
// 
// !ARGUMENTS:
      int number,                      // in - attribute number
      char *name,                      // out - attribute name
      ESMC_DataType *type,             // out - attribute type
      ESMC_DataValue *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//     Allows the caller to get attributes by number instead of by name.
//     This can be useful in iterating through all attributes in a loop.

//
//EOP


  int rc, i;

  // simple sanity check
  if ((number < 0) || (number >= attrCount)) {
      printf("ESMC_AttributeGetByNumber: attribute number must be  0 < N <= %d\n",
                                         attrCount-1);
      return ESMF_FAILURE;
  }

  if (name) strcpy(name, attr[number].attrName);
  if (type) *type = attr[number].dt;
  if (value) *value = attr[number];      // struct contents copy

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetbyNumber

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetNameList - get an ESMF object's attribute name list
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetNameList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int *count,               // out - number of attributes
      char **namelist) const {  // out - namelist
// 
// !DESCRIPTION:
//     Return a list of all attribute names without returning the values.
//
//EOP

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetNameList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSetList - set an ESMF object's attributes
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSetList(
// 
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
      char **namelist,          // in - list of attributes to set
      ESMC_DataValue *values) { // in - list of attribute values
// 
// !DESCRIPTION:
//    Set multiple attributes on an object in one call.  Depending on what is
//    allowed by the interface, all attributes may have to have the same type.
//
//EOP

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetList - get an ESMF object's attributes 
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char **namelist,                   // out - list of attribute names
      ESMC_DataType *typelist,           // out - list of attribute types
      ESMC_DataValue *valuelist) const { // out - list of attribute values
// 
// !DESCRIPTION:
//     Get multiple attributes from an object in a single call
//
//EOP

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeCopy - copy an attribute between two objects
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeCopy(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                 // in - attribute to copy
      ESMC_Base *destination) {   // in - the destination object
// 
// !DESCRIPTION:
//     The specified attribute associated with the source object (this) is
//     copied to the destination object.  << does this assume overwriting the
//     attribute if it already exists in the output or does this require yet
//     another arg to say what to do with collisions? >>

//EOP

  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects 
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeCopyAll(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *destination) {  // in - the destination object
// 
// !DESCRIPTION:
//     All attributes associated with the source object (this) are copied to the
//     destination object.  Some attributes will have to be considered
//     {\tt read only} and won't be updated by this call.  (e.g. an attribute
//     like {\tt name} must be unique and therefore can't be duplicated.)

//EOP

  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopyAll


//-----------------------------------------------------------------------------
// ESMC_Base class utility functions, not methods, since they operate on
//   multiple objects at once
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSetObjectList - set an attribute on multiple ESMF objects
//
// !INTERFACE:
      int ESMC_AttributeSetObjectList(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,    // in - list of ESMC objects
      char *name,                // in - attribute name
      ESMC_DataValue *value) {   // in - attribute value
// 
// !DESCRIPTION:
//     Set the same attribute on multiple objects in one call
//
//EOP

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetObjectList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetObjectList - get an attribute from multiple ESMF objects 
//
// !INTERFACE:
      int ESMC_AttributeGetObjectList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,            // in - list of ESMC objects
      char *name,                        // in - attribute name
      ESMC_DataType *typelist,           // out - list of attribute types
      ESMC_DataValue *valuelist) {       // out - list of attribute values
// 
// !DESCRIPTION:
//     Get the same attribute name from multiple objects in one call
//
//EOP

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetObjectList

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

#endif

} // extern "C"
