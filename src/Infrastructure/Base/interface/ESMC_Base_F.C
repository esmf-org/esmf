// $Id: ESMC_Base_F.C,v 1.6 2004/01/29 23:30:29 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Base method interface (from F90 to C++) file

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
#include "ESMC.h"
#include "ESMC_Base.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base_F.C,v 1.6 2004/01/29 23:30:29 nscollins Exp $";
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
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  char *cname = NULL;
  char *scname = NULL;

  // copy and convert F90 strings to null terminated ones
  scname = ESMC_F90toCstring(superclass, sclen);
  if (!scname) {
      *rc = ESMF_FAILURE;
      return;
  }
  if (name != ESMC_NULL_POINTER) {
      cname = ESMC_F90toCstring(name, nlen);
      if (!cname) {
          delete [] scname;
          *rc = ESMF_FAILURE;
          return;
      }
  } 

  (*base) = new ESMC_Base(scname, cname, *nattrs);
  if (*base != NULL)
      *rc = ESMF_SUCCESS;
  else
      *rc = ESMF_FAILURE;

  delete [] scname;
  if (cname) delete [] cname;
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
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

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
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  char *copts;

  // copy and convert F90 string to null terminated one
  copts = ESMC_F90toCstring(opts, nlen);
  if (!copts) {
      *rc = ESMF_FAILURE;
      return;
  }

  *rc = (*base)->ESMC_Print(copts);

  delete [] copts;
  return;

}  // end c_ESMC_BasePrint


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_GetF90Name - return the object name to an F90 caller
//
// !INTERFACE:
      void FTN(c_esmc_getf90name)(
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
//     return the name to an F90 caller.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;

  strncpy(name, (*base)->ESMC_BaseGetF90Name(), nlen);

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_GetF90Name


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_SetF90Name - set the object name from an F90 caller
//
// !INTERFACE:
      void FTN(c_esmc_setf90name)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      int *rc,                  // out - return code
      int nlen) {               // hidden/in - max strlen count for name
// 
// !DESCRIPTION:
//     set the name from an F90 caller.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;

  (*rc) = (*base)->ESMC_BaseSetF90Name(name, nlen);

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_SetF90Name


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_GetF90ClassName - return the object name to an F90 caller
//
// !INTERFACE:
      void FTN(c_esmc_getf90classname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *classname,          // out - F90, non-null terminated string
      int *rc,                  // out - return code
      int nlen) {               // hidden/in - max strlen count for name
// 
// !DESCRIPTION:
//     return the name to an F90 caller.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;

  *rc = (*base)->ESMC_BaseGetF90ClassName(classname, nlen);

  return;

}  // end c_ESMC_GetF90ClassName


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_SetF90ClassName - set the object name from an F90 caller
//
// !INTERFACE:
      void FTN(c_esmc_setf90classname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *classname,          // in - F90, non-null terminated string
      int *rc,                  // out - return code
      int nlen) {               // hidden/in - max strlen count for name
// 
// !DESCRIPTION:
//     set the name from an F90 caller.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;

  (*rc) = (*base)->ESMC_BaseSetF90ClassName(classname, nlen);

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_SetF90ClassName


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Attribute methods
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSet - set attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetint)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      int *value,               // in - integer value
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  ESMC_Attribute attr;   // encapsulate data values to be set

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeSet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  attr.attrValue.dt = ESMF_DATA_INTEGER;
  attr.attrValue.items = 1;
  attr.attrValue.vi = *value;

  *rc = (*base)->ESMC_AttributeSet(&attr);
  return;

}  // end c_ESMC_AttributeSetInt


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSet - set attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetreal)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      double *value,            // in - real/double value
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  ESMC_Attribute attr;   // encapsulate data values to be set

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeSet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  attr.attrValue.dt = ESMF_DATA_REAL;
  attr.attrValue.items = 1;
  attr.attrValue.vr = *value;

  *rc = (*base)->ESMC_AttributeSet(&attr);
  return;

}  // end c_ESMC_AttributeSetReal


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSet - set attribute on an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributesetlogical)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_Logical *value,      // in - ESMF_Logical value
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  ESMC_Attribute attr;   // encapsulate data values to be set

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeSet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  attr.attrValue.dt = ESMF_DATA_LOGICAL;
  attr.attrValue.items = 1;
  attr.attrValue.vl = *value;

  *rc = (*base)->ESMC_AttributeSet(&attr);
  return;

}  // end c_ESMC_AttributeSetInt


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeSet - set attribute on an ESMF type
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
      char *value,              // in - character value
      int *rc,                  // in - return code
      int nlen,                 // hidden/in - strlen count for name
      int vlen) {               // hidden/in - strlen count for value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  char *vstring;
  ESMC_Attribute attr;   // encapsulate data values to be set

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeSet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  // allocate space, and copy and convert F90 string to null terminated one
  vstring = ESMC_F90toCstring(value, vlen);
  if (!vstring) {
      *rc = ESMF_FAILURE;
      return;
  }

  attr.attrValue.dt = ESMF_DATA_CHARACTER;
  attr.attrValue.items = 1;
  attr.attrValue.vcp = vstring;

  *rc = (*base)->ESMC_AttributeSet(&attr);
  return;

}  // end c_ESMC_AttributeSetInt


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGet - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributegetint)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      int *value,               // out - integer value
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  ESMC_Attribute attr;   // encapsulate data values to get

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  *rc = (*base)->ESMC_AttributeGet(&attr);
  if (*rc != ESMF_SUCCESS) return;

  if (attr.attrValue.dt != ESMF_DATA_INTEGER) {
      printf("ESMF_AttributeGet: attribute %s not type integer\n", name);
      *rc = ESMF_FAILURE;
      return; 
  }

  *value = attr.attrValue.vi;
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_AttributeGetInt


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGet - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributegetreal)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      double *value,            // out - real value
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  ESMC_Attribute attr;   // encapsulate data values to get

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  *rc = (*base)->ESMC_AttributeGet(&attr);
  if (*rc != ESMF_SUCCESS) return;

  if (attr.attrValue.dt != ESMF_DATA_REAL) {
      printf("ESMF_AttributeGet: attribute %s not type real\n", name);
      *rc = ESMF_FAILURE;
      return; 
  }

  *value = attr.attrValue.vr;
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_AttributeGetReal


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGet - get attribute from an ESMF type
//
// !INTERFACE:
      void FTN(c_esmc_attributegetlogical)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // in - F90, non-null terminated string
      ESMC_Logical *value,      // out - logical value
      int *rc,                  // in - return code
      int nlen) {               // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  ESMC_Attribute attr;   // encapsulate data values to get

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  *rc = (*base)->ESMC_AttributeGet(&attr);
  if (*rc != ESMF_SUCCESS) return;

  if (attr.attrValue.dt != ESMF_DATA_LOGICAL) {
      printf("ESMF_AttributeGet: attribute %s not type logical\n", name);
      *rc = ESMF_FAILURE;
      return; 
  }

  *value = attr.attrValue.vl;
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_AttributeGetLogical


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_AttributeGet - get attribute from an ESMF type
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
//     Associate a (name,value) pair with any object type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  int i, status;
  int slen;              // actual attribute string length
  ESMC_Attribute attr;   // encapsulate data values to get

  // simple sanity check before doing any more work
  if ((!name) || (name[0] == '\0')) {
      printf("ESMF_AttributeGet: bad attribute name\n");
      *rc = ESMF_FAILURE;
      return;
  }

  // copy and convert F90 string to null terminated one
  status = ESMC_F90toCstring(name, nlen, attr.attrName, ESMF_MAXSTR);
  if (status != ESMF_SUCCESS) {
      *rc = status;
      return;
  }

  *rc = (*base)->ESMC_AttributeGet(&attr);
  if (*rc != ESMF_SUCCESS) return;

  if (attr.attrValue.dt != ESMF_DATA_CHARACTER) {
      printf("ESMF_AttributeGet: attribute %s not type character\n", name);
      *rc = ESMF_FAILURE;
      return; 
  }

  // make sure destination will be long enough to make a copy
  slen = strlen(attr.attrValue.vcp);
  if (slen >= vlen) {
      printf("ESMF_AttributeGet: attribute %s is %d bytes long, buffer length %s is too short", 
                                  name, slen, vlen);
      *rc = ESMF_FAILURE;
      return; 
  }

  *rc = ESMC_CtoF90string(attr.attrValue.vcp, value, vlen);
  return;

}  // end c_ESMC_AttributeGetChar


#if 0
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetCount - get an ESMF object's number of attributes
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetCount(
// 
// !RETURN VALUE:
//    int attribute count
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of attributes present
//
//EOP
// !REQUIREMENTS:   FLD1.7.5

  
  return attrCount;

} // end ESMC_AttributeGetCount

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
// !REQUIREMENTS:   


  int rc, i;

  // simple sanity check
  if ((number < 0) || (number >= attrCount)) {
      printf("ESMC_AttributeGetByNumber: attribute number must be  0 < N <= %d\n",
                                         attrCount-1);
      return ESMF_FAILURE;
  }

  if (name) strcpy(name, attr[number].attrName);
  if (type) *type = attr[number].attrValue.dt;
  if (value) *value = attr[number].attrValue;      // struct contents copy

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
// !REQUIREMENTS:   FLD1.7.3

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
// !REQUIREMENTS:   (none.  added for completeness)

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
// !REQUIREMENTS:   FLD1.7.4

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
// !REQUIREMENTS:   FLD1.5.4

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
// !REQUIREMENTS:   FLD1.5.4

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
// !REQUIREMENTS:   FLD1.5.5 (pri 2)

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
// !REQUIREMENTS:   FLD1.5.5 (pri 2)

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGetObjectList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Base - native C++ constructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::ESMC_Base(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     increment total number of instances; use for this instance's ID
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  attrCount = 0;
  attrAlloc = 0;
  attr = ESMC_NULL_POINTER;

  ID = ++globalCount;
  refCount = 1;
  strcpy(baseName, "unnamed");
  baseStatus = ESMF_STATE_READY;

 } // end ESMC_Base

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Base - native C++ destructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::~ESMC_Base(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  baseStatus = ESMF_STATE_INVALID;
  if (attr) delete (attr);

  // if we have to support reference counts someday,
  // if (refCount > 0) do something;

 } // end ~ESMC_Base

#endif

} // extern "C"
