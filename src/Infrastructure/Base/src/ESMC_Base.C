// $Id: ESMC_Base.C,v 1.1 2002/10/25 21:04:22 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Base method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Base methods declared
// in the companion file ESMC_Base.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file
#include <ESMC_Base.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base.C,v 1.1 2002/10/25 21:04:22 eschwab Exp $";
//-----------------------------------------------------------------------------

// initialize class-wide instance counter
int ESMC_Base::instCount = 0;

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ESMC_Base routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Read - virtual read-in contents of a Base class
//
// !INTERFACE:
      int ESMC_Base::ESMC_Read(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      base class provides stubs for optional Read/Write
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return ESMF_SUCCESS;

 } // end ESMC_Read

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Write - virtual write-out contents of a Base class
//
// !INTERFACE:
      int ESMC_Base::ESMC_Write(void) const {
// 
// !RETURN VALUE:
//    int error return code
// 
// !ARGUMENTS:
//    const char *options) const {     //  in - print options
// 
// !DESCRIPTION:
//      base class provides stubs for optional Read/Write
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return ESMF_SUCCESS;

} // end ESMC_Write

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Validate - virtual internal consistency check for a Base
//
// !INTERFACE:
      int ESMC_Base::ESMC_Validate(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      base provides stub for required Validate method in derived classes
//      (must define for sub-classes on either C++ or F90 side)
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  return ESMF_SUCCESS;

 } // end ESMC_Validate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Print - virutal print contents of a Base
//
// !INTERFACE:
      int ESMC_Base::ESMC_Print(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      base provides stub for required Validate method in derived classes
//      (must define for sub-classes on either C++ or F90 side)
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return ESMF_SUCCESS;

 } // end ESMC_Print

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetInstCount - get number of base class instances
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetInstCount(void) const {
//
// !RETURN VALUE:
//    int instance count
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     accessor to number of base class instances
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return instCount;

} // end ESMC_BaseGetInstCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetID - set Base class unique ID
//  
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetID(
//  
// !RETURN VALUE:
//    none
//  
// !ARGUMENTS:
      int id) {   // in - ID to set
//  
// !DESCRIPTION: 
//     override default ID (see constructor)
//  
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  ID = id;

}  // end ESMC_BaseSetID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetID - get Base class unique ID
//  
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetID(void) const {
//  
// !RETURN VALUE:
//    int ID
//  
// !ARGUMENTS:
//    none
//  
// !DESCRIPTION:
//     accessor to unique ID
//  
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return ID;
} // end ESMC_BaseGetID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetRefCount - set Base class reference count
//
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetRefCount(
// 
// !RETURN VALUE:
//    none
// 
// !ARGUMENTS:
      int count) {
// 
// !DESCRIPTION:
//     accessor to reference count
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  refCount = count;

} // end ESMC_BaseSetRefCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetRefCount - get Base class reference count
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetRefCount(void) const {
//
// !RETURN VALUE:
//    int reference count
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     accessor to reference count
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return refCount;
} // end ESMC_BaseGetRefCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseSetStatus - set Base class status
//
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetStatus(
// 
// !RETURN VALUE:
//    none
// 
// !ARGUMENTS:
      ESMC_Status status) {   // in - base status to set
// 
// !DESCRIPTION:
//     accessor to base class status
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  baseStatus = status;

}  // end ESMC_BaseSetStatus
 
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseGetStatus - get Base class status
//
// !INTERFACE:
      ESMC_Status ESMC_Base::ESMC_BaseGetStatus(void) const {
// 
// !RETURN VALUE:
//    ESMC_Status
// 
// !ARGUMENTS:
//    none
// 
// !DESCRIPTION:
//     accessor to base class status
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  return baseStatus;

}  // end ESMC_BaseGetStatus

//-----------------------------------------------------------------------------
// Attribute methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeSet - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,               // in - attribute name
      ESMC_DataValue value) {   // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP
// !REQUIREMENTS:  FLD1.5, FLD1.5.3

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGet - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                      // in - attribute name
      ESMC_DataType *type,             // out - attribute type
      ESMC_DataValue *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP
// !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetCount - get an ESMF object's number of attributes
// 
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetCount(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int count) const {    // out - number of attributes for this object
// 
// !DESCRIPTION:
//      Returns number of attributes present
//
//EOP
// !REQUIREMENTS:   FLD1.7.5

  return ESMF_SUCCESS;

} // end ESMC_AttributeGetCount

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AttributeGetbyNumber - get an ESMF object's attribute by number
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeGetbyNumber(
// 
// !RETURN VALUE:
//    int return code
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

  ID = ++instCount;

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

//
//  code goes here
//

 } // end ~ESMC_<Class>
