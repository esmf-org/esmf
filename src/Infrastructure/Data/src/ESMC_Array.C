// $Id: ESMC_Array.C,v 1.1 2002/11/04 22:16:36 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Array method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Array methods declared
// in the companion file ESMC_Array.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//

// associated class definition file
#include <ESMC_Array.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Array.C,v 1.1 2002/11/04 22:16:36 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Array routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayCreate - Create a new Array
//
// !INTERFACE:
      ESMC_Array *ESMC_Array::ESMC_ArrayCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Array
//
// !ARGUMENTS:
    int rank,
    enum ESMC_DataType dt, 
    enum ESMC_DataKind dk,
    void *base, 
    int *offsets, 
    int *lengths, 
    int *strides,
    void *f90ptr, 
    int *rc) {
//
// !DESCRIPTION:
//      Create a new Array from ... Allocates memory for a new Array
//      object and uses the internal routine ESMC_ArrayContruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC_ArrayInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

//
//  code goes here
//

 } // end ESMC_ArrayCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayDestroy - free a Array created with Create
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayDestroy(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a Array object previously allocated
//      via an ESMC_ArrayCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArrayDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayConstruct - fill in an already allocated Array
//
// !INTERFACE:
      void ESMC_Array::ESMC_ArrayConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    ESMC_Array *a,
    int rank,
    enum ESMC_DataType dt, 
    enum ESMC_DataKind dk,
    void *base, 
    int *offsets, 
    int *lengths, 
    int *strides,
    void *f90ptr, 
    int *rc) {
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Array object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC_ArrayDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC_ArrayCreate, which calls
//      ESMC_ArrayConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArrayConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayDestruct - release resources associated w/a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF_ArrayConstruct, does any additional cleanup before the
//      original Array object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC_ArrayDestroy, which calls
//      ESMC_ArrayDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArrayDestruct


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayGetConfig - get configuration info from a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_ArrayConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Array object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArrayGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArraySetConfig - set configuration info for a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArraySetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_ArrayConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Array object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_ArraySetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayGet<Value> - get <Value> for a Array
//
// !INTERFACE:
      //int ESMC_Array::ESMC_ArrayGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Array member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_ArrayGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArraySet<Value> - set <Value> for a Array
//
// !INTERFACE:
      //int ESMC_Array::ESMC_ArraySet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Array member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 //} // end ESMC_ArraySet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayValidate - internal consistency check for a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Array is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//

 } // end ESMC_ArrayValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayPrint - print contents of a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Array.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_ArrayPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Array - native C++ constructor
//
// !INTERFACE:
      ESMC_Array::ESMC_Array(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {  // in
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_Array

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Array - native C++ destructor
//
// !INTERFACE:
      ESMC_Array::~ESMC_Array(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ~ESMC_Array
