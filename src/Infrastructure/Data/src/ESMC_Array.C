// $Id: ESMC_Array.C,v 1.5 2002/12/06 16:42:07 nscollins Exp $
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
// in the companion file ESMC_Array.h.  
//
// The {\tt ESMF\_Array} object allows C++ to emulate the richer
// Fortran language Array operations.  It allows strided access, 
// subsetting operations, known dimension sizes, and typed access 
// to arrays instead of just a starting address to a block of memory.  
//
//-----------------------------------------------------------------------------
//

// associated class definition file
#include "../include/ESMC_Array.h"
#include "../include/ESMC_Alloc.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_Array.C,v 1.5 2002/12/06 16:42:07 nscollins Exp $";
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
      ESMC_Array *ESMC_ArrayCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Array
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    enum ESMC_DataType dt,     // int, float, etc
    enum ESMC_DataKind dk,     // short/long, etc
    void *base,                // if non-null, this is already allocated memory
    int *offsets,              // offset in bytes to start of each dim
    int *lengths,              // number of items in each dim
    int *strides,              // number of bytes between successive items/dim
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This routine is the C++ entry point for creating an ESMF_Array
//      object.  Unlike natural C++ arrays which can be as simple as the
//      base address pointer and the number of bytes necessary to move to
//      the next item, ESMF_Arrays are richer in the associated metadata
//      which allows them to behave more like Fortran arrays.  They store
//      the size of each dimension, allow non-contiguous strides per
//      dimension, and allow whole-array or subsectional operations.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

// TODO: Add code here which does:
//
//   This code needs to make space for the private class data and store the
//   arguments given.  Decide if constructor is going to do much or not.
//
//   There will need to be a #if USING_MIXED_FORTRAN_AND_C++ section.  If
//   we are running with fortran, then it needs to do:
//   - call a *Fortran* allocate routine to get space - not malloc - and
//     it needs to call with a pointer of the correct type/kind/rank so
//     that this array is useable from F90 as well as C++.  
//         f90ptr = ESMF_Allocate(type, kind, rank, shape, ..., &rc);
//     This is overloaded to call a specific routine on the F90 side which
//     is per type/kind/rank.  The new F90 pointer is stored in the private 
//     data for this class as an opaque object; to be returned on demand 
//     for use by fortran code.
//
//   else, if this is a C++ only build then it has to call malloc to get
//   space, and it can ignore F90 pointers completely.
//
//   The return from this routine is a pointer to the new Array data.
//
     ESMC_Array *a = new ESMC_Array;
  
     return a;

 } // end ESMC_ArrayCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayDestroy - free a Array created with Create
//
// !INTERFACE:
      int ESMC_ArrayDestroy(ESMC_Array *array) {
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
    delete array;

    return 0;

 } // end ESMC_ArrayDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayCreate_F - internal routine for fortran use
//
// !INTERFACE:
      ESMC_Array *ESMC_ArrayCreate_F(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Array
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    enum ESMC_DataType dt,     // int, float, etc
    enum ESMC_DataKind dk,     // short/long, etc
    void *base,                // real start of memory 
    int *offsets,              // offset in bytes to start of each dim
    int *lengths,              // number of items in each dim
    int *strides,              // number of bytes between successive items/dim
    void *f90ptr,              // opaque type which fortran understands (dope v)
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This version of Create is only intended for internal use by
//      the ESMF_ArrayCreate fortran routine.  This routine works in a
//      similar manner as the regular ESMC_ArrayCreate routine, but the
//      differences include:  it gets a real fortran 90 array pointer as
//      one of the arguments; instead of calling malloc to make space for
//      the array contents, it passes the f90 pointer back to a fortran
//      routine to do the allocation.  This is so the fortran routine
//      can create the corresponding 'dope vector' in a completely compiler-
//      independent manner.  If we tried to do it here, we would have to
//      reverse-engineer the structure for each version of each compiler
//      we used.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

// TODO: Add code here which does:
//
//   This code needs to make space for the private class data and store the
//   arguments given.  Decide if constructor is going to do much or not.
//
//   It then call a *Fortran* allocate routine to get space - not malloc - and 
//   it needs to call with a pointer of the correct type/kind/rank so
//   that this array is useable from F90 as well as C++.  
     //ESMF_Allocate();
//   This is overloaded to call a specific routine on the F90 side which
//    is per type/kind/rank.
//
//   It then needs to store an opaque pointer to the real F90 array pointer 
//   (also called the dope vector), so that it can be returned to any 
//   fortran routine on demand. 
// 
//   The return from this routine is a pointer to the new Array data.
//
     ESMC_Array *a = new ESMC_Array;
     int ni, nj;
  
     (allocfuncaddr)(f90ptr, &ni, &nj, rc);

     //*rc = ESMF_SUCCESS;
     return a;

 } // end ESMC_ArrayCreate_F

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
// !IROUTINE:  ESMC_AllocFuncStore - internal routine
//
// !INTERFACE:
      ESMC_AllocFuncStore(
//
// !RETURN VALUE:
//    return code
//
// !ARGUMENTS:
      void *func) {   // in - fortran function pointer
//
// !DESCRIPTION:
//      stores a fortran function pointer used to call back into
//      fortran to do an allocation.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    
    if (func != (void *)0)
        allocfuncaddr = func;


    return ESMF_SUCCESS;
        

 } // end ESMC_AllocFuncStore

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DeallocFuncStore - internal routine
//
// !INTERFACE:
      ESMC_DeallocFuncStore(
//
// !RETURN VALUE:
//    return code
//
// !ARGUMENTS:
      void *func) {   // in - fortran function pointer
//
// !DESCRIPTION:
//      stores a fortran function pointer used to call back into
//      fortran to do an allocation.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    
    if (func != (void *)0)
        deallocfuncaddr = func;


    return ESMF_SUCCESS;
        

 } // end ESMC_DeallocFuncStore

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
