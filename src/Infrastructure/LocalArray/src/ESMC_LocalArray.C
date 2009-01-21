// $Id: ESMC_LocalArray.C,v 1.27.2.7 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_LocalArray.C"
//==============================================================================
//
// ESMC LocalArray method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ LocalArray methods declared
// in the companion file ESMC_LocalArray.h.  
//
// The LocalArray class allows C++ to emulate the richer Fortran language array
// operations. It allows strided access, subsetting operations, known dimension,
// sizes, and typed access to arrays instead of just a starting address to a
// block of memory.  
//
//-----------------------------------------------------------------------------

// associated header file
#include "ESMC_LocalArray.h"

// higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>
#include <assert.h>

// ESMF headers
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_LocalArray.C,v 1.27.2.7 2009/01/21 21:25:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

// prototypes for Fortran calls
extern "C" {

  void FTN(f_esmf_localarrayf90allocate)(ESMC_LocalArray**, int *, 
    ESMC_TypeKind*, int *, int *, int *, int *);
 
  void FTN(f_esmf_localarrayf90deallocate)(ESMC_LocalArray**, int*, 
    ESMC_TypeKind *, int *);
 
  void FTN(f_esmf_localarrayadjust)(ESMC_LocalArray**, int *,
    ESMC_TypeKind*, int *, int *, int *, int *);

  void FTN(f_esmf_localarraycopyf90ptr)(ESMC_LocalArray** laIn, 
    ESMC_LocalArray** laOut, int *rc);
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// This section includes all the Local Array create/destroy routines.
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayCreate"
//BOP
// !IROUTINE:  ESMC_LocalArrayCreate - Create a new Array
//
// !INTERFACE:
ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArrayCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  int *icounts,              // number of items in each dim
  void *base,                // if non-null, this is already allocated memory

  ESMC_DataCopy docopy,      // if base != NULL, copy data?
  char *name,                // array name
  int *rc) {                 // return code
//
// !DESCRIPTION:
//  This routine is the C++ entry point for creating an {\tt ESMC\_LocalArray}
//  object.  Unlike natural C++ arrays which can be as simple as the
//  base address pointer and the number of bytes necessary to move to
//  the next item, {\tt ESMC\_LocalArray}s are richer in the associated metadata
//  which allows them to behave more like Fortran arrays.  They store
//  the size of each dimension, allow non-contiguous strides per
//  dimension, and allow whole-array or subsectional operations.
//
//EOP

//   This code needs to make space for the private class data and store the
//   arguments given.
//
//   There will need to be a #if USING_MIXED_FORTRAN_AND_C++ section.  If
//   we are running with fortran, then it needs to do:
//   - call a *Fortran* allocate routine to get space - not malloc - and
//     it needs to create an F90 array pointer of the correct type/kind/rank 
//     so this array is useable from F90 as well as C++.  
//     The new F90 pointer is stored in the private 
//     data for this class as an opaque object; to be returned on demand 
//     for use by fortran code.
//
//   else, if this is a C++ only build then it has to call malloc to get
//   space, and it can ignore F90 pointers completely.
//
//   The return from this routine is a pointer to the new LocalArray data.
//
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  ESMC_LocalArray *a;
  try{
    a = new ESMC_LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.ESMC_LogAllocError(rc);  
    return ESMC_NULL_POINTER;
  }

  localrc = a->ESMC_LocalArrayConstruct(rank, dk, icounts, base, 
    ESMC_FROM_CPLUSPLUS,  NULL, ESMC_ARRAY_DO_ALLOCATE, docopy, ESMF_TRUE,
    name, NULL, NULL, NULL); 
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;
  
} // end ESMC_LocalArrayCreate
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayCreate"
//BOP
// !IROUTINE:  ESMC_LocalArrayCreate - Create a new Array
//
// !INTERFACE:
ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArrayCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  int *icounts,              // number of items in each dim
  int *lbounds,              // lower index number per dim
  int *ubounds,              // upper index number per dim
  void *base,                // if non-null, this is already allocated memory
  ESMC_DataCopy docopy,      // if base != NULL, copy data?
  char *name,                // array name
  int *rc) {                 // return code
//
// !DESCRIPTION:
//  This routine is the C++ entry point for creating an {\tt ESMC\_LocalArray}
//  object.  Unlike natural C++ arrays which can be as simple as the
//  base address pointer and the number of bytes necessary to move to
//  the next item, {\tt ESMC\_LocalArray}s are richer in the associated metadata
//  which allows them to behave more like Fortran arrays.  They store
//  the size of each dimension, allow non-contiguous strides per
//  dimension, and allow whole-array or subsectional operations.
//
//EOP

//   This code needs to make space for the private class data and store the
//   arguments given.
//
//   There will need to be a #if USING_MIXED_FORTRAN_AND_C++ section.  If
//   we are running with fortran, then it needs to do:
//   - call a *Fortran* allocate routine to get space - not malloc - and
//     it needs to create an F90 array pointer of the correct type/kind/rank 
//     so this array is useable from F90 as well as C++.  
//     The new F90 pointer is stored in the private 
//     data for this class as an opaque object; to be returned on demand 
//     for use by fortran code.
//
//   else, if this is a C++ only build then it has to call malloc to get
//   space, and it can ignore F90 pointers completely.
//
//   The return from this routine is a pointer to the new Array data.
//
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  ESMC_LocalArray *a;
  try{
    a = new ESMC_LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.ESMC_LogAllocError(rc);  
    return ESMC_NULL_POINTER;
  }

  localrc = a->ESMC_LocalArrayConstruct(rank, dk, icounts, base, 
    ESMC_FROM_CPLUSPLUS, NULL, ESMC_ARRAY_DO_ALLOCATE, docopy, ESMF_TRUE, name,
    lbounds, ubounds, NULL); 
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;
  
} // end ESMC_LocalArrayCreate
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayCreate"
//BOP
// !IROUTINE:  ESMC_LocalArrayCreate - create LocalArray from copy
//
// !INTERFACE:
ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArrayCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_LocalArray
//
// !ARGUMENTS:
  ESMC_LocalArray *larrayIn,
  int *rc) {                 // return code
//
// !DESCRIPTION:
//      Deep copy from {\tt larrayIn} to {\tt larrayOut}.
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                 // local return code
  
  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // allocate memory for new LocalArray object
  ESMC_LocalArray *larrayOut = new ESMC_LocalArray;

  // call base class routine to set name 
  larrayOut->ESMC_BaseSetName(NULL, "LocalArray");
  
  // copy the contents
  ESMC_Base baseTemp;
  baseTemp = *larrayOut;  // store base object info in temp. variable
  *larrayOut = *larrayIn; // copy larrayIn content into larrayOut includ. base
  *((ESMC_Base*)larrayOut) = baseTemp; // override base part of larrayOut again
  
  // mark this copy to be responsible for deallocation of its data area alloc
  larrayOut->needs_dealloc = ESMF_TRUE;

  // call into F90 copy method
  FTN(f_esmf_localarraycopyf90ptr)(&larrayIn, &larrayOut, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;

  // return successfully 
  if (rc != NULL) *rc = ESMF_SUCCESS;
  return larrayOut;

} // end ESMC_LocalArrayCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayDestroy"
//BOP
// !IROUTINE:  ESMC_LocalArrayDestroy - free a LocalArray created with Create
//
// !INTERFACE:
int ESMC_LocalArray::ESMC_LocalArrayDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  ESMC_LocalArray *array) {
//
// !DESCRIPTION:
//  ESMF routine which destroys a LocalArray object previously allocated
//  via an {\tt ESMC\_LocalArrayCreate} routine.
//
//EOP
 
     int rc;

     // Initialize return code; assume routine not implemented
     rc = ESMC_RC_NOT_IMPL;

    array->ESMC_LocalArrayDestruct();

    delete array;

    rc = ESMF_SUCCESS;
    return rc;

} // end ESMC_LocalArrayDestroy
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayCreateNoData"
//BOPI
// !IROUTINE:  ESMC_LocalArrayCreateNoData - internal routine for fortran use
//
// !INTERFACE:
ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArrayCreateNoData(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  ESMC_ArrayOrigin oflag,    // caller is fortran or C++?
  char *name,                // array name
  int *rc) {                 // return code
//
// !DESCRIPTION:
//  This version of Create is only intended for internal use by the
//  {\tt ESMF\_LocalArrayCreate} fortran routine.  It creates a partially
//  constructed array, then depends on the caller to come back and
//  complete the array with the {\tt ESMF\_LocalArraySetInfo} call.  
//  (It is broken up this way to try to minimize the amount of
//  macro-generated code needed in the {\tt ESMF\_LocalArray.F90} source
//  file.)
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  ESMC_LocalArray *a;
  try{
    a = new ESMC_LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.ESMC_LogAllocError(rc);  
    return ESMC_NULL_POINTER;
  }

  localrc = a->ESMC_LocalArrayConstruct(rank, dk, NULL, NULL, oflag, NULL,
    ESMC_ARRAY_NO_ALLOCATE, ESMC_DATA_NONE, ESMF_FALSE, name, NULL, NULL, NULL);

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;

} // end ESMC_LocalArrayCreateNoData
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayCreate_F"
//BOP
// !IROUTINE:  ESMC_LocalArrayCreate_F - internal routine for fortran use
//
// !INTERFACE:
ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArrayCreate_F(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  int *icounts,              // counts along each dimension
  struct c_F90ptr *f90ptr,   // opaque type which fortran uses (dope v)
  void *base,                // real start of memory 
  ESMC_DataCopy docopy,      // if base is null and this is Copy, alloc here
  char *name,                // array name, default created if NULL
  int *lbounds,              // lower index number per dim
  int *ubounds,              // upper index number per dim
  int *offsets,              // number of bytes to start of data/dim
  int *rc) {                 // return code
//
// !DESCRIPTION:
//  This version of Create is only intended for internal use by the
//  {\tt ESMF\_LocalArrayCreate} fortran routine.  This routine works in a
//  similar manner as the regular {\tt ESMC\_LocalArrayCreate} routine, but
//  the differences include:  it gets a real fortran 90 array pointer as
//  one of the arguments; instead of calling malloc to make space for
//  the array contents, it passes the f90 pointer back to a fortran
//  routine to do the allocation.  This is so the fortran routine
//  can create the corresponding 'dope vector' in a completely compiler-
//  independent manner.  If we tried to do it here, we would have to
//  reverse-engineer the structure for each version of each compiler
//  we used.
//
//EOP

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
//   The return from this routine is a pointer to the new LocalArray data.
//
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  ESMC_LocalArray *a;
  try{
    a = new ESMC_LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.ESMC_LogAllocError(rc);  
    return ESMC_NULL_POINTER;
  }

  if (base == NULL) 
    localrc = a->ESMC_LocalArrayConstruct(rank, dk, icounts, base, 
      ESMC_FROM_FORTRAN, f90ptr, ESMC_ARRAY_DO_ALLOCATE, ESMC_DATA_NONE,
      ESMF_TRUE, name, lbounds, ubounds, offsets); 
  else
    localrc = a->ESMC_LocalArrayConstruct(rank, dk, icounts, base, 
      ESMC_FROM_FORTRAN, f90ptr, ESMC_ARRAY_NO_ALLOCATE, docopy,
      ESMF_FALSE, name, lbounds, ubounds, offsets); 

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;

} // end ESMC_LocalArrayCreate_F
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayConstruct"
//BOP
// !IROUTINE:  ESMC_LocalArrayConstruct - fill in an already allocated LocalArray
//
// !INTERFACE:
int ESMC_LocalArray::ESMC_LocalArrayConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  int irank,                 // dimensionality
  ESMC_TypeKind dk,          // short/long, etc  (*2, *4, *8)
  int *icounts,              // number of items in each dim
  void *base,                // base memory address of data block
  ESMC_ArrayOrigin oflag,    // create called from F90 or C++?
  struct c_F90ptr *f90ptr,   // opaque type which fortran understands (dopev)
  ESMC_ArrayDoAllocate aflag, // do we allocate space or not?
  ESMC_DataCopy docopy,      // do we make a copy of the data?
  ESMC_Logical dflag,        // do we deallocate space or not?
  char *name,                // array name, default created if NULL
  int *lbounds,              // lower index number per dim
  int *ubounds,              // upper index number per dim
  int *offsets) {            // offset in bytes to start of each dim
//
// !DESCRIPTION:
//  ESMF routine which fills in the contents of an already allocated
//  {\tt ESMF\_LocalArray} object.  May need to do additional allocations
//  as needed.  Must call the corresponding {\tt ESMC\_LocalArrayDestruct}
//  routine to free the additional memory.  Intended for internal
//  ESMF use only; end-users use {\tt ESMC\_LocalArrayCreate}, which calls
//  {\tt ESMC\_LocalArrayConstruct}.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // set object members - some defaults may be overridden further down
  rank = irank;
  kind = dk;
  base_addr = base;
  int totalcount = 1;
  for (int i=0; i<rank; i++) {
    counts[i] = icounts ? icounts[i] : 1;        
    lbound[i] = lbounds ? lbounds[i] : 1;
    ubound[i] = ubounds ? ubounds[i] : counts[i];
    bytestride[i] = 1;
    offset[i] = offsets ? offsets[i] : 0;
    totalcount *= counts[i];
  }
  for (int i=rank; i<ESMF_MAXDIM; i++) {
    counts[i] = 1;
    lbound[i] = 1;
    ubound[i] = 1;
    bytestride[i] = 1;
    offset[i] = 0;
  }
  origin = oflag;
  needs_dealloc = dflag;
  byte_count = ESMC_TypeKindSize(kind) * totalcount; 

  // set Fortran dope vector if provided for existing allocation
  if (f90ptr != NULL)
    ESMC_LocalArraySetF90Ptr(f90ptr);
 
  // call into Fortran to do the allocate if necessary
  if (aflag == ESMC_ARRAY_DO_ALLOCATE) {
    ESMC_LocalArray *aptr = this;
    FTN(f_esmf_localarrayf90allocate)(&aptr, &rank, &kind, counts, 
      lbound, ubound, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
  } 

  // call base class routine to set name 
  ESMC_BaseSetName(name, "LocalArray");
    
  // TODO: memcpy from base to base_addr, proper number of bytes?
  //  if docopy flag is set.

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

 } // end ESMC_LocalArrayConstruct
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayDestruct"
//BOP
// !IROUTINE:  ESMC_LocalArrayDestruct - release resources associated w/a LocalArray
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayDestruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMF\_LocalArrayConstruct}, does any additional cleanup before the
//      original {\tt ESMC\_LocalArray} object is freed.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_LocalArrayDestroy}, which calls
//      {\tt ESMC\_LocalArrayDestruct}.  Define for deep classes only.
//
//EOP

   // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    ESMC_LocalArray *aptr = this;

    // check origin and alloc flag, and call dealloc routine if needed 
    if (needs_dealloc != ESMF_TRUE)
    return ESMF_SUCCESS;

    // if there is an F90 dope vector, we have to call back into fortran
    // to deallocate this.   if we want to support a C++ only library,
    // then this code needs to be calling malloc/free or new/delete and
    // needs conditional code to pick the fortran or C++ mem mgt system.

    FTN(f_esmf_localarrayf90deallocate)(&aptr, &rank, &kind, &rc);

    return rc;

 } // end ESMC_LocalArrayDestruct
//-----------------------------------------------------------------------------

 
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayAdjust"
//BOP
// !IROUTINE:  ESMC_LocalArrayAdjust - adjust an already allocated LocalArray
//
// !INTERFACE:
      ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArrayAdjust(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_LocalArray
//
// !ARGUMENTS:
    ESMC_DataCopy copyflag,     // copy or reference original data
    int *lbounds,               // lower index number per dim
    int *ubounds,               // upper index number per dim
    int *rc                     // return code
    ){
//
// !DESCRIPTION:
//      ESMF routine which adjusts the contents of an already allocated
//      {\tt ESMF\_LocalArray} object.
//
//EOP

  // local vars
  int localrc;                 // local return code
  
  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  ESMC_LocalArray *larray;
  
  if (copyflag == ESMC_DATA_COPY){
    // make a copy of the LocalArray object including the data allocation
    larray = ESMC_LocalArrayCreate(this, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return NULL;
  }else{
    // allocate memory for new LocalArray object
    larray = new ESMC_LocalArray;
    // copy the LocalArray members, including the _reference_ to its data alloc.
    *larray = *this;
    // mark this copy not to be responsible for deallocation
    larray->needs_dealloc = ESMF_FALSE;
  }

  // adjust the lbound and ubound members while checking counts
  for (int i=0; i<rank; i++){
    if (larray->counts[i] != ubounds[i] - lbounds[i] + 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
        "- Mismatch of lbounds, ubounds and counts", rc);
      return NULL;
    }
    larray->lbound[i] = lbounds[i];
    larray->ubound[i] = ubounds[i];
  }

  // adjust the F90 dope vector to reflect the new bounds
  FTN(f_esmf_localarrayadjust)(&larray, &rank, &kind, counts, larray->lbound,  
    larray->ubound, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return NULL;

  // return successfully 
  if (rc) *rc = ESMF_SUCCESS;
  return larray;

} // end ESMC_LocalArrayAdjust

 
 
 
 
 
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// get/set routines.
//-----------------------------------------------------------------------------
// Note that most of the Get/Set routines are by value and are inline in
//  the include file.
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArraySetInfo"
//BOP
// !IROUTINE:  ESMC_LocalArraySetInfo - Set the most common F90 needs
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArraySetInfo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    struct c_F90ptr *fptr,    // in - f90 pointer
    void *base,               // in - base memory address
    int *icounts,             // in - counts along each dim
    int *lbounds,             // in - lowest valid index
    int *ubounds,             // in - highest valid index
    int *offsets,             // in - numbytes from base to 1st item/dim
    ESMC_Logical *contig,     // in - is memory chunk contiguous?
    ESMC_Logical *dealloc) {  // in - do we need to deallocate at delete?
//
// !DESCRIPTION:
//     Sets a list of values associated with an already created pointer.
//     This particular set was chosen to mesh well with creation on the
//     F90 side.  Other combinations will probably be useful.
//
//EOP

    int i, rank = this->rank;
    int totalcount;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
    int rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;  

    if (fptr) {
        // note - starts at 1; base includes rank 1 size
        for (i=1; i<rank; i++)
    	bytes += ESMF_F90_PTR_PLUS_RANK;
       
       //fprintf(stderr, "setting f90 ptr from %lx to %lx, %d bytes for rank %d\n", 
       //                (ESMC_I8)fptr, (ESMC_I8)(&this->f90dopev), bytes, rank);
    
        memcpy((void *)(&this->f90dopev), (void *)fptr, bytes);
    }

    if (base)
        base_addr = base;

    // valid values
    totalcount = 1;
    for (i=0; i<rank; i++) {
        counts[i]     = icounts ? icounts[i] : 0;
        offset[i]     = offsets ? offsets[i] : 0;
        bytestride[i] = 0;
        lbound[i] = lbounds ? lbounds[i] : 1;
        ubound[i] = ubounds ? ubounds[i] : counts[i];
        totalcount *= counts[i];
    }
    // filler for unused ranks
    for (i=rank; i<ESMF_MAXDIM; i++) {
        counts[i]     = 0;
        offset[i]     = 0;
        bytestride[i] = 1;
        lbound[i] = 1;
        ubound[i] = 1;
    }
    if (contig)
        iscontig = *contig;
    if (dealloc)
        needs_dealloc = *dealloc;

    byte_count = ESMC_TypeKindSize(kind) * totalcount;

    rc = ESMF_SUCCESS;
    return rc;


 } // end ESMC_LocalArraySetInfo

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayGetInfo"
//BOP
// !IROUTINE:  ESMC_LocalArrayGetInfo - Get the most common F90 needs
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayGetInfo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    struct c_F90ptr *fptr,    // in - f90 pointer
    void *base,               // in - base memory address
    int *icounts,             // in - counts along each dim
    int *lbounds,             // in - lowest valid index
    int *ubounds,             // in - highest valid index
    int *offsets)const{           // in - numbytes from base to 1st item/dim
//
// !DESCRIPTION:
//     Gets a list of values associated with an already created pointer.
//     This particular set was chosen to mesh well with creation on the
//     F90 side.  Other combinations will probably be useful.
//
//EOP

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
    int rc;
 
     // Initialize return code; assume routine not implemented
     rc = ESMC_RC_NOT_IMPL;

    if (fptr) {
        // note - starts at 1; base includes rank 1 size
        for (i=1; i<rank; i++)
    	    bytes += ESMF_F90_PTR_PLUS_RANK;
       
        memcpy((void *)fptr, (void *)(&this->f90dopev), bytes);
    }

    if (base)
        base = base_addr;

    if (icounts) 
        for (i=0; i<rank; i++) 
            icounts[i] = counts[i];
    if (lbounds)
        for (i=0; i<rank; i++)  
            lbounds[i] = lbound[i];
    if (ubounds) 
        for (i=0; i<rank; i++) 
            ubounds[i] = ubound[i];
    if (offsets) 
        for (i=0; i<rank; i++) 
            offsets[i] = offset[i];
    

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_LocalArrayGetInfo

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayGetF90Ptr"
//BOP
// !IROUTINE:  ESMC_LocalArrayGetF90Ptr - get F90Ptr for a LocalArray
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayGetF90Ptr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      struct c_F90ptr *p) const {     // out - fortran 90 array pointer
//
// !DESCRIPTION:
//     Return a stored F90 pointer block.  The size may vary with rank.
//
//EOP

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
    int rc;
    
     // Initialize return code; assume routine not implemented
     rc = ESMC_RC_NOT_IMPL;

    // note - starts at 1; base includes rank 1 size
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    //fprintf(stderr, "getting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
    //                 (ESMC_I8)(&this->f90dopev), (ESMC_I8)p, bytes, rank);

    memcpy((void *)p, (void *)(&this->f90dopev), bytes);

    rc = ESMF_SUCCESS;
    return rc; 

 } // end ESMC_LocalArrayGetF90Ptr

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArraySetF90Ptr"
//BOP
// !IROUTINE:  ESMC_LocalArraySetF90Ptr - set F90Ptr for a LocalArray
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArraySetF90Ptr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const struct c_F90ptr *p) {     // in - f90 pointer block
//
// !DESCRIPTION:
//     Sets the {\tt ESMC\_LocalArray} member F90ptr with the given value.
//     Can be multiple routines, one per value
//
//EOP

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
    int rc;

     // Initialize return code; assume routine not implemented
     rc = ESMC_RC_NOT_IMPL;

    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    //fprintf(stderr, "setting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
    //                  (ESMC_I8)p,  (ESMC_I8)(&this->f90dopev), bytes, rank);

    memcpy((void *)(&this->f90dopev), (void *)p, bytes);

    return ESMF_SUCCESS; 

 } // end ESMC_LocalArraySetF90Ptr

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Higher level Array functions.  Might need to move to another file if
// there are enough of them...
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArraySlice"
//BOP
// !IROUTINE:  ESMC_LocalArraySlice - drop an array by one dimension
//
// !INTERFACE:
      ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArraySlice(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int slicedim,             // in - which dim to slice
      int sliceloc,             // in - at which location on that dim
      int *rc) const {          // out - return code
//
// !DESCRIPTION:
//      Creates a (N-1)D array from an existing one.  Copies data if
//      it exists.  Returns a new array.
//
//EOP

    ESMC_LocalArray *newa;

    // Initialize return code; assume routine not implemented
    if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;


    return NULL;

 } // end ESMC_LocalArraySlice

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayReshape"
//BOP
// !IROUTINE:  ESMC_LocalArrayReshape - change the rank or counts on an array
//
// !INTERFACE:
      ESMC_LocalArray *ESMC_LocalArray::ESMC_LocalArrayReshape(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int rank,                 // in - new rank
      int *newcounts,           // in - new counts along each rank dim
      int *rc) const {          // out - return code
//
// !DESCRIPTION:
//      Creates a new array based on an old one.  The original data is
//      referenced and therefore shared with the old array, but it can be
//      iterated with an F90 pointer of a different configuration.
//
//EOP

    ESMC_LocalArray *newa;

    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    return NULL;

 } // end ESMC_LocalArrayReshape

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Standard methods - Validate, Print, Read, Write, Serialize, Deserialize
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArraySerialize"
//BOPI
// !IROUTINE:  ESMC_LocalArraySerialize - Turn localarray into a byte stream
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArraySerialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *boffset) const {   // inout - original offset, updated to point
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in LocalArray class into a stream of bytes.
//
//EOPI
    int fixedpart, nbytes, rc;
    char *cp;
    int *ip, i;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    fixedpart = sizeof(ESMC_LocalArray);
    if ((*length - *boffset) < fixedpart) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                           "Buffer too short to add a LocalArray object", &rc);
        return ESMF_FAILURE; 
        //buffer = (char *)realloc((void *)buffer, *length + 2*fixedpart);
        //*length += 2 * fixedpart;
    }

    // Serialize the Base class first.
    rc = this->ESMC_Base::ESMC_Serialize(buffer, length, boffset);

    // now the stuff specific to LocalArray
    ip = (int *)(buffer + *boffset);
    *ip++ = rank;
    *ip++ = (int)kind;
    *ip++ = (int)origin;
    *ip++ = (int)needs_dealloc;
    *ip++ = (int)iscontig;
    // skip base addr altogether
    *ip++ = byte_count;
    for (i=0; i<ESMF_MAXDIM; i++) {
        *ip++ = offset[i];
        *ip++ = lbound[i];
        *ip++ = ubound[i];
        *ip++ = counts[i];
        *ip++ = bytestride[i];
    }

    // skip the F90 pointer - it will have to be reconstructed on
    // the remote side.

    cp = (char *)ip;

    // copy the actual data into the buffer.
    // TODO: verify the buffer size first.
    memcpy(cp, base_addr, byte_count);
    cp += byte_count;

    *boffset = (cp - buffer);

    return ESMF_SUCCESS;

 } // end ESMC_LocalArraySerialize

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayDeserialize"
//BOPI
// !IROUTINE:  ESMC_LocalArrayDeserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayDeserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *boffset) {         // inout - original offset, updated to point
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  char *cp;
  int *ip;

  // Deserialize the Base class first.
  localrc = ESMC_Base::ESMC_Deserialize(buffer, boffset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;

  // now the stuff specific to LocalArray
  ip = (int *)(buffer + *boffset);
  rank = *ip++;
  kind = (ESMC_TypeKind)*ip++;
  origin = (ESMC_ArrayOrigin)*ip++;
  needs_dealloc = (ESMC_Logical)*ip++;
  iscontig = (ESMC_Logical)*ip++;
  // skip base addr altogether for now.
  byte_count = *ip++;
  for (int i=0; i<ESMF_MAXDIM; i++) {
    offset[i] = *ip++;
    lbound[i] = *ip++;
    ubound[i] = *ip++;
    counts[i] = *ip++;
    bytestride[i] = *ip++;
  }

  // call a routine which results in the allocation being done
  // from F90, so a dope vector is constructed which we can access
  // later from fortran.
  ESMC_LocalArray *aptr = this;
  FTN(f_esmf_localarrayf90allocate)(&aptr, &rank, &kind, counts, lbound, ubound,
    &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;

  cp = (char *)ip;
  // TODO: verify the buffer size first.
  memcpy(base_addr, cp, byte_count);
  cp += byte_count;

  *boffset = (cp - buffer);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
  
} // end ESMC_LocalArrayDeserialize

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArraySerializeNoData"
//BOPI
// !IROUTINE:  ESMC_LocalArraySerializeNoData - Turn localarray into a byte stream
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArraySerializeNoData(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *boffset) const {   // inout - original offset, updated to point
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in LocalArray class into a stream of bytes.
//
//EOPI
    int fixedpart, nbytes, rc;
    char *cp;
    int *ip, i;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    fixedpart = sizeof(ESMC_LocalArray);
    if ((*length - *boffset) < fixedpart) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                           "Buffer too short to add a LocalArray object", &rc);
        return ESMF_FAILURE; 
        //buffer = (char *)realloc((void *)buffer, *length + 2*fixedpart);
        //*length += 2 * fixedpart;
    }

    // SerializeNoData the Base class first.
    rc = ESMC_Base::ESMC_Serialize(buffer, length, boffset);

    // now the stuff specific to LocalArray
    ip = (int *)(buffer + *boffset);
    *ip++ = rank;
    *ip++ = (int)kind;
    *ip++ = (int)origin;
    *ip++ = (int)needs_dealloc;
    *ip++ = (int)iscontig;

    // skip base addr altogether, skip byte_count, skip offsets, l/u bounds
    // counts, offsets - on the deserialize they will be set to 0.

    // do not serialize the F90 pointer nor data either.

    cp = (char *)ip;
    *boffset = (cp - buffer);

    return ESMF_SUCCESS;

 } // end ESMC_LocalArraySerializeNoData

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayDeserializeNoData"
//BOPI
// !IROUTINE:  ESMC_LocalArrayDeserializeNoData - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayDeserializeNoData(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *boffset) {         // inout - original offset, updated to point
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
    char *cp;
    int *ip, i, nbytes, rc;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    // DeserializeNoData the Base class first.
    rc = ESMC_Base::ESMC_Deserialize(buffer, boffset);

    // now the stuff specific to LocalArray
    ip = (int *)(buffer + *boffset);
    rank = *ip++;
    kind = (ESMC_TypeKind)*ip++;
    origin = (ESMC_ArrayOrigin)*ip++;
    needs_dealloc = (ESMC_Logical)*ip++;
    iscontig = (ESMC_Logical)*ip++;

    // serialize buffer stops here.  the rest has to be explicitly set
    // to 0 by hand.

    base_addr = NULL;
    byte_count = 0;
    for (i=0; i<ESMF_MAXDIM; i++) {
        offset[i] = 0;
        lbound[i] = 0;
        ubound[i] = 0;
        counts[i] = 0;
        bytestride[i] = 0;
    }

    // zero out the f90 pointer area.
    cp = (char *) &f90dopev;
    nbytes = ESMF_F90_PTR_BASE_SIZE;
    for (i=1; i<rank; i++)
        nbytes += ESMF_F90_PTR_PLUS_RANK;
    memset(cp, 0, nbytes);

    // Skip the data, f90 dope vector zero'd out.

    cp = (char *)ip;
    *boffset = (cp - buffer);

    return ESMF_SUCCESS;

 } // end ESMC_LocalArrayDeserializeNoData

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayPrint"
//BOP
// !IROUTINE:  ESMC_LocalArrayPrint - print contents of a LocalArray
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a {\tt ESMC\_LocalArray}.  The options control
//      the type of information and level of detail.  {\tt ESMC\_Base} class
//      method. 

//EOP

//
//  code goes here
//
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int i, j, k, l, m;
    int imax, jmax, kmax, lmax, mmax;
    int tcount, rcount;
    char between = '\n';
    char beforeskip = ' ';
    bool opt_all = false;   // print all data
    bool opt_exc = false;   // print only exclusive region (needs halo len)
    bool opt_byline = false;  // print a row/line
    char msgbuf[ESMF_MAXSTR];

    if (options) {
        if (strstr(options, "full")) opt_all = true;
        if (strstr(options, "exclusive")) opt_exc = true;
        if (strstr(options, "line")) opt_byline = true;
    }

    if (opt_byline) {
        between = ' ';
        beforeskip = '\n';
    }

    sprintf(msgbuf,"LocalArrayPrint: Array at address 0x%08lx:\n", (ESMC_POINTER)this);
    printf(msgbuf);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  
    sprintf(msgbuf,"            rank = %d, kind = %d, ", 
                             this->rank, this->kind);
    printf(msgbuf);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    sprintf(msgbuf,"base_addr = 0x%08lx\n", (ESMC_POINTER)this->base_addr);
    printf(msgbuf);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    sprintf(msgbuf,"            ");
    printf(msgbuf);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
 
    // some error checking against garbage pointers:
    if (rank > 7) {
        sprintf(msgbuf, "invalid rank, %d\n", this->rank);
        printf(msgbuf);
        ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_ERROR);
        return ESMC_RC_OBJ_BAD;
    }

    for (i=0; i<this->rank; i++) {
        sprintf(msgbuf,"dim[%d] = %d  ", i, this->counts[i]);
        printf(msgbuf);
        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    }
    sprintf(msgbuf,"\n");
    printf(msgbuf);
    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    
    // TODO: make this look at one of the option letters to see if user
    //   wants data printed.

        switch (this->kind) {
          case ESMC_TYPEKIND_R4:
            switch (this->rank) {
              case 1:
                sprintf(msgbuf,"  Real, *4, Dim 1, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<tcount; i++) {
                    if (!opt_byline) 
                        sprintf(msgbuf,"(%2d) =  %lg\n", lbound[0]+i,
                          *((ESMC_R4 *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%lg ", *((ESMC_R4 *)(this->base_addr) + i));
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                }
                break;
              case 2:
                sprintf(msgbuf,"  Real, *4, Dim 2, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0;
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline) 
                            sprintf(msgbuf,"(%2d,%2d) =  %lg\n", 
                              lbound[0]+i, lbound[1]+j, 
                              *((ESMC_R4 *)(this->base_addr) + i + j*imax) );
                        else 
                            sprintf(msgbuf,"%lg ",  
                                   *((ESMC_R4 *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                }
                break;
              case 3:
                sprintf(msgbuf,"  Real, *4, Dim 3, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ", 
                        lbound[1]+j, lbound[2]+k);
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %g\n", 
                                   lbound[0]+i, lbound[1]+j, lbound[2]+k,
                                   *((ESMC_R4 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                             sprintf(msgbuf,"%g ", *((ESMC_R4 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                break;    
            }
            break;
          case ESMC_TYPEKIND_R8:
            switch (this->rank) {
              case 1:
                sprintf(msgbuf,"  Real, *8, Dim 1, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<tcount; i++) {
                    if (!opt_byline)
                        sprintf(msgbuf,"(%2d) =  %lg\n", lbound[0]+i,
                          *((ESMC_R8 *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%lg ", *((ESMC_R8 *)(this->base_addr) + i));
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                }
                break;
              case 2:
                sprintf(msgbuf,"  Real, *8, Dim 2, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0;
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d) =  %lg\n", 
                              lbound[0]+i, lbound[1]+j, 
                              *((ESMC_R8 *)(this->base_addr) + i + j*imax) );
                        else
                            sprintf(msgbuf,"%lg ",  
                                   *((ESMC_R8 *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                }
                break;
              case 3:
                sprintf(msgbuf,"  Real, *8, Dim 3, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ",
                          lbound[1]+j, lbound[2]+k);
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %g\n", 
                                   lbound[0]+i, lbound[1]+j, lbound[2]+k,
                                   *((ESMC_R8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                             sprintf(msgbuf,"%g ", *((ESMC_R8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                break;    
            }
            break;
          case ESMC_TYPEKIND_I4:
            switch (this->rank) {
              case 1:
                imax = this->counts[0];
                tcount = imax;
                sprintf(msgbuf,"  Integer, *4, Dim 1, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        sprintf(msgbuf,"(%2d) =  %d\n", lbound[0]+i, 
                               *((int *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%d ",
                               *((int *)(this->base_addr) + i));
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                }
                break;
              case 2:
                sprintf(msgbuf,"  Integer, *4, Dim 2, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0; 
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d) =  %d\n", 
                              lbound[0]+i, lbound[1]+j, 
                              *((int *)(this->base_addr) + i + j*imax) );
                        else
                            sprintf(msgbuf,"%d ", 
                                 *((int *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                }
                break;
              case 3:
                sprintf(msgbuf,"  Integer, *4, Dim 3, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ", 
                          lbound[1]+j, lbound[1]+k);
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %d\n", 
                                   lbound[0]+i, lbound[1]+j, lbound[2]+k,
                                   *((int *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                            sprintf(msgbuf,"%d ", 
                                   *((int *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                break;    
            }
            break;
          case ESMC_TYPEKIND_I8:
            switch (this->rank) {
              case 1:
                imax = this->counts[0];
                tcount = imax;
                sprintf(msgbuf,"  Integer, *8, Dim 1, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        sprintf(msgbuf,"(%2d) =  %ld\n", lbound[0]+i, 
                               *((ESMC_I8 *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%ld ",
                               *((ESMC_I8 *)(this->base_addr) + i));
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                } 
                break;
              case 2:
                sprintf(msgbuf,"  Integer, *8, Dim 2, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0; 
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d) =  %ld\n", 
                              lbound[0]+i, lbound[1]+j, 
                              *((ESMC_I8 *)(this->base_addr) + i + j*imax) );
                        else
                            sprintf(msgbuf,"%ld ", 
                                 *((ESMC_I8 *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                }
                break;
              case 3:
                sprintf(msgbuf,"  Integer, *8, Dim 3, Data values:\n");
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ", 
                          lbound[1]+j, lbound[2]+k);
                          printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %ld\n", 
                                   lbound[0]+i, lbound[1]+j, lbound[2]+k,
                                   *((ESMC_I8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                            sprintf(msgbuf,"%ld ", 
                                   *((ESMC_I8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                           //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                        //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
                printf(msgbuf);
                //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                break;    
            }
            break;
        }

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_LocalArrayPrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayValidate"
//BOP
// !IROUTINE:  ESMC_LocalArrayValidate - internal consistency check
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_LocalArray} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
//
//EOP

//
//  code goes here
//
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_LocalArrayValidate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArrayWrite"
//BOP
// !IROUTINE:  ESMC_LocalArrayWrite - write contents of a LocalArray
//
// !INTERFACE:
      int ESMC_LocalArray::ESMC_LocalArrayWrite(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options,             // in - write options
      const char *filename) const {    // in - file name
//
// !DESCRIPTION:
//      Write the contents of an {\tt ESMC\_LocalArray} to disk.
//
//EOP

//
//  code goes here
//
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    int i, j, k, l, m;
    int imax, jmax, kmax, lmax, mmax;
    int tcount, rcount;
    FILE *ffile = NULL;
    char msgbuf[ESMF_MAXSTR];

    if ((filename == NULL) || (filename[0] == '-')) {
        ffile = stdout;
    } else {
        ffile = fopen(filename, "w");
        if (ffile == NULL) {
            sprintf(msgbuf, "error opening file '%s'\n", filename);
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_FILE_OPEN, 
                                                  msgbuf, &rc);
            return rc;
        }
    }

    fprintf(ffile, "ArrayWrite: Array at address 0x%08lx:  ", 
                           (ESMC_POINTER)this);
    fprintf(ffile, "rank = %d, type_kind = %d\n", 
                             this->rank, this->kind);
    for (i=0; i<this->rank; i++) 
        fprintf(ffile, " dim[%d] = %d  ", i, this->counts[i]);
    fprintf(ffile, "\n");
    
    // TODO: make this look at one of the option letters to see how user
    //   wants data written (ascii, binary, multifile, singlefile).

        switch (this->kind) {
         case ESMC_TYPEKIND_R4:
            switch (this->rank) {
              case 1:
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<tcount; i++) {
                    fprintf(ffile, "%lg\n", *((ESMC_R4 *)(this->base_addr) + i));
                }
                break;
              case 2:
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0;
                for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%lg ",  
                                   *((ESMC_R4 *)(this->base_addr) + i + j*imax) );
                    }
                    fprintf(ffile, "\n");
                }
                break;
              case 3:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%lg ",
                         *((ESMC_R4 *)(this->base_addr) + i + j*imax + k*jmax*imax));
                    }
                    fprintf(ffile, "\n");
                  }
                }
                break;
              default:
                fprintf(ffile, "no code to handle real rank %d yet\n", this->rank);
                break;    
            }
            break;
          case ESMC_TYPEKIND_R8:
            switch (this->rank) {
              case 1:
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<tcount; i++) {
                    fprintf(ffile, "%lg\n", *((ESMC_R8 *)(this->base_addr) + i));
                }
                break;
              case 2:
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0;
                for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%lg ",  
                                   *((ESMC_R8 *)(this->base_addr) + i + j*imax) );
                    }
                    fprintf(ffile, "\n");
                }
                break;
              case 3:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%lg ",
                         *((ESMC_R8 *)(this->base_addr) + i + j*imax + k*jmax*imax));
                    }
                    fprintf(ffile, "\n");
                  }
                }
                break;
              default:
                fprintf(ffile, "no code to handle real rank %d yet\n", this->rank);
                break;    
            }
            break;
          case ESMC_TYPEKIND_I4:
            switch (this->rank) {
              case 1:
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<imax; i++) {
                    fprintf(ffile, "%d\n", *((int *)(this->base_addr) + i));
                }
                break;
              case 2:
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0; 
                for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%d ",
                                    *((int *)(this->base_addr) + i + j*imax) );
                    }
                    fprintf(ffile, "\n");
                }
                break;
              case 3:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%d ", 
                           *((int *)(this->base_addr) + i + j*imax + k*jmax*imax));
                    }
                    fprintf(ffile, "\n");
                  }
                }
                break;
              default:
                fprintf(ffile, "no code to handle integer rank %d yet\n", this->rank);
                break;    
            }
            break;
          case ESMC_TYPEKIND_I8:
            switch (this->rank) {
              case 1:
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<imax; i++) {
                    fprintf(ffile, "%ld\n", *((ESMC_I8 *)(this->base_addr) + i));
                }
                break;
              case 2:
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0; 
                for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%ld ",
                                    *((ESMC_I8 *)(this->base_addr) + i + j*imax) );
                    }
                    fprintf(ffile, "\n");
                }
                break;
              case 3:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    for (i=0; i<imax; i++) {
                        fprintf(ffile, "%ld ", 
                           *((ESMC_I8 *)(this->base_addr) + i + j*imax + k*jmax*imax));
                    }
                    fprintf(ffile, "\n");
                  }
                }
                break;
              default:
                fprintf(ffile, "no code to handle integer rank %d yet\n", this->rank);
                break;    
            }
            break;
        }

    fclose(ffile);

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_LocalArrayWrite

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// native constructor/destructors
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_LocalArray()"
//BOP
// !IROUTINE:  ESMC_LocalArray - native C++ constructor
//
// !INTERFACE:
      ESMC_LocalArray::ESMC_LocalArray(
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

//
//  code goes here
//

 } // end ESMC_LocalArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_LocalArray()"
//BOP
// !IROUTINE:  ~ESMC_LocalArray - native C++ destructor
//
// !INTERFACE:
      ESMC_LocalArray::~ESMC_LocalArray(void) {
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

//
//  code goes here
//

 } // end ~ESMC_LocalArray


