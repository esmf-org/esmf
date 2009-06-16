// $Id: ESMCI_LocalArray.C,v 1.6 2009/06/16 16:45:01 theurich Exp $
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
#define ESMC_FILENAME "ESMCI_LocalArray.C"
//==============================================================================
//
// ESMC LocalArray method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ LocalArray methods declared
// in the companion file ESMCI_LocalArray.h.  
//
// The LocalArray class allows C++ to emulate the richer Fortran language array
// operations. It allows strided access, subsetting operations, known dimension,
// sizes, and typed access to arrays instead of just a starting address to a
// block of memory.  
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_LocalArray.h"

// include higher level, 3rd party or system headers
#include <cstdio>
#include <cstring>

// include ESMF headers
#include "ESMC_Start.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_LocalArray.C,v 1.6 2009/06/16 16:45:01 theurich Exp $";
//-----------------------------------------------------------------------------

  
// prototypes for Fortran calls
extern "C" {

  void FTN(f_esmf_localarrayf90allocate)(ESMCI::LocalArray**, int *, 
    ESMC_TypeKind*, int *, int *, int *, int *);
 
  void FTN(f_esmf_localarrayf90deallocate)(ESMCI::LocalArray**, int*, 
    ESMC_TypeKind *, int *);
 
  void FTN(f_esmf_localarrayadjust)(ESMCI::LocalArray**, int *,
    ESMC_TypeKind*, int *, int *, int *, int *);

  void FTN(f_esmf_localarraycopyf90ptr)(ESMCI::LocalArray** laIn, 
    ESMCI::LocalArray** laOut, int *rc);
  
#ifndef ESMF_NO_INTEGER_1_BYTE
  void FTN(f_esmf_fortrantkrptrcopy1di1)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy2di1)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy3di1)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy4di1)(void *dst, void *src);
#ifndef ESMF_NO_GREATER_THAN_4D
  void FTN(f_esmf_fortrantkrptrcopy5di1)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy6di1)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy7di1)(void *dst, void *src);
#endif
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
  void FTN(f_esmf_fortrantkrptrcopy1di2)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy2di2)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy3di2)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy4di2)(void *dst, void *src);
#ifndef ESMF_NO_GREATER_THAN_4D
  void FTN(f_esmf_fortrantkrptrcopy5di2)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy6di2)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy7di2)(void *dst, void *src);
#endif
#endif
  void FTN(f_esmf_fortrantkrptrcopy1di4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy2di4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy3di4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy4di4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy1di8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy2di8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy3di8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy4di8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy1dr4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy2dr4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy3dr4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy4dr4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy1dr8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy2dr8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy3dr8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy4dr8)(void *dst, void *src);
#ifndef ESMF_NO_GREATER_THAN_4D
  void FTN(f_esmf_fortrantkrptrcopy5di4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy6di4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy7di4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy5di8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy6di8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy7di8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy5dr4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy6dr4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy7dr4)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy5dr8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy6dr8)(void *dst, void *src);
  void FTN(f_esmf_fortrantkrptrcopy7dr8)(void *dst, void *src);
#endif
  
}

  
namespace ESMCI {

  //-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// This section includes all the Local Array create/destroy routines.
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::create - Create a new ESMCI::LocalArray
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  int *icounts,              // number of items in each dim
  void *base,                // if non-null, this is already allocated memory

  CopyFlag docopy,      // if base != NULL, copy data?
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

  LocalArray *a;
  try{
    a = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);  
    return ESMC_NULL_POINTER;
  }

  localrc = a->construct(rank, dk, icounts, base, FROM_CPLUSPLUS,  NULL,
    DO_ALLOCATE, docopy, ESMF_TRUE, name, NULL, NULL, NULL); 
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;
  
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::create - Create a new ESMCI::LocalArray
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  int *icounts,              // number of items in each dim
  int *lbounds,              // lower index number per dim
  int *ubounds,              // upper index number per dim
  void *base,                // if non-null, this is already allocated memory
  CopyFlag docopy,      // if base != NULL, copy data?
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

  LocalArray *a;
  try{
    a = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);  
    return ESMC_NULL_POINTER;
  }

  localrc = a->construct(rank, dk, icounts, base, FROM_CPLUSPLUS, NULL,
    DO_ALLOCATE, docopy, ESMF_TRUE, name, lbounds, ubounds, NULL); 
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;
  
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::create - create ESMCI::LocalArray from copy
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::LocalArray
//
// !ARGUMENTS:
  LocalArray *larrayIn,
  int *lbounds,              // lower index number per dim
  int *ubounds,              // upper index number per dim
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
  LocalArray *larrayOut = new LocalArray;

  // call base class routine to set name 
  larrayOut->ESMC_BaseSetName(NULL, "LocalArray");
  
  // copy the contents
  ESMC_Base baseTemp;
  baseTemp = *larrayOut;  // store base object info in temp. variable
  *larrayOut = *larrayIn; // copy larrayIn content into larrayOut includ. base
  *((ESMC_Base*)larrayOut) = baseTemp; // override base part of larrayOut again
  
  // if lbounds and ubouonds arguments were specified set them in larrayOut
  if (lbounds)
    for (int i=0; i<larrayOut->rank; i++)
      larrayOut->lbound[i] = lbounds[i];
  if (ubounds)
    for (int i=0; i<larrayOut->rank; i++)
      larrayOut->ubound[i] = ubounds[i];
  
  // mark this copy to be responsible for deallocation of its data area alloc
  larrayOut->needs_dealloc = ESMF_TRUE;

  // call into F90 copy method, which will use larrayOut's lbound and ubound
  FTN(f_esmf_localarraycopyf90ptr)(&larrayIn, &larrayOut, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;
  
  // return successfully 
  if (rc != NULL) *rc = ESMF_SUCCESS;
  return larrayOut;

}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::destroy()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::destroy - free an ESMCI::LocalArray object
//
// !INTERFACE:
int LocalArray::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  LocalArray *array) {
//
// !DESCRIPTION:
//  ESMF routine which destroys a LocalArray object previously allocated
//  via an {\tt ESMCI::LocalArray::create()} routine.
//
//EOP
 
    // Initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    localrc = array->destruct();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;

    delete array;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::createNoData()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::createNoData - internal routine, fortran use
//
// !INTERFACE:
LocalArray *LocalArray::createNoData(
//
// !RETURN VALUE:
//     pointer to newly allocated LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  LocalArrayOrigin oflag,    // caller is fortran or C++?
  char *name,                // array name
  int *rc) {                 // return code
//
// !DESCRIPTION:
//  This version of Create is only intended for internal use by the
//  {\tt ESMF\_LocalArrayCreate} fortran routine.  It creates a partially
//  constructed array, then depends on the caller to come back and
//  complete the array with the {\tt SetInfo()} call.  
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

  LocalArray *a;
  try{
    a = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);  
    return ESMC_NULL_POINTER;
  }

  localrc = a->construct(rank, dk, NULL, NULL, oflag, NULL, NO_ALLOCATE,
    DATA_NONE, ESMF_FALSE, name, NULL, NULL, NULL);

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create_F()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::create_F - internal routine for fortran use
//
// !INTERFACE:
LocalArray *LocalArray::create_F(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::LocalArray
//
// !ARGUMENTS:
  int rank,                  // dimensionality
  ESMC_TypeKind dk,          // short/long, etc
  int *icounts,              // counts along each dimension
  struct c_F90ptr *f90ptr,   // opaque type which fortran uses (dope v)
  void *base,                // real start of memory 
  CopyFlag docopy,      // if base is null and this is Copy, alloc here
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

  LocalArray *a;
  try{
    a = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);  
    return ESMC_NULL_POINTER;
  }

  if (base == NULL) 
    localrc = a->construct(rank, dk, icounts, base, FROM_FORTRAN, f90ptr,
      DO_ALLOCATE, DATA_NONE, ESMF_TRUE, name, lbounds, ubounds, offsets); 
  else
    localrc = a->construct(rank, dk, icounts, base, FROM_FORTRAN, f90ptr,
      NO_ALLOCATE, docopy, ESMF_FALSE, name, lbounds, ubounds, offsets); 

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::construct()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::construct() - fill in allocated LocalArray
//
// !INTERFACE:
int LocalArray::construct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  int irank,                 // dimensionality
  ESMC_TypeKind dk,          // short/long, etc  (*2, *4, *8)
  int *icounts,              // number of items in each dim
  void *base,                // base memory address of data block
  LocalArrayOrigin oflag,    // create called from F90 or C++?
  struct c_F90ptr *f90ptr,   // opaque type which fortran understands (dopev)
  LocalArrayDoAllocate aflag, // do we allocate space or not?
  CopyFlag docopy,      // do we make a copy of the data?
  ESMC_Logical dflag,        // do we deallocate space or not?
  char *name,                // array name, default created if NULL
  int *lbounds,              // lower index number per dim
  int *ubounds,              // upper index number per dim
  int *offsets) {            // offset in bytes to start of each dim
//
// !DESCRIPTION:
//  ESMF routine which fills in the contents of an already allocated
//  {\tt ESMCI::LocalArray} object.  May need to do additional allocations
//  as needed.  Must call the corresponding {\tt ESMCI::LocalArray::destruct}
//  routine to free the additional memory.  Intended for internal
//  ESMF use only.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // set object members - some defaults may be overridden further down
  rank = irank;
  typekind = dk;
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
  byte_count = ESMC_TypeKindSize(typekind) * totalcount; 

  // set Fortran dope vector if provided for existing allocation
  if (f90ptr != NULL)
    setFortranPtr(f90ptr);
 
  // call into Fortran to do the allocate if necessary
  if (aflag == DO_ALLOCATE) {
    LocalArray *aptr = this;
    FTN(f_esmf_localarrayf90allocate)(&aptr, &rank, &typekind, counts, 
      lbound, ubound, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
  } 


  // Setup info for calculating index tuple location quickly
  // Needs to be done after lbounds and counts are set
  int currOff=1;
  lOff=0;
  for (int i=0; i<rank; i++) {
    dimOff[i]=currOff;
    lOff +=currOff*lbound[i];

    currOff *=counts[i];
  }  

  // call base class routine to set name 
  ESMC_BaseSetName(name, "LocalArray");
    
  // TODO: memcpy from base to base_addr, proper number of bytes?
  //  if docopy flag is set.
//***
// KDS - Begin change
//***
  if (docopy == DATA_COPY)
  {
    memcpy(base_addr, base, totalcount);
  }
//***
// KDS - End change
//***

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

 }
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::destruct()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::destruct - release LocalArray
//
// !INTERFACE:
      int LocalArray::destruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMCI::LocalArray::construct}, does any additional cleanup before
//      the original {\tt ESMCI::LocalArray} object is freed.  Intended for
//      internal ESMF use only.
//
//EOP

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    LocalArray *aptr = this;

    // check origin and alloc flag, and call dealloc routine if needed 
    if (needs_dealloc != ESMF_TRUE)
    return ESMF_SUCCESS;

    // if there is an F90 dope vector, we have to call back into fortran
    // to deallocate this.   if we want to support a C++ only library,
    // then this code needs to be calling malloc/free or new/delete and
    // needs conditional code to pick the fortran or C++ mem mgt system.
    
    FTN(f_esmf_localarrayf90deallocate)(&aptr, &rank, &typekind, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;

 }
//-----------------------------------------------------------------------------

 
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::adjust()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::adjust - copy and adjust LocalArray object
//
// !INTERFACE:
      LocalArray *LocalArray::adjust(
//
// !RETURN VALUE:
//     pointer to newly allocated LocalArray
//
// !ARGUMENTS:
    CopyFlag copyflag,     // copy or reference original data
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

  // check that lbounds and ubounds arguments match this->counts
  for (int i=0; i<rank; i++){
    if (counts[i] != ubounds[i] - lbounds[i] + 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
        "- Mismatch of lbounds, ubounds and counts", rc);
      return NULL;
    }
  }

  LocalArray *larray;
  
  if (copyflag == DATA_COPY){
    // make a copy of the LocalArray object including the data allocation
    larray = LocalArray::create(this, lbounds, ubounds, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return NULL;
  }else{
    // allocate memory for new LocalArray object
    larray = new LocalArray;
    // copy the LocalArray members, including the _reference_ to its data alloc.
    *larray = *this;
    // mark this copy not to be responsible for deallocation
    larray->needs_dealloc = ESMF_FALSE;
    // adjust the lbound and ubound members in larray copy
    for (int i=0; i<rank; i++){
      larray->lbound[i] = lbounds[i];
      larray->ubound[i] = ubounds[i];
    }
    // adjust the Fortran dope vector to reflect the new bounds
    FTN(f_esmf_localarrayadjust)(&larray, &rank, &typekind, counts,
      larray->lbound, larray->ubound, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return NULL;
  }
  // Setup info for calculating index tuple location quickly
  // Needs to be done after lbounds and counts are set
  int currOff=1;
  larray->lOff=0;
  for (int i=0; i<rank; i++) {
    larray->dimOff[i]=currOff;
    larray->lOff +=currOff*(larray->lbound[i]);

    currOff *=larray->counts[i];
  }  

  // return successfully 
  if (rc) *rc = ESMF_SUCCESS;
  return larray;
}

 
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// get/set routines.
//-----------------------------------------------------------------------------
// Note that most of the Get/Set routines are by value and are inline in
//  the include file.
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::setInfo()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::setInfo - Set the most common Fortran needs
//
// !INTERFACE:
      int LocalArray::setInfo(
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
#ifdef MEMCPYMETHOD
      // note - starts at 1; base includes rank 1 size
      for (i=1; i<rank; i++)
    	bytes += ESMF_F90_PTR_PLUS_RANK;
      memcpy((void *)(&this->f90dopev), (void *)fptr, bytes);
#else
      tkrPtrCopy((void *)(&this->f90dopev), (void *)fptr, typekind, rank);
#endif
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

    byte_count = ESMC_TypeKindSize(typekind) * totalcount;

    // Setup info for calculating index tuple location quickly
    // Needs to be done after lbounds and counts are set
    int currOff=1;
    lOff=0;
    for (int i=0; i<rank; i++) {
      dimOff[i]=currOff;
      lOff +=currOff*lbound[i];
      
      currOff *=counts[i];
    }  
    
    rc = ESMF_SUCCESS;
    return rc;


  }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::getInfo()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::getInfo - Get the most common Fortran needs
//
// !INTERFACE:
      int LocalArray::getInfo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    struct c_F90ptr *fptr,    // out - f90 pointer
    void *base,               // out - base memory address
    int *icounts,             // out - counts along each dim
    int *lbounds,             // out - lowest valid index
    int *ubounds,             // out - highest valid index
    int *offsets)const{       // out - numbytes from base to 1st item/dim
//
// !DESCRIPTION:
//     Gets a list of values associated with a LocalArray object.
//
//EOP

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
    int rc;
 
     // Initialize return code; assume routine not implemented
     rc = ESMC_RC_NOT_IMPL;

    if (fptr) {
#ifdef MEMCPYMETHOD
      // note - starts at 1; base includes rank 1 size
      for (i=1; i<rank; i++)
        bytes += ESMF_F90_PTR_PLUS_RANK;
      memcpy((void *)fptr, (void *)(&this->f90dopev), bytes);
#else
      tkrPtrCopy((void *)fptr, (void *)(&this->f90dopev), typekind, rank);
#endif

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

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::getFortranPtr()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::getFortranPtr - get F90Ptr for a LocalArray
//
// !INTERFACE:
      int LocalArray::getFortranPtr(
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

#ifdef MEMCPYMETHOD
    // note - starts at 1; base includes rank 1 size
    for (i=1; i<rank; i++)
      bytes += ESMF_F90_PTR_PLUS_RANK;
    memcpy((void *)p, (void *)(&this->f90dopev), bytes);
#else
    tkrPtrCopy((void *)p, (void *)(&this->f90dopev), typekind, rank);
#endif

    rc = ESMF_SUCCESS;
    return rc; 

 }
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::setFortranPtr()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::setFortranPtr - set F90Ptr for a LocalArray
//
// !INTERFACE:
      int LocalArray::setFortranPtr(
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

#ifdef MEMCPYMETHOD
    for (i=1; i<rank; i++)
      bytes += ESMF_F90_PTR_PLUS_RANK;
    memcpy((void *)(&this->f90dopev), (void *)p, bytes);
#else
    tkrPtrCopy((void *)(&this->f90dopev), (void *)p, typekind, rank);
#endif
      
    return ESMF_SUCCESS; 

 }


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "getDataInternal"
//BOPI
// !IROUTINE:  getDataInternal - get the data at an index location without error checking
//
// !INTERFACE:
template <class TYPE>
      void LocalArray::getDataInternal(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int *index,          // in - index location
      TYPE *data)          // out - pointer to data. 
{      

//
// !DESCRIPTION:
//    Get the data at a particular index location in a LocalArray. For efficiency's
// sake this routine doesn't do error checking. It's assumed that the error checking
// is occuring at a higher level. For error checking use getData.
//
//EOPI
  int off;

  // Loop through summing up offset for each dimension
  off=-lOff;
  for (int i=0; i<rank; i++) {
    off +=dimOff[i]*index[i];
  }
  
  // Get Data 
  *data=*((TYPE *)((char *)base_addr+ESMC_TypeKindSize(typekind)*off)); 

  // return
  return;
  
} // end getDataInternal

// Add more types here if necessary
template void LocalArray::getDataInternal(int *index, ESMC_R8 *data);
template void LocalArray::getDataInternal(int *index, ESMC_R4 *data);
template void LocalArray::getDataInternal(int *index, ESMC_I4 *data);


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "getData"
//BOPI
// !IROUTINE:  getData - get the data at an index location
//
// !INTERFACE:
template <class TYPE>
      int LocalArray::getData(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int *index,          // in - index location
      TYPE *data)          // out - pointer to data. 
{      

//
// !DESCRIPTION:
//    Get the data at a particular index location in a LocalArray. 
//
// TODO: This method should eventually be made more efficient by precalculating
//        and storing the offsets per Dim. 
//
//EOPI
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Make sure index is within bounds
  for (int i=0; i<rank; i++) {   
    if ((index[i] < lbound[i]) || (index[i] > ubound[i])) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                 "- index outside of LocalArray bounds", &rc);
        return rc;
    }
  }

  // Actually Get Data
  this->getDataInternal(index,data);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
  
} // end getData

// Add more types here if necessary
template int LocalArray::getData(int *index, ESMC_R8 *data);
template int LocalArray::getData(int *index, ESMC_R4 *data);
template int LocalArray::getData(int *index, ESMC_I4 *data);




//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Standard methods - Validate, Print, Read, Write
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::print()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::print - print contents of a LocalArray
//
// !INTERFACE:
      int LocalArray::print(
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
    bool opt_dopev = false;   // print dopev
    bool opt_all = false;   // print all data
    bool opt_exc = false;   // print only exclusive region (needs halo len)
    bool opt_byline = false;  // print a row/line
    char msgbuf[ESMF_MAXSTR];

    if (options) {
        if (strstr(options, "dopev")) opt_dopev = true;
        if (strstr(options, "full")) opt_all = true;
        if (strstr(options, "exclusive")) opt_exc = true;
        if (strstr(options, "line")) opt_byline = true;
    }

    if (opt_byline) {
        between = ' ';
        beforeskip = '\n';
    }

    sprintf(msgbuf,"LocalArrayPrint: Array at address 0x%08lx:\n",
      (ESMC_POINTER)this);
    printf(msgbuf);
    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
  
    sprintf(msgbuf,"            rank = %d, typekind = %d, ", 
                             this->rank, this->typekind);
    printf(msgbuf);
    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
    sprintf(msgbuf,"base_addr = 0x%08lx\n", (ESMC_POINTER)this->base_addr);
    printf(msgbuf);
    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
    sprintf(msgbuf,"            ");
    printf(msgbuf);
    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
    
    if (opt_dopev){ 
      printf("f90dopev: \n");
      int bytes = 0;
      unsigned char *dopev = (unsigned char *)&f90dopev;
      if (base_addr){
        bytes = ESMF_F90_PTR_BASE_SIZE;
        for (i=1; i<rank; i++)
    	  bytes += ESMF_F90_PTR_PLUS_RANK;
      }
      for (i=0; i<bytes; i++)
        printf(" [%03d]\t0x%02x\n", i, (int)dopev[i]);
      
      
    }else{
    
    // some error checking against garbage pointers:
    if (rank > 7) {
        sprintf(msgbuf, "invalid rank, %d\n", this->rank);
        printf(msgbuf);
        ESMC_LogDefault.Write(msgbuf, ESMC_LOG_ERROR, ESMC_CONTEXT);
        return ESMC_RC_OBJ_BAD;
    }

    for (i=0; i<this->rank; i++) {
        sprintf(msgbuf,"dim[%d] = %d  ", i, this->counts[i]);
        printf(msgbuf);
        //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
    }
    sprintf(msgbuf,"\n");
    printf(msgbuf);
    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
    
    // TODO: make this look at one of the option letters to see if user
    //   wants data printed.

    switch (this->typekind) {
      case ESMC_TYPEKIND_R4:
        switch (this->rank) {
          case 1:
            sprintf(msgbuf,"  Real, *4, Dim 1, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            imax = this->counts[0];
            tcount = imax;
            for (i=0; i<tcount; i++) {
                if (!opt_byline) 
                    sprintf(msgbuf,"(%2d) =  %lg\n", lbound[0]+i,
                      *((ESMC_R4 *)(this->base_addr) + i));
                else
                    sprintf(msgbuf,"%lg ", *((ESMC_R4 *)(this->base_addr) + i));
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                   sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                   printf(msgbuf);
                   //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                   i = tcount - 11;
                }
            }
            if (opt_byline) {
                sprintf(msgbuf,"\n");
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            }
            break;
          case 2:
            sprintf(msgbuf,"  Real, *4, Dim 2, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            imax = this->counts[0];
            jmax = this->counts[1];
            tcount = imax * jmax;
            rcount = 0;
            for (j=0; j<jmax; j++) {
                if (opt_byline) {
                    sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                }
            }
            break;
          case 3:
            sprintf(msgbuf,"  Real, *4, Dim 3, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       int krem;
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                }
              }
            }
            break;
          default:
            sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            break;    
        }
        break;
      case ESMC_TYPEKIND_R8:
        switch (this->rank) {
          case 1:
            sprintf(msgbuf,"  Real, *8, Dim 1, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            imax = this->counts[0];
            tcount = imax;
            for (i=0; i<tcount; i++) {
                if (!opt_byline)
                    sprintf(msgbuf,"(%2d) =  %lg\n", lbound[0]+i,
                      *((ESMC_R8 *)(this->base_addr) + i));
                else
                    sprintf(msgbuf,"%lg ", *((ESMC_R8 *)(this->base_addr) + i));
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                   sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                   printf(msgbuf);
                   //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                   i = tcount - 11;
                }
            }
            if (opt_byline) {
                sprintf(msgbuf,"\n");
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            }
            break;
          case 2:
            sprintf(msgbuf,"  Real, *8, Dim 2, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            imax = this->counts[0];
            jmax = this->counts[1];
            tcount = imax * jmax;
            rcount = 0;
            for (j=0; j<jmax; j++) {
                if (opt_byline) {
                    sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                }
            }
            break;
          case 3:
            sprintf(msgbuf,"  Real, *8, Dim 3, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       int krem;
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                }
              }
            }
            break;
          default:
            sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            for (i=0; i<imax; i++) {
                if (!opt_byline)
                    sprintf(msgbuf,"(%2d) =  %d\n", lbound[0]+i, 
                           *((int *)(this->base_addr) + i));
                else
                    sprintf(msgbuf,"%d ",
                           *((int *)(this->base_addr) + i));
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                   sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                   printf(msgbuf);
                   //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                   i = tcount - 11;
                }
            }
            if (opt_byline) {
                sprintf(msgbuf,"\n");
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            }
            break;
          case 2:
            sprintf(msgbuf,"  Integer, *4, Dim 2, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            imax = this->counts[0];
            jmax = this->counts[1];
            tcount = imax * jmax;
            rcount = 0; 
            for (j=0; j<jmax; j++) {
                if (opt_byline) {
                    sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                } 
            }
            break;
          case 3:
            sprintf(msgbuf,"  Integer, *4, Dim 3, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       int krem;
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                } 
              }
            }
            break;
          default:
            sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            for (i=0; i<imax; i++) {
                if (!opt_byline)
                    sprintf(msgbuf,"(%2d) =  %ld\n", lbound[0]+i, 
                           *((ESMC_I8 *)(this->base_addr) + i));
                else
                    sprintf(msgbuf,"%ld ",
                           *((ESMC_I8 *)(this->base_addr) + i));
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                   sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                   printf(msgbuf);
                   //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                   i = tcount - 11;
                }
            }
            if (opt_byline) {
                sprintf(msgbuf,"\n");
                printf(msgbuf);
                //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            } 
            break;
          case 2:
            sprintf(msgbuf,"  Integer, *8, Dim 2, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            imax = this->counts[0];
            jmax = this->counts[1];
            tcount = imax * jmax;
            rcount = 0; 
            for (j=0; j<jmax; j++) {
                if (opt_byline) {
                    sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                } 
            }
            break;
          case 3:
            sprintf(msgbuf,"  Integer, *8, Dim 3, Data values:\n");
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
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
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       int krem;
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                       //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                    //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
                } 
              }
            }
            break;
          default:
            sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
            printf(msgbuf);
            //ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO, ESMC_CONTEXT);
            break;    
        }
        break;
    }
    }

    rc = ESMF_SUCCESS;
    return rc;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::validate()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::validate - internal consistency check
//
// !INTERFACE:
      int LocalArray::validate(
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

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::write()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::write - write contents of a LocalArray
//
// !INTERFACE:
      int LocalArray::write(
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
            ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN, 
                                                  msgbuf, &rc);
            return rc;
        }
    }

    fprintf(ffile, "ArrayWrite: Array at address 0x%08lx:  ", 
                           (ESMC_POINTER)this);
    fprintf(ffile, "rank = %d, typekind = %d\n", 
                             this->rank, this->typekind);
    for (i=0; i<this->rank; i++) 
        fprintf(ffile, " dim[%d] = %d  ", i, this->counts[i]);
    fprintf(ffile, "\n");
    
    // TODO: make this look at one of the option letters to see how user
    //   wants data written (ascii, binary, multifile, singlefile).

        switch (this->typekind) {
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

 }

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// native constructor/destructors
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "LocalArray()"
//BOP
// !IROUTINE:  LocalArray - native C++ constructor
//
// !INTERFACE:
      LocalArray::LocalArray(
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

 } // end LocalArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~LocalArray()"
//BOP
// !IROUTINE:  ~LocalArray - native C++ destructor
//
// !INTERFACE:
      LocalArray::~LocalArray(void) {
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

 } // end ~LocalArray

 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::tkrPtrCopy()"
//BOP
// !IROUTINE:  ESMCI::LocalArray::tkrPtrCopy - portably copy Fortran dope vector
//
// !INTERFACE:
  int LocalArray::tkrPtrCopy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    void *dst, 
    void *src, 
    ESMC_TypeKind typekind,
    int rank){
//
// !DESCRIPTION:
//      
//
//EOP

//
//  code goes here
//
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  switch (typekind){
#ifndef ESMF_NO_INTEGER_1_BYTE
    case ESMC_TYPEKIND_I1:
    switch (rank){
      case 1:
      FTN(f_esmf_fortrantkrptrcopy1di1)(dst, src);
      break;
      case 2:
      FTN(f_esmf_fortrantkrptrcopy2di1)(dst, src);
      break;
      case 3:
      FTN(f_esmf_fortrantkrptrcopy3di1)(dst, src);
      break;
      case 4:
      FTN(f_esmf_fortrantkrptrcopy4di1)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN(f_esmf_fortrantkrptrcopy5di1)(dst, src);
      break;
      case 6:
      FTN(f_esmf_fortrantkrptrcopy6di1)(dst, src);
      break;
      case 7:
      FTN(f_esmf_fortrantkrptrcopy7di1)(dst, src);
      break;
#endif
    }
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
    case ESMC_TYPEKIND_I2:
    switch (rank){
      case 1:
      FTN(f_esmf_fortrantkrptrcopy1di2)(dst, src);
      break;
      case 2:
      FTN(f_esmf_fortrantkrptrcopy2di2)(dst, src);
      break;
      case 3:
      FTN(f_esmf_fortrantkrptrcopy3di2)(dst, src);
      break;
      case 4:
      FTN(f_esmf_fortrantkrptrcopy4di2)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN(f_esmf_fortrantkrptrcopy5di2)(dst, src);
      break;
      case 6:
      FTN(f_esmf_fortrantkrptrcopy6di2)(dst, src);
      break;
      case 7:
      FTN(f_esmf_fortrantkrptrcopy7di2)(dst, src);
      break;
#endif
    }
#endif
    case ESMC_TYPEKIND_I4:
    switch (rank){
      case 1:
      FTN(f_esmf_fortrantkrptrcopy1di4)(dst, src);
      break;
      case 2:
      FTN(f_esmf_fortrantkrptrcopy2di4)(dst, src);
      break;
      case 3:
      FTN(f_esmf_fortrantkrptrcopy3di4)(dst, src);
      break;
      case 4:
      FTN(f_esmf_fortrantkrptrcopy4di4)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN(f_esmf_fortrantkrptrcopy5di4)(dst, src);
      break;
      case 6:
      FTN(f_esmf_fortrantkrptrcopy6di4)(dst, src);
      break;
      case 7:
      FTN(f_esmf_fortrantkrptrcopy7di4)(dst, src);
      break;
#endif
    }
    case ESMC_TYPEKIND_I8:
    switch (rank){
      case 1:
      FTN(f_esmf_fortrantkrptrcopy1di8)(dst, src);
      break;
      case 2:
      FTN(f_esmf_fortrantkrptrcopy2di8)(dst, src);
      break;
      case 3:
      FTN(f_esmf_fortrantkrptrcopy3di8)(dst, src);
      break;
      case 4:
      FTN(f_esmf_fortrantkrptrcopy4di8)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN(f_esmf_fortrantkrptrcopy5di8)(dst, src);
      break;
      case 6:
      FTN(f_esmf_fortrantkrptrcopy6di8)(dst, src);
      break;
      case 7:
      FTN(f_esmf_fortrantkrptrcopy7di8)(dst, src);
      break;
#endif
    }
    case ESMC_TYPEKIND_R4:
    switch (rank){
      case 1:
      FTN(f_esmf_fortrantkrptrcopy1dr4)(dst, src);
      break;
      case 2:
      FTN(f_esmf_fortrantkrptrcopy2dr4)(dst, src);
      break;
      case 3:
      FTN(f_esmf_fortrantkrptrcopy3dr4)(dst, src);
      break;
      case 4:
      FTN(f_esmf_fortrantkrptrcopy4dr4)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN(f_esmf_fortrantkrptrcopy5dr4)(dst, src);
      break;
      case 6:
      FTN(f_esmf_fortrantkrptrcopy6dr4)(dst, src);
      break;
      case 7:
      FTN(f_esmf_fortrantkrptrcopy7dr4)(dst, src);
      break;
#endif
    }
    case ESMC_TYPEKIND_R8:
    switch (rank){
      case 1:
      FTN(f_esmf_fortrantkrptrcopy1dr8)(dst, src);
      break;
      case 2:
      FTN(f_esmf_fortrantkrptrcopy2dr8)(dst, src);
      break;
      case 3:
      FTN(f_esmf_fortrantkrptrcopy3dr8)(dst, src);
      break;
      case 4:
      FTN(f_esmf_fortrantkrptrcopy4dr8)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN(f_esmf_fortrantkrptrcopy5dr8)(dst, src);
      break;
      case 6:
      FTN(f_esmf_fortrantkrptrcopy6dr8)(dst, src);
      break;
      case 7:
      FTN(f_esmf_fortrantkrptrcopy7dr8)(dst, src);
      break;
#endif
    }
    break;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

 }

//-----------------------------------------------------------------------------
 
 

} // namespace ESMCI

