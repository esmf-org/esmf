// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
// LocalArray class implementation (body) file
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
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

  
// prototypes for Fortran calls
extern "C" {

  void FTN_X(f_esmf_localarrayf90allocate)(ESMCI::LocalArray**, int *, 
    ESMC_TypeKind_Flag*, int *, int *, int *, int *);
 
  void FTN_X(f_esmf_localarrayf90deallocate)(ESMCI::LocalArray**, int*, 
    ESMC_TypeKind_Flag *, int *);
 
  void FTN_X(f_esmf_localarrayadjust)(ESMCI::LocalArray**, int *,
    ESMC_TypeKind_Flag*, const int *, const int *, const int *, int *);

  void FTN_X(f_esmf_localarraycopyf90ptr)(const ESMCI::LocalArray** laIn, 
    ESMCI::LocalArray** laOut, int *rc);
  
  void FTN_X(f_esmf_localarrayctof90)(ESMCI::LocalArray**, void *, int *, 
    ESMC_TypeKind_Flag*, int *, int *, int *, int *);
 
#ifndef ESMF_NO_INTEGER_1_BYTE
  void FTN_X(f_esmf_fortrantkrptrcopy1di1)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy2di1)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy3di1)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy4di1)(void *dst, void *src);
#ifndef ESMF_NO_GREATER_THAN_4D
  void FTN_X(f_esmf_fortrantkrptrcopy5di1)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy6di1)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy7di1)(void *dst, void *src);
#endif
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
  void FTN_X(f_esmf_fortrantkrptrcopy1di2)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy2di2)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy3di2)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy4di2)(void *dst, void *src);
#ifndef ESMF_NO_GREATER_THAN_4D
  void FTN_X(f_esmf_fortrantkrptrcopy5di2)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy6di2)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy7di2)(void *dst, void *src);
#endif
#endif
  void FTN_X(f_esmf_fortrantkrptrcopy1di4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy2di4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy3di4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy4di4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy1di8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy2di8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy3di8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy4di8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy1dr4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy2dr4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy3dr4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy4dr4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy1dr8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy2dr8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy3dr8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy4dr8)(void *dst, void *src);
#ifndef ESMF_NO_GREATER_THAN_4D
  void FTN_X(f_esmf_fortrantkrptrcopy5di4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy6di4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy7di4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy5di8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy6di8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy7di8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy5dr4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy6dr4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy7dr4)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy5dr8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy6dr8)(void *dst, void *src);
  void FTN_X(f_esmf_fortrantkrptrcopy7dr8)(void *dst, void *src);
#endif
  
}

  
namespace ESMCI {

  //-----------------------------------------------------------------------------
//
// construct() and destruct()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::construct()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::construct() - fill in allocated LocalArray
//
// !INTERFACE:
int LocalArray::construct(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  bool aflag,                 // allocate space for data?
  CopyFlag docopy,            // make a data copy from ibase_addr?
  ESMC_TypeKind_Flag tk,      // I1, I2, I4, I8, R4, R8
  int irank,                  // 1, 2, ..., ESMF_MAXDIM
  LocalArrayOrigin oflag,     // create called from Fortran or C++?
  bool dflag,                 // responsible for deallocation?
  const int *offsets,         // offset in bytes to start of each dim
  const int *lbounds,         // lower index number per dim
  const int *ubounds,         // upper index number per dim
  const int *icounts,         // number of items in each dim
  void *ibase_addr,           // base memory address of data block
  struct c_F90ptr *f90ptr     // opaque type of Fortran dope vector
  ){
//
// !DESCRIPTION:
//  ESMF routine which fills in the contents of an already allocated
//  {\tt ESMCI::LocalArray} object.  May need to do additional allocations
//  as needed.  Must call the corresponding {\tt ESMCI::LocalArray::destruct}
//  routine to free the additional memory.  Intended for internal
//  ESMF use only.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // set object members - some defaults may be overridden further down
  rank = irank;
  typekind = tk;
  base_addr = ibase_addr;
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
  dealloc = dflag;
  byte_count = ESMC_TypeKind_FlagSize(typekind) * totalcount; 

  // set Fortran dope vector if provided for existing allocation
  if (f90ptr != NULL)
    setFortranDopev(f90ptr);
 
  if (aflag){
    // call into Fortran to do the allocate, also sets internal LocalArray info
    LocalArray *aptr = this;
    FTN_X(f_esmf_localarrayf90allocate)(&aptr, &rank, &typekind, counts, 
      lbound, ubound, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }else if (docopy == DATA_REF){
    // call into Fortran to cast ibase_addr to Fortran pointer
    LocalArray *aptr = this;
    FTN_X(f_esmf_localarrayctof90)(&aptr, ibase_addr, &rank, &typekind, counts, 
      lbound, ubound, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

  // Setup info for calculating index tuple location quickly
  // Needs to be done after lbounds and counts are set
  int currOff=1;
  lOff=0;
  for (int i=0; i<rank; i++){
    dimOff[i]=currOff;
    lOff +=currOff*lbound[i];
    currOff *=counts[i];
  }  

  if (docopy == DATA_COPY){
    if (ibase_addr == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Cannot copy data when ibase_addr not provided", ESMC_CONTEXT, &rc);
      return rc;
    }
    // copy data
    memcpy(base_addr, ibase_addr, byte_count);
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::destruct()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::destruct - release LocalArray
//
// !INTERFACE:
      int LocalArray::destruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void){
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMCI::LocalArray::construct}, does any additional cleanup before
//      the original {\tt ESMCI::LocalArray} object is freed.  Intended for
//      internal ESMF use only.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  LocalArray *aptr = this;

  if (dealloc){
    // must deallocate data allocation
    FTN_X(f_esmf_localarrayf90deallocate)(&aptr, &rank, &typekind, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------
  
  
//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::create - Create new ESMCI::LocalArray object
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//    pointer to newly allocated LocalArray object
//
// !ARGUMENTS:
  ESMC_TypeKind_Flag tk,      // I1, I2, I4, I8, R4, R8
  int rank,                   // 1, 2, ..., ESMF_MAXDIM
  LocalArrayOrigin oflag,     // caller is fortran or C++?
  int *rc){                   // return code
//
// !DESCRIPTION:
//  This version of create() creates a partially constructed LocalArray
//  object. The caller must complete the LocalArray construction with the
//  {\tt LocalArray::setInfo()} call. It is broken up this way to minimize
//  the amount of macro-generated code needed in {\tt ESMF\_LocalArray.cppF90}
//  when used as the back-end to the Fortran API {\tt ESMF_LocalArrayCreate()}.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;

  LocalArray *a;
  try{
    a = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);  
    return ESMC_NULL_POINTER;
  }
  
  localrc = a->construct(false, DATA_NONE, tk, rank, oflag, false,
    NULL, NULL, NULL, NULL, NULL, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return ESMC_NULL_POINTER;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::create - Create new ESMCI::LocalArray object
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//    pointer to newly allocated ESMCI::LocalArray object
//
// !ARGUMENTS:
  ESMC_TypeKind_Flag tk,      // I1, I2, I4, I8, R4, R8
  int rank,                   // 1, 2, ..., ESMF_MAXDIM
  const int *counts,          // number of items in each dim
  void *base_addr,            // if non-null, this is allocated memory
  CopyFlag docopy,            // make a data copy from base_addr?
  int *rc){                   // return code
//
// !DESCRIPTION:
//  This routine is the C++ entry point for creating an {\tt ESMC\_LocalArray}
//  object.  Unlike natural C++ arrays which can be as simple as the
//  base address pointer and the number of bytes necessary to move to
//  the next item, {\tt ESMC\_LocalArray}s are richer in the associated metadata
//  which allows them to behave more like Fortran arrays.
//
//  This routine allocates memory for data and conditionally copies from
//  base_addr.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;

  LocalArray *a;
  try{
    a = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);  
    return ESMC_NULL_POINTER;
  }

  // check whether allocation is needed on the Fortran side
  bool allocateF = true;
  if (docopy == DATA_REF){
    if (base_addr == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Must provide valid pointer for DATA_REF", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    allocateF = false; // no allocation on Fortran side needed
  }
  
  // construct LocalArray internals, allocate memory for data
  localrc = a->construct(allocateF, docopy, tk, rank, FROM_CPLUSPLUS, allocateF,
    NULL, NULL, NULL, counts, base_addr, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return ESMC_NULL_POINTER;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::create - Create new ESMCI::LocalArray object
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//    pointer to newly allocated ESMCI::LocalArray object
//
// !ARGUMENTS:
  ESMC_TypeKind_Flag tk,      // I1, I2, I4, I8, R4, R8
  int rank,                   // 1, 2, ..., ESMF_MAXDIM
  const int *counts,          // number of items in each dim
  const int *lbounds,         // lower index number per dim
  const int *ubounds,         // upper index number per dim
  void *base_addr,            // if non-null, this is allocated memory
  CopyFlag docopy,            // make a data copy from base_addr?
  int *rc){                   // return code
//
// !DESCRIPTION:
//  This routine is the C++ entry point for creating an {\tt ESMC\_LocalArray}
//  object.  Unlike natural C++ arrays which can be as simple as the
//  base address pointer and the number of bytes necessary to move to
//  the next item, {\tt ESMC\_LocalArray}s are richer in the associated metadata
//  which allows them to behave more like Fortran arrays.
//
//  This routine allocates memory for data and conditionally copies from
//  base_addr.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;

  LocalArray *a;
  try{
    a = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);  
    return ESMC_NULL_POINTER;
  }

  // check whether allocation is needed on the Fortran side
  bool allocateF = true;
  if (docopy == DATA_REF){
    if (base_addr == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Must provide valid pointer for DATA_REF", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    allocateF = false; // no allocation on Fortran side needed
  }
  
  // construct LocalArray internals, allocate memory for data
  localrc = a->construct(allocateF, docopy, tk, rank, FROM_CPLUSPLUS, allocateF,
    NULL, lbounds, ubounds, counts, base_addr, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return ESMC_NULL_POINTER;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return a;
  
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::create - create ESMCI::LocalArray from copy
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//    pointer to newly allocated ESMCI::LocalArray object
//
// !ARGUMENTS:
  const LocalArray *larrayIn, // object to copy from
  const int *lbounds,         // lower index number per dim
  const int *ubounds,         // upper index number per dim
  int *rc){                   // return code
//
// !DESCRIPTION:
//  Deep copy from {\tt larrayIn} with the option to adjust lbounds and ubounds.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;

  LocalArray *larrayOut;
  try{
    larrayOut = new LocalArray;
  }catch(...){
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);  
    return ESMC_NULL_POINTER;
  }

  *larrayOut = *larrayIn; // copy larrayIn content into larrayOut
  
  // if lbounds and ubounds arguments were specified set them in larrayOut
  if (lbounds)
    for (int i=0; i<larrayOut->rank; i++)
      larrayOut->lbound[i] = lbounds[i];
  if (ubounds)
    for (int i=0; i<larrayOut->rank; i++)
      larrayOut->ubound[i] = ubounds[i];
  
  // mark LocalArray responsible for deallocation of its data area allocation
  larrayOut->dealloc = true;

  // call into Fortran copy method, which will use larrayOut's lbound and ubound
  FTN_X(f_esmf_localarraycopyf90ptr)(&larrayIn, &larrayOut, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return ESMC_NULL_POINTER;
  
  // return successfully 
  if (rc != NULL) *rc = ESMF_SUCCESS;
  return larrayOut;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::create()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::create - create ESMCI::LocalArray from existing object, adjusting bounds in Fortran dope vector
//
// !INTERFACE:
LocalArray *LocalArray::create(
//
// !RETURN VALUE:
//    pointer to newly allocated ESMCI::LocalArray object
//
// !ARGUMENTS:
  const LocalArray *larrayIn, // object to copy from
  CopyFlag copyflag,          // copy or reference original data
  const int *lbounds,         // lower index number per dim
  const int *ubounds,         // upper index number per dim
  int *rc){                   // return code
//
// !DESCRIPTION:
//  Copy from {\tt larrayIn} with the option to adjust lbounds and ubounds in
//  the Fortran dope vector. Depending on {\tt copyflag} a deep copy of the
//  data is made, in which case memory for the data will be allocated, or
//  the existing data in {\tt larrayIn} will be referenced by the returned
//  LocalArray object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;

  // set some variables according to larrayIn
  int rank = larrayIn->getRank();
  ESMC_TypeKind_Flag typekind = larrayIn->getTypeKind();
  const int *counts = larrayIn->getCounts();
  
  // check that lbounds and ubounds arguments match counts and find totalcount
  int totalcount = 1;
  for (int i=0; i<rank; i++){
    totalcount *= counts[i];
    if (counts[i] != ubounds[i] - lbounds[i] + 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Mismatch of lbounds, ubounds and counts", ESMC_CONTEXT, rc);
      return NULL;
    }
  }

  LocalArray *larrayOut;
  
  if (copyflag == DATA_COPY){
    // make a copy of the LocalArray object including the data allocation
    larrayOut = LocalArray::create(larrayIn, lbounds, ubounds, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return NULL;
  }else{
    // allocate memory for new LocalArray object
    try{
      larrayOut = new LocalArray;
    }catch(...){
      // allocation error
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);  
      return ESMC_NULL_POINTER;
    }
    // copy the LocalArray members, including the _reference_ to its data alloc.
    *larrayOut = *larrayIn;
    // mark this copy not to be responsible for deallocation
    larrayOut->dealloc = false;
    // adjust the lbound and ubound members in larray copy
    for (int i=0; i<rank; i++){
      larrayOut->lbound[i] = lbounds[i];
      larrayOut->ubound[i] = ubounds[i];
    }
    if (totalcount > 0){
      // adjust the Fortran dope vector to reflect the new bounds
      FTN_X(f_esmf_localarrayadjust)(&larrayOut, &rank, &typekind, counts,
        larrayOut->lbound, larrayOut->ubound, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, 
        ESMC_CONTEXT, rc)) return NULL;
    }
  }
  
  // Setup info for calculating index tuple location quickly
  // Needs to be done after lbounds and counts are set
  int currOff=1;
  larrayOut->lOff=0;
  for (int i=0; i<rank; i++) {
    larrayOut->dimOff[i]=currOff;
    larrayOut->lOff +=currOff*(larrayOut->lbound[i]);
    currOff *=larrayOut->counts[i];
  }  

  // return successfully 
  if (rc) *rc = ESMF_SUCCESS;
  return larrayOut;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::destroy()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::destroy - destroy an ESMCI::LocalArray object
//
// !INTERFACE:
int LocalArray::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  LocalArray *localarray) {
//
// !DESCRIPTION:
//  ESMF routine which destroys a LocalArray object previously allocated
//  via an {\tt ESMCI::LocalArray::create()} routine.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  localrc = localarray->destruct();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  delete localarray;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// get() and set()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::setInfo()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::setInfo - Set most LocalArray members
//
// !INTERFACE:
int LocalArray::setInfo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  struct c_F90ptr *fptr,    // in - Fortran dope vector
  void *ibase_addr,         // in - base memory address
  const int *icounts,       // in - counts along each dim
  const int *lbounds,       // in - lowest valid index
  const int *ubounds,       // in - highest valid index
  const int *offsets,       // in - numbytes from base to 1st item/dim
  const bool *cflag,        // in - is memory chunk contiguous?
  const bool *dflag){       // in - do we need to deallocate at delete?
//
// !DESCRIPTION:
//  Sets a list of values associated with an already created pointer.
//  This particular set was chosen to match well with creation on the
//  Fortran side.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;  

  // make a copy of the Fortran dope vector if it was provided
  if (fptr)
    tkrPtrCopy((void *)(&f90dopev), (void *)fptr, typekind, rank);

  // make a copy of the base address of data allocation if it was provided
  if (ibase_addr)
    base_addr = ibase_addr;

  // valid values
  int totalcount = 1;
  for (int i=0; i<rank; i++) {
    counts[i]     = icounts ? icounts[i] : 0;
    offset[i]     = offsets ? offsets[i] : 0;
    bytestride[i] = 0;
    lbound[i] = lbounds ? lbounds[i] : 1;
    ubound[i] = ubounds ? ubounds[i] : counts[i];
    totalcount *= counts[i];
  }
  // filler for unused ranks
  for (int i=rank; i<ESMF_MAXDIM; i++) {
    counts[i]     = 0;
    offset[i]     = 0;
    bytestride[i] = 1;
    lbound[i] = 1;
    ubound[i] = 1;
  }
  if (cflag)
    contig = *cflag;
  if (dflag)
    dealloc = *dflag;

  byte_count = ESMC_TypeKind_FlagSize(typekind) * totalcount;

  // Setup info for calculating index tuple location quickly
  // Needs to be done after lbounds and counts are set
  int currOff=1;
  lOff=0;
  for (int i=0; i<rank; i++) {
    dimOff[i]=currOff;
    lOff +=currOff*lbound[i];
    currOff *=counts[i];
  }  
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::setFortranDopev()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::setFortranDopev - set Fortran dope vector
//
// !INTERFACE:
int LocalArray::setFortranDopev(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  struct c_F90ptr *p){      // in - Fortran dope vector block
//
// !DESCRIPTION:
//  Sets the Fortran dope vector inside {\tt ESMC\_LocalArray} object.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // call portable implementation of Fortran dope vector copy method
  tkrPtrCopy((void *)(&f90dopev), (void *)p, typekind, rank);
      
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::getFortranDopev()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::getFortranDopev - get Fortran dope vector
//
// !INTERFACE:
int LocalArray::getFortranDopev(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  struct c_F90ptr *p) const{      // out - fortran 90 array pointer
//
// !DESCRIPTION:
//     Return a stored F90 pointer block.  The size may vary with rank.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // call portable implementation of Fortran dope vector copy method
  tkrPtrCopy((void *)p, (void *)(&f90dopev), typekind, rank);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------
 

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
  int *index,           // in - index location
  TYPE *data){          // out - pointer to data. 
//
// !DESCRIPTION:
//  Get the data at a particular index location in a LocalArray. For
//  efficiency's sake this routine doesn't do error checking. It's assumed 
//  that the error checking is occurring at a higher level. For error checking
//  use getData.
//
//EOPI
//-----------------------------------------------------------------------------
  int off;

  // Loop through summing up offset for each dimension
  off=-lOff;
  for (int i=0; i<rank; i++)
    off +=dimOff[i]*index[i];
  
  // Get Data 
  *data=*((TYPE *)((char *)base_addr+ESMC_TypeKind_FlagSize(typekind)*off)); 
}
//-----------------------------------------------------------------------------
// Add more types here if necessary
template void LocalArray::getDataInternal(int *index, ESMC_R8 *data);
template void LocalArray::getDataInternal(int *index, ESMC_R4 *data);
template void LocalArray::getDataInternal(int *index, ESMC_I4 *data);
//-----------------------------------------------------------------------------




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
//    int error return code
//
// !ARGUMENTS:
  int *index,           // in - index location
  TYPE *data){          // out - pointer to data. 
//
// !DESCRIPTION:
//  Get the data at a particular index location in a LocalArray. 
//
// TODO: This method should eventually be made more efficient by precalculating
//        and storing the offsets per Dim. 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Make sure index is within bounds
  for (int i=0; i<rank; i++) {   
    if ((index[i] < lbound[i]) || (index[i] > ubound[i])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- index outside of LocalArray bounds", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // Actually Get Data
  this->getDataInternal(index,data);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------
// Add more types here if necessary
template int LocalArray::getData(int *index, ESMC_R8 *data);
template int LocalArray::getData(int *index, ESMC_R4 *data);
template int LocalArray::getData(int *index, ESMC_I4 *data);
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "setDataInternal"
//BOPI
// !IROUTINE:  setDataInternal - set the data at an index location without error checking
//
// !INTERFACE:
template <class TYPE>
void LocalArray::setDataInternal(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
  int *index,           // in - index location
  TYPE data){           // in - data to set 
//
// !DESCRIPTION:
//  Get the data at a particular index location in a LocalArray. For
//  efficiency's sake this routine doesn't do error checking. It's assumed 
//  that the error checking is occurring at a higher level. For error checking
//  use setData.
//
//EOPI
//-----------------------------------------------------------------------------
  int off;

  // Loop through summing up offset for each dimension
  off=-lOff;
  for (int i=0; i<rank; i++)
    off +=dimOff[i]*index[i];
  
  // Set Data 
  *((TYPE *)((char *)base_addr+ESMC_TypeKind_FlagSize(typekind)*off))=data; 
}
//-----------------------------------------------------------------------------
// Add more types here if necessary
template void LocalArray::setDataInternal(int *index, ESMC_R8 data);
template void LocalArray::setDataInternal(int *index, ESMC_R4 data);
template void LocalArray::setDataInternal(int *index, ESMC_I4 data);
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "setData"
//BOPI
// !IROUTINE:  setData - get the data at an index location
//
// !INTERFACE:
template <class TYPE>
int LocalArray::setData(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  int *index,           // in - index location
  TYPE data){          //  in - data. 
//
// !DESCRIPTION:
//  Set the data at a particular index location in a LocalArray. 
//
// TODO: This method should eventually be made more efficient by precalculating
//        and storing the offsets per Dim. 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Make sure index is within bounds
  for (int i=0; i<rank; i++) {   
    if ((index[i] < lbound[i]) || (index[i] > ubound[i])) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- index outside of LocalArray bounds", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // Actually Get Data
  this->setDataInternal(index,data);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------
// Add more types here if necessary
template int LocalArray::setData(int *index, ESMC_R8 data);
template int LocalArray::setData(int *index, ESMC_R4 data);
template int LocalArray::setData(int *index, ESMC_I4 data);
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
//
// print(), write(), validate()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::print()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::print - print contents of a LocalArray
//
// !INTERFACE:
int LocalArray::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  const char *options)const{      //  in - print options
//
// !DESCRIPTION:
//  Print information about a {\tt ESMCI::LocalArray}.  The options control
//  the type of information and level of detail.
//
//EOPI
//-----------------------------------------------------------------------------
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

  if (options){
    if (strstr(options, "dopev")) opt_dopev = true;
    if (strstr(options, "full")) opt_all = true;
    if (strstr(options, "exclusive")) opt_exc = true;
    if (strstr(options, "line")) opt_byline = true;
  }

  if (opt_byline) {
    between = ' ';
    beforeskip = '\n';
  }

  sprintf(msgbuf,"LocalArrayPrint: Array at address %p:\n", this);
  printf("%s", msgbuf);
  sprintf(msgbuf,"            rank = %d, typekind = %d, ", 
                           this->rank, this->typekind);
  printf("%s", msgbuf);
  sprintf(msgbuf,"base_addr = %p\n", this->base_addr);
  printf("%s", msgbuf);
  sprintf(msgbuf,"            ");
  printf("%s", msgbuf);
  
  if (opt_dopev){ 
    printf("f90dopev: \n");
    int bytes = 0;
    unsigned char *dopev = (unsigned char *)&f90dopev;
    if (base_addr)
      bytes = ESMF_FPTR_BASE_SIZE + ESMF_MAXDIM * ESMF_FPTR_PLUS_RANK;
    for (i=0; i<bytes; i++)
      printf(" [%03d]\t0x%02x\n", i, (int)dopev[i]);
    
  }else{
  
  // some error checking against garbage pointers:
  if (rank > 7) {
    sprintf(msgbuf, "invalid rank, %d\n", this->rank);
    printf("%s", msgbuf);
    ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    return ESMC_RC_OBJ_BAD;
  }

  for (i=0; i<this->rank; i++) {
    sprintf(msgbuf,"dim[%d] = %d  ", i, this->counts[i]);
    printf("%s", msgbuf);
  }
  sprintf(msgbuf,"\n");
  printf("%s", msgbuf);
  
  // TODO: make this look at one of the option letters to see if user
  //   wants data printed.

  switch (this->typekind) {
    case ESMC_TYPEKIND_R4:
      switch (this->rank) {
        case 1:
          sprintf(msgbuf,"  Real, *4, Dim 1, Data values:\n");
          printf("%s", msgbuf);
          imax = this->counts[0];
          tcount = imax;
          for (i=0; i<tcount; i++) {
              if (!opt_byline) 
                  sprintf(msgbuf,"(%2d) =  %lg\n", lbound[0]+i,
                    *((ESMC_R4 *)(this->base_addr) + i));
              else
                  sprintf(msgbuf,"%lg ", *((ESMC_R4 *)(this->base_addr) + i));
              printf("%s", msgbuf);
              if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                 sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                 printf("%s", msgbuf);
                 i = tcount - 11;
              }
          }
          if (opt_byline) {
              sprintf(msgbuf,"\n");
              printf("%s", msgbuf);
          }
          break;
        case 2:
          sprintf(msgbuf,"  Real, *4, Dim 2, Data values:\n");
          printf("%s", msgbuf);
          imax = this->counts[0];
          jmax = this->counts[1];
          tcount = imax * jmax;
          rcount = 0;
          for (j=0; j<jmax; j++) {
              if (opt_byline) {
                  sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                  printf("%s", msgbuf);
              }
              for (i=0; i<imax; i++) {
                  if (!opt_byline) 
                      sprintf(msgbuf,"(%2d,%2d) =  %lg\n", 
                        lbound[0]+i, lbound[1]+j, 
                        *((ESMC_R4 *)(this->base_addr) + i + j*imax) );
                  else 
                      sprintf(msgbuf,"%lg ",  
                             *((ESMC_R4 *)(this->base_addr) + i + j*imax) );
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     j = (tcount-11) / imax;
                     i = (tcount-11) % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              }
          }
          break;
        case 3:
          sprintf(msgbuf,"  Real, *4, Dim 3, Data values:\n");
          printf("%s", msgbuf);
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
                  printf("%s", msgbuf);
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
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     int krem;
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     k = (tcount-11) / (imax*jmax);
                     krem = (tcount-11) % (imax*jmax);
                     j = krem / imax;
                     i = krem % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              }
            }
          }
          break;
        default:
          sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
          printf("%s", msgbuf);
          break;    
      }
      break;
    case ESMC_TYPEKIND_R8:
      switch (this->rank) {
        case 1:
          sprintf(msgbuf,"  Real, *8, Dim 1, Data values:\n");
          printf("%s", msgbuf);
          imax = this->counts[0];
          tcount = imax;
          for (i=0; i<tcount; i++) {
              if (!opt_byline)
                  sprintf(msgbuf,"(%2d) =  %lg\n", lbound[0]+i,
                    *((ESMC_R8 *)(this->base_addr) + i));
              else
                  sprintf(msgbuf,"%lg ", *((ESMC_R8 *)(this->base_addr) + i));
              printf("%s", msgbuf);
              if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                 sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                 printf("%s", msgbuf);
                 i = tcount - 11;
              }
          }
          if (opt_byline) {
              sprintf(msgbuf,"\n");
              printf("%s", msgbuf);
          }
          break;
        case 2:
          sprintf(msgbuf,"  Real, *8, Dim 2, Data values:\n");
          printf("%s", msgbuf);
          imax = this->counts[0];
          jmax = this->counts[1];
          tcount = imax * jmax;
          rcount = 0;
          for (j=0; j<jmax; j++) {
              if (opt_byline) {
                  sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                  printf("%s", msgbuf);
              } 
              for (i=0; i<imax; i++) {
                  if (!opt_byline)
                      sprintf(msgbuf,"(%2d,%2d) =  %lg\n", 
                        lbound[0]+i, lbound[1]+j, 
                        *((ESMC_R8 *)(this->base_addr) + i + j*imax) );
                  else
                      sprintf(msgbuf,"%lg ",  
                             *((ESMC_R8 *)(this->base_addr) + i + j*imax) );
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     j = (tcount-11) / imax;
                     i = (tcount-11) % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              }
          }
          break;
        case 3:
          sprintf(msgbuf,"  Real, *8, Dim 3, Data values:\n");
          printf("%s", msgbuf);
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
                  printf("%s", msgbuf);
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
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     int krem;
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     k = (tcount-11) / (imax*jmax);
                     krem = (tcount-11) % (imax*jmax);
                     j = krem / imax;
                     i = krem % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              }
            }
          }
          break;
        default:
          sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
          printf("%s", msgbuf);
          break;    
      }
      break;
    case ESMC_TYPEKIND_I4:
      switch (this->rank) {
        case 1:
          imax = this->counts[0];
          tcount = imax;
          sprintf(msgbuf,"  Integer, *4, Dim 1, Data values:\n");
          printf("%s", msgbuf);
          for (i=0; i<imax; i++) {
              if (!opt_byline)
                  sprintf(msgbuf,"(%2d) =  %d\n", lbound[0]+i, 
                         *((int *)(this->base_addr) + i));
              else
                  sprintf(msgbuf,"%d ",
                         *((int *)(this->base_addr) + i));
              printf("%s", msgbuf);
              if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                 sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                 printf("%s", msgbuf);
                 i = tcount - 11;
              }
          }
          if (opt_byline) {
              sprintf(msgbuf,"\n");
              printf("%s", msgbuf);
          }
          break;
        case 2:
          sprintf(msgbuf,"  Integer, *4, Dim 2, Data values:\n");
          printf("%s", msgbuf);
          imax = this->counts[0];
          jmax = this->counts[1];
          tcount = imax * jmax;
          rcount = 0; 
          for (j=0; j<jmax; j++) {
              if (opt_byline) {
                  sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                  printf("%s", msgbuf);
              }
              for (i=0; i<imax; i++) {
                  if (!opt_byline)
                      sprintf(msgbuf,"(%2d,%2d) =  %d\n", 
                        lbound[0]+i, lbound[1]+j, 
                        *((int *)(this->base_addr) + i + j*imax) );
                  else
                      sprintf(msgbuf,"%d ", 
                           *((int *)(this->base_addr) + i + j*imax) );
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     j = (tcount-11) / imax;
                     i = (tcount-11) % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              } 
          }
          break;
        case 3:
          sprintf(msgbuf,"  Integer, *4, Dim 3, Data values:\n");
          printf("%s", msgbuf);
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
                  printf("%s", msgbuf);
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
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     int krem;
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     k = (tcount-11) / (imax*jmax);
                     krem = (tcount-11) % (imax*jmax);
                     j = krem / imax;
                     i = krem % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              } 
            }
          }
          break;
        default:
          sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
          printf("%s", msgbuf);
          break;    
      }
      break;
    case ESMC_TYPEKIND_I8:
      switch (this->rank) {
        case 1:
          imax = this->counts[0];
          tcount = imax;
          sprintf(msgbuf,"  Integer, *8, Dim 1, Data values:\n");
          printf("%s", msgbuf);
          for (i=0; i<imax; i++) {
              if (!opt_byline)
                  sprintf(msgbuf,"(%2d) =  %lld\n", lbound[0]+i, 
                         *((ESMC_I8 *)(this->base_addr) + i));
              else
                  sprintf(msgbuf,"%lld ",
                         *((ESMC_I8 *)(this->base_addr) + i));
              printf("%s", msgbuf);
              if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                 sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                 printf("%s", msgbuf);
                 i = tcount - 11;
              }
          }
          if (opt_byline) {
              sprintf(msgbuf,"\n");
              printf("%s", msgbuf);
          } 
          break;
        case 2:
          sprintf(msgbuf,"  Integer, *8, Dim 2, Data values:\n");
          printf("%s", msgbuf);
          imax = this->counts[0];
          jmax = this->counts[1];
          tcount = imax * jmax;
          rcount = 0; 
          for (j=0; j<jmax; j++) {
              if (opt_byline) {
                  sprintf(msgbuf,"(*,%2d) = ", lbound[1]+j);
                  printf("%s", msgbuf);
              }
              for (i=0; i<imax; i++) {
                  if (!opt_byline)
                      sprintf(msgbuf,"(%2d,%2d) =  %lld\n", 
                        lbound[0]+i, lbound[1]+j, 
                        *((ESMC_I8 *)(this->base_addr) + i + j*imax) );
                  else
                      sprintf(msgbuf,"%lld ", 
                           *((ESMC_I8 *)(this->base_addr) + i + j*imax) );
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     j = (tcount-11) / imax;
                     i = (tcount-11) % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              } 
          }
          break;
        case 3:
          sprintf(msgbuf,"  Integer, *8, Dim 3, Data values:\n");
          printf("%s", msgbuf);
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
                    printf("%s", msgbuf);
              }
              for (i=0; i<imax; i++) {
                  if (!opt_byline)
                      sprintf(msgbuf,"(%2d,%2d,%2d) =  %lld\n", 
                             lbound[0]+i, lbound[1]+j, lbound[2]+k,
                             *((ESMC_I8 *)(this->base_addr) + 
                             i + j*imax + k*jmax*imax));
                  else
                      sprintf(msgbuf,"%lld ", 
                             *((ESMC_I8 *)(this->base_addr) + 
                             i + j*imax + k*jmax*imax));
                  printf("%s", msgbuf);
                  rcount++;
                  if (!opt_all && (tcount > 22) && (rcount==10)) {
                     int krem;
                     sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                     printf("%s", msgbuf);
                     k = (tcount-11) / (imax*jmax);
                     krem = (tcount-11) % (imax*jmax);
                     j = krem / imax;
                     i = krem % imax;
                  }
              }
              if (opt_byline) {
                  sprintf(msgbuf,"\n");
                  printf("%s", msgbuf);
              } 
            }
          }
          break;
        default:
          sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
          printf("%s", msgbuf);
          break;    
      }
      break;
      default:
        return rc;  // bail out
    }
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::write()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::write - write contents of a LocalArray
//
// !INTERFACE:
int LocalArray::write(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  const char *options,              // in - write options
  const char *filename)const{       // in - file name
//
// !DESCRIPTION:
//  Write the contents of an {\tt ESMCI::LocalArray} to disk.
//
//EOPI
//-----------------------------------------------------------------------------
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
          ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN, msgbuf,
            ESMC_CONTEXT, &rc);
          return rc;
      }
  }

  fprintf(ffile, "ArrayWrite: Array at address 0x%08Lx:  ", 
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
              fprintf(ffile, "%lld\n", *((ESMC_I8 *)(this->base_addr) + i));
          }
          break;
        case 2:
          imax = this->counts[0];
          jmax = this->counts[1];
          tcount = imax * jmax;
          rcount = 0; 
          for (j=0; j<jmax; j++) {
              for (i=0; i<imax; i++) {
                  fprintf(ffile, "%lld ",
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
                  fprintf(ffile, "%lld ", 
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
      default:
        return rc;  // bail out
  }

  fclose(ffile);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::validate()"
//BOPI
// !IROUTINE:  ESMCI::LocalArray::validate - internal consistency check
//
// !INTERFACE:
int LocalArray::validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  const char *options)const{    // in - validate options
//
// !DESCRIPTION:
//  Validates that an {\tt ESMCI::LocalArray} is internally consistent.
//  Returns error code if problems are found.
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocalArray::tkrPtrCopy()"
//BOPI
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
  ESMC_TypeKind_Flag typekind,
  int rank){
//
// !DESCRIPTION:
//      
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  switch (typekind){
#ifndef ESMF_NO_INTEGER_1_BYTE
    case ESMC_TYPEKIND_I1:
    switch (rank){
      case 1:
      FTN_X(f_esmf_fortrantkrptrcopy1di1)(dst, src);
      break;
      case 2:
      FTN_X(f_esmf_fortrantkrptrcopy2di1)(dst, src);
      break;
      case 3:
      FTN_X(f_esmf_fortrantkrptrcopy3di1)(dst, src);
      break;
      case 4:
      FTN_X(f_esmf_fortrantkrptrcopy4di1)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN_X(f_esmf_fortrantkrptrcopy5di1)(dst, src);
      break;
      case 6:
      FTN_X(f_esmf_fortrantkrptrcopy6di1)(dst, src);
      break;
      case 7:
      FTN_X(f_esmf_fortrantkrptrcopy7di1)(dst, src);
      break;
#endif
      default:
        return rc;  // bail out
    }
    break;
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
    case ESMC_TYPEKIND_I2:
    switch (rank){
      case 1:
      FTN_X(f_esmf_fortrantkrptrcopy1di2)(dst, src);
      break;
      case 2:
      FTN_X(f_esmf_fortrantkrptrcopy2di2)(dst, src);
      break;
      case 3:
      FTN_X(f_esmf_fortrantkrptrcopy3di2)(dst, src);
      break;
      case 4:
      FTN_X(f_esmf_fortrantkrptrcopy4di2)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN_X(f_esmf_fortrantkrptrcopy5di2)(dst, src);
      break;
      case 6:
      FTN_X(f_esmf_fortrantkrptrcopy6di2)(dst, src);
      break;
      case 7:
      FTN_X(f_esmf_fortrantkrptrcopy7di2)(dst, src);
      break;
#endif
      default:
        return rc;  // bail out
    }
    break;
#endif
    case ESMC_TYPEKIND_I4:
    switch (rank){
      case 1:
      FTN_X(f_esmf_fortrantkrptrcopy1di4)(dst, src);
      break;
      case 2:
      FTN_X(f_esmf_fortrantkrptrcopy2di4)(dst, src);
      break;
      case 3:
      FTN_X(f_esmf_fortrantkrptrcopy3di4)(dst, src);
      break;
      case 4:
      FTN_X(f_esmf_fortrantkrptrcopy4di4)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN_X(f_esmf_fortrantkrptrcopy5di4)(dst, src);
      break;
      case 6:
      FTN_X(f_esmf_fortrantkrptrcopy6di4)(dst, src);
      break;
      case 7:
      FTN_X(f_esmf_fortrantkrptrcopy7di4)(dst, src);
      break;
#endif
      default:
        return rc;  // bail out
    }
    break;
    case ESMC_TYPEKIND_I8:
    switch (rank){
      case 1:
      FTN_X(f_esmf_fortrantkrptrcopy1di8)(dst, src);
      break;
      case 2:
      FTN_X(f_esmf_fortrantkrptrcopy2di8)(dst, src);
      break;
      case 3:
      FTN_X(f_esmf_fortrantkrptrcopy3di8)(dst, src);
      break;
      case 4:
      FTN_X(f_esmf_fortrantkrptrcopy4di8)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN_X(f_esmf_fortrantkrptrcopy5di8)(dst, src);
      break;
      case 6:
      FTN_X(f_esmf_fortrantkrptrcopy6di8)(dst, src);
      break;
      case 7:
      FTN_X(f_esmf_fortrantkrptrcopy7di8)(dst, src);
      break;
#endif
      default:
        return rc;  // bail out
    }
    break;
    case ESMC_TYPEKIND_R4:
    switch (rank){
      case 1:
      FTN_X(f_esmf_fortrantkrptrcopy1dr4)(dst, src);
      break;
      case 2:
      FTN_X(f_esmf_fortrantkrptrcopy2dr4)(dst, src);
      break;
      case 3:
      FTN_X(f_esmf_fortrantkrptrcopy3dr4)(dst, src);
      break;
      case 4:
      FTN_X(f_esmf_fortrantkrptrcopy4dr4)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN_X(f_esmf_fortrantkrptrcopy5dr4)(dst, src);
      break;
      case 6:
      FTN_X(f_esmf_fortrantkrptrcopy6dr4)(dst, src);
      break;
      case 7:
      FTN_X(f_esmf_fortrantkrptrcopy7dr4)(dst, src);
      break;
#endif
      default:
        return rc;  // bail out
    }
    break;
    case ESMC_TYPEKIND_R8:
    switch (rank){
      case 1:
      FTN_X(f_esmf_fortrantkrptrcopy1dr8)(dst, src);
      break;
      case 2:
      FTN_X(f_esmf_fortrantkrptrcopy2dr8)(dst, src);
      break;
      case 3:
      FTN_X(f_esmf_fortrantkrptrcopy3dr8)(dst, src);
      break;
      case 4:
      FTN_X(f_esmf_fortrantkrptrcopy4dr8)(dst, src);
      break;
#ifndef ESMF_NO_GREATER_THAN_4D
      case 5:
      FTN_X(f_esmf_fortrantkrptrcopy5dr8)(dst, src);
      break;
      case 6:
      FTN_X(f_esmf_fortrantkrptrcopy6dr8)(dst, src);
      break;
      case 7:
      FTN_X(f_esmf_fortrantkrptrcopy7dr8)(dst, src);
      break;
#endif
      default:
        return rc;  // bail out
    }
    break;
    default:
      return rc;  // bail out
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------
 
} // namespace ESMCI
