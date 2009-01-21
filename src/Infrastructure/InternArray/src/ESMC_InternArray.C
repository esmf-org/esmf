// $Id: ESMC_InternArray.C,v 1.11.2.3 2009/01/21 21:25:22 cdeluca Exp $
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Array method implementation (body) file

#define ESMF_FILENAME "ESMC_InternArray.C"
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ InternArray methods declared
// in the companion file ESMC_InternArray.h.  
//
// The {\tt ESMF\_InternArray} object allows C++ to emulate the richer
// Fortran language InternArray operations.  It allows strided access, 
// subsetting operations, known dimension sizes, and typed access 
// to arrays instead of just a starting address to a block of memory.  
//
//-----------------------------------------------------------------------------
//

// for printf
#include <stdio.h>
#include <string.h>
#include <assert.h>
// associated class definition file
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"
#include "ESMC_InternArray.h"
#include "ESMCI_DELayout.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_InternArray.C,v 1.11.2.3 2009/01/21 21:25:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the InternArray routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InternArrayCreate"
//BOP
// !IROUTINE:  ESMC_InternArrayCreate - Create a new Array
//
// !INTERFACE:
      ESMC_InternArray *ESMC_InternArrayCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Array
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    ESMC_TypeKind dk,          // short/long, etc
    int *icounts,              // number of items in each dim
    void *base,                // if non-null, this is already allocated memory
    ESMC_DataCopy docopy,      // if base != NULL, copy data?
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This routine is the C++ entry point for creating an {\tt ESMF\_InternArray}
//      object.  Unlike natural C++ arrays which can be as simple as the
//      base address pointer and the number of bytes necessary to move to
//      the next item, {\tt ESMF\_InternArray}s are richer in the associated metadata
//      which allows them to behave more like Fortran arrays.  They store
//      the size of each dimension, allow non-contiguous strides per
//      dimension, and allow whole-array or subsectional operations.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

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
//   The return from this routine is a pointer to the new InternArray data.
//
     ESMC_InternArray *a = new ESMC_InternArray;
     int status;

     if (rc != NULL)
         *rc = ESMC_RC_NOT_IMPL;

     status = a->ESMC_ArrayConstruct(rank, dk, icounts, base, 
                                     ESMC_FROM_CPLUSPLUS,
                                     NULL, ESMC_ARRAY_DO_ALLOCATE, 
                                     docopy, ESMF_TRUE, 
                                     NULL, NULL, NULL, 0); 
     
     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_InternArrayCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InternArrayCreate"
//BOP
// !IROUTINE:  ESMC_InternArrayCreate - Create a new InternArray
//
// !INTERFACE:
      ESMC_InternArray *ESMC_InternArrayCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_InternArray
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    ESMC_TypeKind dk,          // short/long, etc
    int *icounts,              // number of items in each dim
    void *base,                // if non-null, this is already allocated memory
    ESMC_DataCopy docopy,      // if base != NULL, copy data?
    int halo_width,            // applies to all edges, more dirs to be added
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      Same as above but with halo widths specified.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

     ESMC_InternArray *a = new ESMC_InternArray;
     int status;

     // Initialize return code; assume routine not implemented
     if (rc != NULL) *rc = ESMF_RC_NOT_IMPL;


     status = a->ESMC_ArrayConstruct(rank, dk, icounts, base, 
                                     ESMC_FROM_CPLUSPLUS,
                                     NULL, ESMC_ARRAY_DO_ALLOCATE, 
                                     docopy, ESMF_TRUE, 
                                     NULL, NULL, NULL, halo_width); 
     
     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_InternArrayCreate

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InternArrayDestroy"
//BOP
// !IROUTINE:  ESMC_InternArrayDestroy - free a InternArray created with Create
//
// !INTERFACE:
      int ESMC_InternArrayDestroy(ESMC_InternArray *array) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which destroys a InternArray object previously allocated
//      via an {\tt ESMC\_InternArrayCreate} routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

     // Initialize return code; assume routine not implemented
     int rc = ESMF_RC_NOT_IMPL;

    array->ESMC_ArrayDestruct();

    delete array;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_InternArrayDestroy

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InternArrayCreateNoData"
//BOPI
// !IROUTINE:  ESMC_InternArrayCreateNoData - internal routine for fortran use
//
// !INTERFACE:
      ESMC_InternArray *ESMC_InternArrayCreateNoData(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_InternArray
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    ESMC_TypeKind dk,          // short/long, etc
    ESMC_ArrayOrigin oflag,    // caller is fortran or C++?
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This version of Create is only intended for internal use by
//      the {\tt ESMF\_InternArrayCreate} fortran routine.  It creates a partially
//      constructed array, then depends on the caller to come back and
//      complete the array with the {\tt ESMF\_ArraySetInfo} call.  
//      (It is broken up this way to try to minimize the amount of
//      macro-generated code needed in the {\tt ESMF\_InternArray.F90} source file.)
//
//EOPI

     ESMC_InternArray *a = new ESMC_InternArray;
     int status;

     // Initialize return code; assume routine not implemented
     if (rc != NULL) *rc = ESMF_RC_NOT_IMPL;

     status = a->ESMC_ArrayConstruct(rank, dk, NULL, NULL, oflag,
                            NULL, ESMC_ARRAY_NO_ALLOCATE, 
                            ESMC_DATA_NONE, ESMF_FALSE, 
                            NULL, NULL, NULL, 0);

     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_InternArrayCreateNoData

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InternArrayCreate_F"
//BOP
// !IROUTINE:  ESMC_InternArrayCreate_F - internal routine for fortran use
//
// !INTERFACE:
      ESMC_InternArray *ESMC_InternArrayCreate_F(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_InternArray
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    ESMC_TypeKind dk,          // short/long, etc
    int *icounts,              // counts along each dimension
    struct c_F90ptr *f90ptr,   // opaque type which fortran uses (dope v)
    void *base,                // real start of memory 
    ESMC_DataCopy docopy,      // if base is null and this is Copy, alloc here
    int *lbounds,              // lower index number per dim
    int *ubounds,              // upper index number per dim
    int *offsets,              // number of bytes to start of data/dim
    int halo_widths,           // width of halo region
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This version of Create is only intended for internal use by
//      the {\tt ESMF\_InternArrayCreate} fortran routine.  This routine works in a
//      similar manner as the regular {\tt ESMC\_InternArrayCreate} routine, but the
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
//   The return from this routine is a pointer to the new InternArray data.
//
     ESMC_InternArray *a = new ESMC_InternArray;
     int status;

     // Initialize return code; assume routine not implemented
     if (rc != NULL) *rc = ESMF_RC_NOT_IMPL;

     if (base == NULL) 
         status = a->ESMC_ArrayConstruct(rank, dk, icounts, base, 
                                     ESMC_FROM_FORTRAN, f90ptr, 
                                     ESMC_ARRAY_DO_ALLOCATE,
                                     ESMC_DATA_NONE, ESMF_TRUE, 
                                     lbounds, ubounds,  
                                     offsets, halo_widths); 
     else
         status = a->ESMC_ArrayConstruct(rank, dk, icounts, base, 
                                     ESMC_FROM_FORTRAN, f90ptr, 
                                     ESMC_ARRAY_NO_ALLOCATE, 
                                     docopy, ESMF_FALSE, 
                                     lbounds, ubounds,  
                                     offsets, halo_widths); 

     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_InternArrayCreate_F

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayConstruct"
//BOP
// !IROUTINE:  ESMC_ArrayConstruct - fill in an already allocated InternArray
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayConstruct(
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
    int *lbounds,              // lower index number per dim
    int *ubounds,              // upper index number per dim
    int *offsets,              // offset in bytes to start of each dim
    int halo_width) {          // halo width on all edges
//
// !DESCRIPTION:
//   ESMF routine which fills in the contents of an already
//   allocated {\tt ESMF\_InternArray} object.  May need to do additional allocations
//   as needed.  Must call the corresponding {\tt ESMC\_ArrayDestruct}
//   routine to free the additional memory.  Intended for internal
//   ESMF use only; end-users use {\tt ESMC\_InternArrayCreate}, which calls
//   {\tt ESMC\_ArrayConstruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  
    int i, status, rc;
    ESMC_InternArray *aptr;
    int alloc_stride;
    int C, twidth;

     // Initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL;

    rank = irank;
    kind = dk;

    base_addr = base;
    alloc_stride = 1;
    for (i=0; i<rank; i++) {
        counts[i]   = icounts ? icounts[i] : 1;        
        lbound[i]   = lbounds ? lbounds[i] : 1;
        ubound[i]   = ubounds ? ubounds[i] : counts[i];
        offset[i]    = offsets ? offsets[i] : 0;
        bytestride[i] = alloc_stride;
        alloc_stride *= counts[i];

        // TODO: the allocation space has no interfaces to set it yet,
        // and no code knows about it.  but it has been requested by users.
        // for now set it to 0 so alloc == total always, and slowly start 
        // adding support for it.
        awidth[i][0] = 0;
        awidth[i][1] = 0;

        // TODO: this needs a way to say either which axes are associated
        // with the interngrid, since those are the only ones which need halo space,
        // or require halo widths come in for all data axes and they have
        // already been adjusted to match which are interngrid and non-interngrid axes.
        hwidth[i][0] = halo_width;
        hwidth[i][1] = halo_width;
  
        // TODO: decide what counts really means.  right now, it means
        // allocation space, so halo is subtracted from it to give comp area.
 
        // there is an important change here.   the origin, both for local
        // and for global, is now the min of the computational area.  if there
        // are halo widths, those will be negative on the lower side, above
        // counts on the upper side.  if there are allocation widths, those 
        // min and maxs will be even more negative and positive.

#if 0
        // these are right if counts comes in as the computation area, but
        // then the other things (lbound, ubound, stride, etc) are NOT right.
        ESMC_AxisIndexSet(ai_alloc+i, -awidth[i][0]-hwidth[i][0], 
                          counts[i]+hwidth[i][1]+awidth[i][1]-1);

        ESMC_AxisIndexSet(ai_total+i, -hwidth[i][0], counts[i]+hwidth[i][1]-1);

        ESMC_AxisIndexSet(ai_comp+i, 0, counts[i]-1);

        ESMC_AxisIndexSet(ai_excl+i, hwidth[i][0], counts[i]-hwidth[i][1]-1);
#else
        // use temp vars to try to make this more readable. 
        // C = counts-1, twidth = alloc width plus halo width

        C = counts[i]-1;
        twidth = awidth[i][0] + hwidth[i][0];   // need min sides only

        // allocation/memory space
        ESMC_AxisIndexSet(ai_alloc+i, -twidth, C - twidth);

        // total data area
        ESMC_AxisIndexSet(ai_total+i, awidth[i][0] - twidth, 
                                      C - awidth[i][1] - twidth);

        // computational area
        ESMC_AxisIndexSet(ai_comp+i, awidth[i][0] + hwidth[i][0] - twidth, 
                                     C - awidth[i][1] - hwidth[i][1] - twidth);

        // exclusive area
        ESMC_AxisIndexSet(ai_excl+i, awidth[i][0] + 2*hwidth[i][0] - twidth, 
                                    C - awidth[i][1] - 2*hwidth[i][1] - twidth);
#endif
    }

    // fill out rest of space to null values.
    for (i=rank; i<ESMF_MAXDIM; i++) {
        counts[i] = 1;
        lbound[i] = 1;
        ubound[i] = 1;
        offset[i] = 0;
        bytestride[i] = alloc_stride;
        awidth[i][0] = 0;
        awidth[i][1] = 0;
        hwidth[i][0] = 0;
        hwidth[i][1] = 0;
        ESMC_AxisIndexSet(ai_alloc+i, 0, 0);
        ESMC_AxisIndexSet(ai_total+i, 0, 0);
        ESMC_AxisIndexSet(ai_comp+i, 0, 0);
        ESMC_AxisIndexSet(ai_excl+i, 0, 0);
    }

    origin = oflag;
    needs_dealloc = dflag;

    if (f90ptr != NULL)
        ESMC_ArraySetF90Ptr(f90ptr);
 
    if (aflag == ESMC_ARRAY_DO_ALLOCATE) {
            aptr = this;
            FTN(f_esmf_arrayf90allocate)(&aptr, &rank, &kind, 
                                         counts, lbound, ubound,   
                                         &halo_width, &status);
    } 

    
    // TODO: call LocalArray constructor first, then add halo info
    ESMC_BaseSetName(NULL, "InternArray");

    // TODO: memcpy from base to base_addr, proper number of bytes?
    //  if docopy flag is set.

    return ESMF_SUCCESS;



 } // end ESMC_ArrayConstruct

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayDestruct"
//BOP
// !IROUTINE:  ESMC_ArrayDestruct - release resources associated w/a InternArray
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMF\_ArrayConstruct}, does any additional cleanup before the
//      original {\tt ESMC\_InternArray} object is freed.  Intended for internal ESMF
//      use only; end-users use {\tt ESMC\_InternArrayDestroy}, which calls
//      {\tt ESMC\_ArrayDestruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    ESMC_InternArray *aptr = this;

    // check origin and alloc flag, and call dealloc routine if needed 
    if (needs_dealloc != ESMF_TRUE)
        return ESMF_SUCCESS;

    // if there is an F90 dope vector, we have to call back into fortran
    // to deallocate this.   if we want to support a C++ only library,
    // then this code needs to be calling malloc/free or new/delete and
    // needs conditional code to pick the fortran or C++ mem mgt system.

    FTN(f_esmf_arrayf90deallocate)(&aptr, &rank, &kind, &rc);

    return rc;

 } // end ESMC_ArrayDestruct


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGet"
//BOP
// !IROUTINE:  ESMC_ArrayGet<Value> - get <Value> for a Array
//
// !INTERFACE:
      //int ESMC_InternArray::ESMC_ArrayGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //<value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of {\tt ESMC\_InternArray} member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 //} // end ESMC_ArrayGet<Value>

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySet"
//BOP
// !IROUTINE:  ESMC_ArraySet<Value> - set <Value> for a InternArray
//
// !INTERFACE:
      //int ESMC_InternArray::ESMC_ArraySet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      //const <value type> *value) {     // in - value
//
// !DESCRIPTION:
//     Sets the value of {\tt ESMC\_InternArray} member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    //int rc = ESMC_RC_NOT_IMPL;

    //return rc;

 //} // end ESMC_ArraySet<Value>

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySetInfo"
//BOP
// !IROUTINE:  ESMC_ArraySetInfo - Set the most common F90 needs
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArraySetInfo(
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
    ESMC_Logical contig,      // in - is memory chunk contiguous?
    ESMC_Logical dealloc,     // in - do we need to deallocate at delete?
    int halo_width) {         // in - halo widths for setting AIs
//
// !DESCRIPTION:
//     Sets a list of values associated with an already created pointer.
//     This particular set was chosen to mesh well with creation on the
//     F90 side.  Other combinations will probably be useful.
//
//EOP
// !REQUIREMENTS:  

     // Initialize return code; assume routine not implemented
     int rc = ESMF_RC_NOT_IMPL;

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
    int alloc_stride;
    int C, twidth;
  
    // TODO: there is one compiler; linux, maybe intel maybe g95, which
    // seems to have different sizes for even and odd ranks.  this code
    // cannot handle this strangeness without more code.

    // note - starts at 1; base includes rank 1 size
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
   //fprintf(stderr, "setting f90 ptr from %lx to %lx, %d bytes for rank %d\n", 
   //                (ESMC_I8)fptr, (ESMC_I8)(&this->f90dopev), bytes, rank);

    memcpy((void *)(&this->f90dopev), (void *)fptr, bytes);

    base_addr = base;
    alloc_stride = 1;
    for (i=0; i<rank; i++) {
        counts[i] = icounts ? icounts[i] : 0;
        offset[i] = offsets ? offsets[i] : 0;
        lbound[i] = lbounds ? lbounds[i] : 1;
        ubound[i] = ubounds ? ubounds[i] : counts[i];
        bytestride[i] = alloc_stride;
        alloc_stride *= counts[i];

        // TODO: the allocation space has no interfaces to set it yet,
        // and no code knows about it.  but it has been requested by users.
        // for now set it to 0 so alloc == total always, and slowly start 
        // adding support for it.
        awidth[i][0] = 0;
        awidth[i][1] = 0;

        // TODO: this needs a way to say either which axes are associated
        // with the interngrid, since those are the only ones which need halo space,
        // or require halo widths come in for all data axes and they have
        // already been adjusted to match which are interngrid and non-interngrid axes.
        hwidth[i][0] = halo_width;
        hwidth[i][1] = halo_width;
  
        // TODO: decide what counts really means.  right now, it means
        // allocation space, so halo is subtracted from it to give comp area.
 
        // there is an important change here.   the origin, both for local
        // and for global, is now the min of the computational area.  if there
        // are halo widths, those will be negative on the lower side, above
        // counts on the upper side.  if there are allocation widths, those 
        // min and maxs will be even more negative and positive.

#if 0
        // these are right if counts comes in as the computation area, but
        // then the other things (lbound, ubound, stride, etc) are NOT right.
        ESMC_AxisIndexSet(ai_alloc+i, -awidth[i][0]-hwidth[i][0], 
                          counts[i]+hwidth[i][1]+awidth[i][1]-1);

        ESMC_AxisIndexSet(ai_total+i, -hwidth[i][0], counts[i]+hwidth[i][1]-1);

        ESMC_AxisIndexSet(ai_comp+i, 0, counts[i]-1);

        ESMC_AxisIndexSet(ai_excl+i, hwidth[i][0], counts[i]-hwidth[i][1]-1);
#else
        // use temp vars to try to make this more readable. 
        // C = counts-1, twidth = alloc width plus halo width

        C = counts[i]-1;
        twidth = awidth[i][0] + hwidth[i][0];   // need min sides only

        // allocation/memory space
        ESMC_AxisIndexSet(ai_alloc+i, -twidth, C - twidth);

        // total data area
        ESMC_AxisIndexSet(ai_total+i, awidth[i][0] - twidth, 
                                      C - awidth[i][1] - twidth);

        // computational area
        ESMC_AxisIndexSet(ai_comp+i, awidth[i][0] + hwidth[i][0] - twidth, 
                                     C - awidth[i][1] - hwidth[i][1] - twidth);

        // exclusive area
        ESMC_AxisIndexSet(ai_excl+i, awidth[i][0] + 2*hwidth[i][0] - twidth, 
                                    C - awidth[i][1] - 2*hwidth[i][1] - twidth);
#endif
    }

    // fill out rest of space to null values.
    for (i=rank; i<ESMF_MAXDIM; i++) {
        counts[i] = 1;
        lbound[i] = 1;
        ubound[i] = 1;
        offset[i] = 0;
        bytestride[i] = alloc_stride;
        awidth[i][0] = 0;
        awidth[i][1] = 0;
        hwidth[i][0] = 0;
        hwidth[i][1] = 0;
        ESMC_AxisIndexSet(ai_alloc+i, 0, 0);
        ESMC_AxisIndexSet(ai_total+i, 0, 0);
        ESMC_AxisIndexSet(ai_comp+i, 0, 0);
        ESMC_AxisIndexSet(ai_excl+i, 0, 0);
    }
      
    iscontig = contig;
    needs_dealloc = dealloc;

    rc = ESMF_SUCCESS; 
    return rc;

 } // end ESMC_ArraySetInfo

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGetF90Ptr"
//BOP
// !IROUTINE:  ESMC_ArrayGetF90Ptr - get F90Ptr for a Array
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayGetF90Ptr(
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
// !REQUIREMENTS:  

     // Initialize return code; assume routine not implemented
     int rc = ESMF_RC_NOT_IMPL;

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
  
    // note - starts at 1; base includes rank 1 size
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    //fprintf(stderr, "getting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
    //                 (ESMC_I8)(&this->f90dopev), (ESMC_I8)p, bytes, rank);

    memcpy((void *)p, (void *)(&this->f90dopev), bytes);

    rc = ESMF_SUCCESS; 
    return rc;

 } // end ESMC_ArrayGetF90Ptr

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySetF90Ptr"
//BOP
// !IROUTINE:  ESMC_ArraySetF90Ptr - set F90Ptr for a InternArray
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArraySetF90Ptr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const struct c_F90ptr *p) {     // in - f90 pointer block
//
// !DESCRIPTION:
//     Sets the {\tt ESMC\_InternArray} member F90ptr with the given value.
//
//EOP
// !REQUIREMENTS:  

     // Initialize return code; assume routine not implemented
     int rc = ESMF_RC_NOT_IMPL;

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
  
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    //fprintf(stderr, "setting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
    //                  (ESMC_I8)p,  (ESMC_I8)(&this->f90dopev), bytes, rank);

    memcpy((void *)(&this->f90dopev), (void *)p, bytes);

    rc = ESMF_SUCCESS; 
    return rc;

 } // end ESMC_ArraySetF90Ptr

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySetAxisIndex"
//BOP
// !IROUTINE:  ESMC_ArraySetAxisIndex - set annotation on InternArrays for local/global
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArraySetAxisIndex(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DomainType dt,              // in - domain type, total/comp/excl
      struct ESMC_AxisIndex *ai) {     // in - values to set
//
// !DESCRIPTION:
//     Sets the {\tt ESMC\_InternArray} member {\tt ESMC\_AxisIndex} with the given value.
//
//EOP
// !REQUIREMENTS:  

     int i, rc;

     // Initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL;

     switch(dt) {
       case ESMC_DOMAIN_ALLOCATED:
         for (i=0; i<this->rank; i++) 
             ai_alloc[i] = ai[i];
         break;

       case ESMC_DOMAIN_TOTAL:
         for (i=0; i<this->rank; i++) 
             ai_total[i] = ai[i];
         break;
     
       case ESMC_DOMAIN_COMPUTATIONAL:
         for (i=0; i<this->rank; i++) 
             ai_comp[i] = ai[i];
         break;

       case ESMC_DOMAIN_EXCLUSIVE:
         for (i=0; i<this->rank; i++) 
             ai_excl[i] = ai[i];
         break;

       default:
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                               "domain type", &rc);
         return(rc);
      }

      return ESMF_SUCCESS;

 } // end ESMC_ArraySetAxisIndex

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGetAxisIndex"
//BOP
// !IROUTINE:  ESMC_ArrayGetAxisIndex - get annotation on InternArrays for local/global
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayGetAxisIndex(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DomainType dt,                 // in - domain type, total/comp/excl
      struct ESMC_AxisIndex *ai) const {  // out - values to get
//
// !DESCRIPTION:
//     Gets the {\tt ESMC\_InternArray} member {\tt ESMC\_AxisIndex} with the given value.
//
//EOP
// !REQUIREMENTS:  

     int i, rc;

     // Initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL;

     switch(dt) {
       case ESMC_DOMAIN_ALLOCATED:
         for (i=0; i<this->rank; i++) 
             ai[i] = ai_alloc[i];
         break;
     
       case ESMC_DOMAIN_TOTAL:
         for (i=0; i<this->rank; i++) 
             ai[i] = ai_total[i];
         break;
     
       case ESMC_DOMAIN_COMPUTATIONAL:
         for (i=0; i<this->rank; i++) 
             ai[i] = ai_comp[i];
         break;

       case ESMC_DOMAIN_EXCLUSIVE:
         for (i=0; i<this->rank; i++) 
             ai[i] = ai_excl[i];
         break;

       case ESMC_DOMAIN_OLDTOTAL:
         for (i=0; i<this->rank; i++) {
             ai[i].min = 0;
             ai[i].max = counts[i]-1;
             ai[i].stride = ai_total[i].stride;
         }
         break;
     
       case ESMC_DOMAIN_OLDCOMPUTATIONAL:
         for (i=0; i<this->rank; i++) {
             ai[i].min = hwidth[i][0];
             ai[i].max = counts[i]-1-hwidth[i][1];
             ai[i].stride = ai_comp[i].stride;
         }
         break;

       case ESMC_DOMAIN_OLDEXCLUSIVE:
         for (i=0; i<this->rank; i++) {
             ai[i].min = 2*hwidth[i][0];
             ai[i].max = counts[i]-1-(2*hwidth[i][1]);
             ai[i].stride = ai_excl[i].stride;
         }
         break;

       default:
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                                               "domain type", &rc);
         return(rc);
    }

     rc = ESMF_SUCCESS;
     return rc;

 } // end ESMC_ArrayGetAxisIndex

//---------------------------------------------------------------------------
 // "All" in this routine means all DEs and all ranks, and it assumes
 // that halo widths are applied equally to all ranks, even non-interngrid ranks.
 // We are trying to fix this so halos are only defined for interngrid ranks.
 // So this needs to be defined in the InternArrayComm file, where it can access
 // the InternGrid, the DataMap, and the InternArray so it pulls the right things from
 // the right places.  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGetAllAxisIndices"
//BOPI
// !IROUTINE:  ESMC_ArrayGetAllAxisIndices - get all AIs for local/global
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayGetAllAxisIndices(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      struct ESMC_AxisIndex *global,       // in - associated global AIs
      int nDEs,                            // in -
      int rank,                            // in -
      struct ESMC_AxisIndex *total,        // out - total region
      struct ESMC_AxisIndex *comp,         // out - computational
      struct ESMC_AxisIndex *excl) const { // out - exclusive
//
// !DESCRIPTION:
//  Based on the interngrid and the halo widths of the {\tt ESMC\_InternArray} 
//  on the local DE, compute all requested AI lists for all {\tt DE}s
//  in the DELayout associated with the specified {\tt ESMF_InternGrid}.
//  This does assume that all {\tt ESMC\_InternArray}s associated with 
//  a interngrid have the same halo widths (but they can be different sizes
//  on different sides).
//
//EOPI

     int i, j, ij, count, rc;
     int halo_widths[ESMF_MAXDIM][2];

     // Initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL;

     // TODO: when widths are 2*Ndim, compute all of them.
     for (i=0; i<rank; i++) {
       halo_widths[i][0] = ai_comp[i].min  - ai_total[i].min;
       halo_widths[i][1] = ai_total[i].max - ai_comp[i].max;
     }

     for (j=0; j<rank; j++) {
       for (i=0; i<nDEs; i++) {
         ij = j*nDEs + i;
         count = global[ij].max - global[ij].min;

         // nsc - fixme.  please.  purty please.

         if (total) {
             total[ij].min = 0;
             total[ij].max = count + halo_widths[j][0] + halo_widths[j][1];
             total[ij].stride = count + halo_widths[j][0] + halo_widths[j][1] + 1;
         }

         if (comp) {
             comp[ij].min = halo_widths[j][0];
             comp[ij].max = count + halo_widths[j][0];
             comp[ij].stride = count + halo_widths[j][0] + halo_widths[j][1] + 1;
         }

         if (excl) {
             excl[i].min = halo_widths[j][0] + halo_widths[j][1];
             excl[i].max = count - halo_widths[j][0];
             excl[i].stride = count + halo_widths[j][0] + halo_widths[j][1] + 1;
         }
       }
     }

     rc = ESMF_SUCCESS;
     return rc;

 } // end ESMC_ArrayGetAllAxisIndices


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayValidate"
//BOP
// !IROUTINE:  ESMC_ArrayValidate - internal consistency check for a InternArray
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_InternArray} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_ArrayValidate


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayPrint"
//BOP
// !IROUTINE:  ESMC_ArrayPrint - print contents of a InternArray
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a {\tt ESMC\_InternArray}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
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

    sprintf(msgbuf,"ArrayPrint: InternArray at address 0x%08lx:\n", (ESMC_POINTER)this);
    printf(msgbuf);
      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);

    sprintf(msgbuf,"            rank = %d, kind = %d, ", 
                             this->rank, this->kind);
    printf(msgbuf);
      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    sprintf(msgbuf,"base_addr = 0x%08lx\n", (ESMC_POINTER)this->base_addr);
    printf(msgbuf);
      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    sprintf(msgbuf,"            ");
    printf(msgbuf);
      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    for (i=0; i<this->rank; i++) {
        sprintf(msgbuf,"dim[%d] = %d  ", i, this->counts[i]);
        printf(msgbuf); // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    }
    sprintf(msgbuf,"\n");
    printf(msgbuf);
      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    
    // TODO: make this look at one of the option letters to see if user
    //   wants data printed.

        switch (this->kind) {
          case ESMC_TYPEKIND_R4:
            switch (this->rank) {
              case 1:
                sprintf(msgbuf,"  Real, *4, Dim 1, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<tcount; i++) {
                    if (!opt_byline)
                        sprintf(msgbuf,"(%2d) =  %lg\n", i+lbound[0], 
                               *((ESMC_R4 *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%lg ", *((ESMC_R4 *)(this->base_addr) + i));
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                         // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                } 
                break;
              case 2:
                sprintf(msgbuf,"  Real, *4, Dim 2, Data values:\n");
                printf(msgbuf);
                // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0;
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", j+lbound[1]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d) =  %lg\n", 
                                     i+lbound[0], j+lbound[1], 
                                   *((ESMC_R4 *)(this->base_addr) + i + j*imax) );
                        else
                            sprintf(msgbuf,"%lg ",  
                                   *((ESMC_R4 *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                }
                break;
              case 3:
                sprintf(msgbuf,"  Real, *4, Dim 3, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ", 
                                          j+lbound[1], k+lbound[2]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %g\n", 
                                   i+lbound[0], j+lbound[1], k+lbound[2],
                                   *((ESMC_R4 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                             sprintf(msgbuf,"%g ", *((ESMC_R4 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                break;    
            }
            break;
          case ESMC_TYPEKIND_R8:
            switch (this->rank) {
              case 1:
                sprintf(msgbuf,"  Real, *8, Dim 1, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                tcount = imax;
                for (i=0; i<tcount; i++) {
                    if (!opt_byline)
                        sprintf(msgbuf,"(%2d) =  %lg\n", i+lbound[0], 
                                     *((ESMC_R8 *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%lg ", *((ESMC_R8 *)(this->base_addr) + i));
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                         // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                } 
                break;
              case 2:
                sprintf(msgbuf,"  Real, Dim 2, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0;
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", j+lbound[1]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d) =  %lg\n", 
                                            i+lbound[0], j+lbound[1], 
                                   *((ESMC_R8 *)(this->base_addr) + i + j*imax) );
                        else
                            sprintf(msgbuf,"%lg ",  
                                   *((ESMC_R8 *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                }
                break;
              case 3:
                sprintf(msgbuf,"  Real, Dim 3, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ", j+lbound[1], 
                                                         k+lbound[2]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %lg\n", 
                                   i+lbound[0], j+lbound[1], k+lbound[2],
                                   *((ESMC_R8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                             sprintf(msgbuf,"%lg ", *((ESMC_R8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle real rank %d yet\n", this->rank);
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
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
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        sprintf(msgbuf,"(%2d) =  %d\n", i+lbound[0], 
                               *((int *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%d ",
                               *((int *)(this->base_addr) + i));
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                         // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                } 
                break;
              case 2:
                sprintf(msgbuf,"  Integer, *4, Dim 2, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0; 
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", j+lbound[1]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d) =  %d\n", 
                                    i+lbound[0], j+lbound[1], 
                                 *((int *)(this->base_addr) + i + j*imax) );
                        else
                            sprintf(msgbuf,"%d ", 
                                 *((int *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                }
                break;
              case 3:
                sprintf(msgbuf,"  Integer, *4, Dim 3, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ", 
                                     j+lbound[1], k+lbound[2]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %d\n", 
                                   i+lbound[0], j+lbound[1], k+lbound[2],
                                   *((int *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                            sprintf(msgbuf,"%d ", 
                                   *((int *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
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
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        sprintf(msgbuf,"(%2d) =  %ld\n", i+lbound[0], 
                               *((ESMC_I8 *)(this->base_addr) + i));
                    else
                        sprintf(msgbuf,"%ld ",
                               *((ESMC_I8 *)(this->base_addr) + i));
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                       sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                       printf(msgbuf);
                         // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                       i = tcount - 11;
                    }
                }
                if (opt_byline) {
                    sprintf(msgbuf,"\n");
                    printf(msgbuf);
                      // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                } 
                break;
              case 2:
                sprintf(msgbuf,"  Integer, *8, Dim 2, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                tcount = imax * jmax;
                rcount = 0; 
                for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d) = ", j+lbound[1]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d) =  %ld\n", 
                                    i+lbound[0], j+lbound[1], 
                                 *((ESMC_I8 *)(this->base_addr) + i + j*imax) );
                        else
                            sprintf(msgbuf,"%ld ", 
                                 *((ESMC_I8 *)(this->base_addr) + i + j*imax) );
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           j = (tcount-11) / imax;
                           i = (tcount-11) % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                }
                break;
              case 3:
                sprintf(msgbuf,"  Integer, *8, Dim 3, Data values:\n");
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                tcount = imax * jmax * kmax;
                rcount = 0; 
                for (k=0; k<kmax; k++) {
                  for (j=0; j<jmax; j++) {
                    if (opt_byline) {
                        sprintf(msgbuf,"(*,%2d,%2d) = ", 
                                        j+lbound[1], k+lbound[2]);
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    }
                    for (i=0; i<imax; i++) {
                        if (!opt_byline)
                            sprintf(msgbuf,"(%2d,%2d,%2d) =  %ld\n", 
                                   i+lbound[0], j+lbound[1], k+lbound[2],
                                   *((ESMC_I8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        else
                            sprintf(msgbuf,"%ld ", 
                                   *((ESMC_I8 *)(this->base_addr) + 
                                   i + j*imax + k*jmax*imax));
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                        rcount++;
                        if (!opt_all && (tcount > 22) && (rcount==10)) {
                           int krem;
                           sprintf(msgbuf,"%c skipping to end ...\n", beforeskip);
                           printf(msgbuf);
                             // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                           k = (tcount-11) / (imax*jmax);
                           krem = (tcount-11) % (imax*jmax);
                           j = krem / imax;
                           i = krem % imax;
                        }
                    }
                    if (opt_byline) {
                        sprintf(msgbuf,"\n");
                        printf(msgbuf);
                          // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                    } 
                  }
                }
                break;
              default:
                sprintf(msgbuf,"no code to handle integer rank %d yet\n", this->rank);
                printf(msgbuf);
                  // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
                break;    
            }
            break;
        }

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayPrint

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayWrite"
//BOP
// !IROUTINE:  ESMC_ArrayWrite - write contents of a InternArray
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayWrite(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options,             // in - write options
      const char *filename) const {    // in - file name
//
// !DESCRIPTION:
//      Write the contents of an {\tt ESMC\_InternArray} to disk.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

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

    fprintf(ffile, "ArrayWrite: InternArray at address 0x%08lx:  ", 
                           (ESMC_POINTER)this);
    fprintf(ffile, "rank = %d, kind = %d\n", 
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
              case 4:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                lmax = this->counts[3];
                tcount = imax * jmax * kmax * lmax;
                rcount = 0; 
                for (l=0; l<lmax; l++) {
                  for (k=0; k<kmax; k++) {
                    for (j=0; j<jmax; j++) {
                      for (i=0; i<imax; i++) {
                          fprintf(ffile, "%lg ",
                           *((ESMC_R4 *)(this->base_addr) + i + j*imax + k*jmax*imax 
                                                         + l*kmax*jmax*imax));
                      }
                      fprintf(ffile, "\n");
                    }
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
              case 4:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                lmax = this->counts[3];
                tcount = imax * jmax * kmax * lmax;
                rcount = 0; 
                for (l=0; l<lmax; l++) {
                  for (k=0; k<kmax; k++) {
                    for (j=0; j<jmax; j++) {
                      for (i=0; i<imax; i++) {
                          fprintf(ffile, "%lg ",
                           *((ESMC_R8 *)(this->base_addr) + i + j*imax + k*jmax*imax 
                                                         + l*kmax*jmax*imax));
                      }
                      fprintf(ffile, "\n");
                    }
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
              case 4:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                lmax = this->counts[3];
                tcount = imax * jmax * kmax * lmax;
                rcount = 0; 
                for (l=0; l<lmax; l++) {
                  for (k=0; k<kmax; k++) {
                    for (j=0; j<jmax; j++) {
                      for (i=0; i<imax; i++) {
                          fprintf(ffile, "%d ",
                           *((int *)(this->base_addr) + i + j*imax + k*jmax*imax 
                                                         + l*kmax*jmax*imax));
                      }
                      fprintf(ffile, "\n");
                    }
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
              case 4:
                imax = this->counts[0];
                jmax = this->counts[1];
                kmax = this->counts[2];
                lmax = this->counts[3];
                tcount = imax * jmax * kmax * lmax;
                rcount = 0; 
                for (l=0; l<lmax; l++) {
                  for (k=0; k<kmax; k++) {
                    for (j=0; j<jmax; j++) {
                      for (i=0; i<imax; i++) {
                          fprintf(ffile, "%ld ",
                           *((ESMC_I8 *)(this->base_addr) + i + j*imax + k*jmax*imax 
                                                         + l*kmax*jmax*imax));
                      }
                      fprintf(ffile, "\n");
                    }
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

 } // end ESMC_ArrayWrite


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySerialize"
//BOPI
// !IROUTINE:  ESMC_ArraySerialize - Turn array information into a byte stream
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArraySerialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset) const {   // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in array class into a stream of bytes.
//
//EOPI
    int fixedpart, rc;
    int i, *ip;
    char *cp;
    struct ESMC_AxisIndex *ap;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    fixedpart = sizeof(ESMC_InternArray) + byte_count;
    if ((*length - *offset) < fixedpart) {
        
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                               "Buffer too short to add an InternArray object", &rc);
         return rc;

        //buffer = (char *)realloc((void *)buffer, 
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }

    // First, serialize the base class, then the localarray part, then
    // finally the data unique to arrays.
    rc = ESMC_Base::ESMC_Serialize(buffer, length, offset);
    rc = ESMC_LocalArray::ESMC_Serialize(buffer, length, offset);

    //  then serialize here just the additional data contained in an array:
    //  the AI info and the halo widths.

    ap = (struct ESMC_AxisIndex *)(buffer + *offset);
  
    for (i=0; i<ESMF_MAXDIM; i++) {
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_alloc+i, ap); ap++;
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_total+i, ap); ap++;
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_comp+i, ap);  ap++;
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_excl+i, ap);  ap++;
    }

    ip = (int *)ap;
    for (i=0; i<ESMF_MAXDIM; i++) {
        *ip++ = hwidth[i][0];
        *ip++ = hwidth[i][1];
    }
  
    cp = (char *)ip;
    *offset = (cp - buffer);
   
 // rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_Serialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_ArrayDeserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayDeserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
    int rc;
    int i, *ip;
    char *cp;
    struct ESMC_AxisIndex *ap;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    // First, deserialize the base class, then the localarray part, then
    // finally the data unique to arrays.
    rc = ESMC_Base::ESMC_Deserialize(buffer, offset);
    rc = ESMC_LocalArray::ESMC_Deserialize(buffer, offset);

    //  then deserialize here just the additional data contained in an array:
    //  the AI info and the halo widths.

    ip = (int *)(buffer + *offset);
  
    ap = (struct ESMC_AxisIndex *)ip;
    for (i=0; i<ESMF_MAXDIM; i++) {
        ESMC_AxisIndexCopy(ap, ai_alloc+i); ap++;
        ESMC_AxisIndexCopy(ap, ai_total+i); ap++;
        ESMC_AxisIndexCopy(ap, ai_comp+i);  ap++;
        ESMC_AxisIndexCopy(ap, ai_excl+i);  ap++;
    }

    ip = (int *)ap;
    for (i=0; i<ESMF_MAXDIM; i++) {
        hwidth[i][0] = *ip++;
        hwidth[i][1] = *ip++;
    }
  
    cp = (char *)ip;
    *offset = (cp - buffer);
   
    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayDeserialize

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySerializeNoData"
//BOPI
// !IROUTINE:  ESMC_ArraySerializeNoData - Turn array information into a byte stream
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArraySerializeNoData(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset) const {   // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in array class into a stream of bytes.   This version
//    does not preserve any data associated with the array, just the
//    type/rank/shape information.
//
//EOPI
    int fixedpart, rc;
    struct ESMC_AxisIndex *ap;
    char *cp;
    int i, *ip;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    fixedpart = sizeof(ESMC_InternArray);
    if ((*length - *offset) < fixedpart) {
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                               "Buffer too short to add an InternArray object", 
                               &rc);
         return rc;
        //buffer = (char *)realloc((void *)buffer, *length + 2*fixedpart);
        //*length += 2 * fixedpart;
    }

    // First, serialize the base class, then the localarray part, then
    // finally the data unique to arrays.
    rc = ESMC_Base::ESMC_Serialize(buffer, length, offset);
    rc = ESMC_LocalArray::ESMC_LocalArraySerializeNoData(buffer, length, offset);

    //  then serialize here just the additional data contained in an array:
    //  the AI info and the halo widths.

    ip = (int *)(buffer + *offset);
  
    ap = (struct ESMC_AxisIndex *)ip;
    for (i=0; i<ESMF_MAXDIM; i++) {
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_alloc+i, ap); ap++;
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_total+i, ap); ap++;
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_comp+i, ap);  ap++;
        ESMC_AxisIndexCopy((ESMC_AxisIndex *)ai_excl+i, ap);  ap++;
    }

    ip = (int *)ap;
    for (i=0; i<ESMF_MAXDIM; i++) {
        *ip++ = hwidth[i][0];
        *ip++ = hwidth[i][1];
    }
  
    cp = (char *)ip;
    *offset = (cp - buffer);
   
 // rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_Serialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DeserializeNoData"
//BOPI
// !IROUTINE:  ESMC_ArrayDeserializeNoData - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayDeserializeNoData(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.  This version does not
//    preserve any data, only the type/rank/shape of the array.
//
//EOPI
    struct ESMC_AxisIndex *ap;
    char *cp;
    int i, *ip, rc;

    // call base, then localarray.
    this->ESMC_Base::ESMC_Deserialize(buffer, offset);
    this->ESMC_LocalArray::ESMC_LocalArrayDeserializeNoData(buffer, offset);

    //  then deserialize here just the additional data contained in an array:
    //  the AI info and the halo widths.

    ip = (int *)(buffer + *offset);
  
    ap = (struct ESMC_AxisIndex *)ip;
    for (i=0; i<ESMF_MAXDIM; i++) {
        ESMC_AxisIndexCopy(ap, ai_alloc+i); ap++;
        ESMC_AxisIndexCopy(ap, ai_total+i); ap++;
        ESMC_AxisIndexCopy(ap, ai_comp+i);  ap++;
        ESMC_AxisIndexCopy(ap, ai_excl+i);  ap++;
    }

    ip = (int *)ap;
    for (i=0; i<ESMF_MAXDIM; i++) {
        hwidth[i][0] = *ip++;
        hwidth[i][1] = *ip++;
    }
  
    cp = (char *)ip;
    *offset = (cp - buffer);
   
    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayDeserializeNoData

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InternArray()"
//BOP
// !IROUTINE:  ESMC_InternArray - native C++ constructor
//
// !INTERFACE:
      ESMC_InternArray::ESMC_InternArray(
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

 } // end ESMC_InternArray

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_InternArray()"
//BOP
// !IROUTINE:  ~ESMC_InternArray - native C++ destructor
//
// !INTERFACE:
      ESMC_InternArray::~ESMC_InternArray(void) {
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

 } // end ~ESMC_InternArray


