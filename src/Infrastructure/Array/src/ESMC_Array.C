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

// for printf
#include <stdio.h>
#include <string.h>
#include <assert.h>
// associated class definition file
#include "ESMC_Array.h"
#include "ESMC_DELayout.h"
#include "ESMC_Grid.h"        // grid info

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_Array.C,v 1.6 2003/07/17 22:46:34 nscollins Exp $";
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
    ESMC_DataType dt,          // int, float, etc
    ESMC_DataKind dk,          // short/long, etc
    int *icounts,              // number of items in each dim
    void *base,                // if non-null, this is already allocated memory
    ESMC_DataCopy docopy,      // if base != NULL, copy data?
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This routine is the C++ entry point for creating an {\tt ESMF\_Array}
//      object.  Unlike natural C++ arrays which can be as simple as the
//      base address pointer and the number of bytes necessary to move to
//      the next item, {\tt ESMF\_Array}s are richer in the associated metadata
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
//   The return from this routine is a pointer to the new Array data.
//
     ESMC_Array *a = new ESMC_Array;
     int status;


     status = a->ESMC_ArrayConstruct(rank, dt, dk, icounts, base, 
                                     ESMC_FROM_CPLUSPLUS,
                                     NULL, ESMC_ARRAY_DO_ALLOCATE, 
                                     docopy, ESMF_TF_TRUE, 
                                     NULL, NULL, NULL, NULL, 0); 
     
     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_ArrayCreate

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
    ESMC_DataType dt,          // int, float, etc
    ESMC_DataKind dk,          // short/long, etc
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

     ESMC_Array *a = new ESMC_Array;
     int status;


     status = a->ESMC_ArrayConstruct(rank, dt, dk, icounts, base, 
                                     ESMC_FROM_CPLUSPLUS,
                                     NULL, ESMC_ARRAY_DO_ALLOCATE, 
                                     docopy, ESMF_TF_TRUE, 
                                     NULL, NULL, NULL, NULL, halo_width); 
     
     if (rc != NULL)
         *rc = status;

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
//      via an {\tt ESMC\_ArrayCreate} routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

    array->ESMC_ArrayDestruct();

    delete array;

    return 0;

 } // end ESMC_ArrayDestroy

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_ArrayCreateNoData - internal routine for fortran use
//
// !INTERFACE:
      ESMC_Array *ESMC_ArrayCreateNoData(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Array
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    ESMC_DataType dt,          // int, float, etc
    ESMC_DataKind dk,          // short/long, etc
    ESMC_ArrayOrigin oflag,    // caller is fortran or C++?
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This version of Create is only intended for internal use by
//      the {\tt ESMF\_ArrayCreate} fortran routine.  It creates a partially
//      constructed array, then depends on the caller to come back and
//      complete the array with the {\tt ESMF\_ArraySetInfo} call.  
//      (It is broken up this way to try to minimize the amount of
//      macro-generated code needed in the {\tt ESMF\_Array.F90} source file.)
//
//EOPI

     ESMC_Array *a = new ESMC_Array;
     int status;

     status = a->ESMC_ArrayConstruct(rank, dt, dk, NULL, NULL, oflag,
                            NULL, ESMC_ARRAY_NO_ALLOCATE, 
                            ESMC_DATA_NONE, ESMF_TF_FALSE, 
                            NULL, NULL, NULL, NULL, 0);

     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_ArrayCreateNoData

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_ArrayCreateNoData - internal routine for fortran use
//
// !INTERFACE:
      ESMC_Array *ESMC_ArrayCreateNoData(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Array
//
// !ARGUMENTS:
    int rank,                  // dimensionality
    ESMC_DataType dt,          // int, float, etc
    ESMC_DataKind dk,          // short/long, etc
    ESMC_ArrayOrigin oflag,    // caller is fortran or C++?
    int halo_width,            // max halo depth in all dirs
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      Same as above but with halo width spec.
//
//EOPI

     ESMC_Array *a = new ESMC_Array;
     int status;

     status = a->ESMC_ArrayConstruct(rank, dt, dk, NULL, NULL, oflag,
                            NULL, ESMC_ARRAY_NO_ALLOCATE, 
                            ESMC_DATA_NONE, ESMF_TF_FALSE, 
                            NULL, NULL, NULL, NULL, halo_width);

     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_ArrayCreateNoData

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
    ESMC_DataType dt,          // int, float, etc
    ESMC_DataKind dk,          // short/long, etc
    int *icounts,              // counts along each dimension
    struct c_F90ptr *f90ptr,   // opaque type which fortran uses (dope v)
    void *base,                // real start of memory 
    ESMC_DataCopy docopy,      // if base is null and this is Copy, alloc here
    int *lbounds,              // lower index number per dim
    int *ubounds,              // upper index number per dim
    int *strides,              // number of bytes between successive items/dim
    int *offsets,              // number of bytes to start of data/dim
    int halo_widths,           // width of halo region
    int *rc) {                 // return code
//
// !DESCRIPTION:
//      This version of Create is only intended for internal use by
//      the {\tt ESMF\_ArrayCreate} fortran routine.  This routine works in a
//      similar manner as the regular {\tt ESMC\_ArrayCreate} routine, but the
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
     int status;

     if (base == NULL) 
         status = a->ESMC_ArrayConstruct(rank, dt, dk, icounts, base, 
                                     ESMC_FROM_FORTRAN, f90ptr, 
                                     ESMC_ARRAY_DO_ALLOCATE,
                                     ESMC_DATA_NONE, ESMF_TF_TRUE, 
                                     lbounds, ubounds, strides, 
                                     offsets, halo_widths); 
     else
         status = a->ESMC_ArrayConstruct(rank, dt, dk, icounts, base, 
                                     ESMC_FROM_FORTRAN, f90ptr, 
                                     ESMC_ARRAY_NO_ALLOCATE, 
                                     docopy, ESMF_TF_FALSE, 
                                     lbounds, ubounds, strides, 
                                     offsets, halo_widths); 

     if (rc != NULL)
         *rc = status;

     return a;

 } // end ESMC_ArrayCreate_F

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayConstruct - fill in an already allocated Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
    int irank,                 // dimensionality
    ESMC_DataType dt,          // int, float, etc
    ESMC_DataKind dk,          // short/long, etc  (*2, *4, *8)
    int *icounts,              // number of items in each dim
    void *base,                // base memory address of data block
    ESMC_ArrayOrigin oflag,    // create called from F90 or C++?
    struct c_F90ptr *f90ptr,   // opaque type which fortran understands (dopev)
    ESMC_ArrayDoAllocate aflag, // do we allocate space or not?
    ESMC_DataCopy docopy,      // do we make a copy of the data?
    ESMC_Logical dflag,        // do we deallocate space or not?
    int *lbounds,              // lower index number per dim
    int *ubounds,              // upper index number per dim
    int *strides,              // number of bytes between successive items/dim
    int *offsets,              // offset in bytes to start of each dim
    int halo_width) {          // halo width on all edges
//
// !DESCRIPTION:
//   ESMF routine which fills in the contents of an already
//   allocated {\tt ESMF\_Array} object.  May need to do additional allocations
//   as needed.  Must call the corresponding {\tt ESMC\_ArrayDestruct}
//   routine to free the additional memory.  Intended for internal
//   ESMF use only; end-users use {\tt ESMC\_ArrayCreate}, which calls
//   {\tt ESMC\_ArrayConstruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  
    int i, status;
    ESMC_Array *aptr;
    int total_stride, comp_stride, excl_stride;

    rank = irank;
    type = dt;
    kind = dk;

    base_addr = base;
    total_stride = 1;
    comp_stride = 1;
    excl_stride = 1;
    for (i=0; i<rank; i++) {
        counts[i]     = icounts ? icounts[i] : 1;        
//        lbound[i]   = lbounds ? lbounds[i] : 1;
//        ubound[i]   = ubounds ? ubounds[i] : counts[i];
        bytestride[i] = strides ? strides[i] : 1;
        offset[i]     = offsets ? offsets[i] : 0;

        total_stride *= counts[i];
        ESMC_AxisIndexSet(ai_total+i, 0, counts[i]-1, total_stride);
        if (halo_width == 0) {
            ESMC_AxisIndexSet(ai_comp+i, 0, counts[i]-1, total_stride);
            ESMC_AxisIndexSet(ai_excl+i, 0, counts[i]-1, total_stride);
        } else {
            comp_stride *= counts[i] - 2*halo_width;
            excl_stride *= counts[i] - 4*halo_width;
            ESMC_AxisIndexSet(ai_comp+i, halo_width, 
                                         counts[i]-1-halo_width, comp_stride);
            ESMC_AxisIndexSet(ai_excl+i, halo_width*2, 
                                      counts[i]-1-(halo_width*2), excl_stride);
        }
    }
    for (i=rank; i<ESMF_MAXDIM; i++) {
        counts[i]     = 1;
//        lbound[i] = 1;
//        ubound[i] = 1;
        bytestride[i] = 1;
        offset[i]     = 0;
        ESMC_AxisIndexSet(ai_total+i, 0, 0, 1);
        ESMC_AxisIndexSet(ai_comp+i, 0, 0, 1);
        ESMC_AxisIndexSet(ai_excl+i, 0, 0, 1);
    }

    origin = oflag;
    needs_dealloc = dflag;

    if (f90ptr != NULL)
        ESMC_ArraySetF90Ptr(f90ptr);
 
    if (aflag == ESMC_ARRAY_DO_ALLOCATE) {
            aptr = this;
            FTN(f_esmf_arrayf90allocate)(&aptr, &rank, &type, &kind, 
                                                      counts, &status);
    } 

    
    // TODO: memcpy from base to base_addr, proper number of bytes?
    //  if docopy flag is set.

    return ESMF_SUCCESS;



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
//      {\tt ESMF\_ArrayConstruct}, does any additional cleanup before the
//      original {\tt ESMC\_Array} object is freed.  Intended for internal ESMF
//      use only; end-users use {\tt ESMC\_ArrayDestroy}, which calls
//      {\tt ESMC\_ArrayDestruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

    int rc = ESMF_FAILURE;
    ESMC_Array *aptr = this;

    // check origin and alloc flag, and call dealloc routine if needed 
    if (needs_dealloc != ESMF_TF_TRUE)
        return ESMF_SUCCESS;

    // if there is an F90 dope vector, we have to call back into fortran
    // to deallocate this.   if we want to support a C++ only library,
    // then this code needs to be calling malloc/free or new/delete and
    // needs conditional code to pick the fortran or C++ mem mgt system.

    FTN(f_esmf_arrayf90deallocate)(&aptr, &rank, &type, &kind, &rc);

    return rc;

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
//    Returns the set of resources the {\tt ESMC\_Array} object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    int rc = ESMF_FAILURE;

    return rc;
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
//    Configures the {\tt ESMC\_Array} object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    int rc = ESMF_FAILURE;

    return rc;

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
//     Returns the value of {\tt ESMC\_Array} member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

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
      //const <value type> *value) {     // in - value
//
// !DESCRIPTION:
//     Sets the value of {\tt ESMC\_Array} member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//
    //int rc = ESMF_FAILURE;

    //return rc;

 //} // end ESMC_ArraySet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArraySetInfo - Set the most common F90 needs
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArraySetInfo(
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
    int *strides,             // in - numbytes between consecutive items/dim
    int *offsets,             // in - numbytes from base to 1st item/dim
    ESMC_Logical contig,      // in - is memory chunk contiguous?
    ESMC_Logical dealloc) {   // in - do we need to deallocate at delete?
//
// !DESCRIPTION:
//     Sets a list of values associated with an already created pointer.
//     This particular set was chosen to mesh well with creation on the
//     F90 side.  Other combinations will probably be useful.
//
//EOP
// !REQUIREMENTS:  

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
  
    // note - starts at 1; base includes rank 1 size
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
   //fprintf(stderr, "setting f90 ptr from %lx to %lx, %d bytes for rank %d\n", 
   //                (long int)fptr, (long int)(&this->f90dopev), bytes, rank);

    memcpy((void *)(&this->f90dopev), (void *)fptr, bytes);

    base_addr = base;
    for (i=0; i<rank; i++) {
        counts[i]     = icounts ? icounts[i] : 0;
        offset[i]     = offsets ? offsets[i] : 0;
        bytestride[i] = strides ? strides[i] : 0;
//        lbound[i] = lbounds ? lbounds[i] : 0;
//        ubound[i] = ubounds ? ubounds[i] : counts[i];
    }
    for (i=rank; i<ESMF_MAXDIM; i++) {
        counts[i]     = 1;
        offset[i]     = 0;
        bytestride[i] = 1;
//        lbound[i] = 1;
//        ubound[i] = 1;
    }
    iscontig = contig;
    needs_dealloc = dealloc;

    return ESMF_SUCCESS; 

 } // end ESMC_ArraySetInfo

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayGetF90Ptr - get F90Ptr for a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayGetF90Ptr(
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

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
  
    // note - starts at 1; base includes rank 1 size
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    //fprintf(stderr, "getting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
    //                 (long int)(&this->f90dopev), (long int)p, bytes, rank);

    memcpy((void *)p, (void *)(&this->f90dopev), bytes);

    return ESMF_SUCCESS; 

 } // end ESMC_ArrayGetF90Ptr

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArraySetF90Ptr - set F90Ptr for a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArraySetF90Ptr(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const struct c_F90ptr *p) {     // in - f90 pointer block
//
// !DESCRIPTION:
//     Sets the {\tt ESMC\_Array} member F90ptr with the given value.
//
//EOP
// !REQUIREMENTS:  

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
  
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    //fprintf(stderr, "setting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
    //                  (long int)p,  (long int)(&this->f90dopev), bytes, rank);

    memcpy((void *)(&this->f90dopev), (void *)p, bytes);

    return ESMF_SUCCESS; 

 } // end ESMC_ArraySetF90Ptr

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArraySetAxisIndex - set annotation on Arrays for local/global
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArraySetAxisIndex(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DomainType dt,              // in - domain type, total/comp/excl
      struct ESMC_AxisIndex *ai) {     // in - values to set
//
// !DESCRIPTION:
//     Sets the {\tt ESMC\_Array} member {\tt ESMC\_AxisIndex} with the given value.
//
//EOP
// !REQUIREMENTS:  

     int i;

     switch(dt) {
       case ESMC_DOMAIN_LOCAL:
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
         fprintf(stderr, "bad value for domain type in ESMF_ArraySetAxisIndex\n");
         return ESMF_FAILURE;
    }

    return ESMF_SUCCESS;

 } // end ESMC_ArraySetAxisIndex

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayGetAxisIndex - get annotation on Arrays for local/global
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayGetAxisIndex(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DomainType dt,                 // in - domain type, total/comp/excl
      struct ESMC_AxisIndex *ai) const {  // out - values to get
//
// !DESCRIPTION:
//     Gets the {\tt ESMC\_Array} member {\tt ESMC\_AxisIndex} with the given value.
//
//EOP
// !REQUIREMENTS:  

     int i;

     switch(dt) {
       case ESMC_DOMAIN_LOCAL:
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

       default:
         fprintf(stderr, "bad value for domain type in ESMF_ArrayGetAxisIndex\n");
         return ESMF_FAILURE;
    }

     return ESMF_SUCCESS;

 } // end ESMC_ArrayGetAxisIndex

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayGetAllAxisIndices - get all AIs for local/global
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayGetAllAxisIndices(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Grid *grid,                     // in - associated grid 
      struct ESMC_AxisIndex *total,        // out - total region
      struct ESMC_AxisIndex *comp,         // out - computational
      struct ESMC_AxisIndex *excl) const { // out - exclusive
//
// !DESCRIPTION:
//  Based on the grid and the halo widths of the {\tt ESMC\_Array} 
//  on the local DE, compute all requested AI lists for all {\tt DE}s
//  in the DELayout associated with the specified {\tt ESMF_Grid}.
//  This does assume that all {\tt ESMC\_Array}s associated with 
//  a grid have the same halo widths (but they can be different sizes
//  on different sides).
//
//EOP

     int i;
     int halo_width;   // TODO: make this an array before/after each dim

     // TODO: when widths are 2*Ndim, compute all of them.
     halo_width = ai_comp[0].min - ai_total[0].min;

     // nsc - needs code here
     // nsc - ask grid how many DEs there are.

     // nsc - needs code here
     // nsc - ask grid for the starts along each dim
     // nsc - ai_global

     
     for (i=0; i<rank; i++) {

         // nsc - fixme.  code not implemented

         if (total) {
             total[i].max = 0;
             total[i].min = 0;
             total[i].stride = 1;
         }

         if (comp) {
             comp[i].max = 0;
             comp[i].min = 0;
             comp[i].stride = 1;
         }

         if (excl) {
             excl[i].max = 0;
             excl[i].min = 0;
             excl[i].stride = 1;
         }

     }
    
     return ESMF_SUCCESS;

 } // end ESMC_ArrayGetAllAxisIndices

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayHalo - update the halo of an Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayHalo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *layout,     // in  - layout (temporarily)
      ESMC_AxisIndex *ai_global, // in  - do we need?  jw
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp) {         // in  - size of decomp array
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    int i_exc, j_exc;
    float *fp, *fp0;
    int *ip, *ip0;

//  allocate global-sized array on each DE and fill with distributed data
//  from current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * ai_comp[i].max;
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);
    }

    // switch based on datatype  TODO: this might be a good place to use templates
    switch (this->type) {
      case ESMF_DATA_REAL:
        // allocate global array from this size
        fp = new float[gsize];

        // call layoutgather to fill this array
        fp0 = (float *)this->base_addr;
        layout->ESMC_DELayoutGatherArrayF(fp0, decompids, size_decomp, 
                                          ai_comp, ai_total, fp);

        // switch based on array rank
        switch (this->rank) {
          case 1:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
          case 2:
            {
              //  copy total domain of Array from global array
              int gmax[2];
              int lmax[2];
              int lstart[2];
              gmax[0] = 1;
              for (i=1; i<this->rank; i++) {
                gmax[i] = ai_comp[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = ai_total[i].min - ai_total[i].min + 1;
                lstart[i] = ai_global[i].min + ai_total[i].min;  // jw?
              }
              int local, global;
              for (j=0; j<lmax[1]; j++) {
                j_exc = j + lstart[1] - ai_comp[1].min;
                if (j_exc>=0 && j_exc<ai_comp[1].max) {
                  for (i=0; i<lmax[0]; i++) {
                    i_exc = i + lstart[0] - ai_comp[0].min;
                    if (i_exc>=0 && i_exc<ai_comp[0].max) {
                      local  = lmax[0]*j + i;
                      global = gmax[1]*j_exc +
                               gmax[0]*i_exc;
                      fp0[local] = fp[global];
                    }
                  }
                }
              }
            }
          break;
          case 3:
            {
              //  copy total domain of Array from global array
              int gmax[3];
              int lmax[3];
              int lstart[3];
              gmax[0] = 1;
              for (i=1; i<this->rank; i++) {
                gmax[i] = ai_comp[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = ai_total[i].max - ai_total[i].min + 1;
                lstart[i] = ai_global[i].min + ai_total[i].min;
              }
              int local, global;
              for (k=0; k<lmax[2]; k++) {
                for (j=0; j<lmax[1]; j++) {
                  for (i=0; i<lmax[0]; i++) {
                    local  = lmax[1]*lmax[0]*k +
                             lmax[0]*j + i;
                    global = gmax[2]*gmax[1]*(k+lstart[2]) + 
                             gmax[1]*(j+lstart[1]) +
                             gmax[0]*(i+lstart[0]);
                    fp0[local] = fp[global];
                  }
                }
              }
            }
          break;
          case 4:
            {
              //  copy total domain of Array from global array
              int gmax[4];
              int lmax[4];
              int lstart[4];
              gmax[0] = 1;
              for (i=1; i<this->rank; i++) {
                gmax[i] = ai_comp[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = ai_total[i].min - ai_total[i].min + 1;
                lstart[i] = ai_global[i].min + ai_total[i].min;
              }
              int local, global;
              for (l=0; l<lmax[3]; l++) {
                for (k=0; k<lmax[2]; k++) {
                  for (j=0; j<lmax[1]; j++) {
                    for (i=0; i<lmax[0]; i++) {
                      local  = lmax[2]*lmax[1]*lmax[0]*l +
                               lmax[1]*lmax[0]*k + 
                               lmax[0]*j + i;
                      global = gmax[3]*gmax[2]*gmax[1]*(l+lstart[3]) +
                               gmax[2]*gmax[1]*(k+lstart[2]) + 
                               gmax[1]*(j+lstart[1]) +
                               gmax[0]*(i+lstart[0]);
                      fp0[local] = fp[global];
                    }
                  }
                }
              }
            }
          break;
          case 5:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
          default:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
        }

        // deallocate global array
        delete [] fp;
      break;

      case ESMF_DATA_INTEGER:
        // allocate global array from this size
        ip = new int[gsize];

        // call layoutgather to fill this array
        ip0 = (int *)this->base_addr;
        layout->ESMC_DELayoutGatherArrayI(ip0, decompids, size_decomp, 
                                          ai_comp, ai_total, ip);

        // switch based on array rank
        switch (this->rank) {
          case 1:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
          case 2:
            {
              //  copy total domain of Array from global array
              int gmax[2];
              int lmax[2];
              int lstart[2];
              gmax[0] = 1;
              for (i=1; i<this->rank; i++) {
                gmax[i] = ai_comp[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = ai_total[i].max - ai_total[i].min + 1;
                lstart[i] = ai_global[i].min + ai_total[i].min;
              }
              int local, global;
              for (j=0; j<lmax[1]; j++) {
                j_exc = j + lstart[1] - ai_comp[1].min;
                if (j_exc>=0 && j_exc<ai_comp[1].max) {
                  for (i=0; i<lmax[0]; i++) {
                    i_exc = i + lstart[0] - ai_comp[0].min;
                    if (i_exc>=0 && i_exc<ai_comp[0].max) {
                      local  = lmax[0]*j + i;
                      global = gmax[1]*j_exc +
                               gmax[0]*i_exc;
                      ip0[local] = ip[global];
                    }
                  }
                }
              }
            }
          break;
          case 3:
            {
              //  copy total domain of Array from global array
              int gmax[3];
              int lmax[3];
              int lstart[3];
              gmax[0] = 1;
              for (i=1; i<this->rank; i++) {
                gmax[i] = ai_comp[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = ai_total[i].max - ai_total[i].min + 1;
                lstart[i] = ai_global[i].min + ai_total[i].min;
              }
              int local, global;
              for (k=0; k<lmax[2]; k++) {
                for (j=0; j<lmax[1]; j++) {
                  for (i=0; i<lmax[0]; i++) {
                    local  = lmax[1]*lmax[0]*k +
                             lmax[0]*j + i;
                    global = gmax[2]*gmax[1]*(k+lstart[2]) + 
                             gmax[1]*(j+lstart[1]) +
                             gmax[0]*(i+lstart[0]);
                    ip0[local] = ip[global];
                  }
                }
              }
            }
          break;
          case 4:
            {
              //  copy total domain of Array from global array
              int gmax[4];
              int lmax[4];
              int lstart[4];
              gmax[0] = 1;
              for (i=1; i<this->rank; i++) {
                gmax[i] = ai_comp[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = ai_total[i].max - ai_total[i].min + 1;
                lstart[i] = ai_global[i].min + ai_total[i].min;
              }
              int local, global;
              for (l=0; l<lmax[3]; l++) {
                for (k=0; k<lmax[2]; k++) {
                  for (j=0; j<lmax[1]; j++) {
                    for (i=0; i<lmax[0]; i++) {
                      local  = lmax[2]*lmax[1]*lmax[0]*l +
                               lmax[1]*lmax[0]*k + 
                               lmax[0]*j + i;
                      global = gmax[3]*gmax[2]*gmax[1]*(l+lstart[3]) +
                               gmax[2]*gmax[1]*(k+lstart[2]) + 
                               gmax[1]*(j+lstart[1]) +
                               gmax[0]*(i+lstart[0]);
                      ip0[local] = ip[global];
                    }
                  }
                }
              }
            }
          break;
          case 5:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
          default:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
        }

        // deallocate global array
        delete [] ip;
      break;
      default:
        printf("no code to handle data type %d yet\n", this->type);
      break;
    }

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayHalo


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayAllGather - gather a distributed Array onto all DE's
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayAllGather(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *layout,     // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      ESMC_Array **Array_out) {  // out - new Array on all DE's with the global data
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    int i_exc, j_exc;
    float *fp, *fp0;
    int *ip, *ip0;
    int counts[ESMF_MAXDIM];
    ESMC_Array *gathered;

//  allocate global-sized array on each DE and fill with distributed data
//  from current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * ai_comp[i].max;
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);
      counts[i] = ai_comp[i].max;
    }

    // switch based on datatype  TODO: this might be a good place to use templates
    switch (this->type) {
      case ESMF_DATA_REAL:
        // create array with global data buffer
        gathered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
        // allocate global array from this size
        fp = (float *)(gathered->base_addr);

        // call layoutgather to fill this array
        fp0 = (float *)this->base_addr;
        layout->ESMC_DELayoutGatherArrayF(fp0, decompids, size_decomp, 
                                          ai_comp, ai_total, fp);

      break;

      case ESMF_DATA_INTEGER:
        // create array with global data
        gathered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
        // allocate global array from this size
        ip = (int *)(gathered->base_addr);

        // call layoutgather to fill this array
        ip0 = (int *)this->base_addr;
        layout->ESMC_DELayoutGatherArrayI(ip0, decompids, size_decomp, 
                                          ai_comp, ai_total, ip);

      break;
      default:
        printf("no code to handle data type %d yet\n", this->type);
      break;
    }

    //gathered->ESMC_ArrayPrint();

    *Array_out = gathered;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayAllGather


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayGather - gather a distributed Array onto 1 DE
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayGather(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *layout,     // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      int deid,                  // in  - the DE to collect the data on
      ESMC_Array **Array_out) {  // out - new Array on all DE's with the global data
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    int thisde;
    int i_exc, j_exc;
    float *fp, *fp0;
    int *ip, *ip0;
    int counts[ESMF_MAXDIM];
    ESMC_Array *gathered;

//  allocate global-sized array on 1 DE and fill with distributed data
//  from each current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * ai_comp[i].max;
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);
      counts[i] = ai_comp[i].max;
    }

    layout->ESMC_DELayoutGetDEID(&thisde);

    // switch based on datatype  TODO: this might be a good place to use templates
    switch (this->type) {
      case ESMF_DATA_REAL:
        // create array with global data buffer
        if (thisde == deid) {
          gathered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
          // allocate global array from this size
          fp = (float *)(gathered->base_addr);

          // call layoutgather to fill this array
          fp0 = (float *)this->base_addr;

          // call something which will do a receive
          layout->ESMC_DELayoutGatherArrayF(fp0, decompids, size_decomp, 
                                            ai_comp, ai_total, fp);
        } else {
          // call something which will do a send
          layout->ESMC_DELayoutGatherArrayF(fp0, decompids, size_decomp, 
                                            ai_comp, ai_total, fp);
        } 

      break;

      case ESMF_DATA_INTEGER:
        // create array with global data
        if (thisde == deid) {
          gathered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
          // allocate global array from this size
          ip = (int *)(gathered->base_addr);

          // call layoutgather to fill this array

          // call something which will do a receive
          ip0 = (int *)this->base_addr;
          layout->ESMC_DELayoutGatherArrayI(ip0, decompids, size_decomp, 
                                            ai_comp, ai_total, ip);
        } else {
          // call something which will do a send
          layout->ESMC_DELayoutGatherArrayI(ip0, decompids, size_decomp, 
                                            ai_comp, ai_total, ip);
        }
      break;
      default:
        printf("no code to handle data type %d yet\n", this->type);
      break;
    }

    //gathered->ESMC_ArrayPrint();

    if (thisde == deid)
       *Array_out = gathered;
    else
       *Array_out = NULL;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayGather


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayScatter - scatter a single Array onto N distributed DEs
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *layout,     // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      int deid,                  // in  - the DE the original Array is on
      ESMC_Array **Array_out) {  // out - new Array on all DE's with the global data
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    int thisde;
    int i_exc, j_exc;
    float *fp, *fp0;
    int *ip, *ip0;
    int counts[ESMF_MAXDIM];
    ESMC_Array *scattered;

#if 0
    // TODO: this is simply a copy of gather - it needs to be fleshed out
    // and completed.

//  allocate global-sized array on 1 DE and fill with distributed data
//  from each current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * ai_comp[i].max;
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);
      counts[i] = ai_comp[i].max;
    }

    layout->ESMC_DELayoutGetDEID(&thisde);

    // switch based on datatype  TODO: this might be a good place to use templates
    switch (this->type) {
      case ESMF_DATA_REAL:
        // create array with global data buffer
        if (thisde == deid) {
          scattered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
          // allocate global array from this size
          fp = (float *)(scattered->base_addr);

          // call layoutscatter to fill this array
          fp0 = (float *)this->base_addr;

          // call something which will do a receive
          layout->ESMC_DELayoutScatterArrayF(fp0, decompids, size_decomp, 
                                            ai_comp, ai_total, fp);
        } else {
          // call something which will do a send
          layout->ESMC_DELayoutScatterArrayF(fp0, decompids, size_decomp, 
                                            ai_comp, ai_total, fp);
        } 

      break;

      case ESMF_DATA_INTEGER:
        // create array with global data
        if (thisde == deid) {
          scattered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
          // allocate global array from this size
          ip = (int *)(scattered->base_addr);

          // call layoutscatter to fill this array

          // call something which will do a receive
          ip0 = (int *)this->base_addr;
          layout->ESMC_DELayoutScatterArrayI(ip0, decompids, size_decomp, 
                                            ai_comp, ai_total, ip);
        } else {
          // call something which will do a send
          layout->ESMC_DELayoutScatterArrayI(ip0, decompids, size_decomp, 
                                            ai_comp, ai_total, ip);
        }
      break;
      default:
        printf("no code to handle data type %d yet\n", this->type);
      break;
    }

    //scattered->ESMC_ArrayPrint();

    *Array_out = scattered;
    rc = ESMF_SUCCESS;
#endif

    *Array_out = NULL;
    rc = ESMF_FAILURE;

    return rc;

 } // end ESMC_ArrayScatter


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayRedist - general redistribution of an Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayRedist(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *layout,     // in  - layout (temporarily)
      ESMC_AxisIndex *ai_global, // in  - do we need?  jw
      int rank_trans[],          // in  - translation of old ranks to new
                                 //       Array
      int size_rank_trans,       // in  - size of rank_trans array
      int olddecompids[],        // in  - decomposition identifier for each
                                 //       axis for the original Array
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the redistributed Array
      int size_decomp,           // in  - size of decomp arrays
      ESMC_Array *RedistArray) { // out - Redistributed Array
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    float *fp;
    int *ip, *ip0;

//  allocate global-sized array on each DE and fill with distributed data
//  from current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * ai_global[i].stride;  // jw?  needs to be size of global array
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);  // jw?
    }

    // switch based on datatype
    switch (this->type) {
      case ESMF_DATA_REAL:
        // allocate global array from this size
        fp = new float[gsize];
        delete [] fp;
      break;
      case ESMF_DATA_INTEGER:
        // allocate global array from this size
        ip = new int[gsize];

        // call layoutgather to fill this array
        ip0 = (int *)this->base_addr;
        layout->ESMC_DELayoutGatherArrayI(ip0, olddecompids, size_decomp, 
                                          this->ai_comp, this->ai_comp, ip);

        // switch based on array rank
        switch (this->rank) {
          case 1:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
          case 2:
            {
             //  copy decomposed piece of global array into new Array
              int gmax[2];
              int lmax[2];
              int lstart[2];
              gmax[rank_trans[0]-1] = 1;
              for (i=1; i<this->rank; i++) {
                int i_new = rank_trans[i]-1;
                gmax[i_new] = ai_comp[i-1].max;  // jw?
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = RedistArray->ai_comp[i].max 
                        - RedistArray->ai_comp[i].min + 1;
                lstart[i] = RedistArray->ai_comp[i].stride  // jw nasty -- this is
                  // just here so it will compile
                          + RedistArray->ai_comp[i].min;
              }
              int *ip2 = (int *)RedistArray->base_addr;
              int local, global;
              for (j=0; j<lmax[1]; j++) {
                for (i=0; i<lmax[0]; i++) {
                  local  = lmax[0]*j + i;
                  global = gmax[1]*(j+lstart[1]) +
                           gmax[0]*(i+lstart[0]);
                  ip2[local] = ip[global];
                }
              }
            }
          break;
          case 3:
            {
              //  copy decomposed piece of global array into new Array
              int gmax[3];
              int lmax[3];
              int lstart[3];
              gmax[rank_trans[0]-1] = 1;
              for (i=1; i<this->rank; i++) {
                int i_new = rank_trans[i]-1;
                gmax[i_new] = ai_total[i-1].max;  // jw?
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = RedistArray->ai_total[i].max
                        - RedistArray->ai_total[i].min + 1;
                lstart[i] = ai_global[i].min   //  jw? need to look at this
                          + RedistArray->ai_total[i].min;
              }
              int *ip2 = (int *)RedistArray->base_addr;
              int local, global;
              for (k=0; k<lmax[2]; k++) {
                for (j=0; j<lmax[1]; j++) {
                  for (i=0; i<lmax[0]; i++) {
                    local  = lmax[1]*lmax[0]*k +
                             lmax[0]*j + i;
                    global = gmax[2]*gmax[1]*(k+lstart[2]) + 
                             gmax[1]*(j+lstart[1]) +
                             gmax[0]*(i+lstart[0]);
                    ip2[local] = ip[global];
                  }
                }
              }
            }
          break;
          case 4:
            {
              // call allgatherv to fill this array or if Earl works out a method

              //  copy decomposed piece of global array into new Array
              int gmax[4];
              int lmax[4];
              int lstart[4];
              gmax[rank_trans[0]] = 1;
              for (i=1; i<this->rank; i++) {
                int i_new = rank_trans[i];
                gmax[i_new] = ai_total[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = RedistArray->ai_total[i].max
                        - RedistArray->ai_total[i].min + 1;
                lstart[i] = ai_global[i].min  // jw need to look at this
                          + RedistArray->ai_total[i].min;
              }
              int *ip2 = (int *)RedistArray->base_addr;
              int local, global;
              for (l=0; l<lmax[3]; l++) {
                for (k=0; k<lmax[2]; k++) {
                  for (j=0; j<lmax[1]; j++) {
                    for (i=0; i<lmax[0]; i++) {
                      local  = lmax[2]*lmax[1]*lmax[0]*l +
                               lmax[1]*lmax[0]*k + 
                               lmax[0]*j + i;
                      global = gmax[3]*gmax[2]*gmax[1]*(l+lstart[3]) +
                               gmax[2]*gmax[1]*(k+lstart[2]) + 
                               gmax[1]*(j+lstart[1]) +
                               gmax[0]*(i+lstart[0]);
                      ip2[local] = ip[global];
                    }
                  }
                }
              }
            }
          break;
          case 5:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
          default:
            printf("no code to handle array rank %d yet\n", this->rank);
          break;
        }

        // deallocate global array
        delete [] ip;
      break;
      default:
        printf("no code to handle data type %d yet\n", this->type);
      break;
    }

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayRedist


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
//      Validates that a {\tt ESMC\_Array} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    int rc = ESMF_FAILURE;

    return rc;

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
//      Print information about a {\tt ESMC\_Array}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    int rc = ESMF_FAILURE;
    int i, j, k, l, m;
    int imax, jmax, kmax, lmax, mmax;
    int tcount, rcount;
    char between = '\n';
    char beforeskip = ' ';
    bool opt_all = false;   // print all data
    bool opt_exc = false;   // print only exclusive region (needs halo len)
    bool opt_byline = false;  // print a row/line

    if (options) {
        if (strstr(options, "full")) opt_all = true;
        if (strstr(options, "exclusive")) opt_exc = true;
        if (strstr(options, "line")) opt_byline = true;
    }

    if (opt_byline) {
        between = ' ';
        beforeskip = '\n';
    }

    printf("ArrayPrint: Array at address 0x%08lx:\n", (unsigned long)this);
    printf("            rank = %d, type = %d, kind = %d, ", 
                             this->rank, this->type, this->kind);
    printf("base_addr = 0x%08lx\n", (unsigned long)this->base_addr);
    printf("            ");
    for (i=0; i<this->rank; i++) 
        printf("dim[%d] = %d  ", i, this->counts[i]);
    printf("\n");
    
    // TODO: make this look at one of the option letters to see if user
    //   wants data printed.
    switch (this->type) {
      case ESMF_DATA_REAL:
        switch (this->rank) {
          case 1:
            printf("  Real, Dim 1, Data values:\n");
            imax = this->counts[0];
            tcount = imax;
            for (i=0; i<tcount; i++) {
                if (!opt_byline)
                    printf("(%2d) =  %lg\n", i+1, *((float *)(this->base_addr) + i));
                else
                    printf("%lg ", *((float *)(this->base_addr) + i));
                if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                   printf("%c skipping to end ...\n", beforeskip);
                   i = tcount - 11;
                }
            }
            if (opt_byline) printf("\n");
            break;
          case 2:
            printf("  Real, Dim 2, Data values:\n");
            imax = this->counts[0];
            jmax = this->counts[1];
            tcount = imax * jmax;
            rcount = 0;
            for (j=0; j<jmax; j++) {
                if (opt_byline) printf("(*,%2d) = ", j+1);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        printf("(%2d,%2d) =  %lg\n", i+1, j+1, 
                               *((float *)(this->base_addr) + i + j*imax) );
                    else
                        printf("%lg ",  
                               *((float *)(this->base_addr) + i + j*imax) );
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       printf("%c skipping to end ...\n", beforeskip);
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
                if (opt_byline) printf("\n");
            }
            break;
          case 3:
            printf("  Real, Dim 3, Data values:\n");
            imax = this->counts[0];
            jmax = this->counts[1];
            kmax = this->counts[2];
            tcount = imax * jmax * kmax;
            rcount = 0; 
            for (k=0; k<kmax; k++) {
              for (j=0; j<jmax; j++) {
                if (opt_byline) printf("(*,%2d,%2d) = ", j+1, k+1);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        printf("(%2d,%2d,%2d) =  %g\n", 
                               i+1, j+1, k+1,
                               *((float *)(this->base_addr) + 
                               i + j*imax + k*jmax*imax));
                    else
                         printf("%g ", *((float *)(this->base_addr) + 
                               i + j*imax + k*jmax*imax));
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       int krem;
                       printf("%c skipping to end ...\n", beforeskip);
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
                if (opt_byline) printf("\n");
              }
            }
            break;
          default:
            printf("no code to handle real rank %d yet\n", this->rank);
            break;    
        }
        break;
      case ESMF_DATA_INTEGER:
        switch (this->rank) {
          case 1:
            imax = this->counts[0];
            tcount = imax;
            printf("  Integer, Dim 1, Data values:\n");
            for (i=0; i<imax; i++) {
                if (!opt_byline)
                    printf("(%2d) =  %d\n", i+1, 
                           *((int *)(this->base_addr) + i));
                else
                    printf("%d ",
                           *((int *)(this->base_addr) + i));
                if (!opt_all && (tcount > 22) && ((i+1)==10)) {
                   printf("%c skipping to end ...\n", beforeskip);
                   i = tcount - 11;
                }
            }
            if (opt_byline) printf("\n");
            break;
          case 2:
            printf("  Integer, Dim 2, Data values:\n");
            imax = this->counts[0];
            jmax = this->counts[1];
            tcount = imax * jmax;
            rcount = 0; 
            for (j=0; j<jmax; j++) {
                if (opt_byline) printf("(*,%2d) = ", j+1);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        printf("(%2d,%2d) =  %d\n", 
                                i+1, j+1, 
                             *((int *)(this->base_addr) + i + j*imax) );
                    else
                        printf("%d ", 
                             *((int *)(this->base_addr) + i + j*imax) );
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       printf("%c skipping to end ...\n", beforeskip);
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
                if (opt_byline) printf("\n");
            }
            break;
          case 3:
            printf("  Integer, Dim 3, Data values:\n");
            imax = this->counts[0];
            jmax = this->counts[1];
            kmax = this->counts[2];
            tcount = imax * jmax * kmax;
            rcount = 0; 
            for (k=0; k<kmax; k++) {
              for (j=0; j<jmax; j++) {
                if (opt_byline) printf("(*,%2d,%2d) = ", j+1, k+1);
                for (i=0; i<imax; i++) {
                    if (!opt_byline)
                        printf("(%2d,%2d,%2d) =  %d\n", 
                               i+1, j+1, k+1,
                               *((int *)(this->base_addr) + 
                               i + j*imax + k*jmax*imax));
                    else
                        printf("%d ", 
                               *((int *)(this->base_addr) + 
                               i + j*imax + k*jmax*imax));
                    rcount++;
                    if (!opt_all && (tcount > 22) && (rcount==10)) {
                       int krem;
                       printf("%c skipping to end ...\n", beforeskip);
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
                if (opt_byline) printf("\n");
              }
            }
            break;
          default:
            printf("no code to handle integer rank %d yet\n", this->rank);
            break;    
        }
        break;
      default:
            printf("no code to handle data type %d yet\n", this->type);

      break;
    }

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayWrite - write contents of a Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayWrite(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options,             // in - write options
      const char *filename) const {    // in - file name
//
// !DESCRIPTION:
//      Write the contents of an {\tt ESMC\_Array} to disk.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    int rc = ESMF_FAILURE;
    int i, j, k, l, m;
    int imax, jmax, kmax, lmax, mmax;
    int tcount, rcount;
    FILE *ffile = NULL;

    if ((filename == NULL) || (filename[0] == '-')) {
        ffile = stdout;
    } else {
        ffile = fopen(filename, "w");
        if (ffile == NULL) {
            printf("error opening file '%s'\n", filename);
            return ESMF_FAILURE;
        }
    }

    fprintf(ffile, "ArrayWrite: Array at address 0x%08lx:  ", 
                           (unsigned long)this);
    fprintf(ffile, "rank = %d, type = %d, kind = %d\n", 
                             this->rank, this->type, this->kind);
    for (i=0; i<this->rank; i++) 
        fprintf(ffile, " dim[%d] = %d  ", i, this->counts[i]);
    fprintf(ffile, "\n");
    
    // TODO: make this look at one of the option letters to see how user
    //   wants data written (ascii, binary, multifile, singlefile).
    switch (this->type) {
      case ESMF_DATA_REAL:
        switch (this->rank) {
          case 1:
            imax = this->counts[0];
            tcount = imax;
            for (i=0; i<tcount; i++) {
                fprintf(ffile, "%lg\n", *((float *)(this->base_addr) + i));
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
                               *((float *)(this->base_addr) + i + j*imax) );
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
                     *((float *)(this->base_addr) + i + j*imax + k*jmax*imax));
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
      case ESMF_DATA_INTEGER:
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
      default:
            fprintf(ffile, "no code to handle data type %d yet\n", this->type);

      break;
    }

    fclose(ffile);

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayWrite



//-----------------------------------------------------------------------------
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


