// $Id: ESMC_Array.C,v 1.31 2003/02/25 20:04:28 nscollins Exp $
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

// for printf
#include <stdio.h>
#include <assert.h>
// associated class definition file
#include "ESMC_Array.h"
#include "ESMC_Alloc.h"
#include "ESMC_Layout.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_Array.C,v 1.31 2003/02/25 20:04:28 nscollins Exp $";
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
     a->ESMC_ArraySetOrigin(ESMC_FROM_CPLUSPLUS);
  
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
    int *lens,                 // number of items in each dim
    int *strides,              // number of bytes between successive items/dim
    struct c_F90ptr *f90ptr,     // opaque type which fortran understands (dope v)
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
     int i;
     ESMC_Array *a = new ESMC_Array;

     a->ESMC_ArraySetRank(rank);
     a->ESMC_ArraySetType(dt);
     a->ESMC_ArraySetKind(dk);
     a->ESMC_ArraySetBaseAddr((void *)0);
     switch(rank) {
       case 1:
         a->ESMC_ArraySetLengths(lens[0]); break;
       case 2:
         a->ESMC_ArraySetLengths(lens[0], lens[1]); break;
       case 3:
         a->ESMC_ArraySetLengths(lens[0], lens[1], lens[2]); break;
       case 4:
         a->ESMC_ArraySetLengths(lens[0], lens[1], lens[2], lens[3]); break;
       default:
         printf("no len support for rank %d\n", rank); assert(0); break;
     }
     a->ESMC_ArraySetOrigin(ESMC_FROM_FORTRAN);

     //printf("in ESMC_ArrayCreate_F, a = 0x%08lx\n", (unsigned long)a);

     *rc = ESMF_SUCCESS;
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
    int rc = ESMF_FAILURE;

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
//    Returns the set of resources the Array object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

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
//    Configures the Array object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

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
      //const <value type> *value) {     // in - value
//
// !DESCRIPTION:
//     Sets the value of Array member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    //int rc = ESMF_FAILURE;

    //return rc;

 //} // end ESMC_ArraySet<Value>

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
// !REQUIREMENTS:  developer's guide for classes

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
  
    // note - starts at 1; base includes rank 1 size
    for (i=1; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    fprintf(stderr, "getting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
                     (long int)(&this->f90dopev), (long int)p, bytes, rank);

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
//     Sets the Array member F90ptr with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    int i, rank = this->rank;
    int bytes = ESMF_F90_PTR_BASE_SIZE;
  
    for (i=0; i<rank; i++)
	bytes += ESMF_F90_PTR_PLUS_RANK;
   
    fprintf(stderr, "setting f90 ptr, from %lx to %lx, %d bytes for rank %d\n", 
                      (long int)p,  (long int)(&this->f90dopev), bytes, rank);

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
      struct ESMC_AxisIndex *indexlist) {     // in - values to set
//
// !DESCRIPTION:
//     Sets the Array member AxisIndex with the given value.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
     int i;

     for (i=0; i<this->rank; i++) {
         this->ai[i] = indexlist[i];
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
      struct ESMC_AxisIndex *indexlist) const {     // out - values to get
//
// !DESCRIPTION:
//     Gets the Array member AxisIndex with the given value.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
     int i;

     for (i=0; i<this->rank; i++) {
         indexlist[i] = this->ai[i];
     }

     return ESMF_SUCCESS;

 } // end ESMC_ArrayGetAxisIndex

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
      ESMC_Layout *layout,       // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      ESMC_AxisIndex *AI_exc,    // in  - axis indices for the exclusive domain
                                 //       of the Array
      ESMC_AxisIndex *AI_tot) {  // in  - axis indices for the total domain of
                                 //       the Array
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    float *fp, *fp0;
    int *ip, *ip0;

//  allocate global-sized array on each DE and fill with distributed data
//  from current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * AI_exc[i].max;
      lsize = lsize * (AI_exc[i].r - AI_exc[i].l+1);
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
        layout->ESMC_LayoutGatherArrayI(ip0, decompids, 
                                        size_decomp, AI_exc, ip);

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
                gmax[i] = AI_exc[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = AI_tot[i].r - AI_tot[i].l + 1;
                lstart[i] = AI_tot[i].l;
              }
              int local, global;
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
          break;
          case 3:
            {
              //  copy total domain of Array from global array
              int gmax[3];
              int lmax[3];
              int lstart[3];
              gmax[0] = 1;
              for (i=1; i<this->rank; i++) {
                gmax[i] = AI_exc[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = AI_tot[i].r - AI_tot[i].l + 1;
                lstart[i] = AI_tot[i].l;
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
                gmax[i] = AI_exc[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = AI_tot[i].r - AI_tot[i].l + 1;
                lstart[i] = AI_tot[i].l;
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
// !IROUTINE:  ESMC_ArrayRedist - general redistribution of an Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayRedist(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Layout *layout,       // in  - layout (temporarily)
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
      gsize = gsize * ai[i].max;
      lsize = lsize * (ai[i].r - ai[i].l+1);
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
        layout->ESMC_LayoutGatherArrayI(ip0, olddecompids, 
                                        size_decomp, this->ai, ip);

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
                gmax[i_new] = ai[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = RedistArray->ai[i].r - RedistArray->ai[i].l + 1;
                lstart[i] = RedistArray->ai[i].l;
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
                gmax[i_new] = ai[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = RedistArray->ai[i].r - RedistArray->ai[i].l + 1;
                lstart[i] = RedistArray->ai[i].l;
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
                gmax[i_new] = ai[i-1].max;
              }
              for (i=0; i<this->rank; i++) {
                lmax[i] = RedistArray->ai[i].r - RedistArray->ai[i].l + 1;
                lstart[i] = RedistArray->ai[i].l;
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
//      Validates that a Array is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
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
//      Print information about a Array.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
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

    printf("ArrayPrint: Array at address 0x%08lx:\n", (unsigned long)this);
    printf("            rank = %d, type = %d, kind = %d, ", 
                             this->rank, this->type, this->kind);
    printf("base_addr = 0x%08lx\n", (unsigned long)this->base_addr);
    printf("            ");
    for (i=0; i<this->rank; i++) 
        printf("dim[%d] = %d  ", i, this->length[i]);
    printf("\n");
    
    // TODO: make this look at one of the option letters to see if user
    //   wants data printed.
    switch (this->type) {
      case ESMF_DATA_REAL:
        switch (this->rank) {
          case 1:
            printf("  Real, Dim 1, Data values:\n");
            imax = this->length[0];
            tcount = imax;
            for (i=0; i<tcount; i++) {
                printf("(%2d) =  %lg\n", i+1, *((double *)(this->base_addr) + i));
                if ((tcount > 22) && ((i+1)==10)) {
                   printf(" skipping to end ...\n");
                   i = tcount - 11;
                }
            }
            break;
          case 2:
            printf("  Real, Dim 2, Data values:\n");
            imax = this->length[0];
            jmax = this->length[1];
            tcount = imax * jmax;
            rcount = 0;
            for (j=0; j<jmax; j++) {
                for (i=0; i<imax; i++) {
                    printf("(%2d,%2d) =  %lg\n", i+1, j+1, 
                               *((double *)(this->base_addr) + i + j*imax) );
                    rcount++;
                    if ((tcount > 22) && (rcount==10)) {
                       printf(" skipping to end ...\n");
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
            }
            break;
          case 3:
            printf("  Real, Dim 3, Data values:\n");
            imax = this->length[0];
            jmax = this->length[1];
            kmax = this->length[2];
            tcount = imax * jmax * kmax;
            rcount = 0; 
            for (k=0; k<kmax; k++) {
              for (j=0; j<jmax; j++) {
                for (i=0; i<imax; i++) {
                printf("(%2d,%2d,%2d) =  %g\n", i+1, j+1, k+1,
                     *((double *)(this->base_addr) + i + j*imax + k*jmax*imax));
                    rcount++;
                    if ((tcount > 22) && (rcount==10)) {
                       int krem;
                       printf(" skipping to end ...\n");
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
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
            imax = this->length[0];
            tcount = imax;
            printf("  Integer, Dim 1, Data values:\n");
            for (i=0; i<imax; i++) {
                printf("(%2d) =  %d\n", i+1, *((int *)(this->base_addr) + i));
                if ((tcount > 22) && ((i+1)==10)) {
                   printf(" skipping to end ...\n");
                   i = tcount - 11;
                }
            }
            break;
          case 2:
            printf("  Integer, Dim 2, Data values:\n");
            imax = this->length[0];
            jmax = this->length[1];
            tcount = imax * jmax;
            rcount = 0; 
            for (j=0; j<jmax; j++) {
                for (i=0; i<imax; i++) {
                printf("(%2d,%2d) =  %d\n", i+1, j+1, 
                                *((int *)(this->base_addr) + i + j*imax) );
                    rcount++;
                    if ((tcount > 22) && (rcount==10)) {
                       printf(" skipping to end ...\n");
                       j = (tcount-11) / imax;
                       i = (tcount-11) % imax;
                    }
                }
            }
            break;
          case 3:
            printf("  Integer, Dim 3, Data values:\n");
            imax = this->length[0];
            jmax = this->length[1];
            kmax = this->length[2];
            tcount = imax * jmax * kmax;
            rcount = 0; 
            for (k=0; k<kmax; k++) {
              for (j=0; j<jmax; j++) {
                for (i=0; i<imax; i++) {
                printf("(%2d,%2d,%2d) =  %d\n", i+1, j+1, k+1,
                     *((int *)(this->base_addr) + i + j*imax + k*jmax*imax));
                    rcount++;
                    if ((tcount > 22) && (rcount==10)) {
                       int krem;
                       printf(" skipping to end ...\n");
                       k = (tcount-11) / (imax*jmax);
                       krem = (tcount-11) % (imax*jmax);
                       j = krem / imax;
                       i = krem % imax;
                    }
                }
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

extern "C" {
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AllocFuncStore - internal routine
//
// !INTERFACE:
      int ESMC_AllocFuncStore(
//
// !RETURN VALUE:
//    return code
//
// !ARGUMENTS:
      void (*func)(struct c_F90ptr *, int *, int *, int *)) {   // in - fortran function pointer
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
      int ESMC_DeallocFuncStore(
//
// !RETURN VALUE:
//    return code
//
// !ARGUMENTS:
      void (*func)(struct c_F90ptr *, int *, int *, int *)) {   // in - fortran function pointer
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

}

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


