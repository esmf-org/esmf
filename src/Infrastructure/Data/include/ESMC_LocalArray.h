// $Id: ESMC_LocalArray.h,v 1.3 2003/07/11 23:04:31 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF Array C++ declaration include file
//
//-----------------------------------------------------------------------------
//

 #ifndef ESMC_LocalArray_H
 #define ESMC_LocalArray_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_LocalArray - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Array members and declares method 
// signatures (prototypes).  The companion file ESMC\_LocalArray.C contains
// the definitions (full code bodies) for the Array methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
#include <ESMC_DELayout.h>  //
 //#include <ESMC_XXX.h>   // other dependent classes (subclasses, aggregates,
                        // composites, associates, friends)

// !PUBLIC TYPES:
 class ESMC_LocalArrayConfig;
 class ESMC_LocalArray;

 // dummy structure which is at least as big as an F90 pointer on
 //  each architcture.  ESMF_F90_PTR_xxx are defined in conf.h in
 //  the build directories for each architecture
 // THIS IS NO LONGER TRYING TO MATCH THE SIZE EXACTLY BECAUSE IT CAN
 // BE RANK DEPENDENT ON MANY PLATFORMS.  this is used to reserve space
 // in our array struct.  it may need to be scrapped and computed on the
 // fly for each case.
 //  TODO: check if we can simply save the wrapper and save that and not
 //        have to save the full pointer/dopev
#define MAX_F90_RANK_POSSIBLE 7
struct c_F90ptr {
   unsigned char basepad[ESMF_F90_PTR_BASE_SIZE];      // one of these
#if ESMF_F90_PTR_PLUS_RANK 
   // plus extra space needed per rank
   unsigned char extrapad[MAX_F90_RANK_POSSIBLE*ESMF_F90_PTR_PLUS_RANK];
#endif
};


// !PRIVATE TYPES:

// these must stay in sync with the F90 versions
typedef enum { 
    ESMC_FROM_FORTRAN = 1, 
    ESMC_FROM_CPLUSPLUS 
} ESMC_ArrayOrigin; 

typedef enum { 
    ESMC_DATA_COPY = 1, 
    ESMC_DATA_REF,
    ESMC_DATA_NONE
} ESMC_DataCopy;

typedef enum { 
    ESMC_DOMAIN_LOCAL = 1, 
    ESMC_DOMAIN_COMPUTATIONAL,
    ESMC_DOMAIN_EXCLUSIVE
} ESMC_DomainType;

// this should be public -
typedef enum { 
    ESMC_ARRAY_NO_ALLOCATE = 0, 
    ESMC_ARRAY_DO_ALLOCATE,
    ESMC_ARRAY_ALLOC_IF_BASE_NULL 
} ESMC_ArrayDoAllocate;


// private static data - address of fortran callback funcs
extern "C" {
 void FTN(f_esmf_localarrayf90allocate)(ESMC_LocalArray**, int *, ESMC_DataType*, 
                                       ESMC_DataKind*, int*, int*);
 void FTN(f_esmf_localarrayf90deallocate)(ESMC_LocalArray**, int*, ESMC_DataType*, 
                                       ESMC_DataKind *, int*);
}


// class declaration type
class ESMC_LocalArray : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    int rank;                      // dimensionality
    ESMC_DataType type;            // int, real, etc.
    ESMC_DataKind kind;            // short, long
    ESMC_ArrayOrigin origin;       // was the create called from F90 or C++
    ESMC_Logical needs_dealloc;    // is array responsible for deallocation?
    ESMC_Logical iscontig;         // optimization possible if all contig
    void *base_addr;               // real start of memory
    int offset[ESMF_MAXDIM];       // byte offset from base to 1st element/dim
    int counts[ESMF_MAXDIM];       // number of elements/dim
    int bytestride[ESMF_MAXDIM];   // byte spacing between elements/dim
    struct c_F90ptr f90dopev;      // opaque object which is real f90 ptr
                                   // potentially these could be needed... 
 // void *first_element;           // memory address of the first element
    struct ESMC_AxisIndex ai_local[ESMF_MAXDIM]; // limits for whole array
    struct ESMC_AxisIndex ai_comp[ESMF_MAXDIM];  // for computational region
    struct ESMC_AxisIndex ai_excl[ESMF_MAXDIM];  // never is sent or received
    
// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
    int ESMC_LocalArrayConstruct(int irank, ESMC_DataType dt, 
            ESMC_DataKind dk, int *counts, void *base, 
            ESMC_ArrayOrigin oflag, struct c_F90ptr *f90ptr, 
            ESMC_ArrayDoAllocate aflag, 
            ESMC_DataCopy docopy, ESMC_Logical dflag, 
            int *lbounds, int *ubounds, int *strides, int *offsets);
    int ESMC_LocalArrayDestruct(void);

 // accessor methods for class members
    //int ESMC_ArrayGet<Value>(<value type> *value) const;
    //int ESMC_ArraySet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_LocalArrayWrite(const char *options, const char *filename) const;
    int ESMC_LocalArrayValidate(const char *options) const;
    int ESMC_LocalArrayPrint(const char *options = NULL) const;

 // native C++ constructors/destructors
	ESMC_LocalArray(void);
	~ESMC_LocalArray(void);
  
 // get/set methods for internal data
    int ESMC_LocalArraySetRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int ESMC_LocalArrayGetRank(void) { return this->rank; }

    int ESMC_LocalArraySetType(ESMC_DataType type) { this->type = type; 
                                                     return ESMF_SUCCESS;}
    ESMC_DataType ESMC_LocalArrayGetType(void) { return this->type; }

    int ESMC_LocalArraySetKind(ESMC_DataKind kind) { this->kind = kind; 
                                                     return ESMF_SUCCESS;}
    ESMC_DataKind ESMC_LocalArrayGetKind(void) { return this->kind; }

    int ESMC_LocalArraySetLengths(int n, int *l) { for (int i = 0; i < n; i++)
                                                  this->counts[i] = l[i]; 
                                              return ESMF_SUCCESS;}
    int ESMC_LocalArraySetLengths(int ni, int nj=0, int nk=0, int nl=0, int nm=0) { 
           this->counts[0] = ni; this->counts[1] = nj; 
           this->counts[2] = nk; this->counts[3] = nl; 
           this->counts[4] = nm; return ESMF_SUCCESS;}
    int ESMC_LocalArrayGetLengths(int n, int *l) { for (int i = 0; i < n; i++)
                                                   l[i] = this->counts[i]; 
                                              return ESMF_SUCCESS;}
    int ESMC_LocalArrayGetLengths(int *ni, int *nj=NULL, int *nk=NULL, 
                                int *nl=NULL, int *nm=NULL) { 
           *ni = this->counts[0]; if (nj) *nj = this->counts[1]; 
           if (nk) *nk = this->counts[2]; if (nl) *nl = this->counts[3]; 
           if (nm) *nm = this->counts[4]; return ESMF_SUCCESS;}

    int ESMC_LocalArraySetBaseAddr(void *base_addr) { this->base_addr = base_addr; 
                                                 return ESMF_SUCCESS;}
    int ESMC_LocalArrayGetBaseAddr(void **base) { *base = this->base_addr; 
                                            return ESMF_SUCCESS;}

    int ESMC_LocalArraySetOrigin(ESMC_ArrayOrigin o) { this->origin = o; 
                                                       return ESMF_SUCCESS;}
    ESMC_ArrayOrigin ESMC_LocalArrayGetOrigin(void) { return this->origin; }

    // copy the contents of an f90 ptr
    int ESMC_LocalArraySetF90Ptr(const struct c_F90ptr *p);
    int ESMC_LocalArrayGetF90Ptr(struct c_F90ptr *p) const;

    // set/get the dealloc flag
    int ESMC_LocalArraySetNoDealloc(void) { this->needs_dealloc = ESMF_TF_FALSE; 
                                       return ESMF_SUCCESS;}
    int ESMC_LocalArraySetDealloc(void)   { this->needs_dealloc = ESMF_TF_TRUE; 
                                       return ESMF_SUCCESS;}
    int ESMC_LocalArrayNeedsDealloc(void)  { 
                         return this->needs_dealloc == ESMF_TF_TRUE ? 1 : 0; }

    // get and set useful combinations of values that fortran cares about
    int ESMC_LocalArraySetInfo(struct c_F90ptr *fptr, void *base, int *counts, 
                          int *lbounds, int *ubounds, 
                          int *strides, int *offsets, 
                          ESMC_Logical contig, ESMC_Logical dealloc);
    // TODO: add Get method

  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_LocalArray


// these are functions, but not class methods.
ESMC_LocalArray *ESMC_LocalArrayCreate(int rank, ESMC_DataType dt, ESMC_DataKind dk, 
                    int *counts = NULL, void *base = NULL, 
                    ESMC_DataCopy docopy = ESMC_DATA_REF,
                    int *rc = NULL);
int ESMC_LocalArrayDestroy(ESMC_LocalArray *array);
ESMC_LocalArray *ESMC_LocalArrayCreate_F(int rank, ESMC_DataType dt, ESMC_DataKind dk, 
                    int *icounts = NULL, struct c_F90ptr *f90ptr = NULL, 
                    void *base = NULL, 
                    ESMC_DataCopy docopy = ESMC_DATA_REF,
                    int *lbounds = NULL, int *ubounds = NULL, 
                    int *strides = NULL, int *offsets = NULL, int *rc = NULL);
ESMC_LocalArray *ESMC_LocalArrayCreateNoData(int rank, ESMC_DataType dt, 
                                   ESMC_DataKind dk, ESMC_ArrayOrigin oflag,
                                   int *rc = NULL);


 #endif  // ESMC_LocalArray_H
