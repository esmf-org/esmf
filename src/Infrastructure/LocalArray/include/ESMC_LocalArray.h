// $Id: ESMC_LocalArray.h,v 1.20 2007/03/31 05:51:15 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

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

// !PUBLIC TYPES:
 class ESMC_LocalArray;

 // dummy structure which is at least as big as an F90 pointer on
 //  each architcture.  ESMF_F90_PTR_xxx are defined in conf.h in
 //  the build directories for each architecture
 // on most platforms, there is a bump in size for each additional rank.
 //  so far no platform has a data-type dependency.
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
    ESMC_DATA_DEFER,
    ESMC_DATA_SPACE,
    ESMC_DATA_NONE
} ESMC_DataCopy;

// this should be public -
typedef enum { 
    ESMC_ARRAY_NO_ALLOCATE = 0, 
    ESMC_ARRAY_DO_ALLOCATE,
    ESMC_ARRAY_ALLOC_IF_BASE_NULL,
    ESMF_ARRAY_ALLOC_DEFERRED
} ESMC_ArrayDoAllocate;


// private static data - address of fortran callback funcs
extern "C" {

  void FTN(f_esmf_localarrayf90allocate)(ESMC_LocalArray**, int *, 
    ESMC_TypeKind*, int *, int *, int *, int *);
 
  void FTN(f_esmf_localarrayf90deallocate)(ESMC_LocalArray**, int*, 
    ESMC_TypeKind *, int *);
 
  void FTN(f_esmf_localarrayadjust)(ESMC_LocalArray**, int *,
    ESMC_TypeKind*, int *, int *, int *, int *);
}


// class declaration type
class ESMC_LocalArray : public ESMC_Base {    // inherits from ESMC_Base class

   protected:
    int rank;                      // dimensionality (1, 2, ..., 7)
    ESMC_TypeKind kind;            // short, long (*4, *8)
    ESMC_ArrayOrigin origin;       // was the create called from F90 or C++?
    ESMC_Logical needs_dealloc;    // is array responsible for deallocation?
    ESMC_Logical iscontig;         // optimization possible if all contig
    void *base_addr;               // real start of memory
    int byte_count;                // size (in bytes) of data region
    int offset[ESMF_MAXDIM];       // byte offset from base to 1st element/dim
    int lbound[ESMF_MAXDIM];       // used for fortran indexing
    int ubound[ESMF_MAXDIM];       // used for fortran indexing
    int counts[ESMF_MAXDIM];       // number of elements/dim
    int bytestride[ESMF_MAXDIM];   // byte spacing between elements/dim
    struct c_F90ptr f90dopev;      // opaque object which is real f90 ptr
                                   // this is memcpy'd to save and restore 
                                   // contents are not interpreted by esmf

// !PUBLIC MEMBER FUNCTIONS:
//
// pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
    int ESMC_LocalArrayConstruct(int irank,  
            ESMC_TypeKind dk, int *counts, void *base, 
            ESMC_ArrayOrigin oflag, struct c_F90ptr *f90ptr, 
            ESMC_ArrayDoAllocate aflag, 
            ESMC_DataCopy docopy, ESMC_Logical dflag, char *name,
            int *lbounds, int *ubounds, int *offsets);
    int ESMC_LocalArrayDestruct(void);

    ESMC_LocalArray *ESMC_LocalArrayAdjust(int *lbounds, int *ubounds, int *rc);

 // accessor methods for class members
    //int ESMC_ArrayGet<Value>(<value type> *value) const;
    //int ESMC_ArraySet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_LocalArrayDeserialize(char *buffer, int *boffset);
    int ESMC_LocalArrayDeserializeNoData(char *buffer, int *boffset);
    int ESMC_LocalArrayPrint(const char *options = NULL) const;
    int ESMC_LocalArraySerialize(char *buffer, int *length, int *boffset) const;
    int ESMC_LocalArraySerializeNoData(char *buffer, int *length, int *boffset) const;
    int ESMC_LocalArrayWrite(const char *options, const char *filename) const;
    int ESMC_LocalArrayValidate(const char *options) const;

 // native C++ constructors/destructors
	ESMC_LocalArray(void);
	~ESMC_LocalArray(void);
  
 // get/set methods for internal data
    int ESMC_LocalArraySetRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int ESMC_LocalArrayGetRank(void) { return this->rank; }

    int ESMC_LocalArraySetTypeKind(ESMC_TypeKind kind) { this->kind = kind; 
                                                     return ESMF_SUCCESS;}
    ESMC_TypeKind ESMC_LocalArrayGetTypeKind(void) { return this->kind; }

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

    int ESMC_LocalArrayGetByteCount(int *count) { *count = this->byte_count; 
                                                   return ESMF_SUCCESS;}

    int ESMC_LocalArraySetOrigin(ESMC_ArrayOrigin o) { this->origin = o; 
                                                       return ESMF_SUCCESS;}
    ESMC_ArrayOrigin ESMC_LocalArrayGetOrigin(void) { return this->origin; }

    int ESMC_LocalArrayGetLbounds(int n, int *l) { for (int i = 0; i < n; i++)
                                                      l[i] = this->lbound[i];
                                                   return ESMF_SUCCESS;}
    int ESMC_LocalArrayGetUbounds(int n, int *u) { for (int i = 0; i < n; i++)
                                                      u[i] = this->ubound[i];
                                                   return ESMF_SUCCESS;}
    int ESMC_LocalArrayGetCounts(int n, int *c) { for (int i = 0; i < n; i++)
                                                      c[i] = this->counts[i];
                                                   return ESMF_SUCCESS;}

    int ESMC_LocalArraySetName(char *name) { return ESMC_BaseSetName(name, "LocalArray"); }
    char *ESMC_LocalArrayGetName(void) { return ESMC_BaseGetName(); }

    // copy the contents of an f90 ptr
    int ESMC_LocalArraySetF90Ptr(const struct c_F90ptr *p);
    int ESMC_LocalArrayGetF90Ptr(struct c_F90ptr *p) const;

    // set/get the dealloc flag
    int ESMC_LocalArraySetNoDealloc(void) { this->needs_dealloc = ESMF_FALSE; 
                                       return ESMF_SUCCESS;}
    int ESMC_LocalArraySetDealloc(void)   { this->needs_dealloc = ESMF_TRUE; 
                                       return ESMF_SUCCESS;}
    int ESMC_LocalArrayNeedsDealloc(void)  { 
                         return this->needs_dealloc == ESMF_TRUE ? 1 : 0; }

    // get and set useful combinations of values that fortran cares about
    int ESMC_LocalArraySetInfo(struct c_F90ptr *fptr, void *base, int *counts, 
                          int *lbounds, int *ubounds, int *offsets, 
                          ESMC_Logical *contig, ESMC_Logical *dealloc);
    int ESMC_LocalArrayGetInfo(struct c_F90ptr *fptr, void *base, int *counts, 
                          int *lbounds, int *ubounds, int *offsets);

    // Restructuring methods and higher level functions.  If we accumulate
    // enough of these they should be moved to another source file.

    // create a new array from an old one, decreasing the rank by one.
    ESMC_LocalArray *ESMC_LocalArraySlice(int slicedim, int sliceloc, 
                                          int *rc) const;
 
    // point to the same data but create a different F90 pointer with
    // different rank/sizes - the number of items must match.
    ESMC_LocalArray *ESMC_LocalArrayReshape(int rank, int *newcounts, 
                                            int *rc) const; 
  
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
ESMC_LocalArray *ESMC_LocalArrayCreate(int rank,    
  ESMC_TypeKind dk, int *counts = NULL, void *base = NULL, 
  ESMC_DataCopy docopy = ESMC_DATA_REF, char *name = NULL, int *rc = NULL);
ESMC_LocalArray *ESMC_LocalArrayCreate(int rank,     
  ESMC_TypeKind dk, int *counts, int *lbounds, int *ubounds, 
  void *base = NULL, 
  ESMC_DataCopy docopy = ESMC_DATA_REF, char *name = NULL, int *rc = NULL);
int ESMC_LocalArrayDestroy(ESMC_LocalArray *array);
ESMC_LocalArray *ESMC_LocalArrayCreate_F(int rank, ESMC_TypeKind dk, 
                    int *icounts = NULL, struct c_F90ptr *f90ptr = NULL, 
                    void *base = NULL, 
                    ESMC_DataCopy docopy = ESMC_DATA_REF, char *name = NULL,
                    int *lbounds = NULL, int *ubounds = NULL, 
                    int *offsets = NULL, int *rc = NULL);
ESMC_LocalArray *ESMC_LocalArrayCreateNoData(int rank,  
                                   ESMC_TypeKind dk, ESMC_ArrayOrigin oflag,
                                   char *name = NULL, int *rc = NULL);

// private static data - address of fortran callback funcs
extern "C" {
 void FTN(c_esmc_localarrayserialize)(ESMC_LocalArray**, char *, 
                                      int *, int *, int *);
 void FTN(c_esmc_localarrayserializenodata)(ESMC_LocalArray**, char *, 
                                            int *, int *, int *);
 void FTN(c_esmc_localarraydeserialize)(ESMC_LocalArray**, char *, 
                                        int *, int *);
 void FTN(c_esmc_localarraydeserializenodata)(ESMC_LocalArray**, char *, 
                                              int *, int *);
 void FTN(c_esmc_f90ptrsizeget)(char *, char *, int *, int *);
 void FTN(c_esmc_f90ptrsizeprint)(char *, char *, int *, int *);
}


 #endif  // ESMC_LocalArray_H
