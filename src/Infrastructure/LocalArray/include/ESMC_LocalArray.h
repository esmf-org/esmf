// $Id: ESMC_LocalArray.h,v 1.21.2.7 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_LocalArray_H
#define ESMC_LocalArray_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_LocalArray - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines the C++ LocalArray class and declares method
// signatures (prototypes).  The companion file ESMC\_LocalArray.C contains
// the definitions (full code bodies) for the LocalArray methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_Base.h"  // all classes inherit from the ESMC Base class.

//-------------------------------------------------------------------------

// classes and structs

class ESMC_LocalArray;

#define MAX_F90_RANK_POSSIBLE 7
struct c_F90ptr {
  // Dummy structure which is at least as big as an F90 pointer on
  // each architcture. ESMF_F90_PTR_xxx are defined in conf.h in
  // the build_config directories for each architecture.
  // On most platforms, there is a bump in size for each additional rank.
  // So far no platform we found has a data-type dependency.
   unsigned char basepad[ESMF_F90_PTR_BASE_SIZE];
#if ESMF_F90_PTR_PLUS_RANK 
   // plus extra space needed per rank
   unsigned char extrapad[MAX_F90_RANK_POSSIBLE*ESMF_F90_PTR_PLUS_RANK];
#endif
};

// these must stay in sync with the F90 versions
typedef enum{ 
  ESMC_FROM_FORTRAN = 1, 
  ESMC_FROM_CPLUSPLUS 
}ESMC_ArrayOrigin; 

typedef enum{ 
  ESMC_DATA_COPY = 1, 
  ESMC_DATA_REF,
  ESMC_DATA_DEFER,
  ESMC_DATA_SPACE,
  ESMC_DATA_NONE
}ESMC_DataCopy;

typedef enum{ 
  ESMC_ARRAY_NO_ALLOCATE = 0, 
  ESMC_ARRAY_DO_ALLOCATE,
  ESMC_ARRAY_ALLOC_IF_BASE_NULL,
  ESMF_ARRAY_ALLOC_DEFERRED
}ESMC_ArrayDoAllocate;


// class declaration
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

  public:
    // create() and destroy()
    static ESMC_LocalArray *ESMC_LocalArrayCreate(int rank,
      ESMC_TypeKind dk, int *counts = NULL, void *base = NULL, 
      ESMC_DataCopy docopy = ESMC_DATA_REF, char *name = NULL, int *rc = NULL);
    static ESMC_LocalArray *ESMC_LocalArrayCreate(int rank,     
      ESMC_TypeKind dk, int *counts, int *lbounds, int *ubounds,
      void *base = NULL, ESMC_DataCopy docopy = ESMC_DATA_REF,
      char *name = NULL, int *rc = NULL);
    static ESMC_LocalArray *ESMC_LocalArrayCreate(ESMC_LocalArray *larrayIn,
      int *rc = NULL);
    static int ESMC_LocalArrayDestroy(ESMC_LocalArray *array);

    static ESMC_LocalArray *ESMC_LocalArrayCreateNoData(int rank,  
      ESMC_TypeKind dk, ESMC_ArrayOrigin oflag, char *name = NULL,
      int *rc = NULL);
    static ESMC_LocalArray *ESMC_LocalArrayCreate_F(int rank, ESMC_TypeKind dk,
      int *icounts = NULL, struct c_F90ptr *f90ptr = NULL, void *base = NULL, 
      ESMC_DataCopy docopy = ESMC_DATA_REF, char *name = NULL,
      int *lbounds = NULL, int *ubounds = NULL, int *offsets = NULL,
      int *rc = NULL);
    
  public:
    // construct() and destruct()
    int ESMC_LocalArrayConstruct(int irank, ESMC_TypeKind dk, int *counts,
      void *base, ESMC_ArrayOrigin oflag, struct c_F90ptr *f90ptr,
      ESMC_ArrayDoAllocate aflag, ESMC_DataCopy docopy, ESMC_Logical dflag,
      char *name, int *lbounds, int *ubounds, int *offsets);
    int ESMC_LocalArrayDestruct();

    // adjust()
    ESMC_LocalArray *ESMC_LocalArrayAdjust(ESMC_DataCopy copyflag,
      int *lbounds, int *ubounds, int *rc);

    // required methods inherited and overridden from the ESMC_Base class
    int ESMC_LocalArrayDeserialize(char *buffer, int *boffset);
    int ESMC_LocalArrayDeserializeNoData(char *buffer, int *boffset);
    int ESMC_LocalArrayPrint(const char *options = NULL) const;
    int ESMC_LocalArraySerialize(char *buffer, int *length, int *boffset) const;
    int ESMC_LocalArraySerializeNoData(char *buffer, int *length, int *boffset)
      const;
    int ESMC_LocalArrayWrite(const char *options, const char *filename) const;
    int ESMC_LocalArrayValidate(const char *options) const;

    // native C++ constructors/destructors
    ESMC_LocalArray();
    ~ESMC_LocalArray();
  
    // get/set methods for internal data
    int ESMC_LocalArraySetRank(int rank){
      this->rank = rank; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayGetRank()const{return this->rank;}
    int ESMC_LocalArraySetTypeKind(ESMC_TypeKind kind){
      this->kind = kind;
      return ESMF_SUCCESS;
    }
    ESMC_TypeKind ESMC_LocalArrayGetTypeKind()const{return this->kind;}
    int ESMC_LocalArraySetLengths(int n, int *l){
      for (int i = 0; i < n; i++)
        this->counts[i] = l[i]; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArraySetLengths(int ni,int nj=0,int nk=0,int nl=0,int nm=0){ 
      this->counts[0] = ni; this->counts[1] = nj; 
      this->counts[2] = nk; this->counts[3] = nl; 
      this->counts[4] = nm;
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayGetLengths(int n, int *l)const{
      for (int i = 0; i < n; i++)
        l[i] = this->counts[i]; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayGetLengths(int *ni, int *nj=NULL, int *nk=NULL, 
      int *nl=NULL, int *nm=NULL)const{ 
      *ni = this->counts[0];
      if (nj) *nj = this->counts[1]; 
      if (nk) *nk = this->counts[2];
      if (nl) *nl = this->counts[3]; 
      if (nm) *nm = this->counts[4];
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArraySetBaseAddr(void *base_addr){
      this->base_addr = base_addr; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayGetBaseAddr(void **base)const{
      *base = this->base_addr; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayGetByteCount(int *count)const{
      *count = this->byte_count; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArraySetOrigin(ESMC_ArrayOrigin o){
      this->origin = o; 
      return ESMF_SUCCESS;
    }
    ESMC_ArrayOrigin ESMC_LocalArrayGetOrigin()const{return this->origin;}
    int ESMC_LocalArrayGetLbounds(int n, int *l)const{
      for (int i = 0; i < n; i++)
        l[i] = this->lbound[i];
       return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayGetUbounds(int n, int *u)const{
      for (int i = 0; i < n; i++)
        u[i] = this->ubound[i];
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayGetCounts(int n, int *c)const{
      for (int i = 0; i < n; i++)
        c[i] = this->counts[i];
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArraySetName(char *name){
      return ESMC_BaseSetName(name, "LocalArray");
    }
    char *ESMC_LocalArrayGetName()const{return ESMC_BaseGetName();}
    int ESMC_LocalArraySetNoDealloc(){
      this->needs_dealloc = ESMF_FALSE; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArraySetDealloc(){
      this->needs_dealloc = ESMF_TRUE; 
      return ESMF_SUCCESS;
    }
    int ESMC_LocalArrayNeedsDealloc()const{
      return this->needs_dealloc == ESMF_TRUE ? 1 : 0;
    }
    int ESMC_LocalArraySetInfo(struct c_F90ptr *fptr, void *base, int *counts, 
      int *lbounds, int *ubounds, int *offsets, ESMC_Logical *contig,
      ESMC_Logical *dealloc);
    int ESMC_LocalArrayGetInfo(struct c_F90ptr *fptr, void *base, int *counts, 
      int *lbounds, int *ubounds, int *offsets)const;

    // copy the contents of an f90 ptr
    int ESMC_LocalArraySetF90Ptr(const struct c_F90ptr *p);
    int ESMC_LocalArrayGetF90Ptr(struct c_F90ptr *p) const;
    
    // force the base address in f90 ptr dope vector *if* mismatch detected
    int ESMC_LocalArrayForceF90Ptr(void *base){
      void **f90dopev_cast = (void **)(&f90dopev);
      // The rationale for this call is as follows:
      // - some Fortran compilers will make temporary copies of the actual
      //   data array.
      // - such copies can be detected by comparing the address of the first
      //   array element against the address of the first element of the
      //   original data array, stored in the LocalArray object.
      // - if/when a temporary data copy has been detected this routine tries
      //   to force the base address in the stored f90 ptr dope vector to the
      //   correct base address stored in the LocalArray. The assumption is made
      //   that the base address is that of the first element and is stored 
      //   without offset at the start of the dope vector.
      // - since the details about the f90 ptr dope vector are compiler
      //   dependent the above assumption of where to force the base address
      //   (and with what offset in the address) may turn out to be incorrect.
      //   However, this routine is a last resort in those cases where a 
      //   temporary copy has been detected and it tests out fine on XLF and
      //   lahey.
      if (base != base_addr){
        // Only force base_addr into dope vector if the first element according
        // to dope vector is located somewhere else, i.e. a temporary copy.
        *f90dopev_cast = base_addr;
      }
      return ESMF_SUCCESS;
    }

    // create a new LocalArray from an old one, decreasing the rank by one.
    ESMC_LocalArray *ESMC_LocalArraySlice(int slicedim, int sliceloc, int *rc)
      const;
 
    // point to the same data but create a different F90 pointer with
    // different rank/sizes - the number of items must match.
    ESMC_LocalArray *ESMC_LocalArrayReshape(int rank, int *newcounts, int *rc)
      const;
  
};  // class ESMC_LocalArray

#endif  // ESMC_LocalArray_H
