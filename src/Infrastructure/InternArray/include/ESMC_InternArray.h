// $Id: ESMC_InternArray.h,v 1.9.2.3 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Array C++ declaration include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMC_InternArray_H
#define ESMC_InternArray_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_InternArray - Distributed Data associated with a regular igrid
//
// !DESCRIPTION:
//
// The code in this file defines the C++ InternArray members and declares method // signatures (prototypes).  The companion file ESMC\_InternArray.C contains
// the definitions (full code bodies) for the InternArray methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
#include <ESMCI_DELayout.h>    // communications code
#include <ESMC_LocalArray.h>  // functions to interoperate F90/C++ arrays

// !PUBLIC TYPES:
 class ESMC_InternArray;


// !PRIVATE TYPES:

// class declaration type
class ESMC_InternArray : public ESMC_LocalArray { // inherit from LocalArray cls

   private:

    struct ESMC_AxisIndex ai_alloc[ESMF_MAXDIM]; // allocated space
    struct ESMC_AxisIndex ai_total[ESMF_MAXDIM]; // limits for whole array
    struct ESMC_AxisIndex ai_comp[ESMF_MAXDIM];  // for computational region
    struct ESMC_AxisIndex ai_excl[ESMF_MAXDIM];  // data never sent or received
    int hwidth[ESMF_MAXDIM][2];            // lower/upper halo widths / rank
    int awidth[ESMF_MAXDIM][2];            // lower/upper alloc widths / rank
    
// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_ArrayConstruct(int irank, 
            ESMC_TypeKind dk, int *counts, void *base, 
            ESMC_ArrayOrigin oflag, struct c_F90ptr *f90ptr, 
            ESMC_ArrayDoAllocate aflag, 
            ESMC_DataCopy docopy, ESMC_Logical dflag, 
            int *lbounds, int *ubounds, int *offsets,
            int halo_widths);
    int ESMC_ArrayDestruct(void);

 // optional index values for subsetting and handling arrays standalone
    int ESMC_ArrayGetAxisIndex(ESMC_DomainType dt, 
                               struct ESMC_AxisIndex *index) const;
    int ESMC_ArraySetAxisIndex(ESMC_DomainType dt, 
                               struct ESMC_AxisIndex *index);

// obsolete?
//    int ESMC_ArrayComputeAxisIndex(struct ESMCI::DELayout *delayout, 
//                                   int *decompids, int dlen);

// not at right level; should move to arraycomm
// (still used by arb distribution code for now; but remove it asap)

    int ESMC_ArrayGetAllAxisIndices(struct ESMC_AxisIndex *global, int nDEs,
             int rank, struct ESMC_AxisIndex *total,
             struct ESMC_AxisIndex *comp, struct ESMC_AxisIndex *excl) const; 

    
 // accessor methods for class members
    //int ESMC_ArrayGet<Value>(<value type> *value) const;
    //int ESMC_ArraySet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_ArrayDeserialize(char *buffer, int *offset);
    int ESMC_ArrayDeserializeNoData(char *buffer, int *offset);
    int ESMC_ArrayPrint(const char *options = NULL) const;
    int ESMC_ArraySerialize(char *buffer, int *length, int *offset) const;
    int ESMC_ArraySerializeNoData(char *buffer, int *length, int *offset) const;
    int ESMC_ArrayValidate(const char *options) const;
    int ESMC_ArrayWrite(const char *options, const char *filename) const;

 // native C++ constructors/destructors
	ESMC_InternArray(void);
	~ESMC_InternArray(void);
  
 // get/set methods for internal data
    int ESMC_ArraySetRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int ESMC_ArrayGetRank(void) { return this->rank; }

    int ESMC_ArraySetTypeKind(ESMC_TypeKind kind) { this->kind = kind; 
                                                     return ESMF_SUCCESS;}
    ESMC_TypeKind ESMC_ArrayGetTypeKind(void) { return this->kind; }

    int ESMC_ArrayGetHWidth(int *hw) { *hw = this->hwidth[0][0]; 
                                                     return ESMF_SUCCESS; }
    int ESMC_ArrayGetHWidthList(int *hw) { int *ip = hw;
                                           for (int i = 0; i < rank; i++) 
                                           {  *ip++ = this->hwidth[i][0]; 
                                              *ip++ = this->hwidth[i][1]; }
                                           return ESMF_SUCCESS;}

    int ESMC_ArraySetLengths(int n, int *l) { for (int i = 0; i < n; i++)
                                                  this->counts[i] = l[i]; 
                                              return ESMF_SUCCESS;}
    int ESMC_ArraySetLengths(int ni, int nj=0, int nk=0, int nl=0, int nm=0) { 
           this->counts[0] = ni; this->counts[1] = nj; 
           this->counts[2] = nk; this->counts[3] = nl; 
           this->counts[4] = nm; return ESMF_SUCCESS;}
    int ESMC_ArrayGetLengths(int n, int *l) { for (int i = 0; i < n; i++)
                                                   l[i] = this->counts[i]; 
                                              return ESMF_SUCCESS;}
    int ESMC_ArrayGetLengths(int *ni, int *nj=NULL, int *nk=NULL, 
                                int *nl=NULL, int *nm=NULL) { 
           *ni = this->counts[0]; 
           if (nj) *nj = this->counts[1]; 
           if (nk) *nk = this->counts[2]; 
           if (nl) *nl = this->counts[3]; 
           if (nm) *nm = this->counts[4]; return ESMF_SUCCESS;}

    int ESMC_ArraySetBaseAddr(void *base_addr) { this->base_addr = base_addr; 
                                                 return ESMF_SUCCESS;}
    int ESMC_ArrayGetBaseAddr(void **base) { *base = this->base_addr; 
                                            return ESMF_SUCCESS;}

    int ESMC_ArraySetOrigin(ESMC_ArrayOrigin o) { this->origin = o; 
                                                       return ESMF_SUCCESS;}
    ESMC_ArrayOrigin ESMC_ArrayGetOrigin(void) { return this->origin; }

    int ESMC_ArrayGetLbounds(int n, int *l) { for (int i = 0; i < n; i++)
                                                  l[i] = this->lbound[i]; 
                                              return ESMF_SUCCESS;}
    int ESMC_ArrayGetUbounds(int n, int *u) { for (int i = 0; i < n; i++)
                                                  u[i] = this->ubound[i]; 
                                              return ESMF_SUCCESS;}

    // copy the contents of an f90 ptr
    int ESMC_ArraySetF90Ptr(const struct c_F90ptr *p);
    int ESMC_ArrayGetF90Ptr(struct c_F90ptr *p) const;

    // set/get the dealloc flag
    int ESMC_ArraySetNoDealloc(void) { this->needs_dealloc = ESMF_FALSE; 
                                       return ESMF_SUCCESS;}
    int ESMC_ArraySetDealloc(void)   { this->needs_dealloc = ESMF_TRUE; 
                                       return ESMF_SUCCESS;}
    int ESMC_ArrayNeedsDealloc(void)  { 
                         return this->needs_dealloc == ESMF_TRUE ? 1 : 0; }

    // get and set useful combinations of values that fortran cares about
    int ESMC_ArraySetInfo(struct c_F90ptr *fptr, void *base, int *counts, 
                          int *lbounds, int *ubounds, int *offsets, 
                          ESMC_Logical contig, ESMC_Logical dealloc,
                          int halo_width);
    int ESMC_ArraySetName(char *name) { return ESMC_BaseSetName(name,
      "InternArray"); }
    char *ESMC_ArrayGetName(void) { return ESMC_BaseGetName(); }

    // most important array methods
    int ESMC_ArrayRedist(ESMCI::DELayout *delayout, int global_start[], 
                         int global_dimlengths[], int rank_trans[], 
                         int size_rank_trans, int olddecompids[], 
                         int decompids[], int size_decomp,
                          ESMC_InternArray *RedistArray);
    int ESMC_ArrayHalo(ESMCI::DELayout *delayout,
                       ESMC_AxisIndex *ai_global, int global_dimlengths[],
                       int decompids[], int size_decomp, ESMC_Logical periodic[]);
    int ESMC_ArrayGather(ESMCI::DELayout *delayout, int decompids[], 
                         int size_decomp, int local_axislengths[],
                         int global_dimlengths[], int local_maxlength[],
                         int deid, ESMC_InternArray **Array_out);
    int ESMC_ArrayScatter(ESMCI::DELayout *delayout,
                          int decompids[], int size_decomp,
                          int deid, ESMC_InternArray **Array_out);
    
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_InternArray


// these are functions, but not class methods.
ESMC_InternArray *ESMC_InternArrayCreate(int rank, ESMC_TypeKind dk, 
                    int *counts = NULL, void *base = NULL, 
                    ESMC_DataCopy docopy = ESMC_DATA_REF,
                    int *rc = NULL);
ESMC_InternArray *ESMC_InternArrayCreate(int rank, ESMC_TypeKind dk, 
                    int *counts, void *base, 
                    ESMC_DataCopy docopy,
                    int halo_widths, int *rc);
int ESMC_InternArrayDestroy(ESMC_InternArray *array);
ESMC_InternArray *ESMC_InternArrayCreate_F(int rank, ESMC_TypeKind dk, 
                    int *icounts = NULL, struct c_F90ptr *f90ptr = NULL, 
                    void *base = NULL, 
                    ESMC_DataCopy docopy = ESMC_DATA_REF,
                    int *lbounds = NULL, int *ubounds = NULL, 
                    int *offsets = NULL, 
                    int halo_widths = 0, int *rc = NULL);
ESMC_InternArray *ESMC_InternArrayCreateNoData(int rank, 
                                   ESMC_TypeKind dk, ESMC_ArrayOrigin oflag,
                                   int *rc = NULL);


// fortran accessible functions.
#ifdef ESMC_DATA_ADDR_NEEDS_INDIR
#define XD *
#else
#define XD
#endif

extern "C" {
  void FTN(f_esmf_arrayf90allocate)(ESMC_InternArray**, int *,
                                    ESMC_TypeKind*, int*, int*, 
                                    int *, int *, int *);
  void FTN(f_esmf_arrayf90deallocate)(ESMC_InternArray**, int*,
                                      ESMC_TypeKind *, int*);

  void FTN(c_esmc_arraycreateall)(ESMC_InternArray **ptr, int *rank, 
                                  ESMC_TypeKind *dk,
                                  int *counts, int *lbounds, int *ubounds,
                                  int *status);
  void FTN(c_esmc_arraycreatenodata)(ESMC_InternArray **ptr, int *rank, 
                                     ESMC_TypeKind *dk, 
                                     ESMC_ArrayOrigin *oflag, int *status);
  void FTN(c_esmc_arraysetinfo)(ESMC_InternArray **ptr, 
                            struct c_F90ptr *fptr, void XD *base, int *counts,
                            int *lbounds, int *ubounds, int *offsets,
                            ESMC_Logical *contig, ESMC_Logical *dealloc,
			    int *hwidth, int *status);
  void FTN(c_esmc_arraysetlengths)(ESMC_InternArray **ptr, int *rank, int *lengths, int *status);
  void FTN(c_esmc_arraygetlengths)(ESMC_InternArray **ptr, int *rank, int *lengths, int *status);
  void FTN(c_esmc_arraygetlbounds)(ESMC_InternArray **ptr, int *rank, int *lbounds, int *status);
  void FTN(c_esmc_arraygetubounds)(ESMC_InternArray **ptr, int *rank, int *ubounds, int *status);
  void FTN(c_esmc_arraygethwidth)(ESMC_InternArray **ptr, int *hwidth, int *status);
  void FTN(c_esmc_arraygethwidthlist)(ESMC_InternArray **ptr, int *hwidth, int *status);
  void FTN(c_esmc_arraygetrank)(ESMC_InternArray **ptr, int *rank, int *status);
  void FTN(c_esmc_arraygettypekind)(ESMC_InternArray **ptr, int *kind, int *status);
  void FTN(c_esmc_arraygetname)(ESMC_InternArray **ptr, char *name, int *status, int nlen);
  void FTN(c_esmc_arraydestroy)(ESMC_InternArray **ptr, int *status);
  void FTN(c_esmc_arraysetaxisindex)(ESMC_InternArray **ptr, ESMC_DomainType *dt, 
                                     ESMC_AxisIndex *ai, int *status);
  void FTN(c_esmc_arraygetaxisindex)(ESMC_InternArray **ptr, ESMC_DomainType *dt, 
                                     ESMC_AxisIndex *ai, int *status);
  void FTN(c_esmc_arraygetallaxisindices)(ESMC_InternArray **ptr, 
                                  ESMC_AxisIndex *global, int *nDEs,
                                  int *rank, ESMC_AxisIndex *total,
                                  ESMC_AxisIndex *comp, ESMC_AxisIndex *excl,
                                  int *status);
  void FTN(c_esmc_arraygetallaxisindex)(ESMC_InternArray **ptr, ESMC_DomainType *dt, 
                                  ESMC_AxisIndex *global, int *nDEs,
                                  int *rank, ESMC_AxisIndex *ai,
                                  int *status);
  void FTN(c_esmc_arraysetbaseaddr)(ESMC_InternArray **ptr, void XD *base, int *status);
  void FTN(c_esmc_arraygetbaseaddr)(ESMC_InternArray **ptr, void **base, int *status);
  void FTN(c_esmc_arraysetf90ptr)(ESMC_InternArray **ptr, struct c_F90ptr *p, int *status);
  void FTN(c_esmc_arraygetf90ptr)(ESMC_InternArray **ptr, struct c_F90ptr *p, int *status);
  void FTN(c_esmc_arraysetdealloc)(ESMC_InternArray **ptr, int *status);
  void FTN(c_esmc_arraysetnodealloc)(ESMC_InternArray **ptr, int *status);
  void FTN(c_esmc_arrayneedsdealloc)(ESMC_InternArray **ptr, int flag, int *status);
  void FTN(c_esmc_arrayprint)(ESMC_InternArray **ptr, char *opts, int *status, int clen);
  void FTN(c_esmc_arraywrite)(ESMC_InternArray **ptr, char *opts, char *fname,
                              int *status, int optlen, int flen);
   void FTN(c_esmc_arrayserialize)(ESMC_InternArray **array, char *buf,
                                   int *length, int *offset, int *rc);
   void FTN(c_esmc_arraydeserialize)(ESMC_InternArray **array, char *buf,
                                     int *offset, int *rc);
   void FTN(c_esmc_arrayserializenodata)(ESMC_InternArray **array, char *buf,
                                         int *length, int *offset, int *rc);
   void FTN(c_esmc_arraydeserializenodata)(ESMC_InternArray **array, char *buf, 
                                           int *offset, int *rc);

}


 #endif  // ESMC_InternArray_H

