// $Id: ESMC_Array.h,v 1.32 2004/06/03 12:17:38 nscollins Exp $
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

 #ifndef ESMC_Array_H
 #define ESMC_Array_H

//-----------------------------------------------------------------------------

#include <string.h>
//#include <stdio.h>  // include for debug only

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Array - Distributed Data associated with a regular grid
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Array members and declares method 
// signatures (prototypes).  The companion file ESMC\_Array.C contains
// the definitions (full code bodies) for the Array methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
#include <ESMC_DELayout.h>    // communications code
#include <ESMC_LocalArray.h>  // functions to interoperate F90/C++ arrays

// !PUBLIC TYPES:
 class ESMC_Array;


// !PRIVATE TYPES:

// private static data - address of fortran callback funcs
extern "C" {
 void FTN(f_esmf_arrayf90allocate)(ESMC_Array**, int *, ESMC_DataType*,
                                   ESMC_DataKind*, int*, int*, 
                                   int *, int *, int *);
 void FTN(f_esmf_arrayf90deallocate)(ESMC_Array**, int*, ESMC_DataType*,
                                     ESMC_DataKind *, int*);
}


// class declaration type
class ESMC_Array : public ESMC_LocalArray {  // inherits from LocalArray class

   private:

    struct ESMC_AxisIndex ai_total[ESMF_MAXDIM]; // limits for whole array
    struct ESMC_AxisIndex ai_comp[ESMF_MAXDIM];  // for computational region
    struct ESMC_AxisIndex ai_excl[ESMF_MAXDIM];  // never is sent or received
    int hwidth[ESMF_MAXGRIDDIM][2];              // lower and upper halo widths
    
// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    int ESMC_ArrayConstruct(int irank, ESMC_DataType dt, 
            ESMC_DataKind dk, int *counts, void *base, 
            ESMC_ArrayOrigin oflag, struct c_F90ptr *f90ptr, 
            ESMC_ArrayDoAllocate aflag, 
            ESMC_DataCopy docopy, ESMC_Logical dflag, 
            int *lbounds, int *ubounds, int *offsets,
            int halo_widths);
    int ESMC_ArrayDestruct(void);

 // optional index values for subsetting and handling arrays standalone
    int ESMC_ArrayGetAxisIndex(ESMC_DomainType dt, struct ESMC_AxisIndex *index) const;
    int ESMC_ArraySetAxisIndex(ESMC_DomainType dt, struct ESMC_AxisIndex *index);
    int ESMC_ArrayComputeAxisIndex(struct ESMC_DELayout *delayout, int *decompids,
                 int dlen);
    int ESMC_ArrayGetAllAxisIndices(struct ESMC_AxisIndex *global, int nDEs,
                 int rank, struct ESMC_AxisIndex *total,
                 struct ESMC_AxisIndex *comp, struct ESMC_AxisIndex *excl) const; 

    
 // accessor methods for class members
    //int ESMC_ArrayGet<Value>(<value type> *value) const;
    //int ESMC_ArraySet<Value>(<value type>  value);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_ArrayWrite(const char *options, const char *filename) const;
    int ESMC_ArrayValidate(const char *options) const;
    int ESMC_ArrayPrint(const char *options = NULL) const;

 // native C++ constructors/destructors
	ESMC_Array(void);
	~ESMC_Array(void);
  
 // get/set methods for internal data
    int ESMC_ArraySetRank(int rank) { this->rank = rank; return ESMF_SUCCESS;}
    int ESMC_ArrayGetRank(void) { return this->rank; }

    int ESMC_ArraySetType(ESMC_DataType type) { this->type = type; 
                                                     return ESMF_SUCCESS;}
    ESMC_DataType ESMC_ArrayGetType(void) { return this->type; }

    int ESMC_ArraySetKind(ESMC_DataKind kind) { this->kind = kind; 
                                                     return ESMF_SUCCESS;}
    ESMC_DataKind ESMC_ArrayGetKind(void) { return this->kind; }

    int ESMC_ArrayGetHWidth(int *hw) { *hw = this->hwidth[0][0]; 
                                                     return ESMF_SUCCESS; }

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
    int ESMC_ArraySetName(char *name) { return ESMC_BaseSetName(name, "Array"); }
    char *ESMC_ArrayGetName(void) { return ESMC_BaseGetName(); }

    // most important array methods
    int ESMC_ArrayRedist(ESMC_DELayout *delayout, int global_start[], 
                         int global_dimlengths[], int rank_trans[], 
                         int size_rank_trans, int olddecompids[], 
                         int decompids[], int size_decomp,
                          ESMC_Array *RedistArray);
    int ESMC_ArrayHalo(ESMC_DELayout *delayout,
                       ESMC_AxisIndex *ai_global, int global_dimlengths[],
                       int decompids[], int size_decomp, ESMC_Logical periodic[]);
    int ESMC_ArrayGather(ESMC_DELayout *delayout, int decompids[], 
                            int size_decomp, int global_dimlengths[],
                            int local_maxlength[], int deid, ESMC_Array **Array_out);
    int ESMC_ArrayScatter(ESMC_DELayout *delayout,
                            int decompids[], int size_decomp,
                            int deid, ESMC_Array **Array_out);
    
 // < declare the rest of the public interface methods here >
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//
 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_Array


// these are functions, but not class methods.
ESMC_Array *ESMC_ArrayCreate(int rank, ESMC_DataType dt, ESMC_DataKind dk, 
                    int *counts = NULL, void *base = NULL, 
                    ESMC_DataCopy docopy = ESMC_DATA_REF,
                    int *rc = NULL);
ESMC_Array *ESMC_ArrayCreate(int rank, ESMC_DataType dt, ESMC_DataKind dk, 
                    int *counts, void *base, 
                    ESMC_DataCopy docopy,
                    int halo_widths, int *rc);
int ESMC_ArrayDestroy(ESMC_Array *array);
ESMC_Array *ESMC_ArrayCreate_F(int rank, ESMC_DataType dt, ESMC_DataKind dk, 
                    int *icounts = NULL, struct c_F90ptr *f90ptr = NULL, 
                    void *base = NULL, 
                    ESMC_DataCopy docopy = ESMC_DATA_REF,
                    int *lbounds = NULL, int *ubounds = NULL, 
                    int *offsets = NULL, 
                    int halo_widths = 0, int *rc = NULL);
ESMC_Array *ESMC_ArrayCreateNoData(int rank, ESMC_DataType dt, 
                                   ESMC_DataKind dk, ESMC_ArrayOrigin oflag,
                                   int *rc = NULL);


 #endif  // ESMC_Array_H

