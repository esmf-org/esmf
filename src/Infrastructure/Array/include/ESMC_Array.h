// $Id: ESMC_Array.h,v 1.82 2007/11/02 19:22:03 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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

#ifndef ESMC_Array_H
#define ESMC_Array_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::Array - Array
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Array} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_Array.C}
// contains the full code (bodies) for the {\tt Array} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMF_Pthread.h"

#include "ESMC_Base.h"      // Base is superclass to Array
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"
#include "ESMC_DistGrid.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_LocalArray.h"
#include "ESMC_RHandle.h"

//-------------------------------------------------------------------------

namespace ESMCI {

// classes and structs

class Array;
struct SeqIndex;

// class definition
class Array : public ESMC_Base {    // inherits from ESMC_Base class
  
  private:
    // global information
    ESMC_TypeKind typekind;
    int rank;
    ESMC_IndexFlag indexflag;
    //todo: the LocalArray pointers should be shared between PETs in the same
    //todo: VAS as to allow shared memory operations. Even the LocalArray
    //todo: pointers for Arrays instances on other VAS' may be good to keep
    //todo: in order to allow one-sided access to their data...
    // for now only keep PET-local LocalArray pointers here
    // PET-local information
    ESMC_LocalArray **larrayList;     // [localDeCount]
    void **larrayBaseAddrList;        // [localDeCount]
    int *exclusiveLBound;             // [dimCount*localDeCount]
    int *exclusiveUBound;             // [dimCount*localDeCount]
    int *computationalLBound;         // [dimCount*localDeCount]
    int *computationalUBound;         // [dimCount*localDeCount]
    int *totalLBound;                 // [dimCount*localDeCount]
    int *totalUBound;                 // [dimCount*localDeCount]
    int tensorCount;                  // number of tensor dimensions
    int tensorElementCount;           // number of tensor elements per excl. ele
    int *undistLBound;                // [tensorCount]
    int *undistUBound;                // [tensorCount]
    int *staggerLoc;                  // [tensorElementCount]
    int *vectorDim;                   // [tensorElementCount]
    int *dimmap;                      // [dimCount]
    int *inverseDimmap;               // [rank]
    int *contiguousFlag;              // [localDeCount]
    int *deElementCount;              // [deCount] number of elements in
                                      // exclusive region only considering
                                      // DistGrid dims that are associated with
                                      // the Array dims. 
                                      // Multiply with tensorElementCount to get
                                      // total number of exclusive elements in
                                      // Array.
    // lower level object references
    DistGrid *distgrid;
    DELayout *delayout;
    
  public:
    // constructor and destructor
    Array(){
      typekind = ESMF_NOKIND;
      rank = 0;
      indexflag = ESMF_INDEX_DELOCAL;
      larrayList = NULL;
      larrayBaseAddrList = NULL;
      exclusiveLBound = NULL;
      exclusiveUBound = NULL;
      computationalLBound = NULL;
      computationalUBound = NULL;
      totalLBound = NULL;
      totalUBound = NULL;
      tensorCount = 0;
      undistLBound = NULL;
      undistUBound = NULL;
      staggerLoc = NULL;
      vectorDim = NULL;
      dimmap = NULL;
      inverseDimmap = NULL;
      contiguousFlag = NULL;
      tensorElementCount = 0;
      deElementCount = NULL;
    }
  private:
    Array(ESMC_TypeKind typekind, int rank, ESMC_LocalArray **larrayList,
      DistGrid *distgrid, int *exclusiveLBound, int *exclusiveUBound, 
      int *computationalLBound, int *computationalUBound, int *totalLBound,
      int *totalUBound, int tensorCount, int tensorElementCount,
      int *undistLBoundArray, int *undistUBoundArray, int *staggerLoc,
      int *vectorDim, int *dimmapArray, int *inverseDimmapArray,
      ESMC_IndexFlag indexflagArg, int *rc);
  public:
    ~Array();
    // create() and destroy()
    static Array *create(ESMC_LocalArray **larrayList, int larrayCount,
      DistGrid *distgrid, InterfaceInt *dimmap,
      InterfaceInt *computationalEdgeLWidthArg,
      InterfaceInt *computationalEdgeUWidthArg,
      InterfaceInt *computationalLWidthArg,
      InterfaceInt *computationalUWidthArg,
      InterfaceInt *totalLWidthArg, InterfaceInt *totalUWidthArg,
      ESMC_IndexFlag *indexflag, int *staggerLoc, int *vectorDim,
      InterfaceInt *undistLBoundArg, InterfaceInt *undistUBoundArg, int *rc);
    static Array *create(ESMC_ArraySpec *arrayspec, DistGrid *distgrid,
      InterfaceInt *dimmap, InterfaceInt *computationalEdgeLWidthArg,
      InterfaceInt *computationalEdgeUWidthArg,
      InterfaceInt *computationalLWidthArg, 
      InterfaceInt *computationalUWidthArg, InterfaceInt *totalLWidthArg,
      InterfaceInt *totalUWidthArg, ESMC_IndexFlag *indexflag,
      int *staggerLoc, int *vectorDim, InterfaceInt *undistLBoundArg,
      InterfaceInt *undistUBoundArg, int *rc);
    static int destroy(Array **array);
    // get() and set()
    ESMC_TypeKind getTypekind()             const {return typekind;}
    int getRank()                           const {return rank;}
    ESMC_IndexFlag getIndexflag()           const {return indexflag;}
    ESMC_LocalArray **getLocalarrayList()   const {return larrayList;}
    const int *getExclusiveLBound()         const {return exclusiveLBound;}
    const int *getExclusiveUBound()         const {return exclusiveUBound;}
    const int *getComputationalLBound()     const {return computationalLBound;}
    const int *getComputationalUBound()     const {return computationalUBound;}
    const int *getTotalLBound()             const {return totalLBound;}
    const int *getTotalUBound()             const {return totalUBound;}
    int getTensorCount()                    const {return tensorCount;}
    const int *getUndistLBound()            const {return undistLBound;}
    const int *getUndistUBound()            const {return undistUBound;}
    const int *getStaggerLoc()              const {return staggerLoc;}
    const int *getVectorDim()               const {return vectorDim;}
    const int *getDeElementCount()          const {return deElementCount;}
    int getTensorElementCount()             const {return tensorElementCount;}
    const int *getDimmap()                  const {return dimmap;}
    const int *getInverseDimmap()           const {return inverseDimmap;}
    DistGrid *getDistGrid()                 const {return distgrid;}
    DELayout *getDELayout()                 const {return delayout;}
    int getLinearIndexExclusive(int localDe, int *index, int *rc=NULL) const;
    SeqIndex getSequenceIndexExclusive(int localDe, int *index,
      int *rc=NULL) const;
    void setStaggerLoc(int staggerLocArg){
      for (int i=0; i<tensorElementCount; i++) staggerLoc[i] = staggerLocArg;
    }
    void setStaggerLoc(int staggerLocArg, int tensorIndex){
      staggerLoc[tensorIndex] = staggerLocArg;
    }
    void setVectorDim(int vectorDimArg){
      for (int i=0; i<tensorElementCount; i++) vectorDim[i] = vectorDimArg;
    }
    void setVectorDim(int vectorDimArg, int tensorIndex){
      vectorDim[tensorIndex] = vectorDimArg;
    }
    int setComputationalLWidth(InterfaceInt *computationalLWidthArg);
    int setComputationalUWidth(InterfaceInt *computationalUWidthArg);
    const char *getName()               const {return ESMC_BaseGetName();}
    int setName(char *name){return ESMC_BaseSetName(name, "Array");}
    // misc.
    int print() const;
    int validate() const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset) const;
    int deserialize(char *buffer, int *offset);
    // comms
    int gather(void *array, ESMC_TypeKind typekind, int rank,
      int *counts, int *patch, int rootPet, VM *vm);
    int scatter(void *array, ESMC_TypeKind typekind, int rank,
      int *counts, int *patch, int rootPet, VM *vm);
    static int redistStore(Array *srcArray, Array *dstArray,
      ESMC_RouteHandle **routehandle);
    static int redist(Array *srcArray, Array *dstArray,
      ESMC_RouteHandle **routehandle, ESMC_Logical checkflag=ESMF_FALSE);
    static int redistRelease(ESMC_RouteHandle *routehandle);
    static int sparseMatMulStore(Array *srcArray, Array *dstArray,
      ESMC_RouteHandle **routehandle,
      ESMC_TypeKind typekind = ESMF_NOKIND, void *factorList = NULL,
      int factorListCount = 0, InterfaceInt *factorIndexList = NULL);
    static int sparseMatMul(Array *srcArray, Array *dstArray,
      ESMC_RouteHandle **routehandle, ESMC_Logical zeroflag=ESMF_TRUE,
      ESMC_Logical checkflag=ESMF_FALSE);
    static int sparseMatMulRelease(ESMC_RouteHandle *routehandle);
    
};  // class Array

struct SeqIndex{
  int decompSeqIndex;
  int tensorSeqIndex;
  static int cmp(const void *a, const void *b){
    SeqIndex *aObj = (SeqIndex *)a;
    SeqIndex *bObj = (SeqIndex *)b;
    if (aObj->decompSeqIndex < bObj->decompSeqIndex) return -1;
    if (aObj->decompSeqIndex > bObj->decompSeqIndex) return +1;
    // decompSeqIndex must be equal
    if (aObj->tensorSeqIndex < bObj->tensorSeqIndex) return -1;
    if (aObj->tensorSeqIndex > bObj->tensorSeqIndex) return +1;
    // tensorSeqIndex must be equal
    return 0;
  }
};  // struct seqIndex

} // namespace ESMCI



//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
// The following code is for a first newArray prototype which I used
// to check out some communication ideas: DE-nonblocking paradigm!
//-------------------------------------------------------------------------


#ifdef FIRSTNEWARRAYPROTOTYPE


class ESMC_newArray;

typedef struct{
  int commhandleCount;
  vmk_commhandle **vmk_commh;
  int pthidCount;
  pthread_t pthid[10];
  void *buffer;
}ESMC_newArrayCommHandle;

typedef struct{
  ESMC_newArray *array;     // pointer to calling ESMC_newArray object
  ESMCI::VM *vm;              // pointer to current VM
  int de;                   // DE for DE-based non-blocking operation
  int rootPET;              // root
  void *result;             // result memory location
  ESMC_TypeKind dtk;        // data type kind
  ESMC_Operation op;        // operation flag
}ESMC_newArrayThreadArg;


// class definition
class ESMC_newArray : public ESMC_Base {    // inherits from ESMC_Base class
  private:
    int rank;                 // rank of newArray
    ESMC_TypeKind kind;       // kind of newArray (for F90)
    int **globalDataLBound;   // dataBox for this DE [de][dim]
    int **globalDataUBound;   // dataBox for this DE [de][dim]
    int **localFullLBound;    // fullBox (data + halo) for this DE [de][dim]
    int **localFullUBound;    // fullBox (data + halo) for this DE [de][dim]
    int **globalFullLBound;   // fullBox (data + halo) for this DE [de][dim]
    int **globalFullUBound;   // fullBox (data + halo) for this DE [de][dim]
    int **dataOffset;         // offset dataBox vs. fullBox for DE [de][dim]
    ESMCI::DELayout *delayout;// DELayout on which newArray is defined
    ESMC_LocalArray **localArrays;  // array of LocalArray pointers [localDe]
    ESMC_newArrayCommHandle *commhArray;  // array of commhandles [localDe]
    ESMC_newArrayThreadArg *thargArray;   // array of thread args [localDe]
    ESMC_newArrayThreadArg thargRoot;     // root's thread args
    
    // cached VM information
    int localVAS;             // VAS in which localPET operates
                      // don't cache localPET because that might change
    // cached DELayout information
    int deCount;              // total number of DEs
    int localDeCount;         // number of DEs that map onto localVAS
    int *localDeList;         // list of local DEs
    int *deVASList;           // list of VASs for all DEs
    
  public:
    // Construct and Destruct
    int ESMC_newArrayConstruct(  
      ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
      int *haloWidth,           // halo width
      ESMCI::DELayout *delayout,  // DELayout
      int rootPET,              // root
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayDestruct(void);

    // Get info
    int ESMC_newArrayGet(int *rank, ESMCI::DELayout **delayout, 
      ESMC_LocalArray **localArrays, int len_localArrays,
      int *globalFullLBound, int *len_globalFullLBound,
      int *globalFullUBound, int *len_globalFullUBound,
      int *globalDataLBound, int *len_globalDataLBound,
      int *globalDataUBound, int *len_globalDataUBound,
      int *localDataLBound, int *len_localDataLBound,
      int *localDataUBound, int *len_localDataUBound);
   
    // IO and validation
    int ESMC_newArrayPrint(void);
    
    // Communication
    int ESMC_newArrayScatter(
      ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
      int rootPET,              // root
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things
    
    int ESMC_newArrayScatter(
      ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
      int rootPET,              // root
      ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayScatter(
      ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
      int rootPET,              // root
      int de,                   // DE for DE-based non-blocking scatter
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayScalarReduce(
      void *result,             // result value (scalar)
      ESMC_TypeKind dtk,        // data type kind
      ESMC_Operation op,        // reduce operation
      int rootPET,              // root
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayScalarReduce(
      void *result,             // result value (scalar)
      ESMC_TypeKind dtk,        // data type kind
      ESMC_Operation op,        // reduce operation
      int rootPET,              // root
      ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayScalarReduce(
      void *result,             // result value (scalar)
      ESMC_TypeKind dtk,        // data type kind
      ESMC_Operation op,        // reduce operation
      int rootPET,              // root
      int de,                   // DE for DE-based non-blocking reduce
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayWait(
      int rootPET,              // root
      ESMC_newArrayCommHandle *commh, // commu handle specifying non-block op.
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayWait(
      int de,                   // DE for which to wait
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things
    
    // friend functions that provide thread support for non-blocking comms
    friend void *ESMC_newArrayScatterThread(void *);
    friend void *ESMC_newArrayScalarReduceThread(void *);

};  // end class ESMC_newArray

// external methods:  

ESMC_newArray *ESMC_newArrayCreate(ESMC_LocalArray *larray, int *haloWidth, 
  int deCount, int rootPET, int *rc);

int ESMC_newArrayDestroy(ESMC_newArray **array);

#endif


#endif  // ESMC_Array_H

