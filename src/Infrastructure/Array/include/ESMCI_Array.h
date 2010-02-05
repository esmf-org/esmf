// $Id: ESMCI_Array.h,v 1.36.2.1 2010/02/05 19:51:56 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_Array_H
#define ESMCI_Array_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::Array - Array
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Array} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_Array.C}
// contains the full code (bodies) for the {\tt Array} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_Base.h"      // Base is superclass to Array
#include "ESMCI_VM.h"
#include "ESMCI_DELayout.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_ArraySpec.h"
#include "ESMCI_LocalArray.h"
#include "ESMCI_RHandle.h"

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  class Array;
  struct SeqIndex;
  class SparseMatrix;

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
    LocalArray **larrayList;          // [localDeCount]
    void **larrayBaseAddrList;        // [localDeCount]
    int *exclusiveLBound;             // [redDimCount*localDeCount]
    int *exclusiveUBound;             // [redDimCount*localDeCount]
    int *computationalLBound;         // [redDimCount*localDeCount]
    int *computationalUBound;         // [redDimCount*localDeCount]
    int *totalLBound;                 // [redDimCount*localDeCount]
    int *totalUBound;                 // [redDimCount*localDeCount]
    int tensorCount;                  // number of tensor dimensions
    int tensorElementCount;           // number of tensor elements per element
    int *undistLBound;                // [tensorCount]
    int *undistUBound;                // [tensorCount]
    int *distgridToArrayMap;          // [dimCount] - entries are basis 1
                                      // entry of 0 indicates replicated dim
    int *arrayToDistGridMap;          // [rank]     - entries are basis 1
                                      // entry of 0 indicates undistributed dim
    int *distgridToPackedArrayMap;    // [dimCount] - entries are basis 1
                                      // entry of 0 indicates replicated dim
    int *contiguousFlag;              // [localDeCount]
    int *exclusiveElementCountPDe;    // [deCount] number of elements in
                                      // exclusive region only considering
                                      // DistGrid dims that are associated with
                                      // the Array dims. 
                                      // Multiply with tensorElementCount to get
                                      // total number of elements in exclusive
                                      // Array region.
    int *totalElementCountPLocalDe;   // [localDeCount] number of elements in
                                      // total region only considering
                                      // DistGrid dims that are associated with
                                      // the Array dims. 
                                      // Multiply with tensorElementCount to get
                                      // total number of elements in total
                                      // Array region.
    // lower level object references
    DistGrid *distgrid;
    bool distgridCreator;
    DELayout *delayout;
    int localDeCountAux;  // auxiliary variable for garbage collection
                          // TODO: remove this variable again once
                          // TODO: reference counting scheme is implemented
                          // TODO: and DELayout cannot be pulled from under
                          // TODO: DistGrid and Array until they are destroyed.
    
   public:
    // native constructor and destructor
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
      distgridToArrayMap = NULL;
      arrayToDistGridMap = NULL;
      distgridToPackedArrayMap = NULL;
      contiguousFlag = NULL;
      tensorElementCount = 0;
      exclusiveElementCountPDe = NULL;
      totalElementCountPLocalDe = NULL;
      localDeCountAux = 0;  // auxiliary variable for garbage collection
    }
    Array(int baseID):ESMC_Base(baseID){  // prevent baseID counter increment
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
      distgridToArrayMap = NULL;
      arrayToDistGridMap = NULL;
      distgridToPackedArrayMap = NULL;
      contiguousFlag = NULL;
      tensorElementCount = 0;
      exclusiveElementCountPDe = NULL;
      totalElementCountPLocalDe = NULL;
      localDeCountAux = 0;  // auxiliary variable for garbage collection
    }
   private:
    Array(ESMC_TypeKind typekind, int rank, LocalArray **larrayList,
      DistGrid *distgrid, bool distgridCreator, int *exclusiveLBound,
      int *exclusiveUBound, int *computationalLBound, int *computationalUBound,
      int *totalLBound, int *totalUBound, int tensorCount,
      int tensorElementCount, int *undistLBoundArray, int *undistUBoundArray,
      int *distgridToArrayMapArray, int *arrayToDistGridMapArray,
      int *distgridToPackedArrayMapArray, ESMC_IndexFlag indexflagArg, int *rc);
   public:
    ~Array(){destruct(false);}
   private:
    void destruct(bool followCreator=true);
   public:
    // create() and destroy()
    static Array *create(LocalArray **larrayList, int larrayCount,
      DistGrid *distgrid, CopyFlag copyflag,
      InterfaceInt *distgridToArrayMap,
      InterfaceInt *computationalEdgeLWidthArg,
      InterfaceInt *computationalEdgeUWidthArg,
      InterfaceInt *computationalLWidthArg,
      InterfaceInt *computationalUWidthArg,
      InterfaceInt *totalLWidthArg, InterfaceInt *totalUWidthArg,
      ESMC_IndexFlag *indexflag, InterfaceInt *undistLBoundArg,
      InterfaceInt *undistUBoundArg, int *rc);
    static Array *create(ArraySpec *arrayspec, DistGrid *distgrid,
      InterfaceInt *distgridToArrayMap,
      InterfaceInt *computationalEdgeLWidthArg,
      InterfaceInt *computationalEdgeUWidthArg,
      InterfaceInt *computationalLWidthArg, 
      InterfaceInt *computationalUWidthArg, InterfaceInt *totalLWidthArg,
      InterfaceInt *totalUWidthArg, ESMC_IndexFlag *indexflag,
      InterfaceInt *distLBoundArg, InterfaceInt *undistLBoundArg,
      InterfaceInt *undistUBoundArg, int *rc);
    static Array *create(Array *array, int *rc=NULL);
    static int destroy(Array **array);
    // get() and set()
    ESMC_TypeKind getTypekind()             const {return typekind;}
    int getRank()                           const {return rank;}
    ESMC_IndexFlag getIndexflag()           const {return indexflag;}
    LocalArray **getLocalarrayList()        const {return larrayList;}
    void **getLarrayBaseAddrList()          const {return larrayBaseAddrList;}
    const int *getExclusiveLBound()         const {return exclusiveLBound;}
    const int *getExclusiveUBound()         const {return exclusiveUBound;}
    const int *getComputationalLBound()     const {return computationalLBound;}
    const int *getComputationalUBound()     const {return computationalUBound;}
    const int *getTotalLBound()             const {return totalLBound;}
    const int *getTotalUBound()             const {return totalUBound;}
    int getTensorCount()                    const {return tensorCount;}
    const int *getUndistLBound()            const {return undistLBound;}
    const int *getUndistUBound()            const {return undistUBound;}
    const int *getExclusiveElementCountPDe()const
      {return exclusiveElementCountPDe;}
    const int *getTotalElementCountPLocalDe()const
      {return totalElementCountPLocalDe;}
    int getTensorElementCount()             const {return tensorElementCount;}
    const int *getDistGridToArrayMap()      const {return distgridToArrayMap;}
    const int *getArrayToDistGridMap()      const {return arrayToDistGridMap;}
    const int *getDistGridToPackedArrayMap()const
      {return distgridToPackedArrayMap;}
    DistGrid *getDistGrid()                 const {return distgrid;}
    DELayout *getDELayout()                 const {return delayout;}
    int getLinearIndexExclusive(int localDe, int *index, int *rc=NULL) const;
    SeqIndex getSequenceIndexExclusive(int localDe, int *index,
      int *rc=NULL) const;
    SeqIndex getSequenceIndexPatch(int patch, const int *index, int *rc=NULL)
      const;
    int setComputationalLWidth(InterfaceInt *computationalLWidthArg);
    int setComputationalUWidth(InterfaceInt *computationalUWidthArg);
    const char *getName()               const {return ESMC_BaseGetName();}
    int setName(char *name){return ESMC_BaseSetName(name, "Array");}
    // misc.
    static bool match(Array *array1, Array *array2, int *rc=NULL);
    int print() const;
    int validate() const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset,
      const ESMC_AttReconcileFlag &attreconflag,
      const ESMC_InquireFlag &inquireflag) const;
    int deserialize(char *buffer, int *offset,
      const ESMC_AttReconcileFlag &attreconflag);
    // comms
    int gather(void *array, ESMC_TypeKind typekind, int rank,
      int *counts, int *patch, int rootPet, VM *vm);
    int scatter(void *array, ESMC_TypeKind typekind, int rank,
      int *counts, int *patch, int rootPet, VM *vm);
    static int redistStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, InterfaceInt *srcToDstTransposeMap,
      ESMC_TypeKind typekindFactor = ESMF_NOKIND, void *factor = NULL);
    static int redist(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, ESMC_Logical checkflag=ESMF_FALSE);
    static int redistRelease(RouteHandle *routehandle);
    static int sparseMatMulStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, vector<SparseMatrix> const &sparseMatrix);
    static int sparseMatMul(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle,
      ESMC_RegionFlag zeroflag=ESMF_REGION_TOTAL,
      ESMC_Logical checkflag=ESMF_FALSE);
    static int sparseMatMulRelease(RouteHandle *routehandle);
    
  };  // class Array

  
  struct SeqIndex{
    int decompSeqIndex;
    int tensorSeqIndex;
  };  // struct seqIndex
  bool operator==(SeqIndex a, SeqIndex b);
  bool operator<(SeqIndex a, SeqIndex b);
  
  
  class SeqInd{
    int n;  // number of components in sequence index
    int const *index;
   public:
    SeqInd(){n=0; index=NULL;}
    SeqInd(int n_, int const *index_){
      n = n_;
      index = index_;
    }
    int getIndex(int i)const{return index[i];}
  };
  
  //todo: try to unify SeqIndex and SeqInd structs!
  
  
  class SparseMatrix{
    ESMC_TypeKind typekind;
    void const *factorList;
    int factorListCount;
    int srcN;
    int dstN;
    int const *factorIndexList; // element: (0,..,srcN-1,srcN,..,srcN+dstN-1)
   public:
    SparseMatrix(ESMC_TypeKind const typekind_, void const *factorList_,
      int const factorListCount_, int const srcN_, int const dstN_,
      int const *factorIndexList_);
    ESMC_TypeKind getTypekind()const{return typekind;}
    void const *getFactorList()const{return factorList;}
    int getFactorListCount()const{return factorListCount;}
    SeqInd getSrcSeqIndex(int i)const{
      return SeqInd(srcN, factorIndexList+(srcN+dstN)*i);
    }
    SeqInd getDstSeqIndex(int i)const{
      return SeqInd(dstN, factorIndexList+(srcN+dstN)*i+srcN);
    }
    int getSrcN()const{return srcN;}
    int getDstN()const{return dstN;}
  };

  class ArrayElement : public MultiDimIndexLoop{
    Array *array;                     // associated Array object
    int localDe;                      // localDe index
   public:
    ArrayElement(Array *arrayArg, int localDeArg);
    int getLinearIndexExclusive(){
      return array->getLinearIndexExclusive(localDe, &indexTuple[0]);
    }
    SeqIndex getSequenceIndexExclusive(){
      // getSequenceIndexExclusive() expects basis 0 indexTuple in excl. region
      return array->getSequenceIndexExclusive(localDe, &indexTuple[0]);
    }
  };  // class ArrayElement 
  
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
  VMK::commhandle **vmk_commh;
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
    LocalArray **localArrays; // array of LocalArray pointers [localDe]
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
      LocalArray *larray,  // pointer to LocalArray object
      int *haloWidth,           // halo width
      ESMCI::DELayout *delayout,  // DELayout
      int rootPET,              // root
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayDestruct(void);

    // Get info
    int ESMC_newArrayGet(int *rank, ESMCI::DELayout **delayout, 
      LocalArray **localArrays, int len_localArrays,
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
      LocalArray *larray,  // pointer to LocalArray object
      int rootPET,              // root
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things
    
    int ESMC_newArrayScatter(
      LocalArray *larray,  // pointer to LocalArray object
      int rootPET,              // root
      ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
      ESMCI::VM *vm=NULL);        // optional VM argument to speed up things

    int ESMC_newArrayScatter(
      LocalArray *larray,  // pointer to LocalArray object
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

ESMC_newArray *ESMC_newArrayCreate(LocalArray *larray, int *haloWidth, 
  int deCount, int rootPET, int *rc);

int ESMC_newArrayDestroy(ESMC_newArray **array);

#endif


#endif  // ESMCI_Array_H
