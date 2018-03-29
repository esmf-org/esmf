// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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

namespace ESMCI {
  class Array;
}

#include "ESMCI_Base.h"       // Base is superclass to Array
#include "ESMCI_VM.h"
#include "ESMCI_DELayout.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_ArraySpec.h"
#include "ESMCI_LocalArray.h"
#include "ESMCI_RHandle.h"

#include <cstdio>
#include <stdint.h>
#include <string>
#include <utility>

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  template<typename T> struct SeqIndex;
  template<typename SIT, typename DIT> class SparseMatrix;

  // class definitions

  //============================================================================
  template<typename T> struct SeqIndex{
    T decompSeqIndex;
    int tensorSeqIndex;
    SeqIndex(){
      decompSeqIndex = -1;  // invalidate
      tensorSeqIndex = -1;  // invalidate
    }
    void print(){
      printf("SeqIndex: (%d, %d)\n", decompSeqIndex, tensorSeqIndex);
    }
    void fprint(std::FILE *fp){
      fprintf(fp, "SeqIndex: (%d, %d)\n", decompSeqIndex, tensorSeqIndex);
    }
    bool valid(){
      if (decompSeqIndex == -1) return false; // invalid seqIndex
      return true;  // otherwise valid
    }
  };  // struct seqIndex
  template<typename T> bool operator==(SeqIndex<T> a, SeqIndex<T> b);
  template<typename T> bool operator!=(SeqIndex<T> a, SeqIndex<T> b);
  template<typename T> bool operator<(SeqIndex<T> a, SeqIndex<T> b);

  template<typename T> class SeqInd{
    int n;  // number of components in sequence index
    T const *index;
   public:
    SeqInd(){n=0; index=NULL;}
    SeqInd(int n_, T const *index_){
      n = n_;
      index = index_;
    }
    int getN(){
      return n;
    }
    T getIndex(int i)const{return index[i];}
    void print(){
      std::cout << "SeqInd:" << n <<" (";
      int i;
      for (i=0; i<n-1; i++)
        std::cout << index[i] << ",";
      std::cout << index[i] << ")\n";
    }
  };
  template<typename T> bool operator<(SeqInd<T> a, SeqInd<T> b){
    if (a.getN() != b.getN()) throw ESMC_RC_INTNRL_INCONS;
    for (int i=0; i<a.getN(); i++)
      if (a.getIndex(i) >= b.getIndex(i)) return false;
    return true;
  }

  //todo: try to unify SeqIndex and SeqInd structs!
  //============================================================================


  //============================================================================
  class Array : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    // global information
    ESMC_TypeKind_Flag typekind;
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
    std::vector<std::vector<SeqIndex<ESMC_I4> > > rimSeqIndexI4;
                                      // elements in the rim,
                                      // between exclusive and total bounds
                                      // [localDeCount][rimElementCount[]]
    std::vector<std::vector<SeqIndex<ESMC_I8> > > rimSeqIndexI8;
                                      // elements in the rim,
                                      // between exclusive and total bounds
                                      // [localDeCount][rimElementCount[]]
    std::vector<std::vector<int> > rimLinIndex;       // elements in the rim,
                                      // between exclusive and total bounds
                                      // [localDeCount][rimElementCount[]]
    std::vector<int> rimElementCount; // numb. of elements in rim [localDeCount]
    // special variables for super-vectorization in XXE
    int *sizeSuperUndist;   // [redDimCount+1]
    int *sizeDist;          // [redDimCount*localDeCount]
    // lower level object references
    DistGrid *distgrid;
    bool distgridCreator;
    DELayout *delayout;
    int localDeCountAux;  // auxiliary variable for garbage collection
                          // TODO: remove this variable again once
                          // TODO: reference counting scheme is implemented
                          // TODO: and DELayout cannot be pulled from under
                          // TODO: DistGrid and Array until they are destroyed.
    RouteHandle *ioRH;    // RouteHandle to store redist if needed during IO

   public:
    // native constructor and destructor
    Array(VM *vm=NULL):ESMC_Base(vm){ // allow specific VM instead default
      typekind = ESMF_NOKIND;
      rank = 0;
      indexflag = ESMC_INDEX_DELOCAL;
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
      sizeSuperUndist = NULL;
      sizeDist = NULL;
#if !defined (PARCH_IRIX64)
      rimSeqIndexI4.resize(0);
      rimSeqIndexI8.resize(0);
#endif
      rimLinIndex.resize(0);
      rimElementCount.resize(0);
      localDeCountAux = 0;  // auxiliary variable for garbage collection
      ioRH = NULL;
    }
    Array(int baseID):ESMC_Base(baseID){  // prevent baseID counter increment
      typekind = ESMF_NOKIND;
      rank = 0;
      indexflag = ESMC_INDEX_DELOCAL;
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
      sizeSuperUndist = NULL;
      sizeDist = NULL;
#if !defined (PARCH_IRIX64)
      rimSeqIndexI4.resize(0);
      rimSeqIndexI8.resize(0);
#endif
      rimLinIndex.resize(0);
      rimElementCount.resize(0);
      localDeCountAux = 0;  // auxiliary variable for garbage collection
      ioRH = NULL;
    }
   private:
    Array(ESMC_TypeKind_Flag typekind, int rank, LocalArray **larrayList,
      DistGrid *distgrid, bool distgridCreator, int *exclusiveLBound,
      int *exclusiveUBound, int *computationalLBound, int *computationalUBound,
      int *totalLBound, int *totalUBound, int tensorCount,
      int tensorElementCount, int *undistLBoundArray, int *undistUBoundArray,
      int *distgridToArrayMapArray, int *arrayToDistGridMapArray,
      int *distgridToPackedArrayMapArray, ESMC_IndexFlag indexflagArg, int *rc,
      VM *vm=NULL); // allow specific VM instead default
   public:
    ~Array(){destruct(false);}
   private:
    void destruct(bool followCreator=true, bool noGarbage=false);
   public:
    // helper
    int constructContiguousFlag(int redDimCount);
    // create() and destroy()
    static Array *create(LocalArray **larrayList, int larrayCount,
      DistGrid *distgrid, CopyFlag copyflag,
      InterArray<int> *distgridToArrayMap,
      InterArray<int> *computationalEdgeLWidthArg,
      InterArray<int> *computationalEdgeUWidthArg,
      InterArray<int> *computationalLWidthArg,
      InterArray<int> *computationalUWidthArg,
      InterArray<int> *totalLWidthArg, InterArray<int> *totalUWidthArg,
      ESMC_IndexFlag *indexflag, InterArray<int> *undistLBoundArg,
      InterArray<int> *undistUBoundArg, int *rc);
    static Array *create(ArraySpec *arrayspec, DistGrid *distgrid,
      InterArray<int> *distgridToArrayMap,
      InterArray<int> *computationalEdgeLWidthArg,
      InterArray<int> *computationalEdgeUWidthArg,
      InterArray<int> *computationalLWidthArg,
      InterArray<int> *computationalUWidthArg,
      InterArray<int> *totalLWidthArg,
      InterArray<int> *totalUWidthArg, ESMC_IndexFlag *indexflag,
      InterArray<int> *distLBoundArg, InterArray<int> *undistLBoundArg,
      InterArray<int> *undistUBoundArg, int *rc, VM *vm=NULL);
    static Array *create(Array *array, int rmLeadingTensors=0, int *rc=NULL);
    static Array *create(Array *array, bool rmTensorFlag, int *rc=NULL);
    static int destroy(Array **array, bool noGarbage=false);
    // data copy()
    int copy(Array const *arrayIn);
    // get() and set()
    ESMC_TypeKind_Flag getTypekind()        const {return typekind;}
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
    RouteHandle *getIoRH()                  const {return ioRH;}
    void setIoRH(RouteHandle *rh){ioRH = rh;}
    int getLinearIndexExclusive(int localDe, int const *index)const;
    template<typename T> int getSequenceIndexExclusive(int localDe,
      int const *index, SeqIndex<T> *seqIndex, bool recursive=true,
      bool canonical=false) const;
    template<typename T> SeqIndex<T> getSequenceIndexTile(int tile,
      const int *index, int *rc=NULL) const;
    int getTensorSequenceIndex(const int *index, int *rc=NULL)const;
    int getArbSequenceIndexOffset(const int *index, int *rc=NULL)const;
    int setComputationalLWidth(InterArray<int> *computationalLWidthArg);
    int setComputationalUWidth(InterArray<int> *computationalUWidthArg);
    void setRimMembers();
    template<typename T> int setRimSeqIndex(int localDe,
      InterArray<T> *rimSeqIndexArg);
    template<typename T>
      int getRimSeqIndex(const std::vector<std::vector<SeqIndex<T> > >
      **rimSeqIndex_)const;
    std::vector<std::vector<int> > const &getRimLinIndex()const
      {return rimLinIndex;}
    std::vector<int> const &getRimElementCount()const
      {return rimElementCount;}
    const char *getName()               const {return ESMC_BaseGetName();}
    int setName(const char* name){return ESMC_BaseSetName(name, "Array");}
    int setName(const std::string &name){return ESMC_BaseSetName(name.c_str(), "Array");}
    // misc.
    static bool match(Array const *array1, Array const *array2, int *rc=NULL);
    int read(const std::string &file, const std::string &variableName,
         int *timeslice, ESMC_IOFmt_Flag *iofmt);
    int write(const std::string &file, const std::string &variableName,
         const std::string &convention, const std::string &purpose,
         bool *overwrite, ESMC_FileStatus_Flag *status,
         int *timeslice, ESMC_IOFmt_Flag *iofmt);
    int print() const;
    int validate() const;
    // fileMapList is an int64_t to be compatible with PIO and MPI.
    // Internally, PIO uses a type which is tied to the Fortran type,
    // MPI_OFFSET. Instead of int64_t, an alternative is to use MPI_offset,
    // however, this may run into issues with MPIUNI.
    int constructFileMap(int64_t *fileMapList, int mapListSize,
                         int localDe, int64_t unmap_val = 0) const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset,
      const ESMC_AttReconcileFlag &attreconflag,
      const ESMC_InquireFlag &inquireflag) const;
    int deserialize(char *buffer, int *offset,
      const ESMC_AttReconcileFlag &attreconflag);
    // comms
    int gather(void *array, ESMC_TypeKind_Flag typekind, int rank,
      int *counts, int *tile, int rootPet, VM *vm);
    int scatter(void *array, ESMC_TypeKind_Flag typekind, int rank,
      int *counts, int *tile, int rootPet, VM *vm);
    static int haloStore(Array *array, RouteHandle **routehandle,
      ESMC_HaloStartRegionFlag halostartregionflag=ESMF_REGION_EXCLUSIVE,
      InterArray<int> *haloLDepth=NULL, InterArray<int> *haloUDepth=NULL,
      int *pipelineDepthArg=NULL);
    template<typename IT>
      static int tHaloStore(Array *array, RouteHandle **routehandle,
      ESMC_HaloStartRegionFlag halostartregionflag=ESMF_REGION_EXCLUSIVE,
      InterArray<int> *haloLDepth=NULL, InterArray<int> *haloUDepth=NULL,
      int *pipelineDepthArg=NULL);
    static int halo(Array *array,
      RouteHandle **routehandle, ESMC_CommFlag commflag=ESMF_COMM_BLOCKING,
      bool *finishedflag=NULL, bool *cancelledflag=NULL, bool checkflag=false);
    static int haloRelease(RouteHandle *routehandle);
    static int redistStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, InterArray<int> *srcToDstTransposeMap,
      ESMC_TypeKind_Flag typekindFactor = ESMF_NOKIND, void *factor = NULL,
      bool ignoreUnmatched=false, int *pipelineDepthArg = NULL);
    template<typename SIT, typename DIT>
      static int tRedistStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, InterArray<int> *srcToDstTransposeMap,
      ESMC_TypeKind_Flag typekindFactor = ESMF_NOKIND, void *factor = NULL,
      bool ignoreUnmatched=false, int *pipelineDepthArg = NULL);
    static int redist(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, ESMC_CommFlag commflag=ESMF_COMM_BLOCKING,
      bool *finishedflag=NULL, bool *cancelledflag=NULL,
      ESMC_Region_Flag zeroflag=ESMC_REGION_SELECT, bool checkflag=false);
    static int redistRelease(RouteHandle *routehandle);
    template<typename SIT, typename DIT>
      static int sparseMatMulStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle,
      std::vector<SparseMatrix<SIT,DIT> > const &sparseMatrix,
      bool haloFlag=false, bool ignoreUnmatched=false,
      int *srcTermProcessingArg = NULL, int *pipelineDepthArg = NULL);
    template<typename SIT, typename DIT>
      static int tSparseMatMulStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle,
      std::vector<SparseMatrix<SIT,DIT> > const &sparseMatrix,
      bool haloFlag=false, bool ignoreUnmatched=false,
      int *srcTermProcessingArg = NULL, int *pipelineDepthArg = NULL);
    static int sparseMatMul(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, ESMC_CommFlag commflag=ESMF_COMM_BLOCKING,
      bool *finishedflag=NULL, bool *cancelledflag=NULL,
      ESMC_Region_Flag zeroflag=ESMC_REGION_TOTAL,
      ESMC_TermOrder_Flag termorderflag=ESMC_TERMORDER_FREE,
      bool checkflag=false, bool haloFlag=false);
    static int sparseMatMulRelease(RouteHandle *routehandle);
    static void superVecParam(Array *array, int localDeCount,
      bool superVectorOkay, int superVecSizeUnd[3], int *superVecSizeDis[2],
      int &vectorLength);

  };  // class Array
  //============================================================================


  //============================================================================
  template<typename SIT, typename DIT> class SparseMatrix{
    ESMC_TypeKind_Flag typekind;  // typekind of factors
    void const *factorList;       // vector of factors
    int factorListCount;          // number of factors
    int srcN;                     // src sequence index width (1 or 2)
    int dstN;                     // dst sequence index width (1 or 2)
    void const *factorIndexList;  // vector of sequence index elements, each
                                  // element is a srcN+dstN tuple:
                                  // (0,..,srcN-1,srcN,..,srcN+dstN-1)
   public:
    SparseMatrix(ESMC_TypeKind_Flag const typekind_, void const *factorList_,
      int const factorListCount_, int const srcN_, int const dstN_,
      void const *factorIndexList_);
    ESMC_TypeKind_Flag getTypekind()const{return typekind;}
    void const *getFactorList()const{return factorList;}
    void const *getFactorIndexList()const{return factorIndexList;}
    int getFactorListCount()const{return factorListCount;}
    SeqInd<SIT> getSrcSeqIndex(int i)const{
      char *fil = (char *)factorIndexList;
      return SeqInd<SIT>(srcN,
        (SIT *)(fil+(srcN*sizeof(SIT)+dstN*sizeof(DIT))*i));
    }
    SeqInd<DIT> getDstSeqIndex(int i)const{
      char *fil = (char *)factorIndexList;
      return SeqInd<DIT>(dstN,
        (DIT *)(fil+(srcN*sizeof(SIT)+dstN*sizeof(DIT))*i+srcN*sizeof(SIT)));
    }
    int getSrcN()const{return srcN;}
    int getDstN()const{return dstN;}
  };
  //============================================================================


  //============================================================================
  class ArrayElement : public MultiDimIndexLoop{
    // Iterator type through Array elements.
    Array const *array;               // associated Array object
    int localDe;                      // localDe index
    //
    int linIndex;                     // linear index of array element in array
    void *seqIndex;                   // sequence index of array element
    bool seqIndexRecursiveFlag;       // flag to be used when computing seqIndex
    bool seqIndexCanonicalFlag;       // flag to be used when computing seqIndex
    bool firstDimDecompFlag;          // indicate lowest array dim is decomp
    bool firstDimFirstDecomp;         // first array dim corres. to first decomp
    bool arbSeqIndexFlag;             // arbitrary sequence indices present
    bool lastSeqIndexInvalid;         // internally used flag between next()
    bool blockActiveFlag;             // active block requires hasValid checks
    bool cannotOptimizeLookup;        // must do lookup from tuple each time
    ESMC_TypeKind_Flag indexTK;       // sequence index typekind
   public:
    ArrayElement(Array const *arrayArg, int localDeArg,
      bool seqIndexEnabled, bool seqIndexRecursive, bool seqIndexCanonical);
      // construct iterator through exclusive Array region
    ArrayElement(Array const *arrayArg, int localDeArg, bool blockExclusiveFlag,
      bool seqIndexEnabled, bool seqIndexRecursive, bool seqIndexCanonical);
      // construct iterator through total Array region with block excl. option
    ~ArrayElement(){
      if (seqIndex){
        if (indexTK==ESMC_TYPEKIND_I4)
          delete (SeqIndex<ESMC_I4>*) seqIndex;
        else if (indexTK==ESMC_TYPEKIND_I8)
          delete (SeqIndex<ESMC_I8>*) seqIndex;
      }
    }
    int getLinearIndex()const{
      // return the linear index of ArrayElement into the Array
      return linIndex;
    }
    bool hasValidSeqIndex()const;
    template<typename T> SeqIndex<T> getSequenceIndex()const{
      if (seqIndex)
        return *(SeqIndex<T>*)seqIndex;
      else throw ESMC_RC_ARG_BAD;
    }
    int getTensorSequenceIndex()const;
    int getArbSequenceIndexOffset()const;
    void print()const;
    void next(){
      bool adjusted = MultiDimIndexLoop::next();
      if (!isWithin()) return;  // reached the end of iteration
      if (adjusted){
        // must re-compute linIndex from index tuple
        linIndex = array->getLinearIndexExclusive(localDe, &indexTuple[0]);
      }else{
        // simply increment linIndex
        linIndex++;
      }
      if (seqIndex){
        if (cannotOptimizeLookup || adjusted || lastSeqIndexInvalid){
          // must re-compute seqIndex from index tuple
          if (!blockActiveFlag || hasValidSeqIndex()){
            // a valid sequence index can be queried
            lastSeqIndexInvalid = false; // reset by default
            if (indexTK==ESMC_TYPEKIND_I4){
              int localrc = array->getSequenceIndexExclusive(localDe,
                &indexTuple[0],
                (SeqIndex<ESMC_I4>*)seqIndex, seqIndexRecursiveFlag,
                seqIndexCanonicalFlag);
              if (localrc != ESMF_SUCCESS) throw localrc;
              if (((SeqIndex<ESMC_I4>*)seqIndex)->decompSeqIndex == -1)
                lastSeqIndexInvalid = true;
            }else if (indexTK==ESMC_TYPEKIND_I8){
              int localrc = array->getSequenceIndexExclusive(localDe,
                &indexTuple[0],
                (SeqIndex<ESMC_I8>*)seqIndex, seqIndexRecursiveFlag,
                seqIndexCanonicalFlag);
              if (localrc != ESMF_SUCCESS) throw localrc;
              if (((SeqIndex<ESMC_I8>*)seqIndex)->decompSeqIndex == -1)
                lastSeqIndexInvalid = true;
            }
          }
        }else{
          // opimize by simple index seqIndex increment
          if (firstDimDecompFlag){
            // increment the decompSeqIndex
            if (indexTK==ESMC_TYPEKIND_I4)
              ((SeqIndex<ESMC_I4>*)seqIndex)->decompSeqIndex++;
            else if (indexTK==ESMC_TYPEKIND_I8)
              ((SeqIndex<ESMC_I8>*)seqIndex)->decompSeqIndex++;
          }else{
            // increment the tensorSeqIndex
            if (indexTK==ESMC_TYPEKIND_I4)
              ((SeqIndex<ESMC_I4>*)seqIndex)->tensorSeqIndex++;
            else if (indexTK==ESMC_TYPEKIND_I8)
              ((SeqIndex<ESMC_I8>*)seqIndex)->tensorSeqIndex++;
          }
        }
      }
    }
  };  // class ArrayElement
  //============================================================================

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
  ESMCI::VM *vm;            // pointer to current VM
  int de;                   // DE for DE-based non-blocking operation
  int rootPET;              // root
  void *result;             // result memory location
  ESMC_TypeKind_Flag dtk;   // data type kind
  ESMC_Operation op;        // operation flag
}ESMC_newArrayThreadArg;


// class definition
class ESMC_newArray : public ESMC_Base {    // inherits from ESMC_Base class
  private:
    int rank;                 // rank of newArray
    ESMC_TypeKind_Flag kind;  // kind of newArray (for F90)
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
    int *localDeToDeMap;      // list of local DEs
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
      ESMCI::VM *vm=NULL);      // optional VM argument to speed up things

    int ESMC_newArrayScalarReduce(
      void *result,             // result value (scalar)
      ESMC_TypeKind_Flag dtk,   // data type kind
      ESMC_Operation op,        // reduce operation
      int rootPET,              // root
      ESMCI::VM *vm=NULL);      // optional VM argument to speed up things

    int ESMC_newArrayScalarReduce(
      void *result,             // result value (scalar)
      ESMC_TypeKind_Flag dtk,   // data type kind
      ESMC_Operation op,        // reduce operation
      int rootPET,              // root
      ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
      ESMCI::VM *vm=NULL);      // optional VM argument to speed up things

    int ESMC_newArrayScalarReduce(
      void *result,             // result value (scalar)
      ESMC_TypeKind_Flag dtk,   // data type kind
      ESMC_Operation op,        // reduce operation
      int rootPET,              // root
      int de,                   // DE for DE-based non-blocking reduce
      ESMCI::VM *vm=NULL);      // optional VM argument to speed up things

    int ESMC_newArrayWait(
      int rootPET,              // root
      ESMC_newArrayCommHandle *commh, // commu handle specifying non-block op.
      ESMCI::VM *vm=NULL);      // optional VM argument to speed up things

    int ESMC_newArrayWait(
      int de,                   // DE for which to wait
      ESMCI::VM *vm=NULL);      // optional VM argument to speed up things

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
