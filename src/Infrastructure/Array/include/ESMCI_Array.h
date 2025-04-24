// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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

  // constants and enums

  enum ArrayMatch_Flag {ARRAYMATCH_INVALID=0, ARRAYMATCH_NONE,
    ARRAYMATCH_EXACT, ARRAYMATCH_ALIAS};

  // classes and structs

  template<typename T> struct SeqIndex;
  template<typename SIT, typename DIT> class SparseMatrix;

  // class definitions

//TODO: Figure out how to have code use correct SeqIndex structure automatic.
//TODO: For now just hard-code the use of one or the other via CPP definition.
#define SeqIndexTensor SeqIndex

  //============================================================================
  template<typename T> struct SeqIndexTensor{
    T decompSeqIndex;
    int tensorSeqIndex;
    SeqIndexTensor(){
      decompSeqIndex = -1;  // invalidate
      tensorSeqIndex = -1;  // invalidate
    }
    void print(){
      printf("SeqIndexTensor: (%d, %d)\n", decompSeqIndex, tensorSeqIndex);
    }
    void fprint(std::FILE *fp){
      fprintf(fp, "SeqIndexTensor: (%d, %d)\n", decompSeqIndex, tensorSeqIndex);
    }
    bool valid(){
      if (decompSeqIndex == -1) return false; // invalid seqIndex
      return true;  // otherwise valid
    }
    void incrementTensor(){
      ++tensorSeqIndex;
    }
    void setTensor(int tensorSeqIndex_){
      tensorSeqIndex = tensorSeqIndex_;
    }
    int getTensor(){
      return tensorSeqIndex;
    }
  };  // struct seqIndexTensor
  template<typename T> bool operator==(SeqIndexTensor<T> a, SeqIndexTensor<T> b);
  template<typename T> bool operator!=(SeqIndexTensor<T> a, SeqIndexTensor<T> b);
  template<typename T> bool operator<(SeqIndexTensor<T> a, SeqIndexTensor<T> b);

  template<typename T> struct SeqIndexLite{
    T decompSeqIndex;
    SeqIndexLite(){
      decompSeqIndex = -1;  // invalidate
    }
    void print(){
      printf("SeqIndexLite: (%d)\n", decompSeqIndex);
    }
    void fprint(std::FILE *fp){
      fprintf(fp, "SeqIndexLite: (%d)\n", decompSeqIndex);
    }
    bool valid(){
      if (decompSeqIndex == -1) return false; // invalid seqIndex
      return true;  // otherwise valid
    }
    void incrementTensor(){
      // no-op
    }
    void setTensor(int tensorSeqIndex_){
      // no-op
    }
    int getTensor(){
      return 1; // dummy
    }
  };  // struct seqIndexLite
  template<typename T> bool operator==(SeqIndexLite<T> a, SeqIndexLite<T> b);
  template<typename T> bool operator!=(SeqIndexLite<T> a, SeqIndexLite<T> b);
  template<typename T> bool operator<(SeqIndexLite<T> a, SeqIndexLite<T> b);

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
    // PET-local information
    // larrayList and larrayBaseAddrList hold the PET-local DEs in the first
    // localDe many entries. Then, up to vasLocalDeCount are the DEs that
    // are in the same VAS, and up to ssiLocalDeCount are the DEs that are
    // in the same SSI.
    // Without VAS DE sharing, vasLocalDeCount==localDeCount.
    // Without SSI DE sharing, ssiLocalDeCount==vasLocalDeCount.
    LocalArray **larrayList;          // [ssiLocalDeCount] localDeCount first
    void **larrayBaseAddrList;        // [ssiLocalDeCount] localDeCount first
    VM::memhandle *mh;                // in case memory sharing between PETs
    bool mhCreator;                   // responsible to delete mh resource
    int vasLocalDeCount;              // number of DEs that are in the same VAS
    int ssiLocalDeCount;              // number of DEs that are on the same SSI
    int *localDeToDeMap;              // [ssiLocalDeCount] mapping to DE
    int *exclusiveLBound;             // [redDimCount*ssiLocalDeCount]
    int *exclusiveUBound;             // [redDimCount*ssiLocalDeCount]
    int *computationalLBound;         // [redDimCount*ssiLocalDeCount]
    int *computationalUBound;         // [redDimCount*ssiLocalDeCount]
    int *totalLBound;                 // [redDimCount*ssiLocalDeCount]
    int *totalUBound;                 // [redDimCount*ssiLocalDeCount]
    int tensorCount;                  // number of tensor dimensions
    int tensorElementCount;           // number of tensor elements per element
    int replicatedDimCount;           // number of replicated dimensions
                                      // (i.e., dimensions where distgridToArrayMap[i] == 0)
    int *undistLBound;                // [tensorCount]
    int *undistUBound;                // [tensorCount]
    int *distgridToArrayMap;          // [dimCount] - entries are basis 1
                                      // entry of 0 indicates replicated dim
    int *arrayToDistGridMap;          // [rank]     - entries are basis 1
                                      // entry of 0 indicates undistributed dim
    int *distgridToPackedArrayMap;    // [dimCount] - entries are basis 1
                                      // entry of 0 indicates replicated dim
                                      // distr. Array dims as 1, 2, 3, .. only
    int *contiguousFlag;              // [ssiLocalDeCount]
    int *totalElementCountPLocalDe;   // [ssiLocalDeCount] number of elements in
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
    VM *vmAux;            // need this b/c DELayout may not be available
                          // during destroy (potentially destroyed before Array)
    RouteHandle *ioRH;    // RouteHandle to store redist if needed during IO

   public:
    // native constructor and destructor
    Array(VM *vm=NULL):ESMC_Base(vm){ // allow specific VM instead default
      typekind = ESMF_NOKIND;
      rank = 0;
      indexflag = ESMC_INDEX_DELOCAL;
      larrayList = NULL;
      larrayBaseAddrList = NULL;
      mh = NULL;
      vasLocalDeCount = 0;
      ssiLocalDeCount = 0;
      localDeToDeMap = NULL;
      exclusiveLBound = NULL;
      exclusiveUBound = NULL;
      computationalLBound = NULL;
      computationalUBound = NULL;
      totalLBound = NULL;
      totalUBound = NULL;
      tensorCount = 0;
      replicatedDimCount = 0;
      undistLBound = NULL;
      undistUBound = NULL;
      distgridToArrayMap = NULL;
      arrayToDistGridMap = NULL;
      distgridToPackedArrayMap = NULL;
      contiguousFlag = NULL;
      tensorElementCount = 0;
      totalElementCountPLocalDe = NULL;
      sizeSuperUndist = NULL;
      sizeDist = NULL;
#if !defined (ESMF_OS_IRIX64)
      rimSeqIndexI4.resize(0);
      rimSeqIndexI8.resize(0);
#endif
      rimLinIndex.resize(0);
      rimElementCount.resize(0);
      localDeCountAux = 0;  // auxiliary variable for garbage collection
      vmAux = vm;
      ioRH = NULL;
    }
    Array(int baseID):ESMC_Base(baseID){  // prevent baseID counter increment
      typekind = ESMF_NOKIND;
      rank = 0;
      indexflag = ESMC_INDEX_DELOCAL;
      larrayList = NULL;
      larrayBaseAddrList = NULL;
      mh = NULL;
      vasLocalDeCount = 0;
      ssiLocalDeCount = 0;
      localDeToDeMap = NULL;
      exclusiveLBound = NULL;
      exclusiveUBound = NULL;
      computationalLBound = NULL;
      computationalUBound = NULL;
      totalLBound = NULL;
      totalUBound = NULL;
      tensorCount = 0;
      replicatedDimCount = 0;
      undistLBound = NULL;
      undistUBound = NULL;
      distgridToArrayMap = NULL;
      arrayToDistGridMap = NULL;
      distgridToPackedArrayMap = NULL;
      contiguousFlag = NULL;
      tensorElementCount = 0;
      totalElementCountPLocalDe = NULL;
      sizeSuperUndist = NULL;
      sizeDist = NULL;
#if !defined (ESMF_OS_IRIX64)
      rimSeqIndexI4.resize(0);
      rimSeqIndexI8.resize(0);
#endif
      rimLinIndex.resize(0);
      rimElementCount.resize(0);
      localDeCountAux = 0;  // auxiliary variable for garbage collection
      vmAux = NULL;
      ioRH = NULL;
    }
   private:
    Array(ESMC_TypeKind_Flag typekind, int rank, LocalArray **larrayList,
      VM::memhandle *mh, int vasLocalDeCount, int ssiLocalDeCount,
      int *localDeToDeMap, DistGrid *distgrid, bool distgridCreator,
      int *exclusiveLBound, int *exclusiveUBound, int *computationalLBound,
      int *computationalUBound, int *totalLBound, int *totalUBound,
      int tensorCount, int tensorElementCount, int replicatedDimCount,
      int *undistLBoundArray, int *undistUBoundArray, int *distgridToArrayMapArray,
      int *arrayToDistGridMapArray, int *distgridToPackedArrayMapArray,
      ESMC_IndexFlag indexflagArg, int *rc,
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
      DistGrid *distgrid, DataCopyFlag copyflag,
      InterArray<int> *distgridToArrayMap,
      InterArray<int> *computationalEdgeLWidthArg,
      InterArray<int> *computationalEdgeUWidthArg,
      InterArray<int> *computationalLWidthArg,
      InterArray<int> *computationalUWidthArg,
      InterArray<int> *totalLWidthArg, InterArray<int> *totalUWidthArg,
      ESMC_IndexFlag *indexflag,
      InterArray<int> *undistLBoundArg, InterArray<int> *undistUBoundArg,
      int *rc);
    static Array *create(ArraySpec *arrayspec, DistGrid *distgrid,
      InterArray<int> *distgridToArrayMap,
      InterArray<int> *computationalEdgeLWidthArg,
      InterArray<int> *computationalEdgeUWidthArg,
      InterArray<int> *computationalLWidthArg,
      InterArray<int> *computationalUWidthArg,
      InterArray<int> *totalLWidthArg, InterArray<int> *totalUWidthArg,
      ESMC_IndexFlag *indexflag, ESMC_Pin_Flag *pinflag,
      InterArray<int> *distLBoundArg,
      InterArray<int> *undistLBoundArg, InterArray<int> *undistUBoundArg,
      int *rc, VM *vm=NULL);
    static Array *create(Array *array, DataCopyFlag copyflag,
      DELayout *delayout=NULL, InterArray<int> *trailingTensorSlice=NULL,
      int rmLeadingTensors=0, int *rc=NULL);
    static Array *create(Array *array, bool rmTensorFlag, int *rc=NULL);
    static int destroy(Array **array, bool noGarbage=false);
    // data copy()
    int copy(Array const *arrayIn);
    // get() and set()
    ESMC_TypeKind_Flag getTypekind()        const {return typekind;}
    int getRank()                           const {return rank;}
    int getVasLocalDeCount()                const {return vasLocalDeCount;}
    int getSsiLocalDeCount()                const {return ssiLocalDeCount;}
    ESMC_IndexFlag getIndexflag()           const {return indexflag;}
    LocalArray **getLocalarrayList()        const {return larrayList;}
    void **getLarrayBaseAddrList()          const {return larrayBaseAddrList;}
    const int *getLocalDeToDeMap()          const {return localDeToDeMap;}
    const int *getExclusiveLBound()         const {return exclusiveLBound;}
    const int *getExclusiveUBound()         const {return exclusiveUBound;}
    const int *getComputationalLBound()     const {return computationalLBound;}
    const int *getComputationalUBound()     const {return computationalUBound;}
    const int *getTotalLBound()             const {return totalLBound;}
    const int *getTotalUBound()             const {return totalUBound;}
    int getTensorCount()                    const {return tensorCount;}
    int getReplicatedDimCount()             const {return replicatedDimCount;}
    const int *getUndistLBound()            const {return undistLBound;}
    const int *getUndistUBound()            const {return undistUBound;}
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
    bool isRHCompatible(Array const *array, int *rc=NULL)const;
    static ArrayMatch_Flag match(Array const *array1, Array const *array2,
      int *rc=NULL);
    static bool matchBool(Array const *array1, Array const *array2, int *rc=NULL);
    int read(const std::string &file, const std::string &variableName,
         int *timeslice, ESMC_IOFmt_Flag *iofmt);
    int write(const std::string &file, const std::string &variableName,
         const std::string &convention, const std::string &purpose,
         bool *overwrite, ESMC_FileStatus_Flag *status,
         int *timeslice, ESMC_IOFmt_Flag *iofmt);
    void log(std::string prefix,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO, bool deepFlag=false)const;
    int print() const;
    int sync();
    int validate() const;
    // fileMapList is an int64_t to be compatible with PIO and MPI.
    // Internally, PIO uses a type which is tied to the Fortran type,
    // MPI_OFFSET. Instead of int64_t, an alternative is to use MPI_offset,
    // however, this may run into issues with MPIUNI.
    int constructFileMap(int64_t *fileMapList, int mapListSize,
                         int localDe, int64_t unmap_val=0) const;
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
      RouteHandle **routehandle, InterArray<int> *srcToDstTransposeMap=NULL,
      ESMC_TypeKind_Flag typekindFactor=ESMF_NOKIND, void *factor=NULL,
      bool ignoreUnmatched=false, int *pipelineDepthArg=NULL);
    template<typename SIT, typename DIT>
      static int tRedistStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle, InterArray<int> *srcToDstTransposeMap=NULL,
      ESMC_TypeKind_Flag typekindFactor=ESMF_NOKIND, void *factor=NULL,
      bool ignoreUnmatched=false, int *pipelineDepthArg=NULL);
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
      int *srcTermProcessingArg=NULL, int *pipelineDepthArg=NULL);
    template<typename SIT, typename DIT>
      static int tSparseMatMulStore(Array *srcArray, Array *dstArray,
      RouteHandle **routehandle,
      std::vector<SparseMatrix<SIT,DIT> > const &sparseMatrix,
      bool haloFlag=false, bool ignoreUnmatched=false,
      int *srcTermProcessingArg=NULL, int *pipelineDepthArg=NULL);
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
    void log(ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO)const;
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
          // optimize by simple seqIndex increment
          if (firstDimDecompFlag){
            // increment the decompSeqIndex
            if (indexTK==ESMC_TYPEKIND_I4)
              ((SeqIndex<ESMC_I4>*)seqIndex)->decompSeqIndex++;
            else if (indexTK==ESMC_TYPEKIND_I8)
              ((SeqIndex<ESMC_I8>*)seqIndex)->decompSeqIndex++;
          }else{
            // increment the tensorSeqIndex
            if (indexTK==ESMC_TYPEKIND_I4)
              ((SeqIndex<ESMC_I4>*)seqIndex)->incrementTensor();
            else if (indexTK==ESMC_TYPEKIND_I8)
              ((SeqIndex<ESMC_I8>*)seqIndex)->incrementTensor();
          }
        }
      }
    }
  };  // class ArrayElement
  //============================================================================

} // namespace ESMCI

#endif  // ESMCI_Array_H
