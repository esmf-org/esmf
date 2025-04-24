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

#ifndef ESMCI_DistGrid_H
#define ESMCI_DistGrid_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::DistGrid - DistGrid
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt DistGrid} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_DistGrid.C}
// contains the full code (bodies) for the {\tt DistGrid} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include <vector>

#include "ESMCI_Base.h"       // Base is superclass to DistGrid
#include "ESMCI_VM.h"
#include "ESMCI_DELayout.h"

//-------------------------------------------------------------------------

namespace ESMCI {

  // constants and enums

  enum Decomp_Flag {DECOMP_INVALID=0, DECOMP_BALANCED,
    DECOMP_RESTFIRST, DECOMP_RESTLAST, DECOMP_CYCLIC, DECOMP_SYMMEDGEMAX};

  enum DistGridMatch_Flag {DISTGRIDMATCH_INVALID=0, DISTGRIDMATCH_NONE,
    DISTGRIDMATCH_ELEMENTCOUNT, DISTGRIDMATCH_INDEXSPACE,
    DISTGRIDMATCH_TOPOLOGY, DISTGRIDMATCH_DECOMP,
    DISTGRIDMATCH_EXACT, DISTGRIDMATCH_ALIAS};

  // classes

  class DistGrid;

  // class definition
  class DistGrid : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    ESMC_TypeKind_Flag indexTK;   // integer kind for indexing, default I4
    int dimCount;                 // rank of DistGrid
    int tileCount;                // number of tiles in DistGrid
    int *minIndexPDimPTile;       // lower corner indices [dimCount*tileCount]
    int *maxIndexPDimPTile;       // upper corner indices [dimCount*tileCount]
    ESMC_I8 *elementCountPTile;   // number of elements [tileCount]
    int *minIndexPDimPDe;         // lower corner indices [dimCount*deCount]
    int *maxIndexPDimPDe;         // upper corner indices [dimCount*deCount]
    ESMC_I8 *elementCountPDe;     // number of elements [deCount]
    int *tileListPDe;             // tile indices [deCount]
    int *contigFlagPDimPDe;       // flag contiguous indices [dimCount*deCount]
    int *indexCountPDimPDe;       // number of indices [dimCount*deCount]
    int **indexListPDimPLocalDe;  // local DEs' indices [dimCount*localDeCount]
                                  // [indexCountPDimPDe(localDe,dim)]
    int connectionCount;          // number of elements in connection list
    int **connectionList;         // connection elements
                                  // [connectionCount][2*dimCount+2]
    void ***arbSeqIndexListPCollPLocalDe;// local arb sequence indices
                                  // [diffCollocationCount][localDeCount]
                                  // [elementCountPCollPLocalDe(localDe)]
    int *collocationPDim;         // collocation [dimCount]
    int diffCollocationCount;     // number different seqIndex collocations
    int *collocationTable;        // collocation in packed format 
                                  // [diffCollocationCount]
    int **elementCountPCollPLocalDe; // number of elements 
                                  // [diffCollocationCount][localDeCount]
    int *regDecomp;               // regular decomposition descriptor
                                  // [dimCount*tileCount]
    Decomp_Flag *decompflag;      // decomposition scheme [dimCount*tileCount]
    ESMC_IndexFlag indexflag;     // index scheme
    // lower level object references
    DELayout *delayout;
    bool delayoutCreator;
    VM *vm;    
    int localDeCountAux;  // auxiliary variable for garbage collection
                          // TODO: remove this variable again once
                          // TODO: reference counting scheme is implemented
                          // TODO: and DELayout cannot be pulled from under
                          // TODO: DistGrid and Array until they are destroyed.
        
   public:
    // native constructor and destructor
    DistGrid(VM *vm=NULL):ESMC_Base(vm){ // allow specific VM instead default
      decompflag = NULL;
      // initialize the name for this DistGrid object in the Base class
      ESMC_BaseSetName(NULL, "DistGrid");
    }
    DistGrid(int baseID):ESMC_Base(baseID){ // prevent baseID counter increment
      decompflag = NULL;
      // initialize the name for this DistGrid object in the Base class
      ESMC_BaseSetName(NULL, "DistGrid");
    }
    ~DistGrid(){destruct(false);}
    
   private:
    // construct() and destruct()
    int construct(int dimCount, int tileCount, int *deTileList,
      int *minIndex, int *maxIndex, int *minIndexPDimPDe, int *maxIndexPDimPDe,
      int *contigFlagPDimPDe, int *indexCountPDimPDe, int **indexList,
      int *regDecompArg, InterArray<int> *connectionList,
      Decomp_Flag const *decompflagArg, ESMC_IndexFlag *indexflagArg,
      DELayout *delayout, bool delayoutCreator, VM *vm, 
      ESMC_TypeKind_Flag indexTKArg);
    int destruct(bool followCreator=true, bool noGarbage=false);
   public:
    // create() and destroy()
    static DistGrid *create(DistGrid *dg,
      InterArray<int> *firstExtra, InterArray<int> *lastExtra,
      ESMC_IndexFlag *indexflag, InterArray<int> *connectionList,
      bool balanceFlag, DELayout *delayout=NULL, VM *vm=NULL,
      bool actualFlag=true, int *rc=NULL);
    static DistGrid *create(InterArray<int> *minIndex,
      InterArray<int> *maxIndex, InterArray<int> *regDecomp, 
      Decomp_Flag *decompflag, int decompflagCount,
      InterArray<int> *regDecompFirstExtra, 
      InterArray<int> *regDecompLastExtra, 
      InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
      InterArray<int> *connectionList,
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL,
      ESMC_TypeKind_Flag indexTK=ESMF_NOKIND);
    static DistGrid *create(InterArray<int> *minIndex,
      InterArray<int> *maxIndex, InterArray<int> *deBlockList, 
      InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
      InterArray<int> *connectionList,
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL,
      ESMC_TypeKind_Flag indexTK=ESMF_NOKIND);
    static DistGrid *create(InterArray<int> *minIndex,
      InterArray<int> *maxIndex, InterArray<int> *deBlockList,
      InterArray<int> *deToTileMap,
      InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag,
      InterArray<int> *connectionList,
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL,
      ESMC_TypeKind_Flag indexTK=ESMF_NOKIND);
    static DistGrid *create(InterArray<int> *minIndex,
      InterArray<int> *maxIndex, InterArray<int> *regDecomp, 
      Decomp_Flag *decompflag, int decompflagCount,
      InterArray<int> *regDecompFirstExtra,
      InterArray<int> *regDecompLastExtra, 
      InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
      InterArray<int> *connectionList,
      int fastAxis, VM *vm=NULL, int *rc=NULL,
      ESMC_TypeKind_Flag indexTK=ESMF_NOKIND);
    static DistGrid *create(InterArray<int> *minIndex,
      InterArray<int> *maxIndex, InterArray<int> *regDecomp, 
      Decomp_Flag *decompflag, int decompflagCount1, int decompflagCount2,
      InterArray<int> *regDecompFirstExtra,
      InterArray<int> *regDecompLastExtra, 
      InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
      InterArray<int> *connectionList,
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL,
      ESMC_TypeKind_Flag indexTK=ESMF_NOKIND);
    static int destroy(DistGrid **distgrid, bool noGarbage=false);
    // is()
    bool isLocalDeOnEdgeL(int localDe, int dim, int *rc) const;
    bool isLocalDeOnEdgeU(int localDe, int dim, int *rc) const;
    // get() and set()
    int getDimCount() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return dimCount;
    }
    int getTileCount() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return tileCount;
    }
    ESMC_IndexFlag getIndexflag() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return indexflag;
    }
    ESMC_TypeKind_Flag getIndexTK() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return indexTK;
    }
    int getDiffCollocationCount() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return diffCollocationCount;
    }
    int const *getMinIndexPDimPTile() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return minIndexPDimPTile;
    }
    int const *getMinIndexPDimPTile(int tile, int *rc) const;
    int const *getMaxIndexPDimPTile() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return maxIndexPDimPTile;
    }
    int const *getMaxIndexPDimPTile(int tile, int *rc) const;
    ESMC_I8 const *getElementCountPTile() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return elementCountPTile;
    }
    int const *getMinIndexPDimPDe() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return minIndexPDimPDe;
    }
    int const *getMinIndexPDimPDe(int de, int *rc) const;
    int const *getMaxIndexPDimPDe() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return maxIndexPDimPDe;
    }
    int const *getMaxIndexPDimPDe(int de, int *rc) const;
    ESMC_I8 const *getElementCountPDe() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return elementCountPDe;
    }
    // name accessors
    const char *getName() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return ESMC_BaseGetName();
    }
    int setName(const char* name) {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return ESMC_BaseSetName(name, "DistGrid");
    }
    int setName(const std::string &name) {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return ESMC_BaseSetName(name.c_str(), "DistGrid");
    }
    // misc. get
    ESMC_I8 getElementCountPDe(int de, int *rc) const;
    int const *getTileListPDe() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return tileListPDe;
    }
    int getTilePLocalDe(int localDe, int *rc) const;
    int const *getContigFlagPDimPDe() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return contigFlagPDimPDe;
    }
    int getContigFlagPDimPDe(int de, int dim, int *rc) const;
    int const *getIndexCountPDimPDe() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return indexCountPDimPDe;
    }
    int const *getIndexListPDimPLocalDe(int localDe, int dim, int *rc=NULL)
      const;
    int getConnectionCount() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return connectionCount;
    }
    int *const *getConnectionList() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return connectionList;
    }
    int const *getCollocationPDim() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return collocationPDim;
    }
    int const *getCollocationTable() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return collocationTable;
    }
    DELayout *getDELayout() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return delayout;
    }
    int const *getRegDecomp() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return regDecomp;
    }
    // topology discovery
    template<typename T> int getSequenceIndexLocalDe(int localDe, 
      int const *index, std::vector<T> &seqIndex, bool recursive=true,
      bool canonical=false) const;
    template<typename T> int tGetSequenceIndexLocalDe(T ***t, int de,
      int localDe, int const *index, std::vector<T> &seqIndex, 
      bool recursive=true, bool canonical=false) const;
    template<typename T> int getSequenceIndexTileRelative(int tile,
      int const *index, T *seqIndex)const;
    template<typename T> int getSequenceIndexTile(int tile, int const *index,
      std::vector<T> &seqIndex, bool recursive=true)const;
    template<typename T> int getSequenceIndexTileRecursive(int tile,
      int const *index, int depth, int hops, std::vector<T> &seqIndex)const;
    int getIndexTupleFromSeqIndex(int seqIndex, std::vector<int> &indexTuple,
      int &tile) const;
    // get/set arb sequence indices
    int *const *getElementCountPCollPLocalDe() const {
      if (ESMC_BaseGetStatus()!=ESMF_STATUS_READY) throw ESMC_RC_OBJ_DELETED;
      return elementCountPCollPLocalDe;
    }
    void const *getArbSeqIndexList(int localDe, int collocation=1, int *rc=NULL)
      const;
    template<typename T> int setArbSeqIndex(std::vector<T> &arbSeqIndex, 
      int localDe, int collocation=1);
    template<typename T> int setArbSeqIndex(InterArray<T> *arbSeqIndex, 
      int localDe, int collocation=1);
    int setArbSeqIndex(void *ptr, int localDe, int collocation=1);
    int setCollocationPDim(InterArray<int> *collocationPDim);
    // fill()
    template<typename T> int fillSeqIndexList(std::vector<T> &seqIndexList,
      int localDe, int collocation=1) const;
    template<typename T> int fillSeqIndexList(InterArray<T> *seqIndexList,
      int localDe, int collocation=1) const;
    int fillIndexListPDimPDe(int *indexList, int de, int dim,
      VMK::commhandle **commh, int rootPet, VM *vm=NULL) const;
    // misc.
    void log(std::string prefix,
      ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO, bool deepFlag=false)const;
    static DistGridMatch_Flag match(DistGrid *distgrid1, DistGrid *distgrid2,
      int *rc=NULL);
    int print() const;
    int validate() const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset, ESMC_InquireFlag)
      const;
    static DistGrid *deserialize(char *buffer, int *offset);
    // connections
    static int connection(InterArray<int> *connection, int tileIndexA, 
      int tileIndexB, InterArray<int> *positionVector,
      InterArray<int> *orientationVector);
    // regDecomp
    static int regDecompSetCubic(InterArray<int> *regDecomp, int deCount);
  };  // class DistGrid

  
  
  //============================================================================
  class MultiDimIndexLoop{
    // Iterator type through regular multidimensional structures.
   protected:
    std::vector<int> indexTupleStart;
    std::vector<int> indexTupleEnd;
    std::vector<int> indexTuple;
    std::vector<bool> skipDim;
    std::vector<int> indexTupleBlockStart; // blocked region
    std::vector<int> indexTupleBlockEnd;   // blocked region
    std::vector<int> indexTupleWatchStart; // watched region
    std::vector<int> indexTupleWatchEnd;   // watched region
    bool skipBlockedRegionFlag;
    void *seqIndex;                   // sequence index of current iterator
    DistGrid const *distgrid;         // DistGrid for sequence index optimiz.
    int localDe;                      // for sequence index look up optimization
    bool arbSeq;                      // arbitrary sequence indices present
   public:
    MultiDimIndexLoop();
    MultiDimIndexLoop(std::vector<int> const &sizes,
      bool seqIndexEnabled=false, const DistGrid *distgrid=NULL, int localDe=-1);
    MultiDimIndexLoop(std::vector<int> const &offsets,
      std::vector<int> const &sizes);
    ~MultiDimIndexLoop(){
      if (seqIndex){
        if (distgrid->getIndexTK()==ESMC_TYPEKIND_I4)
          delete (ESMC_I4*) seqIndex;
        else if (distgrid->getIndexTK()==ESMC_TYPEKIND_I8)
          delete (ESMC_I8*) seqIndex;
      }
    }
    template<typename T> T getSequenceIndex()const{
      if (seqIndex){
        T tempSeqIndex;
        ESMC_I8 controlSeqIndex;
        if (distgrid->getIndexTK()==ESMC_TYPEKIND_I4){
          tempSeqIndex = (T)(*(ESMC_I4*)seqIndex);
          controlSeqIndex = (ESMC_I8)(*(ESMC_I4*)seqIndex);
        }else if (distgrid->getIndexTK()==ESMC_TYPEKIND_I8){
          tempSeqIndex = (T)(*(ESMC_I8*)seqIndex);
          controlSeqIndex = (ESMC_I8)(*(ESMC_I8*)seqIndex);
        }
        if ((ESMC_I8)tempSeqIndex == controlSeqIndex)
          return tempSeqIndex;  // no truncation detected, okay to return
      }
      throw ESMC_RC_ARG_BAD;
    }
    void setSkipDim(int dim);
    void setBlockStart(std::vector<int> const &blockStart);
    void setBlockEnd(std::vector<int> const &blockEnd);
    void setWatchStart(std::vector<int> const &watchStart);
    void setWatchEnd(std::vector<int> const &watchEnd);
    void first();
    void last();
    bool adjust();
    bool next();
    bool isFirst()const;
    bool isLast()const;
    bool isWithin()const;
    bool isWithinBlock(int dim)const;
    bool isWithinWatch()const;
    int const *getIndexTuple()const;
    int const *getIndexTupleEnd()const;
    int const *getIndexTupleStart()const;
    void log(ESMC_LogMsgType_Flag msgType=ESMC_LOGMSG_INFO)const;
  };  // class MultiDimIndexLoop
  //============================================================================


} // namespace ESMCI

#endif  // ESMCI_DistGrid_H

