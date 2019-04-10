// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
    ESMC_IndexFlag *indexflag;    // index scheme
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
      indexflag = NULL;
      // initialize the name for this DistGrid object in the Base class
      ESMC_BaseSetName(NULL, "DistGrid");
    }
    DistGrid(int baseID):ESMC_Base(baseID){ // prevent baseID counter increment
      decompflag = NULL;
      indexflag = NULL;
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
    int getDimCount() const {return dimCount;}
    int getTileCount() const {return tileCount;}
    ESMC_TypeKind_Flag getIndexTK() const {return indexTK;}
    int getDiffCollocationCount() const {return diffCollocationCount;}
    int const *getMinIndexPDimPTile() const {return minIndexPDimPTile;}
    int const *getMinIndexPDimPTile(int tile, int *rc) const;
    int const *getMaxIndexPDimPTile() const {return maxIndexPDimPTile;}
    int const *getMaxIndexPDimPTile(int tile, int *rc) const;
    ESMC_I8 const *getElementCountPTile() const {return elementCountPTile;}
    int const *getMinIndexPDimPDe() const {return minIndexPDimPDe;}
    int const *getMinIndexPDimPDe(int de, int *rc) const;
    int const *getMaxIndexPDimPDe() const {return maxIndexPDimPDe;}
    int const *getMaxIndexPDimPDe(int de, int *rc) const;
    ESMC_I8 const *getElementCountPDe() const {return elementCountPDe;}
    // misc. get
    ESMC_I8 getElementCountPDe(int de, int *rc) const;
    int const *getTileListPDe() const {return tileListPDe;}
    int const *getContigFlagPDimPDe() const {return contigFlagPDimPDe;}
    int getContigFlagPDimPDe(int de, int dim, int *rc) const;
    int const *getIndexCountPDimPDe() const {return indexCountPDimPDe;}
    int const *getIndexListPDimPLocalDe(int localDe, int dim, int *rc=NULL)
      const;
    int getConnectionCount() const {return connectionCount;}
    int *const *getConnectionList() const {return connectionList;}
    int const *getCollocationPDim() const {return collocationPDim;}
    int const *getCollocationTable() const {return collocationTable;}
    DELayout *getDELayout() const {return delayout;}
    int const *getRegDecomp() const {return regDecomp;}
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
    int *const *getElementCountPCollPLocalDe()
      const {return elementCountPCollPLocalDe;}
    void const *getArbSeqIndexList(int localDe, int collocation=1, int *rc=NULL)
      const;
    template<typename T> int setArbSeqIndex(InterArray<T> *arbSeqIndex, 
      int localDe, int collocation=1);
    int setArbSeqIndex(void *ptr, int localDe, int collocation=1);
    int setCollocationPDim(InterArray<int> *collocationPDim);
    // fill()
    template<typename T> int fillSeqIndexList(InterArray<T> *seqIndexList,
      int localDe, int collocation=1) const;
    int fillSeqIndexList(std::vector<int> &seqIndexList, int localDe,
      int collocation=1) const;
    int fillIndexListPDimPDe(int *indexList, int de, int dim,
      VMK::commhandle **commh, int rootPet, VM *vm=NULL) const;
    // misc.
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
   public:
    MultiDimIndexLoop();
    MultiDimIndexLoop(std::vector<int> const &sizes);
    MultiDimIndexLoop(std::vector<int> const &offsets,
      std::vector<int> const &sizes);
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
    void log()const;
  };  // class MultiDimIndexLoop
  //============================================================================


} // namespace ESMCI

#endif  // ESMCI_DistGrid_H

