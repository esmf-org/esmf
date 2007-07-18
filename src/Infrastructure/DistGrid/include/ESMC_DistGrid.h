// $Id: ESMC_DistGrid.h,v 1.25 2007/07/18 22:05:06 theurich Exp $
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

#include "ESMC_Base.h"      // Base is superclass to DistGrid
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"

//-------------------------------------------------------------------------

namespace ESMCI {

// constants and enums

enum DecompFlag {DECOMP_DEFAULT=1, DECOMP_HOMOGEN,
  DECOMP_RESTFIRST, DECOMP_RESTLAST, DECOMP_CYCLIC};

// classes

class DistGrid;

// class definition
class DistGrid : public ESMC_Base {    // inherits from ESMC_Base class

  private:
    int dimCount;                 // rank of DistGrid
    int patchCount;               // number of patches in DistGrid
    int *cellCountPPatch;         // number of cells [patchCount]
    int *patchListPDe;            // patch indices [deCount]
    int *minIndexPDimPPatch;      // lower corner indices [dimCount*patchCount]
    int *maxIndexPDimPPatch;      // upper corner indices [dimCount*patchCount]
    int *cellCountPDe;            // number of cells [deCount]
    int *contigFlagPDimPDe;       // flag contiguous indices [dimCount*deCount]
    int *indexCountPDimPDe;       // number of indices [dimCount*deCount]
    int **indexListPDimPLocalDe;  // local DEs' indices [dimCount*localDeCount]
                                  // [indexCountPDimPDe(localDe,dim)]
    ESMC_Logical regDecompFlag;   // flag indicating regular decomposition
    int **connectionList;         // list of connection elements
    int connectionCount;          // number of elements in connection list
    int *arbSeqIndexCountPLocalDe;// number of arb seq. indices [localDeCount]
    int **arbSeqIndexListPLocalDe;// local arb sequence indices [localDeCount]
                                  // [arbSeqIndexCountPLocalDe(localDe)]
    // lower level object references
    DELayout *delayout;
    VM *vm;    
        
  private:
    // construct() and destruct()
    int construct(int dimCount, int patchCount, int *dePatchList,
      int *minIndex, int *maxIndex, int *contigFlagPDimPDe, 
      int *indexCountPDimPDe, int **indexList, ESMC_Logical regDecompFlagArg,
      InterfaceInt *connectionList, DELayout *delayout, VM *vm);
    int destruct();
  public:
    // create() and destroy()
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *regDecomp, 
      DecompFlag *decompflag, int decompflagCount,
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      InterfaceInt *connectionTransformList, 
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL);
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *deBlockList, 
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      InterfaceInt *connectionTransformList, 
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL);
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *regDecomp, 
      DecompFlag *decompflag, int decompflagCount,
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      InterfaceInt *connectionTransformList, 
      int fastAxis, VM *vm=NULL, int *rc=NULL);
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *regDecomp, 
      DecompFlag *decompflag, int decompflagCount1, int decompflagCount2,
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      InterfaceInt *connectionTransformList, 
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL);
    static int destroy(DistGrid **distgrid);
    // get() and set()
    int getDimCount()               const {return dimCount;}
    int getPatchCount()             const {return patchCount;}
    const int *getPatchCellCount()  const {return cellCountPPatch;}
    const int *getDePatchList()     const {return patchListPDe;}
    const int *getMinIndex()        const {return minIndexPDimPPatch;}
    const int *getMinIndex(int patch, int *rc) const;
    const int *getMaxIndex()        const {return maxIndexPDimPPatch;}
    const int *getMaxIndex(int patch, int *rc) const;
    const int *getDeCellCount()     const {return cellCountPDe;}
    int getDeCellCount(int de, int *rc) const;
    const int *getDimContigFlag()   const {return contigFlagPDimPDe;}
    int getDimContigFlag(int de, int dim, int *rc) const;
    const int *getDimExtent()       const {return indexCountPDimPDe;}
    const int *getLocalIndexList(int localDe, int dim, int *rc=NULL) const;
    DELayout *getDELayout()         const {return delayout;}
    ESMC_Logical getRegDecompFlag() const {return regDecompFlag;}
    int getSequenceIndex(int localDe, int *index, int *rc=NULL) const;
    int setArbSeqIndex(InterfaceInt *arbSeqIndex);
    // misc.
    int print() const;
    int validate() const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset) const;
    static DistGrid *deserialize(char *buffer, int *offset);
    // connections
    static int connection(InterfaceInt *connection, int patchIndexA, 
      int patchIndexB, InterfaceInt *positionVector,
      InterfaceInt *orientationVector);
};  // class DistGrid

} // namespace ESMCI

#endif  // ESMCI_DistGrid_H

