// $Id: ESMC_DistGrid.h,v 1.21 2007/06/26 23:01:31 theurich Exp $
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
    int *patchCellCount;          // number of cells for each patch
    int *dePatchList;             // patch index per DE
    int *minIndex;                // minIndex for all patches
    int *maxIndex;                // maxIndex for all patches
    int *deCellCount;             // number of cells for each DE
    int *dimContigFlag;           // flag contiguous indices by DE per dim
    int *dimExtent;               // extent of indexList held by DE per dim
    int **indexList;              // indices held by all DEs per dim
    int **localIndexList;         // indices held by local DEs per dim
    ESMC_Logical regDecompFlag;   // flag indicating regular decomposition
    int **connectionList;         // list of connection elements
    int connectionCount;          // number of elements in connection list
    int arbIdxCount;              // number of local arbitrary indices
    int *localArbIndices;         // local arbitrary indices for 1d unstr. case
    // lower level objects
    DELayout *delayout;
    VM *vm;    
    // cached values from DELayout
    int deCount;
    int localDeCount;
    int *localDeList;
    // cached values from VM
    int localPet;
    int petCount;
        
  private:
    // construct() and destruct()
    int construct(int dimCount, int patchCount, int *dePatchList,
      int *minIndex, int *maxIndex, int *dimContigFlag, 
      int *dimExtent, int **indexList, ESMC_Logical regDecompFlagArg,
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
    const int *getPatchCellCount()  const {return patchCellCount;}
    const int *getDePatchList()     const {return dePatchList;}
    const int *getMinIndex()        const {return minIndex;}
    const int *getMinIndex(int patch, int *rc) const;
    const int *getMaxIndex()        const {return maxIndex;}
    const int *getMaxIndex(int patch, int *rc) const;
    const int *getDeCellCount()     const {return deCellCount;}
    int getDeCellCount(int de, int *rc) const;
    const int *getDimContigFlag()   const {return dimContigFlag;}
    int getDimContigFlag(int de, int dim, int *rc) const;
    const int *getDimExtent()       const {return dimExtent;}
    const int *getLocalIndexList(int de, int dim, int *rc) const;
    DELayout *getDelayout()         const {return delayout;}
    ESMC_Logical getRegDecompFlag() const {return regDecompFlag;}
    int getSequenceIndex(int de, int *index) const;
    int getSequenceDe(int seqindex) const;
    //TODO: remove the getIndexList() call when DistGrid is free of global index
    int *const*getIndexList()       const {return indexList;}
    int setArbIdx(InterfaceInt *argIndices);
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

