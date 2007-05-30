// $Id: ESMC_DistGrid.h,v 1.15 2007/05/30 17:46:20 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC DistGrid include file for C++

// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_DistGrid_H
#define ESMC_DistGrid_H

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_DistGrid - DistGrid
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt DistGrid} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_DistGrid.C}
// contains the full code (bodies) for the {\tt DistGrid} methods.
//
///EOP
//-------------------------------------------------------------------------

#include "ESMC_Base.h"      // Base is superclass to DistGrid
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"


// constants and enums

enum ESMC_DecompFlag {ESMF_DECOMP_DEFAULT=1, ESMF_DECOMP_HOMOGEN,
  ESMF_DECOMP_RESTFIRST, ESMF_DECOMP_RESTLAST, ESMF_DECOMP_CYCLIC};

// classes

class ESMC_DistGrid;



// class definition
class ESMC_DistGrid : public ESMC_Base {    // inherits from ESMC_Base class

  private:
    int dimCount;                 // rank of DistGrid
    int patchCount;               // number of patches in DistGrid
    int *patchCellCount;          // number of cells for each patch
    int *dePatchList;             // patch index per DE
    int *minIndex;               // minIndex for all patches
    int *maxIndex;               // maxIndex for all patches
    int *deCellCount;             // number of cells for each DE
    int *dimContigFlag;           // flag contiguous indices by DE per dim
    int *dimExtent;               // extent of indexList held by DE per dim
    int **indexList;              // indices held by all DEs per dim
    int **localIndexList;         // indices held by local DEs per dim
    ESMC_Logical regDecompFlag;   // flag indicating regular decomposition
    int **connectionList;         // list of connection elements
    int connectionCount;          // number of elements in connection list
    // lower level objects
    ESMC_DELayout *delayout;
    ESMC_VM *vm;    
    // cached values from DELayout
    int deCount;
    int localDeCount;
    int *localDeList;
    // cached values from VM
    int localPet;
    int petCount;
        
  public:
    // Construct and Destruct
    int ESMC_DistGridConstruct(int dimCount, int patchCount, int *dePatchList,
      int *minIndex, int *maxIndex, int *dimContigFlag, 
      int *dimExtent, int **indexList, ESMC_Logical regDecompFlagArg,
      ESMC_InterfaceInt *connectionList, ESMC_DELayout *delayout, ESMC_VM *vm);
    int ESMC_DistGridDestruct(void);
    // Get, Set
    int ESMC_DistGridGet(ESMC_DELayout **delayoutArg=NULL,
      int *patchCount=NULL, ESMC_InterfaceInt *patchList=NULL,
      int *dimCountArg=NULL, ESMC_InterfaceInt *dimExtentArg=NULL, 
      ESMC_Logical *regDecompFlagArg=NULL);
    int ESMC_DistGridGet(int de, int *cellCount);
    int ESMC_DistGridGet(int de, int dim, ESMC_InterfaceInt *indexList=NULL,
      int *contigFlag=NULL);
    int ESMC_DistGridGetLocal(int de, int dim, 
      ESMC_InterfaceInt *localIndexList=NULL);
    int ESMC_DistGridGetSequenceIndex(int de, int *index);
    int ESMC_DistGridGetSequenceDe(int seqindex);
    int ESMC_DistGridGetPatchMinmaxIndex(int patch, int *minIndex, int
      *maxIndex);
    // IO and validation
    int ESMC_DistGridPrint(void);
    int ESMC_DistGridSerialize(char *buffer, int *length, int *offset);
    friend ESMC_DistGrid *ESMC_DistGridDeserialize(char *buffer, int *offset);
};  // end class ESMC_DistGrid


// external methods:
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minIndex,
  ESMC_InterfaceInt *maxIndex, ESMC_InterfaceInt *regDecomp, 
  ESMC_DecompFlag *decompflag, int decompflagCount,
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  ESMC_DELayout *delayout=NULL, ESMC_VM *vm=NULL, int *rc=NULL);
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minIndex,
  ESMC_InterfaceInt *maxIndex, ESMC_InterfaceInt *deBlockList, 
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  ESMC_DELayout *delayout=NULL, ESMC_VM *vm=NULL, int *rc=NULL);
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minIndex,
  ESMC_InterfaceInt *maxIndex, ESMC_InterfaceInt *regDecomp, 
  ESMC_DecompFlag *decompflag, int decompflagCount,
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  int fastAxis, ESMC_VM *vm=NULL, int *rc=NULL);
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minIndex,
  ESMC_InterfaceInt *maxIndex, ESMC_InterfaceInt *regDecomp, 
  ESMC_DecompFlag *decompflag, int decompflagCount1, int decompflagCount2,
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  ESMC_DELayout *delayout=NULL, ESMC_VM *vm=NULL, int *rc=NULL);
int ESMC_DistGridDestroy(ESMC_DistGrid **distgrid);
  
ESMC_DistGrid *ESMC_DistGridDeserialize(char *buffer, int *offset);

int ESMC_Connection(
  ESMC_InterfaceInt *connection, int patchIndexA, int patchIndexB,
  ESMC_InterfaceInt *positionVector,
  ESMC_InterfaceInt *orientationVector);


#endif  // ESMC_DistGrid_H

