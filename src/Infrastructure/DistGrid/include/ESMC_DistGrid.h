// $Id: ESMC_DistGrid.h,v 1.5 2006/04/14 16:17:26 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
    int *dimExtent;               // extent of indexList held by DE per dim
    int **indexList;              // indices held by DE per dim
    ESMC_Logical regDecompFlag;   // flag indicating regular decomposition
    int **connectionElementList;  // list of connection elements
    int connectionElementCount;   // number of elements in connection elem. list
    // lower level objects
    ESMC_DELayout *delayout;
    ESMC_VM *vm;    
    // cached values from DELayout
    int deCount;
    // cached values from VM
    int localPet;
    int petCount;
        
  public:
    // Construct and Destruct
    int ESMC_DistGridConstruct(int dimCount, int patchCount, int *dimExtent, 
      int **indexList, ESMC_Logical regDecompFlagArg, 
      ESMC_InterfaceInt *connectionList,
      ESMC_DELayout *delayout, ESMC_VM *vm);
    int ESMC_DistGridDestruct(void);
    // Get, Set
    int ESMC_DistGridGet(ESMC_DELayout **delayoutArg,
      ESMC_InterfaceInt *patchList, int *dimCountArg,
      ESMC_InterfaceInt *dimExtentArg, ESMC_Logical *regDecompFlagArg);
    int ESMC_DistGridGet(int de, int dim, ESMC_InterfaceInt *indexList);
    // IO and validation
    int ESMC_DistGridPrint(void);
      
};  // end class ESMC_DistGrid


// external methods:
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minCorner,
  ESMC_InterfaceInt *maxCorner, ESMC_InterfaceInt *regDecomp, 
  ESMC_DecompFlag *decompflag, int decompflagCount,
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  ESMC_DELayout *delayout=NULL, ESMC_VM *vm=NULL, int *rc=NULL);
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minCorner,
  ESMC_InterfaceInt *maxCorner, ESMC_InterfaceInt *deBlockList, 
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  ESMC_DELayout *delayout=NULL, ESMC_VM *vm=NULL, int *rc=NULL);
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minCorner,
  ESMC_InterfaceInt *maxCorner, ESMC_InterfaceInt *regDecomp, 
  ESMC_DecompFlag *decompflag, int decompflagCount,
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  int fastAxis, ESMC_VM *vm=NULL, int *rc=NULL);
ESMC_DistGrid *ESMC_DistGridCreate(ESMC_InterfaceInt *minCorner,
  ESMC_InterfaceInt *maxCorner, ESMC_InterfaceInt *regDecomp, 
  ESMC_DecompFlag *decompflag, int decompflagCount1, int decompflagCount2,
  ESMC_InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
  ESMC_InterfaceInt *connectionList,
  ESMC_InterfaceInt *connectionTransformList, 
  ESMC_DELayout *delayout=NULL, ESMC_VM *vm=NULL, int *rc=NULL);
int ESMC_DistGridDestroy(ESMC_DistGrid **distgrid);
  

int ESMC_ConnectionElementConstruct(
  ESMC_InterfaceInt *connectionElement, int patchIndexA, int patchIndexB,
  ESMC_InterfaceInt *positionVector,
  ESMC_InterfaceInt *orientationVector);


#endif  // ESMC_DistGrid_H

