// $Id: ESMC_newDELayout.h,v 1.11 2004/04/09 19:48:49 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC DELayout include file for C++

// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_newDELayout_H
#define ESMC_newDELayout_H

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_newDELayout - DELayout
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt newDELayout} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_newDELayout.C}
// contains the full code (bodies) for the {\tt newDELayout} methods.
//
///EOP
//-------------------------------------------------------------------------

#include <ESMC_Base.h>  
#include <ESMC_VM.h>  

// normal connection weight
const int ESMF_CWGHT_NORMAL = 50;


class ESMC_newDELayout;


// DE type used internally in the ESMC_newDELayout class
typedef struct{
  int deid;         // DE's external id number (in case not base zero)
  int petid;        // Id of the PET associated with this DE
  int pid;          // absolute process ID, specifying virtual memory space
  int nconnect;     // number of connections from this DE
  int *connect_de;  // connected DEs 
  int *connect_w;   // connection weight
  int *coord;       // coordinates of this DE in the layout
}de_type;
  

// class definition
class ESMC_newDELayout : public ESMC_Base {    // inherits from ESMC_Base class
  private:
    ESMC_VM *myvm;  // ptr to this PET's VM instance this layout is running on
    int ndes;       // number of DEs
    de_type *des;   // list that holds all of this layout's DE info
    int nmydes;     // number of DEs associated with instantiating PET
    int *mydes;     // list that holds all of the des indices for this instance
    int ndim;       // dimensionality of this layout
    ESMC_Logical oneToOneFlag;  // indicate whether this is a 1-to-1 layout
    ESMC_Logical logRectFlag;   // indicate whether this is logical rectangular
    int *dims;      // sizes of dimensions in a logical rectangular layout
  public:
    // Construct and Destruct
    int ESMC_newDELayoutConstruct1D(ESMC_VM &vm, int nDEs, int *DEtoPET,  
      int len, ESMC_Logical cyclic);
    int ESMC_newDELayoutConstructND(ESMC_VM &vm, int *nDEs, int nndim, 
      int *DEtoPET, int len, ESMC_Logical cyclic);
    int ESMC_newDELayoutDestruct(void);
    // Get info
    int ESMC_newDELayoutGet(int *nDEs, int *ndim, int *nmyDEs, int *myDEs, 
      int len_myDEs, int *localDe, ESMC_Logical *oneToOneFlag,
      ESMC_Logical *logRectFlag, int *deCountPerDim, int len_deCountPerDim);
    int ESMC_newDELayoutGetDE(int DEid, int *DEcoord, int len_coord, 
      int *DEcde, int len_cde, int *DEcw, int len_cw, int *nDEc);
    int ESMC_newDELayoutGetDEMatch(int DEid, ESMC_newDELayout &layoutMatch,
      int *deMatchCount, int *deMatchList, int len_deMatchList);
    int ESMC_newDELayoutMyDE(int DE, ESMC_Logical *value);
    // IO
    int ESMC_newDELayoutPrint(void);
    // Communication
    int ESMC_newDELayoutCopyCopy(void **srcData1, void **srcData2, 
      void **dstData1, void **dstData2, int len1, int len2, int de1, int de2,
      ESMC_Logical oneToOneFlag);
    int ESMC_newDELayoutCopy(void **srcdata, void **destdata, 
      int len, int srcDE, int destDE, ESMC_Logical oneToOneFlag);
    int ESMC_newDELayoutScatter(void **srcdata, void **destdata, 
      int len, int rootDE, ESMC_Logical oneToOneFlag);
    int ESMC_newDELayoutGather(void **srcdata, void **destdata, 
      int len, int rootDE, ESMC_Logical oneToOneFlag);
    int ESMC_newDELayoutAllGlobalReduce(void **srcdata, void *result, int len,
      ESMC_DataKind dtk, ESMC_newOp op, ESMC_Logical oneToOneFlag);
  private:
    int ESMC_newDELayout::ESMC_newDELayoutFindDEtoPET(int npets);
    int ESMC_newDELayout::ESMC_newDELayoutFillLocal(int mypet);        
};  // end class ESMC_newDELayout


// external methods:  

ESMC_newDELayout *ESMC_newDELayoutCreate(ESMC_VM &vm, int *nDEs, int ndim, 
  int *DEtoPET, int len, ESMC_Logical *cyclic, int *rc);

int ESMC_newDELayoutDestroy(ESMC_newDELayout *layout);
  
void **ESMC_newDELayoutDataCreate(int n, int *rc);
int ESMC_newDELayoutDataAdd(void **ptr, void *a, int index);
int ESMC_newDELayoutDataDestroy(void **ptr);

#endif  // ESMC_newDELayout_H
