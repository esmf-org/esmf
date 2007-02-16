// $Id: ESMC_DELayout.h,v 1.24 2007/02/16 05:27:43 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC DELayout include file for C++

// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_DELayout_H
#define ESMC_DELayout_H

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_DELayout - DELayout
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt DELayout} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_DELayout.C}
// contains the full code (bodies) for the {\tt DELayout} methods.
//
///EOP
//-------------------------------------------------------------------------

#include <ESMC_Base.h>      // Base is superclass to DELayout
#include <ESMC_VM.h>  


// constants and enums
const int ESMC_CWGHT_NORMAL = 50;

enum ESMC_DELayoutServiceReply {ESMC_DELAYOUT_SERVICE_ACCEPT=1,
  ESMC_DELAYOUT_SERVICE_DENY};


// classes

class ESMC_DELayout;


// DE type used internally in the ESMC_DELayout class
typedef struct{
  int de;           // DE id number (in case not base zero)
  int pet;          // PET associated with this DE
  int vas;          // virtual address space
  // - DEPRICATED section
  int nconnect;     // number of connections from this DE
  int *connect_de;  // connected DEs 
  int *connect_w;   // connection weight
  int *coord;       // coordinates of this DE in the layout
}de_type;
  

// class definition
class ESMC_DELayout : public ESMC_Base {    // inherits from ESMC_Base class
  private:
    // --- global section ---
    ESMC_VM *vm;    // ptr to this PET's VM instance this layout is running on
    ESMC_Logical oneToOneFlag;  // indicate whether this is a 1-to-1 layout
    int deCount;    // number of DEs
    de_type *deList;// list that holds all of this layout's DE info
    // --- local section ---
    int localDeCount;// number of DEs associated with instantiating PET
    int *localDeList;// list that holds all of the de indices for this PET
    
    int oldstyle;   // if this flag is set then this is an oldstyle delayout
                    // new style delayouts follow proposal sent out on 02/15/06
    
    // - NEWSTYLE section
    ESMC_DePinFlag dePinFlag; // type of resources DEs are pinned to    
    
    int vasLocalDeCount;// number of DEs associated with local VAS
    int *vasLocalDeList;// list that holds all of the de indices for this VAS
    
    // - DEPRICATED section
    int ndim;       // dimensionality of this layout
    ESMC_Logical logRectFlag;   // indicate whether this is logical rectangular
    int *dims;      // sizes of dimensions in a logical rectangular layout
    
    // - NEWSTYLE work queue   
    int *localServiceOfferCount;// number of times local PET offered service for
                                // a vasLocal DE
    int *maxServiceOfferCount;  // maximum times service for a DE was offered by
                                // any PET (shared memory variable)
    vmk_ipmutex **serviceOfferMutex; // list of shared mutex between PETs
    vmk_ipmutex **serviceMutex; // list of shared mutex between PETs
    int *serviceMutexFlag;      // local flag to indicate that PET holds mutex
    
  public:
    // Construct and Destruct
    int ESMC_DELayoutConstruct(ESMC_VM *vmArg=ESMC_NULL_POINTER, 
      ESMC_DePinFlag *dePinFlagArg=ESMC_NULL_POINTER, 
      int *petMap=ESMC_NULL_POINTER, int petMapCount=0);
    int ESMC_DELayoutDestruct(void);
    // Get, Set
    int ESMC_DELayoutGet(ESMC_VM **vmArg, int *deCountArg, int *petMap, 
      int petMapCount, int *vasMap, int vasMapCount, 
      ESMC_Logical *oneToOneFlagArg, ESMC_DePinFlag *dePinFlagArg,
      int *localDeCountArg, int *localDeListArg, int localDeListCount,
      int *vasLocalDeCountArg, int *vasLocalDeListArg, int vasLocalDeListCount);
    // IO and validation
    friend ESMC_DELayout *ESMC_DELayoutDeserialize(char *buffer, int *offset);
    int ESMC_DELayoutPrint(void);
    int ESMC_DELayoutSerialize(char *buffer, int *length, int *offset);
    int ESMC_DELayoutValidate(void);
    // Synchronization
    ESMC_DELayoutServiceReply ESMC_DELayoutServiceOffer(int de, int *rc);
    int ESMC_DELayoutServiceComplete(int de);
    
    // - DEPRICATED section
    
    // Construct and Destruct
    int ESMC_DELayoutConstruct1D(ESMC_VM &vm, int nDEs, int *DEtoPET,  
      int len, ESMC_Logical cyclic);
    int ESMC_DELayoutConstructND(ESMC_VM &vm, int *nDEs, int nndim, 
      int *DEtoPET, int len, ESMC_Logical cyclic);
    // Get
    int ESMC_DELayoutGetVM(ESMC_VM **vm);
    int ESMC_DELayoutGetDeprecated(int *nDEs, int *ndim, int *nmyDEs, 
      int *myDEs, int len_myDEs, int *localDe, ESMC_Logical *oneToOneFlag,
      ESMC_Logical *logRectFlag, int *deCountPerDim, int len_deCountPerDim);
    int ESMC_DELayoutGetDELocalInfo(int DEid, int *DEcoord, int len_coord, 
      int *DEcde, int len_cde, int *DEcw, int len_cw, int *nDEc, int *pid);
    // Matching functions
    int ESMC_DELayoutGetDEMatchDE(int DEid, ESMC_DELayout &layoutMatch,
      int *deMatchCount, int *deMatchList, int len_deMatchList);
    int ESMC_DELayoutGetDEMatchPET(int DEid, ESMC_VM &vmMatch,
      int *petMatchCount, int *petMatchList, int len_petMatchList);
        
    // ================ don't promote DELayout Comms =================
  private:
    // Communication
    int ESMC_DELayoutCopy(void *srcdata, void *destdata, 
      int blen, int srcDE, int destDE);
    int ESMC_DELayoutCopy(void *srcdata, void *destdata, 
      int len, ESMC_TypeKind dtk, int srcDE, int destDE);
    int ESMC_DELayoutExchange(void *srcData1, void *srcData2, 
      void *dstData1, void *dstData2, int blen1, int blen2, int de1, int de2);
    int ESMC_DELayoutExchange(void *srcData1, void *srcData2, 
      void *dstData1, void *dstData2, int len1, int len2, ESMC_TypeKind dtk1,
      ESMC_TypeKind dtk2, int de1, int de2);
    int ESMC_DELayoutBcast(void *data, int blen, int rootDE);
    int ESMC_DELayoutBcast(void *data, int len, ESMC_TypeKind dtk, int rootDE);
    int ESMC_DELayoutScatter(void *srcdata, void *destdata, 
      int blen, int rootDE);
    int ESMC_DELayoutScatter(void *srcdata, void *destdata, 
      int len, ESMC_TypeKind dtk, int rootDE);
    int ESMC_DELayoutGather(void *srcdata, void *destdata, 
      int blen, int rootDE);
    int ESMC_DELayoutGather(void *srcdata, void *destdata, 
      int len, ESMC_TypeKind dtk, int rootDE);
    int ESMC_DELayoutGatherV(void *srcdata, void *destdata, 
      int *blen, int *bdestdispl, int rootDE);
    // ArrayComm.C uses the following DELayoutComm, so I leave it for now
  public:  
    int ESMC_DELayoutGatherV(void *srcdata, void *destdata, 
      int *blen, int *bdestdispl, ESMC_TypeKind dtk, int rootDE);
    
    
  private:
    int ESMC_DELayoutFindDEtoPET(int npets);
    int ESMC_DELayoutFillLocal(int mypet);        
};  // end class ESMC_DELayout


// external methods:  

ESMC_DELayout *ESMC_DELayoutCreate(int *petMap, int petMapCount,
  ESMC_DePinFlag *dePinFlag, ESMC_VM *vm=NULL, int *rc=NULL);
ESMC_DELayout *ESMC_DELayoutCreate(int *deCount=NULL, 
  ESMC_InterfaceInt *deGrouping=NULL, ESMC_DePinFlag *dePinFlag=NULL,
  ESMC_InterfaceInt *petList=NULL, ESMC_VM *vm=NULL, int *rc=NULL);
int ESMC_DELayoutDestroy(ESMC_DELayout **layout);
  

ESMC_DELayout *ESMC_DELayoutDeserialize(char *buffer, int *offset);


// ==== This section contains  D E P R I C A T E D  code =======================

ESMC_DELayout *ESMC_DELayoutCreate(ESMC_VM &vm, int *nDEs, int ndim, 
  int *DEtoPET, int len, ESMC_Logical *cyclic, int *rc);  // depricated

void **ESMC_DELayoutDataCreate(int n, int *rc);
int ESMC_DELayoutDataAdd(void **ptr, void *a, int index);
int ESMC_DELayoutDataDestroy(void **ptr);



#endif  // ESMC_DELayout_H

