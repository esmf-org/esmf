// $Id: ESMC_DELayout.h,v 1.26 2007/06/20 01:29:20 theurich Exp $
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

#ifndef ESMC_DELayout_H
#define ESMC_DELayout_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::DELayout - DELayout
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt DELayout} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_DELayout.C}
// contains the full code (bodies) for the {\tt DELayout} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_Base.h"      // Base is superclass to DELayout
#include "ESMC_VM.h"

//-------------------------------------------------------------------------

namespace ESMCI {

// constants and enums

const int DELAYOUT_CWGHT_NORMAL = 50;

enum DELayoutServiceReply {DELAYOUT_SERVICE_ACCEPT=1, DELAYOUT_SERVICE_DENY};


// classes

class DELayout;

// DE type used internally in the ESMC_DELayout class
typedef struct{
  int de;           // DE id number (in case not base zero)
  int pet;          // PET associated with this DE
  int vas;          // virtual address space
  // - DEPRECATED section
  int nconnect;     // number of connections from this DE
  int *connect_de;  // connected DEs 
  int *connect_w;   // connection weight
  int *coord;       // coordinates of this DE in the layout
}de_type;
  

// class definition
class DELayout : public ESMC_Base {    // inherits from ESMC_Base class

  private:
    // --- global section ---
    VM *vm;    // ptr to this PET's VM instance this layout is running on
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
    
    // - DEPRECATED section
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
    
  private:
    // construct() and destruct()
    int construct(VM *vmArg=ESMC_NULL_POINTER, 
      ESMC_DePinFlag *dePinFlagArg=ESMC_NULL_POINTER, 
      int *petMap=ESMC_NULL_POINTER, int petMapCount=0);
    int destruct(void);
    
  public:
    // create() and destroy()
    static DELayout *create(int *petMap, int petMapCount,
      ESMC_DePinFlag *dePinFlag, VM *vm=NULL, int *rc=NULL);
    static DELayout *create(int *deCount=NULL,
      InterfaceInt *deGrouping=NULL, ESMC_DePinFlag *dePinFlag=NULL,
      InterfaceInt *petList=NULL, VM *vm=NULL, int *rc=NULL);
    static int destroy(ESMCI::DELayout **layout);
    // get() and set()
    int get(VM **vmArg, int *deCountArg, int *petMap, 
      int petMapCount, int *vasMap, int vasMapCount, 
      ESMC_Logical *oneToOneFlagArg, ESMC_DePinFlag *dePinFlagArg,
      int *localDeCountArg, int *localDeListArg, int localDeListCount,
      int *vasLocalDeCountArg, int *vasLocalDeListArg, int vasLocalDeListCount);
    // misc.
    int print(void);
    int validate(void);
    int serialize(char *buffer, int *length, int *offset);
    static DELayout *deserialize(char *buffer, int *offset);
    // work queue synchronization methods
    DELayoutServiceReply serviceOffer(int de, int *rc);
    int serviceComplete(int de);
    
    // ---------------- DEPRECATED section -------------------
    // construct() and destruct()
  private:
    int construct1D(VM &vm, int nDEs, int *DEtoPET,  
      int len, ESMC_Logical cyclic);  // deprecated
    int constructND(VM &vm, int *nDEs, int nndim, 
      int *DEtoPET, int len, ESMC_Logical cyclic);  // deprecated
  public:  
    // create()
    static DELayout *create(VM &vm, int *nDEs, int ndim, 
      int *DEtoPET, int len, ESMC_Logical *cyclic, int *rc);  // deprecated
    // get()
    int getVM(VM **vm);
    int getDeprecated(int *nDEs, int *ndim, int *nmyDEs, 
      int *myDEs, int len_myDEs, int *localDe, ESMC_Logical *oneToOneFlag,
      ESMC_Logical *logRectFlag, int *deCountPerDim, int len_deCountPerDim);
    int getDELocalInfo(int DEid, int *DEcoord, int len_coord, 
      int *DEcde, int len_cde, int *DEcw, int len_cw, int *nDEc, int *pid);
    int getDEMatchDE(int DEid, DELayout &layoutMatch,
      int *deMatchCount, int *deMatchList, int len_deMatchList);
    int getDEMatchPET(int DEid, VM &vmMatch,
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
    // InternArrayComm.C uses the following DELayoutComm, so I leave it for now
  public:  
    int ESMC_DELayoutGatherV(void *srcdata, void *destdata, 
      int *blen, int *bdestdispl, ESMC_TypeKind dtk, int rootDE);
  private:
    int ESMC_DELayoutFindDEtoPET(int npets);
    int ESMC_DELayoutFillLocal(int mypet);        
    
};  // class DELayout

} // namespace ESMCI

#endif  // ESMC_DELayout_H

