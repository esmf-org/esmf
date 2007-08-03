// $Id: ESMC_DELayout.h,v 1.33 2007/08/03 20:57:45 theurich Exp $
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
class XXE;


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
    de_type *deInfoList;// list that holds all of this layout's DE info
    // --- local section ---
    int localDeCount; // number of DEs associated with instantiating PET
    int *localDeList; // list that holds all of the de indices for this PET
    int *deList;      // localDE index for DE or -1 if not local
    
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
    int destruct();
    
  public:
    // create() and destroy()
    static DELayout *create(int *petMap, int petMapCount,
      ESMC_DePinFlag *dePinFlag, VM *vm=NULL, int *rc=NULL);
    static DELayout *create(int *deCount=NULL,
      InterfaceInt *deGrouping=NULL, ESMC_DePinFlag *dePinFlag=NULL,
      InterfaceInt *petList=NULL, VM *vm=NULL, int *rc=NULL);
    static int destroy(ESMCI::DELayout **layout);
    // get() and set()
    VM *getVM()                     const {return vm;}
    int getDeCount()                const {return deCount;}
    const int *getDeList()          const {return deList;}
    int getLocalDeCount()           const {return localDeCount;}
    const int *getLocalDeList()     const {return localDeList;}
    int getVasLocalDeCount()        const {return vasLocalDeCount;}
    const int *getVasLocalDeList()  const {return vasLocalDeList;}
    int getPet(int i)               const {return deInfoList[i].pet;}
    int getVas(int i)               const {return deInfoList[i].vas;}
    ESMC_Logical getOneToOneFlag()  const {return oneToOneFlag;}
    ESMC_DePinFlag getDePinFlag()   const {return dePinFlag;}
    int getDEMatchDE(int DEid, DELayout &layoutMatch, int *deMatchCount, 
      int *deMatchList, int len_deMatchList)const;
    int getDEMatchPET(int DEid, VM &vmMatch, int *petMatchCount,
      int *petMatchList, int len_petMatchList)const;
    // misc.
    int print() const;
    int validate() const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset) const;
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
    int getDeprecated(int *nDEs, int *ndim, int *nmyDEs, 
      int *myDEs, int len_myDEs, int *localDe, ESMC_Logical *oneToOneFlag,
      ESMC_Logical *logRectFlag, int *deCountPerDim, int len_deCountPerDim)
      const;
    int getDELocalInfo(int DEid, int *DEcoord, int len_coord, 
      int *DEcde, int len_cde, int *DEcw, int len_cw, int *nDEc, int *pid)
      const;
        
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
    // InternArrayComm.C uses the following DELayoutComm, so leave it for now
  public:  
    int ESMC_DELayoutGatherV(void *srcdata, void *destdata, 
      int *blen, int *bdestdispl, ESMC_TypeKind dtk, int rootDE);
  private:
    int ESMC_DELayoutFindDEtoPET(int npets);
    int ESMC_DELayoutFillLocal(int mypet);        
    
};  // class DELayout




class XXE{
  public:
    enum OpId{
      send, recv,
      sendnb, recvnb,
      waitOnIndex, waitOnIndexRange,
      productSum, memCpy,
      // --- ids below are not suitable for direct execution
      waitOnAllSendnb, waitOnAllRecvnb,
      // --- nop
      nop    
    };
    enum OpSubId{
      I4, I8, R4, R8
    };
    struct StreamElement{
      OpId opId;              // id of operation
      OpSubId opSubId;        // id of sub-operation
      char opInfo[7*8];       // 7 x 8-byte to hold info associated with op
    };
  public:
    StreamElement *stream;
    int count;
    int max;
    char **storage;
    int storageCount;
    int storageMaxCount;
    vmk_commhandle ***commhandle;
    int commhandleCount;
    int commhandleMaxCount;
    
  public:
    XXE(int maxArg, int storageMaxCountArg=100, int commhandleMaxCountArg=100){
      // constructor
      stream = new StreamElement[maxArg]; count = 0; max = maxArg;
      storage = new char*[storageMaxCountArg];
      storageCount  = 0;
      storageMaxCount = storageMaxCountArg;
      commhandle = new vmk_commhandle**[commhandleMaxCountArg];
      commhandleCount  = 0;
      commhandleMaxCount = commhandleMaxCountArg;
    }
    ~XXE(){     // destructor
      delete [] stream;
      for (int i=0; i<storageCount; i++)
        delete [] storage[i];
      delete [] storage;
      for (int i=0; i<commhandleCount; i++){
        delete *commhandle[i];
        delete commhandle[i];
      }
      delete [] commhandle;
    }
    int exec();
    int execReady();
    int optimize();
    
  // types to interprete the StreamElement data
  public:
      
    typedef struct{
      OpId opId;
      OpSubId opSubId;
      vmk_commhandle **commhandle;
      void *buffer;
      int size;
      int dstPet;
    }SendnbInfo;

    typedef struct{
      OpId opId;
      OpSubId opSubId;
      vmk_commhandle **commhandle;
      void *buffer;
      int size;
      int srcPet;
    }RecvnbInfo;

    typedef struct{
      OpId opId;
      OpSubId opSubId;
      int index;
    }WaitOnIndexInfo;

    typedef struct{
      OpId opId;
      OpSubId opSubId;
      int indexStart;
      int indexEnd;
    }WaitOnIndexRangeInfo;

    typedef struct{
      OpId opId;
      OpSubId opSubId;
      void *element;
      void *factorList;
      void *valueList;
      int factorCount;
    }ProductSumInfo;

    typedef struct{
      OpId opId;
      OpSubId opSubId;
      void *dstMem;
      void *srcMem;
      int size;
    }MemCpyInfo;

    
    typedef struct{
      OpId opId;
      OpSubId opSubId;
      vmk_commhandle **commhandle;
    }CommhandleInfo;

};  // class XXE


} // namespace ESMCI

#endif  // ESMC_DELayout_H

