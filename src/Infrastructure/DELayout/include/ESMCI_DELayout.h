// $Id: ESMCI_DELayout.h,v 1.1.2.11 2009/01/21 21:25:20 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_DELayout_H
#define ESMCI_DELayout_H

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


// DE type used internally in the DELayout class
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
    VMK::ipmutex **serviceOfferMutex; // list of shared mutex between PETs
    VMK::ipmutex **serviceMutex; // list of shared mutex between PETs
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



//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-- eXtreme eXchange Engine (XXE) --------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


class XXE{
  public:
    enum OpId{
      send, recv,
      sendnb, recvnb, sendnbRRA, recvnbRRA,
      waitOnIndex, waitOnAnyIndexSub, waitOnIndexRange,
      productSumVector,
      productSumScalar, productSumScalarRRA,
      sumSuperScalarDstRRA,
      productSumSuperScalarDstRRA,
      productSumSuperScalarSrcRRA,
      productSumSuperScalarContigRRA,
      zeroScalarRRA, zeroSuperScalarRRA, zeroVector, zeroVectorRRA,
      memCpy, memCpySrcRRA,
      memGatherSrcRRA,
      // --- subs
      xxeSub, xxeSubMulti,
      // --- profiling
      wtimer,
      // --- misc
      message, profileMessage,
      // --- nop
      nop,
      // --- ids below are not suitable for direct execution
      waitOnAllSendnb, waitOnAllRecvnb
    };
    enum TKId{
      I4, I8, R4, R8, BYTE
    };
    struct StreamElement{
      OpId opId;              // id of operation
      int predicateBitField;  // predicate bit field to control conditional exec
      char opInfo[12*8];      // 12 x 8-byte to hold info associated with op
    };
    
  public:
    VM *vm;
    StreamElement *stream;
    int count;
    char **storage;
    int storageCount;
    VMK::commhandle ***commhandle;
    int commhandleCount;
    XXE **xxeSubList;
    int xxeSubCount;
    ESMC_TypeKind typekind[10];
  private:
    int max;
    int storageMaxCount;
    int commhandleMaxCount;
    int xxeSubMaxCount;
    
  public:
    XXE(VM *vmArg, int maxArg=1000, int storageMaxCountArg=1000,
      int commhandleMaxCountArg=1000, int xxeSubMaxCountArg=1000){
      // constructor
      vm = vmArg;
      stream = new StreamElement[maxArg]; count = 0; max = maxArg;
      storage = new char*[storageMaxCountArg];
      storageCount  = 0;
      storageMaxCount = storageMaxCountArg;
      commhandle = new VMK::commhandle**[commhandleMaxCountArg];
      commhandleCount  = 0;
      commhandleMaxCount = commhandleMaxCountArg;
      xxeSubList = new XXE*[xxeSubMaxCountArg];
      xxeSubCount  = 0;
      xxeSubMaxCount = xxeSubMaxCountArg;
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
      for (int i=0; i<xxeSubCount; i++)
        delete xxeSubList[i];
      delete [] xxeSubList;
    }
    void clearReset(int countArg, int storageCountArg=-1, 
      int commhandleCountArg=-1, int xxeSubCountArg=-1){
      count = countArg; // reset
      if (storageCountArg>0){
        for (int i=storageCountArg; i<storageCount; i++)
          delete [] storage[i];
        storageCount = storageCountArg; // reset
      }
      if (commhandleCountArg>0){
        for (int i=commhandleCountArg; i<commhandleCount; i++){
          delete *commhandle[i];
          delete commhandle[i];
        }
        commhandleCount = commhandleCountArg; // reset
      }
      if (xxeSubCountArg>0){
        for (int i=xxeSubCountArg; i<xxeSubCount; i++)
          delete xxeSubList[i];
        xxeSubCount = xxeSubCountArg; // reset
      }
    }
    int exec(int rraCount=0, char **rraList=NULL, int filterBitField=0x0,
      double *dTime=NULL, int indexStart=-1, int indexStop=-1);
    int print(int rraCount=0, char **rraList=NULL, int filterBitField=0x0,
      int indexStart=-1, int indexStop=-1);
    int printProfile();
    int execReady();
    int optimize();
    int optimizeElement(int index);
    
    int growStream(int increase);
    int growStorage(int increase);
    int growCommhandle(int increase);
    int growXxeSub(int increase);
    
    int incCount();
    int incStorageCount();
    int incCommhandleCount();
    int incXxeSubCount();
    
    int storeStorage(char *storage);
    int storeCommhandle(VMK::commhandle **commhandle);
    int storeXxeSub(XXE *xxeSub);
    int appendXxeSub(int predicateBitField, XXE *xxeSub, int rraShift);
    int appendWtimer(int predicateBitField, char *string, int id, int actualId,
      int relativeId=0, XXE *relativeXXE=NULL);
    int appendRecvnb(int predicateBitField, void *buffer, int size, int srcPet,
      int tag=-1);
    int appendSendnb(int predicateBitField, void *buffer, int size, int dstPet,
      int tag=-1);
    int appendSendnbRRA(int predicateBitField, int rraOffset, int size,
      int dstPet, int rraIndex, int tag=-1);
    int appendMemCpySrcRRA(int predicateBitField, int rraOffset, int size,
      void *dstMem, int rraIndex);
    int appendMemGatherSrcRRA(int predicateBitField, void *dstBase,
      TKId dstBaseTK, int rraIndex, int chunkCount,
      int vectorLength=1);
    int appendZeroScalarRRA(int predicateBitField, TKId elementTK,
      int rraOffset, int rraIndex);
    int appendZeroSuperScalarRRA(int predicateBitField, TKId elementTK,
      int rraIndex, int termCount,
      int vectorLength=1);
    int appendZeroVector(int predicateBitField, char *buffer, int byteCount);
    int appendZeroVectorRRA(int predicateBitField, int byteCount, int rraIndex);
    int appendProductSumScalarRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, TKId factorTK, int rraOffset, void *factor, void *value,
      int rraIndex);
    int appendSumSuperScalarDstRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, int rraIndex, int termCount,
      int vectorLength=1);
    int appendProductSumSuperScalarDstRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, TKId factorTK, int rraIndex, int termCount,
      int vectorLength=1);
    int appendProductSumSuperScalarSrcRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, TKId factorTK, int rraIndex, int termCount,
      int vectorLength=1);
    int appendWaitOnIndex(int predicateBitField, int index);
    int appendWaitOnAnyIndexSub(int predicateBitField, int count);
    int appendWaitOnAllSendnb(int predicateBitField);
    int appendProfileMessage(int predicateBitField, char *messageString);
    int appendMessage(int predicateBitField, char *messageString);
    
  private:
    template<typename T, typename U, typename V>
    static void psv(T *element, TKId elementTK, U *factorList, TKId factorTK,
      V *valueList, TKId valueTK, int factorCount, int resolved);
    template<typename T, typename U, typename V>
    static void pss(T *element, TKId elementTK, U *factor, TKId factorTK,
      V *value, TKId valueTK, int resolved);
    template<typename T, typename V>
    static void sssDstRra(T *rraBase, TKId elementTK, int *rraOffsetList,
      V **valueList, TKId valueTK, int termCount, int vectorLength,
      int resolved);
    template<typename T, typename U, typename V>
    static void psssDstRra(T *rraBase, TKId elementTK, int *rraOffsetList,
      U **factorList, TKId factorTK, V **valueList, TKId valueTK,
      int termCount, int vectorLength, int resolved);
    template<typename T, typename U, typename V>
    static void psssSrcRra(T *rraBase, TKId valueTK, int *rraOffsetList,
      U **factorList, TKId factorTK, V **elementList, TKId elementTK,
      int termCount, int vectorLength, int resolved);
    template<typename T, typename U, typename V>
    static void pssscRra(T *rraBase, TKId elementTK, int *rraOffsetList,
      U **factorList, TKId factorTK, V *valueList, TKId valueTK,
      int termCount, int vectorLength, int resolved);

  // types with which to interpret the StreamElement elements
  public:
      
    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      void *buffer;
      int size;
      int dstPet;
      int tag;
    }SendnbInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      void *buffer;
      int size;
      int srcPet;
      int tag;
    }RecvnbInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      int rraOffset;
      int size;
      int dstPet;
      int rraIndex;
      int tag;
    }SendnbRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      int rraOffset;
      int size;
      int srcPet;
      int rraIndex;
      int tag;
    }RecvnbRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      int index;
    }WaitOnIndexInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      int count;
      XXE **xxe;
      int *index;
      int *completeFlag;
    }WaitOnAnyIndexSubInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      int indexStart;
      int indexEnd;
    }WaitOnIndexRangeInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      void *element;
      void *factorList;
      void *valueList;
      int factorCount;
    }ProductSumVectorInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      void *element;
      void *factor;
      void *value;
    }ProductSumScalarInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      int rraOffset;
      void *factor;
      void *value;
      int rraIndex;
    }ProductSumScalarRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId valueTK;
      int *rraOffsetList;
      void **valueList;
      int rraIndex;
      int termCount;
      int vectorLength;
    }SumSuperScalarDstRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      int *rraOffsetList;
      void **factorList;
      void **valueList;
      int rraIndex;
      int termCount;
      int vectorLength;
    }ProductSumSuperScalarDstRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      int *rraOffsetList;
      void **factorList;
      void **elementList;
      int rraIndex;
      int termCount;
      int vectorLength;
    }ProductSumSuperScalarSrcRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      int *rraOffsetList;
      void **factorList;
      void *valueList;
      int rraIndex;
      int termCount;
      int vectorLength;
    }ProductSumSuperScalarContigRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      int rraOffset;
      int rraIndex;
    }ZeroScalarRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      int *rraOffsetList;
      int rraIndex;
      int termCount;
      int vectorLength;
    }ZeroSuperScalarRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      char *buffer;
      int byteCount;
    }ZeroVectorInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      int byteCount;
      int rraIndex;
    }ZeroVectorRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      void *dstMem;
      void *srcMem;
      int size;
    }MemCpyInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      void *dstMem;
      int rraOffset;
      int size;
      int rraIndex;
    }MemCpySrcRRAInfo;
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      void *dstBase;
      TKId dstBaseTK;
      int *rraOffsetList;
      int *countList;
      int rraIndex;
      int chunkCount;
      int vectorLength;
    }MemGatherSrcRRAInfo;
    
    // --- sub-streams
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      XXE *xxe;
      int rraShift;
    }XxeSubInfo;
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      int count;
      XXE **xxe;
    }XxeSubMultiInfo;
    
    // --- profiling
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      char *timerString;
      int timerId;
      int actualWtimerId;
      int relativeWtimerId;
      // members below are for internal use
      int actualWtimerIndex;
      double *relativeWtime;
      XXE *relativeWtimerXXE;
      int sumTermCount;
      double wtime;
      double wtimeSum;
    }WtimerInfo;
    
    // --- misc
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      char *messageString;
    }MessageInfo;
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      char *messageString;
    }ProfileMessageInfo;
    
    // --- meta Info structs (i.e. don't correspond to OpIds)

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
    }CommhandleInfo;

};  // class XXE


} // namespace ESMCI

#endif  // ESMCI_DELayout_H

