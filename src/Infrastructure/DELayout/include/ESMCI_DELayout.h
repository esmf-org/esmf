// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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

#include <vector>

#include "ESMCI_Base.h"       // Base is superclass to DELayout
#include "ESMCI_VM.h"
#include "ESMCI_F90Interface.h"

//-------------------------------------------------------------------------

namespace ESMCI {

// constants and enums

const int DELAYOUT_CWGHT_NORMAL = 50;

enum ServiceReply {SERVICEREPLY_ACCEPT=1, SERVICEREPLY_DENY};


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
    int *localDeToDeMap; // list that holds all of the de indices for this PET
    int *deList;      // localDE index for DE or -1 if not local
    
    int oldstyle;   // if this flag is set then this is an oldstyle delayout
                    // new style delayouts follow proposal sent out on 02/15/06
    
    // - NEWSTYLE section
    ESMC_Pin_Flag pinFlag; // type of resources DEs are pinned to    
    
    int vasLocalDeCount;// number of DEs associated with local VAS
    int *vasLocalDeToDeMap;// list that holds all of the de indices for this VAS
    
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
    
  public:
    // native constructor and destructor
    DELayout(VM *vm=NULL):ESMC_Base(vm){} // allow specific VM instead default
    DELayout(int baseID):ESMC_Base(baseID){}// prevent baseID counter increment
    ~DELayout(){destruct();}
    
  private:
    // construct() and destruct()
    int construct(VM *vmArg=ESMC_NULL_POINTER, 
      ESMC_Pin_Flag *pinFlagArg=ESMC_NULL_POINTER, 
      int *petMap=ESMC_NULL_POINTER, int petMapCount=0);
    int destruct();
    
  public:
    // create() and destroy()
    static DELayout *create(int *petMap, int petMapCount,
      ESMC_Pin_Flag *pinFlag, VM *vm=NULL, int *rc=NULL);
    static DELayout *create(int *deCount=NULL,
      InterfaceInt *deGrouping=NULL, ESMC_Pin_Flag *pinFlag=NULL,
      InterfaceInt *petList=NULL, VM *vm=NULL, int *rc=NULL);
    static int destroy(ESMCI::DELayout **layout, bool noGarbage=false);
    // get() and set()
    VM *getVM()                       const {return vm;}
    int getDeCount()                  const {return deCount;}
    const int *getDeList()            const {return deList;}
    int getLocalDeCount()             const {return localDeCount;}
    const int *getLocalDeToDeMap()    const {return localDeToDeMap;}
    int getVasLocalDeCount()          const {return vasLocalDeCount;}
    const int *getVasLocalDeToDeMap() const {return vasLocalDeToDeMap;}
    int getPet(int i)                 const {return deInfoList[i].pet;}
    int getVas(int i)                 const {return deInfoList[i].vas;}
    ESMC_Logical getOneToOneFlag()    const {return oneToOneFlag;}
    ESMC_Pin_Flag getPinFlag()        const {return pinFlag;}
    int getDEMatchDE(int DEid, DELayout &layoutMatch, int *deMatchCount, 
      int *deMatchList, int len_deMatchList)const;
    int getDEMatchPET(int DEid, VM &vmMatch, int *petMatchCount,
      int *petMatchList, int len_petMatchList)const;
    // misc.
    int print() const;
    int validate() const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset,
      ESMC_InquireFlag inquireflag) const;
    static DELayout *deserialize(char *buffer, int *offset);
    // work queue synchronization methods
    ServiceReply serviceOffer(int de, int *rc);
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
      int len, ESMC_TypeKind_Flag dtk, int srcDE, int destDE);
    int ESMC_DELayoutExchange(void *srcData1, void *srcData2, 
      void *dstData1, void *dstData2, int blen1, int blen2, int de1, int de2);
    int ESMC_DELayoutExchange(void *srcData1, void *srcData2, 
      void *dstData1, void *dstData2, int len1, int len2, ESMC_TypeKind_Flag dtk1,
      ESMC_TypeKind_Flag dtk2, int de1, int de2);
    int ESMC_DELayoutBcast(void *data, int blen, int rootDE);
    int ESMC_DELayoutBcast(void *data, int len, ESMC_TypeKind_Flag dtk, int rootDE);
    int ESMC_DELayoutScatter(void *srcdata, void *destdata, 
      int blen, int rootDE);
    int ESMC_DELayoutScatter(void *srcdata, void *destdata, 
      int len, ESMC_TypeKind_Flag dtk, int rootDE);
    int ESMC_DELayoutGather(void *srcdata, void *destdata, 
      int blen, int rootDE);
    int ESMC_DELayoutGather(void *srcdata, void *destdata, 
      int len, ESMC_TypeKind_Flag dtk, int rootDE);
    int ESMC_DELayoutGatherV(void *srcdata, void *destdata, 
      int *blen, int *bdestdispl, int rootDE);
    int ESMC_DELayoutGatherV(void *srcdata, void *destdata, 
      int *blen, int *bdestdispl, ESMC_TypeKind_Flag dtk, int rootDE);
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
      // --- send, recv, sendrecv
      send, recv, sendRRA, recvRRA, sendrecv, sendRRArecv,
      // --- non-blocking send, recv
      sendnb, recvnb, sendnbRRA, recvnbRRA,
      // --- wait
      waitOnIndex, waitOnAnyIndexSub, waitOnIndexRange, waitOnIndexSub,
      // --- test
      testOnIndex, testOnIndexSub,
      // --- cancel
      cancelIndex,
      // --- product and sum
      productSumVector,
      productSumScalar, productSumScalarRRA,
      sumSuperScalarDstRRA,
      sumSuperScalarListDstRRA,
      productSumSuperScalarDstRRA,
      productSumSuperScalarListDstRRA,
      productSumSuperScalarSrcRRA,
      productSumSuperScalarContigRRA,
      // -- zero
      zeroScalarRRA, zeroSuperScalarRRA, zeroMemset, zeroMemsetRRA,
      // --- mem movement
      memCpy, memCpySrcRRA,
      memGatherSrcRRA,
      // --- unconditional subs
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
      // Generic stream element that any stream element can be cast into. This
      // permits access to the first two members, common to all stream elements.
      OpId opId;              // id of operation
      int predicateBitField;  // predicate bit field to control conditional exec
      char opInfo[16*8];      // 16 x 8-byte padding to hold stream specific
                              // members
    };
    
    // special predefined filter bits
    static int const filterBitRegionTotalZero   = 0x1;  // total dst zero'ing
    static int const filterBitRegionSelectZero  = 0x2;  // select dst element z.
    static int const filterBitNbStart           = 0x4;  // non-block start
    static int const filterBitNbTestFinish      = 0x8;  // non-block test&finish
    static int const filterBitNbWaitFinish      = 0x10; // non-block wait&finish
    static int const filterBitCancel            = 0x20; // cancel
    static int const filterBitNbWaitFinishSingleSum = 0x40; // single sum

    struct BufferInfo{
      // The BufferInfo provides an extra level of indirection to XXE managed
      // communication buffers, allowing the resizing of buffers without having
      // to rewrite XXE stream elements. The relevant size and scaling
      // information is also provided to support buffer resizing during exec(),
      // when the actual execution time vectorLength is known.
      char *buffer;                 // buffer
      int size;                     // size of buffer in byte
      int vectorLengthMultiplier;   // multiplier that allows to determine size
                                    // requirement depending on vectorLength
                                    // during exec()
      
      BufferInfo(char *buffer_, int size_, int vectorLengthMultiplier_){
        // constructor
        buffer = buffer_;
        size = size_;
        vectorLengthMultiplier = vectorLengthMultiplier_;
      }
    };
        
  public:
    VM *vm;
    StreamElement *stream;          // actual stream containing XXE elements
    int count;                      // number of elements in the stream
    char **storage;                 // list of (char *) entries to allocations
                                    // for which this XXE object is responsible
    int storageCount;               // number of elements in storage
    VMK::commhandle ***commhandle;  // list of (commhandle **) entries for 
                                    // which this XXE object is responsible
    int commhandleCount;            // number of elements in commhandle
    XXE **xxeSubList;               // list of (XXE *) entries for which this
                                    // XXE object is responsible
    int xxeSubCount;                // number of elements in xxeSubList
    ESMC_TypeKind_Flag typekind[10];     // place the XXE can store TypeKind info
    std::vector<BufferInfo *>bufferInfoList; // vector of (BufferInfo *) entries
      // The bufferInfoList provides an extra level of indirection to XXE
      // managed communication buffers, and associated size information.
      // At the beginning of exec() the entries in the bufferInfoList are
      // checked against the current exec() conditions (i.e. the vectorLength
      // at execution time), and new, larger buffers are allocated if necessary.
      // Actual XXE elements in the stream that go through the bufferInfoList
      // entries have the "indirectionFlag" set, and thus support buffer
      // updates during exec() through the bufferInfoList indirection, without
      // the need for XXE stream rewrite (which would be far too expensive to
      // do during exec())!
    int lastFilterBitField;         // filterBitField during last exec() call
  private:
    int max;                        // maximum number of elements in stream
    int storageMaxCount;            // maximum number of elements in storage
    int commhandleMaxCount;         // maximum number of elements in commhandle
    int xxeSubMaxCount;             // maximum number of elements in xxeSubList
    
  public:
    XXE(VM *vmArg, int maxArg=1000, int storageMaxCountArg=1000,
      int commhandleMaxCountArg=1000, int xxeSubMaxCountArg=1000){
      // constructor
      vm = vmArg;
      // -> set up internal stream and bookkeeping members
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
      bufferInfoList.reserve(1000);  // initial preparation
      lastFilterBitField = 0x0;
    }
    ~XXE(){
      // destructor
      // -> clean-up all allocations for which this XXE object is responsible:
      // stream of XXE elements
      delete [] stream;
      // memory allocations held in storage
      for (int i=0; i<storageCount; i++)
        delete [] storage[i];
      delete [] storage;
      // CommHandles held in commhandle
      for (int i=0; i<commhandleCount; i++){
        delete *commhandle[i];
        delete commhandle[i];
      }
      delete [] commhandle;
      // XXE sub objects held in xxeSubList
      for (int i=0; i<xxeSubCount; i++)
        delete xxeSubList[i];
      delete [] xxeSubList;
      // BufferInfo objects held in bufferInfoList
      for (int i=0; i<bufferInfoList.size(); i++)
        delete bufferInfoList[i];
      bufferInfoList.clear();
    }
    void clearReset(int countArg, int storageCountArg=-1, 
      int commhandleCountArg=-1, int xxeSubCountArg=-1, 
      int bufferInfoListArg=-1){
      // reset the stream back to a specified position, and clear all
      // bookkeeping elements above specified positions
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
      if (bufferInfoListArg>0){
        std::vector<BufferInfo *>::iterator first =
          bufferInfoList.begin() + bufferInfoListArg;
        std::vector<BufferInfo *>::iterator last = bufferInfoList.end();
        for (std::vector<BufferInfo *>::iterator bi=first; bi!=last; ++bi)
          delete *bi;
        bufferInfoList.erase(first, last);
      }
    }
    int exec(int rraCount=0, char **rraList=NULL, int *vectorLength=NULL,
      int filterBitField=0x0, bool *finished=NULL, bool *cancelled=NULL, 
      double *dTime=NULL, int indexStart=-1, int indexStop=-1);
    int print(FILE *fp, int rraCount=0, char **rraList=NULL,
      int filterBitField=0x0, int indexStart=-1, int indexStop=-1);
    int printProfile(FILE *fp);
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
    int storeBufferInfo(char *buffer, int size, int vectorLengthMultiplier);
    char *getBufferInfoPtr(){
      size_t i=bufferInfoList.size();
      if (i>0)
        return (char *)bufferInfoList[i-1];
      else
        return NULL;
    }
    int appendXxeSub(int predicateBitField, XXE *xxeSub, int rraShift,
      int vectorLengthShift);
    int appendWtimer(int predicateBitField, char *string, int id, int actualId,
      int relativeId=0, XXE *relativeXXE=NULL);
    int appendRecv(int predicateBitField, void *buffer, int size, int srcPet,
      int tag=-1, bool vectorFlag=false, bool indirectionFlag=false);
    int appendSend(int predicateBitField, void *buffer, int size, int dstPet,
      int tag=-1, bool vectorFlag=false, bool indirectionFlag=false);
    int appendSendRRA(int predicateBitField, int rraOffset, int size,
      int dstPet, int rraIndex, int tag=-1, bool vectorFlag=false);
    int appendSendRecv(int predicateBitField, void *srcBuffer, void *dstBuffer,
      int srcSize, int dstSize, int srcPet, int dstPet, int srcTag, int dstTag, 
      bool vectorFlag=false, bool srcIndirectionFlag=false,
      bool dstIndirectionFlag=false);
    int appendSendRRARecv(int predicateBitField, int rraOffset, void *dstBuffer,       int srcSize, int dstSize, int srcPet, int dstPet, int rraIndex,
      int srcTag, int dstTag, bool vectorFlag=false,
      bool dstIndirectionFlag=false);
    int appendRecvnb(int predicateBitField, void *buffer, int size, int srcPet,
      int tag=-1, bool vectorFlag=false, bool indirectionFlag=false);
    int appendSendnb(int predicateBitField, void *buffer, int size, int dstPet,
      int tag=-1, bool vectorFlag=false, bool indirectionFlag=false);
    int appendSendnbRRA(int predicateBitField, int rraOffset, int size,
      int dstPet, int rraIndex, int tag=-1, bool vectorFlag=false);
    int appendMemCpySrcRRA(int predicateBitField, int rraOffset, int size,
      void *dstMem, int rraIndex);
    int appendMemGatherSrcRRA(int predicateBitField, void *dstBase,
      TKId dstBaseTK, int rraIndex, int chunkCount, bool vectorFlag=false,
      bool indirectionFlag=false);
    int appendZeroScalarRRA(int predicateBitField, TKId elementTK,
      int rraOffset, int rraIndex);
    int appendZeroSuperScalarRRA(int predicateBitField, TKId elementTK,
      int rraIndex, int termCount, bool vectorFlag=false);
    int appendZeroMemset(int predicateBitField, void *buffer, int byteCount,
      bool vectorFlag=false, bool indirectionFlag=false);
    int appendZeroMemsetRRA(int predicateBitField, int byteCount, int rraIndex,
      bool vectorFlag=false);
    int appendProductSumScalarRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, TKId factorTK, int rraOffset, void *factor, void *value,
      int rraIndex);
    int appendSumSuperScalarDstRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, int rraIndex, int termCount, void *valueBase,
      bool vectorFlag=false, bool indirectionFlag=false);
    int appendSumSuperScalarListDstRRA(int predicateBitField, 
      TKId elementTK, TKId valueTK,
      std::vector<int> rraIndexList, int termCount,
      std::vector<void *>valueBaseList, bool vectorFlag=false, 
      bool indirectionFlag=false);
    int appendProductSumSuperScalarDstRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, TKId factorTK, int rraIndex, int termCount, void *valueBase,
      bool vectorFlag=false, bool indirectionFlag=false);
    int appendProductSumSuperScalarListDstRRA(int predicateBitField, 
      TKId elementTK, TKId valueTK, TKId factorTK,
      std::vector<int> rraIndexList, int termCount,
      std::vector<void *>valueBaseList, bool vectorFlag=false, 
      bool indirectionFlag=false);
    int appendProductSumSuperScalarSrcRRA(int predicateBitField, TKId elementTK,
      TKId valueTK, TKId factorTK, int rraIndex, int termCount,
      void *elementBase, bool vectorFlag=false, bool indirectionFlag=false);
    int appendWaitOnIndex(int predicateBitField, int index);
    int appendTestOnIndex(int predicateBitField, int index);
    int appendWaitOnAnyIndexSub(int predicateBitField, int count);
    int appendWaitOnAllSendnb(int predicateBitField);
    int appendWaitOnIndexSub(int predicateBitField, XXE *xxeSub, int rraShift,
      int vectorLengthShift, int index);
    int appendTestOnIndexSub(int predicateBitField, XXE *xxeSub, int rraShift,
      int vectorLengthShift, int index);
    int appendCancelIndex(int predicateBitField, int index);
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
      V *valueBase, int *valueOffsetList, TKId valueTK, int termCount,
      int vectorLength, int resolved);
    template<typename T, typename V>
    static void ssslDstRra(T **rraBaseList, int *rraIndexList, TKId elementTK,
      int *rraOffsetList, V **valueBaseList,
      int *valueOffsetList, int *baseListIndexList,
      TKId valueTK, int termCount, int vectorLength, int resolved);
    template<typename T, typename U, typename V>
    static void psssDstRra(T *rraBase, TKId elementTK, int *rraOffsetList,
      U **factorList, TKId factorTK, V *valueBase, int *valueOffsetList,
      TKId valueTK, int termCount, int vectorLength, int resolved);
    template<typename T, typename U, typename V>
    static void pssslDstRra(T **rraBaseList, int *rraIndexList, TKId elementTK,
      int *rraOffsetList, U **factorList, TKId factorTK, V **valueBaseList,
      int *valueOffsetList, int *baseListIndexList,
      TKId valueTK, int termCount, int vectorLength, int resolved);
    template<typename T, typename U, typename V>
    static void psssSrcRra(T *rraBase, TKId valueTK, int *rraOffsetList,
      U **factorList, TKId factorTK, V *elementBase, int *elementOffsetList,
      TKId elementTK, int termCount, int vectorLength, int resolved);
    template<typename T, typename U, typename V>
    static void pssscRra(T *rraBase, TKId elementTK, int *rraOffsetList,
      U **factorList, TKId factorTK, V *valueList, TKId valueTK,
      int termCount, int vectorLength, int resolved);

  public:
      
    // specific stream element types, used to interpret the elements in stream
    
    // Common stream element members and their meaning:
    //  opId                - operation id according to "enum OpId"
    //  predicateBitField   - predicate bit field to control conditional exec
    //  vectorFlag          - true:  scale with vectorLength during exec()
    //                      - false: ignore vectorLength during exec()
    //  indirectionFlag     - true:  interpret buffer as " *(char **)buffer"
    //                        false: interpret buffer as " (char *)buffer"
      
    typedef struct{
      OpId opId;
      int predicateBitField;
      bool activeFlag;
      bool cancelledFlag;
      void *buffer;
      int size;
      int dstPet;
      int tag;
      bool vectorFlag;
      bool indirectionFlag;
    }SendInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      bool activeFlag;
      bool cancelledFlag;
      void *buffer;
      int size;
      int srcPet;
      int tag;
      bool vectorFlag;
      bool indirectionFlag;
    }RecvInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      bool activeFlag;
      bool cancelledFlag;
      int rraOffset;
      int size;
      int dstPet;
      int rraIndex;
      int tag;
      bool vectorFlag;
    }SendRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      bool activeFlag;
      bool cancelledFlag;
      int rraOffset;
      int size;
      int srcPet;
      int rraIndex;
      int tag;
      bool vectorFlag;
    }RecvRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      bool activeFlag;
      bool cancelledFlag;
      void *srcBuffer;
      void *dstBuffer;
      int srcSize;
      int dstSize;
      int srcPet;
      int dstPet;
      int srcTag;
      int dstTag;
      bool vectorFlag;
      bool srcIndirectionFlag;
      bool dstIndirectionFlag;
    }SendRecvInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      bool activeFlag;
      bool cancelledFlag;
      int rraOffset;
      void *dstBuffer;
      int srcSize;
      int dstSize;
      int srcPet;
      int dstPet;
      int rraIndex;
      int srcTag;
      int dstTag;
      bool vectorFlag;
      bool dstIndirectionFlag;
    }SendRRARecvInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      bool activeFlag;
      bool cancelledFlag;
      void *buffer;
      int size;
      int dstPet;
      int tag;
      bool vectorFlag;
      bool indirectionFlag;
    }SendnbInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      bool activeFlag;
      bool cancelledFlag;
      void *buffer;
      int size;
      int srcPet;
      int tag;
      bool vectorFlag;
      bool indirectionFlag;
    }RecvnbInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      bool activeFlag;
      bool cancelledFlag;
      int rraOffset;
      int size;
      int dstPet;
      int rraIndex;
      int tag;
      bool vectorFlag;
    }SendnbRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      VMK::commhandle **commhandle;
      bool activeFlag;
      bool cancelledFlag;
      int rraOffset;
      int size;
      int srcPet;
      int rraIndex;
      int tag;
      bool vectorFlag;
    }RecvnbRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      int index;
    }WaitOnIndexInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      int index;
    }TestOnIndexInfo;

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
      XXE *xxe;
      int rraShift;
      int vectorLengthShift;
      int index;
    }WaitOnIndexSubInfo;
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      XXE *xxe;
      int rraShift;
      int vectorLengthShift;
      int index;
    }TestOnIndexSubInfo;
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      int index;
    }CancelIndexInfo;

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
      void *valueBase;
      int rraIndex;
      int termCount;
      bool vectorFlag;
      bool indirectionFlag;
      int *valueOffsetList;
    }SumSuperScalarDstRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId valueTK;
      int *rraOffsetList;
      void **valueBaseList;
      void **valueBaseListResolve;
      int valueBaseListSize;
      int *rraIndexList;
      int termCount;
      bool vectorFlag;
      bool indirectionFlag;
      int *valueOffsetList;
      int *baseListIndexList;
    }SumSuperScalarListDstRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      int *rraOffsetList;
      void **factorList;
      void *valueBase;
      int rraIndex;
      int termCount;
      bool vectorFlag;
      bool indirectionFlag;
      int *valueOffsetList;
    }ProductSumSuperScalarDstRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      int *rraOffsetList;
      void **factorList;
      void **valueBaseList;
      void **valueBaseListResolve;
      int valueBaseListSize;
      int *rraIndexList;
      int termCount;
      bool vectorFlag;
      bool indirectionFlag;
      int *valueOffsetList;
      int *baseListIndexList;
    }ProductSumSuperScalarListDstRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      TKId elementTK;
      TKId factorTK;
      TKId valueTK;
      int *rraOffsetList;
      void **factorList;
      void *elementBase;
      int rraIndex;
      int termCount;
      bool vectorFlag;
      bool indirectionFlag;
      int *elementOffsetList;
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
      bool vectorFlag;
      bool indirectionFlag;
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
      bool vectorFlag;
    }ZeroSuperScalarRRAInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      void *buffer;
      int byteCount;
      bool vectorFlag;
      bool indirectionFlag;
    }ZeroMemsetInfo;

    typedef struct{
      OpId opId;
      int predicateBitField;
      int byteCount;
      int rraIndex;
      bool vectorFlag;
    }ZeroMemsetRRAInfo;

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
      bool vectorFlag;
      bool indirectionFlag;
    }MemGatherSrcRRAInfo;
    
    // --- sub-streams
    
    typedef struct{
      OpId opId;
      int predicateBitField;
      XXE *xxe;
      int rraShift;
      int vectorLengthShift;
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
      bool activeFlag;
      bool cancelledFlag;
    }CommhandleInfo;

};  // class XXE


} // namespace ESMCI

#endif  // ESMCI_DELayout_H

