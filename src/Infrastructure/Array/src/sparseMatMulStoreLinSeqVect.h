//-----------------------------------------------------------------------------
#undef DEBUGLOG

namespace DD{
  
  template<typename IT> struct Interval{
    IT min;
    IT max;
    IT count;
  };

  template<typename IT> struct SeqIndexFactorLookup{
    vector<int> de;
    vector<FactorElement<SeqIndex<IT> > > factorList;
    int factorCount;  //TODO: get rid of this and use factorList.size()
  public:
    SeqIndexFactorLookup(){
      factorCount = 0;
    }
  };
  
  template<typename IT1, typename IT2> struct FillLinSeqVectInfo{
    Array const *array;
    vector<vector<AssociationElement<SeqIndex<IT1>,SeqIndex<IT2> > > >
      &linSeqVect;
    vector<SeqIndexFactorLookup<IT1> > &seqIndexFactorLookup;
    int localPet;
    int localDeCount;
    const int *localDeElementCount;
    const Interval<IT1> *seqIndexInterval;
    bool tensorMixFlag;
    bool haloRimFlag;   // true indicates that halo rim instead of excl reg used
  public:
    FillLinSeqVectInfo(
      vector<vector<AssociationElement<SeqIndex<IT1>,SeqIndex<IT2> > > >
        &linSeqVect_,
      vector<SeqIndexFactorLookup<IT1> > &seqIndexFactorLookup_
    ):
      // members that need to be set on this level because of reference
      linSeqVect(linSeqVect_),
      seqIndexFactorLookup(seqIndexFactorLookup_)
    {}
  };
  template<typename IT1, typename IT2> struct FillPartnerDeInfo{
    vector<SeqIndexFactorLookup<IT1> > &seqIndexFactorLookupIn;
    vector<SeqIndexFactorLookup<IT2> > &seqIndexFactorLookupOut;
    const Interval<IT1> *seqIndexIntervalIn;
    const Interval<IT2> *seqIndexIntervalOut;
    int localPet;
    bool tensorMixFlag;
  public:
    FillPartnerDeInfo(
      vector<SeqIndexFactorLookup<IT1> > &seqIndexFactorLookupIn_,
      vector<SeqIndexFactorLookup<IT2> > &seqIndexFactorLookupOut_
    ):
      // members that need to be set on this level because of reference
      seqIndexFactorLookupIn(seqIndexFactorLookupIn_),
      seqIndexFactorLookupOut(seqIndexFactorLookupOut_)
    {}
  };

// -- accessLookup()

template<typename T>
int requestSizeFactor(T *t);

template<typename T>
void clientRequest(T *t, int i, char **requestStreamClient);

template<typename T>
void localClientServerExchange(T *t);

template<typename T>
int serverResponseSize(T *t, int count, int i, char **requestStreamServer);

template<typename T>
void serverResponse(T *t, int count, int i, char **requestStreamServer,
  char **responseStreamServer);

template<typename T>
void clientProcess(T *t, char *responseStream, int responseStreamSize);

template<typename T>
void accessLookup(
  ESMCI::VM *vm,
  int petCount,
  int localPet,
  int *localIntervalPerPetCount,
  int *localElementsPerIntervalCount,
  T *t
  ){
  // access look up table
  VMK::commhandle **send1commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **send2commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **send3commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **recv1commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **recv2commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **recv3commhList = new VMK::commhandle*[petCount];
  char **requestStreamClient = new char*[petCount];
  char **requestStreamServer = new char*[petCount];
  int *responseStreamSizeClient = new int[petCount];
  int *responseStreamSizeServer = new int[petCount];
  char **responseStreamClient = new char*[petCount];
  char **responseStreamServer = new char*[petCount];
  
//char msg[1024];
  
  // t-specific routine
  int requestFactor = requestSizeFactor(t);
  // localPet acts as server, posts non-blocking recvs for all client requests
//ESMC_LogDefault.Write("accessLookup: step 1", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    // receive request from Pet "i"
    int count = localIntervalPerPetCount[i];
    if (count>0){
//sprintf(msg, "posting nb-recv for message from PET %d size=%d", i, requestFactor*count);
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 
      requestStreamServer[i] = new char[requestFactor*count];
      recv3commhList[i] = NULL;
      vm->recv(requestStreamServer[i], requestFactor*count, i,
        &(recv3commhList[i]));
    }
  }
  // localPet acts as a client, sends its requests to the appropriate servers
//ESMC_LogDefault.Write("accessLookup: step 2", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet "i"
      requestStreamClient[i] =
        new char[requestFactor*localElementsPerIntervalCount[i]];
      // t-specific client routine
      clientRequest(t, i, requestStreamClient);
      // send information to the serving Pet
      send1commhList[i] = NULL;
//sprintf(msg, "posting nb-send to PET %d size=%d", i, requestFactor*localElementsPerIntervalCount[i]);
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 
#if (defined MUST_USE_BLOCKING_SEND || defined WORKAROUND_NONBLOCKPROGRESSBUG)
      vm->send(requestStreamClient[i],
        requestFactor*localElementsPerIntervalCount[i], i); 
#else
      vm->send(requestStreamClient[i],
        requestFactor*localElementsPerIntervalCount[i], i, 
        &(send1commhList[i]));
#endif
      // post receive to obtain response size from server Pet
      recv1commhList[i] = NULL;
//sprintf(msg, "posting nb-recv for message from PET %d size=%d", i, sizeof(int));
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 

      vm->recv(&(responseStreamSizeClient[i]), sizeof(int), i,
        &(recv1commhList[i]));
    }
  }
  // localPet locally acts as server and client to fill its own request
  // t-specific client-server routine
//ESMC_LogDefault.Write("accessLookup: step 3", ESMC_LOGMSG_INFO); 
  localClientServerExchange(t);
  // localPet acts as server, processing requests from clients, send response sz
//ESMC_LogDefault.Write("accessLookup: step 4", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    int count = localIntervalPerPetCount[i];
    if (count>0){
      // wait for request from Pet "i"
//sprintf(msg, "waiting on nb-recv for message from PET %d", i);
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 
      vm->commwait(&(recv3commhList[i]));
      // t-specific server routine
      int responseStreamSize =
        serverResponseSize(t, count, i, requestStreamServer);
      // send response size to client Pet "i"
      responseStreamSizeServer[i] = responseStreamSize;
      send2commhList[i] = NULL;
//sprintf(msg, "posting nb-send to PET %d size=%d", i, sizeof(int));
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 

#if (defined MUST_USE_BLOCKING_SEND || defined WORKAROUND_NONBLOCKPROGRESSBUG)
      vm->send(&(responseStreamSizeServer[i]), sizeof(int), i);
#else
      vm->send(&(responseStreamSizeServer[i]), sizeof(int), i,
        &(send2commhList[i]));
#endif
    }
  }
  // localPet acts as a client, waits for response size from server and posts
  // receive for response stream
//ESMC_LogDefault.Write("accessLookup: step 5", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet i
      // wait to receive response size from the serving Pet "i"
      vm->commwait(&(recv1commhList[i]));
      int responseStreamSize = responseStreamSizeClient[i];
      if (responseStreamSize>0){
        responseStreamClient[i] = new char[responseStreamSize];
        recv2commhList[i] = NULL;
//sprintf(msg, "posting nb-recv for message from PET %d size=%d", i, responseStreamSize);
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 

        vm->recv(responseStreamClient[i], responseStreamSize, i,
          &(recv2commhList[i]));
      }
    }
  }
  // localPet acts as server, send response stream
//ESMC_LogDefault.Write("accessLookup: step 6", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    int count = localIntervalPerPetCount[i];
    if (count>0){
      int responseStreamSize = responseStreamSizeServer[i];
      if (responseStreamSize>0){
        // construct response stream
        responseStreamServer[i] = new char[responseStreamSize];
        // t-specific server routine
        serverResponse(t, count, i, requestStreamServer, responseStreamServer);
        // send response stream to client Pet "i"
        send3commhList[i] = NULL;
//sprintf(msg, "posting nb-send to PET %d size=%d", i, responseStreamSize);
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 

#if (defined MUST_USE_BLOCKING_SEND || defined WORKAROUND_NONBLOCKPROGRESSBUG)
        vm->send(responseStreamServer[i], responseStreamSize, i);
#else
        vm->send(responseStreamServer[i], responseStreamSize, i,
          &(send3commhList[i]));
#endif
      }
      // garbage collection
      delete [] requestStreamServer[i];
    }
  }
  // localPet acts as a client, waits for response stream from server, process
//ESMC_LogDefault.Write("accessLookup: step 7", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet i
      int responseStreamSize = responseStreamSizeClient[i];
      if (responseStreamSize>0){
        // wait to receive response stream from the serving Pet "i"
//sprintf(msg, "commwait for message from PET %d, of size %d, with localElements: %d", i, responseStreamSize, localElementsPerIntervalCount[i]);
//ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO); 
        vm->commwait(&(recv2commhList[i]));
        // process responseStream and complete t[][] info
        char *responseStream = responseStreamClient[i];
        // t-specific client routine
        clientProcess(t, responseStream, responseStreamSize);
        // garbage collection
        delete [] responseStreamClient[i];
      }
    }
  }
  // localPet acts as a client, wait for sends to complete and collect garbage
//ESMC_LogDefault.Write("accessLookup: step 8", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet i
#if !(defined MUST_USE_BLOCKING_SEND || defined WORKAROUND_NONBLOCKPROGRESSBUG)
      // wait for send
      vm->commwait(&(send1commhList[i]));
#endif
      // garbage collection
      delete [] requestStreamClient[i];
    }
  }
  // localPet acts as server, wait for sends to complete and collect garbage
//ESMC_LogDefault.Write("accessLookup: step 9", ESMC_LOGMSG_INFO); 
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    int count = localIntervalPerPetCount[i];
    if (count>0){
#if !(defined MUST_USE_BLOCKING_SEND || defined WORKAROUND_NONBLOCKPROGRESSBUG)
      vm->commwait(&(send2commhList[i]));
#endif
      if (responseStreamSizeServer[i]>0){
#if !(defined MUST_USE_BLOCKING_SEND || defined WORKAROUND_NONBLOCKPROGRESSBUG)
        vm->commwait(&(send3commhList[i]));
#endif
        // garbage collection
        delete [] responseStreamServer[i];
      }
    }
  }  
  // garbage collection
  delete [] requestStreamClient;
  delete [] requestStreamServer;
  delete [] responseStreamClient;
  delete [] responseStreamServer;
  delete [] responseStreamSizeClient;
  delete [] responseStreamSizeServer;
  delete [] send1commhList;
  delete [] send2commhList;
  delete [] send3commhList;
  delete [] recv1commhList;
  delete [] recv2commhList;
  delete [] recv3commhList;
//ESMC_LogDefault.Write("accessLookup: step 10", ESMC_LOGMSG_INFO); 
}

// --------------------------------------------------
// FillLinSeqVectInfo-specific accessLookup routines:

template<typename IT1, typename IT2> 
  int requestSizeFactor(FillLinSeqVectInfo<IT1,IT2> *fillLinSeqVectInfo){
  return 4*sizeof(int) + sizeof(IT1);
  // lookupIndex, j, decompSeqIndex, tensorSeqIndex, linIndex
}

template<typename IT1, typename IT2> 
  void clientRequest(FillLinSeqVectInfo<IT1,IT2> *fillLinSeqVectInfo, 
  int dstPet, char **requestStreamClient){
  const int localDeCount = fillLinSeqVectInfo->localDeCount;
  const int *localDeElementCount = fillLinSeqVectInfo->localDeElementCount;
  const Interval<IT1> *seqIndexInterval = fillLinSeqVectInfo->seqIndexInterval;
  const bool tensorMixFlag = fillLinSeqVectInfo->tensorMixFlag;
  // fill the requestStreamClient[dstPet] element
  int *requestStreamClientInt = (int *)requestStreamClient[dstPet];
  IT1 seqIndMin = seqIndexInterval[dstPet].min;
  IT1 seqIndMax = seqIndexInterval[dstPet].max;
  IT1 seqIndCount = seqIndexInterval[dstPet].count;
  for (int j=0; j<localDeCount; j++){
    if (fillLinSeqVectInfo->haloRimFlag){
      // loop over the halo rim elements for localDe j
      for (int k=0; k<fillLinSeqVectInfo->array->getRimElementCount()[j]; k++){
        const std::vector<std::vector<SeqIndex<IT1> > > *rimSeqIndex;
        fillLinSeqVectInfo->array->getRimSeqIndex(&rimSeqIndex);
        SeqIndex<IT1> seqIndex = (*rimSeqIndex)[j][k];
        if (seqIndex.valid()){
          IT1 seqInd = seqIndex.decompSeqIndex;
          if (seqInd >= seqIndMin && seqInd <= seqIndMax){
            int lookupIndex = (int)(seqInd - seqIndMin);
            if (tensorMixFlag)
              lookupIndex += (seqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
            *requestStreamClientInt++   = lookupIndex;
            *requestStreamClientInt++   = j;
            IT1 *requestStreamClientIT1 = (IT1 *)requestStreamClientInt;
            *requestStreamClientIT1++   = seqIndex.decompSeqIndex;
            requestStreamClientInt      = (int *)requestStreamClientIT1;
            *requestStreamClientInt++   = seqIndex.tensorSeqIndex;
            *requestStreamClientInt++   =
              fillLinSeqVectInfo->array->getRimLinIndex()[j][k];
          }
        }
      }
    }else{
      // loop over all elements in the exclusive region for localDe j
      ArrayElement arrayElement(fillLinSeqVectInfo->array, j, true, false,
        false);
      while(arrayElement.isWithin()){
        SeqIndex<IT1> seqIndex = arrayElement.getSequenceIndex<IT1>();
        IT1 seqInd = seqIndex.decompSeqIndex;
        if (seqInd >= seqIndMin && seqInd <= seqIndMax){
          int lookupIndex = (int)(seqInd - seqIndMin);
          if (tensorMixFlag)
            lookupIndex += (seqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
          *requestStreamClientInt++   = lookupIndex;
          *requestStreamClientInt++   = j;
          IT1 *requestStreamClientIT1 = (IT1 *)requestStreamClientInt;
          *requestStreamClientIT1++   = seqIndex.decompSeqIndex;
          requestStreamClientInt      = (int *)requestStreamClientIT1;
          *requestStreamClientInt++   = seqIndex.tensorSeqIndex;
          *requestStreamClientInt++   =
            arrayElement.getLinearIndex();
        }
        arrayElement.next();
      } // end while over all exclusive elements
    }
  }
}

template<typename IT1, typename IT2> 
  void localClientServerExchange(FillLinSeqVectInfo<IT1,IT2> 
    *fillLinSeqVectInfo){
  const int localPet = fillLinSeqVectInfo->localPet;
  const int localDeCount = fillLinSeqVectInfo->localDeCount;
  const int *localDeElementCount = fillLinSeqVectInfo->localDeElementCount;
  vector<vector<AssociationElement<SeqIndex<IT1>,SeqIndex<IT2> > > >
    &linSeqVect = fillLinSeqVectInfo->linSeqVect;
  const Interval<IT1> *seqIndexInterval = fillLinSeqVectInfo->seqIndexInterval;
  const bool tensorMixFlag = fillLinSeqVectInfo->tensorMixFlag;
  vector<SeqIndexFactorLookup<IT1> > const &seqIndexFactorLookup =
    fillLinSeqVectInfo->seqIndexFactorLookup;
  // localPet locally acts as server and client
  IT1 seqIndMin = seqIndexInterval[localPet].min;
  IT1 seqIndMax = seqIndexInterval[localPet].max;
  IT1 seqIndCount = seqIndexInterval[localPet].count;
  for (int j=0; j<localDeCount; j++){
    if (fillLinSeqVectInfo->haloRimFlag){
      // loop over the halo rim elements for localDe j
      for (int k=0; k<fillLinSeqVectInfo->array->getRimElementCount()[j]; k++){
        const std::vector<std::vector<SeqIndex<IT1> > > *rimSeqIndex;
        fillLinSeqVectInfo->array->getRimSeqIndex(&rimSeqIndex);
        SeqIndex<IT1> seqIndex = (*rimSeqIndex)[j][k];
        if (seqIndex.valid()){
          IT1 seqInd = seqIndex.decompSeqIndex;
          if (seqInd >= seqIndMin && seqInd <= seqIndMax){
            int lookupIndex = (int)(seqInd - seqIndMin);
            if (tensorMixFlag)
              lookupIndex += (seqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
            int factorCount = seqIndexFactorLookup[lookupIndex].factorCount;
            if (factorCount > 0){
              AssociationElement<SeqIndex<IT1>,SeqIndex<IT2> > element;
              element.factorList =
                seqIndexFactorLookup[lookupIndex].factorList;
              element.linIndex =
                fillLinSeqVectInfo->array->getRimLinIndex()[j][k];
              element.seqIndex = seqIndex;
              linSeqVect[j].push_back(element);
            }
          }
        }
      }
    }else{
      // loop over all elements in the exclusive region for localDe j
      ArrayElement arrayElement(fillLinSeqVectInfo->array, j, true, false,
        false);
      while(arrayElement.isWithin()){
        SeqIndex<IT1> seqIndex = arrayElement.getSequenceIndex<IT1>();
        IT1 seqInd = seqIndex.decompSeqIndex;
        if (seqInd >= seqIndMin && seqInd <= seqIndMax){
          int lookupIndex = (int)(seqInd - seqIndMin);
          if (tensorMixFlag)
            lookupIndex += (seqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
          int factorCount = seqIndexFactorLookup[lookupIndex].factorCount;
          if (factorCount > 0){
            AssociationElement<SeqIndex<IT1>,SeqIndex<IT2> > element;
            element.factorList = seqIndexFactorLookup[lookupIndex].factorList;
            element.linIndex = arrayElement.getLinearIndex();
            element.seqIndex = seqIndex;
            linSeqVect[j].push_back(element);
         }
        }
        arrayElement.next();
      } // end while over all exclusive elements
    }
  }
}

template<typename IT1, typename IT2> 
  int serverResponseSize(FillLinSeqVectInfo<IT1,IT2> *fillLinSeqVectInfo, 
    int count, int srcPet, char **requestStreamServer){
  vector<SeqIndexFactorLookup<IT1> > const &seqIndexFactorLookup =
    fillLinSeqVectInfo->seqIndexFactorLookup;
  // process requestStreamServer[srcPet] and return response stream size
  int indexCounter = 0; // reset
  int factorElementCounter = 0; // reset
  int partnerDeCounter = 0; // reset
  char *requestStreamServerChar = (char *)requestStreamServer[srcPet];
  for (int j=0; j<count; j++){
    int *requestStreamServerInt = (int *)(requestStreamServerChar 
      + j * (4*sizeof(int) + sizeof(IT1)));
    int lookupIndex = *requestStreamServerInt;
    int factorCount = seqIndexFactorLookup[lookupIndex].factorCount;
    if (factorCount > 0){
      ++indexCounter;
      factorElementCounter += factorCount;
      for (int jj=0; jj<factorCount; jj++)
        partnerDeCounter +=
          seqIndexFactorLookup[lookupIndex].factorList[jj].partnerDE.size();
    }
  }
  int responseStreamSize = 
    4*indexCounter*sizeof(int) +            // AssociationElement members
    indexCounter*sizeof(IT1) +              // decompSeqIndex
    factorElementCounter*sizeof(IT2) +      // partnerSeqIndex.decompSeqIndex
    2*factorElementCounter*sizeof(int) +    // FactorElement members
    partnerDeCounter*sizeof(int) +          // partnerDE elements
    8*factorElementCounter;                 // FactorElement factor
  return responseStreamSize;
}
      
template<typename IT1, typename IT2> 
  void serverResponse(FillLinSeqVectInfo<IT1,IT2> *fillLinSeqVectInfo,
    int count, int srcPet, char **requestStreamServer, 
    char **responseStreamServer){
  vector<SeqIndexFactorLookup<IT1> > const &seqIndexFactorLookup =
    fillLinSeqVectInfo->seqIndexFactorLookup;
  // construct response stream
  int *responseStreamInt = (int *)responseStreamServer[srcPet];
  char *requestStreamServerChar = (char *)requestStreamServer[srcPet];
  for (int jj=0; jj<count; jj++){
    int *requestStreamServerInt = (int *)(requestStreamServerChar 
      + jj * (4*sizeof(int) + sizeof(IT1)));
    int lookupIndex = *requestStreamServerInt++;
    int factorCount = seqIndexFactorLookup[lookupIndex].factorCount;
    if (factorCount > 0){
      *responseStreamInt++ = *requestStreamServerInt++;     // j
      IT1 *responseStreamIntIT1 = (IT1 *)responseStreamInt;
      IT1 *requestStreamServerIT1 = (IT1 *)requestStreamServerInt;
      *responseStreamIntIT1++ = *requestStreamServerIT1++;  // decompSeqIndex
      responseStreamInt = (int *)responseStreamIntIT1;
      requestStreamServerInt = (int *)requestStreamServerIT1;
      *responseStreamInt++ = *requestStreamServerInt++;     // tensorSeqIndex
      *responseStreamInt++ = *requestStreamServerInt++;     // linIndex
      *responseStreamInt++ = factorCount;
      for (int k=0; k<factorCount; k++){
        IT2 *responseStreamIT2 = (IT2 *)responseStreamInt;
        *responseStreamIT2++ = seqIndexFactorLookup[lookupIndex].factorList[k]
          .partnerSeqIndex.decompSeqIndex;
        responseStreamInt = (int *)responseStreamIT2;
        *responseStreamInt++ = seqIndexFactorLookup[lookupIndex].factorList[k]
          .partnerSeqIndex.tensorSeqIndex;
        int size = seqIndexFactorLookup[lookupIndex]
          .factorList[k].partnerDE.size();
        *responseStreamInt++ = size;
        for (int kk=0; kk<size; kk++)
          *responseStreamInt++ = seqIndexFactorLookup[lookupIndex]
            .factorList[k].partnerDE[kk];
        char *responseStreamChar = (char *)responseStreamInt;
        for (int kk=0; kk<8; kk++)
          *responseStreamChar++ =
            seqIndexFactorLookup[lookupIndex].factorList[k].factor[kk];
        responseStreamInt = (int *)responseStreamChar;
      }
    }
  }
}        
        
template<typename IT1, typename IT2> 
  void clientProcess(FillLinSeqVectInfo<IT1,IT2> *fillLinSeqVectInfo, 
    char *responseStream, int responseStreamSize){
  vector<vector<AssociationElement<SeqIndex<IT1>,SeqIndex<IT2> > > >
    &linSeqVect = fillLinSeqVectInfo->linSeqVect;
  // process responseStream and fill linSeqVect[][]
  int *responseStreamInt = (int *)responseStream;
  while ((char *)responseStreamInt != responseStream+responseStreamSize){
    int j = *responseStreamInt++;
    IT1 *responseStreamIT1 = (IT1 *)responseStreamInt;
    IT1 decompSeqIndex = *responseStreamIT1++;
    responseStreamInt = (int *)responseStreamIT1;
    int tensorSeqIndex = *responseStreamInt++;
    int linIndex = *responseStreamInt++;
    int factorCount = *responseStreamInt++;
    AssociationElement<SeqIndex<IT1>,SeqIndex<IT2> > element;
    element.factorList.resize(factorCount);
    for (int jj=0; jj<factorCount; jj++){
      IT2 *responseStreamIT2 = (IT2 *)responseStreamInt;
      element.factorList[jj].partnerSeqIndex.decompSeqIndex =
        *responseStreamIT2++;
      responseStreamInt = (int *)responseStreamIT2;
      element.factorList[jj].partnerSeqIndex.tensorSeqIndex =
        *responseStreamInt++;
      int size = *responseStreamInt++;
      element.factorList[jj].partnerDE.resize(size);
      for (int kk=0; kk<size; kk++)
        element.factorList[jj].partnerDE[kk] = *responseStreamInt++;
      char *responseStreamChar = (char *)responseStreamInt;
      for (int kk=0; kk<8; kk++)
        element.factorList[jj].factor[kk] = *responseStreamChar++;
      responseStreamInt = (int *)responseStreamChar;
    }
    element.linIndex = linIndex;
    element.seqIndex.decompSeqIndex = decompSeqIndex;
    element.seqIndex.tensorSeqIndex = tensorSeqIndex;
    linSeqVect[j].push_back(element);
  }
}
        
// -------------------------------------------------
// FillPartnerDeInfo-specific accessLookup routines:

template<typename IT1, typename IT2>
  int requestSizeFactor(FillPartnerDeInfo<IT1,IT2> *fillPartnerDeInfo){
  return 3*sizeof(int); // lookupIndex, localLookupIndex, k
}

template<typename IT1, typename IT2>
  void clientRequest(FillPartnerDeInfo<IT1,IT2> *fillPartnerDeInfo, int dstPet,
  char **requestStreamClient){
  const int localPet = fillPartnerDeInfo->localPet;
  const Interval<IT1> *seqIndexIntervalIn =
    fillPartnerDeInfo->seqIndexIntervalIn;
  const Interval<IT2> *seqIndexIntervalOut =
    fillPartnerDeInfo->seqIndexIntervalOut;
  vector<SeqIndexFactorLookup<IT2> > &seqIndexFactorLookupOut =
    fillPartnerDeInfo->seqIndexFactorLookupOut;
  const bool tensorMixFlag = fillPartnerDeInfo->tensorMixFlag;
  // fill the requestStreamClient[dstPet] element
  IT1 seqIndMin = seqIndexIntervalIn[dstPet].min;
  IT1 seqIndMax = seqIndexIntervalIn[dstPet].max;
  IT1 seqIndCount = seqIndexIntervalIn[dstPet].count;
  int jj = 0; // reset
  int localLookupIndex = 0; // reset
  for (typename vector<DD::SeqIndexFactorLookup<IT2> >::const_iterator
    j=seqIndexFactorLookupOut.begin(); j!=seqIndexFactorLookupOut.end(); ++j){
    for (int k=0; k<j->factorCount; k++){
      IT2 partnerSeqInd = j->factorList[k]
        .partnerSeqIndex.decompSeqIndex;
      if (partnerSeqInd >= seqIndMin && partnerSeqInd <= seqIndMax){
        int lookupIndex = (int)(partnerSeqInd - seqIndMin);
        if (tensorMixFlag){
          lookupIndex += (j->factorList[k]
            .partnerSeqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
        }
        int *requestStreamClientInt = (int *)requestStreamClient[dstPet];
        requestStreamClientInt[3*jj] = lookupIndex;
        requestStreamClientInt[3*jj+1] = localLookupIndex;
        requestStreamClientInt[3*jj+2] = k;
#ifdef DEBUGLOG
        {
          std::stringstream debugmsg;
          debugmsg << "clientRequest()#" << __LINE__ 
            << " ,dstPet=" << dstPet
            << "lookupIndex " << requestStreamClientInt[3*jj];
          ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
        }
#endif
        ++jj; // increment counter
      }
    }
    ++localLookupIndex;
  }
}

template<typename IT1, typename IT2>
  void localClientServerExchange(FillPartnerDeInfo<IT1,IT2> *fillPartnerDeInfo){
  const int localPet = fillPartnerDeInfo->localPet;
  const Interval<IT1> *seqIndexIntervalIn =
    fillPartnerDeInfo->seqIndexIntervalIn;
  const Interval<IT2> *seqIndexIntervalOut =
    fillPartnerDeInfo->seqIndexIntervalOut;
  vector<SeqIndexFactorLookup<IT1> > &seqIndexFactorLookupIn =
    fillPartnerDeInfo->seqIndexFactorLookupIn;
  vector<SeqIndexFactorLookup<IT2> > &seqIndexFactorLookupOut =
    fillPartnerDeInfo->seqIndexFactorLookupOut;
  const bool tensorMixFlag = fillPartnerDeInfo->tensorMixFlag;
  // localPet locally acts as server and client
  IT1 seqIndMin = seqIndexIntervalIn[localPet].min;
  IT1 seqIndMax = seqIndexIntervalIn[localPet].max;
  IT1 seqIndCount = seqIndexIntervalIn[localPet].count;
  for (typename vector<DD::SeqIndexFactorLookup<IT2> >::iterator
    j=seqIndexFactorLookupOut.begin(); j!=seqIndexFactorLookupOut.end(); ++j){
    for (int k=0; k<j->factorCount; k++){
      IT2 partnerSeqInd = j->factorList[k]
        .partnerSeqIndex.decompSeqIndex;
      if (partnerSeqInd >= seqIndMin && partnerSeqInd <= seqIndMax){
        int lookupIndex = (int)(partnerSeqInd - seqIndMin);
        if (tensorMixFlag){
          lookupIndex += (j->factorList[k]
            .partnerSeqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
        }
        j->factorList[k].partnerDE.insert(
          j->factorList[k].partnerDE.end(),
          seqIndexFactorLookupIn[lookupIndex].de.begin(),
          seqIndexFactorLookupIn[lookupIndex].de.end());
      }
    }
  }
}

template<typename IT1, typename IT2>
  int serverResponseSize(FillPartnerDeInfo<IT1,IT2> *fillPartnerDeInfo, 
    int count, int srcPet, char **requestStreamServer){
  vector<SeqIndexFactorLookup<IT1> > &seqIndexFactorLookupIn =
    fillPartnerDeInfo->seqIndexFactorLookupIn;
  int *requestStreamServerInt = (int *)requestStreamServer[srcPet];
  int responseCount = 0;  // reset
  for (int i=0; i<count; i++){
    int lookupIndex = requestStreamServerInt[3*i];
    responseCount += seqIndexFactorLookupIn[lookupIndex].de.size();
    responseCount += 3; // localLookupIndex, k, size
  }
  int responseStreamSize = responseCount * sizeof(int);
  return responseStreamSize;
}
      
template<typename IT1, typename IT2>
  void serverResponse(FillPartnerDeInfo<IT1,IT2> *fillPartnerDeInfo, int count,
    int srcPet, char **requestStreamServer, char **responseStreamServer){
  vector<SeqIndexFactorLookup<IT1> > &seqIndexFactorLookupIn =
    fillPartnerDeInfo->seqIndexFactorLookupIn;
  // construct response stream
  int *responseStreamInt = (int *)responseStreamServer[srcPet];
  int *requestStreamServerInt = (int *)requestStreamServer[srcPet];
  for (int i=0; i<count; i++){
    int lookupIndex = requestStreamServerInt[3*i];
    *responseStreamInt++ = requestStreamServerInt[3*i+1];   // localLookupIndex
    *responseStreamInt++ = requestStreamServerInt[3*i+2];   // k
    int size = seqIndexFactorLookupIn[lookupIndex].de.size();
    *responseStreamInt++ = size;                            // size
    for (int j=0; j<size; j++)
      *responseStreamInt++ = seqIndexFactorLookupIn[lookupIndex].de[j]; // de
  }
}        
        
template<typename IT1, typename IT2>
  void clientProcess(FillPartnerDeInfo<IT1,IT2> *fillPartnerDeInfo,
    char *responseStream, int responseStreamSize){
  vector<SeqIndexFactorLookup<IT2> > &seqIndexFactorLookupOut =
    fillPartnerDeInfo->seqIndexFactorLookupOut;
  // process responseStream and complete seqIndexFactorLookupOut info
  int *responseStreamInt = (int *)responseStream;
  while ((char *)responseStreamInt != responseStream+responseStreamSize){
    int localLookupIndex = *responseStreamInt++;
    int k = *responseStreamInt++;
    int size = *responseStreamInt++;
    for (int j=0; j<size; j++){
      int de = *responseStreamInt++;
      seqIndexFactorLookupOut[localLookupIndex].factorList[k].partnerDE
        .push_back(de);
    }
  }
}

  // -------------------------------------------------
  // specialize ComPat class for total exchange during DE info fill.
  template<typename IT> class FillSelfDeInfo:public ComPat{
    Array const *array;
    int localPet;
    int localDeCount;
    int const *localDeElementCount;
    int const *localDeToDeMap;
    Interval<IT> const *seqIndexInterval;
    vector<SeqIndexFactorLookup<IT> > &seqIndexFactorLookup;
    bool tensorMixFlag;
    int const *localIntervalPerPetCount;
    int const *localElementsPerIntervalCount;
    bool haloRimFlag;
   public:
    FillSelfDeInfo(
      Array const *array_,
      int localPet_,
      int localDeCount_,
      int const *localDeElementCount_,
      int const *localDeToDeMap_,
      Interval<IT> const *seqIndexInterval_,
      vector<SeqIndexFactorLookup<IT> > &seqIndexFactorLookup_,
      bool tensorMixFlag_,
      int const *localIntervalPerPetCount_,
      int const *localElementsPerIntervalCount_,
      bool haloRimFlag_
    ):
      // members that need to be set on this level because of reference
      seqIndexFactorLookup(seqIndexFactorLookup_)
    {
      array = array_;
      localPet = localPet_;
      localDeCount = localDeCount_;
      localDeElementCount = localDeElementCount_;
      localDeToDeMap = localDeToDeMap_;
      seqIndexInterval = seqIndexInterval_;
      tensorMixFlag = tensorMixFlag_;
      localIntervalPerPetCount = localIntervalPerPetCount_;
      localElementsPerIntervalCount = localElementsPerIntervalCount_;
      haloRimFlag = haloRimFlag_;
    }
   private:
    int messageSizeCount(int srcPet, int dstPet)const{
      if (localPet == srcPet)
        return localElementsPerIntervalCount[dstPet];
      else if (localPet == dstPet)
        return localIntervalPerPetCount[srcPet];
      else{
        return 0; // provoke MPI errors
      }
    }
    virtual int messageSize(int srcPet, int dstPet)const{
      return 2 * sizeof(int) * messageSizeCount(srcPet, dstPet);
    }
    virtual void messagePrepare(int srcPet, int dstPet, char *buffer)const{
      IT seqIndMin = seqIndexInterval[dstPet].min;
      IT seqIndMax = seqIndexInterval[dstPet].max;
      IT seqIndCount = seqIndexInterval[dstPet].count;
      int *bufferInt = (int *)buffer;
      int jj = 0; // reset
      for (int j=0; j<localDeCount; j++){
        int de = localDeToDeMap[j];  // global DE number
        if (haloRimFlag){
          // loop over the halo rim elements for localDe j
          for (int k=0; k<array->getRimElementCount()[j]; k++){
            const std::vector<std::vector<SeqIndex<IT> > > *rimSeqIndex;
            array->getRimSeqIndex(&rimSeqIndex);
            SeqIndex<IT> seqIndex = (*rimSeqIndex)[j][k];
            if (seqIndex.valid()){
              IT seqInd = seqIndex.decompSeqIndex;
              if (seqInd >= seqIndMin && seqInd <= seqIndMax){
                int lookupIndex = (int)(seqInd - seqIndMin);
                if (tensorMixFlag)
                  lookupIndex += (seqIndex.tensorSeqIndex - 1)
                  * (int)seqIndCount;
                bufferInt[2*jj] = lookupIndex;
                bufferInt[2*jj+1] = de;
                ++jj; // increment counter
              }
            }
          }
        }else{
          // loop over all elements in the exclusive region for localDe j
          ArrayElement arrayElement(array, j, true, false, false);
          while(arrayElement.isWithin()){
            SeqIndex<IT> seqIndex = arrayElement.getSequenceIndex<IT>();
            IT seqInd = seqIndex.decompSeqIndex;
            if (seqInd >= seqIndMin && seqInd <= seqIndMax){
              int lookupIndex = (int)(seqInd - seqIndMin);
              if (tensorMixFlag)
                lookupIndex += (seqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
              bufferInt[2*jj] = lookupIndex;
              bufferInt[2*jj+1] = de;
              ++jj; // increment counter
            }
            arrayElement.next();
          } // end while over all exclusive elements
        }
      }
    }
    virtual void messageProcess(int srcPet, int dstPet, char *buffer){
      int count = messageSizeCount(srcPet, dstPet);
      int *bufferInt = (int *)buffer;    
      for (int jj=0; jj<count; jj++){
        int lookupIndex = bufferInt[2*jj];
        if (seqIndexFactorLookup[lookupIndex].factorCount > 0){
          // element with factors -> fill in the DE
          seqIndexFactorLookup[lookupIndex].de.push_back(bufferInt[2*jj+1]);
          // this will lead to duplicate de entries for cases with tensor
          // elements but no tensor mixing
          // -> duplicates must be eliminated by the calling code
        }
      }
    }
    virtual void localPrepareAndProcess(int localPet){
      IT seqIndMin = seqIndexInterval[localPet].min;
      IT seqIndMax = seqIndexInterval[localPet].max;
      IT seqIndCount = seqIndexInterval[localPet].count;
      for (int j=0; j<localDeCount; j++){
        int de = localDeToDeMap[j];  // global DE number
        if (haloRimFlag){
          // loop over the halo rim elements for localDe j
          for (int k=0; k<array->getRimElementCount()[j]; k++){
            const std::vector<std::vector<SeqIndex<IT> > > *rimSeqIndex;
            array->getRimSeqIndex(&rimSeqIndex);
            SeqIndex<IT> seqIndex = (*rimSeqIndex)[j][k];
            if (seqIndex.valid()){
              IT seqInd = seqIndex.decompSeqIndex;
              if (seqInd >= seqIndMin && seqInd <= seqIndMax){
                int lookupIndex = (int)(seqInd - seqIndMin);
                if (tensorMixFlag)
                  lookupIndex += (seqIndex.tensorSeqIndex - 1)
                  * (int)seqIndCount;
                if (seqIndexFactorLookup[lookupIndex].factorCount > 0){
                  // element with factors -> fill in the DE
                  seqIndexFactorLookup[lookupIndex].de.push_back(de);
                  // this will lead to duplicate de entries for cases with
                  // tensor elements but no tensor mixing
                  // -> duplicates must be eliminated by the calling code
                }
              }
            }
          }
        }else{
          // loop over all elements in the exclusive region for localDe j
          ArrayElement arrayElement(array, j, true, false, false);
          while(arrayElement.isWithin()){
            SeqIndex<IT> seqIndex = arrayElement.getSequenceIndex<IT>();
            IT seqInd = seqIndex.decompSeqIndex;
            if (seqInd >= seqIndMin && seqInd <= seqIndMax){
              int lookupIndex = (int)(seqInd - seqIndMin);
              if (tensorMixFlag)
                lookupIndex += (seqIndex.tensorSeqIndex - 1) * (int)seqIndCount;
              if (seqIndexFactorLookup[lookupIndex].factorCount > 0){
                // element with factors -> fill in the DE
                seqIndexFactorLookup[lookupIndex].de.push_back(de);
                // this will lead to duplicate de entries for cases with tensor
                // elements but no tensor mixing
                // -> duplicates must be eliminated by the calling code
              }
            }
            arrayElement.next();
          } // end while over all exclusive elements
        }
      }
    }
  };
        
  // -------------------------------------------------
  // abstract parent class for DE info fill stage1 and stage2
  template<typename IT> class SetupSeqIndexFactorLookup:public ComPat{
   protected:
    vector<SeqIndexFactorLookup<IT> > &seqIndexFactorLookup;
    int localPet;
    int petCount;
    SparseMatrix<IT,IT> const *sparseMatrix;
    vector<bool> const &factorPetFlag;
    Interval<IT> const *seqIndexInterval;
    vector<int> const &seqIntervFactorListCountToPet;
    vector<vector<int> > const &seqIntervFactorListIndexToPet;
    vector<vector<int> > const &seqIntervFactorListLookupIndexToPet;
    vector<int> const &seqIntervFactorListCountFromPet;
    bool tensorMixFlag;
    bool dstSetupFlag;
    ESMC_TypeKind_Flag typekindFactors;
   public:
    SetupSeqIndexFactorLookup(
      vector<SeqIndexFactorLookup<IT> > &seqIndexFactorLookup_,
      SparseMatrix<IT,IT> const *sparseMatrix_,
      vector<bool> const &factorPetFlag_,
      vector<int> const &seqIntervFactorListCountToPet_,
      vector<vector<int> > const &seqIntervFactorListIndexToPet_,
      vector<vector<int> > const &seqIntervFactorListLookupIndexToPet_,
      vector<int> const &seqIntervFactorListCountFromPet_
    ):
      // members that need to be set on this level because of reference
      seqIndexFactorLookup(seqIndexFactorLookup_),
      sparseMatrix(sparseMatrix_),
      factorPetFlag(factorPetFlag_),
      seqIntervFactorListCountToPet(seqIntervFactorListCountToPet_),
      seqIntervFactorListIndexToPet(seqIntervFactorListIndexToPet_),
      seqIntervFactorListLookupIndexToPet(seqIntervFactorListLookupIndexToPet_),
      seqIntervFactorListCountFromPet(seqIntervFactorListCountFromPet_)
    {}
  };

  template<typename IT> class SetupSeqIndexFactorLookupStage2;

  // -------------------------------------------------
  // specialize SetupSeqIndexFactorLookup for info fill stage1
  template<typename IT> class SetupSeqIndexFactorLookupStage1:
    public SetupSeqIndexFactorLookup<IT>{
   public:
    SetupSeqIndexFactorLookupStage1(
      vector<SeqIndexFactorLookup<IT> > &seqIndexFactorLookup_,
      int localPet_,
      int petCount_,
      SparseMatrix<IT,IT> const *sparseMatrix_,
      vector<bool> const &factorPetFlag_,
      Interval<IT> const *seqIndexInterval_,
      vector<int> const &seqIntervFactorListCountToPet_,
      vector<vector<int> > const &seqIntervFactorListIndexToPet_,
      vector<vector<int> > const &seqIntervFactorListLookupIndexToPet_,
      vector<int> const &seqIntervFactorListCountFromPet_,
      bool tensorMixFlag_,
      bool dstSetupFlag_,
      ESMC_TypeKind_Flag typekindFactors_
    ):SetupSeqIndexFactorLookup<IT>(
      seqIndexFactorLookup_,
      sparseMatrix_,
      factorPetFlag_,
      seqIntervFactorListCountToPet_,
      seqIntervFactorListIndexToPet_,
      seqIntervFactorListLookupIndexToPet_,
      seqIntervFactorListCountFromPet_
    ){
      SetupSeqIndexFactorLookup<IT>::localPet = localPet_;
      SetupSeqIndexFactorLookup<IT>::petCount = petCount_;
      SetupSeqIndexFactorLookup<IT>::seqIndexInterval = seqIndexInterval_;
      SetupSeqIndexFactorLookup<IT>::tensorMixFlag = tensorMixFlag_;
      SetupSeqIndexFactorLookup<IT>::dstSetupFlag = dstSetupFlag_;
      SetupSeqIndexFactorLookup<IT>::typekindFactors = typekindFactors_;
    }
   private:
    int messageSizeCount(int srcPet, int dstPet)const{
      if (SetupSeqIndexFactorLookup<IT>::localPet == srcPet)
        return SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListCountToPet[dstPet];
      else if (SetupSeqIndexFactorLookup<IT>::localPet == dstPet)
        return SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListCountFromPet[srcPet];
      else
        return 0; // provoke MPI errors
    }
    virtual int messageSize(int srcPet, int dstPet)const{
#ifdef DEBUGLOG
    {
      std::stringstream debugmsg;
      debugmsg << "SetupSeqIndexFactorLookupStage1().messageSize(srcPet=" 
        << srcPet << " ,dstPet=" << dstPet << "): " <<
        sizeof(int) * messageSizeCount(srcPet, dstPet);
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif
      return sizeof(int) * messageSizeCount(srcPet, dstPet);
    }
    virtual void messagePrepare(int srcPet, int dstPet, char *buffer)const{
      memcpy(buffer, &(SetupSeqIndexFactorLookup<IT>::
        seqIntervFactorListLookupIndexToPet[dstPet].front()),
        messageSizeCount(srcPet, dstPet)*sizeof(int));
    }
    virtual void messageProcess(int srcPet, int dstPet, char *buffer){
      int *bufferInt = (int *)buffer;
      for (int i=0; i<messageSizeCount(srcPet, dstPet); i++){
        // loop over buffer entries
        int lookupIndex = bufferInt[i];
        // count this factor
        ++(SetupSeqIndexFactorLookup<IT>::
          seqIndexFactorLookup[lookupIndex].factorCount);
      }
    }
    virtual void localPrepareAndProcess(int localPet){
      for (int i=0; i<messageSizeCount(localPet, localPet); i++){
        // loop over factorList entries in localPet's seqIndex interval
        int lookupIndex = SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListLookupIndexToPet[localPet][i];
        // count this factor
        ++(SetupSeqIndexFactorLookup<IT>::
          seqIndexFactorLookup[lookupIndex].factorCount);
      }
    }
    friend class SetupSeqIndexFactorLookupStage2<IT>;
  };
  
  // -------------------------------------------------
  // specialize SetupSeqIndexFactorLookup for info fill stage2
  template<typename IT> class SetupSeqIndexFactorLookupStage2:
    public SetupSeqIndexFactorLookup<IT>{
   public:
    SetupSeqIndexFactorLookupStage2(
      SetupSeqIndexFactorLookupStage1<IT> const &s1)
      :SetupSeqIndexFactorLookup<IT>(
      s1.seqIndexFactorLookup,
      s1.sparseMatrix,
      s1.factorPetFlag,
      s1.seqIntervFactorListCountToPet,
      s1.seqIntervFactorListIndexToPet,
      s1.seqIntervFactorListLookupIndexToPet,
      s1.seqIntervFactorListCountFromPet
    ){
      SetupSeqIndexFactorLookup<IT>::localPet = s1.localPet;
      SetupSeqIndexFactorLookup<IT>::petCount = s1.petCount;
      SetupSeqIndexFactorLookup<IT>::seqIndexInterval = s1.seqIndexInterval;
      SetupSeqIndexFactorLookup<IT>::tensorMixFlag = s1.tensorMixFlag;
      SetupSeqIndexFactorLookup<IT>::dstSetupFlag = s1.dstSetupFlag;
      SetupSeqIndexFactorLookup<IT>::typekindFactors = s1.typekindFactors;
    }
   private:
    int messageSizeCount(int srcPet, int dstPet)const{
      if (SetupSeqIndexFactorLookup<IT>::localPet == srcPet)
        return SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListCountToPet[dstPet];
      else if (SetupSeqIndexFactorLookup<IT>::localPet == dstPet)
        return SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListCountFromPet[srcPet];
      else{
        return 0; // provoke MPI errors
      }
    }
    virtual int messageSize(int srcPet, int dstPet)const{
      int dataSizeFactors =
        ESMC_TypeKind_FlagSize(SetupSeqIndexFactorLookup<IT>::typekindFactors);
      //todo: reduce waste of bandwidth in case there are _not_ 4-comp ind.
#ifdef DEBUGLOG
      {
        std::stringstream debugmsg;
        debugmsg << "SetupSeqIndexFactorLookupStage2().messageSize(srcPet=" 
          << srcPet << " ,dstPet=" << dstPet << "): " <<
          (2*sizeof(int)+sizeof(IT)+dataSizeFactors) 
          * messageSizeCount(srcPet, dstPet)
          << " from dataSizeFactors=" << dataSizeFactors
          << " messageSizeCount=" << messageSizeCount(srcPet, dstPet);
        ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
      return (2*sizeof(int)+sizeof(IT)+dataSizeFactors)
        * messageSizeCount(srcPet, dstPet);
    }
    virtual void messagePrepare(int srcPet, int dstPet, char *buffer)const{
      ESMC_TypeKind_Flag typekindFactors =
        SetupSeqIndexFactorLookup<IT>::typekindFactors;
      if (typekindFactors == ESMC_TYPEKIND_R4)
        fillStream<ESMC_R4>(srcPet, dstPet, buffer);
      else if (typekindFactors == ESMC_TYPEKIND_R8)
        fillStream<ESMC_R8>(srcPet, dstPet, buffer);
      else if (typekindFactors == ESMC_TYPEKIND_I4)
        fillStream<ESMC_I4>(srcPet, dstPet, buffer);
      else if (typekindFactors == ESMC_TYPEKIND_I8)
        fillStream<ESMC_I8>(srcPet, dstPet, buffer);
    }
    virtual void messageProcess(int srcPet, int dstPet, char *buffer){
      ESMC_TypeKind_Flag typekindFactors =
        SetupSeqIndexFactorLookup<IT>::typekindFactors;
      if (typekindFactors == ESMC_TYPEKIND_R4)
        fillSeqIndexFactorLookupFromStream<ESMC_R4>(srcPet, dstPet, buffer);
      else if (typekindFactors == ESMC_TYPEKIND_R8)
        fillSeqIndexFactorLookupFromStream<ESMC_R8>(srcPet, dstPet, buffer);
      else if (typekindFactors == ESMC_TYPEKIND_I4)
        fillSeqIndexFactorLookupFromStream<ESMC_I4>(srcPet, dstPet, buffer);
      else if (typekindFactors == ESMC_TYPEKIND_I8)
        fillSeqIndexFactorLookupFromStream<ESMC_I8>(srcPet, dstPet, buffer);
    }
    virtual void localPrepareAndProcess(int localPet){
      ESMC_TypeKind_Flag typekindFactors =
        SetupSeqIndexFactorLookup<IT>::typekindFactors;
      if (typekindFactors == ESMC_TYPEKIND_R4)
        fillSeqIndexFactorLookupLocally<ESMC_R4>(localPet);
      else if (typekindFactors == ESMC_TYPEKIND_R8)
        fillSeqIndexFactorLookupLocally<ESMC_R8>(localPet);
      else if (typekindFactors == ESMC_TYPEKIND_I4)
        fillSeqIndexFactorLookupLocally<ESMC_I4>(localPet);
      else if (typekindFactors == ESMC_TYPEKIND_I8)
        fillSeqIndexFactorLookupLocally<ESMC_I8>(localPet);
    }
    template<typename T> void fillStream(int srcPet, int dstPet,
      char *stream)const{
      int *intStream;
      T *factorStream = (T *)stream;
      for (int i=0; i<messageSizeCount(srcPet, dstPet); i++){
        // loop over factorList entries in dstPet's seqIndex interval
        intStream = (int *)factorStream;
        int lookupIndex = SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListLookupIndexToPet[dstPet][i];
        int j = SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListIndexToPet[dstPet][i];
        SeqInd<IT> seqInd;
        if (SetupSeqIndexFactorLookup<IT>::dstSetupFlag)
          seqInd = SetupSeqIndexFactorLookup<IT>::
            sparseMatrix->getSrcSeqIndex(j);
          // reverse to lookupIndex construction b/c src side picks dst and rev.
        else
          seqInd = SetupSeqIndexFactorLookup<IT>::
            sparseMatrix->getDstSeqIndex(j);
          // reverse to lookupIndex construction b/c src side picks dst and rev.
        IT seqIndex = seqInd.getIndex(0);
        int tensorSeqIndex = -1;  // dummy tensorSeqIndex
        if (SetupSeqIndexFactorLookup<IT>::tensorMixFlag)
          tensorSeqIndex = (int)seqInd.getIndex(1);  // set actual tensor seqIndex
        *intStream++ = lookupIndex;     // index into distr. dir lookup table
#ifdef DEBUGLOG
        {
          std::stringstream debugmsg;
          debugmsg << "fillStream()#" << __LINE__ 
            << " ,srcPet=" << srcPet 
            << " ,dstPet=" << dstPet 
            << " lookupIndex " << lookupIndex;
          ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
        }
#endif
        *intStream++ = tensorSeqIndex;  // dummy tensorSeqIndex
        IT *itStream = (IT *)intStream;
        *itStream++ = seqIndex;         // seqIndex
        factorStream = (T *)itStream;
        *factorStream++ = ((T *)SetupSeqIndexFactorLookup<IT>::
          sparseMatrix->getFactorList())[j];  // factor
      }
    }
    template<typename T> void fillSeqIndexFactorLookupFromStream(int srcPet,
      int dstPet, char *stream){
      int *intStream;
      T *factorStream = (T *)stream;
      for (int i=0; i<messageSizeCount(srcPet, dstPet); i++){
        intStream = (int *)factorStream;
        int lookupIndex = *intStream++;   // index into lookup table
        FactorElement<SeqIndex<IT> > factorElement;
        factorElement.partnerSeqIndex.tensorSeqIndex = *intStream++;
        IT *itStream = (IT *)intStream;
        factorElement.partnerSeqIndex.decompSeqIndex = *itStream++;
        factorStream = (T *)itStream;
        *((T *)factorElement.factor) = *factorStream++;
#ifdef DEBUGLOG
        {
          std::stringstream msg;
          msg << "fillSeqIndexFactorLookupFromStream: (srcPet=" 
            << srcPet << " ,dstPet=" << dstPet << "): "
            << " seqIndexFactorLookup.size()=" 
            << SetupSeqIndexFactorLookup<IT>::seqIndexFactorLookup.size()
            << " lookupIndex=" << lookupIndex;
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
        }
        if (lookupIndex<0 ||
          lookupIndex >
          (int)SetupSeqIndexFactorLookup<IT>::seqIndexFactorLookup.size()){
          int rc;
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            "lookupIndex out of range", ESMC_CONTEXT, &rc);
          throw rc;  // bail out with exception
        }
#endif
        SetupSeqIndexFactorLookup<IT>::
          seqIndexFactorLookup[lookupIndex].factorList.push_back(factorElement);
      }
    }
    template<typename T> void fillSeqIndexFactorLookupLocally(int localPet){
      for (int i=0; i<messageSizeCount(localPet, localPet); i++){
        // loop over factorList entries in localPet's seqIndex interval
        int j = SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListIndexToPet[localPet][i];
        SeqInd<IT> seqInd;
        if (SetupSeqIndexFactorLookup<IT>::dstSetupFlag)
          seqInd = SetupSeqIndexFactorLookup<IT>::
            sparseMatrix->getSrcSeqIndex(j);
          // reverse b/c src side picks dst and rev.
        else
          seqInd = SetupSeqIndexFactorLookup<IT>::
            sparseMatrix->getDstSeqIndex(j);
          // reverse b/c src side picks dst and rev.
        IT seqIndex = seqInd.getIndex(0);
        int tensorSeqIndex = -1;  // dummy tensorSeqIndex
        if (SetupSeqIndexFactorLookup<IT>::tensorMixFlag)
          tensorSeqIndex = (int)seqInd.getIndex(1);  // set actual tensor seqIndex
        int lookupIndex = SetupSeqIndexFactorLookup<IT>::
          seqIntervFactorListLookupIndexToPet[localPet][i];
        FactorElement<SeqIndex<IT> > factorElement;
        factorElement.partnerSeqIndex.decompSeqIndex = seqIndex;
        factorElement.partnerSeqIndex.tensorSeqIndex = tensorSeqIndex;
        *((T *)factorElement.factor) = ((T *)SetupSeqIndexFactorLookup<IT>::
          sparseMatrix->getFactorList())[j];
        SetupSeqIndexFactorLookup<IT>::
          seqIndexFactorLookup[lookupIndex].factorList.push_back(factorElement);
      }
    }
  };
  
  // -------------------------------------------------
  void sum(char *a, char *b, ESMC_TypeKind_Flag tk){
    if (tk == ESMC_TYPEKIND_R4)
      *((ESMC_R4 *)a) += *((ESMC_R4 *)b);
    else if (tk == ESMC_TYPEKIND_R8)
      *((ESMC_R8 *)a) += *((ESMC_R8 *)b);
    else if (tk == ESMC_TYPEKIND_I4)
      *((ESMC_I4 *)a) += *((ESMC_I4 *)b);
    else if (tk == ESMC_TYPEKIND_I8)
      *((ESMC_I8 *)a) += *((ESMC_I8 *)b);
  }

  // -------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DD::setupSeqIndexFactorLookup()"
  template<typename IT> int setupSeqIndexFactorLookup(VM *vm,
    vector<SeqIndexFactorLookup<IT> > &seqIndexFactorLookup,
    const int petCount, const int localPet, const int factorListCount,
    const bool dstSetupFlag, vector<SparseMatrix<IT,IT> > const &sparseMatrix,
    const bool tensorMixFlag, DD::Interval<IT> const *seqIndexInterval,
    const int tensorElementCountEff, vector<bool> const &factorPetFlag,
    const ESMC_TypeKind_Flag typekindFactors, const bool haloFlag,
    const bool ignoreUnmatched, Array const *array, const int localDeCount, 
    const int *localDeElementCount, int const *localDeToDeMap,
    int const *localIntervalPerPetCount,
    int const *localElementsPerIntervalCount
    ){
    
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup1"));
#endif

    // set up seqIntervFactorListCountToPet and seqIntervFactorListIndexToPet
    vector<int> seqIntervFactorListCountToPet(petCount);
    for (int i=0; i<petCount; i++)
      seqIntervFactorListCountToPet[i] = 0; // reset
    vector<vector<int> > seqIntervFactorListIndexToPet(petCount);
    vector<vector<int> > seqIntervFactorListLookupIndexToPet(petCount);
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup2"));
#endif

    for (int j=0; j<factorListCount; j++){
      // loop over all factorList entries, find matching interval via bisection
      // and count factor towards those that need to be sent to that PET.
      SeqInd<IT> seqInd;
      if (dstSetupFlag)
        seqInd = sparseMatrix[0].getDstSeqIndex(j);
      else
        seqInd = sparseMatrix[0].getSrcSeqIndex(j);
#if 0
seqInd.print();
#endif
      IT seqIndex = seqInd.getIndex(0);
      IT tensorSeqIndex;
      if (tensorMixFlag)
        tensorSeqIndex = seqInd.getIndex(1);
      else
        tensorSeqIndex = 1;  // dummy
      int iMin=0, iMax=petCount-1;
      int i=petCount/2;
      bool foundFlag=false;     // reset
      do{
#if 0
cout << "dstSetupFlag=" << dstSetupFlag << " seqIndex=" << seqIndex << " i=" <<
  i << " iMin=" << iMin << " iMax=" << iMax << " seqIndexInterval[].min=" << 
  seqIndexInterval[i].min << " seqIndexInterval[].max=" << 
  seqIndexInterval[i].max << "\n";
cout << "dstSetupFlag=" << dstSetupFlag << " tensorSeqIndex=" <<
  tensorSeqIndex << " tensorElementCountEff=" << tensorElementCountEff << "\n";
#endif
        if (seqIndex < seqIndexInterval[i].min){
          iMax = i;
          i = iMin + (iMax - iMin) / 2;
          continue; 
        }
        if (seqIndex > seqIndexInterval[i].max){
          iMin = i;
          i = iMin + 1 + (iMax - iMin) / 2;
          continue; 
        }
        // found interval -> check if tensorSeqIndex is within bounds
        if (tensorSeqIndex < 1 || tensorSeqIndex > tensorElementCountEff){
          // tensorSeqIndex outside Array bounds
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "factorIndexList contains tensorSeqIndex outside Array bounds",
            ESMC_CONTEXT, &localrc);
          return localrc;
        }
        foundFlag = true;  // set
        ++seqIntervFactorListCountToPet[i]; // count this factor for this Pet
        seqIntervFactorListIndexToPet[i].push_back(j); // store factorList ind.
        // determine lookupIndex and store
        int lookupIndex = (int)(seqIndex - seqIndexInterval[i].min);
        if (tensorMixFlag)
          lookupIndex += (int)((tensorSeqIndex - 1) * seqIndexInterval[i].count);
        seqIntervFactorListLookupIndexToPet[i].push_back(lookupIndex); // store
        break;
      }while (iMin != iMax);
      if (!ignoreUnmatched && !foundFlag){
        // seqIndex lies outside Array bounds
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "factorIndexList contains seqIndex outside Array bounds",
          ESMC_CONTEXT, &localrc);
        return localrc;
      }
    }

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup3"));
#endif

    // construct seqIntervFactorListCountFromPet
    vector<int> seqIntervFactorListCountFromPet(petCount);
    vm->alltoall(&(seqIntervFactorListCountToPet.front()), sizeof(int),
      &(seqIntervFactorListCountFromPet.front()), sizeof(int), vmBYTE);
    
#ifdef DEBUGLOG
    {
      std::stringstream debugmsg;
      debugmsg << "setupSeqIndexFactorLookup() seqIntervFactorListCountToPet=";
      for (int i=0; i<seqIntervFactorListCountToPet.size(); i++)
        debugmsg << seqIntervFactorListCountToPet[i] << ", ";
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
    }
    {
      std::stringstream debugmsg;
      debugmsg << "setupSeqIndexFactorLookup() seqIntervFactorListCountFromPet=";
      for (int i=0; i<seqIntervFactorListCountFromPet.size(); i++)
        debugmsg << seqIntervFactorListCountFromPet[i] << ", ";
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup4"));
#endif

    DD::SetupSeqIndexFactorLookupStage1<IT> setupSeqIndexFactorLookupStage1(
      seqIndexFactorLookup,
      localPet,
      petCount,
      (sparseMatrix.size()==0) ? NULL : &(sparseMatrix[0]),
      factorPetFlag,
      seqIndexInterval,
      seqIntervFactorListCountToPet,
      seqIntervFactorListIndexToPet,
      seqIntervFactorListLookupIndexToPet,
      seqIntervFactorListCountFromPet,
      tensorMixFlag,
      dstSetupFlag,
      typekindFactors);
      
#define WITH_RESERVE
#ifdef WITH_RESERVE
    // Determine factorCount in stage1 and use to reserve memory of vector
    // before using it in stage2. Not sure this is really an improvement over
    // just dealing with memory allocation hit, b/c it does require extra
    // communication.
    
    setupSeqIndexFactorLookupStage1.totalExchange(vm);
    
    for (typename vector<SeqIndexFactorLookup<IT> >::iterator
      i=seqIndexFactorLookup.begin(); i!=seqIndexFactorLookup.end(); ++i){
      // obtain memory
      i->factorList.reserve(i->factorCount);  
    }
#endif

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup5"));
#endif
    
    DD::SetupSeqIndexFactorLookupStage2<IT>
      setupSeqIndexFactorLookupStage2(setupSeqIndexFactorLookupStage1);

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup6"));
#endif

    setupSeqIndexFactorLookupStage2.totalExchange(vm);
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup7"));
#endif

    // deal with duplicate sparse matrix entries in seqIndexFactorLookup
    for (typename vector<SeqIndexFactorLookup<IT> >::iterator
      i=seqIndexFactorLookup.begin(); i!=seqIndexFactorLookup.end(); ++i){
#ifdef ASMM_STORE_LOG_on_disabled
      fprintf(asmm_store_log_fp,
        "befr duplicate elimination seqIndexFactorLookup[%d].factorCount=%d\n",
        i-seqIndexFactorLookup.begin(), i->factorCount);
#endif
      sort(i->factorList.begin(), i->factorList.end());
      if (!haloFlag){
        // not halo: sum duplicate sparse matrix entries into the first occur.
        for (int j=0; j<i->factorCount; j++){
          int k;
          for (k=j+1; k<i->factorCount; k++){
            if (i->factorList[j] == i->factorList[k]){
              sum(i->factorList[j].factor,
                i->factorList[k].factor, typekindFactors);
            }else
              break;
          }
          j=k-1;  // skip over the duplicates
        }
      }
      // eliminate duplicates in factorList, only leaving first occurrences
      i->factorList.erase(
        unique(i->factorList.begin(),
        i->factorList.end()),
        i->factorList.end());
      i->factorCount = i->factorList.size();
#ifdef ASMM_STORE_LOG_on_disabled
      fprintf(asmm_store_log_fp,
        "aftr duplicate elimination seqIndexFactorLookup[%d].factorCount=%d\n",
        i-seqIndexFactorLookup.begin(), i->factorCount);
#endif
    }
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup8"));
#endif

    // communicate between Pets to set up "de" member in seqIndexFactorLookup[]
    {
      DD::FillSelfDeInfo<IT> fillSelfDeInfo(
        array,
        localPet,
        localDeCount,
        localDeElementCount,
        localDeToDeMap,
        seqIndexInterval,
        seqIndexFactorLookup,
        tensorMixFlag,
        localIntervalPerPetCount,
        localElementsPerIntervalCount,
        (dstSetupFlag & haloFlag));
      
      fillSelfDeInfo.totalExchange(vm);
    }
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup9"));
#endif

    // eliminate duplicate de entries in seqIndexFactorLookup
    for (typename vector<SeqIndexFactorLookup<IT> >::iterator
      i=seqIndexFactorLookup.begin(); i!=seqIndexFactorLookup.end(); ++i){
      sort(i->de.begin(), i->de.end());
      i->de.erase(unique(i->de.begin(),i->de.end()),i->de.end());
    }
  
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("setupSeqIndexFactorLookup10"));
#endif

    // return successfully
    return ESMF_SUCCESS;
  }
  
} // namespace DD
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::sparseMatMulStoreLinSeqVect()"
//BOPI
// !IROUTINE:  ESMCI::sparseMatMulStoreLinSeqVect
//
// !INTERFACE:
template<typename SIT, typename DIT> int sparseMatMulStoreLinSeqVect(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VM *vm,                                 // in
  Array *srcArray, Array *dstArray,       // in
  vector<SparseMatrix<SIT,DIT> >const &sparseMatrix,// in - sparse matrix vector
  bool haloFlag,                          // in
  bool ignoreUnmatched,                   // in
  bool tensorMixFlag,                     // in
  int const factorListCount,              // in
  vector<bool> &factorPetFlag,            // in
  ESMC_TypeKind_Flag typekindFactors,     // in
  int const srcLocalDeCount,              // in
  int const dstLocalDeCount,              // in
  int srcElementCount,                    // in
  int dstElementCount,                    // in
  const int *srcLocalDeElementCount,      // in
  const int *dstLocalDeElementCount,      // in
  vector<vector<AssociationElement<SeqIndex<SIT>,SeqIndex<DIT> > > >&srcLinSeqVect, // inout
  vector<vector<AssociationElement<SeqIndex<DIT>,SeqIndex<SIT> > > >&dstLinSeqVect  // inout
  ){
//
// !DESCRIPTION:
//    Take the incoming sparse matrix information from "input distribution" and
//    transform it into "run distribution":
//
//      -> srcLinSeqVect
//      -> dstLinSeqVect
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect1.0"));
#endif
  
  // prepare
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  const int *srcLocalDeToDeMap = srcArray->getDELayout()->getLocalDeToDeMap();
  const int *dstLocalDeToDeMap = dstArray->getDELayout()->getLocalDeToDeMap();

  //---------------------------------------------------------------------------
  // Phase II
  //---------------------------------------------------------------------------

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.0"));
#endif

  // communicate srcElementCount across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *srcElementCountList = new int[petCount];
  vm->allgather(&srcElementCount, srcElementCountList, sizeof(int));

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.1"));
#endif

  // communicate dstElementCount across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *dstElementCountList = new int[petCount];
  vm->allgather(&dstElementCount, dstElementCountList, sizeof(int));
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.2"));
#endif

    // set the effective tensorElementCount for src and dst Arrays
  int srcTensorElementCountEff = srcArray->getTensorElementCount();  // default
  int dstTensorElementCountEff = dstArray->getTensorElementCount();  // default
  if (!tensorMixFlag){
    // if there is not tensor mixing then default into tensor for tensor mode
    srcTensorElementCountEff = 1;
    dstTensorElementCountEff = 1;
  }    

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.3"));
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t2);   //gjt - profile
#endif
  
  // find local srcSeqIndex Min/Max
  SIT srcSeqIndexMinMax[2]; // [0]=min, [1]=max
  srcSeqIndexMinMax[0] = srcSeqIndexMinMax[1] = -1; // visibly invalidate
  bool firstMinMax = true;
  for (int i=0; i<srcLocalDeCount; i++){
    if (srcLocalDeElementCount[i]){
      // there are elements for local DE i
      ArrayElement arrayElement(srcArray, i, true, false, false);
      // loop over all elements in exclusive region for local DE i
      while(arrayElement.isWithin()){
        // determine the sequentialized index for the current Array element
        SeqIndex<SIT> seqIndex = arrayElement.getSequenceIndex<SIT>();
        // record seqIndex min and max
        if (firstMinMax){
          srcSeqIndexMinMax[0] = srcSeqIndexMinMax[1]
            = seqIndex.decompSeqIndex; // initialize
          firstMinMax = false;
        }else{
          if (seqIndex.decompSeqIndex < srcSeqIndexMinMax[0])
            srcSeqIndexMinMax[0] = seqIndex.decompSeqIndex;
          if (seqIndex.decompSeqIndex > srcSeqIndexMinMax[1])
            srcSeqIndexMinMax[1] = seqIndex.decompSeqIndex;
        }
        arrayElement.next();
      } // end while over all exclusive elements
    }
  }
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.4"));
#endif

  // communicate srcSeqIndexMinMax across all Pets
  // todo: use nb-allgather and wait right before needed below
  SIT *srcSeqIndexMinMaxList = new SIT[2*petCount];
  vm->allgather(srcSeqIndexMinMax, srcSeqIndexMinMaxList, 2*sizeof(SIT));  
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.5"));
#endif

#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG: tensorMixFlag=" << tensorMixFlag
    << " srcTensorElementCountEff=" << srcTensorElementCountEff
    << " dstTensorElementCountEff=" << dstTensorElementCountEff;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  }
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG: srcSeqIndexMinMax[(0/1)] = " <<
      srcSeqIndexMinMax[0] <<"/"<< srcSeqIndexMinMax[1];
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  }
#endif

  // find local dstSeqIndex Min/Max
  DIT dstSeqIndexMinMax[2]; // [0]=min, [1]=max
  dstSeqIndexMinMax[0] = dstSeqIndexMinMax[1] = -1; // visibly invalidate
  firstMinMax = true;
  for (int i=0; i<dstLocalDeCount; i++){
    if (dstLocalDeElementCount[i]){
      // there are elements for local DE i
      if (haloFlag){
        // for halo the dst elements are in the rim of dstArray
        const std::vector<std::vector<SeqIndex<DIT> > > *rimSeqIndex;
        dstArray->getRimSeqIndex(&rimSeqIndex);
        for (int k=0; k<dstArray->getRimElementCount()[i]; k++){
          SeqIndex<DIT> seqIndex = (*rimSeqIndex)[i][k];
          if (seqIndex.valid()){
            // this rim element holds a valid seqIndex
            // record seqIndex min and max
            if (firstMinMax){
              dstSeqIndexMinMax[0] = dstSeqIndexMinMax[1]
                = seqIndex.decompSeqIndex; // initialize
              firstMinMax = false;
            }else{
              if (seqIndex.decompSeqIndex < dstSeqIndexMinMax[0])
                dstSeqIndexMinMax[0] = seqIndex.decompSeqIndex;
              if (seqIndex.decompSeqIndex > dstSeqIndexMinMax[1])
                dstSeqIndexMinMax[1] = seqIndex.decompSeqIndex;
            }
          }
        }
      }else{
        ArrayElement arrayElement(dstArray, i, true, false, false);
        // loop over all elements in exclusive region for local DE i
        while(arrayElement.isWithin()){
          // determine the sequentialized index for the current Array element
          SeqIndex<DIT> seqIndex = arrayElement.getSequenceIndex<DIT>();
          // record seqIndex min and max
          if (firstMinMax){
            dstSeqIndexMinMax[0] = dstSeqIndexMinMax[1]
              = seqIndex.decompSeqIndex; // initialize
            firstMinMax = false;
          }else{
            if (seqIndex.decompSeqIndex < dstSeqIndexMinMax[0])
              dstSeqIndexMinMax[0] = seqIndex.decompSeqIndex;
            if (seqIndex.decompSeqIndex > dstSeqIndexMinMax[1])
              dstSeqIndexMinMax[1] = seqIndex.decompSeqIndex;
          }
          arrayElement.next();
        } // end while over all exclusive elements
      }
    }
  }

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.6"));
#endif

  // communicate dstSeqIndexMinMax across all Pets
  // todo: use nb-allgather and wait right before needed below
  DIT *dstSeqIndexMinMaxList = new DIT[2*petCount];
  vm->allgather(dstSeqIndexMinMax, dstSeqIndexMinMaxList, 2*sizeof(DIT));
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.7"));
#endif

#ifdef ASMM_STORE_LOG_on
  for (int i=0; i<petCount; i++)
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG: dstSeqIndexMinMaxList[" << i << "(0/1)] = " <<
      dstSeqIndexMinMaxList[i*2] <<"/"<< dstSeqIndexMinMaxList[i*2+1];
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  }
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t3);   //gjt - profile
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.8"));
#endif

  // set up structure and intervals of src and dst distributed directories
  
  // determine the srcSeqIndexMinGlobal and MaxGlobal
  // todo: for nb-allgather(srcSeqIndexMinMaxList) here insert commwait()
  SIT srcSeqIndexMinGlobal, srcSeqIndexMaxGlobal;
  bool pastInitFlag = false; // reset
  for (int i=0; i<petCount; i++){
    if (srcElementCountList[i]){
      // Pet i holds elements in srcArray
      if (pastInitFlag){
        if (srcSeqIndexMinMaxList[2*i] < srcSeqIndexMinGlobal)
          srcSeqIndexMinGlobal = srcSeqIndexMinMaxList[2*i];
        if (srcSeqIndexMinMaxList[2*i+1] > srcSeqIndexMaxGlobal)
          srcSeqIndexMaxGlobal = srcSeqIndexMinMaxList[2*i+1];
      }else{
        // initialization
        srcSeqIndexMinGlobal = srcSeqIndexMinMaxList[2*i];
        srcSeqIndexMaxGlobal = srcSeqIndexMinMaxList[2*i+1];
        pastInitFlag = true; // set
      }
    }
  }
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.9"));
#endif
  
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4a1);   //gjt - profile
#endif
  
  // set up a distributed directory for srcArray seqIndex look-up
  DD::Interval<SIT> *srcSeqIndexInterval = new DD::Interval<SIT>[petCount];
  {
    srcSeqIndexInterval[0].min = srcSeqIndexMinGlobal;  // start
    SIT indicesPerPet = (srcSeqIndexMaxGlobal - srcSeqIndexMinGlobal + 1)
      / (SIT) petCount;
    SIT extraIndices = (srcSeqIndexMaxGlobal - srcSeqIndexMinGlobal + 1)
      % (SIT)petCount;
    for (int i=0; i<petCount-1; i++){
      srcSeqIndexInterval[i].max = srcSeqIndexInterval[i].min + indicesPerPet
        - 1;
      if (i<extraIndices)
        ++srcSeqIndexInterval[i].max;   // distribute extra indices evenly
      srcSeqIndexInterval[i].count = 
        srcSeqIndexInterval[i].max - srcSeqIndexInterval[i].min + 1;
      srcSeqIndexInterval[i+1].min = srcSeqIndexInterval[i].max + 1;
    }
    srcSeqIndexInterval[petCount-1].max = srcSeqIndexMaxGlobal;  // finish
    srcSeqIndexInterval[petCount-1].count = 
      srcSeqIndexInterval[petCount-1].max - srcSeqIndexInterval[petCount-1].min
      + 1;
  }
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.10"));
#endif

#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG: srcElementCountList[localPet]=" << srcElementCountList[localPet] <<
    " srcSeqIndexMinMax = " << srcSeqIndexMinMax[0] <<"/"<< srcSeqIndexMinMax[1] 
    << " srcSeqIndexMinGlobal/MaxGlobal = " << srcSeqIndexMinGlobal <<"/"<<
    srcSeqIndexMaxGlobal << " srcSeqIndexInterval[localPet].min/.max = " <<
    srcSeqIndexInterval[localPet].min <<"/"<< srcSeqIndexInterval[localPet].max;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  }
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4a2);   //gjt - profile
#endif
  
  // It is more efficient to first sort and then count in a sorted list than
  // it is to do the counting from an unsorted list.
  int *srcLocalElementsPerIntervalCount = new int[petCount];
  {
    // prepare temporary seqIndexList for sorting
    //TODO: this scales in memory as the srcArray size, does not consider sparse
    vector<SIT> seqIndexList(srcElementCount);
    int jj=0;
    for (int j=0; j<srcLocalDeCount; j++){
      // loop over all elements in the exclusive region for localDe j
      ArrayElement arrayElement(srcArray, j, true, false, false);
      while(arrayElement.isWithin()){
        SeqIndex<SIT> seqIndex = arrayElement.getSequenceIndex<SIT>();
        seqIndexList[jj] = seqIndex.decompSeqIndex;
        ++jj;
        arrayElement.next();
      } // end while over all exclusive elements
    }
    sort(seqIndexList.begin(), seqIndexList.end());
    jj=0;
    for (int i=0; i<petCount; i++){
      SIT seqIndexMax = srcSeqIndexInterval[i].max;
      int count = 0; // reset
      while (jj<srcElementCount && seqIndexList[jj]<=seqIndexMax){
        ++count;  // increment counter
        ++jj;
      }
      srcLocalElementsPerIntervalCount[i] = count;
    }
  }
  
  int *srcLocalIntervalPerPetCount = new int[petCount];
  
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4a3);   //gjt - profile
#endif
  
  vm->alltoall(srcLocalElementsPerIntervalCount, sizeof(int),
    srcLocalIntervalPerPetCount, sizeof(int), vmBYTE);
  
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4a);   //gjt - profile
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.11"));
#endif
  
  // determine the dstSeqIndexMinGlobal and MaxGlobal
  // todo: for nb-allgather(dstSeqIndexMinMaxList) here insert commwait()
  DIT dstSeqIndexMinGlobal, dstSeqIndexMaxGlobal;
  pastInitFlag = false; // reset
  for (int i=0; i<petCount; i++){
    if (dstElementCountList[i]){
      // Pet i holds elements in dstArray
      if (pastInitFlag){
        if (dstSeqIndexMinMaxList[2*i] < dstSeqIndexMinGlobal)
          dstSeqIndexMinGlobal = dstSeqIndexMinMaxList[2*i];
        if (dstSeqIndexMinMaxList[2*i+1] > dstSeqIndexMaxGlobal)
          dstSeqIndexMaxGlobal = dstSeqIndexMinMaxList[2*i+1];
      }else{
        // initialization
        dstSeqIndexMinGlobal = dstSeqIndexMinMaxList[2*i];
        dstSeqIndexMaxGlobal = dstSeqIndexMinMaxList[2*i+1];
        pastInitFlag = true; // set
      }
    }
  }
    
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.12"));
#endif
    
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4b1);   //gjt - profile
#endif
  
////gjt  
//printf("srcMin/Max = %d/%d, dstMinMax = %d/%d\n", srcSeqIndexMinGlobal,
//  srcSeqIndexMaxGlobal, dstSeqIndexMinGlobal, dstSeqIndexMaxGlobal);

  // set up a distributed directory for dstArray seqIndex look-up
  DD::Interval<DIT> *dstSeqIndexInterval = new DD::Interval<DIT>[petCount];
  {
    dstSeqIndexInterval[0].min = dstSeqIndexMinGlobal;  // start
    DIT indicesPerPet = (dstSeqIndexMaxGlobal - dstSeqIndexMinGlobal + 1)
      / (DIT)petCount;
    DIT extraIndices = (dstSeqIndexMaxGlobal - dstSeqIndexMinGlobal + 1)
      % (DIT)petCount;
    for (int i=0; i<petCount-1; i++){
      dstSeqIndexInterval[i].max = dstSeqIndexInterval[i].min + indicesPerPet
        - 1;
      if (i<extraIndices)
        ++dstSeqIndexInterval[i].max;   // distribute extra indices evenly
      dstSeqIndexInterval[i].count = 
        dstSeqIndexInterval[i].max - dstSeqIndexInterval[i].min + 1;
      dstSeqIndexInterval[i+1].min = dstSeqIndexInterval[i].max + 1;
    }
    dstSeqIndexInterval[petCount-1].max = dstSeqIndexMaxGlobal;  // finish
    dstSeqIndexInterval[petCount-1].count = 
      dstSeqIndexInterval[petCount-1].max - dstSeqIndexInterval[petCount-1].min
      + 1;
  }
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.13"));
#endif

#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG: dstElementCountList[localPet]=" << dstElementCountList[localPet] <<
    " dstSeqIndexMinMax = " << dstSeqIndexMinMax[0] <<"/"<< dstSeqIndexMinMax[1] 
    << " dstSeqIndexMinGlobal/MaxGlobal = " << dstSeqIndexMinGlobal <<"/"<<
    dstSeqIndexMaxGlobal << " dstSeqIndexInterval[localPet].min/.max = " <<
    dstSeqIndexInterval[localPet].min <<"/"<< dstSeqIndexInterval[localPet].max;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  }
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4b2);   //gjt - profile
#endif

  // It is more efficient to first sort and then count in a sorted list than
  // it is to do the counting from an unsorted list.
  int *dstLocalElementsPerIntervalCount = new int[petCount];
  {
    // prepare temporary seqIndexList for sorting
    //TODO: this scales in memory as the dstArray size, does not consider sparse
    vector<DIT> seqIndexList(dstElementCount);
    int jj=0;
    for (int j=0; j<dstLocalDeCount; j++){
      if (haloFlag){
        // loop over the halo rim elements for localDe j
        const std::vector<std::vector<SeqIndex<DIT> > > *rimSeqIndex;
        dstArray->getRimSeqIndex(&rimSeqIndex);
        for (int k=0; k<dstArray->getRimElementCount()[j]; k++){
          SeqIndex<DIT> seqIndex = (*rimSeqIndex)[j][k];
          if (seqIndex.valid()){
            seqIndexList[jj] = seqIndex.decompSeqIndex;
            ++jj;
          }
        }
      }else{
        // loop over all elements in the exclusive region for localDe j
        ArrayElement arrayElement(dstArray, j, true, false, false);
        while(arrayElement.isWithin()){
          SeqIndex<DIT> seqIndex = arrayElement.getSequenceIndex<DIT>();
          seqIndexList[jj] = seqIndex.decompSeqIndex;
          ++jj;
          arrayElement.next();
        } // end while over all exclusive elements
      }
    }
    sort(seqIndexList.begin(), seqIndexList.end());
    jj=0;
    for (int i=0; i<petCount; i++){
      DIT seqIndexMax = dstSeqIndexInterval[i].max;
      int count = 0; // reset
      while (jj<dstElementCount && seqIndexList[jj]<=seqIndexMax){
        ++count;  // increment counter
        ++jj;
      }
      dstLocalElementsPerIntervalCount[i] = count;
    }
  }
  
  int *dstLocalIntervalPerPetCount = new int[petCount];
  
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4b3);   //gjt - profile
#endif

  vm->alltoall(dstLocalElementsPerIntervalCount, sizeof(int),
    dstLocalIntervalPerPetCount, sizeof(int), vmBYTE);
  
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4b);   //gjt - profile
#endif
  
  // garbage collection  
  delete [] srcElementCountList;
  delete [] dstElementCountList;
  delete [] srcSeqIndexMinMaxList;
  delete [] dstSeqIndexMinMaxList;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.14"));
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t4);   //gjt - profile
#endif
  
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "srcSeqIndexInterval[localPet].count=" <<
      srcSeqIndexInterval[localPet].count <<
      " srcTensorElementCountEff=" << srcTensorElementCountEff;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  }
#endif
   
  // allocate local look-up map indexed by srcSeqIndex, i.e. distributed dir.
  vector<DD::SeqIndexFactorLookup<SIT> >
    srcSeqIndexFactorLookup(srcSeqIndexInterval[localPet].count
    * srcTensorElementCountEff);
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.14.1"));
#endif
  
  localrc = DD::setupSeqIndexFactorLookup<SIT>(vm, 
    srcSeqIndexFactorLookup,
    petCount, localPet, factorListCount, 
    false,  // dstSetupFlag
    sparseMatrix, tensorMixFlag,
    srcSeqIndexInterval, srcTensorElementCountEff, factorPetFlag,
    typekindFactors, haloFlag, ignoreUnmatched, srcArray, srcLocalDeCount,
    srcLocalDeElementCount, srcLocalDeToDeMap, srcLocalIntervalPerPetCount,
    srcLocalElementsPerIntervalCount);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
//vm->barrier();  /// only for profiling tests
      
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t5c);   //gjt - profile
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.15"));
#endif

#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "dstSeqIndexInterval[localPet].count=" <<
      dstSeqIndexInterval[localPet].count <<
      " dstTensorElementCountEff=" << dstTensorElementCountEff;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  }
#endif
   
  // allocate local look-up map indexed by dstSeqIndex, i.e. distributed dir.
  vector<DD::SeqIndexFactorLookup<DIT> >
    dstSeqIndexFactorLookup(dstSeqIndexInterval[localPet].count
    * dstTensorElementCountEff);
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.15.1"));
#endif
  
  localrc = DD::setupSeqIndexFactorLookup<DIT>(vm, 
    dstSeqIndexFactorLookup,
    petCount, localPet, factorListCount, 
    true,  // dstSetupFlag
    sparseMatrix, tensorMixFlag,
    dstSeqIndexInterval, dstTensorElementCountEff, factorPetFlag,
    typekindFactors, haloFlag, ignoreUnmatched, dstArray, dstLocalDeCount,
    dstLocalDeElementCount, dstLocalDeToDeMap, dstLocalIntervalPerPetCount,
    dstLocalElementsPerIntervalCount);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
//vm->barrier();  /// only for profiling tests
      
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t5f);   //gjt - profile
#endif
  
#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t5);   //gjt - profile
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.16"));
#endif

  // prepare count arrays for src partner look-up in dstSeqIndexFactorLookup
  int *srcLocalPartnerElementsPerIntervalCount = new int[petCount];
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active srcSeqIndex interval
    DIT seqIndexMin = dstSeqIndexInterval[i].min;
    DIT seqIndexMax = dstSeqIndexInterval[i].max;
    int count = 0; // reset
    for (typename vector<DD::SeqIndexFactorLookup<SIT> >
      ::const_iterator j=srcSeqIndexFactorLookup.begin(); 
      j!=srcSeqIndexFactorLookup.end(); ++j){
      for (int k=0; k<j->factorCount; k++){
        SIT partnerSeqIndex = j->factorList[k]
          .partnerSeqIndex.decompSeqIndex;
        if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax)
          ++count; // increment counter
      }
    }
    srcLocalPartnerElementsPerIntervalCount[i] = count;
  }
  int *dstLocalPartnerIntervalPerPetCount = new int[petCount];
  vm->alltoall(srcLocalPartnerElementsPerIntervalCount, sizeof(int),
    dstLocalPartnerIntervalPerPetCount, sizeof(int), vmBYTE);
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.17"));
#endif

  // fill partnerDE in srcSeqIndexFactorLookup using dstSeqIndexFactorLookup
  {
    DD::FillPartnerDeInfo<DIT,SIT> *fillPartnerDeInfo =
      new DD::FillPartnerDeInfo<DIT,SIT>
        (dstSeqIndexFactorLookup, srcSeqIndexFactorLookup);
      
    fillPartnerDeInfo->localPet = localPet;
    fillPartnerDeInfo->seqIndexIntervalIn = dstSeqIndexInterval;
    fillPartnerDeInfo->seqIndexIntervalOut = srcSeqIndexInterval;
    fillPartnerDeInfo->tensorMixFlag = tensorMixFlag;
      
    DD::accessLookup(vm, petCount, localPet, dstLocalPartnerIntervalPerPetCount,
      srcLocalPartnerElementsPerIntervalCount, fillPartnerDeInfo);
    delete fillPartnerDeInfo;
  }

  // garbage collection
  delete [] srcLocalPartnerElementsPerIntervalCount;
  delete [] dstLocalPartnerIntervalPerPetCount;
      
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.18"));
#endif

  // prepare count arrays for dst partner look-up in srcSeqIndexFactorLookup
  int *dstLocalPartnerElementsPerIntervalCount = new int[petCount];
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active dstSeqIndex interval
    SIT seqIndexMin = srcSeqIndexInterval[i].min;
    SIT seqIndexMax = srcSeqIndexInterval[i].max;
    int count = 0; // reset
    for (typename vector<DD::SeqIndexFactorLookup<DIT> >
      ::const_iterator j=dstSeqIndexFactorLookup.begin(); 
      j!=dstSeqIndexFactorLookup.end(); ++j){
      for (int k=0; k<j->factorCount; k++){
        DIT partnerSeqIndex = j->factorList[k]
          .partnerSeqIndex.decompSeqIndex;
        if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax)
          ++count; // increment counter
      }
    }
    dstLocalPartnerElementsPerIntervalCount[i] = count;
  }
  int *srcLocalPartnerIntervalPerPetCount = new int[petCount];
  vm->alltoall(dstLocalPartnerElementsPerIntervalCount, sizeof(int),
    srcLocalPartnerIntervalPerPetCount, sizeof(int), vmBYTE);
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.19"));
#endif

  // fill partnerDE in dstSeqIndexFactorLookup using srcSeqIndexFactorLookup
  {
    DD::FillPartnerDeInfo<SIT,DIT> *fillPartnerDeInfo =
      new DD::FillPartnerDeInfo<SIT,DIT>
        (srcSeqIndexFactorLookup, dstSeqIndexFactorLookup);
    fillPartnerDeInfo->localPet = localPet;
    fillPartnerDeInfo->seqIndexIntervalIn = srcSeqIndexInterval;
    fillPartnerDeInfo->seqIndexIntervalOut = dstSeqIndexInterval;
    fillPartnerDeInfo->tensorMixFlag = tensorMixFlag;
      
    DD::accessLookup(vm, petCount, localPet, srcLocalPartnerIntervalPerPetCount,
      dstLocalPartnerElementsPerIntervalCount, fillPartnerDeInfo);
    delete fillPartnerDeInfo;
  }

  // garbage collection
  delete [] dstLocalPartnerElementsPerIntervalCount;
  delete [] srcLocalPartnerIntervalPerPetCount;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.20"));
#endif

#ifdef ASMM_STORE_LOG_on_disabled
  fprintf(asmm_store_log_fp, "\n========================================"
    "========================================\n");
  fprintf(asmm_store_log_fp, "========================================"
    "========================================\n\n");
  // some serious printing for src info
  for (typename vector<DD::SeqIndexFactorLookup<SIT> >::const_iterator
    i=srcSeqIndexFactorLookup.begin(); i!=srcSeqIndexFactorLookup.end(); ++i){
    fprintf(asmm_store_log_fp, "gjt srcDistDir: localPet %d, srcSeqIndex = %d, "
      "srcSeqIndexFactorLookup[%d].factorCount = %d\n -> .de: ",
      localPet, (i-srcSeqIndexFactorLookup.begin())
      +srcSeqIndexInterval[localPet].min, i-srcSeqIndexFactorLookup.begin(),
      i->factorCount);
    for (int j=0; j<i->de.size(); j++)
      fprintf(asmm_store_log_fp, "%d, ", i->de[j]);
    fprintf(asmm_store_log_fp, "\n");
    for (int j=0; j<i->factorCount; j++){
      fprintf(asmm_store_log_fp, "\tfactorList[%d]\n"
        "\t\t.partnerSeqIndex.decompSeqIndex = %d\n"
        "\t\t.partnerDE = ", j,
        i->factorList[j].partnerSeqIndex
        .decompSeqIndex);
      for (int jj=0;
        jj<i->factorList[j].partnerDE.size(); jj++)
        fprintf(asmm_store_log_fp, "%d, ",
          i->factorList[j].partnerDE[jj]);
      fprintf(asmm_store_log_fp, "\n");
      fprintf(asmm_store_log_fp, "\t\t.factor = %d\n",
        *((int *)i->factorList[j].factor));
    }
  }
#endif
  
#ifdef ASMM_STORE_LOG_on_disabled
  // some serious printing for dst info
  for (typename vector<DD::SeqIndexFactorLookup<DIT> >::const_iterator
    i=dstSeqIndexFactorLookup.begin(); i!=dstSeqIndexFactorLookup.end(); ++i){
    fprintf(asmm_store_log_fp, "gjt dstDistDir: localPet %d, dstSeqIndex = %d, "
      "dstSeqIndexFactorLookup[%d].factorCount = %d\n -> .de: ",
      localPet, (i-dstSeqIndexFactorLookup.begin())
      +dstSeqIndexInterval[localPet].min, i-dstSeqIndexFactorLookup.begin(),
      i->factorCount);
    for (int j=0; j<i->de.size(); j++)
      fprintf(asmm_store_log_fp, "%d, ", i->de[j]);
    fprintf(asmm_store_log_fp, "\n");
    for (int j=0; j<i->factorCount; j++){
      fprintf(asmm_store_log_fp, "\tfactorList[%d]\n"
        "\t\t.partnerSeqIndex.decompSeqIndex = %d\n"
        "\t\t.partnerDE = ", j,
        i->factorList[j].partnerSeqIndex
        .decompSeqIndex);
      for (int jj=0;
        jj<i->factorList[j].partnerDE.size(); jj++)
        fprintf(asmm_store_log_fp, "%d, ",
          i->factorList[j].partnerDE[jj]);
      fprintf(asmm_store_log_fp, "\n");
      fprintf(asmm_store_log_fp, "\t\t.factor = %d\n",
        *((int *)i->factorList[j].factor));
    }
  }
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t6);   //gjt - profile
#endif
  
  //---------------------------------------------------------------------------
  // Phase III -> represent sparse matrix in "run distribution"
  //---------------------------------------------------------------------------
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.0"));
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.1"));
#endif

  // access srcSeqIndexFactorLookup to construct srcLinSeqVect
  {
    DD::FillLinSeqVectInfo<SIT,DIT> *fillLinSeqVectInfo =
      new DD::FillLinSeqVectInfo<SIT,DIT>
        (srcLinSeqVect, srcSeqIndexFactorLookup);
    fillLinSeqVectInfo->array = srcArray;
    fillLinSeqVectInfo->localPet = localPet;
    fillLinSeqVectInfo->localDeCount = srcLocalDeCount;
    fillLinSeqVectInfo->localDeElementCount = srcLocalDeElementCount;
    fillLinSeqVectInfo->seqIndexInterval = srcSeqIndexInterval;
    fillLinSeqVectInfo->tensorMixFlag = tensorMixFlag;
    fillLinSeqVectInfo->haloRimFlag = false;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.2"));
#endif
    DD::accessLookup(vm, petCount, localPet, srcLocalIntervalPerPetCount,
      srcLocalElementsPerIntervalCount, fillLinSeqVectInfo);
    delete fillLinSeqVectInfo;
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.3"));
#endif
  }

  // access dstSeqIndexFactorLookup to construct dstLinSeqVect
  {
    DD::FillLinSeqVectInfo<DIT,SIT> *fillLinSeqVectInfo =
      new DD::FillLinSeqVectInfo<DIT,SIT>
        (dstLinSeqVect, dstSeqIndexFactorLookup);
    fillLinSeqVectInfo->array = dstArray;
    fillLinSeqVectInfo->linSeqVect = dstLinSeqVect;
    fillLinSeqVectInfo->localPet = localPet;
    fillLinSeqVectInfo->localDeCount = dstLocalDeCount;
    fillLinSeqVectInfo->localDeElementCount = dstLocalDeElementCount;
    fillLinSeqVectInfo->seqIndexInterval = dstSeqIndexInterval;
    fillLinSeqVectInfo->tensorMixFlag = tensorMixFlag;
    fillLinSeqVectInfo->haloRimFlag = haloFlag; // forward HALO

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.4"));
#endif
    DD::accessLookup(vm, petCount, localPet, dstLocalIntervalPerPetCount,
      dstLocalElementsPerIntervalCount, fillLinSeqVectInfo);
    delete fillLinSeqVectInfo;
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.5"));
#endif
  }

  // garbage colletion
  delete [] srcLocalElementsPerIntervalCount;
  delete [] srcLocalIntervalPerPetCount;
  delete [] srcSeqIndexInterval;
  delete [] dstLocalElementsPerIntervalCount;
  delete [] dstLocalIntervalPerPetCount;
  delete [] dstSeqIndexInterval;
  // force vectors out of scope by swapping with empty vector, to free memory
  vector<DD::SeqIndexFactorLookup<SIT> >().swap(srcSeqIndexFactorLookup);
  vector<DD::SeqIndexFactorLookup<DIT> >().swap(dstSeqIndexFactorLookup);
  
#ifdef ASMM_STORE_LOG_on_disabled
  fprintf(asmm_store_log_fp, "\n========================================"
    "========================================\n");
  fprintf(asmm_store_log_fp, "========================================"
    "========================================\n\n");
  // more serious printing
  for (int j=0; j<srcLocalDeCount; j++){
    for (int k=0; k<srcLinSeqVect[j].size(); k++){
      fprintf(asmm_store_log_fp, "localPet: %d, "
        "srcLinSeqVect[%d][%d].linIndex = %d, "
        ".seqIndex = %d/%d, .factorCount = %d\n",
        localPet, j, k, srcLinSeqVect[j][k].linIndex,
        srcLinSeqVect[j][k].seqIndex.decompSeqIndex,
        srcLinSeqVect[j][k].seqIndex.tensorSeqIndex,
        srcLinSeqVect[j][k].factorCount);
      for (int kk=0; kk<srcLinSeqVect[j][k].factorCount; kk++){
        fprintf(asmm_store_log_fp, "\tfactorList[%d]\n"
          "\t\t.partnerSeqIndex = %d/%d\n"
          "\t\t.partnerDE = ", kk,
          srcLinSeqVect[j][k].factorList[kk].partnerSeqIndex.decompSeqIndex,
          srcLinSeqVect[j][k].factorList[kk].partnerSeqIndex.tensorSeqIndex);
        for (int jj=0;
          jj<srcLinSeqVect[j][k].factorList[kk].partnerDE.size(); jj++)
          fprintf(asmm_store_log_fp, "%d, ",
            srcLinSeqVect[j][k].factorList[kk].partnerDE[jj]);
        fprintf(asmm_store_log_fp, "\n");
        fprintf(asmm_store_log_fp, "\t\t.factor = %d\n",
          *((int *)srcLinSeqVect[j][k].factorList[kk].factor));
      }
    }
  }
    
  // more serious printing
  for (int j=0; j<dstLocalDeCount; j++){
    for (int k=0; k<dstLinSeqVect[j].size(); k++){
      fprintf(asmm_store_log_fp, "localPet: %d, "
        "dstLinSeqVect[%d][%d].linIndex = %d, "
        ".seqIndex = %d/%d, .factorCount = %d\n",
        localPet, j, k, dstLinSeqVect[j][k].linIndex,
        dstLinSeqVect[j][k].seqIndex.decompSeqIndex,
        dstLinSeqVect[j][k].seqIndex.tensorSeqIndex,
        dstLinSeqVect[j][k].factorCount);
      for (int kk=0; kk<dstLinSeqVect[j][k].factorCount; kk++){
        fprintf(asmm_store_log_fp, "\tfactorList[%d]\n"
          "\t\t.partnerSeqIndex = %d/%d\n"
          "\t\t.partnerDE = ", kk,
          dstLinSeqVect[j][k].factorList[kk].partnerSeqIndex.decompSeqIndex,
          dstLinSeqVect[j][k].factorList[kk].partnerSeqIndex.tensorSeqIndex);
        for (int jj=0;
          jj<dstLinSeqVect[j][k].factorList[kk].partnerDE.size(); jj++)
          fprintf(asmm_store_log_fp, "%d, ",
            dstLinSeqVect[j][k].factorList[kk].partnerDE[jj]);
        fprintf(asmm_store_log_fp, "\n");
        fprintf(asmm_store_log_fp, "\t\t.factor = %d\n",
          *((int *)dstLinSeqVect[j][k].factorList[kk].factor));
      }
    }
  }
#endif
  
  if (ignoreUnmatched){
    // If there are unmatched src or dst elements in the sparse matrix, they
    // will have lead to FactorElement entries with a partnerDE.size()==0 here.
    // Leaving them like that will lead to issues down the code, so that if
    // unmatched entries are to be supported, these FactorElements must be 
    // removed here.
    //
    //TODO: This could have been done on the SeqIndexFactorLookup level further
    //TODO: up ... probably a more efficient way.
    //
    // Doing this for the src side
    for (int j=0; j<srcLocalDeCount; j++){
      for (unsigned k=0; k<srcLinSeqVect[j].size(); k++){
        for (typename vector<FactorElement<SeqIndex<DIT> > >::iterator
          fe=srcLinSeqVect[j][k].factorList.begin();
          fe!=srcLinSeqVect[j][k].factorList.end();){
          if (fe->partnerDE.size() == 0){
            // eliminate this factorList entry
            fe = srcLinSeqVect[j][k].factorList.erase(fe);
          }else{
            ++fe;
          }
        }
      }
    }
    // Doing this for the dst side
    for (int j=0; j<dstLocalDeCount; j++){
      for (unsigned k=0; k<dstLinSeqVect[j].size(); k++){
        for (typename vector<FactorElement<SeqIndex<SIT> > >::iterator
          fe=dstLinSeqVect[j][k].factorList.begin();
          fe!=dstLinSeqVect[j][k].factorList.end();){
          if (fe->partnerDE.size() == 0){
            // eliminate this factorList entry
            fe = dstLinSeqVect[j][k].factorList.erase(fe);
          }else{
            ++fe;
          }
        }
      }
    }
  }

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.6"));
#endif

    if (haloFlag||ignoreUnmatched){
    // Phase IV below expects each FactorElement inside of srcLinSeqVect and
    // dstLinSeqVect to only reference a single partnerDE (- only partnerDE[0]
    // will be looked at!). Therefore transform FactorElements that have more
    // than one partnerDE entry into multiple FactorElements where each only
    // has a single partnerDE entry.
    // This condition can arise either with halos, or when allowing more
    // general redist conditions that ignore that src and dst are not perfectly
    // matched wrt seqIndices.
    // Doing this for the src side supports forward HALO:
    for (int j=0; j<srcLocalDeCount; j++){
      for (unsigned k=0; k<srcLinSeqVect[j].size(); k++){
        for (unsigned kk=0; kk<srcLinSeqVect[j][k].factorList.size(); kk++){
          if (srcLinSeqVect[j][k].factorList[kk].partnerDE.size() > 1){
            for (unsigned jj=1;
              jj<srcLinSeqVect[j][k].factorList[kk].partnerDE.size(); jj++){
              // construct factorElement with one partnerDE entry
              FactorElement<SeqIndex<DIT> > factorElement =
                srcLinSeqVect[j][k].factorList[kk];
              factorElement.partnerDE.resize(1);
              factorElement.partnerDE[0] =
                srcLinSeqVect[j][k].factorList[kk].partnerDE[jj];
              srcLinSeqVect[j][k].factorList.push_back(factorElement);
            }
            // erase all of the replicated partnerDE entries
            srcLinSeqVect[j][k].factorList[kk].partnerDE.resize(1);
          }
        }
      }
    }
    // Doing this for the dst side supports backward HALO:
    for (int j=0; j<dstLocalDeCount; j++){
      for (unsigned k=0; k<dstLinSeqVect[j].size(); k++){
        for (unsigned kk=0; kk<dstLinSeqVect[j][k].factorList.size(); kk++){
          if (dstLinSeqVect[j][k].factorList[kk].partnerDE.size() > 1){
            for (unsigned jj=1;
              jj<dstLinSeqVect[j][k].factorList[kk].partnerDE.size(); jj++){
              // construct factorElement with one partnerDE entry
              FactorElement<SeqIndex<SIT> > factorElement =
                dstLinSeqVect[j][k].factorList[kk];
              factorElement.partnerDE.resize(1);
              factorElement.partnerDE[0] =
                dstLinSeqVect[j][k].factorList[kk].partnerDE[jj];
              //TODO: It's certainly not correct to add up all the entries
              //TODO: from all of the halo elements in a backward HALO,
              //TODO: instead the average over all elements makes more sense.
              //TODO: However, this would require communication here in order
              //TODO: to set an adjusted factor for these elements also on the 
              //TODO: src side.
              dstLinSeqVect[j][k].factorList.push_back(factorElement);
            }
            // erase all of the replicated partnerDE entries
            dstLinSeqVect[j][k].factorList[kk].partnerDE.resize(1);
          }
        }
      }
    }
  }
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore3.7"));
#endif

#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    // srcLinSeqVect
    for (int j=0; j<srcLocalDeCount; j++){
      for (unsigned k=0; k<srcLinSeqVect[j].size(); k++){
        msg << "ASMM_STORE_LOG:" << __LINE__ << " localPet: " << localPet <<
          " srcLinSeqVect["<< j <<"]["<< k <<"].linIndex = "
          << srcLinSeqVect[j][k].linIndex <<", "
          ".seqIndex = "<< srcLinSeqVect[j][k].seqIndex.decompSeqIndex
          <<"/"<< srcLinSeqVect[j][k].seqIndex.tensorSeqIndex <<
          ", .factorList.size() = "<< srcLinSeqVect[j][k].factorList.size();
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
        msg.str("");  // clear
        for (unsigned kk=0; kk<srcLinSeqVect[j][k].factorList.size(); kk++){
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \tfactorList["<< kk <<"]";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerSeqIndex ="
            << srcLinSeqVect[j][k].factorList[kk].partnerSeqIndex.decompSeqIndex
            <<"/"
            << srcLinSeqVect[j][k].factorList[kk].partnerSeqIndex.tensorSeqIndex;
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerDE =";
          for (unsigned jj=0;
            jj<srcLinSeqVect[j][k].factorList[kk].partnerDE.size(); jj++)
            msg << srcLinSeqVect[j][k].factorList[kk].partnerDE[jj] <<", ";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
          switch (typekindFactors){
          case ESMC_TYPEKIND_R4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R4 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_R8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R8 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I4 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I8 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          default:
            break;
          }
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
        }
      }
    }
    // dstLinSeqVect
    for (int j=0; j<dstLocalDeCount; j++){
      for (unsigned k=0; k<dstLinSeqVect[j].size(); k++){
        msg << "ASMM_STORE_LOG:" << __LINE__ << " localPet: " << localPet <<
          " dstLinSeqVect["<< j <<"]["<< k <<"].linIndex = "
          << dstLinSeqVect[j][k].linIndex <<", "
          ".seqIndex = "<< dstLinSeqVect[j][k].seqIndex.decompSeqIndex
          <<"/"<< dstLinSeqVect[j][k].seqIndex.tensorSeqIndex <<
          ", .factorList.size() = "<< dstLinSeqVect[j][k].factorList.size();
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
        msg.str("");  // clear
        for (unsigned kk=0; kk<dstLinSeqVect[j][k].factorList.size(); kk++){
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \tfactorList["<< kk <<"]";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerSeqIndex ="
            << dstLinSeqVect[j][k].factorList[kk].partnerSeqIndex.decompSeqIndex
            <<"/"
            << dstLinSeqVect[j][k].factorList[kk].partnerSeqIndex.tensorSeqIndex;
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerDE =";
          for (unsigned jj=0;
            jj<dstLinSeqVect[j][k].factorList[kk].partnerDE.size(); jj++)
            msg << dstLinSeqVect[j][k].factorList[kk].partnerDE[jj] <<", ";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
          switch (typekindFactors){
          case ESMC_TYPEKIND_R4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R4 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_R8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R8 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I4 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I8 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          default:
            break;
          }
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
          msg.str("");  // clear
        }
      }
    }
  }
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t7);   //gjt - profile
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVectXX.0"));
#endif

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------
#undef DEBUGLOG
