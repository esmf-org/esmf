//-----------------------------------------------------------------------------

  template<typename IT> struct SparseMatrixIndex{
    SeqIndex<IT> seqIndex;
    int index;  // index into the SparseMatrix
  };
  template<typename IT> bool operator <
    (SparseMatrixIndex<IT> a, SparseMatrixIndex<IT> b){
    return (a.seqIndex < b.seqIndex);
  }
  
  template<typename IT> struct MsgRequElement{
    SeqIndex<IT> seqIndex;
  };
  template<typename IT> bool operator <
    (MsgRequElement<IT> a, MsgRequElement<IT> b){
    return (a.seqIndex < b.seqIndex);
  }

  template<typename SIT, typename DIT, typename T> struct MsgRespElement{
    SeqIndex<SIT> srcSeqIndex;
    SeqIndex<DIT> dstSeqIndex;
    T factor;
  };

  //---------------------------------------------------------------------------
  
  template<typename SIT, typename DIT, typename T>
    class QuerySparseMatrix:public ComPat2{
    SparseMatrix<SIT,DIT> const &sparseMatrix;
    vector<vector<AssociationElement<DIT,SIT> > >&dstLinSeqVect;
    // members that are initialized internally:
    vector<SparseMatrixIndex<DIT> > sparseMatrixDind;
    vector<MsgRequElement<DIT> > request;
   public:
    QuerySparseMatrix(
      SparseMatrix<SIT,DIT> const &sparseMatrix_,
      vector<vector<AssociationElement<DIT,SIT> > >&dstLinSeqVect_
    ):
      // members that need to be set on this level because of reference
      sparseMatrix(sparseMatrix_),
      dstLinSeqVect(dstLinSeqVect_)
    {
    }
    
   private:
       
    virtual void handleLocal(){
      // called on every localPet exactly once, before any other method
      // set up an index mapper into sparseMatrix, that can be sorted
      T *factorList = (T *)(sparseMatrix.getFactorList());
      // eliminate any sparse matrix elements with factor identical to zero
      int factorCount = sparseMatrix.getFactorListCount();
      sparseMatrixDind.reserve(factorCount);  // good guess for better perform
      for (int i=0; i<factorCount; i++){
        if (factorList[i] != (T)0){
          // found non-zero factor
          SparseMatrixIndex<DIT> element;
          element.index = i;  // keep track of index
          // cast SeqInd -> SeqIndex representation
          SeqInd<DIT> seqInd = sparseMatrix.getDstSeqIndex(i);
          element.seqIndex.decompSeqIndex = seqInd.getIndex(0);
          if (seqInd.getN()>1)
            element.seqIndex.tensorSeqIndex = seqInd.getIndex(1);
          else
            element.seqIndex.tensorSeqIndex = 1;
          sparseMatrixDind.push_back(element);
        }
      }
      // sort the sparse matrix elements by dst seqIndex
      sort(sparseMatrixDind.begin(), sparseMatrixDind.end());
      // work through dstLinSeqVect and find matches with sparseMatrixDind
      int size = 0;
      for (unsigned i=0; i<dstLinSeqVect.size(); i++){
        size += dstLinSeqVect[i].size();
        typename vector<AssociationElement<DIT,SIT> >::iterator itD
          = dstLinSeqVect[i].begin();
        typename vector<SparseMatrixIndex<DIT> >::iterator itSM
          = sparseMatrixDind.begin();
        while ((itD != dstLinSeqVect[i].end())
          && (itSM != sparseMatrixDind.end())){
          if (itD->seqIndex == itSM->seqIndex){
            // found a sparse matrix entry with matching dst seqIndex
            // add the factor to the factorList of the dstLinSeqVect element
            FactorElement<SIT> element;
            // cast SeqInd -> SeqIndex representation
            SeqInd<SIT> seqInd = sparseMatrix.getSrcSeqIndex(itSM->index);
            element.partnerSeqIndex.decompSeqIndex = seqInd.getIndex(0);
            if (seqInd.getN()>1)
              element.partnerSeqIndex.tensorSeqIndex = seqInd.getIndex(1);
            else
              element.partnerSeqIndex.tensorSeqIndex = 1;
            *(T *)(element.factor) = factorList[itSM->index];
            itD->factorList.push_back(element);
            // move to the next sparse matrix element
            ++itSM;
            if (itSM == sparseMatrixDind.end()) break;
          }
          // catch up itD with itSM
          while ((itD != dstLinSeqVect[i].end()) && 
            (itD->seqIndex < itSM->seqIndex)){
            ++itD;
          }
          if (itD == dstLinSeqVect[i].end()) break;
          // catch up itSM with itD
          while ((itSM != sparseMatrixDind.end()) &&
            (itSM->seqIndex < itD->seqIndex)){
            ++itSM;
          }
        }
      }
      // prepare request
      request.resize(size);
      if (size>0){
        int j=0;
        for (unsigned i=0; i<dstLinSeqVect.size(); i++){
          typename vector<AssociationElement<DIT,SIT> >::iterator itD;
          for (itD = dstLinSeqVect[i].begin(); itD != dstLinSeqVect[i].end();
            ++itD){
            request[j].seqIndex = itD->seqIndex;
            ++j;
          }
        }
        // sort all the request elements by dst seqIndex
        sort(request.begin(), request.end());
      }
    }
    
    virtual void generateRequest(int responsePet,
      char* &requestBuffer, int &requestSize){
      // called on every localPet for every responsePet != localPet
      // point the requestBuffer to the already precomputed request
      requestSize = request.size() * sizeof(MsgRequElement<DIT>);
      requestBuffer = NULL;
      if (requestSize) requestBuffer = (char *)&(request[0]);
    }
    
    virtual void handleRequest(int requestPet,
      char *requestBuffer, int requestSize,
      char* &responseBuffer, int  &responseSize)const{
      // called on every localPet for every requestPet != localPet
      
      MsgRequElement<DIT> *request = (MsgRequElement<DIT> *)requestBuffer;
      int size = requestSize / sizeof(MsgRequElement<DIT>);
      vector<MsgRespElement<SIT,DIT,T> >response;
      
      // process the request and generate the response
      int iReq = 0; // request index
      typename vector<SparseMatrixIndex<DIT> >::const_iterator itSM
        = sparseMatrixDind.begin();
      T *factorList = (T *)(sparseMatrix.getFactorList());
      while ((iReq < size) && (itSM != sparseMatrixDind.end())){
        if (request[iReq].seqIndex == itSM->seqIndex){
          // found a sparse matrix entry with matching dst seqIndex
          // send the factor to the requester to be recorded in dstLinSeqVect
          MsgRespElement<SIT,DIT,T> element;
          // cast SeqInd -> SeqIndex representation
          SeqInd<SIT> seqInd = sparseMatrix.getSrcSeqIndex(itSM->index);
          element.srcSeqIndex.decompSeqIndex = seqInd.getIndex(0);
          if (seqInd.getN()>1)
            element.srcSeqIndex.tensorSeqIndex = seqInd.getIndex(1);
          else
            element.srcSeqIndex.tensorSeqIndex = 1;
          // record other members
          element.dstSeqIndex = itSM->seqIndex;
          element.factor = factorList[itSM->index];
          response.push_back(element);
          // move to the next sparse matrix element
          ++itSM;
          if (itSM == sparseMatrixDind.end()) break;
        }
        // catch up request with itSM
        while ((iReq < size) && 
          (request[iReq].seqIndex < itSM->seqIndex)){
          ++iReq;
        }
        if (iReq == size) break;
        // catch up itSM with request
        while ((itSM != sparseMatrixDind.end()) &&
          (itSM->seqIndex < request[iReq].seqIndex)){
          ++itSM;
        }
      }
      responseSize = response.size() * sizeof(MsgRespElement<SIT,DIT,T>);
      if (responseSize > requestSize)
        responseBuffer = new char[responseSize];
      else
        responseBuffer = requestBuffer;
      memcpy(responseBuffer, (char *)&(response[0]), responseSize);
    }
    
    virtual void handleResponse(int responsePet,
      char const *responseBuffer, int responseSize)const{
      // called on every localPet for every responsePet != localPet
      MsgRespElement<SIT,DIT,T> *response =
        (MsgRespElement<SIT,DIT,T> *)responseBuffer;
      int size = responseSize / sizeof(MsgRespElement<SIT,DIT,T>);
      for (unsigned i=0; i<dstLinSeqVect.size(); i++){
        typename vector<AssociationElement<DIT,SIT> >::iterator itD
          = dstLinSeqVect[i].begin();
        int iRes = 0; // response index
        while ((iRes < size) && (itD != dstLinSeqVect[i].end())){
          if (response[iRes].dstSeqIndex == itD->seqIndex){
            // found a response with matching dst seqIndex
            // add the factor to the factorList of the dstLinSeqVect element
            FactorElement<SIT> element;
            element.partnerSeqIndex = response[iRes].srcSeqIndex;
            *(T *)(element.factor) = response[iRes].factor;
            itD->factorList.push_back(element);
            // move to the next response element
            ++iRes;
            if (iRes == size) break;
          }
          // catch up itD with response
          while ((itD != dstLinSeqVect[i].end()) && 
            (itD->seqIndex < response[iRes].dstSeqIndex)){
            ++itD;
          }
          if (itD == dstLinSeqVect[i].end()) break;
          // catch up response with itD
          while ((iRes < size) &&
            (response[iRes].dstSeqIndex < itD->seqIndex)){
            ++iRes;
          }
        }
      }
    }
    
  };

//-----------------------------------------------------------------------------

#define SRC_ELEMENT_SORT_VECTOR

//-----------------------------------------------------------------------------

  template<typename IT1, typename IT2> struct FactorElementSort{
    SeqIndex<IT1> seqIndex;
    int de;
    FactorElement<IT2> *fep;
    FactorElementSort(SeqIndex<IT1> seqIndex_, int de_,
      FactorElement<IT2> *fep_){
      seqIndex = seqIndex_;
      de = de_;
      fep = fep_;
    }
  };
  template<typename IT1, typename IT2> bool operator <
    (FactorElementSort<IT1,IT2> a, FactorElementSort<IT1,IT2> b){
    return (*(a.fep) < *(b.fep));
  }

  template<typename IT> struct ElementSort{
    SeqIndex<IT> seqIndex;
    int linIndex;
    int localDe;
    int de;
  };
  template<typename IT> bool operator <
    (ElementSort<IT> a, ElementSort<IT> b){
    return (a.seqIndex < b.seqIndex);
  }
  
  template<typename IT1, typename IT2, typename T> struct MsgElement{
    SeqIndex<IT1> seqIndex;
    int de;
    SeqIndex<IT2> partnerSeqIndex;
    T factor;
  };

  //---------------------------------------------------------------------------

  template<typename SIT, typename DIT, typename T> 
    class FillLinSeqVect:public ComPat2{
    list<FactorElementSort<DIT,SIT> > &dstElementSort;
#ifdef SRC_ELEMENT_SORT_VECTOR
    vector<ElementSort<SIT> > &srcElementSort;
#else
    list<ElementSort<SIT> > &srcElementSort;
#endif
    vector<vector<AssociationElement<SIT,DIT> > >&srcLinSeqVect;
    // members that are initialized internally:
    vector<MsgElement<DIT,SIT,T> > request;
   public:
    FillLinSeqVect(
      list<FactorElementSort<DIT,SIT> > &dstElementSort_,
#ifdef SRC_ELEMENT_SORT_VECTOR
      vector<ElementSort<SIT> > &srcElementSort_,
#else
      list<ElementSort<SIT> > &srcElementSort_,
#endif
      vector<vector<AssociationElement<SIT,DIT> > >&srcLinSeqVect_
    ):
      // members that need to be set on this level because of reference
      dstElementSort(dstElementSort_),
      srcElementSort(srcElementSort_),
      srcLinSeqVect(srcLinSeqVect_)
    {
      request.resize(0);
    }
    
   private:
       
    virtual void handleLocal(){
      // called on every localPet exactly once, before any other method
      typename list<FactorElementSort<DIT,SIT> >::iterator itD =
        dstElementSort.begin();
#ifdef SRC_ELEMENT_SORT_VECTOR
      typename vector<ElementSort<SIT> >::iterator itS =
#else
      typename list<ElementSort<SIT> >::iterator itS =
#endif
        srcElementSort.begin();
      while ((itD != dstElementSort.end()) && (itS != srcElementSort.end())){
        if (itD->fep->partnerSeqIndex == itS->seqIndex){
          // a match means that both sides need to record this...
          // src side now knows about a srcLinSeqVect[][] element to be added
          AssociationElement<SIT,DIT> element;
          element.linIndex = itS->linIndex;
          element.seqIndex = itS->seqIndex;
          element.factorList.resize(1);
          element.factorList[0].partnerSeqIndex = itD->seqIndex;
          element.factorList[0].partnerDe.push_back(itD->de);
          *(T*)(element.factorList[0].factor) = *(T*)(itD->fep->factor);
          srcLinSeqVect[itS->localDe].push_back(element);
          // dst side now knows partnerDe for a FactorElement
          itD->fep->partnerDe.push_back(itS->de);
          // erase the satisfied dstElementSort element
          itD = dstElementSort.erase(itD);  // point to next the next element
          if (itD == dstElementSort.end()) break;
        }
        // catch up itD with itS
        while ((itD != dstElementSort.end()) && 
          (itD->fep->partnerSeqIndex < itS->seqIndex)){
          ++itD;
        }
        if (itD == dstElementSort.end()) break;
        // catch up itS with itD
        while ((itS != srcElementSort.end()) &&
          (itS->seqIndex < itD->fep->partnerSeqIndex)){
          ++itS;
        }
      }
    }
    
    virtual void generateRequest(int responsePet,
      char* &requestBuffer, int &requestSize){
      // called on every localPet for every responsePet != localPet
      unsigned size = dstElementSort.size();
      if (size != request.size()){
        // a new request must be constructed
        request.resize(size);
        typename list<FactorElementSort<DIT,SIT> >::iterator itD;
        int i=0;
        for (itD = dstElementSort.begin(); itD != dstElementSort.end(); ++itD){
          request[i].seqIndex = itD->seqIndex;
          request[i].de = itD->de;
          request[i].partnerSeqIndex = itD->fep->partnerSeqIndex;
          request[i].factor = *(T*)(itD->fep->factor);
          ++i;
        }
      }
      requestSize = size * sizeof(MsgElement<DIT,SIT,T>);
      requestBuffer = NULL;
      if (requestSize) requestBuffer = (char *)&(request[0]);
    }
    
    virtual void handleRequest(int requestPet,
      char *requestBuffer, int requestSize,
      char* &responseBuffer, int  &responseSize)const{
      // called on every localPet for every requestPet != localPet
      MsgElement<DIT,SIT,T> *request = (MsgElement<DIT,SIT,T> *)requestBuffer;
      int size = requestSize / sizeof(MsgElement<DIT,SIT,T>);
      // process the request and fill the response into the same buffer
      int iReq = 0; // request index
      int iRes = 0; // response index
#ifdef SRC_ELEMENT_SORT_VECTOR
      typename vector<ElementSort<SIT> >::iterator itS =
#else
      typename list<ElementSort<SIT> >::iterator itS =
#endif
        srcElementSort.begin();
      while ((iReq < size) && (itS != srcElementSort.end())){
        if (request[iReq].partnerSeqIndex == itS->seqIndex){
          // a match means that both sides need to record this...
          // src side now knows about a srcLinSeqVect[][] element to be added
          AssociationElement<SIT,DIT> element;
          element.linIndex = itS->linIndex;
          element.seqIndex = itS->seqIndex;
          element.factorList.resize(1);
          element.factorList[0].partnerSeqIndex = request[iReq].seqIndex;
          element.factorList[0].partnerDe.push_back(request[iReq].de);
          *(T*)(element.factorList[0].factor) = request[iReq].factor;
          srcLinSeqVect[itS->localDe].push_back(element);
          // dst side now knows partnerDe for a FactorElement
          // ... this means an element is added to the response
          request[iRes].seqIndex = itS->seqIndex;
          request[iRes].de = itS->de;
          ++iRes;
          // move to next request element
          ++iReq;
          if (iReq == size) break;
        }
        // catch up request with itS
        while ((iReq < size) && 
          (request[iReq].partnerSeqIndex < itS->seqIndex)){
          ++iReq;
        }
        if (iReq == size) break;
        // catch up itS with request
        while ((itS != srcElementSort.end()) &&
          (itS->seqIndex < request[iReq].partnerSeqIndex)){
          ++itS;
        }
      }
      responseSize = iRes * sizeof(MsgElement<DIT,SIT,T>);
      responseBuffer = requestBuffer;
    }
    
    virtual void handleResponse(int responsePet,
      char const *responseBuffer, int responseSize)const{
      // called on every localPet for every responsePet != localPet
      MsgElement<DIT,SIT,T> *response =
        (MsgElement<DIT,SIT,T> *)responseBuffer;
      int size = responseSize / sizeof(MsgElement<DIT,SIT,T>);
      int iRes = 0; // response index
      typename list<FactorElementSort<DIT,SIT> >::iterator itD =
        dstElementSort.begin();
      while ((iRes < size) && (itD != dstElementSort.end())){
        if (response[iRes].seqIndex == itD->fep->partnerSeqIndex){
          // a match means that this needs to be recorded in the itD
          // dst side now knows partnerDe for a FactorElement
          itD->fep->partnerDe.push_back(response[iRes].de);
          // erase the satisfied dstElementSort element
          itD = dstElementSort.erase(itD);  // point to next the next element
          // move forward in response stream
          ++iRes;
          if ((iRes == size) || (itD == dstElementSort.end())) break;
        }
        // catch up itD with iRes
        while ((itD != dstElementSort.end()) && 
          (itD->fep->partnerSeqIndex < response[iRes].seqIndex)){
          ++itD;
        }
        if (itD == dstElementSort.end()) break;
        // catch up iRes with itD
        while ((iRes < size) &&
          (response[iRes].seqIndex < itD->fep->partnerSeqIndex)){
          ++iRes;
        }
      }
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("FillLinSeqVect.handleResponse()"));
#endif
    }
    
  };
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::sparseMatMulStoreLinSeqVect_new()"
//BOPI
// !IROUTINE:  ESMCI::sparseMatMulStoreLinSeqVect_new
//
// !INTERFACE:
template<typename SIT, typename DIT> int sparseMatMulStoreLinSeqVect_new(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VM *vm,                                 // in
  Array *srcArray, Array *dstArray,       // in
  vector<SparseMatrix<SIT,DIT> >const &sparseMatrix,// in - sparse matrix vect
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
  vector<vector<AssociationElement<SIT,DIT> > >&srcLinSeqVect, // inout
  vector<vector<AssociationElement<DIT,SIT> > >&dstLinSeqVect  // inout
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
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new1.0"));
#endif
  
  // prepare
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  const int *srcLocalDeToDeMap = srcArray->getDELayout()->getLocalDeToDeMap();
  const int *dstLocalDeToDeMap = dstArray->getDELayout()->getLocalDeToDeMap();
  
  // Step1: construct dstLinSeqVect
  
  if (haloFlag){
    // for halo, straight forward construction of dstLinSeqVect from rim
    for (int i=0; i<dstLocalDeCount; i++){
      if (dstLocalDeElementCount[i]){
        // there are elements for local DE i
        // for halo all the factors are 1
        char factor[8];
        switch (typekindFactors){
        case ESMC_TYPEKIND_R4:
          *(ESMC_R4 *)factor = 1.;
          break;
        case ESMC_TYPEKIND_R8:
          *(ESMC_R8 *)factor = 1.;
          break;
        case ESMC_TYPEKIND_I4:
          *(ESMC_I4 *)factor = 1;
          break;
        case ESMC_TYPEKIND_I8:
          *(ESMC_I8 *)factor = 1;
          break;
        default:
          break;
        }
        // for halo the dst elements are in the rim of dstArray
        const std::vector<std::vector<SeqIndex<DIT> > > *rimSeqIndex;
        dstArray->getRimSeqIndex(&rimSeqIndex);
        for (int k=0; k<dstArray->getRimElementCount()[i]; k++){
          SeqIndex<DIT> seqIndex = (*rimSeqIndex)[i][k];
          if (seqIndex.valid()){
            // this rim element holds a valid seqIndex
            // add the element to dstLinSeqVect[i]
            AssociationElement<DIT,SIT> element;
            element.linIndex = dstArray->getRimLinIndex()[i][k];
            element.seqIndex = seqIndex;
            element.factorList.resize(1);
            element.factorList[0].partnerSeqIndex = seqIndex;
            memcpy(element.factorList[0].factor, factor, 8);
            dstLinSeqVect[i].push_back(element);
          }
        }
      }
    }
    
  }else{  // haloFlag
    // for not-halo, construction of dstLinSeqVect is more complex
    
    // first construct full dstLinSeqVect of all exclusive dstArray elements
    for (int i=0; i<dstLocalDeCount; i++){
      if (dstLocalDeElementCount[i]){
        // there are elements for local DE i
        dstLinSeqVect[i].reserve(dstLocalDeElementCount[i]);
        ArrayElement arrayElement(dstArray, i, true, false, false);
        // loop over all elements in exclusive region for local DE i
        while(arrayElement.isWithin()){
          // add the element to dstLinSeqVect[i]
          AssociationElement<DIT,SIT> element;
          element.linIndex = arrayElement.getLinearIndex();
          element.seqIndex = arrayElement.getSequenceIndex<DIT>();
          element.factorList.resize(0);
          dstLinSeqVect[i].push_back(element);
          arrayElement.next();
        } // end while over all exclusive elements
      }
    }
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new1.1"));
#endif
    
    // sort dstLinSeqVect
    for (int i=0; i<dstLocalDeCount; i++)
      sort(dstLinSeqVect[i].begin(), dstLinSeqVect[i].end());
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new1.2"));
#endif
    
    SparseMatrix<SIT,DIT> sparseMat(typekindFactors, NULL, 0, 0, 0, NULL);
    if (sparseMatrix.size()==1) sparseMat = sparseMatrix[0];
    
    switch (typekindFactors){
    case ESMC_TYPEKIND_R4:
      {
        QuerySparseMatrix<SIT,DIT,ESMC_R4>
          querySparseMatrix(sparseMat, dstLinSeqVect);
        querySparseMatrix.totalExchange(vm);
      }
      break;
    case ESMC_TYPEKIND_R8:
      {
        QuerySparseMatrix<SIT,DIT,ESMC_R8>
          querySparseMatrix(sparseMat, dstLinSeqVect);
        querySparseMatrix.totalExchange(vm);
      }
      break;
    case ESMC_TYPEKIND_I4:
      {
        QuerySparseMatrix<SIT,DIT,ESMC_I4>
          querySparseMatrix(sparseMat, dstLinSeqVect);
        querySparseMatrix.totalExchange(vm);
      }
      break;
    case ESMC_TYPEKIND_I8:
      {
        QuerySparseMatrix<SIT,DIT,ESMC_I8>
          querySparseMatrix(sparseMat, dstLinSeqVect);
        querySparseMatrix.totalExchange(vm);
      }
      break;
    default:
      break;
    }
    
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new1.3"));
#endif

#if 1
    // optimization for very sparse cases, where large sections of the dst
    // array are not connected to by the sparse matrix
    // for other cases this extra loop here is a waste of time
    // clear out dstLinSeqVect elements without factor elements
    for (int i=0; i<dstLocalDeCount; i++){
      typename vector<AssociationElement<DIT,SIT> >::iterator itD
        = dstLinSeqVect[i].begin();
      while (itD != dstLinSeqVect[i].end()){
        // remove dstLinSeqVect elements without factorList elements
        if ((itD->factorList).size()==0)
          itD = dstLinSeqVect[i].erase(itD);
        else
          ++itD;
      }
    }
#endif
    
  } // haloFlag
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.0"));
#endif
  
  // list of dst factor elements, sorted by partnerSeqIndex, i.e. src seqIndex
  // use a list here, because elements will be removed during totalExchange()
  list<FactorElementSort<DIT,SIT> > dstElementSort;
  for (int i=0; i<dstLocalDeCount; i++){
    for (unsigned j=0; j<dstLinSeqVect[i].size(); j++){
      for (unsigned k=0; k<dstLinSeqVect[i][j].factorList.size(); k++){
        dstElementSort.push_back(
          FactorElementSort<DIT,SIT>(dstLinSeqVect[i][j].seqIndex, 
            dstLocalDeToDeMap[i], &(dstLinSeqVect[i][j].factorList[k])));
      }
    }
  }
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.1"));
#endif
  dstElementSort.sort();
  
#ifdef TIMERS
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.2a"));
#endif
  vm->timerReset("linIndex");
  vm->timerReset("seqIndex");
#endif  // TIMERS
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.2"));
#endif

  // setup the src side information, sorted by seqIndex
#ifdef SRC_ELEMENT_SORT_VECTOR
  vector<ElementSort<SIT> > srcElementSort;
  srcElementSort.reserve(srcElementCount);
#else
  list<ElementSort<SIT> > srcElementSort;
#endif
  for (int i=0; i<srcLocalDeCount; i++){
    if (srcLocalDeElementCount[i]){
      // there are elements for local DE i
      ArrayElement arrayElement(srcArray, i, true, false, false);
      // loop over all elements in exclusive region for local DE i
      while(arrayElement.isWithin()){
        // determine the sequentialized index for the current Array element
//  vm->timerStart("seqIndex");
        SeqIndex<SIT> seqIndex = arrayElement.getSequenceIndex<SIT>();
//  vm->timerStop("seqIndex");
        // add element to srcElementSort
        ElementSort<SIT> element;
        element.seqIndex = seqIndex;
//  vm->timerStart("linIndex");
        element.linIndex = arrayElement.getLinearIndex();
//  vm->timerStop("linIndex");
        element.localDe = i;
        element.de = srcLocalDeToDeMap[i];
        srcElementSort.push_back(element);
#if 0
    {
      SeqIndex<SIT> seqIndexTest = arrayElement.getSequenceIndex<SIT>();
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " linIndex=" << element.linIndex
        << " seqIndex = "
        << seqIndex.decompSeqIndex <<"/"<< seqIndex.tensorSeqIndex
        << " arrayElement.getSequenceIndex() = "
        << seqIndexTest.decompSeqIndex <<"/"<< seqIndexTest.tensorSeqIndex;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
    }
#endif
        arrayElement.next();
      } // end while over all exclusive elements
    }
  }
  
#ifdef TIMERS
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.3a"));
#endif
  vm->timerLog("linIndex");
  vm->timerLog("seqIndex");
#endif  // TIMERS
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.3"));
#endif
#ifdef SRC_ELEMENT_SORT_VECTOR
  sort(srcElementSort.begin(), srcElementSort.end());
#else
  srcElementSort.sort();
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.4"));
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new3.0"));
#endif
  
  switch (typekindFactors){
  case ESMC_TYPEKIND_R4:
    {
      FillLinSeqVect<SIT,DIT,ESMC_R4> 
        fillLinSeqVect(dstElementSort, srcElementSort, srcLinSeqVect);
      fillLinSeqVect.totalExchange(vm);
    }
    break;
  case ESMC_TYPEKIND_R8:
    {
      FillLinSeqVect<SIT,DIT,ESMC_R8> 
        fillLinSeqVect(dstElementSort, srcElementSort, srcLinSeqVect);
      fillLinSeqVect.totalExchange(vm);
    }
    break;
  case ESMC_TYPEKIND_I4:
    {
      FillLinSeqVect<SIT,DIT,ESMC_I4> 
        fillLinSeqVect(dstElementSort, srcElementSort, srcLinSeqVect);
      fillLinSeqVect.totalExchange(vm);
    }
    break;
  case ESMC_TYPEKIND_I8:
    {
      FillLinSeqVect<SIT,DIT,ESMC_I8> 
        fillLinSeqVect(dstElementSort, srcElementSort, srcLinSeqVect);
      fillLinSeqVect.totalExchange(vm);
    }
    break;
  default:
    break;
  }
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new4.0"));
#endif
  
#if 0 
  // DO NOT NEED THIS unless for halo reduce the communication during
  // fillLinSeqVect.totalExchange(vm) to not cover factor.
  // fill in factors into srcLinSeqVect
  if (haloFlag){
    // for halo all the factors are 1
    char factor[8];
    switch (typekindFactors){
    case ESMC_TYPEKIND_R4:
      *(ESMC_R4 *)factor = 1.;
      break;
    case ESMC_TYPEKIND_R8:
      *(ESMC_R8 *)factor = 1.;
      break;
    case ESMC_TYPEKIND_I4:
      *(ESMC_I4 *)factor = 1;
      break;
    case ESMC_TYPEKIND_I8:
      *(ESMC_I8 *)factor = 1;
      break;
    default:
      break;
    }
    for (int i=0; i<srcLocalDeCount; i++){
      for (unsigned j=0; j<srcLinSeqVect[i].size(); j++){
        for (unsigned k=0; k<srcLinSeqVect[i][j].factorList.size(); k++){
          memcpy(srcLinSeqVect[i][j].factorList[k].factor, factor, 8);
        }
      }
    }
  }
#endif
  
  // clear out dstLinSeqVect elements that did not find src partners
  for (int i=0; i<dstLocalDeCount; i++){
    typename vector<AssociationElement<DIT,SIT> >::iterator itD
      = dstLinSeqVect[i].begin();
    while (itD != dstLinSeqVect[i].end()){
      // remove factorList elements without partnerDe
      typename vector<FactorElement<SIT> >::iterator it
        = itD->factorList.begin(); 
      while (it != itD->factorList.end()){
        if ((it->partnerDe).size()==0)
          it = itD->factorList.erase(it);
        else
          ++it;
      }
      // remove dstLinSeqVect elements without factorList elements
      if ((itD->factorList).size()==0)
        itD = dstLinSeqVect[i].erase(itD);
      else
        ++itD;
    }
  }
  
#if 0
  // not sure if maybe for general sparse matrix case, where the same src
  // element goes to multiple dst elements, it may be better to combine
  // srcLinSeqVect elements here? Need to do performance profiling for this.
  //
  // sort srcLinSeqVect
  for (int i=0; i<srcLocalDeCount; i++)
    sort(srcLinSeqVect[i].begin(), srcLinSeqVect[i].end());
  // combine elements with the same seqIndex, using factorList
  for (int i=0; i<srcLocalDeCount; i++){
    typename vector<AssociationElement<SIT,DIT> >::iterator itS
      = srcLinSeqVect[i].begin();
    typename vector<AssociationElement<SIT,DIT> >::iterator itSS
      = srcLinSeqVect[i].begin();
    while (itS != srcLinSeqVect[i].end()){
      if (itSS != itS){
        // not the exact same element
        if (itSS->seqIndex == itS->seqIndex){
          // combine itS into itSS
          itSS->factorList.push_back(itS->factorList[0]);
          itS = srcLinSeqVect[i].erase(itS);
        }else{
          itSS = itS;
        }
      }else{
        ++itS;
      }
    }
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new5.0"));
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
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerDe =";
          for (unsigned jj=0;
            jj<srcLinSeqVect[j][k].factorList[kk].partnerDe.size(); jj++)
            msg << srcLinSeqVect[j][k].factorList[kk].partnerDe[jj] <<", ";
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
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerDe =";
          for (unsigned jj=0;
            jj<dstLinSeqVect[j][k].factorList[kk].partnerDe.size(); jj++)
            msg << dstLinSeqVect[j][k].factorList[kk].partnerDe[jj] <<", ";
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
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_newXX.0"));
#endif

  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
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
