  template<typename IT> struct FactorElementSort{
    DD::FactorElement<IT> *fep;
    int de;
    FactorElementSort(DD::FactorElement<IT> *fep_, int de_){
      fep = fep_;
      de = de_;
    }
  };
  template<typename IT> bool operator <
    (FactorElementSort<IT> a, FactorElementSort<IT> b){
    return (*(a.fep) < *(b.fep));
  }

  //----------------------------------------------------------------------------

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
  
  //----------------------------------------------------------------------------
  
  template<typename IT> struct MsgElement{
    SeqIndex<IT> seqIndex;
    int de;
  };

  //----------------------------------------------------------------------------

  template<typename SIT, typename DIT> class FillLinSeqVect:public ComPat2{
    list<FactorElementSort<DIT> > &dstElementSort;
    list<ElementSort<SIT> > &srcElementSort;
    vector<vector<DD::AssociationElement<SIT,DIT> > >&srcLinSeqVect;
    ESMC_TypeKind_Flag typekindFactor;
   public:
    FillLinSeqVect(
      list<FactorElementSort<DIT> > &dstElementSort_,
      list<ElementSort<SIT> > &srcElementSort_,
      vector<vector<DD::AssociationElement<SIT,DIT> > >&srcLinSeqVect_,
      ESMC_TypeKind_Flag typekindFactor_
    ):
      // members that need to be set on this level because of reference
      dstElementSort(dstElementSort_),
      srcElementSort(srcElementSort_),
      srcLinSeqVect(srcLinSeqVect_)
    {
      typekindFactor = typekindFactor_;
    }
    
   private:
       
    virtual void handleLocal()const{
      // called on every localPet exactly once, before any other method
      typename list<FactorElementSort<DIT> >::iterator itD =
        dstElementSort.begin();
      typename list<ElementSort<SIT> >::iterator itS =
        srcElementSort.begin();
#if 1
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " dstElementSort.size() = "
        << dstElementSort.size();
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
    }
#endif 
  
      while ((itD != dstElementSort.end()) && (itS != srcElementSort.end())){
#if 1
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " itD->fep->partnerSeqIndex = "
        << itD->fep->partnerSeqIndex.decompSeqIndex <<"/"<<
        itD->fep->partnerSeqIndex.tensorSeqIndex;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
      msg.str("");  // clear
      msg << "ASMM_STORE_LOG:" << __LINE__ << " itS->seqIndex = "
        << itS->seqIndex.decompSeqIndex <<"/"<<
        itS->seqIndex.tensorSeqIndex;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
    }
#endif
        if (itD->fep->partnerSeqIndex == itS->seqIndex){
          // a match means that both sides need to record this...
          // src side now knows about a srcLinSeqVect[][] element to be added
          DD::AssociationElement<SIT,DIT> element;
          element.linIndex = itS->linIndex;
          element.seqIndex = itS->seqIndex;
          element.factorCount = 1;
          element.factorList.resize(1);
          element.factorList[0].partnerSeqIndex = itS->seqIndex;
          element.factorList[0].partnerDe.push_back(itD->de);
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
#if 1
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " dstElementSort.size() = "
        << dstElementSort.size();
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
    }
#endif 
    }
    
    virtual void generateRequest(int responsePet,
      char* &requestBuffer, int &requestSize)const{
      // called on every localPet for every responsePet != localPet
      int size = dstElementSort.size();
      MsgElement<DIT> *request = NULL;  // must initialize with NULL
      if (size>0){
        // the request buffer is deleted by ComPat2::totalExchange()
        request = new MsgElement<DIT>[size];
        typename list<FactorElementSort<DIT> >::iterator itD;
        int i=0;
        for (itD = dstElementSort.begin(); itD != dstElementSort.end(); ++itD){
          request[i].seqIndex = itD->fep->partnerSeqIndex;
          request[i].de = itD->de;
          ++i;
        }
      }
      requestBuffer = (char *)request;
      requestSize = size * sizeof(MsgElement<DIT>);
    }
    
    virtual void handleRequest(int requestPet,
      char *requestBuffer, int requestSize,
      char* &responseBuffer, int  &responseSize)const{
      // called on every localPet for every requestPet != localPet
      MsgElement<DIT> *request = (MsgElement<DIT> *)requestBuffer;
      int size = requestSize / sizeof(MsgElement<DIT>);
      // process the request and fill the response into the same buffer
      int iReq = 0; // request index
      int iRes = 0; // response index
      typename list<ElementSort<SIT> >::iterator itS =
        srcElementSort.begin();
      while ((iReq < size) && (itS != srcElementSort.end())){
#if 1
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " requestPet=" << requestPet
        << " request[iReq].seqIndex = "
        << request[iReq].seqIndex.decompSeqIndex <<"/"<<
        request[iReq].seqIndex.tensorSeqIndex;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
      msg.str("");  // clear
      msg << "ASMM_STORE_LOG:" << __LINE__ << " itS->seqIndex = "
        << itS->seqIndex.decompSeqIndex <<"/"<<
        itS->seqIndex.tensorSeqIndex;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
    }
#endif
        if (request[iReq].seqIndex == itS->seqIndex){
          // a match means that both sides need to record this...
          // src side now knows about a srcLinSeqVect[][] element to be added
          DD::AssociationElement<SIT,DIT> element;
          element.linIndex = itS->linIndex;
          element.seqIndex = itS->seqIndex;
          element.factorCount = 1;
          element.factorList.resize(1);
          element.factorList[0].partnerSeqIndex = itS->seqIndex;
          element.factorList[0].partnerDe.push_back(request[iReq].de);
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
          (request[iReq].seqIndex < itS->seqIndex)){
          ++iReq;
        }
        if (iReq == size) break;
        // catch up itS with request
        while ((itS != srcElementSort.end()) &&
          (itS->seqIndex < request[iReq].seqIndex)){
          ++itS;
        }
      }
      responseSize = iRes * sizeof(MsgElement<DIT>);
      responseBuffer = requestBuffer;
    }
    
    virtual void handleResponse(int responsePet,
      char const *responseBuffer, int responseSize)const{
      // called on every localPet for every responsePet != localPet
      MsgElement<DIT> *response = (MsgElement<DIT> *)responseBuffer;
      int size = responseSize / sizeof(MsgElement<DIT>);
      int iRes = 0; // response index
      typename list<FactorElementSort<DIT> >::iterator itD =
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
      
      //TODO: how to delete the local 'request' allocation??????
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("FillLinSeqVect.handleResponse()"));
#endif
    }
    
  };

  //----------------------------------------------------------------------------

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
  vector<vector<DD::AssociationElement<SIT,DIT> > >&srcLinSeqVect, // inout
  vector<vector<DD::AssociationElement<DIT,SIT> > >&dstLinSeqVect  // inout
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
  
  // fill dstLinSeqVect with local information
  for (int i=0; i<dstLocalDeCount; i++){
    if (dstLocalDeElementCount[i]){
      // there are elements for local DE i
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
        // for halo the dst elements are in the rim of dstArray
        const std::vector<std::vector<SeqIndex<DIT> > > *rimSeqIndex;
        dstArray->getRimSeqIndex(&rimSeqIndex);
        for (int k=0; k<dstArray->getRimElementCount()[i]; k++){
          SeqIndex<DIT> seqIndex = (*rimSeqIndex)[i][k];
if (i==1) std::cout << "seqIndex=" << seqIndex.decompSeqIndex << "\n";
          if (seqIndex.valid()){
            // this rim element holds a valid seqIndex
            // add the element to dstLinSeqVect[i]
            DD::AssociationElement<DIT,SIT> element;
            element.linIndex = dstArray->getRimLinIndex()[i][k];
            element.seqIndex = seqIndex;
            element.factorCount = 1;
            element.factorList.resize(1);
            element.factorList[0].partnerSeqIndex = seqIndex;
            memcpy(element.factorList[0].factor, factor, 8);
            dstLinSeqVect[i].push_back(element);
          }
        }
      }
    }
  }
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new2.0"));
#endif
  
  // list of dst factor elements, sorted by partnerSeqIndex, i.e. src seqIndex
  // use a list here, because elements will be removed during totalExchange()
  list<FactorElementSort<DIT> > dstElementSort;
  for (int i=0; i<dstLocalDeCount; i++){
    for (unsigned j=0; j<dstLinSeqVect[i].size(); j++){
      dstElementSort.push_back(
        FactorElementSort<DIT>(&(dstLinSeqVect[i][j].factorList[0]),
        dstLocalDeToDeMap[i]));
    }
  }
  dstElementSort.sort();
  
  // setup the src side information, sorted by seqIndex
  list<ElementSort<SIT> > srcElementSort;
  for (int i=0; i<srcLocalDeCount; i++){
    if (srcLocalDeElementCount[i]){
      // there are elements for local DE i
      ArrayElement arrayElement(srcArray, i);
      // loop over all elements in exclusive region for local DE i
      while(arrayElement.isWithin()){
        // determine the sequentialized index for the current Array element
        SeqIndex<SIT> seqIndex;
        localrc = arrayElement.getSequenceIndexExclusive(&seqIndex, false);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
        // add element to srcElementSort
        ElementSort<SIT> element;
        element.seqIndex = seqIndex;
        element.linIndex = arrayElement.getLinearIndexExclusive();
        element.localDe = i;
        element.de = srcLocalDeToDeMap[i];
        srcElementSort.push_back(element);
        arrayElement.next();
      } // end while over all exclusive elements
    }
  }
#ifdef VECT
  sort(srcElementSort.begin(), srcElementSort.end());
#else
  srcElementSort.sort();
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new3.0"));
#endif
  
  FillLinSeqVect<SIT,DIT> fillLinSeqVect(dstElementSort, srcElementSort,
    srcLinSeqVect, dstArray->getTypekind());
  fillLinSeqVect.totalExchange(vm);
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreLinSeqVect_new4.0"));
#endif
  
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
    for (int j=0; j<srcLocalDeCount; j++){
      for (unsigned k=0; k<srcLinSeqVect[j].size(); k++){
        for (int kk=0; kk<srcLinSeqVect[j][k].factorCount; kk++){
          memcpy(srcLinSeqVect[j][k].factorList[kk].factor, factor, 8);
        }
      }
    }
  }
  
  // clear out dstLinSeqVect elements that did not find src partners
  for (int j=0; j<dstLocalDeCount; j++){
    typename vector<DD::AssociationElement<DIT,SIT> >::iterator itD
      = dstLinSeqVect[j].begin();
    while (itD != dstLinSeqVect[j].end()){
      // remove factorList elements without partnerDe
      typename vector<DD::FactorElement<DIT> >::iterator it
        = itD->factorList.begin(); 
      while (it != itD->factorList.end()){
        if ((it->partnerDe).size()==0)
          it = itD->factorList.erase(it);
        else
          ++it;
      }
      // remove dstLinSeqVect elements without factorList elements
      if ((itD->factorList).size()==0)
        itD = dstLinSeqVect[j].erase(itD);
      else
        ++itD;
    }
  }
  
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
          ", .factorCount = "<< srcLinSeqVect[j][k].factorCount;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
        msg.str("");  // clear
        for (int kk=0; kk<srcLinSeqVect[j][k].factorCount; kk++){
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
          ", .factorCount = "<< dstLinSeqVect[j][k].factorCount;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
        msg.str("");  // clear
        for (int kk=0; kk<dstLinSeqVect[j][k].factorCount; kk++){
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

  
