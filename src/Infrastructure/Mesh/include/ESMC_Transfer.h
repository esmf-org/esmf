// $Id: ESMC_Transfer.h,v 1.1 2007/08/07 17:47:59 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Transfer_h
#define ESMC_Transfer_h

#include <ESMC_MeshDB.h>
#include <ESMC_Search.h>
#include <ESMC_MeshField.h>
#include <ESMC_MasterElement.h>


#include <vector>

namespace ESMCI {
namespace MESH {

struct TransferPair {
  typedef std::pair<MEField<>*, MEField<>*> pairtype;
  TransferPair(MEField<> *f1, MEField<> *f2) : fields(f1, f2) {}
  TransferPair() : fields(NULL, NULL) {}
  typedef MEField<> ftype;
  pairtype fields;
};

template<typename fiter>
void Transfer(fiter fields_begin, fiter fields_end, SearchResult &sres, bool low=false);

template<typename fiter>
void RTransfer(UInt pdeg, fiter fields_begin, fiter fields_end, SearchResult &sres, MEField<> &coord, bool low=false);

// Compute the matrix for a transfer
template<typename fiter>
void RMatrixTransfer(UInt pdeg, fiter fields_begin, fiter fields_end,
       SearchResult &sres, MEField<> &coord, bool low);

} // neamespac
} // neamespac

#endif
