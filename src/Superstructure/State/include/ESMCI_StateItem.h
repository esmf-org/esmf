// $Id: ESMCI_StateItem.h,v 1.1 2011/05/11 16:40:39 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_StateItem_H
#define ESMCI_StateItem_H

// !USES:
#include "ESMCI_F90Interface.h"
#include "ESMC_Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// C++ StateItem class declaration
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
namespace ESMCI{

  class StateItem{
    // pointer to fortran derived type
    F90ClassHolder fortranclass;
    // methods
   public:
    StateItem(){}
    StateItem(F90ClassHolder fc){
      fortranclass = fc;
    }
    int castToFortran(F90ClassHolder *fc);
  };
}

extern "C" {
  // Prototypes of the Fortran interface functions.
  void FTN(f_esmf_stateitemcast)(ESMCI::F90ClassHolder *statItemOut,
    ESMCI::StateItem *stateItemIn, int *rc);
}

namespace ESMCI{
  int StateItem::castToFortran(F90ClassHolder *fc){
    int localrc = ESMC_RC_NOT_IMPL;
    int rc=ESMC_RC_NOT_IMPL;
    FTN(f_esmf_stateitemcast)(fc, this, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
      return rc;
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
}

#endif  // ESMCI_StateItem_H
