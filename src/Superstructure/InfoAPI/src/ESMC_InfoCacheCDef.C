// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "./src/Superstructure/InfoAPI/src/ESMC_InfoCacheCDef.C"

#include "ESMC.h"
#include "ESMCI_Base.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"
#include "ESMCI_VM.h"

#include <iostream>
#include <vector>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

typedef long int esmc_address_t;
typedef std::vector<ESMC_Base*> esmc_infocache_t;

ESMC_Base* baseAddressToBase(const esmc_address_t &baseAddress) {
  void *v = (void *) baseAddress;
  return reinterpret_cast<ESMC_Base *>(v);
}

//tdk:todo: add nullptr init checks
extern "C" {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheInitialize()"
int ESMC_InfoCacheInitialize(esmc_infocache_t *infoCache) {
  int esmc_rc = ESMF_FAILURE;
  std::size_t reserve_size = 100;
  infoCache = new esmc_infocache_t;
  try {
    infoCache->reserve(reserve_size);
    esmc_rc = ESMF_SUCCESS;
  } catch (std::bad_alloc &exc) {
    if (ESMC_LogDefault.MsgFoundError(ESMF_FAILURE, exc.what().c_str(), ESMC_CONTEXT, &esmc_rc)) return esmc_rc;
  }
  return esmc_rc;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheDestroy()"
int ESMC_InfoCacheDestroy(esmc_infocache_t *infoCache) {
  delete infoCache;
  return ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheFindUpdate()"
int ESMC_InfoCacheFindUpdate(esmc_infocache_t *infoCache, esmc_address_t &baseAddress, int &i_found, int &i_shouldUpdate) {
  int esmc_rc = ESMF_FAILURE;
  bool shouldUpdate = i_shouldUpdate == 1;  //true
  try {
    //tdk:todo: implement
    ESMC_Base *src_base = baseAddressToBase(baseAddress);
    ESMCI::VMId *src_vmid = base->ESMC_BaseGetVMId();
    int src_baseid = base->ESMC_BaseGetID();
    for (auto dst_base : *infoCache) {
      ESMCI::VMId *dst_vmid = dst_base->ESMC_BaseGetVMId();
      int dst_baseid = dst_base->ESMC_BaseGetID();
      if (ESMCI::VMIdCompare(src_vmid, dst_vmid) && (src_baseid == dst_baseid)) {
        i_found = 1;  //true
        break;
      }
    }
    esmc_rc = ESMF_SUCCESS;
  } catch (...) {
    //tdk:todo: implement
//    if (ESMC_LogDefault.MsgFoundError(ESMF_FAILURE, exc.what().c_str(), ESMC_CONTEXT, &esmc_rc)) return esmc_rc;
  }
  return esmc_rc;
}

}  // extern "C"
