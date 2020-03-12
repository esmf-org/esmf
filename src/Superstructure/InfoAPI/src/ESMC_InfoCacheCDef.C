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
#include "json.hpp"

#include <iostream>
#include <vector>

using json = nlohmann::json;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//tdk:todo: consider putting this in the ESMCI namespace

typedef long int esmc_address_t;
typedef std::vector<ESMC_Base *> esmc_infocache_t;

ESMC_Base* baseAddressToBase(const esmc_address_t &baseAddress) {
  void *v = (void *) baseAddress;
  return reinterpret_cast<ESMC_Base *>(v);
}

bool basesAreEqual(ESMC_Base &src_base, ESMC_Base &dst_base) {
  ESMCI::VMId *src_vmid = src_base.ESMC_BaseGetVMId();
  int src_baseid = src_base.ESMC_BaseGetID();

  ESMCI::VMId *dst_vmid = dst_base.ESMC_BaseGetVMId();
  int dst_baseid = dst_base.ESMC_BaseGetID();

  bool ret = (ESMCI::VMIdCompare(src_vmid, dst_vmid) && (src_baseid == dst_baseid));
  return ret;
}

ESMC_Base* findBase(ESMC_Base &target, esmc_infocache_t &infoCache) {
  ESMC_Base *ret = nullptr;
  for (std::size_t ii = 0; ii < infoCache.size(); ++ii) {
    ESMC_Base *dst_base = infoCache.at(ii);
    if (basesAreEqual(target, *dst_base)) {
      ret = dst_base;
      break;
    }
  }
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "collect_base_geom_objects()"
void collect_geom_base_objects(const json &infoDescStorage, esmc_infocache_t &infoCache) {
  for (json::const_iterator it=infoDescStorage.cbegin(); it!=infoDescStorage.cend(); it++) {
    if (it.value().at("base_is_valid") && (it.value().at("is_geom"))) {
      ESMC_Base *base = baseAddressToBase(it.value().at("base_address"));
      ESMC_Base *ibase = findBase(*base, infoCache);
      if (!ibase) {  // Pointer is null if base not found
        infoCache.push_back(base);
        ESMCI::Info *info = base->ESMC_BaseGetInfo();
        try {
          //tdk:todo: use the correct attributes here
          info->set("_esmf_state_reconcile", true, false);
        }
        ESMC_CATCH_ERRPASSTHRU
      }
    }
    const json &members = it.value().at("members");
    if (not members.is_null()) {
      collect_geom_base_objects(members, infoCache);
    }
  }
}

//tdk:todo: add nullptr init checks
extern "C" {

//tdk:todo: try and make this return a return code instead of the pointer
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheInitialize()"
esmc_infocache_t* ESMC_InfoCacheInitialize(void) {
  esmc_infocache_t *infoCache = new esmc_infocache_t;
  infoCache->reserve(25);
  return infoCache;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheDestroy()"
int ESMC_InfoCacheDestroy(esmc_infocache_t *infoCache) {
  delete infoCache;
  return ESMF_SUCCESS;
}

//#undef  ESMC_METHOD
//#define ESMC_METHOD "ESMC_InfoCacheFind()"
//int ESMC_InfoCacheFind(esmc_infocache_t *infoCache, esmc_address_t &baseAddress, int &i_found) {
//  std::string msg = std::string(ESMC_METHOD) + ": entering"; //tdk:p
//  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_INFO); //tdk:p
//  int esmc_rc = ESMF_FAILURE;
//  bool found = false;
//  try {
//    ESMC_Base *base = baseAddressToBase(baseAddress);
//    found = findBase(*base, *infoCache);
//    ESMC_Base *found = findBase(*base, *infoCache);
//    i_found = (found) ? 1 : 0;  // If pointer is null, we did not find the base
//    esmc_rc = ESMF_SUCCESS;
//  }
//  ESMC_CATCH_ISOC
//  return esmc_rc;
//}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheUpdateGeoms()"
int ESMC_InfoCacheUpdateGeoms(esmc_infocache_t *infoCache, ESMCI::Info *infoDesc) {
  int esmc_rc = ESMF_FAILURE;
  try {
    const json &info_desc_storage = infoDesc->getStorageRef();
    collect_geom_base_objects(info_desc_storage, *infoCache);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
  return esmc_rc;
}

}  // extern "C"
