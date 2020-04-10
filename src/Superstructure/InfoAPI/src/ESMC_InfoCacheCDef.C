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

namespace ESMCI {

const std::size_t ESMC_INFOCACHE_RESERVESIZE = 25;
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

ESMC_Base* findBase(ESMC_Base &target, esmc_infocache_t &infoCache, std::size_t &index) {
  ESMC_Base *ret = nullptr;
  for (std::size_t ii = 0; ii < infoCache.size(); ++ii) {
    ESMC_Base *dst_base = infoCache.at(ii);
    if (basesAreEqual(target, *dst_base)) {
      ret = dst_base;
      index = ii;
    }
  }
  return ret;
}

//tdk:todo: rename variables, etc. to make clear which are field, geometries, etc.
#undef  ESMC_METHOD
#define ESMC_METHOD "collect_base_geom_objects()"
void collect_geom_base_objects(const json &infoDescStorage, esmc_infocache_t &infoCache, ESMC_Base *parentBase, esmc_infocache_t *fieldCache) {
  bool should_serialize_geom = false;
  esmc_infocache_t local_fieldCache;
  if (!fieldCache) {
    local_fieldCache.reserve(ESMCI::ESMC_INFOCACHE_RESERVESIZE);
    fieldCache = &local_fieldCache;
  }
  for (json::const_iterator it=infoDescStorage.cbegin(); it!=infoDescStorage.cend(); it++) {
    ESMC_Base *base = nullptr;
    if (it.value().at("base_is_valid")) {
      base = baseAddressToBase(it.value().at("base_address"));
      // Pointer is null if base not found
      std::size_t index = 0;
      ESMC_Base *ibase = findBase(*base, infoCache, index);
      std::string geom_type;
      if (it.value().at("is_geom") && !ibase) {
        should_serialize_geom = true;
        infoCache.push_back(base);
        assert(fieldCache);
        assert(parentBase);
        fieldCache->push_back(parentBase);
        geom_type = it.value().at("esmf_type");
      }
      if (parentBase) {
        ESMCI::Info *parent_info = parentBase->ESMC_BaseGetInfo();
        try {
          if (!parent_info->hasKey("_esmf_state_reconcile")) {
            parent_info->set("_esmf_state_reconcile/should_serialize_geom",
                             should_serialize_geom, false);
            parent_info->set("_esmf_state_reconcile/geom_type",
                             geom_type, false);
            if (!should_serialize_geom) {
              parent_info->set<int>("_esmf_state_reconcile/field_archetype_id",
                               fieldCache->at(index)->ESMC_BaseGetID(), false);
              parent_info->set<std::string>("_esmf_state_reconcile/field_archetype_base_name",
                               fieldCache->at(index)->ESMC_BaseGetName(), false);
            }
          }
        }
        ESMC_CATCH_ERRPASSTHRU
      }
    }
    if (it.value().at("esmf_type") != "Field") {
      base = nullptr;
    }
    const json &members = it.value().at("members");
    if (not members.is_null()) {
      collect_geom_base_objects(members, infoCache, base, fieldCache);
    }
  }
}

}  // namespace ESMCI

//tdk:todo: add nullptr init checks
extern "C" {

//tdk:todo: try and make this return a return code instead of the pointer
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheInitialize()"
ESMCI::esmc_infocache_t* ESMC_InfoCacheInitialize(void) {
  ESMCI::esmc_infocache_t *infoCache = new ESMCI::esmc_infocache_t;
  infoCache->reserve(ESMCI::ESMC_INFOCACHE_RESERVESIZE);
  return infoCache;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheDestroy()"
int ESMC_InfoCacheDestroy(ESMCI::esmc_infocache_t *infoCache) {
  delete infoCache;
  return ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheUpdateGeoms()"
int ESMC_InfoCacheUpdateGeoms(ESMCI::esmc_infocache_t *infoCache, ESMCI::Info *infoDesc) {
  int esmc_rc = ESMF_FAILURE;
  try {
    const json &info_desc_storage = infoDesc->getStorageRefWritable();
    ESMCI::collect_geom_base_objects(info_desc_storage, *infoCache, nullptr, nullptr);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
  return esmc_rc;
}

//#undef  ESMC_METHOD
//#define ESMC_METHOD "ESMC_InfoCacheTest()"
//int ESMC_InfoCacheTest(ESMCI::Info *infoDesc) {
//  int esmc_rc = ESMF_FAILURE;
//  try {
//    const json &info_desc_storage = infoDesc->getStorageRef();
//    collect_geom_base_objects(info_desc_storage, *infoCache);
//    esmc_rc = ESMF_SUCCESS;
//  }
//  ESMC_CATCH_ISOC
//  return esmc_rc;
//}

}  // extern "C"
