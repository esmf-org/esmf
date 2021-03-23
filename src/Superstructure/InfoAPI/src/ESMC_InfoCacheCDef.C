// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMC_InfoCacheCDef.C"

#define ESMC_CHECK_INIT_INFOCACHE(obj_to_check) \
  if (!obj_to_check) { \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED, "Object pointer is null. Object has not been created appropriately.", ESMC_CONTEXT, nullptr); \
    return ESMC_RC_OBJ_NOT_CREATED;}

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
typedef std::vector<ESMC_Base *> esmc_basecache_t;

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

ESMC_Base* findBase(ESMC_Base &target, esmc_basecache_t &infoCache, std::size_t &index) {
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

#undef  ESMC_METHOD
#define ESMC_METHOD "update_field_metadata_by_geom()"
void update_field_metadata_by_geom(const json &infoDescStorage, esmc_basecache_t &geomCache,
    ESMC_Base *parentBase, std::vector<int> *intVmIdCache, esmc_basecache_t *fieldCache) {
   /*!
   * @brief Traverses the metadata hierarchy provided by infoDescStorage to identify
   *   unique geometry Bases. Field Info metadata is updated allowing Fields to
   *   be disconnected from their host Fields during StateReconcile. The Fields
   *   may then be reassembled using the same metadata.
   * @param infoDescStorage [in] JSON reference from the Info map produced by ESMF_InfoDescribe.
   * @param geomCache [inout] Hold pointers to unique geometry Bases.
   * @param parentBase [inout] Used to identify a geometry Base's parent Field.
   *   This is a nullptr if the object is not a member of a collection.
   * @param intVmIdCache [inout] VM integer identifiers for the objects stored
   *   in fieldCache. This is a nullptr when calling the object non-recursively.
   * @param fieldCache [inout] Holds Base pointers for the unique Fields hosting
   *   the unique geometry Bases. This is a nullptr when the function is called
   *   non-recursively.
   * @throws ESMCI::esmc_error
   * @returns void
   */

  // Flag to indicate if the geometry associated with a Field should be serialized/deserialized.
  bool should_serialize_geom = false;
  // Local Field Base cache created when the function is not called recursively.
  esmc_basecache_t local_fieldCache;
  // Local VM integer Id cache matching the indices of the local Field cache.
  std::vector<int> local_integer_vmid_cache;
  // Create the Field and VM Id caches if their pointers are null.
  if (!fieldCache) {
    assert(!intVmIdCache);
    local_fieldCache.reserve(ESMCI::ESMC_INFOCACHE_RESERVESIZE);
    fieldCache = &local_fieldCache;
    local_integer_vmid_cache.reserve(ESMCI::ESMC_INFOCACHE_RESERVESIZE);
    intVmIdCache = &local_integer_vmid_cache;
  }
  // Main loop over the State hierarchy metadata.
  for (json::const_iterator it=infoDescStorage.cbegin(); it!=infoDescStorage.cend(); it++) {
    // Current base pointer for the target object. This becomes the parent Base
    // when calling recursively. It is also used to indicate if the host Field
    // Base is considered unique.
    ESMC_Base *base = nullptr;
    // Current integer VM identifier for the Base object.
    int curr_integer_vmid;
    // Check that the current Base is valid.
    if (it.value().at("base_is_valid")) {
      // Convert the stored Base address to an actual Base pointer.
      base = baseAddressToBase(it.value().at("base_address"));
      // The index of the found geometry Base...if it is found.
      std::size_t index = 0;
      // Geometries and other ESMF support objects may not provide an integer
      // VM identifier. This happens in StateReconcile for example when top-level
      // State objects are the synchronization target. Geometries may have a VM
      // that differs from the top-level object VMs involved in StateReconcile.
      try {
        json jvmid_int = it.value().at("vmid_int");
        if (jvmid_int.is_null()) {
          // Negative value indicates the VM integer identifier is null.
          curr_integer_vmid = -1;
        } else {
          curr_integer_vmid = jvmid_int;
        }
      }
      ESMF_INFO_CATCH_JSON
      // The geometry type is needed when reconstructing Fields. There is no
      // generic method on Fields to assign a geometry class.
      std::string geom_type;
      if (it.value().at("is_geom")) {
        // Pointer is null if base not found. This searches the geometry Base cache
        // for an existing geometry Base.
        ESMC_Base *ibase = findBase(*base, geomCache, index);
        if (!ibase) {
          // The current iteration target is a geometry and we did not find the
          // geometry base. This geometry needs to be serialized/deserialized.
          should_serialize_geom = true;
          // Add the geometry's base to the cache.
          geomCache.push_back(base);
          assert(fieldCache);
          assert(parentBase);
          assert(intVmIdCache);
          // Add the Field Base hosting the geometry to the cache.
          fieldCache->push_back(parentBase);
          // Also store its integer VM identifier.
          intVmIdCache->push_back(curr_integer_vmid);
          // Get the geometry type.
          geom_type = it.value().at("esmf_type");
        }
      }
      // The parent Base pointer is not null when it is a Field. Field metadata
      // is updated in this code block.
      if (parentBase) {
        assert(intVmIdCache);
        ESMCI::Info *parent_info = parentBase->ESMC_BaseGetInfo();
        try {
          // We may see Fields more than once if they are added to a State more
          // than once.
          if (!parent_info->hasKey("_esmf_state_reconcile")) {
            parent_info->set("_esmf_state_reconcile/should_serialize_geom",
                             should_serialize_geom, false);
            parent_info->set("_esmf_state_reconcile/geom_type",
                             geom_type, false);
            parent_info->set("_esmf_state_reconcile/integer_vmid",
                             curr_integer_vmid, false);
            // When a Field's geometry is not serialized/deserialized, then it
            // needs to be able to find its archetype Field.
            if (!should_serialize_geom) {
              parent_info->set<int>("_esmf_state_reconcile/field_archetype_id",
                fieldCache->at(index)->ESMC_BaseGetID(), false);
              parent_info->set<int>("_esmf_state_reconcile/field_archetype_integer_vmid",
                intVmIdCache->at(index), false);
              parent_info->set<std::string>("_esmf_state_reconcile/field_archetype_base_name",
                fieldCache->at(index)->ESMC_BaseGetName(), false);
            }
          }
        }
        ESMC_CATCH_ERRPASSTHRU
      }
    }
    // Only provide the parent Base when we are recursing the members of a Field.
    if (it.value().at("esmf_type") != "Field") {
      base = nullptr;
    }
    // Recurse an object's members.
    const json &members = it.value().at("members");
    if (not members.is_null()) {
      update_field_metadata_by_geom(members, geomCache, base, intVmIdCache, fieldCache);
    }
  }
}

}  // namespace ESMCI

extern "C" {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheInitialize()"
ESMCI::esmc_basecache_t* ESMC_InfoCacheInitialize(void) {
  ESMCI::esmc_basecache_t *infoCache = new ESMCI::esmc_basecache_t;
  infoCache->reserve(ESMCI::ESMC_INFOCACHE_RESERVESIZE);
  return infoCache;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheDestroy()"
int ESMC_InfoCacheDestroy(ESMCI::esmc_basecache_t *infoCache) {
  ESMC_CHECK_INIT_INFOCACHE(infoCache)
  delete infoCache;
  return ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCacheUpdateFields()"
int ESMC_InfoCacheUpdateFields(ESMCI::esmc_basecache_t *infoCache, ESMCI::Info *infoDesc) {
  ESMC_CHECK_INIT_INFOCACHE(infoCache)
  int esmc_rc = ESMF_FAILURE;
  try {
    const json &info_desc_storage = infoDesc->getStorageRefWritable();
    ESMCI::update_field_metadata_by_geom(info_desc_storage, *infoCache, nullptr,
                                     nullptr, nullptr);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
  return esmc_rc;
}

}  // extern "C"
