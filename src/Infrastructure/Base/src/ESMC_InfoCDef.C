// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMC_InfoCDef.C"

// Info C-Fortran method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//-----------------------------------------------------------------------------

#include "ESMC.h"
#include "ESMCI_Base.h"
#include "ESMCI_Macros.h"
#include "ESMCI_Info.h"
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

extern "C" {

//-----------------------------------------------------------------------------
// Helper Functions
//-----------------------------------------------------------------------------

ESMC_Base *baseAddressToBase(const long long int &baseAddress) {
  void *v = (void *) baseAddress;
  ESMC_Base *base = reinterpret_cast<ESMC_Base *>(v);
  return base;
}

ESMCI::Info *baseAddressToInfo(const long long int &baseAddress) {
  ESMC_Base *base = baseAddressToBase(baseAddress);
  ESMCI::Info *info = base->ESMC_BaseGetInfo();
  return info;
}

void updateDirtyInfo(const json &inqstate, int *ctr, std::vector<long long int> *base_addresses) {
  int l_ctr = 0;
  if (ctr) {
    l_ctr = *ctr;
  }
  for (json::const_iterator it=inqstate.cbegin(); it!=inqstate.cend(); it++) {
    if (it.value().at("base_is_valid")) {
        if (base_addresses) {
          long int base_address = it.value().at("base_address");
          (*base_addresses)[l_ctr] = base_address;
        }
      l_ctr++;
    }
    const json &members = it.value().at("members");
    if (not members.is_null()) {
      updateDirtyInfo(members, &l_ctr, base_addresses);
    }
  }
  if (ctr) {
    *ctr = l_ctr;
  }
}

//-----------------------------------------------------------------------------

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetInfo()"
ESMCI::Info* ESMC_BaseGetInfo(long long int &baseAddress) {
  ESMC_Base *base = reinterpret_cast<ESMC_Base*>((void*)baseAddress);
  return base->ESMC_BaseGetInfo();
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCopy()"
ESMCI::Info* ESMC_InfoCopy(ESMCI::Info *info, int &esmc_rc) {
  if (!info) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_OBJ_NOT_CREATED, "Info pointer is null. Object has not been created appropriately. Was ESMF_InfoCreate used?", ESMC_CONTEXT, &esmc_rc);
    return nullptr;
  }
  esmc_rc = ESMF_SUCCESS;
  return new ESMCI::Info(info->getStorageRef(), info->getTypeStorage());
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCopyForAttribute()"
void ESMC_InfoCopyForAttribute(const ESMCI::Info* src, ESMCI::Info* dst, int &esmc_rc) {
  ESMC_CHECK_INIT(src, esmc_rc)
  ESMC_CHECK_INIT(dst, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    dst->getStorageRefWritable() = src->getStorageRef();
    dst->getTypeStorageWritable() = src->getTypeStorage();
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCopyForAttributeReference()"
void ESMC_InfoCopyForAttributeReference(const long long int &src_base_address, const long long int &dst_base_address, int &esmc_rc) {
  esmc_rc = ESMF_FAILURE;
  try {
    ESMC_Base *src_base = baseAddressToBase(src_base_address);
    ESMC_Base *dst_base = baseAddressToBase(dst_base_address);
    // This is a memory leak but required for some applications. There needs to
    // be a way to track referencers and delete when those are finished.
//    dst_base->ESMC_BaseDeleteInfo();
    dst_base->ESMC_BaseSetInfo(src_base->ESMC_BaseGetInfo());
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCreate()"
ESMCI::Info* ESMC_InfoCreate(int &esmc_rc) {
  esmc_rc = ESMF_SUCCESS;
  return new ESMCI::Info();
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCreateByKey()"
ESMCI::Info* ESMC_InfoCreateByKey(ESMCI::Info *srcInfo, char* key, int &esmc_rc) {
  if (!srcInfo) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_OBJ_NOT_CREATED, "Info pointer is null. Object has not been created appropriately. Was ESMF_InfoCreate used?", ESMC_CONTEXT, &esmc_rc);
    return nullptr;
  }
  esmc_rc = ESMF_FAILURE;
  ESMCI::Info *info;
  try {
    std::string local_key(key);
    // Get the Base info storage
    json new_storage = srcInfo->get<json>(local_key);
    // Get the type storage
    ESMCI::Info type_storage_info(srcInfo->getTypeStorage());
    json type_storage = json::object();
    if (type_storage_info.hasKey(local_key)) {
      type_storage = type_storage_info.get<json>(local_key);
    }

    info = new ESMCI::Info(std::move(new_storage), std::move(type_storage));
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
  return info;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoCreateByParse()"
ESMCI::Info* ESMC_InfoCreateByParse(char *payload, int &esmc_rc) {
  esmc_rc = ESMF_FAILURE;
  ESMCI::Info *info;
  try {
    std::string local_payload(payload);
    info = new ESMCI::Info(local_payload);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
  return info;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoDestroy()"
void ESMC_InfoDestroy(ESMCI::Info* info, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  delete info;
  esmc_rc = ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoDump()"
void ESMC_InfoDump(ESMCI::Info *info, char *output, int &esmc_rc, int &indent) {
  // Test:
  // Notes:
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    const std::string c_output = info->dump(indent);
    for (std::size_t ii = 0; ii < c_output.size(); ++ii) {
      output[ii] = c_output[ii];
    }
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoDumpLength()"
void ESMC_InfoDumpLength(ESMCI::Info *info, int &dump_length, int &esmc_rc, int &indent) {
  // Test:
  // Notes:
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    dump_length = info->dump(indent).size();
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoErase()"
void ESMC_InfoErase(ESMCI::Info* info, char* keyParent, char* keyChild, bool &recursive,
                    int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  // This seems strange. This is the best method to delete from the Fortran
  // interface to avoid passing "" as the parent key when you want to delete
  // from the root. Otherwise a parent and child key are always required which
  // seems redundant.
  try {
    std::string localkeyParent(keyParent);
    std::string localkeyChild(keyChild);
    if (localkeyChild == "") {
      info->erase(localkeyChild, localkeyParent, recursive);
    } else {
      info->erase(localkeyParent, localkeyChild, recursive);
    }
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoInquire()"
void ESMC_InfoInquire(ESMCI::Info *info, ESMCI::Info *inq, char *key,
                       int &fortran_recursive, int *idx, int &fortran_attr_compliance,
                       int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  bool recursive = (fortran_recursive == 1) ? true:false;
  bool attr_compliance = (fortran_attr_compliance == 1) ? true:false;
  try {
    std::string localKey(key);
    json jinq = info->inquire(localKey, recursive, idx, attr_compliance);
    json &inqref = inq->getStorageRefWritable();
    inqref = std::move(jinq);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoIsEqual()"
void ESMC_InfoIsEqual(ESMCI::Info *lhs, ESMCI::Info *rhs, int &res, int &esmc_rc) {
  ESMC_CHECK_INIT(lhs, esmc_rc)
  ESMC_CHECK_INIT(rhs, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {

#if defined (__INTEL_COMPILER)
    // TODO: This is not efficient but required for a peculiar situation with
    //  Intel and Intel MPI. I think it has something to do with unsigned integer
    //  and integer comparison following deserialization.
    //TODO (bekozi): This fix is temporary. It is inefficient to string serialize
    //  for the purposes of comparison.
    bool local_res = lhs->getStorageRef().dump() == rhs->getStorageRef().dump();
#else
    bool local_res = lhs->getStorageRef() == rhs->getStorageRef();
#endif

#if 0
    std::string prefix = std::string(ESMC_METHOD) + ": ";
    std::string msg;

    msg = prefix + "local_res=" + std::to_string(local_res);
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);

    std::string lhs_dump = lhs->getStorageRef().dump();
    std::string rhs_dump = rhs->getStorageRef().dump();

    msg = prefix + "lhs_dump=" + lhs_dump;
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    msg = prefix + "rhs_dump=" + rhs_dump;
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);

    bool as_strings_eq = lhs_dump == rhs_dump;
    msg = prefix + "as_strings_eq=" + std::to_string(as_strings_eq);
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
#endif

    res = 0;  //false
    if (local_res) res = 1;  //true
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoIsPresent()"
void ESMC_InfoIsPresent(ESMCI::Info *info, char *key, int &fortran_bool_res,
        int &esmc_rc, int &fortran_bool_recursive, int &fortran_bool_isptr) {
  ESMC_CHECK_INIT(info, esmc_rc)
  bool recursive = (fortran_bool_recursive == 1) ? true:false;
  bool isptr = (fortran_bool_isptr == 1) ? true:false;
  try {
    std::string local_key(key);
    bool res = info->hasKey(local_key, isptr, recursive);
    fortran_bool_res = (res == true) ? 1:0;
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoGetTK()"
void ESMC_InfoGetTK(ESMCI::Info *info, char *key, int &typekind, int &esmc_rc,
                    int &fortran_bool_recursive) {
  ESMC_CHECK_INIT(info, esmc_rc)
  bool recursive = fortran_bool_recursive == 1;
  try {
    std::string local_key(key);
    const json *sp = &(info->getStorageRef());
    try {
      json::json_pointer jp = info->formatKey(local_key);
      ESMCI::update_json_pointer(info->getStorageRef(), &sp, jp, recursive);
      bool is_32bit = ESMCI::retrieve_32bit_flag(info->getTypeStorage(), jp, recursive);
      typekind = ESMCI::json_type_to_esmf_typekind(*sp, true, is_32bit);
    }
    ESMC_CATCH_ERRPASSTHRU
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoGetArrayMeta()"
void ESMC_InfoGetArrayMeta(ESMCI::Info *info, char *key, int &fortran_bool_is_array,
                           int &size, int &fortran_bool_recursive, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  try {
    bool recursive = fortran_bool_recursive == 1;
    std::string local_key(key);
    json const *j = info->getPointer(local_key, recursive);
    fortran_bool_is_array = j->is_array() ? 1:0;
    size = static_cast<int>(j->size());
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoIsSet()"
void ESMC_InfoIsSet(ESMCI::Info *info, char *key, int &isSet, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  std::string local_key(key);
  try {
    isSet = !(info->isNull(local_key));
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoUpdate()"
void ESMC_InfoUpdate(ESMCI::Info *to_update, ESMCI::Info *new_contents,
    int &recursive_int, int &overwrite_int, int &esmc_rc) {
  ESMC_CHECK_INIT(to_update, esmc_rc)
  ESMC_CHECK_INIT(new_contents, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  bool recursive = recursive_int == 1;
  bool overwrite = overwrite_int == 1;
  try {
    if (recursive) {
      to_update->update_for_attribute(*new_contents, overwrite);
    } else {
      to_update->update(*new_contents);
    }
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoBaseSyncDo"
void ESMC_InfoBaseSyncDo(const std::vector<long long int> &base_addresses,
                         const int &rootPet, const long long int &vmAddress,
                         int &markClean, int &esmc_rc) {
  esmc_rc = ESMF_FAILURE;
  try {
    void *v = (void *)vmAddress;
    ESMCI::VM *vm = reinterpret_cast<ESMCI::VM *>(v);
    int localPet = vm->getLocalPet();
    json j = json::object();
    bool markClean_bool = markClean == 1;
    bool dirty = true;
    if (localPet == rootPet) {
      // For each base address, blind-cast to an Info object and determine
      // if that object has been updated (dirty). If it has been updated, then
      // stick its serialized string representation inside a JSON map. For
      // broadcasting.
      for (std::size_t ii = 0; ii < base_addresses.size(); ++ii) {
        ESMCI::Info *info = baseAddressToInfo(base_addresses[ii]);
        bool is_dirty = info->isDirty();
        if (is_dirty) {
          try {
            // Serialize the JSON storage to string and include the type storage.
            j[std::to_string(ii)] = info->dump_with_type_storage();
            // This data will be broadcast and we can consider the object
            // clean.
            if (markClean_bool) {
              dirty = false;
            } else {
              dirty = true;
            }
            info->setDirty(dirty);
          }
          ESMF_INFO_CATCH_JSON
        }
      }
    }

    // Broadcast the update map.
    ESMCI::Info binfo(std::move(j));
    broadcastInfo(&binfo, rootPet, *vm);
    // Update for each string key/index in the update map.
    const json &storage = binfo.getStorageRef();
    int ikey;
    for (json::const_iterator it=storage.cbegin(); it!=storage.cend(); it++) {
      ikey = std::stoi(it.key());  // Convert the string index to an integer
      ESMCI::Info *info_to_update = baseAddressToInfo(base_addresses[ikey]);
      // This object is created from a serialized string stored in the update
      // map.
      ESMCI::Info rhs;
      try {
        rhs.parse_with_type_storage(it.value());
      }
      ESMF_CATCH_INFO
      try {
        if (localPet != rootPet) {
          info_to_update->getStorageRefWritable() = rhs.getStorageRef();
          info_to_update->getTypeStorageWritable() = rhs.getTypeStorage();

          // Since these data were broadcast, they should be marked as dirty in
          // case the root PET is switched to this PET. This may happen with
          // sequential calls to the synchronize routine.
          if (markClean_bool) {
            dirty = false;
          } else {
            dirty = true;
          }
          info_to_update->setDirty(dirty);

          // Note (bekozi): Commented in favor or replacing to avoid tracking
          // down removals in the root PET.
//          info_to_update->update(rhs);
        }
      }
      ESMF_INFO_CATCH_JSON
    }
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoBaseSync()"
void ESMC_InfoBaseSync(ESMCI::Info *inqstate, int &rootPet, long long int &vmAddress, int &markClean, int &esmc_rc) {
  ESMC_CHECK_INIT(inqstate, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    const json &j_inqstate = inqstate->getStorageRef();
    int ctr = 0;
    updateDirtyInfo(j_inqstate, &ctr, nullptr);
    std::vector<long long int> base_addresses(ctr, 0);
    updateDirtyInfo(j_inqstate, nullptr, &base_addresses);
    ESMC_InfoBaseSyncDo(base_addresses, rootPet, vmAddress, markClean, esmc_rc);
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoBroadcast()"
void ESMC_InfoBroadcast(ESMCI::Info *info, int &rootPet, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    ESMCI::VM *vm = ESMCI::VM::getCurrent(&esmc_rc);
    ESMC_CHECK_RC("", esmc_rc, "Did not get current VM");
    broadcastInfo(info, rootPet, *vm);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoReadJSON()"
void ESMC_InfoReadJSON(ESMCI::Info *info, char *filename, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    std::string filename2(filename);
    std::ifstream i(filename2, std::ifstream::in);
    if (!i.good()){
      std::string errmsg = "File location not working: " + filename2;
      ESMC_CHECK_RC("ESMC_RC_FILE_READ", ESMC_RC_FILE_READ, errmsg);
    }
    json j;
    try {
      i >> j;
    } catch (json::parse_error& e) {
      ESMF_INFO_THROW_JSON(e, "ESMC_RC_FILE_READ", ESMC_RC_FILE_READ);
    }
    i.close();
    json &out = info->getStorageRefWritable();
    out = move(j);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoWriteJSON()"
void ESMC_InfoWriteJSON(ESMCI::Info *info, char *filename, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    std::string filename2(filename);
    std::ofstream file;
    file.open(filename2);
    if (!file.is_open()) {
      std::string errmsg = "Error opening output file: " + filename2;
      ESMC_CHECK_RC("ESMC_RC_FILE_OPEN", ESMC_RC_FILE_OPEN, errmsg);
    }
    file << info->getStorageRef();
    file.close();
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

//-----------------------------------------------------------------------------
// Getting and Setting Characters
//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoGetCH()"
void ESMC_InfoGetCH(ESMCI::Info* info, char *key, char *value,
  int &vlen, int &esmc_rc, char *def, int *index, int &fortran_bool_recursive,
  int &fortran_bool_strlen_only) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  // String pointer used to define the default value if present
  std::string *def_str_ptr;
  // String object that holds the default if present
  std::string def_str;
  // Convert from Fortran integer to bool
  bool recursive = fortran_bool_recursive == 1;
  bool strlen_only = fortran_bool_strlen_only == 1;
  try {
    if (def) {
      // Set the default pointer to the string object created from the char
      // array from Fortran
      def_str = std::string(def);
      def_str_ptr = &def_str;
    } else {
      def_str_ptr = nullptr;
    }
    std::string as_str;
    std::string local_key(key);
    as_str = info->get<std::string>(local_key, def_str_ptr, index, recursive);

    // Transfer the string characters into the Fortran character array using
    // spaces to fill the Fortran array if we are past the max string length.
    if (not strlen_only) {
      for (int ii = 0; ii < vlen; ++ii) {
        if (ii < (int) as_str.size()) {
          value[ii] = as_str[ii];
        } else {
          value[ii] = ' ';
        }
      }
    } else {
      // If value is nullptr, only the stored string length is requested
      vlen = (int)as_str.size();
    }
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoSetCH()"
void ESMC_InfoSetCH(ESMCI::Info *info, char *key, char *value,
                              bool &force, int &esmc_rc, int *index, char *pkey) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    std::string local_key(key);
    std::string local_value(value);
    std::string local_pkey(pkey);
    std::string *local_pkeyp = nullptr;
    if (local_pkey.size() != 0) {local_pkeyp = &local_pkey;}
    info->set<std::string>(local_key, local_value, force, index, local_pkeyp);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoSetArrayCH()"
void ESMC_InfoSetArrayCH(ESMCI::Info *info, char *key, int &count,
                          bool &force, int &esmc_rc, char *pkey) {
  // Notes:
  //  * Only allocates storage. Does not actually insert anything!
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    std::string local_key(key);
    std::string local_pkey(pkey);
    std::string *local_pkeyp = nullptr;
    if (local_pkey.size() != 0) {local_pkeyp = &local_pkey;}
    info->set<std::vector<std::string>>(local_key, nullptr, count, force, local_pkeyp);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoSetINFO()"
void ESMC_InfoSetINFO(ESMCI::Info *info, char *key,
  ESMCI::Info *value, bool &force, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    std::string local_key(key);
    info->set(local_key, *value, force);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoSetNULL()"
void ESMC_InfoSetNULL(ESMCI::Info *info, char *key, bool &force, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    std::string local_key(key);
    info->setNull(local_key, force);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoSetDirty"
void ESMC_InfoSetDirty(ESMCI::Info *info, int &flag, int &esmc_rc) {
  ESMC_CHECK_INIT(info, esmc_rc)
  esmc_rc = ESMF_FAILURE;
  try {
    bool cflag = flag == 1;
    info->setDirty(cflag);
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
}

}  // extern "C"
