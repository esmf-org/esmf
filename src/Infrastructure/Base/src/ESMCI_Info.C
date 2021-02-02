// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_Info.C"

// Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//-----------------------------------------------------------------------------

#include "ESMC.h"
#include "ESMCI_Macros.h"
#include "ESMCI_Info.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"
#include "ESMCI_VM.h"
#include "json.hpp"

#include <assert.h>
#include <vector>
#include <iostream>
#include <fstream>
#include <limits>

using json = nlohmann::json;  // Convenience rename for JSON namespace.

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

#define HANDLE_JSON_TYPE_CHECK(dst) \
  if (!allow_implicit) { \
    try { \
      T archetype; \
      handleJSONTypeCheck(key, json(archetype), dst); \
    } \
    ESMC_CATCH_ERRPASSTHRU }

namespace ESMCI {

//-----------------------------------------------------------------------------
// Helper Functions -----------------------------------------------------------
//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "alignOffset()"
void alignOffset(int &offset) {
  int nbytes = offset % 8;
  if (nbytes!=0) offset += (8 - nbytes);
}

void dolog(const std::string &logmsg, const std::string &method, int line) {
  std::string local_logmsg;
  local_logmsg = method + std::string("@") + std::to_string(line) + ": " + logmsg;
  ESMC_LogWrite(local_logmsg.c_str(), ESMC_LOGMSG_INFO);
}

#undef  ESMC_METHOD
#define ESMC_METHOD "check_is_object()"
void check_is_object(const json &j) {
  // Test:
  // Notes:
  if (!j.is_object()) {
    std::string msg = "JSON Object required. Type is: " + std::string(j.type_name());
    ESMC_CHECK_RC("ESMF_RC_ARG_BAD", ESMF_RC_ARG_BAD, msg)
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "check_is_not_object()"
void check_is_not_object(const json &j) {
  // Test:
  // Notes:
  if (j.is_object()) {
    std::string msg = "JSON Object not allowed";
    ESMC_CHECK_RC("ESMF_RC_ARG_BAD", ESMF_RC_ARG_BAD, msg)
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "is_attpack()"
bool is_attpack(const json &j) {
  bool ret = false;
  if (j.is_object()) {
    for (json::const_iterator it=j.cbegin(); it!=j.cend(); it++) {
      if (!it.value().is_object()) { break; }
    }
    ret = true;
  }
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "get_attpack_count()"
std::size_t get_attpack_count(const json &j) {
  // Test: test_get_attpack_count
  std::size_t ret = 0;
  try {
    check_is_object(j);
    for (json::const_iterator it = j.cbegin(); it != j.cend(); it++) {
      if (is_attpack(it.value())) { ret++; }
    }
  }
  ESMF_CATCH_INFO
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "json_array_type()"
json::value_t json_array_type(const json::array_t &jarray) noexcept {
  // Test:
  // Notes: returns null type if zero-length; return type of first element assuming
  //  that remaining elements have the same type
  json::value_t ret;
  if (jarray.size() == 0) {
    ret = json::value_t::null;
  } else {
    ret = jarray.at(0).type();
  }
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "create_json_attribute_count_map()"
count_map_t create_json_attribute_count_map(void) {
  // Test:
  // Notes:
  count_map_t counts;
  counts.reserve(4);
  counts["attPackCount"] = 0;
  counts["attPackCountTotal"] = 0;
  counts["attrCount"] = 0;
  counts["attrCountTotal"] = 0;
  return counts;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "update_json_attribute_count_map()"
void update_json_attribute_count_map(count_map_t &counts, const json &j, bool first) {
  // Test: test_update_json_attribute_count_map
  // Throws: esmc_error
  // Notes:

#if 0
    std::string logmsg = "j.dump()=" + j.dump();
    dolog(logmsg, ESMC_METHOD, __LINE__);
#endif

  try {
    check_is_object(j);
    std::size_t attpack_count = 0;
    std::size_t attr_count = 0;
    for (json::const_iterator it = j.cbegin(); it != j.cend(); it++) {
      if (is_attpack(it.value())) {
        for (json::const_iterator it2 = it.value().cbegin();
             it2 != it.value().cend(); it2++) {
          if (!(it2.value().is_object())) {
            esmc_error exc("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD,
                                "iteration target must be a JSON object");
            throw exc;
          };
          attpack_count++;
          if (first) counts.at("attPackCount")++;
          counts.at("attPackCountTotal")++;
          update_json_attribute_count_map(counts, it2.value(), false);
        }
      } else {
        // Only AttPack objects are allowed with the legacy ESMF Attribute
        // JSON proxy storage. This means a convention and purpose are required
        // to create the expected nesting structure.
        check_is_not_object(it.value());
        attr_count++;
      }

    }
    if (first) counts.at("attrCount") = attr_count;
    counts.at("attrCountTotal") += attr_count;
  }
  ESMF_CATCH_INFO
}

#undef  ESMC_METHOD
#define ESMC_METHOD "update_json_pointer(<const>)"
void update_json_pointer(const json &j, json const **jdp, const json::json_pointer &key,
  bool recursive) {
  // Test: test_update_json_pointer
  // Notes:
  // Throws: json::out_of_range when key not found
  try {
    *jdp = &(j.at(key));
  } catch (json::out_of_range &e) {
    if (recursive) {
      for (json::const_iterator it=j.cbegin(); it!=j.cend(); it++) {
        if (it.value().is_object()) {
          update_json_pointer(it.value(), jdp, key, true);
        }
      }
    }
    if (!*jdp) {
      throw(e);
    }
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "update_json_pointer(<const>)"
void update_json_pointer(json &j, json **jdp, const json::json_pointer &key,
  bool recursive) {
  // Test: test_update_json_pointer (for const overload)
  // Notes:
  // Throws: json::out_of_range when key not found
  try {
    *jdp = &(j.at(key));
  } catch (json::out_of_range &e) {
    if (recursive) {
      for (json::iterator it=j.begin(); it!=j.end(); it++) {
        if (it.value().is_object()) {
          update_json_pointer(it.value(), jdp, key, true);
        }
      }
    }
    if (!*jdp) {
      throw(e);
    }
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "update_json_pointer(<const> + container)"
void update_json_pointer(json &j, json **jdp, const json::json_pointer &key,
                         bool recursive, json **container) {
  // Test: test_update_json_pointer
  // Notes:
  // Throws: json::out_of_range when key not found
  try {
    *jdp = &(j.at(key));
    *container = &j;
  } catch (json::out_of_range &e) {
    if (recursive) {
      for (json::iterator it=j.begin(); it!=j.end(); it++) {
        if (it.value().is_object()) {
          update_json_pointer(it.value(), jdp, key, true, container);
        }
      }
    }
    if (!*jdp) {
      throw(e);
    }
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "find_by_index()"
json::iterator find_by_index(json &j, const std::size_t index_target, bool recursive,
  bool attr_compliance, std::size_t *index_current, bool *found) {
  // Test: test_find_by_index
  // Notes:
  // Throws:
  json::iterator ret;
  try {
    check_is_object(j);
    esmc_error exc("ESMC_RC_NOT_FOUND", ESMC_RC_NOT_FOUND,
                        "index out of range");
    std::size_t local_index_default = 0;
    std::size_t *local_index_current = nullptr;
    bool local_found = false;
    bool first = false;
    if (index_current) {
      local_index_current = index_current;
    } else {
      first = true;
      found = &local_found;
      local_index_current = &local_index_default;
    }
    for (json::iterator it = j.begin(); it != j.end(); it++) {
      if (attr_compliance && is_attpack(it.value())) continue;
      if ((*local_index_current) == index_target) {
        ret = it;
        *found = true;
        break;
      }
      (*local_index_current)++;
    }
    if (!(*found) && recursive && attr_compliance) {
      for (json::iterator it = j.begin(); it != j.end(); it++) {
        if (is_attpack(it.value())) {
          for (json::iterator it2 = it.value().begin();
               it2 != it.value().end(); it2++) {
            check_is_object(it2.value());
            ret = find_by_index(it2.value(), index_target, recursive,
                                attr_compliance, local_index_current, found);
            if (*found) break;
          }
        }
      }
    }
    if (first && !(*found)) throw exc;
  }
  ESMF_CATCH_INFO
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "handleHasKey()"
bool handleHasKey(const Info &info, const json::json_pointer &jpkey, bool force) {
  // Exceptions:  ESMCI::esmc_error
  bool has_key;
  try {
    has_key = info.hasKey(jpkey, true);
  }
  catch (ESMCI::esmc_error &exc_esmf) {
    ESMC_ERRPASSTHRU(exc_esmf)
  }
  if (has_key && !force) {
    std::string msg = "Key \'" + std::string(jpkey) + "\' already in map and force=false.";
    ESMC_CHECK_RC("ESMC_RC_CANNOT_SET", ESMC_RC_CANNOT_SET, msg)
  }
  return has_key;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "handleJSONTypeCheck()"
void handleJSONTypeCheck(
  key_t &key,       // used for error string formatting
  const json &src,  // the source target; the JSON type we are interested in checking
  const json &dst   // the destination target used to check against
                         ) {
  if (!src.is_null() && src.type() != dst.type()) {
    if ((src.is_array() && src.size() == 1) || (dst.is_array() && dst.size() == 1)) {
      if (src.is_array() && dst.is_array()) {
        handleJSONTypeCheck(key, src[0], dst[0]);
      } else if (src.is_array()) {
        handleJSONTypeCheck(key, src[0], dst);
      } else if (dst.is_array()) {
        handleJSONTypeCheck(key, src, dst[0]);
      } else {
        std::string errmsg = "Types not equivalent. The key is: " + key;
        ESMC_CHECK_RC("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD, errmsg)
      }
    } else {
      // Allow unsigned to integer conversion and vice-versa.
      if (!((src.type() == json::value_t::number_integer ||
             src.type() == json::value_t::number_unsigned) &&
            (dst.type() == json::value_t::number_integer ||
             dst.type() == json::value_t::number_unsigned))) {
        std::string errmsg = "Types not equivalent. The key is: " + key;
        ESMC_CHECK_RC("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD, errmsg)
      }
    }
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "isIn(<string,vector>)"
bool isIn(key_t& target, const std::vector<std::string>& container) {
  auto it = std::find(container.cbegin(), container.cend(), target);
  return !(it == container.cend());
}

#undef  ESMC_METHOD
#define ESMC_METHOD "isIn(<string vector,vector>)"
bool isIn(const std::vector<std::string>& target, const std::vector<std::string>& container) {
  std::size_t count = 0;
  std::size_t required = target.size();
  bool ret;
  if (target.size() == 0) {
    ret = true;
  } else {
    ret = false;
  }
  if (!ret) {
    for (const auto &t : target) {
      if (isIn(t, container)) {
        ++count;
      }
      if (count == required) {
        ret = true;
        break;
      }
    }
  }
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "isIn(<string,json>)"
bool isIn(key_t& target, const json& j) {
  if (j.is_null()) {
    return false;
  } else {
    return j.find(target) != j.end();
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "json_type_to_esmf_typekind()"
ESMC_TypeKind_Flag json_type_to_esmf_typekind(const json &j, bool allow_array, bool is_32bit) {
  ESMC_TypeKind_Flag esmf_type;
  if (j.type() == json::value_t::null) {
    esmf_type = ESMF_NOKIND;
  } else if (j.type() == json::value_t::boolean) {
    esmf_type = ESMC_TYPEKIND_LOGICAL;
  } else if (j.type() == json::value_t::number_integer || j.type() == json::value_t::number_unsigned) {
     if (is_32bit) {
      esmf_type = ESMC_TYPEKIND_I4;
    } else {
      esmf_type = ESMC_TYPEKIND_I8;
    }
  } else if (j.type() == json::value_t::number_float) {
    if (is_32bit) {
      esmf_type = ESMC_TYPEKIND_R4;
    } else {
      esmf_type = ESMC_TYPEKIND_R8;
    }
  } else if (j.type() == json::value_t::object) {
    esmf_type = ESMF_NOKIND;
  } else if (j.type() == json::value_t::array) {
    if (allow_array && j.size() > 0) {
      esmf_type = ESMC_TYPEKIND_I4;
      for (std::size_t ii = 0; ii < j.size(); ++ii) {
        ESMC_TypeKind_Flag curr_esmf_type = json_type_to_esmf_typekind(j.at(ii), false, is_32bit);
        if (curr_esmf_type > esmf_type) {
          esmf_type = curr_esmf_type;
        }
      }
    } else {
      esmf_type = ESMF_NOKIND;
    }
  } else if (j.type() == json::value_t::string) {
    esmf_type = ESMC_TYPEKIND_CHARACTER;
  } else {
    assert(false);
  }
  return esmf_type;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "check_init_from_json()"
void check_init_from_json(const json j) {
  // Test:
  // Notes:
  if (!j.is_object()) {
    std::string msg = "Can only create Info from JSON value_t::object_t types";
    ESMC_CHECK_RC("ESMC_RC_OBJ_NOT_CREATED", ESMC_RC_OBJ_NOT_CREATED, msg);
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "has_key_json()"
bool has_key_json(const json &target, const json::json_pointer &jp, bool recursive) {
  // Exceptions:  ESMCI::esmc_error
  bool ret;
  // Use JSON pointer syntax. This is slower than just attempting to find
  // the key. JSON pointers do not work with find. See: https://github.com/nlohmann/json/issues/1182#issuecomment-409708389
  // for an explanation.
  try {
    try {
      json const *dummy = nullptr;
      update_json_pointer(target, &dummy, jp, recursive);
      ret = true;
    }
    catch (json::out_of_range& e) {
      ret = false;
    }
  }
  ESMC_CATCH_ERRPASSTHRU
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "retrieve_32bit_flag"
bool retrieve_32bit_flag(const json &j, const json::json_pointer &jp, bool recursive) {
  bool ret = false;
  // Only attempt to get 32-bit information if the type storage is initialized
  // and has at least a single entry.
  if (!j.is_null() && j.size() > 0) {
    const json *ts = nullptr;
    try {
      update_json_pointer(j, &ts, jp, recursive);
      try {
        if (ts->is_boolean()) { ret = *ts; }
      }
      ESMF_INFO_CATCH_JSON
    } catch (json::out_of_range &e) {
      // This is okay, and we default to the standard JSON type definitions with
      // no checking for 32-bit types
    }
  }
  return ret;
}

//-----------------------------------------------------------------------------
// Info Implementations -------------------------------------------------------
//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "Info(json&)"
Info::Info(const json& storage) {
  try {
    check_init_from_json(storage);
    this->storage = storage;
    this->type_storage = json::object();
  }
  ESMF_CATCH_INFO
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info(json&)"
Info::Info(const json& storage, const json& type_storage) {
  try {
    check_init_from_json(storage);
    check_init_from_json(type_storage);
    this->storage = storage;
    this->type_storage = type_storage;
  }
  ESMF_CATCH_INFO
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info(json&&)"
Info::Info(json&& storage) {
  try {
    check_init_from_json(storage);
    this->storage = std::move(storage);
    this->type_storage = json::object();
  }
  ESMF_CATCH_INFO
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info(json&&, json&&)"
Info::Info(json&& storage, json&& type_storage) {
  try {
    check_init_from_json(storage);
    check_init_from_json(type_storage);
    this->storage = std::move(storage);
    this->type_storage = std::move(type_storage);
  }
  ESMF_CATCH_INFO
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info(string&)"
Info::Info(key_t& input) {
  // Exceptions: ESMCI::esmc_error
  try {
    this->storage = json::parse(input);
    this->type_storage = json::object();
  }
  ESMF_CATCH_INFO
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::dump()"
std::string Info::dump(void) const {
  // Exceptions: ESMCI::esmc_error

  std::string ret;
  try {
    ret = this->dump(0);
  } catch (ESMCI::esmc_error &e) {
    ESMC_ERRPASSTHRU(e);
  }
  return ret;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::dump_with_type_storage"
std::string Info::dump_with_type_storage(void) {
  // Test: test_dump_and_parse_with_type_storage
  std::string ret;
  try {
    bool has_type_storage = this->getTypeStorage().size() > 0;
    if (has_type_storage) {
      this->getStorageRefWritable()["_esmf_info_type_storage"] = this->getTypeStorage();
    }
    ret = this->dump(0);
    if (has_type_storage) this->erase("", "_esmf_info_type_storage");
  }
  ESMC_CATCH_ERRPASSTHRU
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::deserialize()"
void Info::deserialize(char *buffer, int *offset) {
  // Test: testSerializeDeserialize, testSerializeDeserialize2
  // Exceptions:  ESMCI:esmc_error
  alignOffset(*offset);

  // Act like an integer to get the string length.
  int *ip = (int *)(buffer + *offset);
  int length = *ip;

  // Move 4 bytes to the start of the string actual.
  (*offset) += sizeof(int);
  std::string infobuffer(&(buffer[*offset]), length);
  try {
    this->parse_with_type_storage(infobuffer);
  }
  catch (esmc_error &e) {
    ESMC_ERRPASSTHRU(e);
  }
  (*offset) += length;
  alignOffset(*offset);
  return;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::dump(int indent)"
std::string Info::dump(int indent) const {
  // Exceptions: ESMCI::esmc_error

  if (indent < 0) {
    ESMC_CHECK_RC("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD, "indent must be >= 0");
  }
  std::string ret;
  const json &j = this->getStorageRef();
  try {
    if (indent == 0) {
      ret = j.dump();
    } else {
      ret = j.dump(indent);
    }
  } catch (json::type_error &e) {
    ESMF_INFO_THROW_JSON(e, "ESMC_RC_ARG_INCOMP", ESMC_RC_ARG_INCOMP);
  }
  return ret;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::erase()"
void Info::erase(key_t &keyParent, key_t &keyChild, bool recursive) {
  // Exceptions: ESMCI::esmc_error
  try {
    json::json_pointer key = this->formatKey(keyParent);
    try {
      json &j = this->getStorageRefWritable();
      json *jp = nullptr;
      json *container = nullptr;
      update_json_pointer(j, &jp, key, recursive);
      assert(jp);
      if (recursive) {
        try {
          key = this->formatKey(keyChild);
          json *dummy = nullptr;
          update_json_pointer(*jp, &dummy, key, recursive, &container);
          assert(dummy);
          assert(container);
          jp = container;
        }
        ESMF_CATCH_INFO
      }
      json &found = jp->at(keyChild); // Check that the key exists
      jp->erase(keyChild);

      // Erase from the type storage if the key combination is present
      if (this->getTypeStorage().size() > 0) {
        try {
          ESMCI::Info type_storage(this->getTypeStorage());
          json::json_pointer type_target(key.to_string() + "/" + keyChild);
          if (type_storage.hasKey(type_target, recursive)) {
            type_storage.erase(keyParent, keyChild, recursive);
            this->getTypeStorageWritable() = std::move(type_storage.getTypeStorageWritable());
          }
        }
        ESMC_CATCH_ERRPASSTHRU
      }
    }
    ESMF_INFO_CATCH_JSON
  }
  ESMF_CATCH_INFO
  this->dirty = true;
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::formatKey()"
json::json_pointer Info::formatKey(key_t& key) {
  // Exceptions:  ESMCI:esmc_error
  std::string localKey;

  if (key != "" && key[0] != '/') {
    localKey = '/' + key;
  } else {
    localKey = key;
  }

  if (localKey.find("///") != std::string::npos){
    std::string msg = "Triple forward slashes not allowed in key names";
    ESMC_CHECK_RC("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD, msg);
  }

  try {
    json::json_pointer jp(localKey);
    return jp;
  }
  catch (json::parse_error &e) {
    ESMF_INFO_THROW_JSON(e, "ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD);
  }
};

#undef  ESMC_METHOD
#define ESMC_METHOD "check_overflow(<template>)"
template <typename T, typename T2>
void check_overflow(T dst, T2 tocheck) {
  // Exceptions:  ESMCI:esmc_error
  T2 max = std::numeric_limits<T>::max();
  if (tocheck > max) {
    const std::string errmsg = "Overflow error during type conversion.";
    ESMC_CHECK_RC("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD, errmsg)
  }
};
template void check_overflow(int, long int);
template void check_overflow(int, unsigned long int);
template void check_overflow(float, double);

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::get()"
template <typename T>
T Info::get(key_t &key, const T *def, const int *index, bool recursive, std::string *ikey,
  bool allow_implicit) const {
  // Exceptions:  ESMCI:esmc_error

#if 0
    std::string prefix = std::string(ESMC_METHOD) + ": ";
    std::string msg;
    msg = prefix + "key=" + key;
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    msg = prefix + "recursive=" + std::to_string(recursive);
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    msg = prefix + "this->dump()=...";
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    ESMC_LogWrite(this->dump().c_str(), ESMC_LOGMSG_DEBUG);
#endif

  T ret;
  try {
    json::json_pointer jpath = this->formatKey(key);
    try {
      json const *jp = nullptr;
      update_json_pointer(this->getStorageRef(), &jp, jpath, recursive);
      assert(jp);
      if (index) {
        if (jp->is_array()) {
          json::array_t const *jarr = jp->get_ptr < json::array_t const * > ();
          try {
            const json &at_index = jarr->at(*index);
            if (at_index.is_number()) {
              try { check_overflow(ret, at_index); }
              ESMC_CATCH_ERRPASSTHRU
            }
            ret = at_index;
            HANDLE_JSON_TYPE_CHECK(at_index)
          }
          catch (std::out_of_range &e) {
            ESMC_CHECK_RC("ESMC_RC_ARG_OUTOFRANGE", ESMC_RC_ARG_OUTOFRANGE, e.what())
          }
        } else if (jp->is_object()) {
          if (*index >= (int) jp->size()) {
            std::string msg = "'index' greater than object count";
            ESMC_CHECK_RC("ESMC_RC_ARG_OUTOFRANGE", ESMC_RC_ARG_OUTOFRANGE, msg)
          }
          int ctr = 0;
          for (json::const_iterator it = jp->cbegin();
               it != jp->cend(); it++) {
            if (ctr == *index) {
              ret = it.value();
              if (ikey) {
                *ikey = it.key();
              }
              break;
            } else {
              ctr++;
            }
          }
        } else if (jp->is_string()) {
          if (*index >= (int) jp->size()) {
            std::string msg = "'index' greater than string length";
            ESMC_CHECK_RC("ESMC_RC_ARG_OUTOFRANGE", ESMC_RC_ARG_OUTOFRANGE, msg)
          }
          ret = jp[*index];
        } else {
          std::string msg = "'index' only supported for JSON strings, arrays, or objects. JSON type is: ";
          msg = msg + jp->type_name();
          ESMC_CHECK_RC("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD, msg);
        }
      } else {
        if (jp->is_null() && def) {
          ret = *def;
        } else {
          try {
            if (jp->is_number()) {
              try { check_overflow(ret, *jp); }
              ESMC_CATCH_ERRPASSTHRU
            }
            ret = *jp;  // Standard retrieval location
            HANDLE_JSON_TYPE_CHECK(*jp)
          } catch (json::type_error &e) {
#if 0
            std::string emsg2 = std::string(__FILE__) + ":" + std::to_string(__LINE__) + " " + ESMC_METHOD + ": JSON dump: " + this->dump();
            ESMC_LogWrite(emsg2.c_str(), ESMC_LOGMSG_ERROR);
#endif
            std::string emsg = std::string(__FILE__) + ":" + std::to_string(__LINE__) + " " + ESMC_METHOD + ": Failed type check (JSON trace will follow): " + key;
            ESMC_LogWrite(emsg.c_str(), ESMC_LOGMSG_ERROR);
            ESMF_INFO_THROW_JSON(e, "ESMF_RC_ATTR_WRONGTYPE", ESMF_RC_ATTR_WRONGTYPE)
          }
        }
      }
    } catch (json::out_of_range &e) {
      if (def) {
        ret = *def;
      } else {
        std::string emsg = std::string(__FILE__) + ":" + std::to_string(__LINE__) + " " + ESMC_METHOD + ": Key not found (JSON trace will follow): " + key;
        ESMC_LogWrite(emsg.c_str(), ESMC_LOGMSG_ERROR);
        ESMF_INFO_THROW_JSON(e, "ESMF_RC_ATTR_NOTSET", ESMF_RC_ATTR_NOTSET)
      }
    }
  }
  ESMF_CATCH_INFO
  return ret;
}
template float Info::get(key_t&, const float*, const int*, bool, std::string*, bool) const;
template double Info::get(key_t&, const double*, const int*, bool, std::string*, bool) const;
template int Info::get(key_t&, const int*, const int*, bool, std::string*, bool) const;
template long int Info::get(key_t&, const long int*, const int*, bool, std::string*, bool) const;
template bool Info::get(key_t&, const bool*, const int*, bool, std::string*, bool) const;
template std::string Info::get(key_t&, const std::string*, const int*, bool, std::string*, bool) const;
template json Info::get(key_t&, const json*, const int*, bool, std::string*, bool) const;

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::get() Info"
void Info::get(ESMCI::Info &info, key_t &key) const {
  // Test: testGetInfoObject
  // Notes:

#if 0
  std::string msg = std::string(ESMC_METHOD) + ": key=" + key;
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  msg = std::string(ESMC_METHOD) + ": this dump=" + this->dump(0);
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
#endif

  json j;
  try {
    j = this->get<json>(key);

#if 0
    std::string msg2 = std::string(ESMC_METHOD) + ": j dump=" + j.dump();
    ESMC_LogWrite(msg2.c_str(), ESMC_LOGMSG_DEBUG);
#endif
    
    check_init_from_json(j);
  }
  ESMF_CATCH_INFO
  info.getStorageRefWritable() = std::move(j);

#if 0
  std::string msg3 = std::string(ESMC_METHOD) + ": info dump=" + info.dump(0);
  ESMC_LogWrite(msg3.c_str(), ESMC_LOGMSG_DEBUG);
#endif

}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::getvec() vector<T>"
template <typename T>
std::vector<T> Info::getvec(key_t& key, bool recursive) const {
  std::vector<T> ret;
  try {
    json const *j;
    try {
      j = this->getPointer(key, recursive);
    }
    ESMC_CATCH_ERRPASSTHRU
    ret = j->get<std::vector<T>>();
  }
  ESMF_CATCH_INFO
  return ret;
};
template std::vector<std::string> Info::getvec(key_t&, bool) const;

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::getPointer()"
json const * Info::getPointer(key_t& key, bool recursive) const {
  // Exceptions:  ESMCI:esmc_error

  json const *ret = nullptr;
  try {
    json::json_pointer jpath = this->formatKey(key);
    try {
      update_json_pointer(this->getStorageRef(), &ret, jpath, recursive);
      assert(ret);
    }
    ESMF_INFO_CATCH_JSON
  }
  ESMF_CATCH_INFO
  return ret;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::hasKey()"
bool Info::hasKey(key_t& key, bool isptr, bool recursive) const {
  // Exceptions:  ESMCI::esmc_error

#if 0
  std::string prefix = std::string(ESMC_METHOD) + ": ";
  std::string msg;
  msg = prefix + "key=" + key;
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  msg = prefix + "isptr=" + std::to_string(isptr);
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  msg = prefix + "recursive=" + std::to_string(recursive);
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  msg = prefix + "this->dump()=...";
  ESMC_LogWrite((this->dump()).c_str(), ESMC_LOGMSG_DEBUG);
#endif

  bool ret;
  if (isptr || recursive) {
    // Use JSON pointer syntax. This is slower than just attempting to find
    // the key. JSON pointers do not work with find. See: https://github.com/nlohmann/json/issues/1182#issuecomment-409708389
    // for an explanation.
    try {
      json::json_pointer jp = this->formatKey(key);
      ret = this->hasKey(jp, recursive); // Call overload for JSON Pointer
    }
    ESMF_CATCH_INFO

  } else {
    // This is faster because it avoids exceptions. However, it does not work
    // with JSON pointers.
    ret = !(this->getStorageRef().find(key) == storage.end());
  }

#if 0
  std::string prefix2 = std::string(ESMC_METHOD) + ": ";
  std::string msg2;
  msg2 = prefix2 + "ret=" + std::to_string(ret);
  ESMC_LogWrite(msg2.c_str(), ESMC_LOGMSG_DEBUG);
#endif

  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::hasKey()"
bool Info::hasKey(const json::json_pointer &jp, bool recursive) const {
  // Exceptions:  ESMCI::esmc_error
  bool ret;
  try {
    const json &storage = this->getStorageRef();
    ret = has_key_json(storage, jp, recursive);
  }
  ESMC_CATCH_ERRPASSTHRU
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::inquire()"
json Info::inquire(key_t &key, bool recursive, const int *idx, bool attr_compliance) const {
  // Test: testInquire, testInquire32Bit
  // Notes:

#if 0
    std::string prefix = std::string(ESMC_METHOD) + ": ";
    std::string msg;
    msg = prefix + "key=" + key;
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    msg = prefix + "recursive=" + std::to_string(recursive);
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    msg = prefix + "attr_compliance=" + std::to_string(attr_compliance);
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    msg = prefix + "this->dump()=...";
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    ESMC_LogWrite(this->dump().c_str(), ESMC_LOGMSG_DEBUG);
    msg = prefix + "this->type_storage.dump()=...";
    ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
    ESMC_LogWrite(this->type_storage.dump().c_str(), ESMC_LOGMSG_DEBUG);
#endif

  bool is_32bit = false;
  json j = json::object();
  try {
    j["isDirty"] = this->isDirty();
    j["key"] = json::value_t::null;
    const json *sp = &(this->getStorageRef());
    try {
      json::json_pointer jp = this->formatKey(key);
      update_json_pointer(this->getStorageRef(), &sp, jp, recursive);

      // Find out if the output data is 32-bit
      is_32bit = retrieve_32bit_flag(this->getTypeStorage(), jp, recursive);
    }
    ESMC_CATCH_ERRPASSTHRU

    // Handle finding by index for arrays and objects -------------------------

    if (idx) {
      if (sp->is_array()) {
        try {
          sp = &(sp->at(*idx));
        }
        catch (std::out_of_range &e) {
          ESMC_CHECK_RC("ESMC_RC_ARG_OUTOFRANGE", ESMC_RC_ARG_OUTOFRANGE, e.what())
        }
      } else if (sp->is_object()) {
        json::iterator it = find_by_index(const_cast<json&>(*sp), (std::size_t)(*idx), recursive, attr_compliance);
        j["key"] = it.key();
        sp = &(it.value());

        // Find out if the output data is 32-bit
        try {
          json::json_pointer jp = this->formatKey(j["key"]);
          is_32bit = retrieve_32bit_flag(this->getTypeStorage(), jp, true);
        }
        ESMC_CATCH_ERRPASSTHRU

      } else {
        std::string msg = "'idx' only supported for JSON arrays or objects";
        ESMC_CHECK_RC("ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD, msg);
      }
    }

    // Counting ---------------------------------------------------------------

    const json &sk = *sp;
    auto sk_size = sk.size();
    j["size"] = sk_size;
    if (sk.is_object() && attr_compliance) {
      count_map_t counts = create_json_attribute_count_map();
      update_json_attribute_count_map(counts, sk, true);
      // Bring over information from the count map
      for (std::pair<std::string, int> element : counts)
      {
        j[element.first] = element.second;
      }
    }

    // Type inquire -----------------------------------------------------------

    j["ESMC_TypeKind_Flag"] = json_type_to_esmf_typekind(sk, true, is_32bit);
    std::string json_typename;
    bool is_array = false;
    if (sk.is_array()) {
      is_array = true;
      if (sk_size == 0) {
        json j_null;
        json_typename = j_null.type_name();
      }
      else {
        const json &e = sk[0];
        json_typename = e.type_name();
      }
    } else {
      json_typename = sk.type_name();
    }
    j["jsonType"] = json_typename;
    j["isArray"] = is_array;
    j["isStructured"] = sk.is_structured();
    j["isNull"] = sk.is_null();
  }
  ESMF_CATCH_INFO

#if 0
  msg = prefix + "j.dump()=...";
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  ESMC_LogWrite(j.dump().c_str(), ESMC_LOGMSG_DEBUG);
#endif

  return j;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::parse()"
void Info::parse(key_t& input) {
  // Exceptions:  ESMCI:esmc_error

  try {
    this->getStorageRefWritable() = json::parse(input);
    check_init_from_json(this->getStorageRef());
  }
  ESMF_CATCH_INFO
  return;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::parse_with_type_storage()"
void Info::parse_with_type_storage(key_t& input) {
  // Exceptions:  ESMCI:esmc_error
  // Test: test_dump_and_parse_with_type_storage

  try {
    this->parse(input);
    if (this->hasKey("_esmf_info_type_storage", false)) {
      this->getTypeStorageWritable() = this->getStorageRef()["_esmf_info_type_storage"];
      this->erase("", "_esmf_info_type_storage");
    }
  }
  ESMF_CATCH_INFO
  return;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::isNull()"
bool Info::isNull(key_t &key) const {
  bool ret;
  try {
    json::json_pointer jp = this->formatKey(key);
    try {
      ret = this->getStorageRef().at(jp).is_null();
    }
    ESMF_INFO_CATCH_JSON
  }
  ESMC_CATCH_ERRPASSTHRU
  return ret;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::serialize()"
void Info::serialize(char *buffer, int *length, int *offset, ESMC_InquireFlag inquireflag) {
  // Test: testSerializeDeserialize, testSerializeDeserialize2
  // Exceptions:  ESMCI:esmc_error
  std::string infobuffer;
  try {
    infobuffer = this->dump_with_type_storage();
  }
  ESMC_CATCH_ERRPASSTHRU
  alignOffset(*offset);
  // If this is not an inquire operation, transfer the string info dump
  // into the serialization buffer. Update the offset in the process.
  int n = (int) infobuffer.length();
  if (inquireflag == ESMF_NOINQUIRE) {
    int *ip = (int *)(buffer + *offset);
    *ip = n;
//    int *ibuffer = reinterpret_cast<int*>(buffer);
//    ibuffer[*offset] = n;
  }
  // Need 32 bits (4 bytes) to store the length of the string buffer for a
  // later deserialize.
  (*offset) += sizeof(int);
  // Adjust the offset for the length of the string representation. If not
  // inquiring, also adjust the offset.
  for (int ii=0; ii<n; ++ii) {
    if (inquireflag == ESMF_NOINQUIRE) { buffer[*offset] = infobuffer[ii]; }
    (*offset)++;
  }
  alignOffset(*offset);
  return;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::set_32bit_type_storage()"
void Info::set_32bit_type_storage(key_t &key, bool flag, const key_t * const pkey) {
  // Test: test_set_32bit_type_storage

  if (this->type_storage.is_null()) {
    this->type_storage = json::object();
  }
  try {
    json *jobject = nullptr;
    if (pkey) {
      // Set the target JSON container to the parent key location. The parent key
      // location must be an object to proceed.
      try {
        const json::json_pointer jpkey_parent = this->formatKey(*pkey);
        bool has_pkey = has_key_json(this->type_storage, jpkey_parent, true);
        if (!has_pkey) {
          this->type_storage[jpkey_parent] = json::object();
        }
        try {
          update_json_pointer(this->type_storage, &jobject, jpkey_parent, true);
          ESMC_CHECK_NULLPTR(jobject)
          check_is_object(*jobject);
        }
        ESMF_INFO_CATCH_JSON
      }
      ESMC_CATCH_ERRPASSTHRU
    } else {
      jobject = &(this->type_storage);
    }
    try {
      const json::json_pointer jpkey = this->formatKey(key);
      try {
        (*jobject)[jpkey] = flag;
      }
      ESMF_INFO_CATCH_JSON
    }
    ESMC_CATCH_ERRPASSTHRU
  }
  ESMF_CATCH_INFO

}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::set(<json>)"
void Info::set(key_t &key, json &&j, bool force, const int *index, const key_t * const pkey) {
  // Test:
  // Notes: parent key (pkey) must exist in the map

#if 0
  std::string prefix = std::string(ESMC_METHOD) + ": ";
  std::string msg;
  msg = prefix + "key=" + key;
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  msg = prefix + "j.dump()=" + j.dump();
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  msg = prefix + "force=" + std::to_string(force);
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  if (index) {
    msg = prefix + "*index=" + std::to_string(*index);
  } else {
    msg = prefix + "index=nullptr";
  }
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  if (pkey) {
    msg = prefix + "*pkey=" + *pkey;
  } else {
    msg = prefix + "pkey=nullptr";
  }
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  msg = prefix + "this->dump()=...";
  ESMC_LogWrite(msg.c_str(), ESMC_LOGMSG_DEBUG);
  ESMC_LogWrite(this->dump().c_str(), ESMC_LOGMSG_DEBUG);
#endif

  try {
    json *jobject = nullptr;
    if (pkey) {
      // Set the target JSON container to the parent key location. The parent key
      // location must be an object to proceed.
      try {
        const json::json_pointer jpkey_parent = this->formatKey(*pkey);
        bool has_pkey = this->hasKey(jpkey_parent, true);
        if (!has_pkey) {
          this->getStorageRefWritable()[jpkey_parent] = json::object();
        }
        try {
          update_json_pointer(this->getStorageRefWritable(), &jobject, jpkey_parent, true);
          ESMC_CHECK_NULLPTR(jobject)
          check_is_object(*jobject);
        }
        ESMF_INFO_CATCH_JSON
      }
      ESMC_CATCH_ERRPASSTHRU
    } else {
      jobject = &(this->getStorageRefWritable());
    }

    bool has_key = true;  // Safer to assume the key exists
    try {
      const json::json_pointer jpkey = this->formatKey(key);
      // Only check for the key's existence if there is no index. If an index is
      // provided, then the key must exist to set it.
      if (!index) {
        try {
          has_key = has_key_json(*jobject, jpkey, false);
          if (!force && has_key) {
            std::string msg = "Key \'" + std::string(jpkey) +
                              "\' already in map and force=false.";
            ESMC_CHECK_RC("ESMC_RC_CANNOT_SET", ESMC_RC_CANNOT_SET, msg)
          }
        }
        ESMC_CATCH_ERRPASSTHRU
      }
      if (index) {
        // Find the target array when setting by index.
        json *jarrayp = nullptr;
        try {
          update_json_pointer(*jobject, &jarrayp, jpkey, false);
          ESMC_CHECK_NULLPTR(jarrayp)
        }
        ESMF_INFO_CATCH_JSON
        if (!jarrayp->is_array()) {
          const std::string msg = "When setting with an index, the target must be array";
          ESMC_CHECK_RC("ESMC_RC_OBJ_BAD", ESMC_RC_OBJ_BAD, msg);
        }
        json::array_t &jarray = jarrayp->get_ref<json::array_t&>();
        if (jarray.size() > 0) {
          json::value_t jarray_type = json_array_type(jarray);
          if (!j.is_null() && jarray_type != json::value_t::null && jarray_type != j.type()) {
            const std::string msg = "Target JSON array for index has a different type. ESMF JSON arrays used in Info are type safe";
            ESMC_CHECK_RC("ESMC_RC_OBJ_BAD", ESMC_RC_OBJ_BAD, msg);
          }
        }
        try {
          jarray.at(*index) = std::move(j);
        }
        catch (std::out_of_range &exc) {
          ESMC_CHECK_RC("ESMC_RC_ARG_OUTOFRANGE", ESMC_RC_ARG_OUTOFRANGE,
                            std::string(exc.what()));
        }
      } else {
        if (!j.is_null() && has_key) {
          try {
            handleJSONTypeCheck(key, jobject->at(jpkey), j);
          }
          ESMC_CATCH_ERRPASSTHRU
        }
        try {
          (*jobject)[jpkey] = std::move(j);
        }
        ESMF_INFO_CATCH_JSON
      }
    }
    ESMC_CATCH_ERRPASSTHRU

  }
  ESMF_CATCH_INFO
  this->dirty = true;

#if 0
  std::string msg2;
  std::string prefix2 = std::string(ESMC_METHOD) + ": ";
  msg2 = prefix2 + "this->dump() <exiting>=...";
  ESMC_LogWrite(msg2.c_str(), ESMC_LOGMSG_DEBUG);
  ESMC_LogWrite(this->dump().c_str(), ESMC_LOGMSG_DEBUG);
#endif
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::set(<Info>)"
void Info::set(key_t &key, const ESMCI::Info &info, bool force, const key_t * const pkey) {
  // Test: testSetGet
  try {
    // Copy "info" storage into new object
    json j = info.getStorageRef();
    // Move the contents of the copied "info" storage into this object.
    this->set(key, std::move(j), force, nullptr, pkey);
    // Need to move over the type storage as well.
    ESMCI::Info type_storage;
    type_storage.set(key, info.getTypeStorage(), force, nullptr, pkey);
    this->getTypeStorageWritable() = std::move(type_storage.getStorageRefWritable());
  }
  ESMF_CATCH_INFO
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::setNull()"
void Info::setNull(key_t &key, bool force, const int *index, const key_t * const pkey) {
  try {
    json j = json::value_t::null;
    this->set(key, std::move(j), force, index, pkey);
  }
  ESMF_CATCH_INFO
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::set(<scalar>)"
template <typename T>
void Info::set(key_t &key, T value, bool force, const int *index, const key_t * const pkey) {
  // Exceptions:  ESMCI:esmc_error
  try {
    json j = value;
    this->set(key, std::move(j), force, index, pkey);
  }
  ESMF_CATCH_INFO
  this->dirty = true;
};
template void Info::set<float>(key_t&, float, bool, const int*, const key_t * const);
template void Info::set<double>(key_t&, double, bool, const int*, const key_t * const);
template void Info::set<int>(key_t&, int, bool, const int*, const key_t * const);
template void Info::set<long int>(key_t&, long int, bool, const int*, const key_t * const);
template void Info::set<std::string>(key_t&, std::string, bool, const int*, const key_t * const);
template void Info::set<bool>(key_t&, bool, bool, const int*, const key_t * const);

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::set(<array>)"
template <typename T>
void Info::set(key_t &key, T *values, int count, bool force, const key_t * const pkey) {
  // Exceptions:  ESMCI:esmc_error
  assert(count >= 0);
  try {
    json::array_t jarray = json::array();
    if (values) {
      // If values are not null, transfer said values into the JSON array.
      jarray.reserve(count);
      for (auto ii = 0; ii < count; ii++) {
        jarray.push_back(values[ii]);
      }
    } else {
      // If there are no values provided, reserve the space for future
      // setting by index.
      jarray.resize(count);
    }
    json j = std::move(jarray);
    int *dummy_index = nullptr;
    this->set(key, std::move(j), force, dummy_index, pkey);
  }
  ESMF_CATCH_INFO
};
template void Info::set<float>(key_t&, float*, int, bool, const key_t * const);
template void Info::set<double>(key_t&, double*, int, bool, const key_t * const);
template void Info::set<int>(key_t&, int*, int, bool, const key_t * const);
template void Info::set<long int>(key_t&, long int*, int, bool, const key_t * const);
template void Info::set<bool>(key_t&, bool*, int, bool, const key_t * const);
template void Info::set<std::vector<std::string>>(key_t&, std::vector<std::string>*, int, bool, const key_t * const);

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::update()"
void Info::update(const Info &info) {
  try {
    this->getStorageRefWritable().update(info.getStorageRef());
    this->getTypeStorageWritable().update(info.getTypeStorage());
  }
  ESMF_INFO_CATCH_JSON;
  this->dirty = true;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::update_for_attribute()"
void do_update_for_attribute(json &to_update, const json &new_contents, bool overwrite) {
  check_is_object(to_update);
  check_is_object(new_contents);
  // Loop over the key/values in the root object
  for (json::const_iterator nc=new_contents.cbegin(); nc!=new_contents.cend(); nc++) {
    const json *location_nc = nullptr;
    json *location_tu = nullptr;
    // Check if the new content key is in the object to update
    bool is_in = isIn(nc.key(), to_update);
    if (is_in) {
      // If the new content value is an object, we descend into it for the
      // recursive update.
      if (nc.value().is_object()) {
        do_update_for_attribute(to_update.at(nc.key()), nc.value(), overwrite);
      } else {
        // Otherwise we replace the value if it exists provided we are overwriting.
        if (overwrite) { to_update[nc.key()] = nc.value(); }
      }
    } else {
      // Insert the new key into the map.
      to_update[nc.key()] = nc.value();
    }
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "Info::update_for_attribute()"
void Info::update_for_attribute(const Info &info, bool overwrite) {
  // Test: test_update_for_attribute
  // Throws: esmc_error
  try {
    const json &new_contents = info.getStorageRef();
    json &to_update = this->getStorageRefWritable();
    do_update_for_attribute(to_update, new_contents, overwrite);

    // Update the type storage
    const json &new_type_contents = info.getTypeStorage();
    json &to_update_types = this->getTypeStorageWritable();
    do_update_for_attribute(to_update_types, new_type_contents, overwrite);
  }
  ESMF_INFO_CATCH_JSON;
  // Data is considered not dirty after the update since this is primarily used
  // by StateReconcile.
  this->dirty = false;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  Info::ESMC_Print - Print the {\tt Info} contents
//
// !INTERFACE:
  int Info::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
    bool tofile,            // stream to stdout or file
    const char *filename,   // filename
    bool append) const {    // append or start new
//
// !DESCRIPTION:
//     Print the contents of an {\tt Attribute} object
//
//EOPI
    int strsize=4*ESMF_MAXSTR;
    char msgbuf[strsize];
    std::ofstream fp;
    int esmc_rc = ESMF_SUCCESS;

    if (tofile) {
      sprintf(msgbuf, "%s\n", filename);
      // open file for writing and append to previous contents
      if (append)
        fp.open(msgbuf, std::ofstream::out | std::ofstream::app);
        // open file for writing and throw away previous contents
      else
        fp.open(msgbuf, std::ofstream::out | std::ofstream::trunc);
    }

    try {
      if (tofile)
        fp << this->dump(2);
      else
        printf("%s", this->dump(2).c_str());
    } catch (ESMCI::esmc_error &exc) {
      esmc_rc = exc.getReturnCode();
      ESMC_LogDefault.MsgFoundError(esmc_rc, exc.what(), ESMC_CONTEXT, nullptr);
      return esmc_rc;
    }

    if (tofile)
      fp.close();
    else
      fflush (stdout);

    return esmc_rc;

  }  // end ESMC_Print

//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "PackageFactory::getOrCreateJSON()"
json PackageFactory::getOrCreateJSON(key_t& key, key_t& uri) {
  try {
    json ret = this->cache.at(key);
    return ret;
  } catch (json::out_of_range& e) {
    std::string localuri;
    if (uri == "") {
      localuri = this->uris.value(key, uri);
    } else {
      localuri = uri;
    }
    std::ifstream i(localuri, std::ifstream::in);
    if (!i.good()){
      std::string errmsg = "File location is bad for key '" + key + "': " + localuri;
      ESMC_CHECK_RC("ESMC_RC_FILE_READ", ESMC_RC_FILE_READ, errmsg);
    }
    json j;
    try {
      i >> j;
    } catch (json::parse_error& e) {
      ESMF_INFO_THROW_JSON(e, "ESMC_RC_FILE_READ", ESMC_RC_FILE_READ);
    }
    i.close();
    this->cache[key] = move(j);
    return this->getOrCreateJSON(key);
  }
}

//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "broadcastInfo()"
void broadcastInfo(ESMCI::Info* info, int rootPet, const ESMCI::VM &vm) {
  // Exceptions:  ESMCI:esmc_error
  int localPet = vm.getLocalPet();
  std::size_t target_size = 0;  // Size of serialized info storage
  std::string target;  // Serialize storage buffer

  if (localPet == rootPet) {
    // If this is the root, serialize the info storage to std::string
    try {
      target = info->dump_with_type_storage();
    }
    ESMC_CATCH_ERRPASSTHRU
    target_size = target.size();
  }
  // Broadcast size of the string buffer holding the serialized info.
  // Used for allocating destination string buffers on receiving PETs.
  int esmc_rc = const_cast<ESMCI::VM&>(vm).broadcast(&target_size, sizeof(target_size), rootPet);
  ESMC_CHECK_RC("", esmc_rc, ESMCI_ERR_PASSTHRU);
  std::string target_received(target_size, '\0');  // Allocate receive buffer
  if (localPet == rootPet) {
    // If this is root, just move the data to the receive buffer with no copy.
    target_received = std::move(target);
  }
  // Broadcast the string buffer
  esmc_rc = const_cast<ESMCI::VM&>(vm).broadcast(&target_received[0], target_size, rootPet);
  ESMC_CHECK_RC("", esmc_rc, ESMCI_ERR_PASSTHRU);
  if (localPet != rootPet) {
    // If not root, then parse the incoming string buffer into attribute storage.
    try {
      info->parse_with_type_storage(target_received);
    }
    ESMC_CATCH_ERRPASSTHRU
  }
  return;
}

}  // namespace ESMCI
