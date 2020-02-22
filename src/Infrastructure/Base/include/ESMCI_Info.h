// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Info C++ include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMCI_INFO_H
#define ESMCI_INFO_H

//-----------------------------------------------------------------------------

#include <vector>
#include <fstream>

#include "ESMCI_Util.h"
#include "json.hpp"

using json = nlohmann::json;  // Convenience rename for JSON namespace.

// Standard ESMF check error macros
#define ESMF_INFO_CHECKRC(name_rc, actual_rc, msg) {\
  if (actual_rc != ESMF_SUCCESS) {\
    ESMCI::esmf_info_error local_macro_error(name_rc, actual_rc, msg); \
    if (ESMC_LogDefault.MsgFoundError(actual_rc, local_macro_error.what(), ESMC_CONTEXT, nullptr)) \
      throw(local_macro_error);}}

#define ESMF_INFO_THROW_JSON(json_exc, name_rc, actual_rc) {\
  ESMC_LogDefault.MsgFoundError(actual_rc, json_exc.what(), ESMC_CONTEXT, nullptr); \
  throw(ESMCI::esmf_info_error(name_rc, actual_rc, json_exc.what()));}

#define ESMF_INFO_ERRPASSTHRU(exc_info) {\
  ESMC_LogDefault.MsgFoundError(exc_info.getReturnCode(), exc_info.what(), ESMC_CONTEXT, nullptr); \
  throw(exc_info);}

#define ESMF_INFO_CATCH_ERRPASSTHRU \
  catch (ESMCI::esmf_info_error &exc_info) {ESMF_INFO_ERRPASSTHRU(exc_info)}

#define ESMF_INFO_CATCH_ISOC \
  catch (ESMCI::esmf_info_error &exc_info) {\
    ESMC_LogDefault.MsgFoundError(exc_info.getReturnCode(), exc_info.what(), ESMC_CONTEXT, nullptr); \
    esmf_rc = exc_info.getReturnCode();} \
  catch(...) {\
    std::string msg;\
  if (esmf_rc == ESMF_SUCCESS) {\
    msg = "Unhandled throw and return code is ESMF_SUCCESS. Changing return code to ESMF_FAILURE";\
    esmf_rc = ESMF_FAILURE;} \
  else {\
    msg = "Unhandled throw";}\
  ESMC_LogDefault.MsgFoundError(esmf_rc, msg, ESMC_CONTEXT, nullptr);}

#define ESMF_INFO_CATCH_JSON \
  catch (json::out_of_range &e) {\
    ESMF_INFO_THROW_JSON(e, "ESMC_RC_NOT_FOUND", ESMC_RC_NOT_FOUND);} \
  catch (json::type_error &e) {\
    ESMF_INFO_THROW_JSON(e, "ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD);} \
  catch (json::parse_error &exc_json) {\
    ESMF_INFO_THROW_JSON(exc_json, "ESMC_RC_ARG_BAD", ESMC_RC_ARG_BAD);}

#define ESMF_CATCH_INFO \
  ESMF_INFO_CATCH_JSON \
  catch (ESMCI::esmf_info_error &exc_info) {\
    ESMF_INFO_ERRPASSTHRU(exc_info);} \
  catch (...) {\
    ESMF_INFO_CHECKRC("ESMF_FAILURE", ESMF_FAILURE, "Unhandled throw");}

#define ESMF_INFO_CHECKINIT(info_to_check, esmf_rc_to_return) \
  if (!info_to_check) { \
    ESMC_LogDefault.MsgFoundError(ESMF_RC_OBJ_NOT_CREATED, "Info pointer is null. Object has not been created appropriately. Was ESMF_InfoCreate used?", ESMC_CONTEXT, &esmf_rc_to_return); \
    return;}

#define ESMF_INFO_CHECK_NULLPTR(target) \
  if (!target) { \
    ESMF_INFO_CHECKRC("ESMF_RC_ARG_BAD", ESMF_RC_ARG_BAD, "Pointer may not be null") \
  } \

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  Info
//
// !DESCRIPTION:
// The code in this file implements the Info defined type
// and methods.
//
//-----------------------------------------------------------------------------
//
// !USES:

namespace ESMCI {

class VM;

typedef const std::string key_t;
typedef std::vector<json> const *vecjson_t;
typedef json::array_t const *arrjson_t;
typedef std::unordered_map<std::string, std::size_t> count_map_t;

enum ESMC_ISOCType {C_INT, C_LONG, C_FLOAT, C_DOUBLE, C_CHAR};

//-----------------------------------------------------------------------------

class esmf_info_error : public std::exception
{
public:
  esmf_info_error(key_t &code_name, int esmf_rc, key_t &msg);

  key_t getCodeName() {return this->code_name;}

  int getReturnCode() {return this->esmf_rc;}

  const char* what() const noexcept {return this->msg.c_str();}

private:
  std::string msg;
  int esmf_rc;
  key_t code_name;
};

//-----------------------------------------------------------------------------

void alignOffset(int &offset);
std::size_t get_attpack_count(const json &j);
json::iterator find_by_index(json &j, std::size_t index, bool recursive, bool attr_compliance, std::size_t *index_current = nullptr, bool *found = nullptr);
void update_json_pointer(const json &j, json const **jdp, const json::json_pointer &key, bool recursive);
count_map_t create_json_attribute_count_map(void);
void update_json_attribute_count_map(count_map_t &counts, const json &j, bool first);
bool isIn(key_t& target, const std::vector<std::string>& container);
bool isIn(const std::vector<std::string>& target, const std::vector<std::string>& container);
bool isIn(key_t& target, const json& j);
ESMC_TypeKind_Flag json_type_to_esmf_typekind(const json &j, const bool allow_array) noexcept;

//-----------------------------------------------------------------------------

class Info {

private:
  bool dirty = false;
  json storage;  // JSON object store for keys/values managed by this instance

protected:
  virtual void init(void) {this->storage = json::object();}

public:
  Info(void) {this->init();}
//  Info(void) = default;  // Default constructor
  virtual ~Info(void) = default;  // Default destructor
  Info(Info&&) = delete; // Move constructor
  Info(const Info&) = delete; // Copy constructor
  Info&operator=(const Info&) = delete; // Copy assignment
  Info&operator=(Info&&) = delete; // Move assignment

  explicit Info(const json& storage); // Copy constructor from JSON
  explicit Info(json&& storage); // Move constructor from JSON
  explicit Info(key_t& input); // Constructor from string

  std::string dump(void) const;
  std::string dump(int indent) const;

  void erase(key_t& key, key_t& keyChild, bool recursive = false);

  static json::json_pointer formatKey(key_t &key);

  //---------------------------------------------------------------------------
  template <typename T>
  T get(key_t &key, const T *def = nullptr, const int *index = nullptr, bool recursive = false, std::string *ikey = nullptr) const;

  void get(ESMCI::Info &info, key_t &key) const;
  //---------------------------------------------------------------------------

  std::size_t getCountPack(void) const {return get_attpack_count(this->getStorageRef());}

  template <typename T>
  std::vector<T> getvec(key_t &key, bool recursive = false) const;

  virtual const json& getStorageRef(void) const { return this->storage; }
  virtual json& getStorageRefWritable(void) { return this->storage; }

  json const * getPointer(key_t &key, bool recursive = false) const;

  bool hasKey(key_t &key, bool isptr = false, bool recursive = false) const;
  bool hasKey(const json::json_pointer &jp, bool recursive = false) const;

  json inquire(key_t& key, bool recursive = false, const int *idx = nullptr,
    bool attr_compliance = false) const;

  bool isDirty() const {return this->dirty;}
  void setDirty(bool flag) {this->dirty = flag;}

  bool isSetNull(key_t &key) const;

  void parse(key_t &input);

  void deserialize(char *buffer, int *offset);

  void serialize(char *buffer, int *length, int *offset,
    ESMC_InquireFlag inquireflag);

  void set(key_t &key, json &&j, bool force, const int *index = nullptr,
    const key_t * const pkey = nullptr);
  void set(key_t &key, const ESMCI::Info &info, bool force,
    const key_t * const pkey = nullptr);
  template <typename T>
  void set(key_t &key, T value, bool force, const int *index = nullptr,
    const key_t * const pkey = nullptr);
  template <typename T>
  void set(key_t &key, T *values, int count, bool force,
    const key_t * const pkey = nullptr);

  void setNull(key_t &key, bool force, const int *index = nullptr,
           const key_t * const pkey = nullptr);

  void update(const Info &info);

  int ESMC_Print(bool tofile, const char *filename, bool append) const;
};

//-----------------------------------------------------------------------------

void broadcastInfo(ESMCI::Info* info, int rootPet, const ESMCI::VM &vm);

//-----------------------------------------------------------------------------

// NOTE: This class left here because it has some useful boilerplate for reading
// JSON data from file. It is not used in any implementation.
class PackageFactory {
  private:
    json cache = json::object();
    json uris = json::object();
  public:
    PackageFactory(void) = default;  // Default constructor
    ~PackageFactory(void) = default; // Default destructor

    json getOrCreateJSON(key_t &key, key_t &uri = "");
  };

} // namespace

#endif  // ifdef barrier
