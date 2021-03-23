// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//=============================================================================

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <iostream>
#include <limits>

#include "ESMC.h"
#include "ESMC_Test.h"
#include "ESMCI_Info.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"
#include "ESMCI_VM.h"

using namespace ESMCI;
using namespace std;

//=============================================================================
//BOP
// !PROGRAM: ESMC_InfoUTest - Internal Attribute JSON functionality
//
// !DESCRIPTION: Test Info class
//
//EOP
//-----------------------------------------------------------------------------

void finalizeFailure(int& rc, char failMsg[], string msg) {
  rc = ESMF_FAILURE;
  strcpy(failMsg, msg.c_str());
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testbroadcastInfo()"
void testbroadcastInfo(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  ESMCI::VM *vm = ESMCI::VM::getCurrent(&rc);
  ESMC_CHECK_RC("", rc, "Did not get current VM");

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  Info info;

  int rootPet;
  // Use a non-zero root pet for parallel testing
  if (petCount == 1) {
    rootPet = 0;
  } else {
    rootPet = 1;
  }

  int desired = 5;
  if (localPet == rootPet) {
    try {
        info.set("foo", desired, false);
        info.set("maintain_i4", 44, false);
        info.set_32bit_type_storage("maintain_i4", true, nullptr);
    }
    ESMC_CATCH_ERRPASSTHRU
  }

  try {
    ESMCI::broadcastInfo(&info, rootPet, *vm);
  }
  ESMC_CATCH_ERRPASSTHRU

  try {
    int actual = info.get<int>("foo");
    if (actual != desired) {
      return finalizeFailure(rc, failMsg, "Value not broadcast");
    }
    if (info.getTypeStorage().size() == 0) {
      return finalizeFailure(rc, failMsg, "Type storage not broadcast");
    }
    if (!info.getTypeStorage()["maintain_i4"]) {
      return finalizeFailure(rc, failMsg, "Type storage value missing");
    }
    if (info.hasKey("_esmf_info_type_storage")) {
      return finalizeFailure(rc, failMsg, "Type storage should be erased");
    }
  }
  ESMC_CATCH_ERRPASSTHRU
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testConstructor()"
void testConstructor(int& rc, char failMsg[]) {
  Info info;

  // Test constructing from a JSON object instance creates a copy.
  json root;
  int desired = 5;
  root["foo"] = desired;
  Info a(root);
  root["foo"] = 10;

  try {
    auto actual = a.getPointer("/foo");
    if (*actual != desired){
      return finalizeFailure(rc, failMsg, "JSON object changed value");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  root.clear();
  try {
    auto actual2 = a.getPointer("/foo");
    if (*actual2 != desired){
      return finalizeFailure(rc, failMsg, "Clear removed desired value");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  //---------------------------------------------------------------------------
  // Test move constructor

  json src;
  src["foo"] = 112;

  auto srcPtr = src.at("foo").get_ptr<const json::number_integer_t*>();

  Info dst(move(src));

  if (!src.is_null()){
    return finalizeFailure(rc, failMsg, "JSON object not moved");
  }

  try {
    auto actual3 = dst.getPointer("foo")->get_ptr<const json::number_integer_t*>();
    if (*actual3 != 112) {
      return finalizeFailure(rc, failMsg, "Value bad after move");
    }
    if (&*actual3 != &*srcPtr) {
      return finalizeFailure(rc, failMsg, "Pointer addresses not equal after move");
    }
  }
  ESMC_CATCH_ERRPASSTHRU
  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testGet()"
void testGet(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  Info info;

  try {
    info.set("target", 50, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  try {
    auto actual = info.get<int>("target");

    if (actual != 50) {
      return finalizeFailure(rc, failMsg, "Could not get target");
    }

    auto actual_ptr = info.getPointer("target");

    std::size_t addr1 = (std::size_t)&actual;
    std::size_t addr2 = (std::size_t)&*actual_ptr;
    if (addr1 == addr2) {
      return finalizeFailure(rc, failMsg, "Addresses should not be equal");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  try {
    info.get<int>("blah");
    return finalizeFailure(rc, failMsg, "Error not raised for missing key");
  } catch (esmc_error& err) {
    if (err.getReturnCode() != ESMF_RC_ATTR_NOTSET) {
      return finalizeFailure(rc, failMsg, "Wrong error return code");
    }
  }

  // Test get with a default value ============================================

  Info info2;
  rc = ESMF_FAILURE;
  int def = 3000;
  try {
    auto actual2 = info2.get<int>("blah-dee-blah", &def);
    if (actual2 != def) {
      return finalizeFailure(rc, failMsg, "Did not get default value");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  rc = ESMF_SUCCESS;
  return;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testGetObjectIndex()"
void testGetObjectIndex(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  json j = {{"foo1", 1}, {"foo2", 2}, {"foo3", 3}};
  Info info(j);

  int actual;
  std::string actual_key;
  try {
    std::string key = "";
    int index = 1;
    actual = info.get<int>(key, nullptr, &index, false, &actual_key);
  }
  ESMC_CATCH_ERRPASSTHRU

  // Test object index retrieval with key value
  if (actual != 2 && actual_key != "foo2") {
    return finalizeFailure(rc, failMsg, "Did not get object index");
  }

  // Test wrong type
  try {
    std::string key = "foo3";
    int index = 1;
    actual = info.get<int>(key, nullptr, &index);
    return finalizeFailure(rc, failMsg, "Did not catch wrong type");
  }
  catch (ESMCI::esmc_error &exc_esmf) {
    rc = ESMF_SUCCESS;
  }

  // Test index out of range
  try {
    std::string key = "";
    int index = 11;
    actual = info.get<int>(key, nullptr, &index);
    return finalizeFailure(rc, failMsg, "Did not catch out of range");
  }
  catch (ESMCI::esmc_error &exc_esmf) {
    rc = ESMF_SUCCESS;
  }
}

#undef ESMC_METHOD
#define ESMC_METHOD "testSetGetIndex()"
void testSetGetIndex(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  Info info;
  int n = 25;
  double values[n];
  for (int ii = 0; ii < n; ++ii) {
    values[ii] = (double)ii / (double)n;
  }
  std::string key = "the-key";
  try {
    info.set<double>(key, nullptr, n, false);
    for (int ii = 0; ii < n; ++ii) {
      info.set<double>(key, values[ii], false, &ii);
    }
  }
  catch (ESMCI::esmc_error &exc_esmf) {
    ESMC_ERRPASSTHRU(exc_esmf);
  }
  double diff;
  double actual;
  for (int ii = 0; ii < n; ++ii) {
    try {
      actual = info.get<double>(key, nullptr, &ii);
    }
    catch (ESMCI::esmc_error &exc_esmf) {
      ESMC_ERRPASSTHRU(exc_esmf);
    }
    diff = std::abs(values[ii] - actual);
    if (diff >= 1e-16) {
      return finalizeFailure(rc, failMsg, "Values are not equal");
    }
  }

  // Test vector std::out_of_range is handled =================================

  int jj = 1000;
  try {
    actual = info.get<double>(key, nullptr, &jj);
  }
  catch (ESMCI::esmc_error &exc_esmf) {
    if (exc_esmf.getReturnCode() != ESMC_RC_ARG_OUTOFRANGE) {
      return finalizeFailure(rc, failMsg, "Did not handle out_of_range");
    } else {
      rc = ESMF_SUCCESS;
    }
  }

  // Test a key has to exist when using an index ==============================

  std::string not_there = "not_there";
  int noidx = 5;
  try {
    info.set<int>(not_there, 111, false, &noidx);
  }
  catch (ESMCI::esmc_error &exc_esmf) {
    if (exc_esmf.getReturnCode() == ESMC_RC_NOT_FOUND) {
      rc = ESMF_SUCCESS;
    } else {
      return finalizeFailure(rc, failMsg, "Key must exist with an index");
    }
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testHasKey()"
void testHasKey(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  Info info;

  try {
    info.set("/neverEver", 13, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  bool actual = info.hasKey("/hello", true);
  if (actual){
    return finalizeFailure(rc, failMsg, "Key is not present");
  }

  bool actual2 = info.hasKey("/neverEver", true);
  if (!actual2){
    return finalizeFailure(rc, failMsg, "Key is present");
  }

  // Test not using a JSON pointer.
  bool actual3 = info.hasKey("neverEver");
  if (!actual3){
    return finalizeFailure(rc, failMsg, "Key is present with non-pointer");
  }

  rc = ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testErase()"
void testErase(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  Info info;

  string key = "/something/nested";
  try {
    info.set(key, 10, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  try {
    info.erase("/something", "nested");
  }
  ESMC_CATCH_ERRPASSTHRU

  const json &storage = info.getStorageRef();
  const json &actual = storage["something"];
  if (actual.find("nested") != actual.end()){
    return finalizeFailure(rc, failMsg, "Nested item not deleted");
  }

  //---------------------------------------------------------------------------
  // Test errors handled with bad key combinations and erase.

  rc = ESMF_FAILURE;
  bool failed = true;
  try {
    info.erase("/nothing", "nested");
    failed = true;
  }
  catch (esmc_error &err) {
    if (err.getReturnCode() == ESMC_RC_NOT_FOUND){
      failed = false;
    }
  }
  if (failed) {
    return finalizeFailure(rc, failMsg, "Error not handled for missing parent");
  }

  rc = ESMF_FAILURE;
  try {
    info.erase("/something", "underground");
    failed = true;
  }
  catch (esmc_error &err){
    if (err.getReturnCode() == ESMC_RC_NOT_FOUND){
      failed = false;
    }
  }
  if (failed) {
    return finalizeFailure(rc, failMsg, "Error not handled for missing child");
  }

  //---------------------------------------------------------------------------
  // Test an erase with a 32-bit flag

  try {
    Info info2;
    info2.set<int>("/ESMF/General/foo", 33, false);
    info2.getTypeStorageWritable()[json::json_pointer("/ESMF/General/foo")] = true;
    info2.erase("/ESMF/General", "foo");
    Info info2_type_storage(info2.getTypeStorage());
    if (info2_type_storage.hasKey("/ESMF/General/foo")) {
      return finalizeFailure(rc, failMsg, "key still present in type storage after erase");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  rc = ESMF_SUCCESS;
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testFormatKey()"
void testFormatKey(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  Info info;
  bool has_key;
  try {
    has_key = info.hasKey("/foo/~", true);
  }
  catch (ESMCI::esmc_error &exc_esmf) {
    if (exc_esmf.getReturnCode() != ESMC_RC_ARG_BAD) {
      return finalizeFailure(rc, failMsg, "Did not handle JSON parse error");
    }
  }
  rc = ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testGetInfoObject()"
void testGetInfoObject(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  json j = {{"foo1", 1}, {"foo2", {{"nest1", 55}}}, {"foo3", 3}};
  Info info(j);
  Info actual;
  try {
    info.get(actual, "foo2");
  }
  ESMF_CATCH_INFO
  int desired = j["foo2"]["nest1"];
  if (actual.get<int>("nest1") != desired) {
    return finalizeFailure(rc, failMsg, "Did not get Info object");
  }
  rc = ESMF_SUCCESS;
}

#undef ESMC_METHOD
#define ESMC_METHOD "test_get_attpack_count()"
void test_get_attpack_count(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  json j;
  j["ESMF"]["General"]["foo"] = 5;
  j["ESMF"]["General"]["fool"] = 55;
  j["NUOPC"]["General"]["foo2"] = 3;
  j["NUOPC"]["General"]["fool2"] = 33;
  std::size_t c = get_attpack_count(j);
  if (c!=2) {return finalizeFailure(rc, failMsg, "AttPack count incorrect");}

  json j2;
  j2["what"] = "nothing";
  std::size_t c2 = get_attpack_count(j2);
  if (c2!=0) {return finalizeFailure(rc, failMsg, "AttPack count incorrect");}
  rc = ESMF_SUCCESS;
}

#undef ESMC_METHOD
#define ESMC_METHOD "test_update_json_attribute_count_map()"
void test_update_json_attribute_count_map(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  json j;
  j["ESMF"]["General"]["foo"] = 5;
  j["ESMF"]["General"]["fool"] = 55;
  j["ESMF"]["General"]["very"]["specific"]["first"] = "kinda";
  j["ESMF"]["General"]["very"]["specific"]["second"] = "probably";
  j["ESMF"]["General"]["very"]["specific"]["third"] = "maybe";
  j["ESMF"]["FOOBAR"]["foo_instance"] = "certainly";
  j["NUOPC"]["General"]["foo2"] = 3;
  j["NUOPC"]["General"]["fool2"] = 33;

  auto counts = create_json_attribute_count_map();
  update_json_attribute_count_map(counts, j, true);

  if (counts.at("attPackCountTotal") != 4) return finalizeFailure(rc, failMsg, "attPackCountTotal incorrect");
  if (counts.at("attrCountTotal") != 8) return finalizeFailure(rc, failMsg, "attrCountTotal incorrect");
  if (counts.at("attrCount") != 0) return finalizeFailure(rc, failMsg, "attrCount incorrect");
  if (counts.at("attPackCount") != 3) return finalizeFailure(rc, failMsg, "attPackCount incorrect");

  // Test with a nested ESMF AttPack

  json j2 = "{\"ESMF\":{\"Extended\":{\"ESMF\":{\"General\":{\"LongName\":\"Edge pressure tendency\",\"ShortName\":\"field\",\"StandardName\":\"default_standard_name\",\"Units\":\"Pa s-1\"}}}}}"_json;
  auto counts2 = create_json_attribute_count_map();
  update_json_attribute_count_map(counts2, j2, true);

  if (counts2.at("attPackCountTotal") != 2) return finalizeFailure(rc, failMsg, "attPackCountTotal incorrect");
  if (counts2.at("attrCountTotal") != 4) return finalizeFailure(rc, failMsg, "attrCountTotal incorrect");
  if (counts2.at("attrCount") != 0) return finalizeFailure(rc, failMsg, "attrCount incorrect");
  if (counts2.at("attPackCount") != 1) return finalizeFailure(rc, failMsg, "attPackCount incorrect");

  rc = ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testSetGet()"
void testSetGet(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  Info info;

  // Test setting a single value ==============================================

  int value = 10;
  string key = "theKey";
  try {
    info.set(key, value, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  const json& storage = info.getStorageRef();

  if (storage["theKey"] != value){
    return finalizeFailure(rc, failMsg, "Did not set key correctly");
  }

  rc = ESMF_FAILURE;
  try {
    auto actual = info.getPointer(key);

    if (*actual != value){
      return finalizeFailure(rc, failMsg, "Did not get pointer key correctly");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  //---------------------------------------------------------------------------

  int value2 = 33;
  string keyp = "/root/group1/group2";
  rc = ESMF_FAILURE;
  try {
    info.set(keyp, value2, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  if (storage["root"]["group1"]["group2"] != value2){
    return finalizeFailure(rc, failMsg, "Did not set nested key correctly");
  }

  rc = ESMF_FAILURE;
  try {
    auto actual2 = info.getPointer(keyp);
    if (*actual2 != value2){
      return finalizeFailure(rc, failMsg, "Did not get nested key correctly");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  //---------------------------------------------------------------------------

  key = "/twiceSet";
  try {
    info.set(key, 10, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  value = 12;
  try {
    info.set(key, value, true);
  }
  ESMC_CATCH_ERRPASSTHRU

  if (*info.getPointer(key) != value){
    return finalizeFailure(rc, failMsg, "Did not overload existing key correctly");
  }

  // Test with string data type ===============================================

  key = "hello";
  string value3 = "world";
  try {
    info.set(key, value3, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  try {
    string actual3 = info.get<string>(key);
    if (actual3 != value3) {
      return finalizeFailure(rc, failMsg, "Did not get string value");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  // Test using a JSON Pointer with an array ==================================

  std::vector<int> c_vector {1, 2, 3, 4};
  json ja;
  json j_vec(c_vector);
  ja["foo"] = j_vec;
  Info infovec(ja);

  try {
    auto actual4 = infovec.get<int>("/foo/2");
    if (actual4 != c_vector[2]) {
      return finalizeFailure(rc, failMsg, "Did not get array element value");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  // Test with an array pointer ===============================================

  int c_int_arr[4] = {1, 2, 3, 4};
  int count = 4;

  Info info2;

  try {
    info2.set("the-key", c_int_arr, count, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  json::array_t apref = info2.getStorageRef()["the-key"];

  for (auto ii=0; ii<count; ii++) {
    if (apref[ii] != c_int_arr[ii]) {
      return finalizeFailure(rc, failMsg, "Element array not equal");
    }
  }

  // Test modifying internal storage ==========================================

  Info mstore;
  json& jstore = mstore.getStorageRefWritable();
  jstore["i am an int"] = 111;
  if (mstore.get<int>("i am an int") != 111) {
    return finalizeFailure(rc, failMsg, "Did not modify internal storage");
  }

  // Test using a parent key when setting =====================================

  Info pkey_test;
  json& pkey_test_storage = pkey_test.getStorageRefWritable();
  pkey_test_storage["parent"]["storage"] = json::object();
  std::string pkey = "/parent/storage";
  try {
    pkey_test.set("foo", 22, false, nullptr, &pkey);
  }
  ESMC_CATCH_ERRPASSTHRU
  if (pkey_test.get<int>("/parent/storage/foo") != 22) {
    return finalizeFailure(rc, failMsg, "Parent key set failure");
  }

  // Test setting an Info object ==============================================

  Info info_to_set;
  Info info_host;
  int actual;
  try {
    info_to_set.set<int>("a", 55, false);
    info_host.set("hello", info_to_set, false);
    actual = info_host.get<int>("/hello/a");
  }
  ESMC_CATCH_ERRPASSTHRU

  if (actual != 55) {
    return finalizeFailure(rc, failMsg, "Did not set info object");
  }

  rc = ESMF_SUCCESS;
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testSetGetErrorHandling()"
void testSetGetErrorHandling(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  //---------------------------------------------------------------------------
  // Test the ESMF info exception

  esmc_error ae("ESMC_RC_NOT_FOUND", ESMC_RC_NOT_FOUND, "a message");
  const char *actual = ae.what();
  string desired = "Error/Return Code 541 (ESMC_RC_NOT_FOUND) - a message";
  if (actual != desired || ae.getReturnCode() != ESMC_RC_NOT_FOUND) {
    return finalizeFailure(rc, failMsg, "Error string output not correct");
  }

  //---------------------------------------------------------------------------
  // Test trying to get a value that is not in the map or is the wrong type
  // will error.

  Info info;

  bool failed = true;
  string key = "/theKey";
  try {
    auto actual = info.getPointer(key);
  }
  catch (esmc_error& err) {
    if (err.getReturnCode() == ESMC_RC_NOT_FOUND){
      failed = false;
    }
  }

  // Test is expected to fail as we have not added anything at this key.
  if (failed){
    return finalizeFailure(rc, failMsg, "Return code not compliant with get error");
  }

  //---------------------------------------------------------------------------
  // Test setting force to false will error out if the map has already been
  // created.

  string key2 = "/theKey2";
  try {
    info.set(key2, 111, false);
  }
  ESMC_CATCH_ERRPASSTHRU

  failed = true;
  try {
    info.set(key2, 222, false);
  }
  catch (esmc_error &err) {
    if (err.getReturnCode() == ESMC_RC_CANNOT_SET) {
      failed = false;
    }
  }
  if (failed){
    return finalizeFailure(rc, failMsg, "Error not handled with existing key");
  }

  //---------------------------------------------------------------------------
  // Test a malformed key

  failed = true;
  string key3 = "///key";
  try {
    info.set(key3, 111, false);
  }
  catch (esmc_error &err) {
    if (err.getReturnCode() == ESMC_RC_ARG_BAD) {
      failed = false;
    }
  }
  if (failed){
    return finalizeFailure(rc, failMsg, "Key is not parseable");
  }

  //---------------------------------------------------------------------------

  rc = ESMF_SUCCESS;
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testDumpLength()"
void testDumpLength(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  Info info;
  std::string infobuff;
  try {
    infobuff = info.dump();
  }
  ESMC_CATCH_ERRPASSTHRU

  std::size_t sf = infobuff.length();
  try {
    info.set("the_int", 50, false);
    infobuff = info.dump();
  }
  ESMC_CATCH_ERRPASSTHRU

  std::size_t sf2 = infobuff.length();
  if (sf2 < sf) {
    return finalizeFailure(rc, failMsg, "Length too small with int");
  }

  rc = ESMF_SUCCESS;
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testSerializeDeserialize()"
void testSerializeDeserialize(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  Info info;
  try {
    info.set("foo", 16, false);
  }
  catch (esmc_error &e) {
    ESMC_ERRPASSTHRU(e);
  }
  char *null_buffer = nullptr;
  int inquire_length = 0;
  int offset = 0;
  try {
    info.serialize(null_buffer, &inquire_length, &offset, ESMF_INQUIREONLY);
  }
  catch (esmc_error &e) {
    ESMC_ERRPASSTHRU(e);
  }
  if (inquire_length != 0) {
    return finalizeFailure(rc, failMsg, "Should not have adjusted length");
  }
  inquire_length = offset;
  char buffer[inquire_length];
  offset = 0;
  try {
    info.serialize(buffer, &inquire_length, &offset, ESMF_NOINQUIRE);
  }
  catch (esmc_error &e) {
    ESMC_ERRPASSTHRU(e);
  }

  Info deinfo;
  int deoffset = 0;
  try {
    deinfo.deserialize(buffer, &deoffset);
  }
  catch (esmc_error &e) {
    ESMC_ERRPASSTHRU(e);
  }
  if (deoffset != offset) {
    return finalizeFailure(rc, failMsg, "Deserialize offset incorrect");
  }
  if (info.getStorageRef() != deinfo.getStorageRef()) {
    return finalizeFailure(rc, failMsg, "Storage not equal");
  }

  rc = ESMF_SUCCESS;
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testSerializeDeserialize2()"
void testSerializeDeserialize2(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  try {
    Info info(std::string("{\"foo\":16}"));
    Info info2(std::string("{\"foo2\":\"a string\"}"));
    Info info3(std::string("{\"foo3\":3.144}"));
    vector<Info*> infops = {&info, &info2, &info3};
    int inquire_length = 0;
    int offset = 0;
    char *null_buffer = nullptr;
    for (auto element : infops) {
      try {
        element->set_32bit_type_storage("foo", true, nullptr);
        element->serialize(null_buffer, &inquire_length, &offset, ESMF_INQUIREONLY);
      }
      ESMC_CATCH_ERRPASSTHRU
    }
    inquire_length = offset;
    offset = 0;
    char buffer[inquire_length];
    for (auto element : infops) {
      try {
        element->serialize(buffer, &inquire_length, &offset, ESMF_NOINQUIRE);
      }
      ESMC_CATCH_ERRPASSTHRU
    }

    Info infod;
    Info info2d;
    Info info3d;
    vector<Info*> info2ps = {&infod, &info2d, &info3d};
    offset = 0;
    for (auto element : info2ps) {
      try {
        element->deserialize(buffer, &offset);
      }
      ESMC_CATCH_ERRPASSTHRU
    }

    for (std::size_t ii = 0; ii < infops.size(); ++ii) {
      Info *actual = info2ps[ii];
      Info *desired = infops[ii];
      if (actual->getStorageRef() != desired->getStorageRef()) {
        return finalizeFailure(rc, failMsg, "Deserialized incorrect");
      }
      if (actual->getTypeStorage().size() == 0) {
        return finalizeFailure(rc, failMsg, "Type storage not present");
      }
      if (actual->inquire("foo")["ESMF_TYPEKIND"] == ESMC_TYPEKIND_I4) {
        return finalizeFailure(rc, failMsg, "Type storage incorrect");
      }
    }
  }
  ESMC_CATCH_ERRPASSTHRU
  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testInquire()"
void testInquire(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  const bool def_recursive = false;
  const int *def_idx = nullptr;

  json j;
  j["ESMF"]["General"]["n"] = 100;
  j["ESMF"]["General"]["x"] = 1000;
  j["NUOPC"]["General"]["a"] = 111;
  j["NUOPC"]["General"]["b"] = 1111;
  ESMCI::Info info(std::move(j));
  json inq;
  try {
    inq = info.inquire("", def_recursive, def_idx, true);
  }
  ESMC_CATCH_ERRPASSTHRU
  if(inq.at("attPackCount")!=2) {
    return finalizeFailure(rc, failMsg, "Wrong inquire count");
  }

  try {
    inq = info.inquire("/ESMF/General");
  }
  ESMC_CATCH_ERRPASSTHRU
  if(inq.at("size")!=2) {
    return finalizeFailure(rc, failMsg, "Wrong inquire size with key");
  }

  try {
    inq = info.inquire("", true, def_idx, true);
  }
  ESMC_CATCH_ERRPASSTHRU
  if(inq.at("attrCountTotal")!=4) {
    return finalizeFailure(rc, failMsg, "Wrong inquire attribute count with recursive");
  }

  info.getStorageRefWritable()["NUOPC"]["General"]["foobar"] = {3, 4, 5};
  try {
    int idx = 1;
    inq = info.inquire("/NUOPC/General/foobar", false, &idx);
  }
  ESMC_CATCH_ERRPASSTHRU
  if(inq.at("jsonType")!="number") {
    return finalizeFailure(rc, failMsg, "Wrong inquire count with recursive");
  }

  // Test using an object index
  try {
    int idx = 1;
    inq = info.inquire("/ESMF/General", false, &idx);
  }
  ESMC_CATCH_ERRPASSTHRU
  if (inq.at("key") != "x") {
    return finalizeFailure(rc, failMsg, "Did not use object index correctly");
  }

  // Test the size of a character value
  json j2;
  j2["character"] = "name";
  ESMCI::Info info2(j2);
  json inq2;
  try {
    inq2 = info2.inquire("character");
  }
  ESMC_CATCH_ERRPASSTHRU
  if (inq2.at("size") != 1) {
    return finalizeFailure(rc, failMsg, "Wrong size");
  }

  rc = ESMF_SUCCESS;
  return;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testInquire32Bit()"
void testInquire32Bit(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  ESMCI::Info info;

  // Test an I4
  try {
    info.set<int>("/ESMF/General/foo", 45, false);
    info.set_32bit_type_storage("/ESMF/General/foo", true, nullptr);
    json inquire = info.inquire("/ESMF/General/foo");
    if (inquire["ESMC_TypeKind_Flag"] != ESMC_TYPEKIND_I4) {
      return finalizeFailure(rc, failMsg, "Wrong storage type");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  // Test an R4
  try {
    info.set<float>("/ESMF/General/foo2", 33.3, false);
    info.set_32bit_type_storage("/ESMF/General/foo2", true, nullptr);
    json inquire = info.inquire("/ESMF/General/foo2");
    if (inquire["ESMC_TypeKind_Flag"] != ESMC_TYPEKIND_R4) {
      return finalizeFailure(rc, failMsg, "Wrong storage type");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  // Test empty 32-bit mapping
  try {
    info.set<int>("/ESMF/General/foo3", 3, false);
    json inquire = info.inquire("/ESMF/General/foo3");
    if (inquire["ESMC_TypeKind_Flag"] != ESMC_TYPEKIND_I8) {
      return finalizeFailure(rc, failMsg, "Wrong storage type");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  // Test using an index
  try {
    const int idx = 1;
    json inquire = info.inquire("", true, &idx, true);
    if (inquire["ESMC_TypeKind_Flag"] != ESMC_TYPEKIND_R4) {
      return finalizeFailure(rc, failMsg, "Wrong storage type");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  rc = ESMF_SUCCESS;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testUpdate()"
void testUpdate(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  json update_target =  R"( {"color": "red", "price": 17.99} )"_json;
  Info update_target_info(update_target);

  json used_to_update = R"( {"color": "blue", "speed": 100} )"_json;
  Info used_to_update_info(used_to_update);

  const json& lhs = update_target_info.getStorageRef();
  const json& rhs = used_to_update_info.getStorageRef();

  if (lhs == rhs){
    return finalizeFailure(rc, failMsg, "Storage should not be equal");
  }

  rc = ESMF_FAILURE;
  try {
    update_target_info.update(used_to_update_info);
  }
  ESMC_CATCH_ERRPASSTHRU

  const json desired = R"( {"color": "blue", "price": 17.99, "speed": 100} )"_json;

  if (lhs != desired){
    return finalizeFailure(rc, failMsg, "Storage not updated");
  }

  rc = ESMF_SUCCESS;
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_update_json_pointer()"
void test_update_json_pointer(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  bool failed = false;

  json j;
  j["foo"] = 5;
  j["nest"]["twice"]["foobar"] = "bingo";
  j["nest"]["twice"]["an_array"] = {44, 55, 66};
  j["nest"]["once"]["once_again"]["deeper"] = 77;

  //---------------------------------------------------------------------------

  json const *jdp = nullptr;
  json::json_pointer key("/wonderbar");
  bool recursive = false;
  try {
    update_json_pointer(j, &jdp, key, recursive);
    failed = true;
  } catch (json::out_of_range &e) {
    if (jdp) {
      return finalizeFailure(rc, failMsg, "Pointer should be null with missing key");
    }
  }
  if (failed) {
    return finalizeFailure(rc, failMsg, "Did not catch exception");
  }

  //---------------------------------------------------------------------------

  jdp = nullptr;
  json::json_pointer key2("/deeper");
  recursive = true;
  update_json_pointer(j, &jdp, key2, recursive);
  if (&*jdp!=&j["nest"]["once"]["once_again"]["deeper"]) {
    return finalizeFailure(rc, failMsg, "Pointer not updated with recursive");
  }

  //---------------------------------------------------------------------------

  jdp = nullptr;
  json::json_pointer key21("/not there");
  recursive = true;
  try {
    update_json_pointer(j, &jdp, key21, recursive);
    failed = true;
  } catch (json::out_of_range &e) {
    if (jdp) {
      return finalizeFailure(rc, failMsg, "Pointer should be null with missing key & recursive");
    }
  }
  if (failed) {
    return finalizeFailure(rc, failMsg, "Did not catch exception");
  }

  //---------------------------------------------------------------------------

  jdp = nullptr;
  json::json_pointer key3("/nest/twice/an_array");
  recursive = true;
  update_json_pointer(j, &jdp, key3, recursive);
  if (&*jdp!=&j["nest"]["twice"]["an_array"]) {
    return finalizeFailure(rc, failMsg, "Pointer not updated to array");
  }

  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_find_by_index()"
void test_find_by_index(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  json j;
  j["0"] = 0;
  j["1"] = 1;
  j["2"] = 2;
  j["nest"]["3"] = 3;
  j["nest"]["4"] = 4;
  j["nest"]["5"] = 5;

  json::iterator it = find_by_index(j, 1, false, false);
  if (it.key() != "1") {return finalizeFailure(rc, failMsg, "wrong key found for 1");}
  if (it.value() != 1) {return finalizeFailure(rc, failMsg, "wrong index found for 1");}


  it = find_by_index(j, 3, true, false);
  if (it.key() != "nest") {return finalizeFailure(rc, failMsg, "wrong key found for 4");}

  try {
    it = find_by_index(j, 10, true, false);
    return finalizeFailure(rc, failMsg, "did not catch out of range");
  }
  catch (esmc_error &exc) {
    if (exc.getReturnCode() != ESMC_RC_NOT_FOUND) {
      return finalizeFailure(rc, failMsg, "wrong rc");
    }
  }

  json jattrs;
  jattrs["NUOPC"]["General"]["0"] = 0;
  jattrs["NUOPC"]["General"]["1"] = 1;
  jattrs["NUOPC"]["General"]["2"] = 2;
  jattrs["NUOPC"]["Instance"]["3"] = 3;
  jattrs["NUOPC"]["Instance"]["4"] = 4;
  jattrs["NUOPC"]["Instance"]["5"] = 5;

  it = find_by_index(jattrs, 2, true, true);
  if (it.key() != "2") {return finalizeFailure(rc, failMsg, "wrong key found for 2 with attr");}
  if (it.value() != 2) {return finalizeFailure(rc, failMsg, "wrong index found for 2 with attr");}

  it = find_by_index(jattrs, 4, true, true);
  if (it.key() != "4") {return finalizeFailure(rc, failMsg, "wrong key found for 4 with attr");}
  if (it.value() != 4) {return finalizeFailure(rc, failMsg, "wrong index found for 4 with attr");}

  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_implicit_conversion()"
void test_implicit_conversion(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  ESMCI::Info info;

  // Test float to integer.
  try {
    info.set("float", 33.3, false);
    int an_int = info.get<int>("float", nullptr, nullptr, false, nullptr, false);
    return finalizeFailure(rc, failMsg, "did not catch implicit conversion");
  }
  catch (esmc_error &exc) {
    if (exc.getReturnCode() != ESMC_RC_ARG_BAD) {
      return finalizeFailure(rc, failMsg, "wrong rc");
    }
  }

  // Test float to integer with an array.
  try {
    double values[3] = {1, 2, 3};
    info.set("float-array", values, 3, false);
    int index = 1;
    int an_int = info.get<int>("float-array", nullptr, &index, false, nullptr, false);
    return finalizeFailure(rc, failMsg, "did not catch implicit conversion for array");
  }
  catch (esmc_error &exc) {
    if (exc.getReturnCode() != ESMC_RC_ARG_BAD) {
      return finalizeFailure(rc, failMsg, "wrong rc");
    }
  }

  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_bit_overflow()"
void test_bit_overflow(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  ESMCI::Info info;

  // Test integer overflow
  try {
    info.set("bigint", ((long int)(std::numeric_limits<int>::max()))+10, false);
    int too_small = info.get<int>("bigint");
    return finalizeFailure(rc, failMsg, "did not catch overflow");
  }
  catch (esmc_error &exc) {
    if (exc.getReturnCode() != ESMC_RC_ARG_BAD) {
      return finalizeFailure(rc, failMsg, "wrong rc");
    }
  }

  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_isNull()"
void test_isNull(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  ESMCI::Info info;

  try {
    info.set("/NUOPC/Instance/One", json(), false);
    info.set("/NUOPC/Instance/Two", json(55), false);
    if (!info.isNull("/NUOPC/Instance/One")) {
      return finalizeFailure(rc, failMsg, "should be null");
    }
    if (info.isNull("/NUOPC/Instance/Two")) {
      return finalizeFailure(rc, failMsg, "should not be null");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_update_for_attribute()"
void test_update_for_attribute(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  // Test with overwrite true -------------------------------------------------

  ESMCI::Info to_update;
  ESMCI::Info new_contents;

  try {
    to_update.set("/NUOPC/Instance/One", json(44), false);

    new_contents.set("/NUOPC/Instance/One", json(66), false);
    new_contents.set("/NUOPC/Instance/Two", json(55), false);
    new_contents.set("/ESMF/General/Three", json(987), false);

    to_update.update_for_attribute(new_contents, true);
    if (!(to_update.get<int>("/NUOPC/Instance/One")==66)) {
      return finalizeFailure(rc, failMsg, "wrong value 66");
    }
    if (!(to_update.get<int>("/NUOPC/Instance/Two")==55)) {
      return finalizeFailure(rc, failMsg, "wrong value 55");
    }
    if (!(to_update.get<int>("/ESMF/General/Three")==987)) {
      return finalizeFailure(rc, failMsg, "wrong value 987");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  // Test with overwrite false ------------------------------------------------

  ESMCI::Info to_update2;
  ESMCI::Info new_contents2;

  try {
    to_update2.set("/NUOPC/Instance/One", json(44), false);

    new_contents2.set("/NUOPC/Instance/One", json(66), false);
    new_contents2.set("/NUOPC/Instance/Two", json(55), false);
    new_contents2.set("/ESMF/General/Three", json(987), false);

    to_update2.update_for_attribute(new_contents2, false);
    if (!(to_update2.get<int>("/NUOPC/Instance/One")==44)) {
      return finalizeFailure(rc, failMsg, "wrong value 44");
    }
    if (!(to_update2.get<int>("/NUOPC/Instance/Two")==55)) {
      return finalizeFailure(rc, failMsg, "wrong value 55");
    }
    if (!(to_update2.get<int>("/ESMF/General/Three")==987)) {
      return finalizeFailure(rc, failMsg, "wrong value 987");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_set_32bit_type_storage()"
void test_set_32bit_type_storage(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  try {
    ESMCI::Info info;

    info.set_32bit_type_storage("/ESMF/General/is_i4", true, nullptr);
    std::string pkey = "/NUOPC/Instance";
    info.set_32bit_type_storage("is_r4", false, &pkey);

    json &type_storage = info.getTypeStorageWritable();
//    std::cout << type_storage.dump(2) << std::endl;

    if (!(type_storage["ESMF"]["General"]["is_i4"])) {
      return finalizeFailure(rc, failMsg, "wrong flag, should be true");
    }
    if (type_storage["NUOPC"]["Instance"]["is_r4"]) {
      return finalizeFailure(rc, failMsg, "wrong flag, should be false");
    }

  }
  ESMC_CATCH_ERRPASSTHRU

  rc = ESMF_SUCCESS;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "test_dump_and_parse_with_type_storage()"
void test_dump_and_parse_with_type_storage(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;

  try {
    ESMCI::Info info;

    info.set<int>("/ESMF/General/is_i4", 33, false);
    info.set_32bit_type_storage("/ESMF/General/is_i4", true, nullptr);

    std::string actual = info.dump_with_type_storage();

    if (actual != "{\"ESMF\":{\"General\":{\"is_i4\":33}},\"_esmf_info_type_storage\":{\"ESMF\":{\"General\":{\"is_i4\":true}}}}") {
      return finalizeFailure(rc, failMsg, "did not dump type storage");
    }
    if (info.hasKey("_esmf_info_type_storage", true)) {
      return finalizeFailure(rc, failMsg, "type storage not removed");
    }

    // Test dumping with no type storage --------------------------------------

    ESMCI::Info info_no_types;
    std::string mostly_empty = info_no_types.dump_with_type_storage();

    // ------------------------------------------------------------------------
    // Test parsing the dumped string and make sure the type storage is set
    // as expected.

    ESMCI::Info info_parse;
    info_parse.parse_with_type_storage(actual);
    if (info_parse.hasKey("_esmf_info_type_storage", true)) {
      return finalizeFailure(rc, failMsg, "type storage not erased");
    }
    if (!info_parse.getTypeStorage()["ESMF"]["General"]["is_i4"]) {
      return finalizeFailure(rc, failMsg, "type storage not assigned");
    }

    // Test parsing with no type storage in the string. -----------------------

    std::string to_parse = "{\"ESMF\":{\"General\":{\"is_i4\":33}}}";
    ESMCI::Info info2;
    info2.parse_with_type_storage(to_parse);
    if (info2.getTypeStorage().size() != 0) {
      return finalizeFailure(rc, failMsg, "type storage assigned");
    }
  }
  ESMC_CATCH_ERRPASSTHRU

  rc = ESMF_SUCCESS;
};

int main(void) {

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = ESMF_FAILURE;

  strcpy(failMsg, "Did not return ESMF_SUCCESS");  // Default fail message

  //---------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testConstructor");
  testConstructor(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testErase");
  testErase(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testSetGet");
  testSetGet(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testSetGetErrorHandling");
  testSetGetErrorHandling(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testUpdate");
  testUpdate(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testHasKey");
  testHasKey(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testGet");
  testGet(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testbroadcastInfo");
  testbroadcastInfo(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testFormatKey");
  testFormatKey(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testDumpLength");
  testDumpLength(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testSerializeDeserialize");
  testSerializeDeserialize(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testSerializeDeserialize2");
  testSerializeDeserialize2(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testSetGetIndex");
  testSetGetIndex(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_get_attpack_count");
  test_get_attpack_count(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_update_json_attribute_count_map");
  test_update_json_attribute_count_map(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testInquire");
  testInquire(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testInquire32Bit");
  testInquire32Bit(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_update_json_pointer");
  test_update_json_pointer(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testGetObjectIndex");
  testGetObjectIndex(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testGetInfoObject");
  testGetInfoObject(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_find_by_index");
  test_find_by_index(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_implicit_conversion");
  test_implicit_conversion(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_bit_overflow");
  test_bit_overflow(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_isNull");
  test_isNull(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_update_for_attribute");
  test_update_for_attribute(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_set_32bit_type_storage");
  test_set_32bit_type_storage(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "test_dump_and_parse_with_type_storage");
  test_dump_and_parse_with_type_storage(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  return 0;
};
