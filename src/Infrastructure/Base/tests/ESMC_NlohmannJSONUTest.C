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
//==============================================================================

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <iostream>

#include "ESMC.h"
#include "ESMC_Test.h"
#include "ESMCI_VM.h"

// JSON header
#include "json.hpp"

using namespace std;
using json = nlohmann::json;  // Convenience rename for JSON namespace

//==============================================================================
//BOP
// !PROGRAM: ESMC_NlohmannJSONUTest - Test the nlohmann/json for Modern C++
//           library
//
// !DESCRIPTION: Test inclusion of JSON header file and creating a basic object
//
//EOP
//------------------------------------------------------------------------------

class MockInfoNoCopy
{
  public:
    json *storage;

    MockInfoNoCopy(json &storage){
      this->storage = &storage;
    };

    string dump() const{
      return this->storage->dump(2);
    }

    json* getPointer() const{
      return this->storage;
    }

    const json& getStorageRef() const{
      return *(this->storage);
    }
};

void print_vector(vector<int> vec) {
  for (auto ii : vec) {
    cout << ii << " ";
  }
  cout << endl;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "main()"
int main(void) {

  // Test variables
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = ESMF_FAILURE;
  int failed = false;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Basic JSON Map Creation");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  json root;

  root["ESMF"] = json::object();

  root["ESMF"]["General"] = json::object();
  root["ESMF"]["Extended"] = json::object();

  // Reference the ESMF/Extended JSON object using keys
  root["ESMF"]["Extended"]["long_name"] = "foobar";

  // Reference the ESMF/General JSON object then add something to it
  json &general = root["ESMF"]["General"];
  general["what"] = "has been added";

  // Use a JSON Pointer
  json::json_pointer jp("/foo/bar/nest");
  root[jp] = "a deep nest";

  // Add an element then erase it
  json toClear = {{"one",   1},
                  {"two",   2},
                  {"three", 2.9}};
  root["toClear"] = toClear;
  root.erase("toClear");

  ESMC_Test(true, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Reference and pointer access using the JSON API (1)");
  strcpy(failMsg, "Values not equal for references");
  failed = false;

  json j;
  int desired = 123;
  string key = "theNumber";
  j[key] = desired;

  int const &the_ref1 = j[key].get_ref<const json::number_integer_t &>();
  int const &the_ref2 = j[key].get_ref<const json::number_integer_t &>();

  if (the_ref1 != the_ref2) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Reference and pointer access using the JSON API (2)");
  strcpy(failMsg, "Addresses are equal for references");
  failed = false;

  if (&the_ref1 == &the_ref2) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Reference and pointer access using the JSON API (3)");
  strcpy(failMsg, "Pointer value is not the desired value");
  failed = false;

  auto ptr = j[key].get_ptr<const json::number_integer_t *const>();
  if (*ptr != desired) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Reference and pointer access using the JSON API (4)");
  strcpy(failMsg, "Pointer values are not equal");
  failed = false;

  auto *ptr2 = j.at(key).get_ptr<const json::number_integer_t*>();
  if (*ptr != *ptr2) {
    failed = true;
  }
  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Reference and pointer access using the JSON API (5)");
  strcpy(failMsg, "Pointer addresses are not equal");
  failed = false;

  if (&*ptr != &*ptr2) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Mock info constructor (1)");
  strcpy(failMsg, "Storage reference not equivalent");
  failed = false;

  json j2;
  MockInfoNoCopy minfo(j2);
  string key2 = "something";
  string value2 = "nothing";
  j2[key2] = value2;

  const json &refVar = minfo.getStorageRef();
  if (&refVar != &j2) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Mock info constructor (2)");
  strcpy(failMsg, "Value not added to target JSON object");
  failed = false;

  json *ref = minfo.getPointer();
  string actual2 = ref->at(key2);

  if (actual2 != value2) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Mock info constructor (3)");
  strcpy(failMsg, "Address target not equivalent");
  failed = false;

  if (&j2 != &*ref) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Move constructor (1)");
  strcpy(failMsg, "Addresses not equal after move");
  failed = false;

  json src;
  src["something"] = 13;

  auto *srcPtr = src.at("something").get_ptr<json::number_integer_t *>();

  json dst(move(src));

  auto *dstPtr = dst.at("something").get_ptr<json::number_integer_t *>();

  if (&*srcPtr != &*dstPtr) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__,  __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Move constructor (2)");
  strcpy(failMsg, "Source not null");
  failed = false;

  if (!src.is_null()) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Vector retrieval");
  strcpy(failMsg, "Vectors not working");
  failed = false;

  json jv;
  vector<int> c_vec({1, 2, 3, 4});
  jv["foo"] = c_vec;

  // Retrieve the pointer to the array stored in JSON.
  vector<json>* booyah = jv.at("foo").get_ptr<json::array_t*>();

  // Allocate an integer vector to copy JSON array into.
  vector<int> booyah2(booyah[0].size(), -999);

  // Copy JSON data into new vector.
  for (std::size_t ii=0; ii<booyah[0].size(); ++ii) {
    booyah2[ii] = booyah[0][ii];
  }

  // Test desired vector matches JSON vector.
  for (int ii=0; ii<=3; ++ii) {
    if (c_vec[ii] != booyah[0][ii]) {
      failed = true;
    }
  }

  // Test desired vector matches copied vector.
  for (int ii=0; ii<=3; ++ii) {
    if (c_vec[ii] != booyah2[ii]) {
      failed = true;
    }
  }

  // Test changing JSON store changes JSON vector pointer.
  jv["foo"][1] = -999;
  if (booyah[0][1] != -999) {
    failed = true;
  }

  // Test changing JSON vector pointer changes JSON store.
  booyah[0][2] = 4444;
  if (jv["foo"][2] != 4444) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Vector reservation");
  strcpy(failMsg, "Vector reservation failed");
  failed = false;

  json jr;
  jr["the_array"] = json::array();
  json::array_t* the_array_ptr = jr.at("the_array").get_ptr<json::array_t*>();
  the_array_ptr->reserve(3);

  if (the_array_ptr->capacity() != 3) {
    failed = true;
  }

  the_array_ptr->push_back(333);

  if (the_array_ptr[0][0] != 333) {
    failed = true;
  }

  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Pointer to internal information");
  strcpy(failMsg, "Pointer result bad");
  failed = false;

  json jjp;
  jjp = json::object();
  json::object_t& refjjp = jjp.get_ref<json::object_t&>();
  jjp["ESMF"]["General"]["woohoo"] = 33;
  jjp["ESMF"]["General"]["huh"] = "boom";
  jjp["ESMF"]["General"]["life"] = true;
  jjp["NUOPC"]["Special"]["woohoo"] = 99;

  json::object_t *jjpp = jjp.at("NUOPC").at("Special").get_ptr<json::object_t*>();

  (*jjpp)["woohoo"] = 111;
  json jnest = {{"woot", 768}, {"now", "is the time"}};
  (*jjpp)["nest"] = jnest;
  json::object_t &refj = *jjpp;
  refj["weather"] = 45;

  json *storagep = nullptr;
  json &egen = jjp["ESMF"]["General"];
  storagep = &egen;

  if (refj.at("woohoo") != 111 &&
      refj.at("nest").at("woot") != 768 &&
      refj.at("weather") == 45 &&
      (*storagep).value("woohoo", 777) != 33) failed = true;
  ESMC_Test(!failed, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Equality operator between in-memory and parse");
  strcpy(failMsg, "Wrong result");
  failed = false;

  json j1;
  j1["22-State022"]["base_id"] = 22;
  j1["22-State022"]["base_is_valid"] = true;
  j1["22-State022"]["esmf_type"] = "State";
  j1["22-State022"]["info"]["fvarname"] = "state";
  j1["22-State022"]["members"]["14-Field014"]["base_id"] = 14;
  j1["22-State022"]["members"]["14-Field014"]["base_is_valid"] = true;
  j1["22-State022"]["members"]["14-Field014"]["esmf_type"] = "Field";
  j1["22-State022"]["members"]["14-Field014"]["members"] = json::value_t::null;
  j1["22-State022"]["members"]["14-Field014"]["info"]["fvarname"] = "field3";

  json j3 = json::parse("{\"22-State022\":{\"base_id\":22,\"base_is_valid\":true,\"esmf_type\":\"State\",\"info\":{\"fvarname\":\"state\"},\"members\":{\"14-Field014\":{\"base_id\":14,\"base_is_valid\":true,\"esmf_type\":\"Field\",\"info\":{\"fvarname\":\"field3\"},\"members\":null}}}}");

  ESMC_Test(j1==j3, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
