// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_HConfig.C"
//==============================================================================
//
// HConfig class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ HConfig methods declared
// in the companion file ESMCI_HConfig.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_HConfig.h"

// include higher level, 3rd party or system headers
#include <cerrno>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <sstream>
#include <string>
#include <vector>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"

// LogErr headers
#include "ESMCI_LogErr.h"                  // for LogErr

using namespace std;

extern "C" {

  void FTN_X(f_esmf_configsetstring)(ESMCI_Config** config, const char* value,
    const char* label, int* rc, ESMCI_FortranStrLenArg vlen, 
    ESMCI_FortranStrLenArg llen);

}; // end prototypes for fortran interface

namespace ESMCI {

//-----------------------------------------------------------------------------
//
// Explicit template instantiation (do not confuse with specialization!!!)
// The reason for explicit instantiation here is that it will tell the compiler
// explicitly to instantiate the following special instantiations of the
// template. This way the definition of the templated methods do not have to
// sit with the declaration in the header file, but can be located in the
// source file.
//
//-----------------------------------------------------------------------------
template std::string HConfig::as<std::string>(bool *asOkay, int *rc);
template bool HConfig::as<bool>(bool *asOkay, int *rc);
template ESMC_I4 HConfig::as<ESMC_I4>(bool *asOkay, int *rc);
template ESMC_I8 HConfig::as<ESMC_I8>(bool *asOkay, int *rc);
template ESMC_R4 HConfig::as<ESMC_R4>(bool *asOkay, int *rc);
template ESMC_R8 HConfig::as<ESMC_R8>(bool *asOkay, int *rc);
template std::string HConfig::asMapKey<std::string>(bool *asOkay, int *rc);
template bool HConfig::asMapKey<bool>(bool *asOkay, int *rc);
template ESMC_I4 HConfig::asMapKey<ESMC_I4>(bool *asOkay, int *rc);
template ESMC_I8 HConfig::asMapKey<ESMC_I8>(bool *asOkay, int *rc);
template ESMC_R4 HConfig::asMapKey<ESMC_R4>(bool *asOkay, int *rc);
template ESMC_R8 HConfig::asMapKey<ESMC_R8>(bool *asOkay, int *rc);
template std::string HConfig::asMapVal<std::string>(bool *asOkay, int *rc);
template bool HConfig::asMapVal<bool>(bool *asOkay, int *rc);
template ESMC_I4 HConfig::asMapVal<ESMC_I4>(bool *asOkay, int *rc);
template ESMC_I8 HConfig::asMapVal<ESMC_I8>(bool *asOkay, int *rc);
template ESMC_R4 HConfig::asMapVal<ESMC_R4>(bool *asOkay, int *rc);
template ESMC_R8 HConfig::asMapVal<ESMC_R8>(bool *asOkay, int *rc);

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::create()"
//BOP
// !IROUTINE:  ESMCI::HConfig::create - Create a new HConfig
//
// !INTERFACE:
HConfig HConfig::create(
//
// !RETURN VALUE:
//  pointer to newly allocated HConfig
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Set up HConfig internals.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;
  hconfig.node = NULL;

  try{

    // new object
    hconfig.node = new YAML::Node;

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return hconfig;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::HConfig.", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::destroy()"
//BOP
// !IROUTINE:  ESMCI::HConfig::destroy - free a HConfig
//
// !INTERFACE:
int HConfig::destroy(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *hconfig){   // in - HConfig object to destroy
//
// !DESCRIPTION:
//  ESMF routine which destroys a HConfig object.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (hconfig == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to HConfig", ESMC_CONTEXT, &rc);
    return rc;
  }

  // protect node from delete in case this is an iterator but user calls
  // the destroy call on it
  if (hconfig->node){
    // delete the YAML::Node
    delete (hconfig->node);
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::load()"
//BOP
// !IROUTINE:  ESMCI::HConfig::load - load a HConfig from string
//
// !INTERFACE:
int HConfig::load(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    const std::string& content){       // in
// 
// !DESCRIPTION: 
//  ESMF routine which loads HConfig from string.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      *node = YAML::Load(content);
    else{
      // iterator cannot be used here
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must NOT be iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception loading content from string", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::loadFile()"
//BOP
// !IROUTINE:  ESMCI::HConfig::loadFile - load a HConfig from file
//
// !INTERFACE:
int HConfig::loadFile(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    const std::string& filename){       // in
// 
// !DESCRIPTION: 
//  ESMF routine which loads HConfig from file.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      *node = YAML::LoadFile(filename);
    else{
      // iterator cannot be used here
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must NOT be iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception loading content from file", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::saveFile()"
//BOP
// !IROUTINE:  ESMCI::HConfig::saveFile - save a HConfig to file
//
// !INTERFACE:
int HConfig::saveFile(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    const std::string& filename){       // in
// 
// !DESCRIPTION: 
//  ESMF routine which saves HConfig to file. Only localPet==0 does the writing.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (VM::getCurrent()->getLocalPet() == 0){
      std::ofstream fout(filename);
      if (node){
        // node
        fout << *node;
      }else
        // iterator
        if (type==YAML::NodeType::Map){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
          return rc;
        }else
          fout << (YAML::Node)(*iter);
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception loading content from file", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::createAt()"
//BOP
// !IROUTINE:  ESMCI::HConfig::createAt - node at location
//
// !INTERFACE:
HConfig HConfig::createAt(
//
// !RETURN VALUE:
//  node
//
// !ARGUMENTS:
    int *index,           // in  - if present, access by index, Fortran base 1
    int *rc) {            // out - return code
//
// !DESCRIPTION:
//  Return node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    // new object
    hconfig.node = new YAML::Node;

    if (node){
      // node
      if (index){
        if (node->Type()!=YAML::NodeType::Sequence){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "HConfig object MUST be sequence when index specified",
            ESMC_CONTEXT, rc);
          return hconfig;
        }else{
          *hconfig.node = (*node)[*index-1];
        }
      }else{
        *hconfig.node = (*node);
      }
    }else{
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return hconfig;
      }else{
        if (index){
          if (iter->Type()!=YAML::NodeType::Sequence){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
              "HConfig iterator MUST be sequence when index specified",
              ESMC_CONTEXT, rc);
            return hconfig;
          }else
            *hconfig.node = (*iter)[*index-1];
        }else{
          *hconfig.node = ((YAML::Node)(*iter));
        }
      }
    }
    hconfig.type = hconfig.node->Type();

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
template <typename T> inline YAML::Node HConfig::find(T self, HConfig *key){
  if (key->node->IsScalar()){
    // handle scalar key as simple string direct indexing
    return (self)[key->node->as<std::string>()];
  }else{
    // handle complex key by iteration and comparing serialized node
    std::stringstream ss_key;
    ss_key << *(key->node);
    for (auto it=self.begin(); it!=self.end(); ++it){
      std::stringstream ss_self;
      ss_self << it->first;
      if (ss_self.str() == ss_key.str())
        return it->second;
    }
  }
  return YAML::Node();
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::createAtKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::createAtKey - node at location
//
// !INTERFACE:
HConfig HConfig::createAtKey(
//
// !RETURN VALUE:
//  node
//
// !ARGUMENTS:
    HConfig *key,         // in  - access by key
    int *rc) {            // out - return code
//
// !DESCRIPTION:
//  Return node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    // new object
    hconfig.node = new YAML::Node;

    if (node){
      // node
      if (node->Type()!=YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object MUST be map when key specified",
          ESMC_CONTEXT, rc);
        return hconfig;
      }else
        *hconfig.node = HConfig::find(*node, key);
    }else{
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return hconfig;
      }else{
        if (iter->Type()!=YAML::NodeType::Map){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "HConfig iterator MUST be map when key specified",
            ESMC_CONTEXT, rc);
          return hconfig;
        }else
          *hconfig.node = HConfig::find(*iter, key);
      }
    }
    hconfig.type = hconfig.node->Type();

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::createAtMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::createAtMapKey - node at location
//
// !INTERFACE:
HConfig HConfig::createAtMapKey(
//
// !RETURN VALUE:
//  node
//
// !ARGUMENTS:
    int *index,           // in  - if present, access by index, Fortran base 1
    int *rc) {            // out - return code
//
// !DESCRIPTION:
//  Return node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    // new object
    hconfig.node = new YAML::Node;

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      if (index){
        if (iter->first.Type()!=YAML::NodeType::Sequence){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "HConfig iterator must be sequence when index specified",
            ESMC_CONTEXT, rc);
          return hconfig;
        }else
          *hconfig.node = (iter->first)[*index-1];
      }else{
        *hconfig.node = ((YAML::Node)(iter->first));
      }
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }
    hconfig.type = hconfig.node->Type();

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::createAtMapKeyKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::createAtMapKeyKey - node at location
//
// !INTERFACE:
HConfig HConfig::createAtMapKeyKey(
//
// !RETURN VALUE:
//  node
//
// !ARGUMENTS:
    HConfig *key,         // in  - access by key
    int *rc) {            // out - return code
//
// !DESCRIPTION:
//  Return node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    // new object
    hconfig.node = new YAML::Node;

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      if (iter->first.Type()!=YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig iterator MUST be map when key specified",
          ESMC_CONTEXT, rc);
        return hconfig;
      }else
        *hconfig.node = HConfig::find(iter->first, key);
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }
    hconfig.type = hconfig.node->Type();

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::createAtMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::createAtMapVal - node at location
//
// !INTERFACE:
HConfig HConfig::createAtMapVal(
//
// !RETURN VALUE:
//  node
//
// !ARGUMENTS:
    int *index,           // in  - if present, access by index, Fortran base 1
    int *rc) {            // out - return code
//
// !DESCRIPTION:
//  Return node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    // new object
    hconfig.node = new YAML::Node;

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      if (index){
        if (iter->second.Type()!=YAML::NodeType::Sequence){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "HConfig iterator must be sequence when index specified",
            ESMC_CONTEXT, rc);
          return hconfig;
        }else
          *hconfig.node = (iter->second)[*index-1];
      }else{
        *hconfig.node = ((YAML::Node)(iter->second));
      }
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }
    hconfig.type = hconfig.node->Type();

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::createAtMapValKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::createAtMapValKey - node at location
//
// !INTERFACE:
HConfig HConfig::createAtMapValKey(
//
// !RETURN VALUE:
//  node
//
// !ARGUMENTS:
    HConfig *key,         // in  - access by key
    int *rc) {            // out - return code
//
// !DESCRIPTION:
//  Return node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    // new object
    hconfig.node = new YAML::Node;

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      if (iter->second.Type()!=YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig iterator MUST be map when key specified",
          ESMC_CONTEXT, rc);
        return hconfig;
      }else
        *hconfig.node = HConfig::find(iter->second, key);
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }
    hconfig.type = hconfig.node->Type();

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::add()"
//BOP
// !IROUTINE:  ESMCI::HConfig::add - add value in node
//
// !INTERFACE:
int HConfig::add(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *value){  // in  - value to be added
// 
// !DESCRIPTION: 
//  ESMF routine to add node value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    if (node){
      // node
      if (node->Type()!=YAML::NodeType::Sequence
        && node->Type()!=YAML::NodeType::Null){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object MUST be sequence or NULL when adding item to the end",
          ESMC_CONTEXT, &rc);
        return rc;
      }else
        node->push_back(YAML::Load(ss.str()));
    }else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else{
        if (iter->Type()!=YAML::NodeType::Sequence
          && iter->Type()!=YAML::NodeType::Null){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "HConfig object MUST be sequence or NULL when adding item to the end",
            ESMC_CONTEXT, &rc);
          return rc;
        }else
          iter->push_back(YAML::Load(ss.str()));
      }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::add()"
//BOP
// !IROUTINE:  ESMCI::HConfig::add - add value in node
//
// !INTERFACE:
int HConfig::add(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *key,     // in  - key to be added
    HConfig *value){  // in  - value to be added
// 
// !DESCRIPTION: 
//  ESMF routine to add node value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    YAML::Node keyNode;
    if (key->node)
      keyNode = *(key->node);
    else
      keyNode = (YAML::Node)*(key->iter);
    if (node){
      // node
      if (node->Type()!=YAML::NodeType::Map
        && node->Type()!=YAML::NodeType::Null){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object MUST be map or NULL when adding item with key",
          ESMC_CONTEXT, &rc);
        return rc;
      }else
        (*node)[keyNode] = YAML::Load(ss.str());
    }else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else{
        if (iter->Type()!=YAML::NodeType::Map
          && iter->Type()!=YAML::NodeType::Null){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "HConfig object MUST be map or NULL when adding item with key",
            ESMC_CONTEXT, &rc);
          return rc;
        }else
          ((YAML::Node)(*iter))[keyNode] = YAML::Load(ss.str());
      }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::addMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::addMapKey - add value in map key node
//
// !INTERFACE:
int HConfig::addMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *value){  // in  - value to be added
// 
// !DESCRIPTION: 
//  ESMF routine to add map key node value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    if ((node==NULL) && (type==YAML::NodeType::Map)){
      if (iter->first.Type()!=YAML::NodeType::Sequence
        && iter->first.Type()!=YAML::NodeType::Null){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object MUST be sequence or NULL when adding item to the end",
          ESMC_CONTEXT, &rc);
        return rc;
      }else
        iter->first.push_back(YAML::Load(ss.str()));
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::addMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::addMapKey - add value in map key node
//
// !INTERFACE:
int HConfig::addMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *key,     // in  - key to be added
    HConfig *value){  // in  - value to be added
// 
// !DESCRIPTION: 
//  ESMF routine to add map key node value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    YAML::Node keyNode;
    if (key->node)
      keyNode = *(key->node);
    else
      keyNode = (YAML::Node)*(key->iter);
    if ((node==NULL) && (type==YAML::NodeType::Map)){
      if (iter->first.Type()!=YAML::NodeType::Sequence
        && iter->first.Type()!=YAML::NodeType::Null){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object MUST be sequence or NULL when adding item to the end",
          ESMC_CONTEXT, &rc);
        return rc;
      }else
        (iter->first)[keyNode] = YAML::Load(ss.str());
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::addMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::addMapVal - add value in map value node
//
// !INTERFACE:
int HConfig::addMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *value){  // in  - value to be added
// 
// !DESCRIPTION: 
//  ESMF routine to add map value node value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    if ((node==NULL) && (type==YAML::NodeType::Map)){
      if (iter->second.Type()!=YAML::NodeType::Sequence
        && iter->second.Type()!=YAML::NodeType::Null){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object MUST be sequence or NULL when adding item to the end",
          ESMC_CONTEXT, &rc);
        return rc;
      }else
        iter->second.push_back(YAML::Load(ss.str()));
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::addMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::addMapVal - add value in map value node
//
// !INTERFACE:
int HConfig::addMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *key,     // in  - key to be added
    HConfig *value){  // in  - value to be added
// 
// !DESCRIPTION: 
//  ESMF routine to add map value node value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    YAML::Node keyNode;
    if (key->node)
      keyNode = *(key->node);
    else
      keyNode = (YAML::Node)*(key->iter);
    if ((node==NULL) && (type==YAML::NodeType::Map)){
      if (iter->second.Type()!=YAML::NodeType::Sequence
        && iter->second.Type()!=YAML::NodeType::Null){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object MUST be sequence or NULL when adding item to the end",
          ESMC_CONTEXT, &rc);
        return rc;
      }else
        (iter->second)[keyNode] = YAML::Load(ss.str());
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::getSize()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getSize - Get size of the node
//
// !INTERFACE:
int HConfig::getSize(
//
// !RETURN VALUE:
//  int
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return the size of the node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  int size = 0;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      // node
      size = node->size();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return size;
      }else
        size = iter->size();
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return size;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return size;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::getSizeMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getSizeMapKey - Get size of the node
//
// !INTERFACE:
int HConfig::getSizeMapKey(
//
// !RETURN VALUE:
//  int
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return the size of the node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  int size = 0;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      size = iter->first.size();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return size;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return size;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return size;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::getSizeMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getSizeMapVal - Get size of the node
//
// !INTERFACE:
int HConfig::getSizeMapVal(
//
// !RETURN VALUE:
//  int
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return the size of the node.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  int size = 0;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      size = iter->second.size();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return size;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return size;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return size;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
inline string HConfig::tagRef(YAML::Node &self){
  string value = self.Tag();
  if ((value == "?") || (value == "")){
    // not a valid tag found -> implement implicit tag resolution
    switch (self.Type()){
      case YAML::NodeType::Null:
        self.SetTag("tag:yaml.org,2002:null");
        break;
      case YAML::NodeType::Scalar:
        self.SetTag("tag:yaml.org,2002:str");  // default scalar
        bool bDummy;
        ESMC_I8 iDummy;
        ESMC_R8 fDummy;
        if (YAML::convert<bool>::decode(self, bDummy))
          self.SetTag("tag:yaml.org,2002:bool");
        else if (YAML::convert<ESMC_I8>::decode(self, iDummy))
          self.SetTag("tag:yaml.org,2002:int");
        else if (YAML::convert<ESMC_R8>::decode(self, fDummy))
          self.SetTag("tag:yaml.org,2002:float");
        break;
      case YAML::NodeType::Sequence:
        self.SetTag("tag:yaml.org,2002:seq");
        break;
      case YAML::NodeType::Map:
        self.SetTag("tag:yaml.org,2002:map");
        break;
      default:
        break;
    }
    value = self.Tag();  // determine final outcome
  }
  return value;
}
//-----------------------------------------------------------------------------
inline string HConfig::tag(YAML::Node self){
  string value = self.Tag();
  if ((value == "?") || (value == "")){
    // not a valid tag found -> implement implicit tag resolution
    switch (self.Type()){
      case YAML::NodeType::Null:
        self.SetTag("tag:yaml.org,2002:null");
        break;
      case YAML::NodeType::Scalar:
        self.SetTag("tag:yaml.org,2002:str");  // default scalar
        bool bDummy;
        ESMC_I8 iDummy;
        ESMC_R8 fDummy;
        if (YAML::convert<bool>::decode(self, bDummy))
          self.SetTag("tag:yaml.org,2002:bool");
        else if (YAML::convert<ESMC_I8>::decode(self, iDummy))
          self.SetTag("tag:yaml.org,2002:int");
        else if (YAML::convert<ESMC_R8>::decode(self, fDummy))
          self.SetTag("tag:yaml.org,2002:float");
        break;
      case YAML::NodeType::Sequence:
        self.SetTag("tag:yaml.org,2002:seq");
        break;
      case YAML::NodeType::Map:
        self.SetTag("tag:yaml.org,2002:map");
        break;
      default:
        break;
    }
    value = self.Tag();  // determine final outcome
  }
  return value;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::getTag()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getTag - Get the tag of the node
//
// !INTERFACE:
string HConfig::getTag(
//
// !RETURN VALUE:
//  string
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as string
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  string value = "";

#ifdef ESMF_YAMLCPP
  try{
    if (node){
      // node
      value = HConfig::tagRef(*node);
    }else{
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return value;
      }else{
        value = HConfig::tag(*iter);
      }
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return value;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return value;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::getTagMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getTagMapKey - Get the tag of the node
//
// !INTERFACE:
string HConfig::getTagMapKey(
//
// !RETURN VALUE:
//  string
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as string
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  string value = "";

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      value = HConfig::tag(iter->first);
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return value;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return value;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return value;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::getTagMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getTagMapVal - Get the tag of the node
//
// !INTERFACE:
string HConfig::getTagMapVal(
//
// !RETURN VALUE:
//  string
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as string
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  string value = "";

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      value = HConfig::tag(iter->second);
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return value;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return value;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return value;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isNull()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isNull - access node type
//
// !INTERFACE:
int HConfig::isNull(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      // node
      *flag = node->IsNull();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else
        *flag = iter->IsNull();
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isScalar()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isScalar - access node type
//
// !INTERFACE:
int HConfig::isScalar(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      // node
      *flag = node->IsScalar();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else
        *flag = iter->IsScalar();
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isSequence()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isSequence - access node type
//
// !INTERFACE:
int HConfig::isSequence(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      // node
      *flag = node->IsSequence();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else
        *flag = iter->IsSequence();
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isMap()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMap - access node type
//
// !INTERFACE:
int HConfig::isMap(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      // node
      *flag = node->IsMap();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else
        *flag = iter->IsMap();
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isDefined()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isDefined - access node type
//
// !INTERFACE:
int HConfig::isDefined(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      // node
      *flag = node->IsDefined();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else
        *flag = iter->IsDefined();
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isNullMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isNullMapKey - access node type
//
// !INTERFACE:
int HConfig::isNullMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->first.IsNull();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isScalarMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isScalarMapKey - access node type
//
// !INTERFACE:
int HConfig::isScalarMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->first.IsScalar();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isSequenceMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isSequenceMapKey - access node type
//
// !INTERFACE:
int HConfig::isSequenceMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->first.IsSequence();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isMapMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapMapKey - access node type
//
// !INTERFACE:
int HConfig::isMapMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->first.IsMap();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isDefinedMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isDefinedMapKey - access node type
//
// !INTERFACE:
int HConfig::isDefinedMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->first.IsDefined();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isNullMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isNullMapVal - access node type
//
// !INTERFACE:
int HConfig::isNullMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->second.IsNull();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isScalarMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isScalarMapVal - access node type
//
// !INTERFACE:
int HConfig::isScalarMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->second.IsScalar();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isSequenceMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isSequenceMapVal - access node type
//
// !INTERFACE:
int HConfig::isSequenceMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->second.IsSequence();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isMapMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapMapVal - access node type
//
// !INTERFACE:
int HConfig::isMapMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->second.IsMap();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isDefinedMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isDefinedMapVal - access node type
//
// !INTERFACE:
int HConfig::isDefinedMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      // map iterator
      *flag = iter->second.IsDefined();
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isIterator()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isIterator - access node type
//
// !INTERFACE:
int HConfig::isIterator(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    *flag = (node==NULL);
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isSequenceIterator()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isSequenceIterator - access node type
//
// !INTERFACE:
int HConfig::isSequenceIterator(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    *flag = ((node==NULL) & (type==YAML::NodeType::Sequence));
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::isMapIterator()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapIterator - access node type
//
// !INTERFACE:
int HConfig::isMapIterator(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    bool *flag){      // out
// 
// !DESCRIPTION: 
//  ESMF routine to access node type
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    *flag = ((node==NULL) & (type==YAML::NodeType::Map));
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::iterBegin()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterBegin - Iterator pointing to first item
//
// !INTERFACE:
HConfig HConfig::iterBegin(
//
// !RETURN VALUE:
//  pointer to iterator
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return iterator pointing to first item.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    if (node){
      // node
      hconfig.type = node->Type();
      hconfig.iter = node->begin();
    }else{
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return hconfig;
      }else{
        hconfig.type = iter->Type();
        hconfig.iter = iter->begin();
      }
    }

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::iterEnd()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterEnd - Iterator pointing to one past the last item
//
// !INTERFACE:
HConfig HConfig::iterEnd(
//
// !RETURN VALUE:
//  pointer to iterator
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return iterator pointing to one past the last item.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    if (node){
      // node
      hconfig.type = node->Type();
      hconfig.iter = node->end();
    }else{
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return hconfig;
      }else{
        hconfig.type = iter->Type();
        hconfig.iter = iter->end();
      }
    }

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::iterBeginMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterBeginMapKey - Iterator pointing to first item
//
// !INTERFACE:
HConfig HConfig::iterBeginMapKey(
//
// !RETURN VALUE:
//  pointer to iterator
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return iterator pointing to first item.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      hconfig.type = iter->first.Type();
      hconfig.iter = iter->first.begin();
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::iterEndMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterEndMapKey - Iterator pointing to one past the last item
//
// !INTERFACE:
HConfig HConfig::iterEndMapKey(
//
// !RETURN VALUE:
//  pointer to iterator
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return iterator pointing to one past the last item.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      hconfig.type = iter->first.Type();
      hconfig.iter = iter->first.end();
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::iterBeginMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterBeginMapVal - Iterator pointing to first item
//
// !INTERFACE:
HConfig HConfig::iterBeginMapVal(
//
// !RETURN VALUE:
//  pointer to iterator
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return iterator pointing to first item.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      hconfig.type = iter->second.Type();
      hconfig.iter = iter->second.begin();
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::iterEndMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterEndMapVal - Iterator pointing to one past the last item
//
// !INTERFACE:
HConfig HConfig::iterEndMapVal(
//
// !RETURN VALUE:
//  pointer to iterator
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return iterator pointing to one past the last item.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  HConfig hconfig;

#ifdef ESMF_YAMLCPP
  hconfig.node = NULL;

  try{

    if ((node==NULL) && (type==YAML::NodeType::Map)){
      // map iterator
      hconfig.type = iter->second.Type();
      hconfig.iter = iter->second.end();
    }else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return hconfig;
    }

  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return hconfig;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return hconfig;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::iterNext()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterNext - one step forward iterator
//
// !INTERFACE:
int HConfig::iterNext(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    ){
// 
// !DESCRIPTION: 
//  Steps the iterator forward by one step.
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP

  try{
    // make sure this is an iterator, or else error out
    if (node){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
    ++iter;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::as()"
//BOP
// !IROUTINE:  ESMCI::HConfig::as - Interpret value
//
// !INTERFACE:
template<typename T> T HConfig::as(
//
// !RETURN VALUE:
//  T
//
// !ARGUMENTS:
    bool *asOkay,       // out - indicate if as() access okay
    int *rc) {          // out - return code
//
// !DESCRIPTION:
//  Return value interpreted
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  T value = T();

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      *asOkay = YAML::convert<T>::decode(*node, value);
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return value;
      }else
        *asOkay = YAML::convert<T>::decode(*iter, value);
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return value;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return value;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::asMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapKey - Interpret value
//
// !INTERFACE:
template<typename T> T HConfig::asMapKey(
//
// !RETURN VALUE:
//  T
//
// !ARGUMENTS:
    bool *asOkay,       // out - indicate if as() access okay
    int *rc) {          // out - return code
//
// !DESCRIPTION:
//  Return value interpreted
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  T value = T();

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      *asOkay = YAML::convert<T>::decode(iter->first, value);
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return value;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return value;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return value;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::asMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapVal - Interpret value
//
// !INTERFACE:
template<typename T> T HConfig::asMapVal(
//
// !RETURN VALUE:
//  T
//
// !ARGUMENTS:
    bool *asOkay,       // out - indicate if as() access okay
    int *rc) {          // out - return code
//
// !DESCRIPTION:
//  Return value interpreted
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  T value = T();

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      *asOkay = YAML::convert<T>::decode(iter->second, value);
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, rc);
      return value;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, rc);
    return value;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
#endif

  return value;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::set()"
//BOP
// !IROUTINE:  ESMCI::HConfig::set - set value in node
//
// !INTERFACE:
int HConfig::set(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *value){  // in  - value to be set
// 
// !DESCRIPTION: 
//  ESMF routine to set node value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    if (node){
      // node
      *node = YAML::Load(ss.str());
    }else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, &rc);
        return rc;
      }else
        (YAML::Node)*iter = YAML::Load(ss.str());
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::setMapKey()"
//BOP
// !IROUTINE:  ESMCI::HConfig::setMapKey - set map key value in node
//
// !INTERFACE:
int HConfig::setMapKey(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *value){  // in  - value to be set
// 
// !DESCRIPTION: 
//  ESMF routine to set node map key value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    if ((node==NULL) && (type==YAML::NodeType::Map))
      iter->first = YAML::Load(ss.str());
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::setMapVal()"
//BOP
// !IROUTINE:  ESMCI::HConfig::setMapVal - set map val value in node
//
// !INTERFACE:
int HConfig::setMapVal(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    HConfig *value){  // in  - value to be set
// 
// !DESCRIPTION: 
//  ESMF routine to set node map val value via deep copy
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{
    std::stringstream ss;
    if (value->node)
      ss << *(value->node);
    else
      ss << *(value->iter);
    if ((node==NULL) && (type==YAML::NodeType::Map))
      iter->second = YAML::Load(ss.str());
    else{
      // error
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "HConfig object must be map iterator", ESMC_CONTEXT, &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception accessing node information", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::HConfig::toConfig()"
//BOP
// !IROUTINE:  ESMCI::HConfig::toConfig - fill a Config from HConfig
//
// !INTERFACE:
int HConfig::toConfig(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    ESMCI_Config **config){       // in
// 
// !DESCRIPTION: 
//  ESMF routine which attempts to fill Config from HConfig to the level
//  that this is supported. Expectation on the highest level (doc-level) is
//  a map of scalars keys to the following three value options:
//    (1) scalar
//    (2) list of scalars
//    (3) list of lists of scalars
//  Anything else is not supported, and is silently skipped.
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;

#ifdef ESMF_YAMLCPP
  try{

    if (node->IsMap()){
      // only support map on the doc level
      for (auto it=node->begin(); it!=node->end(); ++it){
        if (it->first.IsScalar()){
          // support scalar keys
          string label = it->first.as<string>() + ":";  // use colon separator
          vector<vector<string> > values;
          if (it->second.IsScalar()){
            // support scalar values
            values.resize(1);
            values[0].push_back(it->second.as<string>());
          }else if (it->second.IsSequence()){
            // support sequence values
            bool firstRound = true; // mark first round for check below
            bool isScalar;          // will be set below for first round
            for (auto itV=it->second.begin(); itV!=it->second.end(); ++itV){
              if (itV->IsScalar()){
                // support sequence of scalar values
                if (firstRound){
                  firstRound = false;
                  isScalar = true;
                  values.resize(1);
                }else if (!isScalar){
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                    "Inconsistent values in list", ESMC_CONTEXT, &rc);
                  throw rc;
                }
                values[0].push_back(itV->as<string>());
              }else if (itV->IsSequence()){
                // support sequence of sequences
                if (firstRound){
                  firstRound = false;
                  isScalar = false;
                }else if (isScalar){
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                    "Inconsistent values in list", ESMC_CONTEXT, &rc);
                  throw rc;
                }
                vector<string> vString;
                for (auto itVV=itV->begin(); itVV!=itV->end(); ++itVV){
                  if (itVV->IsScalar()){
                    // support sequence of sequences of scalar values
                    vString.push_back(itVV->as<string>());
                  }
                }
                values.push_back(vString);
              }
            }
          }
          if (values.size()){
            // values present
            string value;
            bool tableFlag = false;
            if (values.size()>1){
              label += ":";
              tableFlag = true;
            }
            for (auto it=values.begin(); it!=values.end(); ++it){
              if (tableFlag) value += "\n";
              for (auto itV=it->begin(); itV!=it->end(); ++itV)
                value += *itV + " "; // use white space delimiter
            }
            if (tableFlag) value += "\n::";
            FTN_X(f_esmf_configsetstring)(config, value.c_str(),
              label.c_str(), &localrc, value.size(), label.size());
            if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
              ESMC_CONTEXT, &rc)) throw rc;
          }
        }
      }
    }
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    // catch any other exception, e.g. thrown by YAML-CPP
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception thrown by YAML-CPP", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
