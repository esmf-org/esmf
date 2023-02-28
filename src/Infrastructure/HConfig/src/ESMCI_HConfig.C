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
    hconfig.isIter = false;

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
  if (!hconfig->isIter){
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
    *node = YAML::Load(content);
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
    *node = YAML::LoadFile(filename);

    std::stringstream debugmsg;
    debugmsg << "node.Type: " << node->Type();
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);

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
    if (isIter)
      if (type==YAML::NodeType::Map)
        *flag = iter->second.IsNull();
      else
        *flag = iter->IsNull();
    else
      *flag = node->IsNull();
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
    if (isIter)
      if (type==YAML::NodeType::Map)
        *flag = iter->second.IsScalar();
      else
        *flag = iter->IsScalar();
    else
      *flag = node->IsScalar();
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
    if (isIter)
      if (type==YAML::NodeType::Map)
        *flag = iter->second.IsSequence();
      else
        *flag = iter->IsSequence();
    else
      *flag = node->IsSequence();
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
    if (isIter)
      if (type==YAML::NodeType::Map)
        *flag = iter->second.IsMap();
      else
        *flag = iter->IsMap();
    else
      *flag = node->IsMap();
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
    if (isIter)
      if (type==YAML::NodeType::Map)
        *flag = iter->second.IsDefined();
      else
        *flag = iter->IsDefined();
    else
      *flag = node->IsDefined();
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
#define ESMC_METHOD "ESMCI::HConfig::asString()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asString - Interpret value as string
//
// !INTERFACE:
string HConfig::asString(
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

  string value;

#ifdef ESMF_YAMLCPP
  try{
    if (isIter)
      if (type==YAML::NodeType::Map)
        value = iter->second.as<string>();
      else
        value = iter->as<string>();
    else
      value = node->as<string>();
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

    if (isIter){
      hconfig.isIter = true;
      if (type==YAML::NodeType::Map){
        hconfig.type = iter->second.Type();
        hconfig.iter = iter->second.begin();
      }else{
        hconfig.type = iter->Type();
        hconfig.iter = iter->begin();
      }
    }else{
      hconfig.isIter = true;
      hconfig.type = node->Type();
      hconfig.iter = node->begin();
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

    if (isIter){
      hconfig.isIter = true;
      if (type==YAML::NodeType::Map){
        hconfig.type = iter->second.Type();
        hconfig.iter = iter->second.end();
      }else{
        hconfig.type = iter->Type();
        hconfig.iter = iter->end();
      }
    }else{
      hconfig.isIter = true;
      hconfig.type = node->Type();
      hconfig.iter = node->end();
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
    if (!isIter){
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
