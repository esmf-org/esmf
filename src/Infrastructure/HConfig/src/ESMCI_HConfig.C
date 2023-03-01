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
      if (index){
        *hconfig.node = (*node)[*index-1];
        hconfig.type = (*node)[*index-1].Type();
      }else{
        *hconfig.node = (*node);
        hconfig.type = (*node).Type();
      }
    }else{
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return hconfig;
      }else{
        if (index){
          *hconfig.node = (*iter)[*index-1];
          hconfig.type = (*iter)[*index-1].Type();
        }else{
          *hconfig.node = ((YAML::Node)(*iter));
          hconfig.type = (*iter).Type();
        }
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
#define ESMC_METHOD "ESMCI::HConfig::getMapKeySize()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getMapKeySize - Get size of the node
//
// !INTERFACE:
int HConfig::getMapKeySize(
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
#define ESMC_METHOD "ESMCI::HConfig::getMapValSize()"
//BOP
// !IROUTINE:  ESMCI::HConfig::getMapValSize - Get size of the node
//
// !INTERFACE:
int HConfig::getMapValSize(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapKeyNull()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapKeyNull - access node type
//
// !INTERFACE:
int HConfig::isMapKeyNull(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapKeyScalar()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapKeyScalar - access node type
//
// !INTERFACE:
int HConfig::isMapKeyScalar(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapKeySequence()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapKeySequence - access node type
//
// !INTERFACE:
int HConfig::isMapKeySequence(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapKeyMap()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapKeyMap - access node type
//
// !INTERFACE:
int HConfig::isMapKeyMap(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapKeyDefined()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapKeyDefined - access node type
//
// !INTERFACE:
int HConfig::isMapKeyDefined(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapValNull()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapValNull - access node type
//
// !INTERFACE:
int HConfig::isMapValNull(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapValScalar()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapValScalar - access node type
//
// !INTERFACE:
int HConfig::isMapValScalar(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapValSequence()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapValSequence - access node type
//
// !INTERFACE:
int HConfig::isMapValSequence(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapValMap()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapValMap - access node type
//
// !INTERFACE:
int HConfig::isMapValMap(
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
#define ESMC_METHOD "ESMCI::HConfig::isMapValDefined()"
//BOP
// !IROUTINE:  ESMCI::HConfig::isMapValDefined - access node type
//
// !INTERFACE:
int HConfig::isMapValDefined(
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
#define ESMC_METHOD "ESMCI::HConfig::iterMapKeyBegin()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterMapKeyBegin - Iterator pointing to first item
//
// !INTERFACE:
HConfig HConfig::iterMapKeyBegin(
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
#define ESMC_METHOD "ESMCI::HConfig::iterMapKeyEnd()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterMapKeyEnd - Iterator pointing to one past the last item
//
// !INTERFACE:
HConfig HConfig::iterMapKeyEnd(
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
#define ESMC_METHOD "ESMCI::HConfig::iterMapValBegin()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterMapValBegin - Iterator pointing to first item
//
// !INTERFACE:
HConfig HConfig::iterMapValBegin(
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
#define ESMC_METHOD "ESMCI::HConfig::iterMapValEnd()"
//BOP
// !IROUTINE:  ESMCI::HConfig::iterMapValEnd - Iterator pointing to one past the last item
//
// !INTERFACE:
HConfig HConfig::iterMapValEnd(
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

  string value = "";

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      value = node->as<string>();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return value;
      }else
        value = iter->as<string>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapKeyString()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapKeyString - Interpret value as string
//
// !INTERFACE:
string HConfig::asMapKeyString(
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
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->first.as<string>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapValString()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapValString - Interpret value as string
//
// !INTERFACE:
string HConfig::asMapValString(
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
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->second.as<string>();
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
#define ESMC_METHOD "ESMCI::HConfig::asI4()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asI4 - Interpret value as I4
//
// !INTERFACE:
ESMC_I4 HConfig::asI4(
//
// !RETURN VALUE:
//  ESMC_I4
//
// !ARGUMENTS:
    int *rc) {            // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as I4
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_I4 value = 0;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      value = node->as<ESMC_I4>();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return value;
      }else
        value = iter->as<int>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapKeyI4()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapKeyI4 - Interpret value as I4
//
// !INTERFACE:
ESMC_I4 HConfig::asMapKeyI4(
//
// !RETURN VALUE:
//  ESMC_I4
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as I4
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_I4 value = 0;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->first.as<ESMC_I4>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapValI4()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapValI4 - Interpret value as I4
//
// !INTERFACE:
ESMC_I4 HConfig::asMapValI4(
//
// !RETURN VALUE:
//  ESMC_I4
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as I4
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_I4 value = 0;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->second.as<ESMC_I4>();
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
#define ESMC_METHOD "ESMCI::HConfig::asI8()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asI8 - Interpret value as I8
//
// !INTERFACE:
ESMC_I8 HConfig::asI8(
//
// !RETURN VALUE:
//  ESMC_I8
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as I8
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_I8 value = 0;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      value = node->as<ESMC_I8>();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return value;
      }else
        value = iter->as<int>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapKeyI8()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapKeyI8 - Interpret value as I8
//
// !INTERFACE:
ESMC_I8 HConfig::asMapKeyI8(
//
// !RETURN VALUE:
//  ESMC_I8
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as I8
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_I8 value = 0;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->first.as<ESMC_I8>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapValI8()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapValI8 - Interpret value as I8
//
// !INTERFACE:
ESMC_I8 HConfig::asMapValI8(
//
// !RETURN VALUE:
//  ESMC_I8
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as I8
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_I8 value = 0;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->second.as<ESMC_I8>();
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
#define ESMC_METHOD "ESMCI::HConfig::asR4()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asR4 - Interpret value as R4
//
// !INTERFACE:
ESMC_R4 HConfig::asR4(
//
// !RETURN VALUE:
//  ESMC_R4
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as R4
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_R4 value = 0.;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      value = node->as<ESMC_R4>();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return value;
      }else
        value = iter->as<int>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapKeyR4()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapKeyR4 - Interpret value as R4
//
// !INTERFACE:
ESMC_R4 HConfig::asMapKeyR4(
//
// !RETURN VALUE:
//  ESMC_R4
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as R4
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_R4 value = 0.;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->first.as<ESMC_R4>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapValR4()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapValR4 - Interpret value as R4
//
// !INTERFACE:
ESMC_R4 HConfig::asMapValR4(
//
// !RETURN VALUE:
//  ESMC_R4
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as R4
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_R4 value = 0.;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->second.as<ESMC_R4>();
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
#define ESMC_METHOD "ESMCI::HConfig::asR8()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asR8 - Interpret value as R8
//
// !INTERFACE:
ESMC_R8 HConfig::asR8(
//
// !RETURN VALUE:
//  ESMC_R8
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as R8
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_R8 value = 0.;

#ifdef ESMF_YAMLCPP
  try{
    if (node)
      value = node->as<ESMC_R8>();
    else
      // iterator
      if (type==YAML::NodeType::Map){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "HConfig object must NOT be map iterator", ESMC_CONTEXT, rc);
        return value;
      }else
        value = iter->as<int>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapKeyR8()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapKeyR8 - Interpret value as R8
//
// !INTERFACE:
ESMC_R8 HConfig::asMapKeyR8(
//
// !RETURN VALUE:
//  ESMC_R8
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as R8
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_R8 value = 0.;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->first.as<ESMC_R8>();
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
#define ESMC_METHOD "ESMCI::HConfig::asMapValR8()"
//BOP
// !IROUTINE:  ESMCI::HConfig::asMapValR8 - Interpret value as R8
//
// !INTERFACE:
ESMC_R8 HConfig::asMapValR8(
//
// !RETURN VALUE:
//  ESMC_R8
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Return value interpreted as R8
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_R8 value = 0.;

#ifdef ESMF_YAMLCPP
  try{
    if ((node==NULL) && (type==YAML::NodeType::Map))
      value = iter->second.as<ESMC_R8>();
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
