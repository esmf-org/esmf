// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research, 
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
HConfig *HConfig::create(
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
  
  HConfig *hconfig = NULL;
  try{

    // new object
    hconfig = new HConfig;

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::HConfig.", ESMC_CONTEXT, rc);
    return NULL;
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
    HConfig **hconfig){   // in - HConfig object to destroy
    
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
  if (hconfig == ESMC_NULL_POINTER || *hconfig == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to HConfig", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // delete the HConfig object
  delete (*hconfig);            // completely delete the object, free heap
  *hconfig = ESMC_NULL_POINTER; // mark as invalid

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
  try {
    this->doc = YAML::Load(content);
  } catch(...) {
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
  try {
    this->doc = YAML::LoadFile(filename);
  } catch(...) {
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
#define ESMC_METHOD "ESMCI::HConfig::toConfig()"
//BOP
// !IROUTINE:  ESMCI::HConfig::toConfig - fill a Config from HConfig to the level possible
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
//  ESMF routine which attempts to fill Config from HConfig.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;

  std::stringstream debugmsg;
  debugmsg << "gjt in ESMCI::HConfig::toConfig";
  ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);

#ifdef ESMF_YAMLCPP
  try {

    YAML::Node node;
    node = doc["my_file_names"];
    for (auto it=node.begin(); it!=node.end(); it++){
      debugmsg.str("");  //clear
      debugmsg << "doc[]: " << it->as<string>();
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
    }
    string label=string("radius_of_the_earth");
    node = doc[label];
    debugmsg.str("");  //clear
    debugmsg << "doc[]: " << node.as<string>();
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);

#if 0
    FTN_X(f_esmf_configsetstring)(config, node.as<string>().c_str(),
      label.c_str(), &localrc, node.as<string>().size(), label.size());
#endif

  } catch(...) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception in YAML-CPP", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
#endif

  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
