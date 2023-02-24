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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_HConfig_H
#define ESMCI_HConfig_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::HConfig - Hierarchical Config
//              
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt HConfig} class and declares
// method signatures (prototypes).  The companion file {\tt ESMCI_HConfig.C}
// contains the full code (bodies) for the {\tt HConfig} methods.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI {
  class HConfig;
}

#include "ESMCI_Config.h"

#ifdef ESMF_YAMLCPP
#include "yaml-cpp/yaml.h"
#endif

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  class HConfig{

    private:
#ifdef ESMF_YAMLCPP
      YAML::Node *node;
#endif

    public:
      // depend on default constructor and destructor
      static HConfig *create(int *rc=NULL);
      static int destroy(HConfig **hconfig);
      int load(const std::string& content);
      int loadFile(const std::string& filename);
      int isNull(bool *flag);
      int isScalar(bool *flag);
      int isSequence(bool *flag);
      int isMap(bool *flag);
      int isDefined(bool *flag);
//      HConfig *iterBegin(int *rc=NULL);
//      HConfig *iterEnd(int *rc=NULL);
      int toConfig(ESMCI_Config **config);
  };   // class HConfig

} // namespace ESMCI

#endif  // ESMCI_HConfig_H
