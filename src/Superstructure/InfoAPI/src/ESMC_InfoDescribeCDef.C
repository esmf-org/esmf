// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMC_InfoDescribeCDef.C"

#include "ESMC.h"
#include "ESMCI_Base.h"
#include "ESMCI_Macros.h"
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

//namespace ESMCI {}  // namespace ESMCI

extern "C" {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_InfoDescribeSearch()"
int ESMC_InfoDescribeSearch(ESMCI::Info *toSearch, char *rootKey, ESMCI::Info *searchCriteria, int &found) {
  int esmc_rc = ESMF_FAILURE;
  try {
    ESMC_CHECK_NULLPTR(toSearch)
    ESMC_CHECK_NULLPTR(searchCriteria)
    std::string local_rootKey(rootKey);
    assert(local_rootKey.at(0)=='/');
    const json &j_search_criteria = searchCriteria->getStorageRef();
    const json &j_to_search = toSearch->getStorageRef();
    std::vector<bool> local_found;
    local_found.reserve(j_search_criteria.size()+10);
    for (json::const_iterator cit=j_search_criteria.cbegin(); cit!=j_search_criteria.cend(); cit++) {
      // We always expect the key to at least exist.
      try {
        json::json_pointer jpkey(local_rootKey+"/"+cit.key());
        if (j_to_search.at(jpkey) == cit.value()) {
          local_found.push_back(true);
        } else {
          local_found.push_back(false);
        }
      }
      ESMF_CATCH_INFO
    }
    found = 1;  //true
    for (auto element : local_found) {
      if (!element) {
        found = 0;  //false
        break;
      }
    }
    esmc_rc = ESMF_SUCCESS;
  }
  ESMC_CATCH_ISOC
  return esmc_rc;
}

}  // extern "C"
