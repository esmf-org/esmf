// $Id: ESMCI_GlobalIds.h,v 1.6 2011/01/05 20:05:44 svasquez Exp $
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_GlobalIds_h
#define ESMCI_GlobalIds_h

#include <vector>

namespace ESMCI {

// Retrieve a list of new global ids, given the set of current ids
void GlobalIds(const std::vector<long> &current_ids,
                          std::vector<long> &new_ids);


} // namespace

#endif
