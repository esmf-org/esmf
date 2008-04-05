//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_GlobalIds_h
#define ESMC_GlobalIds_h

#include <vector>

namespace ESMC {

// Retrieve a list of new global ids, given the set of current ids
void GlobalIds(const std::vector<long> &current_ids,
                          std::vector<long> &new_ids);


} // namespace

#endif
