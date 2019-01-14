// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MEImprint_h
#define ESMCI_MEImprint_h

#include <Mesh/include/Legacy/ESMCI_MeshObj.h>

#include <string>
#include <vector>

namespace ESMCI {

class MasterElementBase;

void MEImprintValSets(const std::string &imname, MeshObj &obj,
               const MasterElementBase &me, std::vector<UInt> &nvalSet, std::vector<UInt> &valSetObj);
// Imprint a mesh object with the necessary contexts to define a field
// A common name should be used when imprinting a family of objects, e.g.
// linear lagrange
// Returns number of distinct contexts found
void MEImprint(const std::string &imname, MeshObj &obj,
               const MasterElementBase &me);

} // namespace

#endif
