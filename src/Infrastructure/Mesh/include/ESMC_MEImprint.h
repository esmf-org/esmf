// $Id: ESMC_MEImprint.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_MEImprint_h
#define ESMC_MEImprint_h

#include <ESMC_MeshObj.h>

#include <string>
#include <vector>

namespace ESMCI {
namespace MESH {

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
} // namespace

#endif
