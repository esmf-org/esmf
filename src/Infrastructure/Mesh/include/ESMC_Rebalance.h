// $Id: ESMC_Rebalance.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_Rebalance_h
#define ESMC_Rebalance_h

namespace ESMCI {
namespace MESH {

class Mesh;
class CommReg;

// Re load balance the mesh.
// We only provide rebalance at the granularity of the Genesis mesh.
// Returns false if no rebalancing is needed.
//
// Mesh must be in a consistent state before this is called
bool Rebalance(Mesh &mesh);

} // namespace
} // namespace

#endif
