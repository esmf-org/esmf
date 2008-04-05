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
#ifndef ESMC_Rebalance_h
#define ESMC_Rebalance_h

namespace ESMC {

class Mesh;
class CommReg;

// Re load balance the mesh.
// We only provide rebalance at the granularity of the Genesis mesh.
// Returns false if no rebalancing is needed.
//
// Mesh must be in a consistent state before this is called
bool Rebalance(Mesh &mesh);

} // namespace

#endif
