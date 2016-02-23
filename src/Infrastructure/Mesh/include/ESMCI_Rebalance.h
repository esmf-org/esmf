// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Rebalance_h
#define ESMCI_Rebalance_h

namespace ESMCI {

class Mesh;
class CommReg;

// Re load balance the mesh.
// We only provide rebalance at the granularity of the Genesis mesh.
// Returns false if no rebalancing is needed.
//
// Mesh must be in a consistent state before this is called
bool Rebalance(Mesh &mesh);


// Or you can do this in two steps. 1) get the comm. 2) run the comm
void GetRebalanceComm(Mesh &mesh, CommReg &comm);
bool Rebalance(Mesh &mesh, CommReg &comm);

} // namespace

#endif
