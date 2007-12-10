// $Id: ESMC_GridToMesh.h,v 1.1 2007/12/10 18:09:25 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF GridToMesh C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document petessing.)
//-----------------------------------------------------------------------------
//
#ifndef ESMC_GridToMesh_h
#define ESMC_GridToMesh_h

//-----------------------------------------------------------------------------

namespace ESMC {
class Mesh;
}

namespace ESMCI {

class Grid;

// Create a mesh from the given grid.
void GridToMesh(const ESMCI::Grid &grid, int staggerLoc, ESMC::Mesh &mesh);

} // namespace


#endif
