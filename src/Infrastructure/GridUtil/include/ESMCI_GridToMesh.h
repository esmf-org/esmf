// $Id: ESMCI_GridToMesh.h,v 1.2 2010/09/17 03:13:32 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
#ifndef ESMCI_GridToMesh_h
#define ESMCI_GridToMesh_h

//-----------------------------------------------------------------------------

#include <vector>

#include "ESMCI_Grid.h"
#include "Mesh/include/ESMCI_Mesh.h"


namespace ESMCI {

class Mesh;
class Grid;
class Array;

// Create a mesh from the given grid.
 void GridToMesh(const ESMCI::Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, const std::vector<ESMCI::Array*> &arrays, ESMCI::InterfaceInt *maskValuesArg, int *regridConserve);

 void CpMeshDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);
 
 void PutElemAreaIntoArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array);

} // namespace


#endif  // ESMCI_GridToMesh_h
