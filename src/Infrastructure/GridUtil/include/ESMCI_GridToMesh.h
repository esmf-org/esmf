// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
#include "ESMCI_PointList.h"
#include "Mesh/include/ESMCI_Mesh.h"


namespace ESMCI {

#if 0

class Mesh;
class Grid;
class Array;

// Create a mesh from the given grid.
 void GridToMesh(const ESMCI::Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, const std::vector<ESMCI::Array*> &arrays, ESMCI::InterfaceInt *maskValuesArg, int *regridConserve);

 void CpMeshDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);

 void CpMeshElemDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);

 void PutElemAreaIntoArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array);

#endif
 void GridToPointList(ESMCI::Grid &grid, ESMC_StaggerLoc staggerLoc, ESMCI::InterfaceInt *maskValuesArg, ESMCI::PointList **_pl, int *localrc);

} // namespace


#endif  // ESMCI_GridToMesh_h
