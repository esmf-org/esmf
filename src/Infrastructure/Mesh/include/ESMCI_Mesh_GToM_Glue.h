// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------


#include "ESMCI_Grid.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"
#include "Mesh/include/Legacy/ESMCI_IOField.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/Legacy/ESMCI_Phedra.h"

#include <limits>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>


namespace ESMCI {

  void ESMCI_GridToMesh(const Grid &grid_, int staggerLoc, 
                      const std::vector<ESMCI::Array*> &arrays, 
                      ESMCI::InterArray<int> *maskValuesArg,
                      int *regridConserve, Mesh **out_meshpp, int *rc);

  void ESMCI_GridToMeshCell(const Grid &grid_, 
                            const std::vector<ESMCI::Array*> &arrays, 
                            Mesh **out_meshpp, int *rc);

  void CpMeshElemDataToArrayCell(Grid *grid, ESMCI::Mesh *mesh, ESMCI::Array *array, MEField<> *dataToArray);

  void PutElemAreaIntoArrayCell(Grid *grid, ESMCI::Mesh *mesh, ESMCI::Array *array);

#if 0
  void ESMCI_GridToMesh(const Grid &grid_, int staggerLoc, ESMCI::Mesh &mesh, 
                        const std::vector<ESMCI::Array*> &arrays, ESMCI::InterArray<int> *maskValuesArg,
                        int *regridConserve);

  // Only works for scalar data right now, but would be pretty easy to add more dimensions 
  void ESMCI_CpMeshDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);

  // Assumes array is on center staggerloc of grid
  void ESMCI_CpMeshElemDataToArray(Grid &grid, int staggerloc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);

  void ESMCI_PutElemAreaIntoArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array);
#endif

} // namespace

