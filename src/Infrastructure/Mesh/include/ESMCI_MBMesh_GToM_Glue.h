// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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


#ifndef ESMCI_MBMesh_GToM_Glue_h
#define ESMCI_MBMesh_GToM_Glue_h

#include "ESMCI_Grid.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_MathUtil.h"

#include <limits>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>


namespace ESMCI {

  void MBMesh_GridToMeshCell(const Grid &grid_, MBMesh **out_meshpp, int *rc);

} // namespace

#endif
