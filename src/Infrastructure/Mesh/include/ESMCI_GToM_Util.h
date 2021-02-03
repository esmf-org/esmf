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


#ifndef ESMCI_GToM_Glue_h
#define ESMCI_GToM_Glue_h

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

#define GTOM_BAD_ID (std::numeric_limits<UInt>::max())
#define GTOM_BAD_PROC -1

#define NUM_QUAD_CORNERS 4

  bool get_global_id_from_tile(DistGrid *distgrid, int tile, int *index,
                               int *_gid, bool *_is_local);

  void convert_localDE_to_tile_info(DistGrid *distgrid, int localDE, int *de_index, int *_tile, int *tile_index);

  bool get_global_id_from_localDE(DistGrid *distgrid, int localDE, int *index,
                                  int *_gid, bool *_is_local);

  void calc_corner_offset(Grid *grid, int corner_offset[NUM_QUAD_CORNERS][2]);

  void calc_center_offset(int corner_offset[NUM_QUAD_CORNERS][2], int center_offset[NUM_QUAD_CORNERS][2]);


  void gid_to_proc(int gid, DistGrid *distgrid, int *_proc);



} // namespace

#endif
