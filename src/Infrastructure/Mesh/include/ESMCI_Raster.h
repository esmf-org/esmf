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
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_RASTER_H
#define ESMCI_RASTER_H

#include <string>
#include <ostream>
#include <iterator>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_Grid.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void ESMCI_raster_to_mesh_create_info(Grid *raster_grid,
                                      Array *raster_array,
                                      InterArray<int> *raster_mask_values,
                                      int &mesh_pdim, int &mesh_orig_sdim,
                                      ESMC_CoordSys_Flag &mesh_coordSys,
                                      int &mesh_num_nodes,
                                      int *&mesh_node_ids,
                                      double *&mesh_node_coords,
                                      int *&mesh_node_owners,
                                      int &mesh_num_elems,
                                      int *&mesh_elem_ids,
                                      int *&mesh_elem_num_conns,
                                      int *&mesh_elem_conns
                                      );

#endif // ESMCI_RASTER_H
