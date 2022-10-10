// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_FILEIO_UTIL_H
#define ESMCI_FILEIO_UTIL_H

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

#include "Mesh/include/ESMCI_Mesh.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void convert_global_elem_conn_to_local_node_and_elem_info(int num_local_elem, int tot_num_elem_conn, int *num_elem_conn, int *global_elem_conn,
                                                          int &num_node, int*& node_ids, int*& local_elem_conn);

void divide_ids_evenly_as_possible(int num_ids, int local_pet, int pet_count, int &min_id, int &max_id);

void get_ids_from_distgrid(ESMCI::DistGrid *distgrid, std::vector<int> &ids);

void get_ids_divided_evenly_across_pets(int num_ids, int local_pet, int pet_count, std::vector<int> &ids);

void convert_coords_between_coord_sys(ESMC_CoordSys_Flag coord_sys_from, 
                                      ESMC_CoordSys_Flag coord_sys_to, 
                                      int coord_dim, int num_coords, double *coords);

void convert_numElementConn_to_elementType(int pdim, int num_elem, int *num_elem_conn, int*& elem_type);

#endif // ESMCI_FILEIO_UTIL_H
