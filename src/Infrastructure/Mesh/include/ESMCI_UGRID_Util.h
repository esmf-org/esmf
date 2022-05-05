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

#ifndef ESMCI_UGRID_UTIL_H
#define ESMCI_UGRID_UTIL_H

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

// These internal functions can only be used if PIO is available
#ifdef ESMF_PIO

#include <pio.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void get_mesh_topo_id_from_UGRID_file(int pioFileDesc, char *filename, int &mesh_topo_id);

void get_dim_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, 
                             int &dim);

void get_elementConn_id_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, int dim, 
                                   int &elementConn_id);

void get_elementCount_from_UGRID_file(int pioFileDesc, char *filename, int elementConn_id, 
                                      PIO_Offset &elementCount);

void get_elementConn_info_from_UGRID_file(int pioSystemDesc, int pioFileDesc, char *filename, int elementConn_id, 
                                          PIO_Offset elementCount, int num_elem, int *elem_ids, 
                                          int &totNumElementConn, int *&numElementConn, int *&elementConn);

void get_nodeCoord_ids_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, int dim, 
                                       int *nodeCoord_ids);

void get_nodeCount_from_UGRID_file(int pioFileDesc, char *filename, int dim, int *nodeCoord_ids, PIO_Offset &nodeCount);

void get_coords_from_UGRID_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                ESMC_CoordSys_Flag coord_sys, int dim, int *coordVar_ids, 
                                PIO_Offset global_count, 
                                int num_ids, int *ids, 
                                double *&coords);


void get_coordsys_from_UGRID_file(int pioFileDesc, char *filename, 
                                  int dim, int *nodeCoord_ids, 
                                  ESMC_CoordSys_Flag &coord_sys);

void get_elemCoord_ids_from_UGRID_file(int pioFileDesc, char *filename, int mesh_topo_id, int dim, int *elemCoord_ids, int &centerCoordsPresent);


void get_mask_from_UGRID_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                              char *maskVarName, 
                              PIO_Offset global_count, 
                              int num_ids, int *ids, 
                              int *&mask);

#endif // ifdef ESMF_PIO

#endif // ESMCI_UGRID_UTIL_H

