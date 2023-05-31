// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_SHAPEFILE_UTIL_H
#define ESMCI_SHAPEFILE_UTIL_H

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

// These internal functions can only be used if SHAPEFILE is available
#ifdef ESMF_SHAPEFILE

// TODO: Change when we know name of shapefile include file
#include <ogr_api.h>
#include <gdal.h>
#include <ogr_srs_api.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void open_();
void get_mesh_topo_id_from_SHP_file();
void get_dim_from_SHP_file();
void get_elementConn_id_from_SHP_file();
void get_elementCount_from_SHP_file();
void get_elementConn_info_from_SHP_file();
void get_nodeCoord_ids_from_SHP_file();
void get_nodeCount_from_SHP_file();
void get_coords_from_SHP_file();
void get_coordsys_from_SHP_file();
void get_elemCoord_ids_from_SHP_file();
void get_mask_from_SHP_file();
void ESMCI_mesh_create_from_SHP_file();

#endif // ifdef ESMF_SHAPEFILE

#endif // ESMCI_SHAPEFILE_UTIL_H
