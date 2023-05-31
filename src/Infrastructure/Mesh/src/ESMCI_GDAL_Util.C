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

#include <string>
#include <ostream>
#include <iterator>
#include <algorithm>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h" //only for the conservative flag in add_elements
#include "Mesh/include/Legacy/ESMCI_MeshVTK.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_MeshUtils.h"
#include "Mesh/include/Legacy/ESMCI_GlobalIds.h"
#include "Mesh/include/ESMCI_MeshRedist.h"
#include "Mesh/include/ESMCI_MeshDual.h"
#include "Mesh/include/ESMCI_Mesh_Glue.h"
#include "Mesh/include/ESMCI_FileIO_Util.h"

// These internal functions can only be used if PIO is available
#ifdef ESMF_SHAPEFILE

// TODO: SWITCH THIS TO SHAPELIB, WHEN WE KNOW WHAT IT'S CALLED
#include <ogr_api.h>
#include <gdal.h>
#include <ogr_srs_api.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
using namespace ESMCI;


// Get the id of mesh topology dummy variable
// TODO: allow the user to specify the name of the mesh topo variable
void get_mesh_topo_id_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_mesh_topo_id_from_SHP_file()"

}


// Get the dimension of the mesh in the SHP file
// (This dimension is both the pdim and orig_sdim of the mesh)
void get_dim_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_dim_from_SHP_file()"

}


// Get the id of the elementConn array
void get_elementConn_id_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementConn_id_from_SHP_file()"

}


void get_elementCount_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementCount_from_SHP_file()"

}


// Get elementConn info out from a SHP file. 
// Note that the output is a 1D array 
// (The elem conn info is collapsed to get rid of unnecessary values)
void get_elementConn_info_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_elementConn_info_from_SHP_file()"

}


// Get the ids of the node coordinate variables
// nodeCoord_ids - must at least be of size dim
void get_nodeCoord_ids_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_nodeCoord_ids_from_SHP_file()"

}


void get_nodeCount_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_nodeCount_from_SHP_file()"

}


// Get coords from SHP format file
void get_coords_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_coords_from_SHP_file()"

}



/* XMRKX */
void get_coordsys_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "_get_coordsys_from_SHP_file()"

}


void get_elemCoord_ids_from_SHP_file(){
#undef ESMC_METHOD
#define ESMC_METHOD "get_elemCoord_ids_from_SHP_file()"

}


// Get mask from a variable in a SHP format file
// This was set up to work for either element or node masks. The variable just has
// to have the correct size, and the global count and ids have to be for 
// the correct entity (either elems or nodes).
void get_mask_from_SHP_file() {
#undef ESMC_METHOD
#define ESMC_METHOD "get_mask_from_SHP_file()"

}

#endif // ifdef ESMF_SHAPEFILE

