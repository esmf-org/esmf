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

#ifndef ESMCI_GDAL_UTIL_H
#define ESMCI_GDAL_UTIL_H

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

// These internal functions can only be used if GDAL is available
#ifdef ESMF_GDAL
#include <ogr_api.h>
#include <gdal.h>
#include <ogr_srs_api.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

// general routines
void open_();

// shapefile routines
void ESMCI_GDAL_SHP_get_dim_from_file(OGRDataSourceH hDS, char *filename, int &dim);
void ESMCI_GDAL_process_shapefile_serial(OGRDataSourceH hDS, double *&nodeCoords, 
			      int *&nodeIDs, int *&elemIDs, 
			      int *&elemConn, int *&numElemConn,
			      int *totNumElemConn, int *numNodes, 
			      int *numElems);
void ESMCI_GDAL_process_shapefile_distributed(
// inputs
		       OGRDataSourceH hDS, 
		       int *nFeatures,
		       int *&featureIDs,
		       int *&globFeatureIDs,
// outputs
		       double *&nodeCoords, 
		       std::vector<int> &nodeIDs, 
		       std::vector<int> &elemIDs, 
		       std::vector<int> &elemConn,
		       std::vector<double> &elemCoords,
		       std::vector<int> &numElemConn, 
		       int *totNumElemConn, 
		       int *nNodes, 
		       int *nElems);
void ESMCI_mesh_create_from_SHP_file();

void ESMCI_GDAL_SHP_get_feature_info(OGRDataSourceH hDS, int *nFeatures, int *&FeatureIDs);

void convert_global_elem_conn_to_local_elem_info(int num_local_elem, int tot_num_elem_conn, int *num_elem_conn, int *global_elem_conn, int*& local_elem_conn);
#endif // ifdef ESMF_GDAL

#endif // ESMCI_GDAL_UTIL_H
