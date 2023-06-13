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
void get_dim_from_SHP_file(OGRDataSourceH hDS, char *filename, int &dim);
void process_shapefile(OGRDataSourceH hDS, double *&nodeCoords, int *&nodeIDs, int *&elemIDs, 
		       int *&elemConn, int *&numElemConn,
		       int *totNumElemConn, int *numNodes, int *numElems);
void ESMCI_mesh_create_from_SHP_file();

#endif // ifdef ESMF_GDAL

#endif // ESMCI_GDAL_UTIL_H
