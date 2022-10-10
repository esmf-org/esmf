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

#ifndef ESMCI_MESH_FILEIO_H
#define ESMCI_MESH_FILEIO_H

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

void ESMCI_mesh_create_from_file(char *filename, 
                                 ESMC_FileFormat_Flag fileformat, 
                                 bool convert_to_dual, bool add_user_area, 
                                 ESMC_CoordSys_Flag coord_sys, 
                                 ESMC_MeshLoc_Flag maskFlag, 
                                 char *maskVarName,
                                 ESMCI::DistGrid *node_distgrid, 
                                 ESMCI::DistGrid *elem_distgrid, 
                                 Mesh **out_mesh, int *rc);

#endif // ESMCI_MESH_FILEIO_H
