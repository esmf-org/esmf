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

#ifndef ESMCI_ESMFMESH_UTIL_H
#define ESMCI_ESMFMESH_UTIL_H

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


void get_coordDim_from_ESMFMesh_file(int pioFileDesc, char *filename, PIO_Offset &coordDim);

void get_elementCount_from_ESMFMesh_file(int pioFileDesc, char *filename, PIO_Offset &elementCount);

void get_nodeCount_from_ESMFMesh_file(int pioFileDesc, char *filename, PIO_Offset &nodeCount);

void get_coordsys_from_ESMFMesh_file(int pioFileDesc, char *filename, ESMC_CoordSys_Flag &coord_sys);

void get_elemConn_info_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, PIO_Offset elementCount, int num_elem, int *elem_ids, 
                                          int &totNumElementConn, int *&numElementConn, int *&elementConn);

void get_nodeCoords_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                       PIO_Offset nodeCount, PIO_Offset coordDim, 
                                       int num_node, int *node_ids, 
                                       double *&nodeCoords);

void get_nodeMask_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                     PIO_Offset nodeCount, 
                                     int num_node, int *node_ids, 
                                     int *&nodeMask);

void get_elementMask_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                        PIO_Offset elementCount, 
                                        int num_elem, int *elem_ids, 
                                        int *&elementMask);

void get_elementArea_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                        PIO_Offset elementCount, 
                                        int num_elem, int *elem_ids, 
                                        int &areaPresent, double *&elementArea);

void get_centerCoords_from_ESMFMesh_file(int pioSystemDesc, int pioFileDesc, char *filename, 
                                         PIO_Offset elementCount, PIO_Offset coordDim, 
                                         int num_elem, int *elem_ids, 
                                         int &centerCoordsPresent, double *&centerCoords);

void get_origGridRank_from_ESMFMesh_file(int pioFileDesc, char *filename, bool &has_origGridRank, PIO_Offset &origGridRank);

void get_origGridDims_from_ESMFMesh_file(int pioFileDesc, char *filename, bool &has_origGridDims, int *origGridDims);

#endif // ifdef ESMF_PIO

#endif // ESMCI_ESMFMESH_UTIL_H
