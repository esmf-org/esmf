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

#ifndef ESMCI_Mesh_XGrid_Glue_h
#define ESMCI_Mesh_XGrid_Glue_h

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void ESMCI_xgridregrid_create(Mesh **meshsrcpp, Mesh **meshdstpp, 
                              Mesh **mesh,
                              int *compute_midmesh,
                              int *regridMethod, 
                              int *unmappedaction,
                              ESMC_CoordSys_Flag *coordSys,
                              int *nentries, ESMCI::TempWeights **tweights,
                              int*rc);

void ESMCI_meshmerge(Mesh **srcmeshpp, Mesh **dstmeshpp,
                     Mesh **meshpp,
                     int*rc);

void ESMCI_Mesh_XGrid_calc_wgts_from_side_mesh(Mesh *src_side_mesh, Mesh *dst_xgrid_mesh,
                                               int *nentries, ESMCI::TempWeights **tweights,
                                               int*rc);

void ESMCI_Mesh_XGrid_calc_wgts_to_side_mesh(Mesh *src_xgrid_mesh, Mesh *dst_side_mesh,
                                             int *nentries, ESMCI::TempWeights **tweights,
                                             int*rc);

#endif
