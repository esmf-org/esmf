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

#ifndef ESMCI_MBMesh_Glue_h
#define ESMCI_MBMesh_Glue_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <string>
#include <ostream>
#include <iterator>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

#include "Mesh/include/Legacy/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_XGridUtil.h"
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void MBMesh_create(MBMesh **mbmpp,
                   int *pdim, int *sdim,
                   ESMC_CoordSys_Flag *coordSys, int *rc);


void MBMesh_addnodes(MBMesh **mbmpp, int *num_nodes, int *nodeId,
                     double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                     ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                     int *rc);


void MBMesh_addelements(MBMesh **mbmpp,
                        int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                         int *_areaPresent, double *elemArea,
                        int *_coordsPresent, double *elemCoords,
                        int *_num_elemConn, int *elemConn, int *regridConserve,
                        ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                        int *rc);


void MBMesh_destroy(MBMesh **mbmpp, int *rc);

void MBMesh_write(MBMesh **mbmpp, char *fname, int *rc,
                  ESMCI_FortranStrLenArg nlen);


void MBMesh_createnodedistgrid(MBMesh **meshpp, int *ngrid, int *num_lnodes, int *rc);

void MBMesh_createelemdistgrid(MBMesh **meshpp, int *egrid, int *num_lelems, int *rc);

void MBMesh_createredistelems(MBMesh **src_meshpp, int *num_elem_gids, int *elem_gids,
                              MBMesh **output_meshpp, int *rc);

void MBMesh_createredistnodes(MBMesh **src_meshpp, int *num_node_gids, int *node_gids,
                              MBMesh **output_meshpp, int *rc);

void MBMesh_createredist(MBMesh **src_meshpp, int *num_node_gids, int *node_gids,
                            int *num_elem_gids, int *elem_gids,  MBMesh **output_meshpp, int *rc);

void MBMesh_checkelemlist(MBMesh **meshpp, int *_num_elem_gids, int *elem_gids,
                                           int *rc);

void MBMesh_checknodelist(MBMesh **meshpp, int *_num_node_gids, int *node_gids,
                                             int *rc);

void MBMesh_FitOnVM(MBMesh **meshpp, VM **new_vm, int *rc);

void MBMesh_getarea(MBMesh **mbmpp, int *num_elem, double *elem_areas, int *rc);


void MBMesh_GetCentroid(MBMesh *meshp, int *num_elem, double *elem_centroid, int *rc);

void MBMesh_GetDimensions(MBMesh *meshp, int *sdim, int *pdim, int *rc);

void MBMesh_GetElemCount(MBMesh *meshp, int *elemCount, int *rc);

void MBMesh_GetNodeCount(MBMesh *meshp, int *nodeCount, int *rc);

void MBMesh_GetElemConnCount(MBMesh *meshp, int *elemConnCount, int *rc);

void MBMesh_GetElemInfoPresence(MBMesh *meshp, 
                                int *elemMaskIsPresent,
                                int *elemAreaIsPresent,
                                int *elemCoordsIsPresent,
                                int *rc);

void MBMesh_GetElemCreateInfo(MBMesh *mesh,
                              ESMCI::InterArray<int> *elemIds,
                              ESMCI::InterArray<int> *elemTypes,
                              ESMCI::InterArray<int> *elemConn,
                              ESMCI::InterArray<int> *elemMask,
                              ESMCI::InterArray<ESMC_R8> *elemArea,
                              ESMCI::InterArray<ESMC_R8> *elemCoords, int *rc);

void MBMesh_GetNodeInfoPresence(MBMesh *meshp, 
                                int *nodeMaskIsPresent,
                                int *rc);

void MBMesh_GetNodeCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *nodeIds,
                              ESMCI::InterArray<ESMC_R8> *nodeCoords,
                              ESMCI::InterArray<int> *nodeOwners,
                              ESMCI::InterArray<int> *nodeMask,
                              int *rc);

void MBMesh_geteleminfointoarray(MBMesh *vmbmp,
                                 ESMCI::DistGrid *elemDistgrid, 
                                 int numElemArrays,
                                 int *infoTypeElemArrays, 
                                 ESMCI::Array **elemArrays, 
                                 int *rc);

void MBMesh_SetElemCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *elemMask,
                              ESMCI::InterArray<ESMC_R8> *elemArea,
                              int *rc);

void MBMesh_getlocalcoords(MBMesh **meshpp, double *ncoords,
                               int *_orig_sdim, int *rc);

void MBMesh_getlocalelemcoords(MBMesh **meshpp, double *ecoords,
                               int *_orig_sdim, int *rc);

void MBMesh_serialize(MBMesh **mbmpp, char *buffer, int *length, 
                      int *offset, ESMC_InquireFlag *inquireflag, int *rc,
                      ESMCI_FortranStrLenArg buffer_l);

void MBMesh_deserialize(MBMesh **mbmpp, char *buffer, int *offset, int *rc,
                        ESMCI_FortranStrLenArg buffer_l);

void MBMesh_turnonelemmask(MBMesh **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc);
void MBMesh_turnoffelemmask(MBMesh **mbmpp, int *rc);

void MBMesh_turnonnodemask(MBMesh **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc);
void MBMesh_turnoffnodemask(MBMesh **meshpp, int *rc);

void MBMesh_getelemfrac(MBMesh *mbmesh, int *_num_elem, double *elem_fracs, int *rc);

void MBMesh_get_elem_frac_into_Array(MBMesh *mbmesh, ESMCI::Array *array, int *rc);

#endif // ESMF_MOAB

#endif // ESMCI_Mesh_Glue_h
