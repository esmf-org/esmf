// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
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

void MBMesh_create(void **mbmpp,
                   int *pdim, int *sdim,
                   ESMC_CoordSys_Flag *coordSys, int *rc);


void MBMesh_addnodes(void **mbmpp, int *num_nodes, int *nodeId,
                     double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                     ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                     int *rc);


void MBMesh_addelements(void **mbmpp,
                        int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                         int *_areaPresent, double *elemArea,
                        int *_coordsPresent, double *elemCoords,
                        int *_num_elemConn, int *elemConn, int *regridConserve,
                        ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                        int *rc);


void MBMesh_destroy(void **mbmpp, int *rc);

void MBMesh_write(void **mbmpp, char *fname, int *rc,
                  ESMCI_FortranStrLenArg nlen);


void MBMesh_createnodedistgrid(void **meshpp, int *ngrid, int *num_lnodes, int *rc);

void MBMesh_createelemdistgrid(void **meshpp, int *egrid, int *num_lelems, int *rc);

void MBMesh_createredistelems(void **src_meshpp, int *num_elem_gids, int *elem_gids,
                              void **output_meshpp, int *rc);

void MBMesh_createredistnodes(void **src_meshpp, int *num_node_gids, int *node_gids,
                              void **output_meshpp, int *rc);

void MBMesh_createredist(void **src_meshpp, int *num_node_gids, int *node_gids,
                            int *num_elem_gids, int *elem_gids,  void **output_meshpp, int *rc);

void MBMesh_checkelemlist(void **meshpp, int *_num_elem_gids, int *elem_gids,
                                           int *rc);

void MBMesh_checknodelist(void **meshpp, int *_num_node_gids, int *node_gids,
                                             int *rc);

void MBMesh_FitOnVM(void **meshpp, VM **new_vm, int *rc);

void MBMesh_getarea(void **mbmpp, int *num_elem, double *elem_areas, int *rc);

void MBMesh_geteleminfointoarray(void *vmbmp,
                                 ESMCI::DistGrid *elemDistgrid, 
                                 int numElemArrays,
                                 int *infoTypeElemArrays, 
                                 ESMCI::Array **elemArrays, 
                                 int *rc);

void MBMesh_GetElemCreateInfo(MBMesh *mesh,
                              ESMCI::InterArray<int> *elemIds,
                              ESMCI::InterArray<int> *elemTypes,
                              ESMCI::InterArray<int> *elemConn,
                              ESMCI::InterArray<int> *elemMask,
                              ESMCI::InterArray<ESMC_R8> *elemArea,
                              ESMCI::InterArray<ESMC_R8> *elemCoords, int *rc){
void MBMesh_getlocalcoords(void **meshpp, double *ncoords,
                               int *_orig_sdim, int *rc);

void MBMesh_getlocalelemcoords(void **meshpp, double *ecoords,
                               int *_orig_sdim, int *rc);

void MBMesh_serialize(void **mbmpp, char *buffer, int *length, 
                      int *offset, ESMC_InquireFlag *inquireflag, int *rc,
                      ESMCI_FortranStrLenArg buffer_l);

void MBMesh_deserialize(void **mbmpp, char *buffer, int *offset, int *rc,
                        ESMCI_FortranStrLenArg buffer_l);

void MBMesh_turnonelemmask(void **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc);
void MBMesh_turnoffelemmask(void **mbmpp, int *rc);

void MBMesh_turnonnodemask(void **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc);
void MBMesh_turnoffnodemask(void **meshpp, int *rc);

EntityType get_entity_type(int pdim, int etype);

int ElemType2NumNodes(int pdim, int sdim, int etype);

#endif // ESMF_MOAB

#endif // ESMCI_Mesh_Glue_h
