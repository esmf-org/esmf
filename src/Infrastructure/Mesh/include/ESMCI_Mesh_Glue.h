// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_Mesh_Glue_h
#define ESMCI_Mesh_Glue_h

#include <string>
#include <ostream>
#include <iterator>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Array.h"

#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h" //only for the conservative flag in add_elements
#include "Mesh/include/Legacy/ESMCI_MeshVTK.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_MeshUtils.h"
#include "Mesh/include/Legacy/ESMCI_GlobalIds.h"
#include "Mesh/include/Legacy/ESMCI_FindPnts.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/Legacy/ESMCI_Phedra.h"
#include "Mesh/include/ESMCI_XGridUtil.h"
#include "Mesh/include/Legacy/ESMCI_MeshMerge.h"
#include "Mesh/include/ESMCI_MeshRedist.h"
#include "Mesh/include/ESMCI_MeshDual.h"
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;


void ESMCI_meshcreate(Mesh **meshpp,
                      int *pdim, int *sdim,
                      ESMC_CoordSys_Flag *coordSys, int *rc);

void ESMCI_meshaddnodes(Mesh **meshpp, int *num_nodes, int *nodeId,
                            double *nodeCoord, InterArray<int> *nodeOwner, InterArray<int> *nodeMaskII,
                            ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                            int *rc);

void ESMCI_meshwrite(Mesh **meshpp, char *fname, int *rc,
                     ESMCI_FortranStrLenArg nlen);

void ESMCI_meshwritewarrays(Mesh **meshpp, char *fname, ESMCI_FortranStrLenArg nlen,
                            int num_nodeArrays, ESMCI::Array **nodeArrays, 
                            int num_elemArrays, ESMCI::Array **elemArrays, 
                            int *rc);

void ESMCI_meshaddelements(Mesh **meshpp,
                           int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                           int *_areaPresent, double *elemArea,
                           int *_coordsPresent, double *elemCoords,
                           int *_elemConn_size, int *elemConn, 
                           ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                           int *rc);

/**
 * Routines for reading in a test VTK mesh to fortran arrays (for testing the array interface)
 */
void ESMCI_meshvtkheader(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc,
                         ESMCI_FortranStrLenArg nlen);

void ESMCI_meshvtkbody(char *filename, int *nodeId, double *nodeCoord,
                    int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc,
                       ESMCI_FortranStrLenArg nlen);

void ESMCI_meshdestroy(Mesh **meshpp, int *rc);

void ESMCI_meshfreememory(Mesh **meshpp, int *rc);


void ESMCI_meshget(Mesh **meshpp, int *num_nodes, int *num_elements, int *rc);

void ESMCI_MeshGetNodeCount(Mesh *meshpp, int *nodeCount, int *rc);
void ESMCI_MeshGetElemCount(Mesh *meshpp, int *elemCount, int *rc);
void ESMCI_MeshGetElemConnCount(Mesh *meshpp, int *elemConnCount, int *rc);
void ESMCI_MeshGetOwnedNodeCount(Mesh *meshpp, int *nodeCount, int *rc);
void ESMCI_MeshGetOwnedElemCount(Mesh *meshpp, int *elemCount, int *rc);

void ESMCI_MeshGetElemInfoPresence(Mesh *mesh, 
                                   int *elemMaskIsPresent,
                                   int *elemAreaIsPresent,
                                   int *elemCoordsIsPresent,
                                   int *rc);

void ESMCI_MeshGetElemCreateInfo(Mesh *mesh,
                                 ESMCI::InterArray<int> *elemIds,
                                 ESMCI::InterArray<int> *elemTypes,
                                 ESMCI::InterArray<int> *elemConn,
                                 ESMCI::InterArray<int> *elemMask,
                                 ESMCI::InterArray<ESMC_R8> *elemArea,
                                 ESMCI::InterArray<ESMC_R8> *elemCoords, int *rc);

void ESMCI_MeshSetElemInfo(Mesh *mesh,
                           ESMCI::InterArray<int> *elemMask,
                           ESMCI::InterArray<ESMC_R8> *elemArea,
                           int *rc);

void ESMCI_MeshGetNodeInfoPresence(Mesh *mesh, 
                                   int *nodeMaskIsPresent,
                                   int *rc);

void ESMCI_MeshGetNodeCreateInfo(Mesh *mesh,
                                 ESMCI::InterArray<int> *nodeIds,
                                 ESMCI::InterArray<ESMC_R8> *nodeCoords,
                                 ESMCI::InterArray<int> *nodeOwners,
                                 ESMCI::InterArray<int> *nodeMask,
                                 int *rc);

void ESMCI_meshcreatenodedistgrid(Mesh **meshpp, DistGrid **dg, int *rc);

void ESMCI_meshcreateelemdistgrid(Mesh **meshpp, DistGrid **dg, int *rc);

void ESMCI_meshserialize(Mesh **meshpp,
                         char *buffer, int *length, int *offset,
                         ESMC_InquireFlag *inquireflag, int *rc,
                         ESMCI_FortranStrLenArg buffer_l);

void ESMCI_meshdeserialize(Mesh **meshpp,
                           char *buffer, int *offset, int *rc,
                           ESMCI_FortranStrLenArg buffer_l);

void ESMCI_meshfindpnt(Mesh **meshpp, int *unmappedaction, int *dimPnts, int *numPnts,
                       double *pnts, int *pets, int *rc);

void ESMCI_getlocalelemcoords(Mesh **meshpp, double *elemCoord, int *_orig_sdim, int *rc);

void ESMCI_getlocalcoords(Mesh **meshpp, double *nodeCoord, int *_orig_sdim, int *rc);

void ESMCI_geteleminfointoarray(Mesh *mesh, 
                                ESMCI::DistGrid *elemDistgrid, 
                                int numElemArrays,
                                int *infoTypeElemArrays, 
                                ESMCI::Array **elemArrays, 
                                int *rc);

void ESMCI_getconnectivity(Mesh **meshpp, double *connCoord, int *nodesPerElem,
                                   int *_orig_sdim, int *rc);

void ESMCI_meshgetarea(Mesh **meshpp, int *num_elem, double *elem_areas, int *rc);

void ESMCI_meshgetdimensions(Mesh **meshpp, int *sdim, int *pdim, int *rc);

void ESMCI_meshgetcentroid(Mesh **meshpp, int *num_elem, double *elem_centroid, int *rc);

void ESMCI_meshgetfrac(Mesh **meshpp, int *_num_elem, double *elem_fracs, int *rc);

void ESMCI_meshgetfrac2(Mesh **meshpp, int *num_elem, double *elem_fracs, int *rc);

// Interface to internal code to triangulate a polygon
// Input is: pnts (the polygon of size numPnts*sdim
//           td a temporary buffer of the same size as pnts
//           ti a temporary buffer of size numPnts
// Output is: tri_ind, which are the 0-based indices of the triangles
//             making up the triangulization. tri_ind should be of size 3*(numPnts-2).
//
void ESMCI_triangulate(int *pdim, int *sdim, int *numPnts,
                       double *pnts, double *td, int *ti, int *triInd, int *rc);

void ESMCI_meshturnoncellmask(Mesh **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc);

// Turn OFF masking
void ESMCI_meshturnoffcellmask(Mesh **meshpp, int *rc);

////////////
void ESMCI_meshturnonnodemask(Mesh **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc);

// Turn OFF masking
void ESMCI_meshturnoffnodemask(Mesh **meshpp, int *rc);

////////////

void ESMCI_get_polygon_area(int *spatialdim, int *nedges,
                            double *points, double *area, int *rc);

void ESMCI_meshcreatefrommeshes(Mesh **meshapp, Mesh **meshbpp, Mesh **meshpp,
                                ESMC_MeshOp_Flag * meshop, double * threshold, int *rc);


void ESMCI_meshcreateredistelems(Mesh **src_meshpp, int *num_elem_gids, int *elem_gids,
                                 Mesh **output_meshpp, int *rc);


void ESMCI_meshcreateredistnodes(Mesh **src_meshpp, int *num_node_gids, int *node_gids,
                                 Mesh **output_meshpp, int *rc);


void ESMCI_meshcreateredist(Mesh **src_meshpp, int *num_node_gids, int *node_gids,
                            int *num_elem_gids, int *elem_gids,  Mesh **output_meshpp, int *rc);

// This method verifies that nodes in node_gids array are the same as the local nodes in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of nodes in both cases are the same and that every
// entry in node_gids is contained in meshpp
void ESMCI_meshchecknodelist(Mesh **meshpp, int *_num_node_gids, int *node_gids,
                             int *rc);


// This method verifies that elems in elem_gids array are the same as the local elems in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of elems in both cases are the same and that every
// entry in elem_gids is contained in meshpp
void ESMCI_meshcheckelemlist(Mesh **meshpp, int *_num_elem_gids, int *elem_gids,
                             int *rc);

// Interface to internal code to convert coords from spherical in degrees to Cartesian
// Input is: lon, lat - spherical coordinates in degrees
// Output is: x,y,z - Cartesian coordinates
//
void ESMCI_sphdeg_to_cart(double *lon, double *lat,
                          double *x, double *y, double *z, int *rc);

// This method sets the pole values so a 2D Mesh from a SCRIP grid can still be used in regrid with poles
void ESMCI_meshsetpoles(Mesh **meshpp, int *_pole_obj_type, int *_pole_val, int *_min_pole_gid, int *_max_pole_gid,
                        int *rc);

void ESMCI_meshcreatedual(Mesh **src_meshpp, Mesh **output_meshpp, int *rc);

void ESMCI_MeshFitOnVM(Mesh **meshpp, VM **new_vm,int *rc);

void ESMCI_meshcreate_easy_elems(Mesh **meshpp,
                                 int *pdim, int *sdim,
                                 int *num_elems,
                                 InterArray<int> *elemIdsII,
                                 int *elemTypes,
                                 InterArray<int> *elemMaskII,
                                 int *num_elemCorners,
                                 double *elemCornerCoords,
                                 int *has_elemArea,
                                 double *elemArea,
                                 int *has_elemCoords,
                                 double *elemCoords,
                                 ESMC_CoordSys_Flag *coordSys, int *rc);

#endif // ESMCI_Mesh_Glue_h
