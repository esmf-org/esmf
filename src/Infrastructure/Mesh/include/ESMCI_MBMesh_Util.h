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

#ifndef ESMCI_MBMesh_Util_h
#define ESMCI_MBMesh_Util_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include "ESMCI_PointList.h"
#include "Util/include/ESMC_Util.h"
#include "Util/include/ESMCI_F90Interface.h"

#include <vector>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

void MBMesh_get_gid(MBMesh *mbmp, EntityHandle eh, int *gid);

void MBMesh_get_elem_coords_3D_ccw(MBMesh *mbmp, EntityHandle elem, 
                                   int max_num_nodes, double *tmp_coords, 
                                   int *num_nodes, double *coords);
void MBMesh_get_elem_coords(MBMesh *mbmp, EntityHandle elem, int max_num_nodes, int *num_nodes, double *coords);

void MBMesh_get_elem_centroid(MBMesh *mbmp, EntityHandle elem, double *centroid);

void MBMesh_get_local_elem_gids(MBMesh *mbmp, std::vector<UInt> &egids);

// expects pcoords in domain [-1,1] and translates to [0,1]
// useful for translating pcoords from MOAB to ESMF domain
void translate(double *pcoords);

ESMCI::PointList *MBMesh_to_PointList(MBMesh *mesh, ESMC_MeshLoc_Flag meshLoc, ESMCI::InterArray<int> *maskValuesArg, int *rc);

// Traslate element type to number of nodes around the element
int MBMesh_ElemType2NumNodes(int pdim, int etype);

// Go from number of nodes to ESMf entity type
int MBMesh_num_nodes_to_esmf_etype(int pdim, int num_corner_nodes);

//Get the entity type from parametric dimension and ESMF etype
EntityType MBMesh_get_entity_type(int pdim, int etype);


// Add nodes in a group
void MBMesh_add_nodes_in_a_group(MBMesh *mbmp,   // Mesh to add elems to
                                 int num_nodes, // the number of new nodes to add
                                 int *node_ids,   // id for each new node
                                 double *node_coords, // node original coords
                                 int *node_orig_pos, // orig pos of each new node (if NULL just order starting from 0)
                                 int *node_owners, // owner for each new node
                                 int *node_mask_vals, // optional elem mask value (if NULL ignored)
                                 int *node_masks // optional elem mask (if NULL ignored)
                                 );



// Vector based wrapper for the above
void MBMesh_add_nodes_in_a_group(MBMesh *mbmp,   // Mesh to add elems to
                                 std::vector<int> &node_ids,   // id for each new node
                                 std::vector<double> &node_coords, // node original coords
                                 std::vector<int> &node_orig_pos, // orig pos of each new node (if NULL just order starting from 0)
                                 std::vector<int> &node_owners, // owner for each new node
                                 std::vector<int> &node_mask_vals, // optional elem mask value (if NULL ignored)
                                 std::vector<int> &node_masks // optional elem mask (if NULL ignored)
                                 );


// This routine takes in a some  element creation information, and sees if any elements need to be split
void MBMesh_detect_split_elems(
                               // In
                               int pdim, 
                               int num_elems,                                           
                               int *elemType, 
                               int *elemConn, 
                               
                               // Out
                               bool &is_split_local,
                               bool &is_split
                               );


// This routine should only be run when split elems have been detected by the above. 
// It takes in a set of element creation information, and outputs the new split element creation information
void MBMesh_generate_info_for_split_elems(
                                          // In
                                          bool is_split_local,
                                          int pdim, 
                                          int orig_sdim, 
                                          int sdim,
                                          int num_elems,
                                          int *elemId,
                                          int *elemType, 
                                          int *orig_pos, 
                                          int *elemMask,
                                          int areaPresent, double *elemArea,
                                          int elemCoordsPresent, double *elemCoords,
                                          int num_elemConn,
                                          int *elemConn,
                                          double *nodeCoords, // node coords in orig. node order
                                          
                                          // Out
                                          int  &max_non_split_id,
                                          std::map<int,int> &split_to_orig_id, 
                                          std::map<int,double> &split_id_to_frac,
                                          int  &num_elems_wsplit,
                                          int *&elemId_wsplit,
                                          int *&elemType_wsplit,
                                          int *&orig_pos_wsplit,
                                          int *&elemMask_wsplit,
                                          double *&elemArea_wsplit,
                                          double *&elemCoords_wsplit,
                                          int *&elemConn_wsplit
                                          );



// Take a set of element information (e.g. from the above),
// and then add the elements for the information in chunks to the mesh
// (adding the elems in chunks is more efficient in MOAB).
void MBMesh_add_elems_in_groups_by_type(MBMesh *mbmp, int localPet, 
                                        int num_elems,
                                        int *elemId, 
                                        int *elemType,
                                        int *orig_pos,
                                        int *elemMaskVal,
                                        int *elemMask,
                                        int areaPresent, double *elemArea,
                                        int elemCoordsPresent, double *elemCoords,
                                        int *elemConn                              
                                        );

// Vector based wrapper for the above
void MBMesh_add_elems_in_groups_by_type(MBMesh *mbmp, 
                                        int all_elems_owner, // all new elems owner will be set to this
                                        std::vector<int> &elem_ids,
                                        std::vector<int> &elem_types,
                                        std::vector<int> &elem_orig_pos,
                                        std::vector<int> &elem_mask_vals,
                                        std::vector<int> &elem_masks,
                                        std::vector<double> &elem_areas,
                                        std::vector<double> &elem_coords,
                                        std::vector<int> &elem_conns
                                        );





#endif // ESMF_MOAB

#endif
