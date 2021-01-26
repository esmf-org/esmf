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
#define ESMC_FILENAME "ESMCI_MBMesh_GToM_Glue.C"
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------

//#include "ESMCI_Mesh_GToM_Glue.h"

#include "ESMCI_Grid.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_MathUtil.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_GToM_Glue.h"
#include "Mesh/include/ESMCI_GToM_Util.h"

#include <limits>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>


// Some xlf compilers don't define this
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

//#define G2M_DBG

// using namespace ESMCI;

namespace ESMCI {

  //================== GTOMCELL ========================

#undef  ESMC_METHOD
#define ESMC_METHOD "MBMEsh_GridToMeshCell()"
 /* XMRKX */

  // DEBUG
  //bool mbmmesh_gqcn_debug=false;

  static void _get_quad_corner_nodes_from_tile(MBMesh *mesh, 
              std::map<int,EntityHandle> *gid_to_node_map, 
              DistGrid *cnrDistgrid, int tile, int index[2], 
              int cnr_offset[NUM_QUAD_CORNERS][2], int *local_node_index, 
              EntityHandle *cnr_nodes, bool *_all_nodes_ok) {
  // Init output
  *_all_nodes_ok=true;


  // Loop getting global ids
  int cnr_gids[NUM_QUAD_CORNERS];
  for (int i=0; i<NUM_QUAD_CORNERS; i++) {

    // calc index of corner
    int cnr_index[2];
    cnr_index[0]=index[0]+cnr_offset[i][0];
    cnr_index[1]=index[1]+cnr_offset[i][1];

    // DEBUG
    //   if (mbmesh_gqcn_debug) {
    //  printf("%d# in _get_quad_corner_nodes BEFORE cnr_index=%d %d \n",Par::Rank(),cnr_index[0],cnr_index[1]);
    //}

    // Get global id, if we can't then leave
    bool is_local;
    if (!get_global_id_from_tile(cnrDistgrid, tile, cnr_index,
                        cnr_gids+i, &is_local)) {
      *_all_nodes_ok=false;
      return;
    }

    //if (gqcn_debug) {
    //  printf("%d# in _get_quad_corner_nodes AFTER cnr_index=%d %d \n",Par::Rank(),cnr_index[0],cnr_index[1]);
    //}

  }

  // Convert gids to nodes
  for (int i=0; i<NUM_QUAD_CORNERS; i++) {

    // get gid of this corner
    int gid=cnr_gids[i];

    // declare node that we're looking for
    EntityHandle node;

    // If a node with this gid doesn't already exist, then create, otherwise
    // get the one from the map
    std::map<int,EntityHandle>::iterator mi =  gid_to_node_map->find(gid);
    if (mi == gid_to_node_map->end()) {

      // We don't necessarily know the coordinates yet, so 
      // use {0,0,0} and set later.
      double node_coords[3]={0.0, 0.0, 0.0};
      
      // If we didn't find it then we need to make a new one
      node=mesh->add_node(node_coords, gid, *local_node_index, GTOM_BAD_PROC);
      
      // Add to map
      (*gid_to_node_map)[gid]=node;
      
      // Advance to next node index
      (*local_node_index)++;

    } else {
      // Get node pointer
      node=mi->second;
    }

    // Put into list
    cnr_nodes[i]=node;
  }
}

  static void _get_quad_corner_nodes_from_localDE(MBMesh *mesh, 
              std::map<int,EntityHandle> *gid_to_node_map, DistGrid *cnrDistgrid, 
              int localDE, int de_index[2], 
              int cnr_offset[NUM_QUAD_CORNERS][2], int *local_node_index, 
              EntityHandle *cnr_nodes, bool *_all_nodes_ok) {

    //// Translate localDE info into tile info ////
    int tile;
    int tile_index[ESMF_MAXDIM];
    convert_localDE_to_tile_info(cnrDistgrid, localDE, de_index, &tile, tile_index);

    // Call into get get quad corner nodes from tile
    _get_quad_corner_nodes_from_tile(mesh, gid_to_node_map, cnrDistgrid, tile, tile_index, cnr_offset,
                                     local_node_index, cnr_nodes, _all_nodes_ok);
}



  // Add a node that you need on this PET, but that doesn't necessarily have a local element to host it
  // ...although don't add it if there isn't a cell anyplace to host it.
  static void _force_add_node(int node_gid, int cnr_de_index[2], MBMesh *mesh, std::map<int,EntityHandle> *gid_to_node_map,
                              int localDE, int center_offset[NUM_QUAD_CORNERS][2], DistGrid *centerDistgrid, 
                              int cnr_offset[NUM_QUAD_CORNERS][2], DistGrid *cnrDistgrid,
                              int *local_node_index, int *local_elem_index, 
                              EntityType etype, bool *added_node) {

    // Init indicator variable for whether node was successfully added
    *added_node=false;

    // Need to use tile version of _get_global_id() because if the localDE is empty (as
    // is likely the case with the center localDE here, then _get_global_id_from_localDE() 
    // won't work. Therefore, find tile & tile index for corner localDE and index
    int tile;
    int cnr_tile_index[ESMF_MAXDIM];
    convert_localDE_to_tile_info(cnrDistgrid, localDE, cnr_de_index, &tile, cnr_tile_index);

    // Loop to find a suitable element host
    for (int i=0; i<NUM_QUAD_CORNERS; i++) {

      // calc index of corner
      int center_index[2];
      center_index[0]=cnr_tile_index[0]+center_offset[i][0]; 
      center_index[1]=cnr_tile_index[1]+center_offset[i][1];

      // Get global id, if we can't then not a suitable element
      bool is_local;
      int elem_gid=GTOM_BAD_ID;
      if (!get_global_id_from_tile(centerDistgrid, tile, center_index,
                          &elem_gid, &is_local)) continue;
    
      // DEBUG
      //if (node_gid == 7) {
      //  printf("%d# in _force_add id=%d elem_id=%d\n",Par::Rank(),node_gid,elem_gid);
      //  gqcn_debug=true;
      // }


      // If didn't find a good one, then move onto next 
      if (elem_gid == GTOM_BAD_ID) continue;
    
      // Assuming that element isn't already in mesh, because otherwise it's nodes
      // (e.g. the node to be added) would already be in the mesh. Therefore, 
      // not checking to see if element is in mesh.
      
      // Add the node here?? Or just trust that _get_quad_corner_nodes will do it????
      // LET _get_quad_... do it, that way if it can't find all the nodes it won't 
      // add the element or the forced node. 

      // Get quad corner nodes for this element, this should add the node (among others)
      bool all_nodes_ok=true;
      EntityHandle cnr_nodes[NUM_QUAD_CORNERS];
      _get_quad_corner_nodes_from_tile(mesh, gid_to_node_map, cnrDistgrid,
                                       tile, center_index, cnr_offset,
                                       local_node_index, cnr_nodes, &all_nodes_ok);

      // DEBUG
      //if (node_gid == 7) {
      //  printf("%d# in _force_add id=%d all_nodes_ok=%d\n",Par::Rank(),node_gid,all_nodes_ok);
      //  gqcn_debug=false;
      //}


      // If we didn't get all the nodes, then move onto next 
      if (!all_nodes_ok) continue;

      // Get proc based on gid
      int proc;
      gid_to_proc(elem_gid, centerDistgrid, &proc);


      // Create new element
      mesh->add_elem(etype, NUM_QUAD_CORNERS, cnr_nodes, 
                     elem_gid, *local_elem_index, proc);


      // Advance to next elem index
      (*local_elem_index)++;


      // DON'T NEED THIS RIGHT NOW, BUT KEEP IN CASE WE EVENTUALY ADD THIS 
      // KIND OF CAPABILITY TO MBMesh
#if 0
      // If not local, turn off OWNED_ID, ACTIVE_ID, and turn on SHARED_ID. This is 
      // what happens for ghost elems, so it makes sense to do that here. 
      // (This seems to happen automatically for nodes, but not elems)
      if (proc != Par::Rank()) {
        // Setup for changing attribute
        const Context &ctxt = GetMeshObjContext(*elem);
        Context newctxt(ctxt);

        // Clear OWNED_ID since we're not owned
        newctxt.clear(Attr::OWNED_ID);

        // Clear ACTIVE_ID since we're not locally owned 
        newctxt.clear(Attr::ACTIVE_ID);

        // turn on SHARED_ID since owned someplace else 
        newctxt.set(Attr::SHARED_ID);

        // If attribute has changed change in elem
        if (newctxt != ctxt) {
          Attr attr(GetAttr(*elem), newctxt);
          mesh->update_obj(elem, attr);
        }
      }
#endif

      // DEBUG OUTPUT
      //printf("%d# in _force_add elem_id=%d proc=%d owner=%d is_local=%d\n",Par::Rank(),elem_gid,proc,elem->get_owner(),GetAttr(*elem).is_locally_owned());

      // Indicate that we've successfully added the node (via adding the elem that contains it) 
      *added_node=true;

      // If we've successfully added the element, then leave
      break;
    }
  }




  //// Create Node fields/tags in mesh
  static void _create_nfields(Grid *grid, MBMesh *mesh) {

    // Masks
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CORNER, ESMC_GRIDITEM_MASK)) {
      mesh->setup_node_mask();
    }

  }


  // Set node fields in mesh
  static void _set_nfields(Grid *grid, MBMesh *mesh, std::map<int,EntityHandle> *gid_to_node_map) {
    int localrc;

    // Get distgrid for the center staggerloc
    DistGrid *cnrDistgrid;
    grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CORNER, &cnrDistgrid);

    // Get localDECount
    int localDECount=cnrDistgrid->getDELayout()->getLocalDeCount();
                       
    // Loop again adding information to nodes
    for (int lDE=0; lDE < localDECount; lDE++) {

      // Get Corner DE bounds
      int cnr_ubnd[ESMF_MAXDIM];
      int cnr_lbnd[ESMF_MAXDIM];
      grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
      grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);


      // Loop over bounds
      int index[2];
      int nonde_index[2];
      for (int i0=cnr_lbnd[0]; i0<=cnr_ubnd[0]; i0++){
        for (int i1=cnr_lbnd[1]; i1<=cnr_ubnd[1]; i1++){

          // Set index
          index[0]=i0;
          index[1]=i1;

          // De based index
          int de_index[2];
          de_index[0]=i0-cnr_lbnd[0];
          de_index[1]=i1-cnr_lbnd[1];

          // Get node global id
          int node_gid;
          bool is_local;
          if (!get_global_id_from_localDE(cnrDistgrid, lDE, de_index,
                              &node_gid, &is_local)) {
            continue; // If we can't find a global id, then just skip
          }

          // Only set node information if this is the owner
          if (!is_local) continue;

          // Get associated node
          std::map<int,EntityHandle>::iterator mi =  gid_to_node_map->find(node_gid);
          
          // If it doesn't exist, then go to next
          if (mi == gid_to_node_map->end()) continue;

          // Get node pointer
          EntityHandle node=mi->second;

          // Set Mask
          if (mesh->has_node_mask) {
              int mask_val;
              localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                   ESMC_GRIDITEM_MASK,
                                                   lDE, index, &mask_val);
              if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
                throw localrc;  // bail out with exception

              mesh->set_node_mask_val(node,mask_val);
          }


          // Set Coords

          // Get original coords from Grid
          double orig_coord[ESMF_MAXDIM];
          localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                lDE, index, orig_coord);
          if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
            throw localrc;  // bail out with exception

          // Set coords in Mesh
          mesh->set_node_coords(node, orig_coord);
        }
      }
    } 

  }


  // Create element fields on mesh
  static void _create_efields(Grid *grid, MBMesh *mesh) {

    // Mask
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_MASK)) {
      mesh->setup_elem_mask();
    }

    // Area
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_AREA)) {
      mesh->setup_elem_area();
    }

    // elem coords
    if (grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CENTER)) {                     
      mesh->setup_elem_coords();
    }

    // Fracs are added in mesh create, because they are always on
  }

  // Set element fields in mesh
  static void _set_efields(Grid *grid, MBMesh *mesh) {
    int localrc;

    // Get elems
    Range elems;
    int merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // Loop and set up gid_to_elem_map
    std::map<int,EntityHandle> gid_to_elem_map;
    for (Range::iterator it=elems.begin(); it !=elems.end(); it++) {
      EntityHandle elem=*it;

      // Get node global id
      int gid=mesh->get_gid(elem);

      // Add to map
      gid_to_elem_map[gid]=elem;
    }

    // Get distgrid for the center staggerloc
    DistGrid *centerDistgrid;
    grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

    // Get localDECount
    int localDECount=centerDistgrid->getDELayout()->getLocalDeCount();

    // Loop setting fields on each element
    for (int lDE=0; lDE < localDECount; lDE++) {

      // Get Center DE bounds
      int ubnd[ESMF_MAXDIM];
      int lbnd[ESMF_MAXDIM];
      grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
      grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

      // Loop over bounds
      int index[2];
      int de_index[2];
      for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
        for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

          // Set index
          index[0]=i0;
          index[1]=i1;

          // De based index
          de_index[0]=i0-lbnd[0];
          de_index[1]=i1-lbnd[1];

          // Get elem global id
          int elem_gid;
          bool is_local;
          if (!get_global_id_from_localDE(centerDistgrid, lDE, de_index,
                              &elem_gid, &is_local)) {
            continue; // If we can't find a global id, then just skip
          }

          // Only set elem information if this is the owner
          if (!is_local) continue;

          // Get associated node
          std::map<int,EntityHandle>::iterator mi =  gid_to_elem_map.find(elem_gid);
          
          // If it doesn't exist, then go to next
          if (mi == gid_to_elem_map.end()) continue;

          // Get node pointer
          EntityHandle elem=mi->second;

          // Mask Val
          if (mesh->has_elem_mask) {
            int mask_val;
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_MASK,
                                                 lDE, index, &mask_val);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
            
            // Set elem mask value
            mesh->set_elem_mask_val(elem, mask_val);
          }


          // Area
          if (mesh->has_elem_area) {
            double elem_area;
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_AREA,
                                                 lDE, index, &elem_area);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            // Set elem area
            mesh->set_elem_area(elem, elem_area);
          }


          // Elem Coords
          if (mesh->has_elem_coords) {

            // Get original coord
            double orig_coords[ESMF_MAXDIM];
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                  lDE, index, orig_coords);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            // Set elem coords
            mesh->set_elem_coords(elem, orig_coords);

            //printf("id=%d orig_coords=%g %g coords=%g %g %g\n",elem.get_id(),orig_coord[0],orig_coord[1],d[0],d[1],d[2]);
          }

        }
      }
    }

  }


 /* XMRKX */


void MBMesh_GridToMeshCell(const Grid &grid_,
                           MBMesh **out_meshpp, 
                           int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GridToMeshCell()"
  Trace __trace("GridToMeshCell(const Grid &grid_, ESMCI::Mesh &mesh)");

  try {

    //    printf("MBMesh_GridToMeshCell(): Beg\n");

  // local error code
  int localrc;
  
  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception


  // Get grid pointer
  Grid *grid = &(const_cast<Grid&>(grid_));

  // The Grid needs to have corner coordinates
  if (!grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CORNER)) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "To use this method the Grid must contain coordinates at corner staggerloc.", ESMC_CONTEXT, &localrc);
    throw localrc;
  }

  // The Grid currently can't be arbitrarily distributed
  if (grid->getDecompType() != ESMC_GRID_NONARBITRARY) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "To use this method the Grid can't be arbitrarily distributed.", ESMC_CONTEXT, &localrc);
    throw localrc;
  }

 // Get dimCount
 int dimCount=grid->getDimCount();

 // Only supporting 2D right now
 if (dimCount != 2) {
   Throw() << "This method currently only supports 2D Grids";
 }


 // Create Mesh
 // parameteric and original spatial dim are both dimCount in a grid. 
 MBMesh *mesh = new MBMesh(dimCount, dimCount, grid->getCoordSys());

 // Set output mesh
 *out_meshpp = mesh;

 // Get distgrid for the center staggerloc
 DistGrid *centerDistgrid;
 grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CENTER, &centerDistgrid);

 // Get centerLocalDECount
 int centerLocalDECount=centerDistgrid->getDELayout()->getLocalDeCount();

 // Get distgrid for the corner staggerloc
 DistGrid *cnrDistgrid;
 grid->getStaggerDistgrid(ESMCI_STAGGERLOC_CORNER, &cnrDistgrid);

 // Get cnrLocalDECount
 int cnrLocalDECount=cnrDistgrid->getDELayout()->getLocalDeCount();

 // Setup topology
 // For 2D this is a quadrilateral
 EntityType etype=MBQUAD;

 // Get index offsets for the corners around a center
 int cnr_offset[NUM_QUAD_CORNERS][2];
 calc_corner_offset(grid, cnr_offset);

 // Get index offsets for the centers around a corner
 int center_offset[NUM_QUAD_CORNERS][2];
 calc_center_offset(cnr_offset, center_offset);

 // Space for corner nodes
 EntityHandle cnr_nodes[NUM_QUAD_CORNERS];

 // printf("offset=[%d %d] [%d %d]  [%d %d]  [%d %d]\n",cnr_offset[0][0],
 //cnr_offset[0][1],cnr_offset[1][0],cnr_offset[1][1],cnr_offset[2][0],cnr_offset[2][1],
 //cnr_offset[3][0], cnr_offset[3][1]);

 // Map from gid to nodes
 std::map<int,EntityHandle> gid_to_node_map;

 // Loop over center DEs adding cells
 int local_node_index=0;
 int local_elem_index=0;
 for (int lDE=0; lDE < centerLocalDECount; lDE++) {

   // Get Center DE bounds
   int ubnd[ESMF_MAXDIM];
   int lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(centerDistgrid, lDE, ubnd);
   grid->getDistExclusiveLBound(centerDistgrid, lDE, lbnd);

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);

   // DEBUG
   //printf("%d# lDE=%d CENTER lbnd=%d %d ubnd=%d %d\n",Par::Rank(),lDE,lbnd[0],lbnd[1],ubnd[0],ubnd[1]);
   //printf("%d# lDE=%d CORNER lbnd=%d %d ubnd=%d %d\n",Par::Rank(),lDE,cnr_lbnd[0],cnr_lbnd[1],cnr_ubnd[0],cnr_ubnd[1]);

   // Loop over bounds
   for (int i0=lbnd[0]; i0<=ubnd[0]; i0++){
     for (int i1=lbnd[1]; i1<=ubnd[1]; i1++){

       // Set index
       int index[2];
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-lbnd[0];
       de_index[1]=i1-lbnd[1];

       // Get Element corner nodes
       bool all_nodes_ok=true;
       _get_quad_corner_nodes_from_localDE(mesh, &gid_to_node_map,
                                           cnrDistgrid, lDE, de_index, cnr_offset, 
                                           &local_node_index, cnr_nodes, &all_nodes_ok);

       // If we didn't get all the nodes, then go to next element
       if (!all_nodes_ok) continue;

       // Get element id
       int elem_gid;
       bool is_local;
       if (!get_global_id_from_localDE(centerDistgrid, lDE, de_index,
                           &elem_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }

       // Create new element
       mesh->add_elem(etype, NUM_QUAD_CORNERS, cnr_nodes, 
                      elem_gid, local_elem_index, localPet);


       // Advance to next elem index
       local_elem_index++;

       // DEBUG
       //if (elem_gid==9) {
       //   printf("gid=%d i=%d %d lDE=%d nodes=%d %d %d %d \n",elem_gid,i0,i1,lDE,cnr_nodes[0]->get_id(),cnr_nodes[1]->get_id(),cnr_nodes[2]->get_id(),cnr_nodes[3]->get_id());
       //}
     }
   }
 }



 // Loop adding local nodes that weren't added as part of local cell creation
 for (int lDE=0; lDE < cnrLocalDECount; lDE++) {

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);


   // Loop over bounds
   int index[2];
   int nonde_index[2];
   for (int i0=cnr_lbnd[0]; i0<=cnr_ubnd[0]; i0++){
     for (int i1=cnr_lbnd[1]; i1<=cnr_ubnd[1]; i1++){

       // Set index
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-cnr_lbnd[0];
       de_index[1]=i1-cnr_lbnd[1];

       // Get node global id
       int node_gid;
       bool is_local;
       if (!get_global_id_from_localDE(cnrDistgrid, lDE, de_index,
                           &node_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }

       // Only set node information if this is the owner
       if (!is_local) continue;

       // Get associated node
       std::map<int,EntityHandle>::iterator mi =  gid_to_node_map.find(node_gid);

       // If it exists then go to next
       if (mi != gid_to_node_map.end()) continue;


       // Otherwise add
       bool added_node=false;
       _force_add_node(node_gid, de_index, mesh, &gid_to_node_map, 
                       lDE, center_offset, centerDistgrid, 
                       cnr_offset, cnrDistgrid,
                       &local_node_index, &local_elem_index, 
                       etype, &added_node);
     }
   }
 }


 // Loop setting local node owners
 for (int lDE=0; lDE < cnrLocalDECount; lDE++) {

   // Get Corner DE bounds
   int cnr_ubnd[ESMF_MAXDIM];
   int cnr_lbnd[ESMF_MAXDIM];
   grid->getDistExclusiveUBound(cnrDistgrid, lDE, cnr_ubnd);
   grid->getDistExclusiveLBound(cnrDistgrid, lDE, cnr_lbnd);


   // Loop over bounds
   int index[2];
   int nonde_index[2];
   for (int i0=cnr_lbnd[0]; i0<=cnr_ubnd[0]; i0++){
     for (int i1=cnr_lbnd[1]; i1<=cnr_ubnd[1]; i1++){

       // Set index
       index[0]=i0;
       index[1]=i1;

       // De based index
       int de_index[2];
       de_index[0]=i0-cnr_lbnd[0];
       de_index[1]=i1-cnr_lbnd[1];

       // Get node global id
       int node_gid;
       bool is_local;
       if (!get_global_id_from_localDE(cnrDistgrid, lDE, de_index,
                           &node_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }

       // Only set node information if this is the owner
       if (!is_local) continue;


       // Get associated node
       std::map<int,EntityHandle>::iterator mi =  gid_to_node_map.find(node_gid);

       // If it doesn't exist, then go to next
       if (mi == gid_to_node_map.end()) continue;

       // Get node pointer
       EntityHandle node=mi->second;

       // Set owner to the current processor
       mesh->set_owner(node, localPet);
     }
   }
 }


 // Get range of nodes
 // TODO: maybe hide this in a MBMesh wrapper? 
 Range nodes;
 int merr=mesh->mesh->get_entities_by_dimension(0, 0, nodes);
 if (merr != MB_SUCCESS) {
   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
 }

 // Loop through Mesh nodes setting the remaining owners
 for (Range::iterator it=nodes.begin(); it != nodes.end(); it++) {
   EntityHandle node=*it;

   // Get node global id
   int gid=mesh->get_gid(node);

   // get node owner
   int owner=mesh->get_owner(node);

   // If the owner is already set then skip
   if (owner != GTOM_BAD_PROC) continue;

   // Get proc based on gid
   int proc;
   gid_to_proc(gid, cnrDistgrid, &proc);

   // Set owner to the current processor
   mesh->set_owner(node, proc);
 }

 // Add node fields
 _create_nfields(grid, mesh);

 // Set node fields
 _set_nfields(grid, mesh, &gid_to_node_map);

 // Don't need gid_to_node_map anymore, so clear it to save memory
 gid_to_node_map.clear();

 // Add element fields
 _create_efields(grid, mesh);

 // Set element fields
 _set_efields(grid, mesh);

 // Setup parallel information
 mesh->setup_parallel();

// Do halo communication on all appropriate node tags
 mesh->halo_comm_nodes_all_tags(true);

 // Finalize nodes and elems
 mesh->finalize_nodes();
 mesh->finalize_elems();


 // Debug output
 // mesh->debug_output_nodes();

 // Do halo communication on all appropriate elem tags
 mesh->halo_comm_elems_all_tags();

 // Debug output
 //mesh->debug_output_elems();

  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT,rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "UNKNOWN", ESMC_CONTEXT,rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                  "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }


// Set successful return code
 if (rc!=NULL) *rc = ESMF_SUCCESS;
}

} // namespace
