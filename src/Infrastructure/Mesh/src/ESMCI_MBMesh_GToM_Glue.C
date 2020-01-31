// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_GridToMesh.C"
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------

#include "ESMCI_Mesh_GToM_Glue.h"

#include "ESMCI_Grid.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Ptypes.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_DDir.h"
#include "Mesh/include/ESMCI_MathUtil.h"

#include "Mesh/include/ESMCI_MBMesh.h"
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



#if 0


  //// Node Fields
#define GTOM_NFIELD_MASK 0
#define GTOM_NFIELD_MASK_VAL 1
#define GTOM_NFIELD_COORD 2
#define GTOM_NFIELD_ORIG_COORD 3
#define GTOM_NFIELD_NUM 4
  
  static void create_nfields(Grid *grid, Mesh *mesh,
                             IOField<NodalField> *nfields[GTOM_NFIELD_NUM]) {

    // Init field array to null
    for (int i=0; i<GTOM_NFIELD_NUM; i++) {
      nfields[i]=NULL;
    }

    // Masks
   if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CORNER, ESMC_GRIDITEM_MASK)) {
     nfields[GTOM_NFIELD_MASK] = mesh->RegisterNodalField(*mesh, "mask", 1);
     nfields[GTOM_NFIELD_MASK_VAL] = mesh->RegisterNodalField(*mesh, "node_mask_val", 1);
   }

    // Coords
    if (grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CORNER)) {
                                           
      // Add node coord field
      nfields[GTOM_NFIELD_COORD]=mesh->RegisterNodalField(*mesh, "coordinates", mesh->spatial_dim()); 

      // If not cartesian then add original coordinates field
      if (grid->getCoordSys() != ESMC_COORDSYS_CART) {
        nfields[GTOM_NFIELD_ORIG_COORD]=mesh->RegisterNodalField(*mesh, "orig_coordinates", grid->getDimCount()); 
      }      
    } else {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
         "- Grid does not contain coordinates at staggerloc=ESMF_STAGGERLOC_CORNER. ", ESMC_CONTEXT, &localrc);
      throw localrc;
    }

  }


  // Set node fields in mesh
  static void set_nfields(Grid *grid, Mesh *mesh,
                          IOField<NodalField> *nfields[GTOM_NFIELD_NUM]) {
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
          Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, node_gid);

          // If it doesn't exist then go to next
          if (mi == mesh->map_end(MeshObj::NODE)) {
            continue;
          }

          // Get node 
          MeshObj &node=*mi;

          // Mask
          // Init to 0 (set later in ESMF_FieldRegridStore()
          if (nfields[GTOM_NFIELD_MASK]) {
            double *d=nfields[GTOM_NFIELD_MASK]->data(node);
            *d=0.0;
          }

          // Mask Val
          // Get data from grid
          if (nfields[GTOM_NFIELD_MASK_VAL]) {
            double *d=nfields[GTOM_NFIELD_MASK_VAL]->data(node);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                 ESMC_GRIDITEM_MASK,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // (Cart) Coords
          // Get data from grid
          if (nfields[GTOM_NFIELD_COORD]) {

            // Get original coord
            double orig_coord[ESMF_MAXDIM];
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                  lDE, index, orig_coord);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            // Get data field in mesh
            double *d=nfields[GTOM_NFIELD_COORD]->data(node);

            // Call into coordsys method to convert to Cart
            localrc=ESMCI_CoordSys_ConvertToCart(grid->getCoordSys(),
                                                 grid->getDimCount(),
                                                 orig_coord,  // Input coordinates 
                                                 d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // Original Coords
          // Get data from grid
          if (nfields[GTOM_NFIELD_ORIG_COORD]) {
            double *d=nfields[GTOM_NFIELD_ORIG_COORD]->data(node);
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CORNER,
                                                  lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }
          
        }
      }
    } 
  }


  /// Element fields
#define GTOM_EFIELD_MASK 0
#define GTOM_EFIELD_MASK_VAL 1
#define GTOM_EFIELD_AREA     2
#define GTOM_EFIELD_COORD 3
#define GTOM_EFIELD_ORIG_COORD 4
#define GTOM_EFIELD_FRAC 5
#define GTOM_EFIELD_FRAC2 6
#define GTOM_EFIELD_NUM 7

  // Create element fields on mesh
  static void create_efields(Grid *grid, Mesh *mesh,
                             MEField<> *efields[GTOM_EFIELD_NUM]) {

    // Init field array to null
    for (int i=0; i<GTOM_EFIELD_NUM; i++) {
      efields[i]=NULL;
    }

    // Set context
    Context ctxt; ctxt.flip();


    // Mask
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_MASK)) {
      efields[GTOM_EFIELD_MASK] = mesh->RegisterField("elem_mask",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

      efields[GTOM_EFIELD_MASK_VAL] = mesh->RegisterField("elem_mask_val",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    }

    // Area
    if (grid->hasItemStaggerLoc(ESMCI_STAGGERLOC_CENTER, ESMC_GRIDITEM_AREA)) {
      efields[GTOM_EFIELD_AREA] = mesh->RegisterField("elem_area",
                          MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    }


    // COORDS
    if (grid->hasCoordStaggerLoc(ESMCI_STAGGERLOC_CENTER)) {
                                           
      // Add element coords field
      efields[GTOM_EFIELD_COORD] = mesh->RegisterField("elem_coordinates",
                 MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, mesh->spatial_dim(), true);

      // If not cartesian then add original coordinates field
      if (grid->getCoordSys() != ESMC_COORDSYS_CART) {
        efields[GTOM_EFIELD_ORIG_COORD]= mesh->RegisterField("elem_orig_coordinates",
                   MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, grid->getDimCount(), true);
      }      
    }


    // Fracs are always there
    efields[GTOM_EFIELD_FRAC] = mesh->RegisterField("elem_frac",
                                             MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    efields[GTOM_EFIELD_FRAC2] = mesh->RegisterField("elem_frac2",
                                             MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  }

  // Set element fields in mesh
  static void set_efields(Grid *grid, Mesh *mesh,
                          MEField<> *efields[GTOM_EFIELD_NUM]) {
    int localrc;

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

          // Get associated elem
          Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::ELEMENT, elem_gid);

          // If it doesn't exist then go to next
          if (mi == mesh->map_end(MeshObj::ELEMENT)) {
            continue;
          }

          // Get elem
          MeshObj &elem=*mi;


          // Mask
          // Init to 0 (set later in ESMF_FieldRegridStore()
          if (efields[GTOM_EFIELD_MASK]) {
            double *d=efields[GTOM_EFIELD_MASK]->data(elem);
            *d=0.0;
          }

          // Mask Val
          // Get data from grid
          if (efields[GTOM_EFIELD_MASK_VAL]) {
            double *d=efields[GTOM_EFIELD_MASK_VAL]->data(elem);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_MASK,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // Area
          // Get data from grid
          if (efields[GTOM_EFIELD_AREA]) {
            double *d=efields[GTOM_EFIELD_AREA]->data(elem);
            localrc=grid->getItemInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                 ESMC_GRIDITEM_AREA,
                                                 lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // (Cart) Coords
          // Get data from grid
          if (efields[GTOM_EFIELD_COORD]) {

            // Get original coord
            double orig_coord[ESMF_MAXDIM];
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                  lDE, index, orig_coord);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            // Get data field in mesh
            double *d=efields[GTOM_EFIELD_COORD]->data(elem);

            // Call into coordsys method to convert to Cart
            localrc=ESMCI_CoordSys_ConvertToCart(grid->getCoordSys(),
                                                 grid->getDimCount(), 
                                                 orig_coord,  // Input coordinates 
                                                 d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception

            //printf("id=%d orig_coords=%g %g coords=%g %g %g\n",elem.get_id(),orig_coord[0],orig_coord[1],d[0],d[1],d[2]);
          }


          // Original Coords
          // Get data from grid
          if (efields[GTOM_EFIELD_ORIG_COORD]) {
            double *d=efields[GTOM_EFIELD_ORIG_COORD]->data(elem);
            localrc=grid->getCoordInternalConvert(ESMCI_STAGGERLOC_CENTER,
                                                  lDE, index, d);
            if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
              throw localrc;  // bail out with exception
          }


          // Init fracs
          if (efields[GTOM_EFIELD_FRAC]) {
            double *d=efields[GTOM_EFIELD_FRAC]->data(elem);
            *d=1.0;
          }

          if (efields[GTOM_EFIELD_FRAC2]) {
            double *d=efields[GTOM_EFIELD_FRAC2]->data(elem);
            *d=1.0;
          }
        }

      }
    }
  }


  bool gqcn_debug=false;

  static void _get_quad_corner_nodes_from_tile(Mesh *mesh, DistGrid *cnrDistgrid, int tile, int index[2], int cnr_offset[NUM_QUAD_CORNERS][2],
                                     int *local_node_index, std::vector<MeshObj*> *cnr_nodes, bool *_all_nodes_ok) {
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
    //   if (gqcn_debug) {
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
    MeshObj *node;

    // If a node with this gid already exists, then put into list
    Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, gid);
    if (mi != mesh->map_end(MeshObj::NODE)) {

      // Get node pointer
      node=&*mi;

      // Put into list
      (*cnr_nodes)[i]=node;

      // Go to next iteration
      continue;
    }

    // If we didn't find it then we need to make a new one

    // Create new node in mesh object
    node = new MeshObj(MeshObj::NODE,     // node...
                                   gid,               // unique global id
                                   *local_node_index
                                   );
    // Advance to next node index
    (*local_node_index)++;

    // Set owner to bad proc value and change later
    node->set_owner(BAD_PROC);

    // Eventually need to figure out pole id stuff
    UInt nodeset=0; // UInt nodeset = gni->getPoleID();   // Do we need to partition the nodes in any sets?
    mesh->add_node(node, nodeset);


    // Put into list
    (*cnr_nodes)[i]=node;
  }
}

  static void _get_quad_corner_nodes_from_localDE(Mesh *mesh, DistGrid *cnrDistgrid, int localDE, int de_index[2], int cnr_offset[NUM_QUAD_CORNERS][2],
                                     int *local_node_index, std::vector<MeshObj*> *cnr_nodes, bool *_all_nodes_ok) {

    //// Translate localDE info into tile info ////
    int tile;
    int tile_index[ESMF_MAXDIM];
    convert_localDE_to_tile_info(cnrDistgrid, localDE, de_index, &tile, tile_index);

    // Call into get get quad corner nodes from tile
    get_quad_corner_nodes_from_tile(mesh, cnrDistgrid, tile, tile_index, cnr_offset,
                                     local_node_index, cnr_nodes, _all_nodes_ok);
}

/* XMRKX */

  // Add a node that you need on this PET, but that doesn't necessarily have a local element to host it
  // ...although don't add it if there isn't a cell anyplace to host it.
  static void _force_add_node(int node_gid, int cnr_de_index[2], Mesh *mesh, int localDE, 
                              int center_offset[NUM_QUAD_CORNERS][2], DistGrid *centerDistgrid, 
                              int cnr_offset[NUM_QUAD_CORNERS][2], DistGrid *cnrDistgrid,
                              int *local_node_index, int *local_elem_index, std::vector<MeshObj*> *cnr_nodes,
                              const MeshObjTopo *elem_topo, bool *added_node) {

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
      if (elem_gid == BAD_ID) continue;
    
      // Assuming that element isn't already in mesh, because otherwise it's nodes
      // (e.g. the node to be added) would already be in the mesh. Therefore, 
      // not checking to see if element is in mesh.
      
      // Add the node here?? Or just trust that _get_quad_corner_nodes will do it????
      // LET _get_quad_... do it, that way if it can't find all the nodes it won't 
      // add the element or the forced node. 

      // Get quad corner nodes for this element, this should add the node (among others)
      bool all_nodes_ok=true;
      _get_quad_corner_nodes_from_tile(mesh, cnrDistgrid, tile, center_index, cnr_offset,
                             local_node_index, cnr_nodes, &all_nodes_ok);

      // DEBUG
      //if (node_gid == 7) {
      //  printf("%d# in _force_add id=%d all_nodes_ok=%d\n",Par::Rank(),node_gid,all_nodes_ok);
      //  gqcn_debug=false;
      //}


      // If we didn't get all the nodes, then move onto next 
      if (!all_nodes_ok) continue;

      // Create Element
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT,    // Mesh equivalent of Cell
                                  elem_gid,            // unique global id
                                  (*local_elem_index)++
                                  );

      // Add element
      UInt block_id = 1;  // Any reason to use different sets for cells?
      mesh->add_element(elem, *cnr_nodes, block_id, elem_topo);

      // Get proc based on gid
      int proc;
      _gid_to_proc(elem_gid, centerDistgrid, &proc);

      // Set proc
      elem->set_owner(proc);

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

      // DEBUG OUTPUT
      //printf("%d# in _force_add elem_id=%d proc=%d owner=%d is_local=%d\n",Par::Rank(),elem_gid,proc,elem->get_owner(),GetAttr(*elem).is_locally_owned());

      // Indicate that we've successfully added the node (via adding the elem that contains it) 
      *added_node=true;

      // If we've successfully added the element, then leave
      break;
    }
  }



#endif

 /* XMRKX */


void MBMesh_GridToMeshCell(const Grid &grid_,
                           void **out_meshpp, 
                           int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "GridToMeshCell()"
  Trace __trace("GridToMeshCell(const Grid &grid_, ESMCI::Mesh &mesh)");

  try {

    printf("MBMesh_GridToMeshCell(): Beg\n");

  // local error code
  int localrc;

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
 *out_meshpp=(void *)mesh;


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

 // Space for corner verts
 EntityHandle cnr_nodes[NUM_QUAD_CORNERS];

 // printf("offset=[%d %d] [%d %d]  [%d %d]  [%d %d]\n",cnr_offset[0][0],
 //cnr_offset[0][1],cnr_offset[1][0],cnr_offset[1][1],cnr_offset[2][0],cnr_offset[2][1],
 //cnr_offset[3][0], cnr_offset[3][1]);

#if 0

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


 /// STOPPED HERE /// 

       // Get Element corner nodes
       bool all_nodes_ok=true;
       _get_quad_corner_nodes_from_localDE(mesh, cnrDistgrid, lDE, de_index, cnr_offset, &local_node_index,
                                           cnr_nodes, &all_nodes_ok);

       // If we didn't get all the nodes, then go to next element
       if (!all_nodes_ok) continue;

       // Get element id
       int elem_gid;
       bool is_local;
       if (!get_global_id_from_localDE(centerDistgrid, lDE, de_index,
                           &elem_gid, &is_local)) {
         continue; // If we can't find a global id, then just skip
       }


       // Create Element
       MeshObj *elem = new MeshObj(MeshObj::ELEMENT,    // Mesh equivalent of Cell
                                   elem_gid,            // unique global id
                                   local_elem_index++
                                   );

       // Add element
       UInt block_id = 1;  // Any reason to use different sets for cells?
       mesh->add_element(elem, cnr_nodes, block_id, elem_topo);

       // Set owner to the current processor
       elem->set_owner(Par::Rank());

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
       Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, node_gid);

       // If it exists then go to next
       if (mi != mesh->map_end(MeshObj::NODE)) continue;

       // Otherwise add
       bool added_node=false;
       _force_add_node(node_gid, de_index, mesh, lDE, 
                       center_offset, centerDistgrid, 
                       cnr_offset, cnrDistgrid,
                       &local_node_index, &local_elem_index, &cnr_nodes,
                       elem_topo, &added_node);

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
       Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::NODE, node_gid);

       // If it doesn't exist then go to next
       if (mi == mesh->map_end(MeshObj::NODE)) {
         continue;
       }

       // Get node pointer
       MeshObj *node=&*mi;

       // Set owner to the current processor
       node->set_owner(Par::Rank());

     }
   }
 }

 // Loop through Mesh nodes setting the remaining owners
 MeshDB::iterator ni = mesh->node_begin(), ne = mesh->node_end();
 for (; ni != ne; ++ni) {
   MeshObj *node=&*ni;

   // Get node global id
   int gid=node->get_id();

   // get node owner
   int owner=node->get_owner();

   // If the owner is already set then skip
   if (owner != GTOM_BAD_PROC) continue;

   // Get proc based on gid
   int proc;
   gid_to_proc(gid, cnrDistgrid, &proc);

   // Set owner
   node->set_owner(proc);
 }

 // Add node Fields
 IOField<NodalField> *nfields[GTOM_NFIELD_NUM];
 create_nfields(grid, mesh, nfields);

 // Set node fields
 // (This has to happen before mesh is committed below)
 set_nfields(grid, mesh, nfields);

 // Add element Fields
 MEField<> *efields[GTOM_EFIELD_NUM];
 create_efields(grid, mesh, efields);

 // Finalize the Mesh
 mesh->build_sym_comm_rel(MeshObj::NODE);
 mesh->Commit();

 // Set element fields
 set_efields(grid, mesh, efields);

 // Halo fields, so entities with an owner on another processor
 // will have the correct value
 {
   std::vector<MEField<>*> fds;

   Mesh::FieldReg::MEField_iterator fi = mesh->Field_begin(), fe = mesh->Field_end();

   for (; fi != fe; ++fi) fds.push_back(&*fi);

   mesh->HaloFields(fds.size(), &fds[0]);
 }


#if 0
 // DEBUG OUTPUT
  {
     // Output list of nodes with info
    MeshDB::iterator ni = mesh->node_begin(), ne = mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node=*ni;

      //      printf("%d# node: id=%d owner=%d is_local=%d data_index=%d \n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned(),node.get_data_index());
      printf("%d# node: id=%d owner=%d is_local=%d is_active=%d is_shared=%d data_index=%d\n",Par::Rank(),node.get_id(),node.get_owner(),GetAttr(node).is_locally_owned(),GetAttr(node).GetContext().is_set(Attr::ACTIVE_ID), GetAttr(node).is_shared(),node.get_data_index());

    }

   printf("\n");

     // Output list of elems with info
    MeshDB::iterator ei = mesh->elem_begin_all(), ee = mesh->elem_end_all();
    for (; ei != ee; ++ei) {
      MeshObj &elem=*ei;

      printf("%d# elem: id=%d owner=%d is_local=%d is_active=%d is_shared=%d data_index=%d\n",Par::Rank(),elem.get_id(),elem.get_owner(),GetAttr(elem).is_locally_owned(),GetAttr(elem).GetContext().is_set(Attr::ACTIVE_ID), GetAttr(elem).is_shared(),elem.get_data_index());
    }
  }
#endif

#endif


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
