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

#include <Mesh/include/Regridding/ESMCI_MeshRegrid.h>
#include <Mesh/include/Legacy/ESMCI_MeshRead.h>
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Regridding/ESMCI_CreepFill.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {


// This method converts unmasked Mesh nodes to a PointList
 static void _create_pointlist_from_mesh_nodes(Mesh *mesh, UInt pole_constraint_id,PointList **_pointlist) {

   // Coord Pointer
   MEField<> *coord_ptr = mesh->GetCoordField();

   // Mask Pointer
   MEField<> *mask_ptr = mesh->GetField("mask");

   // Loop through counting local unmasked nodes
   int num_points=0;
   MeshDB::iterator ni = mesh->node_begin(), ne = mesh->node_end();
   for (; ni != ne; ++ni) {
     MeshObj &node=*ni;

     // If not local, then go on to next node
     if (!GetAttr(node).is_locally_owned()) continue;

     // If this node is a newly added pole, then skip, because
     // it's not a point in the original grid
     if (GetMeshObjContext(node).is_set(pole_constraint_id)) {
       continue;
     }

     // If masked, then go on to next
     if (mask_ptr) {
       double *msk=mask_ptr->data(node);
       if (*msk > 0.5) continue;
     }

     // Count point
     num_points++;
   }

   // Create PointList
   PointList *pointlist = new ESMCI::PointList(num_points, mesh->spatial_dim());

   // Loop through filling PointList
   ni = mesh->node_begin();
   for (; ni != ne; ++ni) {
     MeshObj &node=*ni;

     // If not local, then go on to next node
     if (!GetAttr(node).is_locally_owned()) continue;

     // If this node is a newly added pole, then skip, because
     // it's not a point in the original grid
     if (GetMeshObjContext(node).is_set(pole_constraint_id)) {
       continue;
     }

     // If masked, then go on to next
     if (mask_ptr) {
       double *msk=mask_ptr->data(node);
       if (*msk > 0.5) continue;
     }

     // Get pointer to coords of this node
     double *coords=coord_ptr->data(node);

     // Add point
     pointlist->add(node.get_id(), coords);
   }


  // Output
  *_pointlist=pointlist;

}

static void _create_pointlist_of_mesh_nodes_not_in_wmat(Mesh *mesh, WMat &wts, PointList **_missing_points) {

   // Coord Pointer
   MEField<> *coord_ptr = mesh->GetCoordField();

   // Mask Pointer
   MEField<> *mask_ptr = mesh->GetField("mask");

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Get mesh node iterator that goes through in order of id
  Mesh::MeshObjIDMap::const_iterator ni=mesh->map_begin(MeshObj::NODE), ne=mesh->map_end(MeshObj::NODE);

  // Count all points that don't have weights
  int num_missing=0;
  for (; ni != ne; ++ni) {
    const MeshObj &node=*ni;

    // Skip non local nodes
    if (!GetAttr(node).is_locally_owned()) continue;

    // Skip masked elements
    if (mask_ptr != NULL) {
      double *m=mask_ptr->data(node);
      if (*m > 0.5) continue;
    }

    // get node id
    int node_id=node.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than elem id
    while ((wi != we) && (wi->first.id <node_id)) {
      wi++;
    }

    // If the current weight is not equal to the node id, then we must have passed it,
    // so count it.
    if (wi->first.id != node_id) {
      num_missing++;
    }
  }

  // Create Pointlist
  PointList *missing_points = new ESMCI::PointList(num_missing, mesh->spatial_dim());

  // Get weight iterators
  wi =wts.begin_row();

  // Get mesh elem iterator that goes through in order of id
  ni=mesh->map_begin(MeshObj::NODE);

  // Count all points that don't have weights
  for (; ni != ne; ++ni) {
    const MeshObj &node=*ni;

    // Skip non local nodes
    if (!GetAttr(node).is_locally_owned()) continue;

    // Skip masked elements
    if (mask_ptr != NULL) {
      double *m=mask_ptr->data(node);
      if (*m > 0.5) continue;
    }

    // get node id
    int node_id=node.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than elem id
    while ((wi != we) && (wi->first.id <node_id)) {
      wi++;
    }

    // If the current weight is not equal to the elem id, then we must have passed it, so add it to the list
    if (wi->first.id != node_id) {
      double *node_coord=coord_ptr->data(node);
      missing_points->add(node_id, node_coord);
    }
  }

  // Output
  *_missing_points=missing_points;
}


static void _create_pointlist_of_mesh_nodes_in_wmat(Mesh *mesh, WMat &wts, PointList **_mapped_points) {

   // Coord Pointer
   MEField<> *coord_ptr = mesh->GetCoordField();

   // Mask Pointer
   MEField<> *mask_ptr = mesh->GetField("mask");

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Get mesh node iterator that goes through in order of id
  Mesh::MeshObjIDMap::const_iterator ni=mesh->map_begin(MeshObj::NODE), ne=mesh->map_end(MeshObj::NODE);

  // Count all points that don't have weights
  int num_mapped=0;
  for (; ni != ne; ++ni) {
    const MeshObj &node=*ni;

    // Skip non local nodes
    if (!GetAttr(node).is_locally_owned()) continue;

    // Skip masked elements
    if (mask_ptr != NULL) {
      double *m=mask_ptr->data(node);
      if (*m > 0.5) continue;
    }

    // get node id
    int node_id=node.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id <node_id)) {
      wi++;
    }

    // If the current weight is equal to the node id, then we found it, so count it
    if (wi->first.id == node_id) {
      num_mapped++;
    }
  }

  // Create Pointlist
  PointList *mapped_points = new ESMCI::PointList(num_mapped, mesh->spatial_dim());

  // Get weight iterators
  wi =wts.begin_row();

  // Get mesh elem iterator that goes through in order of id
  ni=mesh->map_begin(MeshObj::NODE);

  // Count all points that don't have weights
  for (; ni != ne; ++ni) {
    const MeshObj &node=*ni;

    // Skip non local nodes
    if (!GetAttr(node).is_locally_owned()) continue;

    // Skip masked elements
    if (mask_ptr != NULL) {
      double *m=mask_ptr->data(node);
      if (*m > 0.5) continue;
    }

    // get node id
    int node_id=node.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than elem id
    while ((wi != we) && (wi->first.id <node_id)) {
      wi++;
    }

    // If the current weight is equal to the node id, then we found it, so add it to the list
    if (wi->first.id == node_id) {
      double *node_coord=coord_ptr->data(node);
      mapped_points->add(node_id, node_coord);
    }
  }

  // Output
  *_mapped_points=mapped_points;
}


// Get list of dst ids in wts (i.e. mapped points)
 static void _get_dst_gids_in_wmat(WMat &wts, std::vector<int> &dst_gids) {

   // Get weight iterators
   WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();
   for (; wi != we; ++wi) {
    
     // get dst id
     int dst_gid=wi->first.id;

     // Stick it into the list
     dst_gids.push_back(dst_gid);
   }
 }


// Get the list of ids in the pointlist, but not in the wts
// Assumes input pointlist is sorted
static void _create_pointlist_of_points_not_in_wmat(PointList *pointlist, WMat &wts, PointList **_missing_points) {

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Count number of missing points
  int num_missing=0;
  int curr_num_pts = pointlist->get_curr_num_pts();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If the current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != id) {
      num_missing++;
    }
  }

  // Create Pointlist
  PointList *missing_points = new ESMCI::PointList(num_missing, pointlist->get_coord_dim());

  // Add missing points to PointList
  wi =wts.begin_row();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If teh current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != id) {
      missing_points->add(id, pointlist->get_coord_ptr(i));
    }
  }

  // Output
  *_missing_points=missing_points;
}

// Get the list of ids in the pointlist and in the wts
// Assumes input pointlist is sorted
static void _create_pointlist_of_points_in_wmat(PointList *pointlist, WMat &wts, PointList **_mapped_points) {

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Count number of missing points
  int num_mapped=0;
  int curr_num_pts = pointlist->get_curr_num_pts();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If the current weight is equal to the id, then we found it, so count it
    if (wi->first.id == id) {
      num_mapped++;
    }
  }

  // Create Pointlist
  PointList *mapped_points = new ESMCI::PointList(num_mapped, pointlist->get_coord_dim());

  // Add mapped points to PointList
  wi =wts.begin_row();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If the current weight is equal to the node id, then we found it, so add it to the list
    if (wi->first.id == id) {
      mapped_points->add(id, pointlist->get_coord_ptr(i));
    }
  }

  // Output
  *_mapped_points=mapped_points;
}


 void _replace_mapped_with_mapped_extrap(WMat &status) {

   WMat::WeightMap::iterator wi = status.begin_row(), we = status.end_row();
   for (; wi != we; ++wi) {

     // Get row and column info
     const WMat::Entry &row = wi->first;
     std::vector<WMat::Entry> &col = wi->second;

     // Loop column
     for (int i = 0; i < col.size(); ++i) {
       WMat::Entry &entry = col[i];

       if (entry.id == ESMC_REGRID_STATUS_MAPPED) entry.id=ESMC_REGRID_STATUS_EXTRAP_MAPPED;
     }
   }
 }


 // Migrate stod weights so the correct ones are available for merging in the 
 // dtod weights
 static void _migrate_wts_for_dtod_stod_merge(WMat &dtod_wts, WMat &stod_wts) {

   // Get current ids on this pet for src of migration
   std::vector<UInt> src_mig_gids;
   stod_wts.GetRowGIDS(src_mig_gids);

   // Get current ids again to start dst of migrate
   std::vector<UInt> dst_mig_gids;
   stod_wts.GetRowGIDS(dst_mig_gids);

   // Now add any needed for creep
   std::set<UInt> creep_gids; // use a set for efficieny
   WMat::WeightMap::iterator wi = dtod_wts.weights.begin(), we = dtod_wts.weights.end();
   for (; wi != we; ++wi) {
     std::vector<WMat::Entry> &col = wi->second;
     for (UInt i = 0; i < col.size(); i++) {
       creep_gids.insert(col[i].id);
     }
  }

   // merge into dst mig gids
   std::copy(creep_gids.begin(), creep_gids.end(), std::back_inserter(dst_mig_gids));

   // Now migrate
   Migrator mig(dst_mig_gids.size(), dst_mig_gids.size() > 0 ? &dst_mig_gids[0] : NULL, 0,
                src_mig_gids.size(), src_mig_gids.size() > 0 ? &src_mig_gids[0] : NULL);
   mig.Migrate(stod_wts);
 }

 // Prune out any weights in stod_wts that don't have a  local
 // destination in dst_mesh. This is the typical condition for regrid_wts and it's dst_mesh
  template<class Type>
  void _prune_wts_after_dtod_stod_merge(Type &dst_mesh, WMat &stod_wts) {
    Throw()<<" Unknown type for weight prune.";
  }

  // Prune weights using a mesh
  template<>
  void _prune_wts_after_dtod_stod_merge(Mesh &dst_mesh, WMat &stod_wts) {

   // Loop through weights, pruning out ones that aren't in dst_mesh
   WMat::WeightMap::iterator wi = stod_wts.weights.begin(), we = stod_wts.weights.end();
   while(wi != we) {

     // Get row and column info
     const WMat::Entry &row = wi->first;

     // Get gid
     UInt gid=row.id;

     // Save temporary iterator in case we need to erase
     WMat::WeightMap::iterator tmp_wi = wi;

     // Advance to next position before wi is invalidated by erase
     ++wi;

     // If gid isn't in dst_mesh or isn't local, then erase it
     Mesh::MeshObjIDMap::iterator mi =  dst_mesh.map_find(MeshObj::NODE, gid);
     // If it isn't in mesh then erase
     if (mi == dst_mesh.map_end(MeshObj::NODE)) {
       stod_wts.weights.erase(tmp_wi);
     } else { // If it is in the mesh, but isn't local, then erase it
       MeshObj &node=*mi;
       if (!GetAttr(node).is_locally_owned()) {
         stod_wts.weights.erase(tmp_wi);
       }
     }
   }
 }

  // Prune weights using a PointList
  template<>
  void _prune_wts_after_dtod_stod_merge(PointList &pointlist, WMat &stod_wts) {

    // Get weight iterators
    WMat::WeightMap::iterator wi =stod_wts.begin_row(),we = stod_wts.end_row();

    // Loop through points
    int curr_num_pts = pointlist.get_curr_num_pts();
    for (int i=0; i<curr_num_pts; i++) {

      // get id
      int id = pointlist.get_id(i);

      // Erase any weights that aren't in point list
      while ((wi != we) && (wi->first.id < id)) {

        // Save temporary iterator to erase
        WMat::WeightMap::iterator tmp_wi = wi;
        
        // Advance to next position before wi is invalidated by erase
        wi++;

        // Erase
        stod_wts.weights.erase(tmp_wi);
      }

      // If at end of weights leave
      if (wi == we) break;
      
      // If the current weight is equal to the id, then advance to the next (i.e. don't erase)
      if (wi->first.id == id) {
        wi++;
      }
    }

    // If not at end of weights, erase everything else
    if (wi != we) {
      stod_wts.weights.erase(wi,we);
    }
  }


 // This method merges a set of weights that map from destination ids to destination ids into
 // a set of weights which map source to destination ids (e.g. regrid weights). This means converting
 // the dest. to dest. weights to source to dest. using those weights and then adding them in. 
  template<class Type>
  void _merge_dst_to_dst_wts_into_src_to_dst_wts(Type &dst, WMat &dtod_wts, 
                                                        WMat &stod_wts) {

   // If parallel, migrate stod_wts to make sure the correct entries are on this
   // processor to allow the merge
   if (Par::Size() > 1) {
     _migrate_wts_for_dtod_stod_merge(dtod_wts, stod_wts);
   }

   // New column vector to be filled with merged weights
   std::vector<IWeights::Entry> new_cols;

   // Loop through creep weights
   WMat::WeightMap::iterator dwi = dtod_wts.begin_row(), dwe = dtod_wts.end_row();
   for (; dwi != dwe; ++dwi) {
     const WMat::Entry &dw_row = dwi->first;
     std::vector<WMat::Entry> &dw_cols = dwi->second;

     // Clear new_cols before we start filling it 
     new_cols.clear();

     // Loop through incoming col vector processing ids
     for (int i=0; i<dw_cols.size(); i++) {
       const WMat::Entry &dwc = dw_cols[i];

       // Get id
       int id=dwc.id;

       // Get weight
       double wgt=dwc.value;

       // Lower bound row
       WMat::Entry lower(id);
       WMat::WeightMap::const_iterator ri = stod_wts.weights.lower_bound(lower);

       // Upper Bound row
       WMat::Entry upper(id+1);
       WMat::WeightMap::const_iterator re = stod_wts.weights.lower_bound(upper);

       // If there are no rows which match complain
       if (ri == stod_wts.weights.end()) {
         Throw() << "destination id not found in weight matrix.";
       }

       // Loop over rows which match
       // see if any have the same dst id
       bool found_id=false;
       for (; ri != re; ++ri) {
         const WMat::Entry &tmp_row = ri->first;
         
         // Leave if they have the same dst id
         if (tmp_row.id == id) {
           found_id=true;
           break;
         }
       }

       // If there are no rows which match complain
       if (!found_id) {
         Throw() << "destination id not found in weight matrix.";
       }
       
       // Get columns of weights corresponding to tmp_row
       const std::vector<WMat::Entry> &tmp_cols = ri->second;
       for (int j=0; j<tmp_cols.size(); j++) {
         const WMat::Entry &tc = tmp_cols[j];

         // Set col entry info 
         WMat::Entry new_col_entry(tc.id, 0, wgt*tc.value, 0);

         // Push into new cols
         new_cols.push_back(new_col_entry);
       }
     }

     // Sort new col to enable merging entries         
     std::sort(new_cols.begin(),new_cols.end());

     // Merge entries with the same id
     int prev=0;
     for (int i=1; i<new_cols.size(); i++) {
       // If not the same id, then copy into a new entry
       if (new_cols[i].id != new_cols[prev].id) {
         prev++;
         new_cols[prev].id=new_cols[i].id;
         new_cols[prev].value=new_cols[i].value;
       } else { // if the same id, then just sum weights 
         new_cols[prev].value += new_cols[i].value;
       }
     }

     // Resize to just hold the merged entries
     new_cols.resize(prev+1);

     // Add to weight matrix
     stod_wts.InsertRow(dw_row, new_cols);
   }

   // If parallel, now prune the weights again to make sure just 
   // the correct weights for the mesh are present
   if (Par::Size() > 1) {
     _prune_wts_after_dtod_stod_merge(dst, stod_wts);
   }
 }

template void _merge_dst_to_dst_wts_into_src_to_dst_wts(Mesh &dst, WMat &dtod_wts, 
                                                               WMat &stod_wts);

template void _merge_dst_to_dst_wts_into_src_to_dst_wts(PointList &dst, WMat &dtod_wts, 
                                                               WMat &stod_wts);


  // Implementation of ESMC_EXTRAPMETHOD_NEAREST_STOD
  void _extrapmethod_nearest_stod(Mesh *srcmesh, PointList *srcpointlist, 
                                  PointList *dstpointlist,
                                  IWeights &wts,
                                  MAP_TYPE mtype,
                                  UInt pole_constraint_id, // Only valid when srcmesh exists
                                  bool set_dst_status, WMat &dst_status) {


   // Construct pointlist for srcmesh
   PointList *srcpointlist_extrap=NULL;
   PointList *srcpointlist_from_mesh=NULL;
   if (srcmesh != NULL) {
     _create_pointlist_from_mesh_nodes(srcmesh, pole_constraint_id, &srcpointlist_from_mesh);
     srcpointlist_extrap=srcpointlist_from_mesh;
   } else if (srcpointlist != NULL) {
     srcpointlist_extrap=srcpointlist;
   } else {
     Throw() << "No destination geometry object.";
   }

   // DEBUG
   //srcpointlist_extrap->WriteVTK("src_pl");

   // Construct a new point list which just contains destination points not in the weight matrix
   PointList *missing_points=NULL;
   if (dstpointlist != NULL) {
     _create_pointlist_of_points_not_in_wmat(dstpointlist, wts, &missing_points);
   } else {
     Throw() << "No destination geometry object.";
   }

   // DEBUG
   //missing_points->WriteVTK("missing_pl");

   // Set info for calling into interp
   IWeights extrap_wts;
   WMat extrap_dst_status;

   // Build the rendezvous grids
   Interp interp((Mesh *)NULL, srcpointlist_extrap,(Mesh *)NULL, missing_points,
                 (Mesh *)NULL, false, ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST,
                 set_dst_status, extrap_dst_status,
                 mtype, ESMCI_UNMAPPEDACTION_IGNORE, false,
                 0, 0.0);

   // Create the weight matrix
   interp(0, extrap_wts, set_dst_status, extrap_dst_status);

   // Merge extrap weights into regridding matrix
   wts.MergeDisjoint(extrap_wts);

   // If status was requested merge that too
   if (set_dst_status) {
     // Change from ...MAPPED to ...MAPPED_EXTRAP
     _replace_mapped_with_mapped_extrap(extrap_dst_status);

     // Replace old status with extrap ones
     dst_status.MergeReplace(extrap_dst_status);
   }

   // Cleanup
   if (srcpointlist_from_mesh != NULL) delete srcpointlist_from_mesh;
   if (missing_points != NULL) delete missing_points;
  }


  // Implementation of ESMC_EXTRAPMETHOD_NEAREST_IDAVG
  void _extrapmethod_nearest_idavg(Mesh *srcmesh, PointList *srcpointlist, 
                                   PointList *dstpointlist,
                                   IWeights &wts,
                                   MAP_TYPE mtype,
                                   UInt pole_constraint_id, // Only valid when srcmesh exists
                                   int extrapNumSrcPnts,
                                   ESMC_R8 extrapDistExponent,
                                   bool set_dst_status, WMat &dst_status) {


   // Construct pointlist for srcmesh
   PointList *srcpointlist_extrap=NULL;
   PointList *srcpointlist_from_mesh=NULL;
   if (srcmesh != NULL) {
     _create_pointlist_from_mesh_nodes(srcmesh, pole_constraint_id, &srcpointlist_from_mesh);
     srcpointlist_extrap=srcpointlist_from_mesh;
   } else if (srcpointlist != NULL) {
     srcpointlist_extrap=srcpointlist;
   } else {
     Throw() << "No destination geometry object.";
   }

   // DEBUG
   //srcpointlist_extrap->WriteVTK("src_pl");

   // Construct a new point list which just contains destination points not in the weight matrix
   PointList *missing_points=NULL;
   if (dstpointlist != NULL) {
     _create_pointlist_of_points_not_in_wmat(dstpointlist, wts, &missing_points);
   } else {
     Throw() << "No destination geometry object.";
   }

   // DEBUG
   //missing_points->WriteVTK("missing_pl");

   // Set info for calling into interp
   IWeights extrap_wts;
   WMat extrap_dst_status;

   // Build the rendezvous grids
   Interp interp((Mesh *)NULL, srcpointlist_extrap,(Mesh *)NULL, missing_points,
                 (Mesh *)NULL, false, ESMC_REGRID_METHOD_NEAREST_IDAVG,
                 set_dst_status, extrap_dst_status,
                 mtype, ESMCI_UNMAPPEDACTION_IGNORE, false, 
                 extrapNumSrcPnts, extrapDistExponent);

   // Create the weight matrix
   interp(0, extrap_wts, set_dst_status, extrap_dst_status);

   // Merge extrap weights into regridding matrix
   wts.MergeDisjoint(extrap_wts);

   // If status was requested merge that too
   if (set_dst_status) {
     // Change from ...MAPPED to ...MAPPED_EXTRAP
     _replace_mapped_with_mapped_extrap(extrap_dst_status);

     // Replace old status with extrap ones
     dst_status.MergeReplace(extrap_dst_status);
   }

   // Cleanup
   if (srcpointlist_from_mesh != NULL) delete srcpointlist_from_mesh;
   if (missing_points != NULL) delete missing_points;
  }

  // Implementation of ESMC_EXTRAPMETHOD_NEAREST_D
  void _extrapmethod_nearest_d(Mesh *dstmesh, PointList *dstpointlist,
                               IWeights &wts,
                               MAP_TYPE mtype,
                               bool set_dst_status, WMat &dst_status) {
    
    
    // Calculate a source pointlist from dst pointlist and wmat
    PointList *dst_mapped_points=NULL;
    if (dstpointlist != NULL) {
      _create_pointlist_of_points_in_wmat(dstpointlist, wts, &dst_mapped_points);
    } else if (dstmesh != NULL) {
      _create_pointlist_of_mesh_nodes_in_wmat(dstmesh, wts, &dst_mapped_points);
    } else {
      Throw() << "Missing destination geometry object.";
    }
    
    // Construct a new point list which just contains destination points not in the weight matrix
    PointList *dst_missing_points=NULL;
    if (dstpointlist != NULL) {
      _create_pointlist_of_points_not_in_wmat(dstpointlist, wts, &dst_missing_points);
    } else if (dstmesh != NULL) {
      _create_pointlist_of_mesh_nodes_not_in_wmat(dstmesh, wts, &dst_missing_points);
    } else {
      Throw() << "Missing destination geometry object.";
    }
    
    // Translate extrap method to regrid method
    int regridMethod=ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST;
    
    // Nearest weights and status
    IWeights nrst_extrap_wts;
    WMat nrst_extrap_dst_status;
    
    // Build the rendezvous grids
    Interp interp((Mesh *)NULL, dst_mapped_points, 
                  (Mesh *)NULL, dst_missing_points,
                  (Mesh *)NULL, false, regridMethod,
                  set_dst_status, nrst_extrap_dst_status,
                  mtype, ESMCI_UNMAPPEDACTION_IGNORE, false, 
                  0, 0.0);
    
    // Create the weight matrix
    interp(0, nrst_extrap_wts, set_dst_status, nrst_extrap_dst_status);
    
#if 0
    // DEBUG
    {
      WMat::WeightMap::iterator wi = nrst_extrap_wts.weights.begin(), we = nrst_extrap_wts.weights.end();
      for (; wi != we; ++wi) {
        const WMat::Entry &row = wi->first;
        std::vector<WMat::Entry> &col = wi->second;
        
        printf("nrst_wts row id=%d :: col ids= ",row.id);
        for (UInt i = 0; i < col.size(); i++) {
          printf(" %d ",col[i].id);
        }
        printf("\n");
        
      }
    }
#endif
    
    // Merge nearest weights into regrid weights
    if (dstpointlist != NULL) {
      _merge_dst_to_dst_wts_into_src_to_dst_wts(*dstpointlist, nrst_extrap_wts, wts);
    } else if (dstmesh != NULL) {
      _merge_dst_to_dst_wts_into_src_to_dst_wts(*dstmesh, nrst_extrap_wts, wts);
    } else {
      Throw() << "Missing destination geometry object.";
    }


    // If status was requested merge that too
    if (set_dst_status) {
      // Change from ...MAPPED to ...MAPPED_NRST_EXTRAP
      _replace_mapped_with_mapped_extrap(nrst_extrap_dst_status);
      
      // Replace old status with extrap ones
      dst_status.MergeReplace(nrst_extrap_dst_status);
    }
    
    // Clean up
    if (dst_missing_points != NULL) delete dst_missing_points;
    if (dst_mapped_points != NULL) delete dst_mapped_points;
  }
  
  // Implementation of ESMC_EXTRAPMETHOD_CREEP
  void _extrapmethod_creep(Mesh *dstmesh, 
                           IWeights &wts,
                           int extrapNumLevels,
                           int extrapNumInputLevels, 
                           bool set_dst_status, WMat &dst_status) {

     // Set info for calling into interp
     IWeights extrap_wts;
     WMat extrap_dst_status;
     std::vector<int> valid_gids;

     // Get list of valid gids
     _get_dst_gids_in_wmat(wts, valid_gids);

     // Get creep fill weights
     CreepFill(*dstmesh, valid_gids, extrapNumLevels, extrapNumInputLevels,
               extrap_wts, set_dst_status, extrap_dst_status);

     // Merge creep weights into regrid weights
     _merge_dst_to_dst_wts_into_src_to_dst_wts(*dstmesh, extrap_wts, wts);

     // If status was requested merge that too
     if (set_dst_status) {
       dst_status.MergeReplace(extrap_dst_status);
     }
  }

  // Implementation of ESMC_EXTRAPMETHOD_CREEP_NRST_D
  void _extrapmethod_creep_nrst_d(Mesh *dstmesh, 
                                  IWeights &wts,
                                  MAP_TYPE mtype,
                                  int extrapNumLevels,
                                  int extrapNumInputLevels, 
                                  bool set_dst_status, WMat &dst_status) {

     // Call into Creep fill/merge weights/merge status
     // (Inside braces so weights, etc. go away when not needed)
     {
       IWeights extrap_wts;
       WMat extrap_dst_status;
       std::vector<int> valid_gids;

       // Get list of valid gids
       _get_dst_gids_in_wmat(wts, valid_gids);

       // Get creep fill weights
       CreepFill(*dstmesh, valid_gids, extrapNumLevels, extrapNumInputLevels,
                 extrap_wts, set_dst_status, extrap_dst_status);

       // Merge creep weights into regrid weights
       _merge_dst_to_dst_wts_into_src_to_dst_wts(*dstmesh, extrap_wts, wts);
       
       // If status was requested merge that too
       if (set_dst_status) {
         dst_status.MergeReplace(extrap_dst_status);
       }
     }


     // Do nearest neighbor on missing points
     {
       // Calculate a source pointlist from dst pointlist and wmat
       PointList *dst_mapped_points=NULL;
       _create_pointlist_of_mesh_nodes_in_wmat(dstmesh, wts, &dst_mapped_points);
       
       // Calculate a pointList containing missing points from dst pointlist and wmat
       PointList *dst_missing_points=NULL;
       _create_pointlist_of_mesh_nodes_not_in_wmat(dstmesh, wts, &dst_missing_points);
       
       // Translate extrap method to regrid method
       int regridMethod=ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST;
       
       // Nearest weights and status
       IWeights nrst_extrap_wts;
       WMat nrst_extrap_dst_status;

       // Build the rendezvous grids
       Interp interp((Mesh *)NULL, dst_mapped_points, 
                     (Mesh *)NULL, dst_missing_points,
                     (Mesh *)NULL, false, regridMethod,
                     set_dst_status, nrst_extrap_dst_status,
                     mtype, ESMCI_UNMAPPEDACTION_IGNORE, false, 
                     0, 0.0);
       
       // Create the weight matrix
       interp(0, nrst_extrap_wts, set_dst_status, nrst_extrap_dst_status);

#if 0
       // DEBUG
       {
         WMat::WeightMap::iterator wi = nrst_extrap_wts.weights.begin(), we = nrst_extrap_wts.weights.end();
         for (; wi != we; ++wi) {
           const WMat::Entry &row = wi->first;
           std::vector<WMat::Entry> &col = wi->second;

           printf("nrst_wts row id=%d :: col ids= ",row.id);
           for (UInt i = 0; i < col.size(); i++) {
             printf(" %d ",col[i].id);
           }
           printf("\n");

         }
       }
#endif

       
       // Merge nearest weights into regrid weights
       _merge_dst_to_dst_wts_into_src_to_dst_wts(*dstmesh, nrst_extrap_wts, wts);
       
       // If status was requested merge that too
       if (set_dst_status) {
         // Change from ...MAPPED to ...MAPPED_NRST_EXTRAP
         _replace_mapped_with_mapped_extrap(nrst_extrap_dst_status);
         
         // Replace old status with extrap ones
         dst_status.MergeReplace(nrst_extrap_dst_status);
       }
     
       // Clean up
       if (dst_missing_points != NULL) delete dst_missing_points;
       if (dst_mapped_points != NULL) delete dst_mapped_points;
     }
  }


  // Upper level extrap method that branches out to the implementations of the specific methods
  void extrap(Mesh *srcmesh, PointList *srcpointlist, Mesh *dstmesh, PointList *dstpointlist,
              IWeights &wts,
              MAP_TYPE mtype,
              UInt pole_constraint_id, // Only valid when srcmesh exists
              int extrapMethod,
              int extrapNumSrcPnts,
              ESMC_R8 extrapDistExponent,
              int extrapNumLevels,
              int extrapNumInputLevels, 
              bool set_dst_status, WMat &dst_status) {

   //   printf("BOB: in extrap() extrapMethod=%d\n",extrapMethod);

    // Branch to each type of extrapolation
    if (extrapMethod == ESMC_EXTRAPMETHOD_NONE) {

      // Don't do anything

    } else if (extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_STOD) {

      // Call into method to do nearest stod
      _extrapmethod_nearest_stod(srcmesh, srcpointlist, 
                                 dstpointlist,
                                 wts,
                                 mtype,
                                 pole_constraint_id, 
                                 set_dst_status, dst_status);

    } else if (extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_IDAVG) {

      // Call into method to do nearest idavg
      _extrapmethod_nearest_idavg(srcmesh, srcpointlist, 
                                  dstpointlist,
                                  wts,
                                  mtype,
                                  pole_constraint_id, 
                                  extrapNumSrcPnts,
                                  extrapDistExponent,
                                  set_dst_status, dst_status);

    } else if (extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_D) {

      // Call into method to do nearest d
      _extrapmethod_nearest_d(dstmesh, dstpointlist,
                              wts,
                              mtype,
                              set_dst_status, dst_status);

    } else if (extrapMethod == ESMC_EXTRAPMETHOD_CREEP) {

      // Call into method to do creep fill
      _extrapmethod_creep(dstmesh, 
                          wts,
                          extrapNumLevels,
                          extrapNumInputLevels, 
                          set_dst_status, dst_status);
      
    } else if (extrapMethod == ESMC_EXTRAPMETHOD_CREEP_NRST_D) {

      // Call into method to do creep fill plus nearest destination to fill in remaining spots
      _extrapmethod_creep_nrst_d(dstmesh, 
                                 wts,
                                 mtype,
                                 extrapNumLevels,
                                 extrapNumInputLevels, 
                                 set_dst_status, dst_status);

    } else {
      Throw() << " Unrecognized extrapolation method.";
    }

  }


}
