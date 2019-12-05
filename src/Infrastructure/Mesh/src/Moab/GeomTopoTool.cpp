/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include "moab/GeomTopoTool.hpp"
#include "moab/GeomQueryTool.hpp"
#include "moab/OrientedBoxTreeTool.hpp"
#include "moab/Range.hpp"
#include "MBTagConventions.hpp"
#include "moab/Interface.hpp"
#include "moab/CN.hpp"
#include "moab/Skinner.hpp"
#include "Internals.hpp"
#include <iostream>

namespace moab {

// Tag name used for saving sense of faces in volumes.
// We assume that the surface occurs in at most two volumes.
// Code will error out if more than two volumes per surface.
// The tag data is a pair of tag handles, representing the
// forward and reverse volumes, respectively.  If a surface
// is non-manifold in a single volume, the same volume will
// be listed for both the forward and reverse slots.
const char GEOM_SENSE_2_TAG_NAME[] = "GEOM_SENSE_2";

const char GEOM_SENSE_N_ENTS_TAG_NAME[] = "GEOM_SENSE_N_ENTS";
const char GEOM_SENSE_N_SENSES_TAG_NAME[] = "GEOM_SENSE_N_SENSES";
const char OBB_ROOT_TAG_NAME[] = "OBB_ROOT";
const char OBB_GSET_TAG_NAME[] = "OBB_GSET";

const char IMPLICIT_COMPLEMENT_NAME[] = "impl_complement";

  
GeomTopoTool::GeomTopoTool(Interface *impl, bool find_geoments, EntityHandle modelRootSet,
                           bool p_rootSets_vector, bool restore_rootSets) :
  mdbImpl(impl), sense2Tag(0), senseNEntsTag(0), senseNSensesTag(0),
  geomTag(0), gidTag(0), obbRootTag(0), obbGsetTag(0),
  modelSet(modelRootSet), updated(false), 
  setOffset(0), m_rootSets_vector(p_rootSets_vector), oneVolRootSet(0)
{

  obbTree = new OrientedBoxTreeTool(impl, NULL, true);
  
  ErrorCode rval = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1,
      MB_TYPE_INTEGER, geomTag, MB_TAG_CREAT|MB_TAG_SPARSE);
  MB_CHK_SET_ERR_CONT(rval, "Error: Failed to create geometry dimension tag");
  
  // global id tag is not really needed, but mbsize complains if we do not set it for
  // geometry entities
  rval = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, 
        MB_TYPE_INTEGER, gidTag, MB_TAG_CREAT|MB_TAG_DENSE);
  MB_CHK_SET_ERR_CONT(rval,  "Error: Failed to create global id tag");

  rval = mdbImpl->tag_get_handle(NAME_TAG_NAME, NAME_TAG_SIZE,
      MB_TYPE_OPAQUE, nameTag, MB_TAG_CREAT|MB_TAG_SPARSE);
  MB_CHK_SET_ERR_CONT(rval, "Error: Failed to create name tag");

  rval = mdbImpl->tag_get_handle(OBB_ROOT_TAG_NAME, 1,
                  MB_TYPE_HANDLE, obbRootTag, MB_TAG_CREAT|MB_TAG_SPARSE);
  MB_CHK_SET_ERR_CONT(rval, "Error: Failed to create obb root tag");

  rval = mdbImpl->tag_get_handle(OBB_GSET_TAG_NAME, 1,
                  MB_TYPE_HANDLE, obbGsetTag, MB_TAG_CREAT|MB_TAG_SPARSE);
  MB_CHK_SET_ERR_CONT(rval, "Error: Failed to create obb gset tag");

  // set this value to zero for comparisons
  impl_compl_handle = 0;
  
  maxGlobalId[0] = maxGlobalId[1] = maxGlobalId[2] = maxGlobalId[3] =maxGlobalId[4] =0;
  if (find_geoments) {
    find_geomsets();
    if (restore_rootSets){
      rval = restore_obb_index();
      if (MB_SUCCESS != rval){
        rval = delete_all_obb_trees();
        MB_CHK_SET_ERR_CONT(rval, "Error: Failed to delete existing obb trees");
        rval = construct_obb_trees();
        MB_CHK_SET_ERR_CONT(rval, "Error: Failed to rebuild obb trees");
      }
    }
  }
}

GeomTopoTool::~GeomTopoTool() {
  delete obbTree;
}
    
  
int GeomTopoTool::dimension(EntityHandle this_set)
{
  ErrorCode result;
  if (0 == geomTag) {
    result = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag);
    MB_CHK_SET_ERR(result, "Failed to get the geometry dimension tag");
  }

  // check if the geo set belongs to this model
  if (modelSet)
  {
    if(!mdbImpl->contains_entities(modelSet, &this_set, 1 ))
    {
      // this g set does not belong to the current model
      return -1;
    }
  }
  // get the data for those tags
  int dim;
  result = mdbImpl->tag_get_data(geomTag, &this_set, 1, &dim);
  if (MB_SUCCESS != result)
    return -1;
  return dim;
}

int GeomTopoTool::global_id(EntityHandle this_set)
{
  ErrorCode result;
  if (0 == gidTag) {
    result = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gidTag);
    MB_CHK_SET_ERR_CONT(result, "Failed to get the global id tag");
    if (MB_SUCCESS != result) {
      return -1;
    }
  }

  // check if the geo set belongs to this model
  if (modelSet)
  {
    if(!mdbImpl->contains_entities(modelSet, &this_set, 1 ))
    {
      // this g set does not belong to the current model
      return -1;
    }
  }

  // get the data for those tags
  int id;
  result = mdbImpl->tag_get_data(gidTag, &this_set, 1, &id);
  if (MB_SUCCESS != result)
    return -1;
  return id;
}

EntityHandle GeomTopoTool::entity_by_id( int dimension1, int id )
{
  if (0 > dimension1 && 3 < dimension1) {
    MB_CHK_SET_ERR_CONT(MB_FAILURE, "Incorrect dimension provided");
  };
  const Tag tags[] = { gidTag, geomTag };
  const void* const vals[] = { &id, &dimension1 };
  ErrorCode rval;

  Range results;
  rval = mdbImpl->get_entities_by_type_and_tag( 0, MBENTITYSET, tags, vals, 2, results );

  if ( MB_SUCCESS != rval )
      return 0;

  return results.front();
}

  
ErrorCode GeomTopoTool::other_entity(EntityHandle bounded,
    EntityHandle not_this, EntityHandle across, EntityHandle &other)
{
  other = 0;

  // get all children of bounded
  Range bdy, tmpr;
  ErrorCode rval = mdbImpl->get_child_meshsets(bounded, bdy);
  MB_CHK_SET_ERR(rval, "Failed to get the bounded entity's child meshsets");

  // get all the parents of across
  rval = mdbImpl->get_parent_meshsets(across, tmpr);

  // possible candidates is the intersection
  bdy = intersect(bdy, tmpr);

  // if only two, choose the other
  if (1 == bdy.size() && *bdy.begin() == not_this) {
    return MB_SUCCESS;
  }
  else if (2 == bdy.size()) {
    if (*bdy.begin() == not_this)
      other = *bdy.rbegin();
    if (*bdy.rbegin() == not_this)
      other = *bdy.begin();
    else
      return MB_FAILURE;
  }
  else {
    return MB_FAILURE;
  }

  return MB_SUCCESS;
}


ErrorCode GeomTopoTool::restore_obb_index()
{

  if (m_rootSets_vector) resize_rootSets();
  
  ErrorCode rval;
  EntityHandle root;

  for (int dim = 2; dim <=3; dim++)
    for (Range::iterator rit = geomRanges[dim].begin(); rit != geomRanges[dim].end(); ++rit) {
      rval = mdbImpl->tag_get_data(obbRootTag, &(*rit), 1, &root);

      if (MB_SUCCESS == rval)
        set_root_set(*rit, root);
      else{
        return MB_TAG_NOT_FOUND;
      }
    }

  return MB_SUCCESS;

}
  
ErrorCode GeomTopoTool::find_geomsets(Range *ranges)
{
  ErrorCode rval;
  // get all sets with this tag
  Range geom_sets;

  if (0 == geomTag) {
    rval = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag);
    MB_CHK_SET_ERR(rval, "Failed to get geom dimension tag handle");
  }

  rval = mdbImpl->get_entities_by_type_and_tag(modelSet, MBENTITYSET,
					       &geomTag, NULL, 1, geom_sets);
  MB_CHK_SET_ERR(rval, "Failed to get the geometry entities");

  rval = separate_by_dimension(geom_sets);
  MB_CHK_SET_ERR(rval, "Failed to separate geometry sets by dimension");

  if (ranges) {
    for (int i = 0; i < 5; i++)
    {
      ranges[i] = geomRanges[i];
    }
 }

  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::get_gsets_by_dimension(int dim, Range &gset)
{
   ErrorCode rval;
 
   const int val = dim;
   const void* const dim_val[] = { &val };
   rval = mdbImpl->get_entities_by_type_and_tag(modelSet, MBENTITYSET, &geomTag,
       dim_val, 1, gset);
   MB_CHK_SET_ERR(rval, "Failed to get entity set by type and tag");
  
   return MB_SUCCESS;
}


ErrorCode GeomTopoTool::resize_rootSets() {

  ErrorCode rval;
  
  // store original offset for later
  EntityHandle orig_offset = setOffset;
  
  // get all surfaces and volumes
  Range surfs, vols;
  rval = get_gsets_by_dimension(2, surfs);
  MB_CHK_SET_ERR(rval, "Could not get surface sets");
  rval = get_gsets_by_dimension(3, vols);
  MB_CHK_SET_ERR(rval, "Could not get volume sets");

  // check the vector size
  Range surfs_and_vols;
  surfs_and_vols = vols;
  surfs_and_vols.merge(surfs);

  // update the setOffset
  setOffset = surfs_and_vols.front();

  EntityHandle exp_size = surfs_and_vols.back() - setOffset + 1;
  
  // if new EnitytHandle(s) are lower than the original offset
  if ( setOffset < orig_offset ) {
    // insert empty values at the beginning of the vector
    rootSets.insert(rootSets.begin(),orig_offset-setOffset,0);
  }

  if ( exp_size != rootSets.size() ) {
    // resize rootSets vector if necessary (new space will be added at the back)
    rootSets.resize(exp_size);
  }
  
  return MB_SUCCESS;
}  

ErrorCode GeomTopoTool::is_owned_set(EntityHandle eh) {
  // make sure entity set is part of the model
  Range model_ents;
  ErrorCode rval = mdbImpl->get_entities_by_handle(modelSet, model_ents);
  MB_CHK_SET_ERR(rval, "Failed to get entities");
  if(model_ents.find(eh) == model_ents.end())
    {
      MB_SET_ERR(MB_FAILURE, "Entity handle not in model set");
    }
  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::delete_obb_tree(EntityHandle gset, bool vol_only) {

  ErrorCode rval;

  // Make sure this set is part of the model
  rval = is_owned_set(gset);
  MB_CHK_SET_ERR(rval, "Entity set is not part of this model");

  // Find the dimension of the entity
  int dim;
  rval = mdbImpl->tag_get_data(geomTag, &gset, 1, &dim);
  MB_CHK_SET_ERR(rval, "Failed to get dimension");

  // Attempt to find a root for this set
  EntityHandle root;
  rval = get_root(gset, root);
  MB_CHK_SET_ERR(rval, "Failed to find an obb tree root for the entity set");
 
  // Create range of tree nodes to delete
  Range nodes_to_delete;
  nodes_to_delete.insert(root);

  // If passed ent is a vol and 'vol_only' is true, delete vol root and all nodes between vol and surf root
  if (dim == 3 && vol_only){
    // Range of child nodes to check before adding to delete list
    Range child_tree_nodes;
    rval = mdbImpl->get_child_meshsets(root, child_tree_nodes);
    MB_CHK_SET_ERR(rval, "Problem getting child tree nodes");

    // Traverse the tree, checking each child node until 
    // a surface root node is reached
    while(child_tree_nodes.size() != 0){
      EntityHandle child = *child_tree_nodes.begin();
      EntityHandle surf;
      rval = mdbImpl->tag_get_data(obbGsetTag, &child, 1, &surf);
      // If the node has a gset tag, it is a surf root. Stop here.
      // If not, it is a tree node that needs to 1) have its children checked and
      //  2) be added to delete range
      if (MB_TAG_NOT_FOUND == rval){
        Range new_child_tree_nodes;
        rval = mdbImpl->get_child_meshsets(child, new_child_tree_nodes);
        MB_CHK_SET_ERR(rval, "Problem getting child nodes");
        child_tree_nodes.insert_list(new_child_tree_nodes.begin(), new_child_tree_nodes.end());
        nodes_to_delete.insert(child);
      }
      // We're done checking this node, so can erase from child list
      child_tree_nodes.erase(child);
    }

  }
  // If passed ent is a surf or a vol and 'vol_only' is false, recursively gather all child nodes 
  // and add them to delete list
  else{
    Range all_tree_nodes;
    rval = mdbImpl->get_child_meshsets( root, all_tree_nodes, 0 );
    MB_CHK_SET_ERR(rval, "Failed to get child tree node sets");
    nodes_to_delete.insert_list(all_tree_nodes.begin(), all_tree_nodes.end());
  }

  // Remove the root nodes from the GTT data structures
  for (Range::iterator it = nodes_to_delete.begin(); it != nodes_to_delete.end(); ++it){ 
    // Check to see if node is a root
    EntityHandle vol_or_surf;
    rval = mdbImpl->tag_get_data(obbGsetTag, &(*it), 1, &vol_or_surf);
    if (MB_SUCCESS == rval){
      // Remove from set of all roots
      rval = remove_root(vol_or_surf);
      MB_CHK_SET_ERR(rval, "Failed to remove node from GTT data structure");
    }
  }
  
  // Delete the tree nodes from the database
  rval = mdbImpl->delete_entities(nodes_to_delete);
  MB_CHK_SET_ERR(rval, "Failed to delete node set");

  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::delete_all_obb_trees(){

  ErrorCode rval;

  for (Range::iterator rit = geomRanges[3].begin(); rit != geomRanges[3].end(); ++rit){
    EntityHandle root;
    rval = mdbImpl->tag_get_data(obbRootTag, &(*rit), 1, &root);
    if (MB_SUCCESS == rval){
      rval = delete_obb_tree(*rit, false);
      MB_CHK_SET_ERR(rval, "Failed to delete obb tree");
    }
  }

  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::construct_obb_tree(EntityHandle eh)
{
  ErrorCode rval;
  int dim;

  rval = is_owned_set(eh);
  MB_CHK_SET_ERR(rval, "Entity set is not part of this model");

  // get the type
  EntityType type = mdbImpl->type_from_handle(eh); 

  // find the dimension of the entity
  rval = mdbImpl->tag_get_data(geomTag, &eh, 1, &dim);
  MB_CHK_SET_ERR(rval, "Failed to get dimension");

  // ensure that the rootSets vector is of the correct size
  if (m_rootSets_vector && (eh < setOffset || eh > setOffset + rootSets.size()) ) {
    rval = resize_rootSets();
    MB_CHK_SET_ERR(rval, "Error setting offset and sizing rootSets vector.");
  }

  EntityHandle root;
  //if it's a surface
  if(dim == 2 && type == MBENTITYSET){ 
    rval = get_root(eh, root);
    if(MB_SUCCESS == rval)
      {
        std::cerr << "Surface obb tree already exists" << std::endl;
        return MB_SUCCESS;
      }
    else if(MB_INDEX_OUT_OF_RANGE != rval)
      {
        MB_CHK_SET_ERR(rval, "Failed to get surface obb tree root");
      }
    
    Range tris;
    rval = mdbImpl->get_entities_by_dimension(eh, 2, tris);
    MB_CHK_SET_ERR(rval, "Failed to get entities by dimension");
    
    if (tris.empty()) {
      std::cerr << "WARNING: Surface has no facets" << std::endl;
    }
    
    rval = obbTree->build(tris, root);
    MB_CHK_SET_ERR(rval, "Failed to build obb Tree for surface");
    
    rval = mdbImpl->add_entities(root, &eh, 1);
    MB_CHK_SET_ERR(rval, "Failed to add entities to root set");

    // add this root to the GeomTopoTool tree root indexing
    set_root_set(eh, root);
 
    // if just building tree for surface, return here
    return MB_SUCCESS;
  }
  //if it's a volume
  else if(dim == 3 && type == MBENTITYSET){
    //get its surfaces
    Range tmp_surfs, surf_trees;
    rval = mdbImpl->get_child_meshsets(eh, tmp_surfs);
    MB_CHK_SET_ERR(rval, "Failed to get surface meshsets");
    
    // get OBB trees or create for each surface
    for (Range::iterator j = tmp_surfs.begin(); j != tmp_surfs.end(); ++j) {
      rval = get_root(*j, root);
      // if root doesn't exist, create obb tree
      if( MB_INDEX_OUT_OF_RANGE == rval)
        {
          rval = construct_obb_tree(*j);
          MB_CHK_SET_ERR(rval, "Failed to get create surface obb tree");
          rval = get_root(*j, root);
          MB_CHK_SET_ERR(rval, "Failed to get surface obb tree root");
        }
      else
        {
          MB_CHK_SET_ERR(rval, "Failed to get surface obb tree root");
        }

      surf_trees.insert(root);
    }
    
    // build OBB tree for volume
    rval = obbTree->join_trees(surf_trees, root);
    MB_CHK_SET_ERR(rval, "Failed to join the obb trees");
    
    // add this root to the GeomTopoTool tree root indexing
    set_root_set(eh, root);
    
    return MB_SUCCESS;
  }
  else {
    MB_SET_ERR(MB_FAILURE, "Improper dimension or type for constructing obb tree");
  }
  
}

ErrorCode GeomTopoTool::set_root_set(EntityHandle vol_or_surf, EntityHandle root) {

  // Tag the vol or surf with its obb root (obbRootTag)
  ErrorCode rval;
  rval = mdbImpl->tag_set_data(obbRootTag, &vol_or_surf, 1, &root);
  MB_CHK_SET_ERR(rval, "Failed to set the obb root tag");

  // Tag obb root with corresponding gset (obbGsetTag)
  rval = mdbImpl->tag_set_data(obbGsetTag, &root, 1, &vol_or_surf);
  MB_CHK_SET_ERR(rval, "Failed to set the obb gset tag");

  // Add to the set of all roots
  if (m_rootSets_vector)
    rootSets[vol_or_surf - setOffset] = root;
  else
    mapRootSets[vol_or_surf] = root;  

  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::remove_root(EntityHandle vol_or_surf) {
 
  // Find the root of the vol or surf 
  ErrorCode rval;
  EntityHandle root;
  rval = mdbImpl->tag_get_data(obbRootTag, &(vol_or_surf), 1, &root);
  MB_CHK_SET_ERR(rval, "Failed to get obb root tag");
  
  // If the ent is a vol, remove its root from obbtreetool
  int dim;
  rval = mdbImpl->tag_get_data(geomTag, &vol_or_surf, 1, &dim);
  MB_CHK_SET_ERR(rval, "Failed to get dimension");
  if (dim == 3){
    rval = obbTree->remove_root(root);
    MB_CHK_SET_ERR(rval, "Failed to remove root from obbTreeTool");
  }

  // Delete the obbGsetTag data from the root 
  rval = mdbImpl->tag_delete_data(obbGsetTag, &root, 1);
  MB_CHK_SET_ERR(rval, "Failed to delete obb root tag");

  // Delete the obbRootTag data from the vol or surf 
  rval = mdbImpl->tag_delete_data(obbRootTag, &vol_or_surf, 1);
  MB_CHK_SET_ERR(rval, "Failed to delete obb root tag");

  // Remove the root from set of all roots
  if(m_rootSets_vector)
  {
    unsigned int index = vol_or_surf - setOffset;
    if( index < rootSets.size() ) {
      rootSets[index] = 0;
    }
    else {
      return MB_INDEX_OUT_OF_RANGE;
    }
  }
  else {
     mapRootSets[vol_or_surf] = 0;
  }
  
  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::construct_obb_trees(bool make_one_vol)
{
  ErrorCode rval;
  EntityHandle root;
  
  // get all surfaces and volumes
  Range surfs, vols, vol_trees;
  rval = get_gsets_by_dimension(2, surfs);
  MB_CHK_SET_ERR(rval, "Could not get surface sets");
  rval = get_gsets_by_dimension(3, vols);
  MB_CHK_SET_ERR(rval, "Could not get volume sets");

  // for surface
  Range one_vol_trees; 
  for (Range::iterator i = surfs.begin(); i != surfs.end(); ++i) {
    rval = construct_obb_tree(*i);
    MB_CHK_SET_ERR(rval, "Failed to construct obb tree for surface");
    // get the root set of this volume
    rval = get_root(*i, root);
    MB_CHK_SET_ERR(rval, "Failed to get obb tree root for surface");
    // add to the Range of volume root sets
    one_vol_trees.insert(root);
  }

  // for volumes
  for (Range::iterator i = vols.begin(); i != vols.end(); ++i) {
    // create tree for this volume
    rval = construct_obb_tree(*i);
    MB_CHK_SET_ERR(rval, "Failed to construct obb tree for volume");
  }

  // build OBB tree for volume
  if (make_one_vol) {
    rval = obbTree->join_trees(one_vol_trees, root);
    MB_CHK_SET_ERR(rval, "Failed to join surface trees into one volume");
    oneVolRootSet = root;
  }

  return rval;
}

//! Restore parent/child links between GEOM_TOPO mesh sets
ErrorCode GeomTopoTool::restore_topology_from_adjacency()
{

  // look for geometric topology sets and restore parent/child links between them
  // algorithm:
  // - for each entity of dimension d=D-1..0:
  //   . get d-dimensional entity in entity
  //   . get all (d+1)-dim adjs to that entity
  //   . for each geom entity if dim d+1, if it contains any of the ents,
  //     add it to list of parents
  //   . make parent/child links with parents

  // the  geomRanges are already known, separated by dimension

  std::vector<EntityHandle> parents;
  Range tmp_parents;
  ErrorCode result;

  // loop over dimensions
  for (int dim = 2; dim >= 0; dim--) {
    // mark entities of next higher dimension with their owners; regenerate tag
    // each dimension so prev dim's tag data goes away
    Tag owner_tag;
    EntityHandle dum_val = 0;
    result = mdbImpl->tag_get_handle("__owner_tag", 1,
        MB_TYPE_HANDLE, owner_tag, MB_TAG_DENSE|MB_TAG_CREAT, &dum_val);
    if (MB_SUCCESS != result)
      continue;
    Range dp1ents;
    std::vector<EntityHandle> owners;
    for (Range::iterator rit = geomRanges[dim + 1].begin(); rit != geomRanges[dim
        + 1].end(); ++rit) {
      dp1ents.clear();
      result = mdbImpl->get_entities_by_dimension(*rit, dim + 1, dp1ents);
      if (MB_SUCCESS != result)
        continue;
      owners.resize(dp1ents.size());
      std::fill(owners.begin(), owners.end(), *rit);
      result = mdbImpl->tag_set_data(owner_tag, dp1ents, &owners[0]);
      if (MB_SUCCESS != result)
        continue;
    }

    for (Range::iterator d_it = geomRanges[dim].begin(); d_it
        != geomRanges[dim].end(); ++d_it) {
      Range dents;
      result = mdbImpl->get_entities_by_dimension(*d_it, dim, dents);
      if (MB_SUCCESS != result)
        continue;
      if (dents.empty())
        continue;

      // get (d+1)-dimensional adjs
      dp1ents.clear();
      result = mdbImpl->get_adjacencies(&(*dents.begin()), 1, dim + 1, false,
          dp1ents);
      if (MB_SUCCESS != result || dp1ents.empty())
        continue;

      // get owner tags
      parents.resize(dp1ents.size());
      result = mdbImpl->tag_get_data(owner_tag, dp1ents, &parents[0]);
      if (MB_TAG_NOT_FOUND == result) {
	MB_CHK_SET_ERR(result, "Could not find owner tag");
      }
      if (MB_SUCCESS != result)
        continue;

      // compress to a range to remove duplicates
      tmp_parents.clear();
      std::copy(parents.begin(), parents.end(), range_inserter(tmp_parents));
      for (Range::iterator pit = tmp_parents.begin(); pit != tmp_parents.end(); ++pit) {
        result = mdbImpl->add_parent_child(*pit, *d_it);
	MB_CHK_SET_ERR(result, "Failed to create parent child relationship");
      }

      // store surface senses within regions, and edge senses within surfaces
      if (dim == 0)
        continue;
      const EntityHandle *conn3 = NULL, *conn2 = NULL;
      int len3 = 0, len2 = 0, err = 0, num = 0, sense = 0, offset = 0;
      for (size_t i = 0; i < parents.size(); ++i) {
        result = mdbImpl->get_connectivity(dp1ents[i], conn3, len3, true);
	MB_CHK_SET_ERR(result, "Failed to get the connectivity of the element");
        result = mdbImpl->get_connectivity(dents.front(), conn2, len2, true);
	MB_CHK_SET_ERR(result, "Failed to get the connectivity of the first element");
	if (len2 > 4) {
	  MB_SET_ERR(MB_FAILURE, "Connectivity of incorrect length");
	}
        err = CN::SideNumber(TYPE_FROM_HANDLE(dp1ents[i]), conn3, conn2, len2,
            dim, num, sense, offset);
        if (err)
          return MB_FAILURE;

        result = set_sense(*d_it, parents[i], sense);
        if (MB_MULTIPLE_ENTITIES_FOUND == result ) {
          if (2==dim)
            std::cerr << "Warning: Multiple volumes use surface with same sense."
                << std::endl << "         Some geometric sense data lost."
                << std::endl;
        } else if (MB_SUCCESS != result) {
          return result;
        }
      }
    }

    // now delete owner tag on this dimension, automatically removes tag data
    result = mdbImpl->tag_delete(owner_tag);
    MB_CHK_SET_ERR(result, "Failed to delete the owner tag");

  } // dim

  return result;
}

ErrorCode GeomTopoTool::separate_by_dimension(const Range &geom_sets)
{
  ErrorCode result;

  result = check_geom_tag();
  MB_CHK_SET_ERR(result, "Could not verify geometry dimension tag");
  
  // get the data for those tags
  std::vector<int> tag_vals(geom_sets.size());
  result = mdbImpl->tag_get_data(geomTag, geom_sets, &tag_vals[0]);
  MB_CHK_SET_ERR(result, "Failed to get the geometry dimension tag");

  Range::const_iterator git;
  std::vector<int>::iterator iit;

  for (int i=0; i<5; i++)
    this->geomRanges[i].clear();

  for (git = geom_sets.begin(), iit = tag_vals.begin(); git != geom_sets.end(); ++git, ++iit) {
    if (0 <= *iit && 4 >= *iit)
      geomRanges[*iit].insert(*git);
    else {
      // assert(false);
      // do nothing for now
    }
  }

  // establish the max global ids so far, per dimension
  if (0 == gidTag) {
    result = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gidTag);
    MB_CHK_SET_ERR(result, "Failed to get the global id tag handle");
  }

  for (int i=0; i<=4; i++)
  {
    maxGlobalId[i] = 0;
    for (Range::iterator it =geomRanges[i].begin(); it!=geomRanges[i].end(); ++it)
    {
      EntityHandle set = *it;
      int gid;

      result = mdbImpl->tag_get_data(gidTag, &set, 1, &gid);
      if (MB_SUCCESS == result)
      {
        if (gid>maxGlobalId[i])
          maxGlobalId[i] = gid;
      }
    }

  }

  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::construct_vertex_ranges(const Range &geom_sets,
    const Tag verts_tag)
{
  // construct the vertex range for each entity and put on that tag
  Range *temp_verts, temp_elems;
  ErrorCode result = MB_SUCCESS;
  for (Range::const_iterator it = geom_sets.begin(); it != geom_sets.end(); ++it) {
    temp_elems.clear();

    // get all the elements in the set, recursively
    result = mdbImpl->get_entities_by_handle(*it, temp_elems, true);
    MB_CHK_SET_ERR(result, "Failed to get the geometry set entities");

    // make the new range
    temp_verts = new (std::nothrow) Range();
    if(NULL == temp_verts) {
      MB_SET_ERR(MB_FAILURE, "Could not construct Range object");
    }

    // get all the verts of those elements; use get_adjacencies 'cuz it handles ranges better
    result = mdbImpl->get_adjacencies(temp_elems, 0, false, *temp_verts,
        Interface::UNION);
    if (MB_SUCCESS != result) {
      delete temp_verts;
    }
    MB_CHK_SET_ERR(result, "Failed to get the element's adjacent vertices");
    
    // store this range as a tag on the entity
    result = mdbImpl->tag_set_data(verts_tag, &(*it), 1, &temp_verts);
    if (MB_SUCCESS != result) {
      delete temp_verts;
    }
    MB_CHK_SET_ERR(result, "Failed to get the adjacent vertex data");
    
    delete temp_verts;
    temp_verts = NULL;
  }

  return result;
}

//! Store sense of entity relative to wrt_entity.
//!\return MB_MULTIPLE_ENTITIES_FOUND if surface already has a forward volume.
//!        MB_SUCCESS if successful
//!        otherwise whatever internal error code occurred.
ErrorCode GeomTopoTool::set_sense(EntityHandle entity, EntityHandle wrt_entity,
    int sense)
{
  // entity is lower dim (edge or face), wrt_entity is face or volume
  int edim = dimension(entity);
  int wrtdim = dimension(wrt_entity);
  if (-1 == edim || -1 == wrtdim)
    MB_SET_ERR(MB_FAILURE, "Non-geometric entity provided");
  if (wrtdim - edim != 1)
    MB_SET_ERR(MB_FAILURE, "Entity dimension mismatch");
  if (sense < -1 || sense > 1)
    MB_SET_ERR(MB_FAILURE, "Invalid sense data provided");

  ErrorCode rval;

  if (1 == edim) {
    // this case is about setting the sense of an edge in a face
    // it could be -1, 0 (rare, non manifold), or 1
    rval = check_edge_sense_tags(true);
    MB_CHK_SET_ERR(rval, "Failed to check the curve to surface sense tag handles");
    std::vector<EntityHandle> higher_ents;
    std::vector<int> senses;
    rval = get_senses(entity, higher_ents, senses);// the tags should be defined here
    // if there are no higher_ents, we are fine, we will just set them
    // if wrt_entity is not among higher_ents, we will add it to the list
    // it is possible the entity (edge set) has no prior faces adjancent; in that case, the
    // tag would not be set, and rval could be MB_TAG_NOT_FOUND; it is not a fatal error
    if (MB_SUCCESS != rval &&  MB_TAG_NOT_FOUND != rval)
      MB_CHK_SET_ERR(rval, "cannot determine sense tags for edge");
    bool append = true;
    if (!higher_ents.empty()) {
      std::vector<EntityHandle>::iterator it = std::find(higher_ents.begin(),
          higher_ents.end(), wrt_entity);
      if (it != higher_ents.end()) {
        // we should not reset the sense, if the sense is the same
        // if the sense is different, put BOTH
        unsigned int idx = it - higher_ents.begin();
        int oldSense = senses[idx];
        if (oldSense == sense)
          return MB_SUCCESS; // sense already set fine, do not reset
        if (0!=oldSense && oldSense+sense !=0)
          return MB_MULTIPLE_ENTITIES_FOUND;
        senses[idx]=SENSE_BOTH; // allow double senses
        // do not need to add a new sense, but still need to reset the tag
        // because of a new value
        append = false;
      }
    }
    if (append)
    {
      // what happens if a var tag data was already set before, and now it is
      // reset with a different size??
      higher_ents.push_back(wrt_entity);
      senses.push_back(sense);
    }
    // finally, set the senses :
    int dum_size = higher_ents.size();
    void *dum_ptr = &higher_ents[0];
    rval = mdbImpl->tag_set_by_ptr(senseNEntsTag, &entity, 1, &dum_ptr, &dum_size);
    MB_CHK_SET_ERR(rval, "Failed to set the sense data");

    dum_ptr = &senses[0];
    dum_size = higher_ents.size();
    rval = mdbImpl->tag_set_by_ptr(senseNSensesTag, &entity, 1, &dum_ptr, &dum_size);
    MB_CHK_SET_ERR(rval, "Failed to set the sense data by pointer");

  } else {
    // this case is about a face in the volume
    // there could be only 2 volumes

    rval = check_face_sense_tag(true);
    MB_CHK_SET_ERR(rval, "Failed to verify the face sense tag");

    EntityHandle sense_data[2] = { 0, 0 };
    rval = mdbImpl->tag_get_data(sense2Tag, &entity, 1, sense_data);
    if (MB_TAG_NOT_FOUND != rval && MB_SUCCESS != rval)
      MB_CHK_SET_ERR(rval, "Failed to get the sense2Tag data");

    if (0 == sense) {
      if (0 != sense_data[0] && wrt_entity != sense_data[0])
        return MB_MULTIPLE_ENTITIES_FOUND;
      if (0 != sense_data[1] && wrt_entity != sense_data[1])
        return MB_MULTIPLE_ENTITIES_FOUND;
      sense_data[0] = sense_data[1] = wrt_entity;
    } else if (-1 == sense) {
      if (0 != sense_data[1] && wrt_entity != sense_data[1])
        return MB_MULTIPLE_ENTITIES_FOUND;
      if (sense_data[1] == wrt_entity)
        return MB_SUCCESS; // already set as we want
      sense_data[1] = wrt_entity;
    } else if (1 == sense) {
      if (0 != sense_data[0] && wrt_entity != sense_data[0])
        return MB_MULTIPLE_ENTITIES_FOUND;
      if (sense_data[0] == wrt_entity)
        return MB_SUCCESS; // already set as we want
      sense_data[0] = wrt_entity;
    }
    return mdbImpl->tag_set_data(sense2Tag, &entity, 1, sense_data);

  }
  return MB_SUCCESS;
}

//! Get the sense of entity with respect to wrt_entity
//! Returns MB_ENTITY_NOT_FOUND if no relationship found
ErrorCode GeomTopoTool::get_sense(EntityHandle entity, EntityHandle wrt_entity,
    int & sense)
{
  // entity is lower dim (edge or face), wrt_entity is face or volume
  int edim = dimension(entity);
  int wrtdim = dimension(wrt_entity);
  if (-1 == edim || -1 == wrtdim)
    MB_SET_ERR(MB_FAILURE, "Non-geometric entity provided");
  if (wrtdim - edim != 1)
    MB_SET_ERR(MB_FAILURE, "Entity dimension mismatch");

  ErrorCode rval;

  if (1 == edim) {
    // edge in face
    rval = check_edge_sense_tags(false);
    MB_CHK_SET_ERR(rval, "Failed to check the curve to surface sense tag handles");
    
    std::vector<EntityHandle> faces;
    std::vector<int> senses;
    rval = get_senses(entity, faces, senses);// the tags should be defined here
    MB_CHK_SET_ERR(rval, "Failed to get the curve to surface sense data");

    std::vector<EntityHandle>::iterator it = std::find(faces.begin(),
        faces.end(), wrt_entity);
    if (it == faces.end())
      return MB_ENTITY_NOT_FOUND;
    unsigned int index = it - faces.begin();
    sense = senses[index];
  } else {
    // face in volume
    rval = check_face_sense_tag(false);
    MB_CHK_SET_ERR(rval, "Failed to check the surface to volume sense tag handle");
    EntityHandle sense_data[2] = { 0, 0 };
    rval = mdbImpl->tag_get_data(sense2Tag, &entity, 1, sense_data);
    if (MB_TAG_NOT_FOUND != rval && MB_SUCCESS != rval)
      MB_CHK_SET_ERR(rval, "Failed to get the surface to volume sense data");
    if ((wrt_entity == sense_data[0]) && (wrt_entity == sense_data[1]))
      sense = 0;
    else if (wrt_entity == sense_data[0])
      sense = 1;
    else if (wrt_entity == sense_data[1])
      sense = -1;
    else
      return MB_ENTITY_NOT_FOUND;
  }
  return MB_SUCCESS;

}

ErrorCode GeomTopoTool::get_surface_senses(EntityHandle surface_ent,
					   EntityHandle &forward_vol,
					   EntityHandle &reverse_vol) {
  ErrorCode rval;
  // this method should only be called to retrieve surface to volume
  // sense relationships
  int ent_dim = dimension(surface_ent);
  // verify the incoming entity dimensions for this call
  if( ent_dim != 2 ) {
    MB_SET_ERR(MB_FAILURE, "Entity dimension is incorrect for surface meshset");
  }
  
  // get the sense information for this surface
  EntityHandle parent_vols[2] = {0 , 0};
  rval = mdbImpl->tag_get_data(sense2Tag, &surface_ent, 1, parent_vols);
  MB_CHK_SET_ERR(rval, "Failed to get surface sense data");

  // set the outgoing values
  forward_vol = parent_vols[0];
  reverse_vol = parent_vols[1];
  
  return MB_SUCCESS;    
}

ErrorCode GeomTopoTool::set_surface_senses(EntityHandle surface_ent,
					   EntityHandle forward_vol,
					   EntityHandle reverse_vol) {
  ErrorCode rval;
  // this mthod should only be called to retrieve surface to volume
  // sense relationships
  int ent_dim = dimension(surface_ent);
  // verify the incoming entity dimensions for this call
  if( ent_dim != 2 ) {
    MB_SET_ERR(MB_FAILURE, "Entity dimension is incorrect for surface meshset");
  }

  // set the sense information for this surface
  EntityHandle parent_vols[2] = {forward_vol, reverse_vol};
  rval = mdbImpl->tag_set_data(sense2Tag, &surface_ent, 1, parent_vols);
  MB_CHK_SET_ERR(rval, "Failed to set surface sense data");

  return MB_SUCCESS;
}

// get sense of surface(s) wrt volume
ErrorCode GeomTopoTool::get_surface_senses(EntityHandle volume,
                                           int num_surfaces,
                                           const EntityHandle* surfaces,
                                           int* senses_out)
{

  /* The sense tags do not reference the implicit complement handle.
     All surfaces that interact with the implicit complement should have
     a null handle in the direction of the implicit complement. */
  //if (volume == impl_compl_handle)
  //  volume = (EntityHandle) 0;

  for (int surf_num = 0; surf_num < num_surfaces; surf_num++) {
    get_sense( surfaces[surf_num], volume, senses_out[surf_num]);
  }
  
  return MB_SUCCESS;
}

  
ErrorCode GeomTopoTool::get_senses(EntityHandle entity,
    std::vector<EntityHandle> &wrt_entities, std::vector<int> &senses)
{
  // the question here is: the wrt_entities is supplied or not?
  // I assume not, we will obtain it !!
  int edim = dimension(entity);

  if (-1 == edim)
    MB_SET_ERR(MB_FAILURE, "Non-geometric entity provided");

  ErrorCode rval;
  wrt_entities.clear();
  senses.clear();

  if (1 == edim)// edge
  {
    rval = check_edge_sense_tags(false);
    MB_CHK_SET_ERR(rval, "Failed to check the curve to surface sense tag handles");
    const void *dum_ptr;
    int num_ents;
    rval = mdbImpl->tag_get_by_ptr(senseNEntsTag, &entity, 1, &dum_ptr, &num_ents);
    MB_CHK_ERR(rval);

    const EntityHandle *ents_data = static_cast<const EntityHandle*> (dum_ptr);
    std::copy(ents_data, ents_data + num_ents, std::back_inserter(wrt_entities));

    rval = mdbImpl->tag_get_by_ptr(senseNSensesTag, &entity, 1, &dum_ptr,
        &num_ents);
    MB_CHK_ERR(rval);

    const int *senses_data = static_cast<const int*> (dum_ptr);
    std::copy(senses_data, senses_data + num_ents, std::back_inserter(senses));

  } else // face in volume, edim == 2
  {
    rval = check_face_sense_tag(false);
    MB_CHK_SET_ERR(rval, "Failed to check the surface to volume sense tag handle");
    EntityHandle sense_data[2] = { 0, 0 };
    rval = mdbImpl->tag_get_data(sense2Tag, &entity, 1, sense_data);
    MB_CHK_SET_ERR(rval, "Failed to get the surface to volume sense data");
    if (sense_data[0] != 0 && sense_data[1] == sense_data[0]) {
      wrt_entities.push_back(sense_data[0]);
      senses.push_back(0);// both
    } else {
      if (sense_data[0] != 0) {
        wrt_entities.push_back(sense_data[0]);
        senses.push_back(1);
      }
      if (sense_data[1] != 0) {
        wrt_entities.push_back(sense_data[1]);
        senses.push_back(-1);
      }

    }

  }
  // filter the results with the sets that are in the model at this time
  // this was introduced because extracting some sets (e.g. neumann set, with mbconvert)
  //   from a model would leave some sense tags not defined correctly
  // also, the geom ent set really needs to be part of the current model set
  unsigned int currentSize =0;

  for (unsigned int index=0; index<wrt_entities.size(); index++)
  {
    EntityHandle wrt_ent=wrt_entities[index];
    if (wrt_ent )
    {
      if (mdbImpl->contains_entities(modelSet, &wrt_ent, 1))
      {
        wrt_entities[currentSize] = wrt_entities[index];
        senses[currentSize] = senses[index];
        currentSize++;
      }
    }
  }
  wrt_entities.resize(currentSize);
  senses.resize(currentSize);
  //
  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::set_senses(EntityHandle entity, std::vector<
    EntityHandle> &wrt_entities, std::vector<int> &senses)
{
  // not efficient, and maybe wrong
  for (unsigned int i = 0; i < wrt_entities.size(); i++) {
    ErrorCode rval = set_sense(entity, wrt_entities[i], senses[i]);
    MB_CHK_SET_ERR(rval, "Failed to set the sense");
  }

  return MB_SUCCESS;
}


ErrorCode GeomTopoTool::next_vol(EntityHandle surface, EntityHandle old_volume,
                                 EntityHandle& new_volume)
{
  std::vector<EntityHandle> parents;
  ErrorCode rval = mdbImpl->get_parent_meshsets( surface, parents );

  if (MB_SUCCESS == rval) {
    if (parents.size() != 2)
      rval = MB_FAILURE;
    else if (parents.front() == old_volume)
      new_volume = parents.back();
    else if( parents.back() == old_volume )
      new_volume = parents.front();
    else
      rval = MB_FAILURE;
  }

  if( rval != MB_SUCCESS ){
    std::cerr << "mesh error in next_vol for surf " << surface << std::endl;  // todo: use geomtopotool to get id by entity handle
  }

  return rval;

}

  
ErrorCode GeomTopoTool::check_geom_tag(bool create) {
  ErrorCode rval;
  unsigned flags = create ? MB_TAG_DENSE|MB_TAG_CREAT : MB_TAG_DENSE;
  if (!geomTag) {
    //get any kind of tag that already exists
    rval = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag, flags);
    MB_CHK_SET_ERR(rval, "Could not get/create the geometry dimension tag");
  }
  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::check_gid_tag(bool create) {
  ErrorCode rval;
  unsigned flags = create ? MB_TAG_DENSE|MB_TAG_CREAT : MB_TAG_DENSE;
  if (!gidTag) {
    //get any kind of tag that already exists
    rval = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gidTag, flags);
    MB_CHK_SET_ERR(rval, "Could not get/create the global id tag");
  }
  return MB_SUCCESS;
}

// move the sense tag existence creation in private methods
// verify sense face tag
ErrorCode GeomTopoTool::check_face_sense_tag(bool create)
{
  ErrorCode rval;
  unsigned flags = create ? MB_TAG_SPARSE|MB_TAG_CREAT|MB_TAG_ANY : MB_TAG_SPARSE|MB_TAG_ANY;
  if (!sense2Tag) {
    EntityHandle def_val[2] = {0, 0};
    rval = mdbImpl->tag_get_handle(GEOM_SENSE_2_TAG_NAME, 2,
				   MB_TYPE_HANDLE, sense2Tag, flags, def_val);
    MB_CHK_SET_ERR(rval, "Could not get/create the sense2Tag");
  }
  return MB_SUCCESS;
}

  // verify sense edge tags
ErrorCode GeomTopoTool::check_edge_sense_tags(bool create)
{
  ErrorCode rval;
  unsigned flags = MB_TAG_VARLEN|MB_TAG_SPARSE;
  if (create) flags |= MB_TAG_CREAT;
  if (!senseNEntsTag) {
    rval = mdbImpl->tag_get_handle(GEOM_SENSE_N_ENTS_TAG_NAME,
                                   0, MB_TYPE_HANDLE, senseNEntsTag, flags);
    MB_CHK_SET_ERR(rval, "Failed to get the curve to surface entity tag handle");
    rval = mdbImpl->tag_get_handle(GEOM_SENSE_N_SENSES_TAG_NAME,
                                   0, MB_TYPE_INTEGER, senseNSensesTag, flags);
    MB_CHK_SET_ERR(rval, "Failed to get the curve to surface sense tag handle");
  }
  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::add_geo_set(EntityHandle set, int dim, int gid)
{
  if (dim <0 || dim > 4)
    MB_SET_ERR(MB_FAILURE, "Invalid geometric dimension provided");

  // see if it is not already set
  if (geomRanges[dim].find(set) != geomRanges[dim].end())
  {
    return MB_SUCCESS; // nothing to do, we already have it as a geo set of proper dimension
  }
  updated=false;// this should trigger at least an obb recomputation
  // get the geom topology tag
  ErrorCode result;
  if (0 == geomTag) {
    result = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag);
    MB_CHK_SET_ERR(result, "Failed to get the geometry dimension tag handle");
  }

  if (0 == gidTag) {
    result = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gidTag);
    MB_CHK_SET_ERR(result, "Failed to get the global id tag handle");
  }

  // make sure the added set has the geom tag properly set
  result = mdbImpl->tag_set_data(geomTag, &set, 1, &dim);
  MB_CHK_SET_ERR(result, "Failed set the geometry dimension tag value");
  
  geomRanges[dim].insert(set);
  // not only that, but also add it to the root model set
  if (modelSet)
  {
    result = mdbImpl->add_entities(modelSet, &set, 1);
    MB_CHK_SET_ERR(result, "Failed to add new geometry set to the tool's modelSet");
  }

  // set the global ID value
  // if passed 0, just increase the max id for the dimension
  if (0 == gid)
  {
    gid = ++maxGlobalId[dim];
  }
  
  result = mdbImpl->tag_set_data(gidTag, &set, 1, &gid);
  MB_CHK_SET_ERR(result, "Failed to get the global id tag value for the geom entity");
  
  return MB_SUCCESS;
}

// will assume no geo sets are defined for this surface
  // will output a mesh_set that contains everything (all sets of interest), for proper output
ErrorCode GeomTopoTool::geometrize_surface_set(EntityHandle surface, EntityHandle & output)
{
  // usual scenario is to read a surface smf file, and "geometrize" it, and output it as a
  //  h5m file with proper sets, tags defined for mesh-based geometry

  // get all triangles/quads from the surface, then build loops
  // we may even have to
  // proper care has to be given to the orientation, material to the left!!!
  // at some point we may have to reorient triangles, not only edges, for proper definition
  bool debugFlag = false;

  Range surface_ents, edge_ents, loop_range;

  // most of these should be triangles and quads
  ErrorCode  rval = mdbImpl->get_entities_by_dimension(surface, 2, surface_ents);
  MB_CHK_SET_ERR(rval, "Failed to get the surface entities");

  EntityHandle face =  surface;
  if (!surface)// in the case it is root set, create another set
  {
    rval = mdbImpl->create_meshset(MESHSET_SET, face);
    MB_CHK_SET_ERR(rval, "Failed to create a the new surface meshset");
  }
  // set the geo tag
  rval = add_geo_set(face, 2);
  MB_CHK_SET_ERR(rval, "Failed to add the geometry set to the tool");

  // this will be our output set, will contain all our new geo sets
  rval = mdbImpl->create_meshset(MESHSET_SET, output);
  MB_CHK_SET_ERR(rval, "Failed to create the output meshset");

  // add first geo set (face) to the output set
  rval = mdbImpl->add_entities(output, &face, 1);
  MB_CHK_SET_ERR(rval, "Failed to add the new meshset to the output meshset");
  
  // how many edges do we need to create?
  // depends on how many loops we have
  // also, we should avoid non-manifold topology
  if (!surface) {// in this case, surface is root, so we have to add entities
    rval = mdbImpl->add_entities(face, surface_ents);
    MB_CHK_SET_ERR(rval, "Failed to add surface entities to the surface meshset");
  }


  Skinner tool(mdbImpl);
  rval = tool.find_skin(0, surface_ents, 1, edge_ents);
  MB_CHK_SET_ERR(rval, "Failed to skin the surface entities");
  if (debugFlag)
  {
    std::cout<< "skinning edges: " << edge_ents.size() << "\n";
    for (Range::iterator it= edge_ents.begin(); it!=edge_ents.end(); ++it)
    {
      EntityHandle ed=*it;
      std::cout<< "edge: " << mdbImpl->id_from_handle(ed) << " type:" << mdbImpl->type_from_handle(ed)<< "\n" ;
      std::cout << mdbImpl->list_entity(ed);
    }
  }

  std::vector<EntityHandle> edges_loop;

  Range pool_of_edges=edge_ents;
  Range used_edges;// these edges are already used for some loops
  // get a new one

  while (!pool_of_edges.empty())
  {
    // get the first edge, and start a loop with it
    EntityHandle current_edge = pool_of_edges[0];
    if (debugFlag)
    {
      std::cout << "Start current edge: "<<  mdbImpl->id_from_handle(current_edge) <<"\n ";
      std::cout << mdbImpl->list_entity(current_edge);
    }
    // get its triangle / quad and see its orientation
    std::vector<EntityHandle> tris;
    rval = mdbImpl->get_adjacencies(&current_edge, 1, 2, false, tris);
    MB_CHK_SET_ERR(rval, "Failed to get the adjacent triangles to the current edge");
    if (tris.size()!=1 )
      MB_SET_ERR(MB_FAILURE, "Edge not on boundary");

    int side_n, sense, offset;
    rval = mdbImpl->side_number(tris[0], current_edge, side_n, sense, offset);
    MB_CHK_SET_ERR(rval, "Failed to get the current edge's side number");

    const EntityHandle * conn2;
    int nnodes2;
    rval = mdbImpl->get_connectivity(current_edge, conn2, nnodes2);
    MB_CHK_SET_ERR(rval,"Failed to get the current edge's connectivity");
    
    if( nnodes2!=2 )
      MB_SET_ERR(MB_FAILURE, "Incorrect number of nodes found.");

    EntityHandle start_node = conn2[0];
    EntityHandle next_node = conn2[1];
    
    if (sense == -1)
    {
      // revert the edge, and start well
      EntityHandle nn2[2]={conn2[1], conn2[0]};
      rval = mdbImpl-> set_connectivity(current_edge, nn2, 2);
      MB_CHK_SET_ERR(rval, "Failed to set the connectivity of the current edge");
      
      start_node = nn2[0]; // or conn2[0] !!! beware: conn2 is modified
      next_node = nn2[1];// or conn2[1]   !!!
      // reset connectivity of edge
      if (debugFlag)
        std::cout << " current edge needs reversed\n";
    }
    // start a new loop of edges
    edges_loop.clear(); // every edge loop starts fresh
    edges_loop.push_back(current_edge);
    used_edges.insert(current_edge);
    pool_of_edges.erase(current_edge);

    if (debugFlag)
    {
     std::cout << " start node: " << start_node << "\n";
     std::cout << mdbImpl->list_entity(start_node);
     std::cout << " next node: " << next_node << "\n";
     std::cout << mdbImpl->list_entity(next_node);
    }
    while (next_node != start_node)
    {
      // find the next edge in the skin
      std::vector<EntityHandle> candidate_edges;
      rval = mdbImpl->get_adjacencies(&next_node, 1, 1, false, candidate_edges);
      MB_CHK_SET_ERR(rval, "Failed to get the adjacent edges to the next node");
      // filter the edges that are used, or the edges not in the skin
      std::vector<EntityHandle> good_edges;
      for (int k=0; k<(int)candidate_edges.size(); k++)
      {
        EntityHandle edg = candidate_edges[k];
        if (used_edges.find(edg) != used_edges.end())
          continue;
        if (pool_of_edges.find(edg) != pool_of_edges.end() )
          good_edges.push_back(edg);
      }
      if (good_edges.size()!=1)
      {
        std::cout<< " good_edges.size()=" <<  good_edges.size() << " STOP\n";
	MB_SET_ERR(MB_FAILURE, "Number of good edges is not one. Could not complete the loop");
      }
      // see if the orientation is good; if not, revert it

      current_edge = good_edges[0];
      rval = mdbImpl-> get_connectivity(current_edge, conn2, nnodes2);
      MB_CHK_SET_ERR(rval, "Failed to get the connectivity of the current edge");
      if( nnodes2!=2)
	MB_SET_ERR(MB_FAILURE, "Incorrect number of nodes found");

      if (conn2[0] != next_node)
      {
        if (conn2[1]!=next_node)
        {
          // the edge is not connected then to current edge
          // bail out
          std::cout<< "edge " << mdbImpl->id_from_handle (current_edge) << " not connected to node "<<
              next_node << "\n";
          MB_SET_ERR(MB_FAILURE, "Current edge is not connected to node");;
        }
        if (debugFlag)
        {
         std::cout << " revert edge " << mdbImpl->id_from_handle (current_edge) << "\n";
         std::cout << mdbImpl->list_entity(current_edge);
        }
        // orientation should be reversed
        EntityHandle nn2[2]={conn2[1], conn2[0]};
        rval = mdbImpl-> set_connectivity(current_edge, nn2, 2);
	MB_CHK_SET_ERR(rval, "Failed to set the connectivity of the current edge");

        {
         std::cout << "after revert edge " << mdbImpl->id_from_handle (current_edge) << "\n";
         std::cout << mdbImpl->list_entity(current_edge);
         std::cout << " conn2: " << conn2[0] << " " << conn2[1] << "\n";
        }
      }
      // before reversion, conn2 was something { n1, next_node}
      // after reversion, conn2 became {next_node, n1}, so the
      // new next node will be still conn2[1]; big surprise, as
      //  I didn't expect the conn2 to change.
      // it seems that const EntityHandle * conn2 means conn2 cannot be
      // changed, but what is pointed to by it will change when we reset connectivity for edge
      next_node = conn2[1];

      if (debugFlag)
      {
        std::cout << " current edge: "<<  mdbImpl->id_from_handle(current_edge) <<"\n ";
        std::cout << mdbImpl->list_entity(current_edge);
        std::cout << "next node: " << next_node << "\n ";
        std::cout << mdbImpl->list_entity(next_node);
      }
      edges_loop.push_back(current_edge);
      used_edges.insert(current_edge);
      pool_of_edges.erase(current_edge);

    }
    // at this point, we have a loop formed;
    // create a geo edge, a vertex set, and add it to our sets

    EntityHandle edge;
    rval = mdbImpl->create_meshset(MESHSET_ORDERED, edge);
    MB_CHK_SET_ERR(rval, "Failed to create the edge meshset");

    rval = add_geo_set(edge, 1);
    MB_CHK_SET_ERR(rval, "Failed to add the edge meshset to the tool's model set");
    // add the mesh edges:
    // add loops edges to the edge set
    rval = mdbImpl->add_entities(edge, &edges_loop[0], edges_loop.size());//
    MB_CHK_SET_ERR(rval, "Failed to add entities to the edge meshset");
    // create a vertex set
    EntityHandle vertex;
    rval = mdbImpl->create_meshset(MESHSET_SET, vertex);
    MB_CHK_SET_ERR(rval, "Failed to create the vertex meshset");
    rval = add_geo_set(vertex, 0);
    MB_CHK_SET_ERR(rval, "Failed to add the vertex meshset to the tool's model set");
    // add one node to the vertex set

    rval = mdbImpl->add_entities(vertex, &start_node, 1);//
    MB_CHK_SET_ERR(rval, "Failed to add entities to the vertex meshset");
    
    rval = mdbImpl ->add_parent_child( face, edge);
    MB_CHK_SET_ERR(rval, "Failed to create the edge to face parent child relationship");
    
    rval = mdbImpl ->add_parent_child( edge, vertex);
    MB_CHK_SET_ERR(rval, "Failed to create the vertex to edge parent child relationship");

    // the sense of the edge in face is for sure positive (forward)
    rval = set_sense(edge, face, 1);//
    MB_CHK_SET_ERR(rval, "Failed to set the edge to face sense");
    // also add our sets to the output set, to be sure to be exported

    rval = mdbImpl->add_entities(output, &edge, 1);
    MB_CHK_SET_ERR(rval, "Failed to add the edge meshset to the output set");
    rval = mdbImpl->add_entities(output, &vertex, 1);
    MB_CHK_SET_ERR(rval, "Failed to add the vertex meshset to the output set");

    if (debugFlag)
    {
      std::cout << "add edge with start node " << start_node <<
           " with " << edges_loop.size() << " edges\n";
    }

  }


  return MB_SUCCESS;
}


/*
 *  This would create a deep copy of the geom topo model, into a new geom topo tool
 * sets will be duplicated, but entities not
 * modelSet will be a new one
 * if the original set was null (root), a new model set will be created for
 * original model, and its entities will be the original g sets
 * Scenario: split a face along a ground line, then write only one surface
 *   the common line has 2 faces in var len tag for sense edge; if we write only one
 *   surface to a new database, the var len tag of the edge will be extracted with 2 values, but
 *   only one value will make sense, the other will be zero.
 *
 *   There is no workaround; we need to create a duplicate model that has only that surface
 *   and its children (and grand-children). Then the var len sense edge-face tags will have
 *   the right size.
 *
 */
ErrorCode GeomTopoTool::duplicate_model(GeomTopoTool *& duplicate, std::vector<EntityHandle> * pvGEnts)
{
  // will
  EntityHandle rootModelSet;
  ErrorCode rval = mdbImpl->create_meshset(MESHSET_SET, rootModelSet);
  MB_CHK_SET_ERR(rval, "Failed to create the rootModelSet");
  
  if (0 == geomTag) {
    rval = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag);
    MB_CHK_SET_ERR(rval, "Failed to get the geometry dimension tag handle");
    
  }
  if (0 == gidTag) {
    rval = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1,MB_TYPE_INTEGER, gidTag);
    MB_CHK_SET_ERR(rval, "Failed to get the global id tag handle");
    
  }
  // extract from the geomSet the dimension, children, and grand-children
  Range depSets;// dependents of the geomSet, including the geomSet
  // add in this range all the dependents of this, to filter the ones that need to be deep copied

  if (pvGEnts!=NULL)
  {
    unsigned int numGents = pvGEnts->size();
    for (unsigned int k = 0; k<numGents; k++)
    {
      EntityHandle geomSet=(*pvGEnts)[k];
      // will keep accumulating to the depSets range
      rval = mdbImpl->get_child_meshsets(geomSet, depSets, 0); // 0 for numHops means that all
      // dependents are returned, not only the direct children.
      MB_CHK_SET_ERR(rval, "Failed to get the geometry set's child meshsets");
      
      depSets.insert(geomSet);
    }

  }

  // add to the root model set copies of the gsets, with correct sets
  // keep a map between sets to help in copying parent/child relations
  std::map <EntityHandle, EntityHandle> relate;
  // each set will get the same entities as the original
  for (int dim=0; dim<5; dim++)
  {
    int gid = 0;
    unsigned int set_options = ( (1!=dim) ? MESHSET_SET : MESHSET_ORDERED );
    for (Range::iterator it=geomRanges[dim].begin(); it!=geomRanges[dim].end(); ++it)
    {
      EntityHandle set=*it;
      if (pvGEnts != NULL && depSets.find(set)==depSets.end())
        continue; // this means that this set is not of interest, skip it
      EntityHandle newSet;
      rval = mdbImpl->create_meshset(set_options, newSet);
      MB_CHK_SET_ERR(rval, "Failed to create new meshset");
      
      relate[set] = newSet;
      rval = mdbImpl->add_entities(rootModelSet, &newSet, 1);
      MB_CHK_SET_ERR(rval, "Failed to add the new meshset to the tool's modelSet");
      
      // make it a geo set, and give also global id in order
      rval = mdbImpl->tag_set_data(geomTag, &newSet, 1, &dim);
      MB_CHK_SET_ERR(rval, "Failed to set the new meshset's geometry dimension data");
      
      gid++;// increment global id, everything starts with 1 in the new model!
      rval = mdbImpl->tag_set_data(gidTag, &newSet, 1, &gid);
      MB_CHK_SET_ERR(rval, "Failed to get the new meshset's global id data");
      
      if (dim==1)
      {
        // the entities are ordered, we need to retrieve them ordered, and set them ordered
        std::vector<EntityHandle> mesh_edges;
        rval = mdbImpl->get_entities_by_handle(set, mesh_edges);
	MB_CHK_SET_ERR(rval, "Failed to get the meshset entities by handle");
	
        rval = mdbImpl->add_entities(newSet, &(mesh_edges[0]), (int)mesh_edges.size());
	MB_CHK_SET_ERR(rval, "Failed to add the new entities to the new meshset");
	
      }
      else
      {
        Range ents;
        rval = mdbImpl->get_entities_by_handle(set, ents);
	MB_CHK_SET_ERR(rval, "Failed to add the entities to the existing meshset");
	
        rval = mdbImpl->add_entities(newSet, ents);
	MB_CHK_SET_ERR(rval, "Failed to add the entities to the new meshset");
	
      }
      //set parent/child relations if dim>=1
      if (dim>=1)
      {
        Range children;
        // the children of geo sets are only g sets
        rval = mdbImpl->get_child_meshsets(set, children); // num_hops = 1 by default
	MB_CHK_SET_ERR(rval, "Failed to get the child meshsets of the existing set");
	
        for (Range::iterator it2=children.begin(); it2!=children.end(); ++it2)
        {
          EntityHandle newChildSet = relate[*it2];
          rval = mdbImpl->add_parent_child(newSet, newChildSet);
	  MB_CHK_SET_ERR(rval, "Failed to create parent child relationship to the new meshset");
	  
        }
      }

    }
  }

  duplicate = new GeomTopoTool(mdbImpl, true, rootModelSet); // will retrieve the
  // sets and put them in ranges

  // this is the lazy way to it:
  // newgtt->restore_topology_from_adjacency(); // will reset the sense entities, and with this, the model
  // represented by this new gtt will be complete
  // set senses by peeking at the old model
  // make sure we have the sense tags defined
  rval = check_face_sense_tag(true);
  MB_CHK_SET_ERR(rval, "Failed to check the face to volume sense tag handle");
  
  rval = check_edge_sense_tags(true);
  MB_CHK_SET_ERR(rval, "Failed to check the curve to surface sense tag handles");


  for (int dd=1; dd<=2; dd++) // do it for surfaces and edges
  {
    for (Range::iterator it=geomRanges[dd].begin(); it!=geomRanges[dd].end(); ++it)
    {
      EntityHandle surf=*it;
      if (pvGEnts != NULL && depSets.find(surf)==depSets.end())
        continue; // this means that this set is not of interest, skip it
      EntityHandle newSurf = relate[surf];
      // we can actually look at the tag data, to be more efficient
      // or use the
      std::vector<EntityHandle> solids;
      std::vector<int> senses;
      rval = this->get_senses(surf, solids, senses);
      MB_CHK_SET_ERR(rval, "Failed to get the sense data for the surface with respect to its volumes");
      
      std::vector<EntityHandle> newSolids;
      std::vector<int> newSenses;
      for (unsigned int i = 0; i<solids.size(); i++)
      {
        if (pvGEnts != NULL && depSets.find(solids[i])==depSets.end())
          continue; // this means that this set is not of interest, skip it
        EntityHandle newSolid = relate[solids[i]];
        // see which "solids" are in the new model
        newSolids.push_back(newSolid);
        newSenses.push_back(senses[i]);
      }
      rval = duplicate->set_senses(newSurf, newSolids, newSenses);
      MB_CHK_SET_ERR(rval, "Failed to set the sense data for the surface with respect to the new volumes");
      
    }
  }
  // if the original root model set for this model is 0 (root set), then create
  // a new set and put all the old sets in the new model set
  // in this way, the existing gtt remains valid (otherwise, the modelSet would contain all the
  // gsets, the old ones and the new ones; the root set contains everything)
  if (modelSet==0)
  {
    rval = mdbImpl->create_meshset(MESHSET_SET, modelSet);
    MB_CHK_SET_ERR(rval, "Failed to create the modelSet meshset");
    
    // add to this new set all previous sets (which are still in ranges)
    for (int dim=0; dim<5; dim++)
    {
      rval = mdbImpl->add_entities(modelSet, geomRanges[dim]);
      MB_CHK_SET_ERR(rval, "Failed to add the geometric meshsets to the tool's modelSet");
      
    }

  }
  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::get_implicit_complement(EntityHandle &implicit_complement) {
  if ( impl_compl_handle ) {
    implicit_complement = impl_compl_handle;
    return MB_SUCCESS;
  }
  else {
    return MB_ENTITY_NOT_FOUND;
  }
}

ErrorCode GeomTopoTool::setup_implicit_complement() {

  // if the implicit complement is already setup,
  // we're done
  if( impl_compl_handle != 0 ) {
    std::cout << "IPC already exists!" << std::endl;
    return MB_SUCCESS;
  }

  // if not, then query for a set with it's name
  Range entities;
  const void* const tagdata[] = {IMPLICIT_COMPLEMENT_NAME};
  ErrorCode rval = mdbImpl->get_entities_by_type_and_tag( modelSet, MBENTITYSET,
                                                           &nameTag, tagdata, 1,
                                                           entities );
  // query error
  MB_CHK_SET_ERR(rval, "Unable to query for implicit complement");

  // if we found exactly one, set it as the implicit complement
  if(entities.size() == 1) {
    impl_compl_handle = entities.front();
    return MB_SUCCESS;
  }
  
  // found too many
  if (entities.size() > 1) 
    MB_CHK_SET_ERR(MB_MULTIPLE_ENTITIES_FOUND, "Too many implicit complement sets");

  // found none
  if (entities.empty()) {
    // create implicit complement if requested
    rval = generate_implicit_complement(impl_compl_handle);
    MB_CHK_SET_ERR(rval, "Could not create implicit complement");
    
    rval = mdbImpl->tag_set_data(nameTag, &impl_compl_handle, 1, &IMPLICIT_COMPLEMENT_NAME);
    MB_CHK_SET_ERR(rval, "Could not set the name tag for the implicit complement");
    
    rval = add_geo_set(impl_compl_handle, 3);
    MB_CHK_SET_ERR(rval, "Failed to add implicit complement to model");
    
    // assign category tag - this is presumably for consistency so that the
    // implicit complement has all the appearance of being the same as any
    // other volume
    Tag category_tag;
    rval = mdbImpl->tag_get_handle(CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE,
				   MB_TYPE_OPAQUE, category_tag, MB_TAG_SPARSE|MB_TAG_CREAT);
    MB_CHK_SET_ERR(rval, "Could not get the category tag");
    
    static const char volume_category[CATEGORY_TAG_SIZE] = "Volume\0";
    rval = mdbImpl->tag_set_data(category_tag, &impl_compl_handle, 1, volume_category );
    MB_CHK_SET_ERR(rval, "Could not set the category tag for the implicit complement");
    
    return MB_SUCCESS;
  }
  
  return MB_FAILURE;
}

ErrorCode GeomTopoTool::generate_implicit_complement(EntityHandle &implicit_complement_set) {

  ErrorCode rval;
  rval= mdbImpl->create_meshset(MESHSET_SET,implicit_complement_set);
  MB_CHK_SET_ERR(rval, "Failed to create mesh set for implicit complement");

  // make sure the sense2Tag is set
  if(!sense2Tag){
    check_face_sense_tag(true);
  }

  // get all geometric surface sets
  Range surfs;
  rval = get_gsets_by_dimension(2, surfs);
  MB_CHK_SET_ERR(rval, "Could not get surface sets");
  
  // search through all surfaces
  std::vector<EntityHandle> parent_vols;  
  for (Range::iterator surf_i = surfs.begin(); surf_i != surfs.end(); ++surf_i) {

    parent_vols.clear();
    // get parents of each surface
    rval = mdbImpl->get_parent_meshsets( *surf_i, parent_vols );
    MB_CHK_SET_ERR(rval, "Failed to get volume meshsets");

    // if only one parent, get the OBB root for this surface
    if (parent_vols.size() == 1 ) {

      // add this surf to the topology of the implicit complement volume
      rval = mdbImpl->add_parent_child(implicit_complement_set,*surf_i);
      MB_CHK_SET_ERR(rval, "Could not add surface to implicit complement set");
      
      // get the surface sense wrt original volume
      EntityHandle sense_data[2] = {0,0};
      rval = get_surface_senses(*surf_i, sense_data[0], sense_data[1]);
      MB_CHK_SET_ERR(rval, "Could not get surface sense data");

      // set the surface sense wrt implicit complement volume
      if(0==sense_data[0] && 0==sense_data[1])
	MB_SET_ERR(MB_FAILURE, "No sense data for current surface");
      if(0==sense_data[0])
        sense_data[0] = implicit_complement_set;
      else if(0==sense_data[1])
        sense_data[1] = implicit_complement_set;
      else
	MB_SET_ERR(MB_FAILURE, "Could not insert implicit complement into surface sense data");
      
      // set the new sense data for this surface
      rval = set_surface_senses(*surf_i, sense_data[0], sense_data[1]);
      MB_CHK_SET_ERR(rval, "Failed to set sense tag data");
    }
  } //end surface loop
  
  return MB_SUCCESS;
}
  
#define  RETFALSE(a, b) { std::cout<<a<<"\n"; mdbImpl->list_entity(b); return false; }
bool GeomTopoTool::check_model()
{
  // vertex sets should have one node
  Range::iterator rit;
  ErrorCode rval;
  for (rit = geomRanges[0].begin(); rit!=geomRanges[0].end(); ++rit)
  {
    EntityHandle vSet = *rit;
    Range nodes;
    rval = mdbImpl->get_entities_by_handle(vSet, nodes);
    if (MB_SUCCESS!=rval)
      RETFALSE(" failed to get nodes from vertex set ", vSet);
    if (nodes.size()!=1)
      RETFALSE(" number of nodes is different from 1 ", vSet)
    EntityType type = mdbImpl->type_from_handle(nodes[0]);
    if (type != MBVERTEX)
      RETFALSE(" entity in vertex set is not a node ", nodes[0])
    // get all parents, and see if they belong to geomRanges[1]
    Range edges;
    rval = mdbImpl->get_parent_meshsets(vSet, edges);
    if (MB_SUCCESS!=rval)
      RETFALSE(" can't get parent edges for a node set ", vSet)
    Range notEdges = subtract(edges, geomRanges[1] );
    if (!notEdges.empty())
      RETFALSE(" some parents of a node set are not geo edges ", notEdges[0])
  }

  // edges to be formed by continuous chain of mesh edges, oriented correctly
  for (rit = geomRanges[1].begin(); rit!=geomRanges[1].end(); ++rit)
  {
    EntityHandle edge = *rit;
    std::vector<EntityHandle> mesh_edges;
    rval = mdbImpl->get_entities_by_type(edge, MBEDGE, mesh_edges);
    if (MB_SUCCESS!=rval)
      RETFALSE(" can't get mesh edges from edge set", edge)
    int num_edges = (int)mesh_edges.size();
    if (num_edges==0)
      RETFALSE(" no mesh edges in edge set ", edge)
    EntityHandle firstNode;
    EntityHandle currentNode; // will also hold the last node in chain of edges
    const EntityHandle * conn2;
    int nnodes2;
    // get all parents, and see if they belong to geomRanges[1]
    for (int i=0; i<num_edges; i++)
    {
      rval = mdbImpl->get_connectivity(mesh_edges[i], conn2, nnodes2);
      if (MB_SUCCESS!=rval || nnodes2!=2)
        RETFALSE(" mesh edge connectivity is wrong ", mesh_edges[i])
      if (i==0)
      {
        firstNode = conn2[0];
        currentNode = conn2[1];
      }

      else // if ( (i>0) )
      {
        // check the current node is conn[0]
        if (conn2[0]!=currentNode)
        {
          std::cout<<"i="<<i  << " conn2:" << conn2[0] << " " << conn2[1] << " currentNode:" << currentNode << "\n";
          mdbImpl->list_entity(mesh_edges[i]);
          RETFALSE(" edges are not contiguous in edge set ", edge)
        }
        currentNode = conn2[1];
      }
    }
    // check the children of the edge set; do they have the first and last node?
    Range vertSets;
    rval = mdbImpl->get_child_meshsets(edge, vertSets);
    if (MB_SUCCESS!=rval)
      RETFALSE(" can't get vertex children ", edge)
    Range notVertices = subtract(vertSets, geomRanges[0] );
    if (!notVertices.empty())
      RETFALSE(" children sets that are not vertices ", notVertices[0])
    for (Range::iterator it=vertSets.begin(); it!=vertSets.end(); ++it)
    {
      if ( !mdbImpl->contains_entities(*it,  &firstNode,  1)&&
          !mdbImpl->contains_entities(*it,  &currentNode,  1) )
        RETFALSE(" a vertex set is not containing the first and last nodes ", *it)
    }
    // check now the faces / parents
    Range faceSets;
    rval = mdbImpl->get_parent_meshsets(edge, faceSets);
    if (MB_SUCCESS!=rval)
      RETFALSE(" can't get edge parents ", edge)
    Range notFaces = subtract(faceSets, geomRanges[2] );
    if (!notFaces.empty())
      RETFALSE(" parent sets that are not faces ", notFaces[0])

    // for a geo edge, check the sense tags with respect to the adjacent faces
    // in general, it is sufficient to check one mesh edge (the first one)
    // edge/face  senses
    EntityHandle firstMeshEdge = mesh_edges[0];
    // get all entities/elements adjacent to it
    Range adjElem;
    rval = mdbImpl->get_adjacencies(&firstMeshEdge, 1, 2, false, adjElem);
    if (MB_SUCCESS!=rval)
      RETFALSE(" can't get adjacent elements to the edge ", firstMeshEdge)
    for (Range::iterator it2=adjElem.begin(); it2!=adjElem.end(); ++it2)
    {
      EntityHandle elem=*it2;
      // find the geo face it belongs to
      EntityHandle gFace=0;
      for (Range::iterator  fit=faceSets.begin(); fit!=faceSets.end(); ++fit)
      {
        EntityHandle possibleFace = *fit;
        if (mdbImpl->contains_entities(possibleFace,  &elem,  1) )
        {
          gFace=possibleFace;
          break;
        }
      }
      if (0==gFace)
        RETFALSE(" can't find adjacent surface that contains the adjacent element to the edge ", firstMeshEdge)

      // now, check the sense of mesh_edge in element, and the sense of gedge in gface
      // side_number
      int side_n, sense, offset;
      rval = mdbImpl->side_number(elem, firstMeshEdge, side_n, sense, offset);
      if (MB_SUCCESS != rval)
        RETFALSE(" can't get sense and side number of an element ", elem)
      // now get the sense
      int topoSense;
      rval = this->get_sense(edge, gFace, topoSense);
      if (topoSense!=sense)
        RETFALSE(" geometric topo sense and element sense do not agree ", edge)
    }

  }
  // surfaces to be true meshes
  // for surfaces, check that the skinner will find the correct boundary

  // use the skinner for boundary check
  Skinner tool(mdbImpl);

  for (rit = geomRanges[2].begin(); rit!=geomRanges[2].end(); ++rit)
  {
    EntityHandle faceSet = *rit;
    // get all boundary edges (adjacent edges)

    Range edges;
    rval = mdbImpl->get_child_meshsets(faceSet, edges);
    if (MB_SUCCESS!=rval)
      RETFALSE(" can't get children edges for a face set ", faceSet)
    Range notEdges = subtract(edges, geomRanges[1] );
    if (!notEdges.empty())
      RETFALSE(" some children of a face set are not geo edges ", notEdges[0])

    Range boundary_mesh_edges;
    for (Range::iterator it = edges.begin(); it!=edges.end(); ++it)
    {
      rval = mdbImpl->get_entities_by_type(*it, MBEDGE, boundary_mesh_edges);
      if (MB_SUCCESS!=rval)
        RETFALSE(" can't get edge elements from the edge set ", *it)
    }
    // skin the elements of the surface
      // most of these should be triangles and quads
    Range surface_ents, edge_ents;
    rval = mdbImpl->get_entities_by_dimension(faceSet, 2, surface_ents);
    if (MB_SUCCESS!=rval)
      RETFALSE(" can't get surface elements from the face set ", faceSet)

    rval = tool.find_skin(0, surface_ents, 1, edge_ents);
    if (MB_SUCCESS != rval)
      RETFALSE("can't skin a surface ", surface_ents[0])

    // those 2 ranges for boundary edges now must be equal
    if (boundary_mesh_edges!=edge_ents)
      RETFALSE("boundary ranges are different", boundary_mesh_edges[0])

  }

  // solids to be filled correctly, maybe a skin procedure too.
  // (maybe the solids are empty)

  return true;
}

bool GeomTopoTool::have_obb_tree() {
  return rootSets.size() !=0 || mapRootSets.size() !=0;
}

// This function gets coordinates of the minimum and maxmiumum points
// from an OBB/AABB, ie. such that these points represent
// the maximum and minium extents of an AABB
ErrorCode GeomTopoTool::get_bounding_coords(EntityHandle volume, double minPt[3],
                          double maxPt[3])
{
  double center[3], axis1[3], axis2[3], axis3[3];

    // get center point and vectors to OBB faces
  ErrorCode rval = get_obb(volume, center, axis1, axis2, axis3);
  MB_CHK_SET_ERR(rval, "Failed to get the oriented bounding box of the volume");

    // compute min and max vertices
  for (int i=0; i<3; i++)
  {
    double sum = fabs(axis1[i]) + fabs(axis2[i]) + fabs(axis3[i]);
    minPt[i] = center[i] - sum;
    maxPt[i] = center[i] + sum;
  }
  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::get_obb(EntityHandle volume, double center[3], double axis1[3],
                          double axis2[3], double axis3[3])
{
  //find EntityHandle node_set for use in box
  EntityHandle root;
  ErrorCode rval = get_root(volume, root);
  MB_CHK_SET_ERR(rval, "Failed to get volume's obb tree root");

  // call box to get center and vectors to faces
  return obbTree->box(root, center, axis1, axis2, axis3);

}

Range GeomTopoTool::get_ct_children_by_dimension(EntityHandle parent, int desired_dimension)
{
  Range all_children, desired_children;
  Range::iterator it;
  int actual_dimension;

  desired_children.clear();
  all_children.clear();
  mdbImpl->get_child_meshsets(parent, all_children);
  
  for ( it = all_children.begin() ; it != all_children.end() ; ++it) {
    mdbImpl->tag_get_data(geomTag, &(*it), 1, &actual_dimension);
    if ( actual_dimension == desired_dimension )
      desired_children.insert(*it);
  }
  
  return desired_children;
}

// runs GeomQueryTool point_in_vol and to test if vol A is inside vol B
//  returns true or false
  bool GeomTopoTool::A_is_in_B(EntityHandle volume_A, EntityHandle volume_B, GeomQueryTool* GQT)
{
  ErrorCode rval;

  Range child_surfaces, triangles, vertices;
  double coord[3]; // coord[0] = x, etc.
  int result; // point in vol result; 0=F, 1=T

  // find coordinates of any point on surface of A
  // get surface corresponding to volume, then get the triangles  
  child_surfaces = get_ct_children_by_dimension(volume_A, 2);
  rval = mdbImpl->get_entities_by_type(*child_surfaces.begin(), MBTRI, triangles); MB_CHK_ERR(rval);

  // now get 1st triangle vertices
  rval = mdbImpl->get_connectivity(&(*triangles.begin()),1,vertices); MB_CHK_ERR(rval);
  
  // now get coordinates of first vertex
  rval = mdbImpl->get_coords(&(*vertices.begin()), 1 , &(coord[0])); MB_CHK_ERR(rval);

  // if point on A is inside vol B, return T; o.w. return F
  rval = GQT->point_in_volume(volume_B, coord, result);
  MB_CHK_SET_ERR(rval, "Failed to complete point in volume query.");
   
  return (result != 0);
}  
  
                                          
ErrorCode GeomTopoTool::insert_in_tree(EntityHandle ct_root, EntityHandle volume, GeomQueryTool* GQT)
{
  ErrorCode rval;

  bool inserted = false;
  EntityHandle current_volume = volume; // volume to be inserted 
  EntityHandle tree_volume = ct_root; // volume already existing in the tree
  EntityHandle parent=ct_root;
  Range child_volumes;

  // while not inserted in tree
  while ( !inserted )
    {
      // if current volume is insde of tree volume-- always true if tree volume
	  // is the root of the tree
      if ( tree_volume == ct_root ||
		  (tree_volume != ct_root && A_is_in_B(current_volume, tree_volume, GQT)) ) {
       
	     parent = tree_volume;  
        
        // if tree_volume has children then we must test them,
        // (tree_volume will change)
        child_volumes = get_ct_children_by_dimension(tree_volume, 3);
        if (child_volumes.size() > 0 ) 
          tree_volume = child_volumes.pop_front();
        // otherwise current_volume is the only child of the tree volume
        else { 
          rval = mdbImpl->add_parent_child(parent, current_volume);
          MB_CHK_SET_ERR(rval, "Failed to add parent-child relationship.");
          
          inserted = true;
        }
      // if current volume is not in the tree volume, the converse may be true
      } else {
        // if the tree volume is inside the current volume
        if( A_is_in_B(tree_volume, current_volume, GQT) ) {
          // reverse their parentage
          rval = mdbImpl->remove_parent_child(parent, tree_volume);
          MB_CHK_SET_ERR(rval, "Failed to remove parent-child relationship.");
          rval = mdbImpl->add_parent_child(current_volume, tree_volume);
          MB_CHK_SET_ERR(rval, "Failed to add parent-child relationship.");
        }
        
        if (child_volumes.size() == 0 ) {
          rval = mdbImpl->add_parent_child(parent, current_volume);
          MB_CHK_SET_ERR(rval, "Failed to add parent-child relationship.");
          inserted = true;
        } else
          tree_volume = child_volumes.pop_front();
      }   
    }
  return MB_SUCCESS;
}


ErrorCode GeomTopoTool::restore_topology_from_geometric_inclusion(const Range &flat_volumes) {

  ErrorCode rval;
  // local var will go out of scope if errors appear, no need to free it also
  GeomQueryTool  GQT(this);
  std::map<EntityHandle,EntityHandle> volume_surface; //map of volume
                                                      // to its surface

  EntityHandle ct_root;
  // create root meshset-- this will be top of tree
  std::string meshset_name = "build_hierarchy_root";
  rval = mdbImpl->create_meshset( MESHSET_SET, ct_root); MB_CHK_ERR(rval);
  rval = mdbImpl->tag_set_data( nameTag, &ct_root, 1, meshset_name.c_str()); MB_CHK_ERR(rval);

  for ( Range::iterator vol = flat_volumes.begin(); vol != flat_volumes.end(); vol++) {
    //get the surface corresponding to each volume
    // at this point, each volume meshset only has one 'child' surface
    // which exactly corresponds to that volume
    Range child_surfaces = get_ct_children_by_dimension(*vol, 2);
    volume_surface[*vol]=*child_surfaces.begin();
    
    rval = insert_in_tree(ct_root, *vol, &GQT);
    MB_CHK_SET_ERR(rval,"Failed to insert volume into tree.");
  }
  
  //for each original volume, get its child volumes
  for ( Range::iterator parent_it = flat_volumes.begin() ; parent_it != flat_volumes.end(); parent_it++)
    {
      Range volume_children = get_ct_children_by_dimension(*parent_it, 3);
      
      if (volume_children.size() !=0)
        {
          //loop over all of original volume's child volumes
          for ( Range::iterator child_it = volume_children.begin() ;
                child_it != volume_children.end() ; ++child_it )
            {
              //set the sense of the surface mapped to the child volume to REVERSE
              // wrt the parent volume
              rval = set_sense(volume_surface[*child_it], *parent_it, SENSE_REVERSE);
              MB_CHK_SET_ERR(rval, "Failed to set sense.");
              
              //add the child volume's surface as a child of the original volume
              // and delete the child volume as a child of original volume
              rval = mdbImpl->add_parent_child(*parent_it,volume_surface[*child_it]);
              MB_CHK_SET_ERR(rval, "Failed to add parent-child relationship.");
              rval = mdbImpl->remove_parent_child(*parent_it,*child_it);
              MB_CHK_SET_ERR(rval, "Failed to remove parent-child relationship.");
            }
        }
      
    }
  
  return MB_SUCCESS;
}  
  

} // namespace moab
