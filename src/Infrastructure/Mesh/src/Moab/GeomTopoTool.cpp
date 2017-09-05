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
#include "moab/Range.hpp"
#include "MBTagConventions.hpp"
#include "moab/Interface.hpp"
#include "moab/CN.hpp"
#include "moab/Skinner.hpp"
#include "Internals.hpp"
#include <assert.h>
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

GeomTopoTool::GeomTopoTool(Interface *impl, bool find_geoments, EntityHandle modelRootSet) :
  mdbImpl(impl), sense2Tag(0), senseNEntsTag(0), senseNSensesTag(0),
  geomTag(0), gidTag(0), modelSet(modelRootSet), updated(false), obbTree(impl, NULL, true),
  setOffset(0), contiguous(true), oneVolRootSet(0)
{

  ErrorCode result = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1,
      MB_TYPE_INTEGER, geomTag, MB_TAG_CREAT|MB_TAG_SPARSE);
  if (MB_SUCCESS != result) {
    std::cerr << "Error: Failed to create geometry dimension tag." << std::endl;
  }
  // global id tag is not really needed, but mbsize complains if we do not set it for
  // geometry entities

  result = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, 
        MB_TYPE_INTEGER, gidTag, MB_TAG_CREAT|MB_TAG_DENSE);
  if (MB_SUCCESS != result && MB_ALREADY_ALLOCATED != result) {
    std::cerr << "Error: Failed to create global id tag." << std::endl;
  }

  maxGlobalId[0] = maxGlobalId[1] = maxGlobalId[2] = maxGlobalId[3] =maxGlobalId[4] =0;
  if (find_geoments)
    find_geomsets();
}

int GeomTopoTool::dimension(EntityHandle this_set)
{
  ErrorCode result;
  if (0 == geomTag) {
    result = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag);
    if (MB_SUCCESS != result)
      return result;
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
    if (MB_SUCCESS != result)
      return result;
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
ErrorCode GeomTopoTool::other_entity(EntityHandle bounded,
    EntityHandle not_this, EntityHandle across, EntityHandle &other)
{
  other = 0;

  // get all children of bounded
  Range bdy, tmpr;
  ErrorCode rval = mdbImpl->get_child_meshsets(bounded, bdy);
  if (MB_SUCCESS != rval)
    return rval;

  // get all the parents of across
  rval = mdbImpl->get_parent_meshsets(across, tmpr);

  // possible candidates is the intersection
  bdy = intersect(bdy, tmpr);

  // if only two, choose the other
  if (1 == bdy.size()) {
    assert(*bdy.begin() == not_this);
    return MB_SUCCESS;
  } else if (2 == bdy.size()) {
    if (*bdy.begin() == not_this)
      other = *bdy.rbegin();
    if (*bdy.rbegin() == not_this)
      other = *bdy.begin();
    else
      return MB_FAILURE;
  } else {
    // attempt to find right answer using senses, though we might be screwed anyway
    assert(false);
  }

  return MB_SUCCESS;
}
ErrorCode GeomTopoTool::find_geomsets(Range *ranges)
{
  // get all sets with this tag
  Range geom_sets;
  ErrorCode result = mdbImpl->get_entities_by_type_and_tag(modelSet, MBENTITYSET,
      &geomTag, NULL, 1, geom_sets);
  if (MB_SUCCESS != result || geom_sets.empty())
    return result;

  result = separate_by_dimension(geom_sets);
  if (MB_SUCCESS != result)
    return result;

  if (ranges) {
    for (int i = 0; i < 5; i++)
    {
      ranges[i] = geomRanges[i];
    }
  }

  return MB_SUCCESS;
}

ErrorCode GeomTopoTool::construct_obb_trees(bool make_one_vol)
{
  ErrorCode rval;

  // get all surfaces and volumes
  Range surfs, vols, vol_trees;
  const int three = 3;
  const void* const three_val[] = { &three };
  rval = mdbImpl->get_entities_by_type_and_tag(modelSet, MBENTITYSET, &geomTag,
      three_val, 1, vols);
  if (MB_SUCCESS != rval)
    return rval;

  const int two = 2;
  const void* const two_val[] = { &two };
  rval = mdbImpl->get_entities_by_type_and_tag(modelSet, MBENTITYSET, &geomTag,
      two_val, 1, surfs);
  if (MB_SUCCESS != rval)
    return rval;

  if (vols.empty() && !surfs.empty()) {
    setOffset = surfs.front();
  } else if (!vols.empty() && surfs.empty()) {
    setOffset = vols.front();
  } else {
    setOffset = (surfs.front() < vols.front() ? surfs.front() : vols.front());
  }
  EntityHandle minSet = setOffset;
  EntityHandle maxSet = setOffset;
  Range::iterator it;
  for (it = surfs.begin(); it != surfs.end(); ++it) {
    EntityHandle sf = *it;
    if (sf > maxSet)
      maxSet = sf;
    if (sf < minSet)
      minSet = sf;
  }
  for (it = vols.begin(); it != vols.end(); ++it) {
    EntityHandle sf = *it;
    if (sf > maxSet)
      maxSet = sf;
    if (sf < minSet)
      minSet = sf;
  }
  if (surfs.size() + vols.size() == maxSet - minSet + 1)
    contiguous = true;
  else
    contiguous = false; // need map arrangements
  // for surface
  EntityHandle root;
  if (contiguous)
    rootSets.resize(surfs.size() + vols.size());
  for (Range::iterator i = surfs.begin(); i != surfs.end(); ++i) {
    Range tris;
    rval = mdbImpl->get_entities_by_dimension(*i, 2, tris);
    if (MB_SUCCESS != rval)
      return rval;

    if (tris.empty()) {
      std::cerr << "WARNING: Surface has no facets." << std::endl;
    }

    rval = obbTree.build(tris, root);
    if (MB_SUCCESS != rval)
      return rval;

    rval = mdbImpl->add_entities(root, &*i, 1);
    if (MB_SUCCESS != rval)
      return rval;

    //surfRootSets[*i - surfOffset] = root;
    if (contiguous)
      rootSets[*i - setOffset] = root;
    else
      mapRootSets[*i] = root;
  }

  // for volumes
  Range trees;
  for (Range::iterator i = vols.begin(); i != vols.end(); ++i) {
    // get all surfaces in volume
    Range tmp_surfs;
    rval = mdbImpl->get_child_meshsets(*i, tmp_surfs);
    if (MB_SUCCESS != rval)
      return rval;

    // get OBB trees for each surface
    if (!make_one_vol)
      trees.clear();
    for (Range::iterator j = tmp_surfs.begin(); j != tmp_surfs.end(); ++j) {
      rval = get_root(*j, root);
      if (MB_SUCCESS != rval )
        return rval;
      if(!root)
        return MB_FAILURE;
      trees.insert(root);
    }

    // build OBB tree for volume
    if (!make_one_vol) {
      rval = obbTree.join_trees(trees, root);
      if (MB_SUCCESS != rval)
        return rval;
      if (contiguous)
        rootSets[*i - setOffset] = root;
      else
        mapRootSets[*i] = root;
    }
  }

  // build OBB tree for volume
  if (make_one_vol) {
    rval = obbTree.join_trees(trees, root);
    if (MB_SUCCESS != rval)
      return rval;
    oneVolRootSet = root;
  }

  return rval;
}

//! Restore parent/child links between GEOM_TOPO mesh sets
ErrorCode GeomTopoTool::restore_topology()
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
      assert(MB_TAG_NOT_FOUND != result);
      if (MB_SUCCESS != result)
        continue;

      // compress to a range to remove duplicates
      tmp_parents.clear();
      std::copy(parents.begin(), parents.end(), range_inserter(tmp_parents));
      for (Range::iterator pit = tmp_parents.begin(); pit != tmp_parents.end(); ++pit) {
        result = mdbImpl->add_parent_child(*pit, *d_it);
        if (MB_SUCCESS != result)
          return result;
      }

      // store surface senses within regions, and edge senses within surfaces
      if (dim == 0)
        continue;
      const EntityHandle *conn3 = NULL, *conn2 = NULL;
      int len3 = 0, len2 = 0, err = 0, num = 0, sense = 0, offset = 0;
      for (size_t i = 0; i < parents.size(); ++i) {
        result = mdbImpl->get_connectivity(dp1ents[i], conn3, len3, true);
        if (MB_SUCCESS != result)
          return result;
        result = mdbImpl->get_connectivity(dents.front(), conn2, len2, true);
        if (MB_SUCCESS != result)
          return result;
        assert(len2 <= 4);
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
    if (MB_SUCCESS != result)
      return result;

  } // dim

  return result;
}

ErrorCode GeomTopoTool::separate_by_dimension(const Range &geom_sets)
{
  ErrorCode result;

  if (0 == geomTag) {

    result = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag);
    if (MB_SUCCESS != result)
      return result;
  }

  // get the data for those tags
  std::vector<int> tag_vals(geom_sets.size());
  result = mdbImpl->tag_get_data(geomTag, geom_sets, &tag_vals[0]);
  if (MB_SUCCESS != result)
    return result;

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
    if (MB_SUCCESS != result)
      return result;
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
    if (MB_SUCCESS != result)
      return result;

    // make the new range
    temp_verts = new (std::nothrow) Range();
    assert(NULL != temp_verts);

    // get all the verts of those elements; use get_adjacencies 'cuz it handles ranges better
    result = mdbImpl->get_adjacencies(temp_elems, 0, false, *temp_verts,
        Interface::UNION);
    if (MB_SUCCESS != result) {
      delete temp_verts;
      return result;
    }

    // store this range as a tag on the entity
    result = mdbImpl->tag_set_data(verts_tag, &(*it), 1, &temp_verts);
    if (MB_SUCCESS != result) {
      delete temp_verts;
      return result;
    }

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
    return MB_FAILURE;// not geometry entities
  if (wrtdim - edim != 1)
    return MB_FAILURE; // dimension mismatch
  if (sense < -1 || sense > 1)
    return MB_FAILURE; // invalid sense

  ErrorCode rval;

  if (1 == edim) {
    // this case is about setting the sense of an edge in a face
    // it could be -1, 0 (rare, non manifold), or 1
    rval = check_edge_sense_tags(true);
    if (rval!=MB_SUCCESS)
      return rval;
    std::vector<EntityHandle> higher_ents;
    std::vector<int> senses;
    rval = get_senses(entity, higher_ents, senses);// the tags should be defined here
    // if there are no higher_ents, we are fine, we will just set them
    // if wrt_entity is not among higher_ents, we will add it to the list
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
    if (MB_SUCCESS != rval)
      return rval;

    dum_ptr = &senses[0];
    dum_size = higher_ents.size();
    rval = mdbImpl->tag_set_by_ptr(senseNSensesTag, &entity, 1, &dum_ptr,
        &dum_size);
    if (MB_SUCCESS != rval)
      return rval;
  } else {
    // this case is about a face in the volume
    // there could be only 2 volumes

    rval = check_face_sense_tag(true);
    if (rval!=MB_SUCCESS)
      return rval;

    EntityHandle sense_data[2] = { 0, 0 };
    rval = mdbImpl->tag_get_data(sense2Tag, &entity, 1, sense_data);
    if (MB_TAG_NOT_FOUND != rval && MB_SUCCESS != rval)
      return rval;

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
    return MB_FAILURE;// not geometry entities
  if (wrtdim - edim != 1)
    return MB_FAILURE; // dimension mismatch
  ErrorCode rval;

  if (1 == edim) {
    // edge in face
    rval = check_edge_sense_tags(false);
    if (rval!=MB_SUCCESS)
      return rval;
    std::vector<EntityHandle> faces;
    std::vector<int> senses;
    rval = get_senses(entity, faces, senses);// the tags should be defined here

    if (rval != MB_SUCCESS)
      return rval;

    std::vector<EntityHandle>::iterator it = std::find(faces.begin(),
        faces.end(), wrt_entity);
    if (it == faces.end())
      return MB_ENTITY_NOT_FOUND;
    unsigned int index = it - faces.begin();
    sense = senses[index];
  } else {
    // face in volume
    rval = check_face_sense_tag(false);
    if (rval!=MB_SUCCESS)
      return rval;
    EntityHandle sense_data[2] = { 0, 0 };
    rval = mdbImpl->tag_get_data(sense2Tag, &entity, 1, sense_data);
    if (MB_TAG_NOT_FOUND != rval && MB_SUCCESS != rval)
      return rval;
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

ErrorCode GeomTopoTool::get_senses(EntityHandle entity,
    std::vector<EntityHandle> &wrt_entities, std::vector<int> &senses)
{
  //
  // the question here is: the wrt_entities is supplied or not?
  // I assume not, we will obtain it !!
  int edim = dimension(entity);

  if (-1 == edim)
    return MB_FAILURE;// not geometry entity

  ErrorCode rval;
  wrt_entities.clear();
  senses.clear();

  if (1 == edim)// edge
  {
    rval = check_edge_sense_tags(false);
    if (rval!=MB_SUCCESS)
      return rval;
    const void *dum_ptr;
    int num_ents;
    rval = mdbImpl->tag_get_by_ptr(senseNEntsTag, &entity, 1, &dum_ptr, &num_ents);
    if (MB_SUCCESS != rval)
      return rval;

    const EntityHandle *ents_data = static_cast<const EntityHandle*> (dum_ptr);
    std::copy(ents_data, ents_data + num_ents, std::back_inserter(wrt_entities));

    rval = mdbImpl->tag_get_by_ptr(senseNSensesTag, &entity, 1, &dum_ptr,
        &num_ents);
    if (MB_SUCCESS != rval)
      return rval;

    const int *senses_data = static_cast<const int*> (dum_ptr);
    std::copy(senses_data, senses_data + num_ents, std::back_inserter(senses));

  } else // face in volume, edim == 2
  {
    rval = check_face_sense_tag(false);
    if (rval!=MB_SUCCESS)
      return rval;
    EntityHandle sense_data[2] = { 0, 0 };
    rval = mdbImpl->tag_get_data(sense2Tag, &entity, 1, sense_data);
    if (MB_SUCCESS != rval)
      return rval;
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
    if (MB_SUCCESS != rval)
      return rval;
  }

  return MB_SUCCESS;
}

// move the sense tag existence creation in private methods
// verify sense face tag
ErrorCode GeomTopoTool::check_face_sense_tag(bool create)
{
  ErrorCode rval;
  unsigned flags = create ? MB_TAG_SPARSE|MB_TAG_CREAT : MB_TAG_SPARSE;
  if (!sense2Tag) {
    EntityHandle def_val[2] = {0, 0};
    rval = mdbImpl->tag_get_handle(GEOM_SENSE_2_TAG_NAME, 2,
        MB_TYPE_HANDLE, sense2Tag, flags, def_val);
    if (MB_SUCCESS != rval)
      return rval;
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
    if (MB_SUCCESS != rval)
      return rval;
    rval = mdbImpl->tag_get_handle(GEOM_SENSE_N_SENSES_TAG_NAME,
                                   0, MB_TYPE_INTEGER, senseNSensesTag, flags);
    if (MB_SUCCESS != rval)
      return rval;
  }
  return MB_SUCCESS;
}

ErrorCode  GeomTopoTool::add_geo_set(EntityHandle set, int dim, int gid)
{
  if (dim <0 || dim > 4)
    return MB_FAILURE;
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
    if (MB_SUCCESS != result)
      return result;
  }

  if (0 == gidTag) {
    result = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gidTag);
    if (MB_SUCCESS != result)
      return result;
  }

  // make sure the added set has the geom tag properly set
  result = mdbImpl->tag_set_data(geomTag, &set, 1, &dim);
  if (MB_SUCCESS != result)
      return result;
  geomRanges[dim].insert(set);
  // not only that, but also add it to the root model set
  if (modelSet)
  {
    result = mdbImpl->add_entities(modelSet, &set, 1);
    if (MB_SUCCESS != result)
      return result;
  }

  // set the global ID value
  // if passed 0, just increase the max id for the dimension
  if (0 == gid)
  {
    gid = ++maxGlobalId[dim];
  }
  result = mdbImpl->tag_set_data(gidTag, &set, 1, &gid);
  if (MB_SUCCESS != result)
      return result;
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

  if (MB_SUCCESS != rval)
    return rval;
    // mb

  EntityHandle face =  surface;
  if (!surface)// in the case it is root set, create another set
  {
    rval = mdbImpl->create_meshset(MESHSET_SET, face);
    if (MB_SUCCESS != rval)
    return rval;
  }
  // set the geo tag
  rval = add_geo_set(face, 2);
  if (MB_SUCCESS != rval)
    return rval;

  // this will be our output set, will contain all our new geo sets
  rval = mdbImpl->create_meshset(MESHSET_SET, output);
  if (MB_SUCCESS != rval)
    return rval;

  // add first geo set (face) to the output set
  rval = mdbImpl->add_entities(output, &face, 1);
  if (MB_SUCCESS != rval)
    return rval;
  // how many edges do we need to create?
  // depends on how many loops we have
  // also, we should avoid non-manifold topology
  if (!surface) {// in this case, surface is root, so we have to add entities
    rval = mdbImpl->add_entities(face, surface_ents);
    if (MB_SUCCESS != rval)
      return rval;
  }


  Skinner tool(mdbImpl);
  rval = tool.find_skin(0, surface_ents, 1, edge_ents);
  if (MB_SUCCESS != rval)
    return rval;
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
    if (MB_SUCCESS != rval)
      return rval;
    if (tris.size()!=1 )
      return MB_FAILURE; // not on boundary
    int side_n, sense, offset;
    rval = mdbImpl->side_number(tris[0], current_edge, side_n, sense, offset);
    if (MB_SUCCESS != rval)
      return rval;

    const EntityHandle * conn2;
    int nnodes2;
    rval = mdbImpl-> get_connectivity(current_edge, conn2, nnodes2);
    if (MB_SUCCESS != rval)
      return rval;
    if( nnodes2!=2 )
      return MB_FAILURE;
    EntityHandle start_node = conn2[0];
    EntityHandle next_node = conn2[1];
    if (sense == -1)
    {
      // revert the edge, and start well
      EntityHandle nn2[2]={conn2[1], conn2[0]};
      rval = mdbImpl-> set_connectivity(current_edge, nn2, 2);
      if (MB_SUCCESS != rval)
        return rval;
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
      if (MB_SUCCESS != rval)
        return rval;
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
        // cannot complete the loop
        return MB_FAILURE;
      }
      // see if the orientation is good; if not, revert it

      current_edge = good_edges[0];
      rval = mdbImpl-> get_connectivity(current_edge, conn2, nnodes2);
      if (MB_SUCCESS != rval )
        return rval;
      if( nnodes2!=2)
        return MB_FAILURE;

      if (conn2[0] != next_node)
      {
        if (conn2[1]!=next_node)
        {
          // the edge is not connected then to current edge
          // bail out
          std::cout<< "edge " << mdbImpl->id_from_handle (current_edge) << " not connected to node "<<
              next_node << "\n";
          return MB_FAILURE;
        }
        if (debugFlag)
        {
         std::cout << " revert edge " << mdbImpl->id_from_handle (current_edge) << "\n";
         std::cout << mdbImpl->list_entity(current_edge);
        }
        // orientation should be reversed
        EntityHandle nn2[2]={conn2[1], conn2[0]};
        rval = mdbImpl-> set_connectivity(current_edge, nn2, 2);
        if (MB_SUCCESS != rval)
          return rval;

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
    if (MB_SUCCESS != rval)
      return rval;

    rval = add_geo_set(edge, 1);
    if (MB_SUCCESS != rval)
      return rval;
    // add the mesh edges:
    // add loops edges to the edge set
    rval = mdbImpl->add_entities(edge, &edges_loop[0], edges_loop.size());//
    if (MB_SUCCESS != rval)
      return rval;
    // create a vertex set
    EntityHandle vertex;
    rval = mdbImpl->create_meshset(MESHSET_SET, vertex);
    if (MB_SUCCESS != rval)
      return rval;
    rval = add_geo_set(vertex, 0);
    if (MB_SUCCESS != rval)
      return rval;
    // add one node to the vertex set

    rval = mdbImpl->add_entities(vertex, &start_node, 1);//
    if (MB_SUCCESS != rval)
      return rval;

    rval = mdbImpl ->add_parent_child( face, edge);
    if (MB_SUCCESS != rval)
      return rval;
    rval = mdbImpl ->add_parent_child( edge, vertex);
    if (MB_SUCCESS != rval)
      return rval;

    // the sense of the edge in face is for sure positive (forward)
    rval = set_sense(edge, face, 1);//
    if (MB_SUCCESS != rval)
      return rval;
    // also add our sets to the output set, to be sure to be exported

    rval = mdbImpl->add_entities(output, &edge, 1);
    if (MB_SUCCESS != rval)
      return rval;
    rval = mdbImpl->add_entities(output, &vertex, 1);
    if (MB_SUCCESS != rval)
      return rval;

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
  if (MB_SUCCESS!=rval)
    return rval;
  if (0 == geomTag) {
    rval = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geomTag);
    if (MB_SUCCESS != rval)
      return rval;
  }
  if (0 == gidTag) {
    rval = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1,MB_TYPE_INTEGER, gidTag);
    if (MB_SUCCESS != rval)
      return rval;
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
      if (MB_SUCCESS != rval)
        return rval;
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
      if (MB_SUCCESS!=rval)
        return rval;
      relate[set] = newSet;
      rval = mdbImpl->add_entities(rootModelSet, &newSet, 1);
      if (MB_SUCCESS!=rval)
        return rval;
      // make it a geo set, and give also global id in order
      rval = mdbImpl->tag_set_data(geomTag, &newSet, 1, &dim);
      if (MB_SUCCESS!=rval)
        return rval;
      gid++;// increment global id, everything starts with 1 in the new model!
      rval = mdbImpl->tag_set_data(gidTag, &newSet, 1, &gid);
      if (MB_SUCCESS!=rval)
        return rval;
      if (dim==1)
      {
        // the entities are ordered, we need to retrieve them ordered, and set them ordered
        std::vector<EntityHandle> mesh_edges;
        rval = mdbImpl->get_entities_by_handle(set, mesh_edges);
        if (MB_SUCCESS!=rval)
          return rval;
        rval = mdbImpl->add_entities(newSet, &(mesh_edges[0]), (int)mesh_edges.size());
        if (MB_SUCCESS!=rval)
          return rval;
      }
      else
      {
        Range ents;
        rval = mdbImpl->get_entities_by_handle(set, ents);
        if (MB_SUCCESS!=rval)
          return rval;
        rval = mdbImpl->add_entities(newSet, ents);
        if (MB_SUCCESS!=rval)
          return rval;
      }
      //set parent/child relations if dim>=1
      if (dim>=1)
      {
        Range children;
        // the children of geo sets are only g sets
        rval = mdbImpl->get_child_meshsets(set, children); // num_hops = 1 by default
        if (MB_SUCCESS!=rval)
           return rval;
        for (Range::iterator it2=children.begin(); it2!=children.end(); ++it2)
        {
          EntityHandle newChildSet = relate[*it2];
          rval = mdbImpl->add_parent_child(newSet, newChildSet);
          if (MB_SUCCESS!=rval)
            return rval;
        }
      }

    }
  }

  duplicate = new GeomTopoTool(mdbImpl, true, rootModelSet); // will retrieve the
  // sets and put them in ranges

  // this is the lazy way to it:
  // newgtt->restore_topology(); // will reset the sense entities, and with this, the model
  // represented by this new gtt will be complete
  // set senses by peeking at the old model
  // make sure we have the sense tags defined
  rval = check_face_sense_tag(true);
  if (rval!=MB_SUCCESS)
    return rval;
  rval = check_edge_sense_tags(true);
  if (rval!=MB_SUCCESS)
    return rval;

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
      if (MB_SUCCESS!=rval)
         return rval;
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
      if (MB_SUCCESS!=rval)
        return rval;
    }
  }
  // if the original root model set for this model is 0 (root set), then create
  // a new set and put all the old sets in the new model set
  // in this way, the existing gtt remains valid (otherwise, the modelSet would contain all the
  // gsets, the old ones and the new ones; the root set contains everything)
  if (modelSet==0)
  {
    rval = mdbImpl->create_meshset(MESHSET_SET, modelSet);
    if (MB_SUCCESS != rval)
      return rval;
    // add to this new set all previous sets (which are still in ranges)
    for (int dim=0; dim<5; dim++)
    {
      rval = mdbImpl->add_entities(modelSet, geomRanges[dim]);
      if (MB_SUCCESS != rval)
        return rval;
    }

  }
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
} // namespace moab


