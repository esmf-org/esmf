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

#ifdef __MFC_VER
#pragma warning(disable:4786)
#endif

#include "moab/Skinner.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include <vector>
#include <set>
#include <algorithm>
#include <math.h>
#include <assert.h>
#include <iostream>
#include "moab/Util.hpp"
#include "Internals.hpp"
#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#include "AEntityFactory.hpp"
#include "moab/ScdInterface.hpp"

#ifdef M_PI
#  define SKINNER_PI M_PI
#else
#  define SKINNER_PI 3.1415926535897932384626
#endif

namespace moab {

Skinner::~Skinner()
{
  // delete the adjacency tag
}


ErrorCode Skinner::initialize()
{
  // go through and mark all the target dimension entities
  // that already exist as not deleteable
  // also get the connectivity tags for each type
  // also populate adjacency information
  EntityType type;
  DimensionPair target_ent_types = CN::TypeDimensionMap[mTargetDim];

  void* null_ptr = NULL;

  ErrorCode result = thisMB->tag_get_handle("skinner adj", sizeof(void*), MB_TYPE_OPAQUE, mAdjTag, 
                                            MB_TAG_DENSE|MB_TAG_CREAT, &null_ptr);
 MB_CHK_ERR(result);

  if(mDeletableMBTag == 0) {
    result = thisMB->tag_get_handle("skinner deletable", 1, MB_TYPE_BIT, mDeletableMBTag, MB_TAG_BIT|MB_TAG_CREAT);
   MB_CHK_ERR(result);
  }
  
  Range entities;

    // go through each type at this dimension 
  for(type = target_ent_types.first; type <= target_ent_types.second; ++type)
  {
      // get the entities of this type in the MB
    thisMB->get_entities_by_type(0, type, entities);

      // go through each entity of this type in the MB
      // and set its deletable tag to NO
    Range::iterator iter, end_iter;
    end_iter = entities.end();
    for(iter = entities.begin(); iter != end_iter; ++iter)
    {
      unsigned char bit = 0x1;
      result = thisMB->tag_set_data(mDeletableMBTag, &(*iter), 1, &bit);
      assert(MB_SUCCESS == result);
        // add adjacency information too
      if (TYPE_FROM_HANDLE(*iter) != MBVERTEX)
        add_adjacency(*iter);
    }
  }

  return MB_SUCCESS;
}

ErrorCode Skinner::deinitialize()
{
  ErrorCode result;
  if (0 != mDeletableMBTag) {
    result = thisMB->tag_delete( mDeletableMBTag);
    mDeletableMBTag = 0;
    MB_CHK_ERR(result);
  }

  // remove the adjacency tag
  std::vector< std::vector<EntityHandle>* > adj_arr;
  std::vector< std::vector<EntityHandle>* >::iterator i;
  if (0 != mAdjTag) {
    for (EntityType t = MBVERTEX; t != MBMAXTYPE; ++t) {
      Range entities;
      result = thisMB->get_entities_by_type_and_tag( 0, t, &mAdjTag, 0, 1, entities ); MB_CHK_ERR(result);
      adj_arr.resize( entities.size() );
      result = thisMB->tag_get_data( mAdjTag, entities, &adj_arr[0] ); MB_CHK_ERR(result);
      for (i = adj_arr.begin(); i != adj_arr.end(); ++i)
        delete *i;
    }
  
    result = thisMB->tag_delete(mAdjTag);
    mAdjTag = 0;
    MB_CHK_ERR(result);
  }

  return MB_SUCCESS;
}


ErrorCode Skinner::add_adjacency(EntityHandle entity)
{
  std::vector<EntityHandle> *adj = NULL;
  const EntityHandle *nodes;
  int num_nodes;
  ErrorCode result = thisMB->get_connectivity(entity, nodes, num_nodes, true); MB_CHK_ERR(result);
  const EntityHandle *iter =
    std::min_element(nodes, nodes+num_nodes);

  if(iter == nodes+num_nodes)
    return MB_SUCCESS;

  // add this entity to the node
  if(thisMB->tag_get_data(mAdjTag, iter, 1, &adj) == MB_SUCCESS && adj != NULL)
  {
    adj->push_back(entity);    
  }
  // create a new vector and add it
  else
  {
    adj = new std::vector<EntityHandle>;
    adj->push_back(entity);
    result = thisMB->tag_set_data(mAdjTag, iter, 1, &adj); MB_CHK_ERR(result);
  }

  return MB_SUCCESS;
}

void Skinner::add_adjacency(EntityHandle entity, 
                               const EntityHandle *nodes,
                               const int num_nodes)
{
  std::vector<EntityHandle> *adj = NULL;
  const EntityHandle *iter = 
    std::min_element(nodes, nodes+num_nodes);

  if(iter == nodes+num_nodes)
    return;

    // should not be setting adjacency lists in ho-nodes
  assert(TYPE_FROM_HANDLE(entity) == MBPOLYGON || 
         num_nodes == CN::VerticesPerEntity(TYPE_FROM_HANDLE(entity)));

  // add this entity to the node
  if(thisMB->tag_get_data(mAdjTag, iter, 1, &adj) == MB_SUCCESS && adj != NULL)
  {
    adj->push_back(entity);    
  }
  // create a new vector and add it
  else
  {
    adj = new std::vector<EntityHandle>;
    adj->push_back(entity);
    thisMB->tag_set_data(mAdjTag, iter, 1, &adj);
  }
}

ErrorCode Skinner::find_geometric_skin(const EntityHandle meshset, Range &forward_target_entities)
{
    // attempts to find whole model skin, using geom topo sets first then
    // normal find_skin function
  bool debug = true;

    // look for geom topo sets
  Tag geom_tag;
  ErrorCode result = thisMB->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, 
                                        geom_tag, MB_TAG_SPARSE|MB_TAG_CREAT);

  if (MB_SUCCESS != result)
    return result;
  
    // get face sets (dimension = 2)
  Range face_sets;
  int two = 2;
  const void *two_ptr = &two;
  result = thisMB->get_entities_by_type_and_tag(meshset, MBENTITYSET, &geom_tag, &two_ptr, 1,
                                                 face_sets);

  Range::iterator it;
  if (MB_SUCCESS != result)
    return result;
  else if (face_sets.empty())
    return MB_ENTITY_NOT_FOUND;

    // ok, we have face sets; use those to determine skin
  Range skin_sets;
  if (debug) std::cout << "Found " << face_sets.size() << " face sets total..." << std::endl;
  
  for (it = face_sets.begin(); it != face_sets.end(); ++it) {
    int num_parents;
    result = thisMB->num_parent_meshsets(*it, &num_parents);
    if (MB_SUCCESS != result)
      return result;
    else if (num_parents == 1)
      skin_sets.insert(*it);
  }

  if (debug) std::cout << "Found " << skin_sets.size() << " 1-parent face sets..." << std::endl;

  if (skin_sets.empty())
    return MB_FAILURE;
      
    // ok, we have the shell; gather up the elements, putting them all in forward for now
  for (it = skin_sets.begin(); it != skin_sets.end(); ++it) {
    result = thisMB->get_entities_by_handle(*it, forward_target_entities, true);
    if (MB_SUCCESS != result)
      return result;
  }
        
  return result;
}

ErrorCode Skinner::find_skin( const EntityHandle meshset,
                              const Range& source_entities,
                              bool get_vertices,
                              Range& output_handles,
                              Range* output_reverse_handles,
                              bool create_vert_elem_adjs,
                              bool create_skin_elements, 
                              bool look_for_scd)
{
  if (source_entities.empty())
    return MB_SUCCESS;

  if (look_for_scd) {
    ErrorCode rval = find_skin_scd(source_entities, get_vertices, output_handles, create_skin_elements);
      // if it returns success, it's all scd, and we don't need to do anything more
    if (MB_SUCCESS == rval) return rval;
  }

  Core* this_core = dynamic_cast<Core*>(thisMB);
  if (this_core && create_vert_elem_adjs && 
      !this_core->a_entity_factory()->vert_elem_adjacencies())
    this_core->a_entity_factory()->create_vert_elem_adjacencies();
    
  return find_skin_vertices( meshset,
                             source_entities,
                             get_vertices ? &output_handles : 0,
                             get_vertices ? 0 : &output_handles,
                             output_reverse_handles,
                             create_skin_elements );
  
}

ErrorCode Skinner::find_skin_scd(const Range& source_entities,
                                 bool get_vertices,
                                 Range& output_handles,
                                 bool create_skin_elements) 
{
    // get the scd interface and check if it's been initialized
  ScdInterface *scdi = NULL;
  ErrorCode rval = thisMB->query_interface(scdi);
  if (!scdi) return MB_FAILURE;
  
    // ok, there's scd mesh; see if the entities passed in are all in a scd box
    // a box needs to be wholly included in entities for this to work
  std::vector<ScdBox*> boxes, myboxes;
  Range myrange;
  rval = scdi->find_boxes(boxes);
  if (MB_SUCCESS != rval) return rval;
  for (std::vector<ScdBox*>::iterator bit = boxes.begin(); bit != boxes.end(); ++bit) {
    Range belems((*bit)->start_element(), (*bit)->start_element() + (*bit)->num_elements()-1);
    if (source_entities.contains(belems)) {
      myboxes.push_back(*bit);
      myrange.merge(belems);
    }
  }
  if (myboxes.empty() || myrange.size() != source_entities.size()) return MB_FAILURE;

    // ok, we're all structured; get the skin for each box
  for (std::vector<ScdBox*>::iterator bit = boxes.begin(); bit != boxes.end(); ++bit) {
    rval = skin_box(*bit, get_vertices, output_handles, create_skin_elements);
    if (MB_SUCCESS != rval) return rval;
  }
  
  return MB_SUCCESS;
}

ErrorCode Skinner::skin_box(ScdBox *box, bool get_vertices, Range &output_handles, 
                            bool create_skin_elements) 
{
  HomCoord bmin = box->box_min(), bmax = box->box_max();

    // don't support 1d boxes
  if (bmin.j() == bmax.j() && bmin.k() == bmax.k()) return MB_FAILURE;
  
  int dim = (bmin.k() == bmax.k() ? 1 : 2);
  
  ErrorCode rval;
  EntityHandle ent;

    // i=min
  for (int k = bmin.k(); k < bmax.k(); k++) {
    for (int j = bmin.j(); j < bmax.j(); j++) {
      ent = 0;
      rval = box->get_adj_edge_or_face(dim, bmin.i(), j, k, 0, ent, create_skin_elements);
      if (MB_SUCCESS != rval) return rval;
      if (ent) output_handles.insert(ent);
    }
  }
    // i=max
  for (int k = bmin.k(); k < bmax.k(); k++) {
    for (int j = bmin.j(); j < bmax.j(); j++) {
      ent = 0;
      rval = box->get_adj_edge_or_face(dim, bmax.i(), j, k, 0, ent, create_skin_elements);
      if (MB_SUCCESS != rval) return rval;
      if (ent) output_handles.insert(ent);
    }
  }
    // j=min
  for (int k = bmin.k(); k < bmax.k(); k++) {
    for (int i = bmin.i(); i < bmax.i(); i++) {
      ent = 0;
      rval = box->get_adj_edge_or_face(dim, i, bmin.j(), k, 1, ent, create_skin_elements);
      if (MB_SUCCESS != rval) return rval;
      if (ent) output_handles.insert(ent);
    }
  }
    // j=max
  for (int k = bmin.k(); k < bmax.k(); k++) {
    for (int i = bmin.i(); i < bmax.i(); i++) {
      ent = 0;
      rval = box->get_adj_edge_or_face(dim, i, bmax.j(), k, 1, ent, create_skin_elements);
      if (MB_SUCCESS != rval) return rval;
      if (ent) output_handles.insert(ent);
    }
  }
    // k=min
  for (int j = bmin.j(); j < bmax.j(); j++) {
    for (int i = bmin.i(); i < bmax.i(); i++) {
      ent = 0;
      rval = box->get_adj_edge_or_face(dim, i, j, bmin.k(), 2, ent, create_skin_elements);
      if (MB_SUCCESS != rval) return rval;
      if (ent) output_handles.insert(ent);
    }
  }
    // k=max
  for (int j = bmin.j(); j < bmax.j(); j++) {
    for (int i = bmin.i(); i < bmax.i(); i++) {
      ent = 0;
      rval = box->get_adj_edge_or_face(dim, i, j, bmax.k(), 2, ent, create_skin_elements);
      if (MB_SUCCESS != rval) return rval;
      if (ent) output_handles.insert(ent);
    }
  }

  if (get_vertices) {
    Range verts;
    rval = thisMB->get_adjacencies(output_handles, 0, true, verts, Interface::UNION);
    if (MB_SUCCESS != rval) return rval;
    output_handles.merge(verts);
  }
  
  return MB_SUCCESS;
}

ErrorCode Skinner::find_skin_noadj(const Range &source_entities,
                                 Range &forward_target_entities,
                                 Range &reverse_target_entities/*,
                                 bool create_vert_elem_adjs*/)
{
  if(source_entities.empty())
    return MB_FAILURE;
  
  // get our working dimensions
  EntityType type = thisMB->type_from_handle(*(source_entities.begin()));
  const int source_dim = CN::Dimension(type);
  mTargetDim = source_dim - 1;

  // make sure we can handle the working dimensions
  if(mTargetDim < 0 || source_dim > 3)
    return MB_FAILURE;

  initialize();

  Range::const_iterator iter, end_iter;
  end_iter = source_entities.end();
  const EntityHandle *conn;
  EntityHandle match;

  direction direct;
  ErrorCode result;
    // assume we'll never have more than 32 vertices on a facet (checked
    // with assert later)
  EntityHandle sub_conn[32];
  std::vector<EntityHandle> tmp_conn_vec;
  int num_nodes, num_sub_nodes, num_sides;
  int sub_indices[32];// Also, assume that no polygon has more than 32 nodes
  // we could increase that, but we will not display it right in visit moab h5m , anyway
  EntityType sub_type;

  // for each source entity
  for(iter = source_entities.begin(); iter != end_iter; ++iter)
  {
    // get the connectivity of this entity
    int actual_num_nodes_polygon=0;
    result = thisMB->get_connectivity(*iter, conn, num_nodes, false, &tmp_conn_vec);
    if (MB_SUCCESS != result)
      return result;
    
    type = thisMB->type_from_handle(*iter);
    Range::iterator seek_iter;
    
    // treat separately polygons (also, polyhedra will need special handling)
    if (MBPOLYGON == type)
    {
      // treat padded polygons, if existing; count backwards, see how many of the last nodes are repeated
      // assume connectivity is fine, otherwise we could be in trouble
      actual_num_nodes_polygon = num_nodes;
      while (actual_num_nodes_polygon >= 3 &&
          conn[actual_num_nodes_polygon-1]==conn[actual_num_nodes_polygon-2])
        actual_num_nodes_polygon--;
      num_sides = actual_num_nodes_polygon;
      sub_type = MBEDGE;
      num_sub_nodes = 2;
    }
    else// get connectivity of each n-1 dimension entity
      num_sides = CN::NumSubEntities( type, mTargetDim );
    for(int i=0; i<num_sides; i++)
    {
      if(MBPOLYGON==type)
      {
        sub_conn[0] = conn[i];
        sub_conn[1] = conn[i+1];
        if (i+1 == actual_num_nodes_polygon)
          sub_conn[1]=conn[0];
      }
      else
      {
        CN::SubEntityNodeIndices( type, num_nodes, mTargetDim, i, sub_type, num_sub_nodes, sub_indices );
        assert((size_t)num_sub_nodes <= sizeof(sub_indices)/sizeof(sub_indices[0]));
        for(int j=0; j<num_sub_nodes; j++)
          sub_conn[j] = conn[sub_indices[j]];
      }
        
        // see if we can match this connectivity with
        // an existing entity
      find_match( sub_type, sub_conn, num_sub_nodes, match, direct );

        // if there is no match, create a new entity
      if(match == 0)
      {
        EntityHandle tmphndl=0;
        int indices[MAX_SUB_ENTITY_VERTICES];
        EntityType new_type;
        int num_new_nodes;
        if(MBPOLYGON==type)
        {
          new_type = MBEDGE;
          num_new_nodes = 2;
        }
        else
        {
          CN::SubEntityNodeIndices( type, num_nodes, mTargetDim, i, new_type, num_new_nodes, indices );
          for(int j=0; j<num_new_nodes; j++)
            sub_conn[j] = conn[indices[j]];
        }
        result = thisMB->create_element(new_type, sub_conn, num_new_nodes,
                                        tmphndl);
        assert(MB_SUCCESS == result);
        add_adjacency(tmphndl, sub_conn, CN::VerticesPerEntity(new_type));
        forward_target_entities.insert(tmphndl);
      }
        // if there is a match, delete the matching entity
        // if we can.
      else
      {
        if ( (seek_iter = forward_target_entities.find(match)) != forward_target_entities.end())
        {
          forward_target_entities.erase(seek_iter);
          remove_adjacency(match);
          if(/*!use_adjs &&*/ entity_deletable(match))
          {
            result = thisMB->delete_entities(&match, 1);
            assert(MB_SUCCESS == result);
          }
        }
        else if ( (seek_iter = reverse_target_entities.find(match)) != reverse_target_entities.end())
        {
          reverse_target_entities.erase(seek_iter);
          remove_adjacency(match);
          if(/*!use_adjs &&*/ entity_deletable(match))
          {
            result = thisMB->delete_entities(&match, 1);
            assert(MB_SUCCESS == result);
          }
        }
        else
        {
          if(direct == FORWARD)
          {
            forward_target_entities.insert(match);
          }
          else
          {
            reverse_target_entities.insert(match);
          }
        }
      }
    }
  }

  deinitialize();

  return MB_SUCCESS;
}


void Skinner::find_match( EntityType type, 
                             const EntityHandle *conn,
                             const int num_nodes,
                             EntityHandle& match,
                             Skinner::direction &direct)
{
  match = 0;

  if (type == MBVERTEX) {
    match = *conn;
    direct = FORWARD;
    return;
  }

  const EntityHandle *iter = std::min_element(conn, conn+num_nodes);

  std::vector<EntityHandle> *adj = NULL;

  ErrorCode result = thisMB->tag_get_data(mAdjTag, iter, 1, &adj);
  if(result == MB_FAILURE || adj == NULL)
  {
    return;
  }

  std::vector<EntityHandle>::iterator jter, end_jter;
  end_jter = adj->end();

  const EntityHandle *tmp;
  int num_verts;

  for(jter = adj->begin(); jter != end_jter; ++jter)
  {
    EntityType tmp_type;
    tmp_type = thisMB->type_from_handle(*jter);

    if( type != tmp_type )
      continue;

    result = thisMB->get_connectivity(*jter, tmp, num_verts, false);
    assert(MB_SUCCESS == result && num_verts >= CN::VerticesPerEntity(type));
    // FIXME: connectivity_match appears to work only for linear elements,
    //        so ignore higher-order nodes.
    if(connectivity_match(conn, tmp, CN::VerticesPerEntity(type), direct))
    {
      match = *jter;
      break;
    }        
  }
}

bool Skinner::connectivity_match( const EntityHandle *conn1,
                                     const EntityHandle *conn2,
                                     const int num_verts,
                                     Skinner::direction &direct)
{
  const EntityHandle *iter =
    std::find(conn2, conn2+num_verts, conn1[0]);
  if(iter == conn2+num_verts)
    return false;

  bool they_match = true;

  int i;
  unsigned int j = iter - conn2;
    
  // first compare forward
  for(i = 1; i<num_verts; ++i)
  {
    if(conn1[i] != conn2[(j+i)%num_verts])
    {
      they_match = false;
      break;
    }
  }
  
  if(they_match == true)
  {
    // need to check for reversed edges here
    direct = (num_verts == 2 && j) ? REVERSE : FORWARD;
    return true;
  }
  
  they_match = true;
  
  // then compare reverse
  j += num_verts;
  for(i = 1; i < num_verts; )
  {
    if(conn1[i] != conn2[(j-i)%num_verts])
    {
      they_match = false;
      break;
    }
    ++i;
  }
  if (they_match)
  {
    direct = REVERSE;
  }
  return they_match;
}

  
ErrorCode Skinner::remove_adjacency(EntityHandle entity)
{
  std::vector<EntityHandle> nodes, *adj = NULL;
  ErrorCode result = thisMB->get_connectivity(&entity, 1, nodes); MB_CHK_ERR(result);
  std::vector<EntityHandle>::iterator iter = 
    std::min_element(nodes.begin(), nodes.end());

  if(iter == nodes.end())
    return MB_FAILURE;

  // remove this entity from the node
  if(thisMB->tag_get_data(mAdjTag, &(*iter), 1, &adj) == MB_SUCCESS && adj != NULL)
  {
    iter = std::find(adj->begin(), adj->end(), entity);
    if(iter != adj->end())
      adj->erase(iter);
  }

  return result;
}

bool Skinner::entity_deletable(EntityHandle entity)
{
  unsigned char deletable=0;
  ErrorCode result = thisMB->tag_get_data(mDeletableMBTag, &entity, 1, &deletable);
  assert(MB_SUCCESS == result);
  if(MB_SUCCESS == result && deletable == 1)
    return false;
  return true;
}

ErrorCode Skinner::classify_2d_boundary( const Range &boundary,
                                               const Range &bar_elements,
                                               EntityHandle boundary_edges,
                                               EntityHandle inferred_edges,
                                               EntityHandle non_manifold_edges,
                                               EntityHandle other_edges,
                                               int &number_boundary_nodes)
{
  Range bedges, iedges, nmedges, oedges;
  ErrorCode result = classify_2d_boundary(boundary, bar_elements,
                                             bedges, iedges, nmedges, oedges,
                                             number_boundary_nodes);MB_CHK_ERR(result);
  
    // now set the input meshsets to the output ranges
  result = thisMB->clear_meshset(&boundary_edges, 1);MB_CHK_ERR(result);
  result = thisMB->add_entities(boundary_edges, bedges); MB_CHK_ERR(result);

  result = thisMB->clear_meshset(&inferred_edges, 1);MB_CHK_ERR(result);
  result = thisMB->add_entities(inferred_edges, iedges);MB_CHK_ERR(result);

  result = thisMB->clear_meshset(&non_manifold_edges, 1); MB_CHK_ERR(result);
  result = thisMB->add_entities(non_manifold_edges, nmedges);MB_CHK_ERR(result);

  result = thisMB->clear_meshset(&other_edges, 1);MB_CHK_ERR(result);
  result = thisMB->add_entities(other_edges, oedges);MB_CHK_ERR(result);

  return MB_SUCCESS;
}

ErrorCode Skinner::classify_2d_boundary( const Range &boundary,
                                               const Range &bar_elements,
                                               Range &boundary_edges,
                                               Range &inferred_edges,
                                               Range &non_manifold_edges,
                                               Range &other_edges,
                                               int &number_boundary_nodes)
{

  // clear out the edge lists

  boundary_edges.clear();
  inferred_edges.clear();
  non_manifold_edges.clear();
  other_edges.clear();

  number_boundary_nodes = 0;

  // make sure we have something to work with
  if(boundary.empty())
  {
    return MB_FAILURE;
  }
  
  // get our working dimensions
  EntityType type = thisMB->type_from_handle(*(boundary.begin()));
  const int source_dim = CN::Dimension(type);

  // make sure we can handle the working dimensions
  if(source_dim != 2)
  {
    return MB_FAILURE;
  }
  mTargetDim = source_dim - 1;

  // initialize
  initialize();

  // additional initialization for this routine
  // define a tag for MBEDGE which counts the occurrences of the edge below
  // default should be 0 for existing edges, if any

  Tag count_tag;
  int default_count = 0;
  ErrorCode result = thisMB->tag_get_handle(0, 1, MB_TYPE_INTEGER,
                                        count_tag, MB_TAG_DENSE|MB_TAG_CREAT, &default_count); MB_CHK_ERR(result);

 
  Range::const_iterator iter, end_iter;
  end_iter = boundary.end();

  std::vector<EntityHandle> conn;
  EntityHandle sub_conn[2];
  EntityHandle match;

  Range edge_list;
  Range boundary_nodes;
  Skinner::direction direct;
  
  EntityType sub_type;
  int num_edge, num_sub_ent_vert;
  const short* edge_verts;
  
  // now, process each entity in the boundary

  for(iter = boundary.begin(); iter != end_iter; ++iter)
  {
    // get the connectivity of this entity
    conn.clear();
    result = thisMB->get_connectivity(&(*iter), 1, conn, false);
    assert(MB_SUCCESS == result);

    // add node handles to boundary_node range
    std::copy(conn.begin(), conn.begin()+CN::VerticesPerEntity(type), 
              range_inserter(boundary_nodes));

    type = thisMB->type_from_handle(*iter);
    
    // get connectivity of each n-1 dimension entity (edge in this case)
    const struct CN::ConnMap* conn_map = &(CN::mConnectivityMap[type][0]);
    num_edge = CN::NumSubEntities( type, 1 );
    for(int i=0; i<num_edge; i++)
    {
      edge_verts = CN::SubEntityVertexIndices( type, 1, i, sub_type, num_sub_ent_vert );
      assert( sub_type == MBEDGE && num_sub_ent_vert == 2 );
      sub_conn[0] = conn[edge_verts[0]];
      sub_conn[1] = conn[edge_verts[1]];
      int num_sub_nodes = conn_map->num_corners_per_sub_element[i];
      
      // see if we can match this connectivity with
      // an existing entity
      find_match( MBEDGE, sub_conn, num_sub_nodes, match, direct );
  
      // if there is no match, create a new entity
      if(match == 0)
      {
        EntityHandle tmphndl=0;
        int indices[MAX_SUB_ENTITY_VERTICES];
        EntityType new_type;
        int num_new_nodes;
        CN::SubEntityNodeIndices( type, conn.size(), 1, i, new_type, num_new_nodes, indices );
        for(int j=0; j<num_new_nodes; j++)
          sub_conn[j] = conn[indices[j]];
        
        result = thisMB->create_element(new_type, sub_conn,  
                                        num_new_nodes, tmphndl);
        assert(MB_SUCCESS == result);
        add_adjacency(tmphndl, sub_conn, num_sub_nodes);
        //target_entities.insert(tmphndl);
        edge_list.insert(tmphndl);
        int count;
        result = thisMB->tag_get_data(count_tag, &tmphndl, 1, &count);
        assert(MB_SUCCESS == result);
        count++;
        result = thisMB->tag_set_data(count_tag, &tmphndl, 1, &count);
        assert(MB_SUCCESS == result);

      }
      else
      {
        // We found a match, we must increment the count on the match
        int count;
        result = thisMB->tag_get_data(count_tag, &match, 1, &count);
        assert(MB_SUCCESS == result);
        count++;
        result = thisMB->tag_set_data(count_tag, &match, 1, &count);
        assert(MB_SUCCESS == result);

        // if the entity is not deletable, it was pre-existing in
        // the database.  We therefore may need to add it to the
        // edge_list.  Since it will not hurt the range, we add
        // whether it was added before or not
        if(!entity_deletable(match))
        {
          edge_list.insert(match);
        }
      }
    }
  }

  // Any bar elements in the model should be classified separately
  // If the element is in the skin edge_list, then it should be put in
  // the non-manifold edge list.  Edges not in the edge_list are stand-alone
  // bars, and we make them simply boundary elements

  if (!bar_elements.empty())
  {
    Range::iterator bar_iter;
    for(iter = bar_elements.begin(); iter != bar_elements.end(); ++iter)
    {
      EntityHandle handle = *iter;
      bar_iter = edge_list.find(handle);
      if (bar_iter != edge_list.end())
      {
        // it is in the list, erase it and put in non-manifold list
        edge_list.erase(bar_iter);
        non_manifold_edges.insert(handle);
      }
      else
      {
        // not in the edge list, make it a boundary edge
        boundary_edges.insert(handle);
      }
    }
  }

  // now all edges should be classified.  Go through the edge_list,
  // and put all in the appropriate lists

  Range::iterator edge_iter, edge_end_iter;
  edge_end_iter = edge_list.end();
  int count;
  for(edge_iter = edge_list.begin(); edge_iter != edge_end_iter; ++edge_iter)
  {
    // check the count_tag
    result = thisMB->tag_get_data(count_tag, &(*edge_iter), 1, &count);
    assert(MB_SUCCESS == result);
    if (count == 1)
    {
      boundary_edges.insert(*edge_iter);
   }
    else if (count == 2)
    {
      other_edges.insert(*edge_iter);
    }
    else
    {
      non_manifold_edges.insert(*edge_iter);
    }
  }

  // find the inferred edges from the other_edge_list

  double min_angle_degrees = 20.0;
  find_inferred_edges(const_cast<Range&> (boundary), other_edges, inferred_edges, min_angle_degrees);

  // we now want to remove the inferred_edges from the other_edges

  Range temp_range;
 
  std::set_difference(other_edges.begin(), other_edges.end(),
                      inferred_edges.begin(), inferred_edges.end(),
                      range_inserter(temp_range),
                      std::less<EntityHandle>() );

  other_edges = temp_range;

  // get rid of count tag and deinitialize

  result = thisMB->tag_delete(count_tag);
  assert(MB_SUCCESS == result);
  deinitialize();

  // set the node count
  number_boundary_nodes = boundary_nodes.size();

  return MB_SUCCESS;
} 

void Skinner::find_inferred_edges(Range &skin_boundary,
                                     Range &candidate_edges,
                                     Range &inferred_edges,
                                     double reference_angle_degrees)
{

  // mark all the entities in the skin boundary
  Tag mark_tag;
  ErrorCode result = thisMB->tag_get_handle(0, 1, MB_TYPE_BIT, mark_tag, MB_TAG_CREAT);
  assert(MB_SUCCESS == result);
  unsigned char bit = true;
  result = thisMB->tag_clear_data(mark_tag, skin_boundary, &bit );
  assert(MB_SUCCESS == result);

  // find the cosine of the reference angle

  double reference_cosine = cos(reference_angle_degrees*SKINNER_PI/180.0);
  
  // check all candidate edges for an angle greater than the minimum

  Range::iterator iter, end_iter = candidate_edges.end();
  std::vector<EntityHandle> adjacencies;
  std::vector<EntityHandle>::iterator adj_iter;
  EntityHandle face[2];

  for(iter = candidate_edges.begin(); iter != end_iter; ++iter)
  {

    // get the 2D elements connected to this edge
    adjacencies.clear();
    result = thisMB->get_adjacencies(&(*iter), 1, 2, false, adjacencies);
    if (MB_SUCCESS != result) 
      continue;

    // there should be exactly two, that is why the edge is classified as nonBoundary
    // and manifold

    int faces_found = 0;
    for(adj_iter = adjacencies.begin(); adj_iter != adjacencies.end() && faces_found < 2; ++adj_iter)
    {
      // we need to find two of these which are in the skin
      unsigned char is_marked = 0;
      result = thisMB->tag_get_data(mark_tag, &(*adj_iter), 1, &is_marked);
      assert(MB_SUCCESS == result);
      if(is_marked)
      {
        face[faces_found] = *adj_iter;
        faces_found++;
      } 
    }

//    assert(faces_found == 2 || faces_found == 0);
    if (2 != faces_found) 
      continue;

    // see if the two entities have a sufficient angle

    if ( has_larger_angle(face[0], face[1], reference_cosine) )
    {
       inferred_edges.insert(*iter);
    }
  }
  
  result = thisMB->tag_delete(mark_tag);
  assert(MB_SUCCESS == result);
}

bool Skinner::has_larger_angle(EntityHandle &entity1,
                                 EntityHandle &entity2,
                                 double reference_angle_cosine)
{
  // compare normals to get angle.  We assume that the surface quads
  // which we test here will be approximately planar

  double norm[2][3];
  Util::normal(thisMB, entity1, norm[0][0], norm[0][1], norm[0][2]);
  Util::normal(thisMB, entity2, norm[1][0], norm[1][1], norm[1][2]);

  double cosine = norm[0][0] * norm[1][0] + norm[0][1] * norm[1][1] + norm[0][2] * norm[1][2];

  if (cosine < reference_angle_cosine)
  {
    return true;
  }


  return false;
}

  // get skin entities of prescribed dimension
ErrorCode Skinner::find_skin(const EntityHandle this_set,
                             const Range &entities,
                                 int dim,
                                 Range &skin_entities,
                                 bool create_vert_elem_adjs,
                                 bool create_skin_elements)
{
  Range tmp_skin;
  ErrorCode result = find_skin(this_set, entities, (dim==0), tmp_skin, 0,
                                 create_vert_elem_adjs, create_skin_elements);
  if (MB_SUCCESS != result || tmp_skin.empty()) return result;
  
  if (tmp_skin.all_of_dimension(dim)) {
    if (skin_entities.empty())
      skin_entities.swap(tmp_skin);
    else
      skin_entities.merge(tmp_skin);
  }
  else {
    result = thisMB->get_adjacencies( tmp_skin, dim, create_skin_elements, skin_entities,
                                      Interface::UNION );MB_CHK_ERR(result);
    if (this_set)
      result = thisMB->add_entities(this_set, skin_entities);
  }
  
  return result;
}

ErrorCode Skinner::find_skin_vertices( const EntityHandle this_set,
                                       const Range& entities,
                                           Range* skin_verts,
                                           Range* skin_elems,
                                           Range* skin_rev_elems,
                                           bool create_skin_elems,
                                           bool corners_only )
{
  ErrorCode rval;
  if (entities.empty())
    return MB_SUCCESS;
  
  const int dim = CN::Dimension(TYPE_FROM_HANDLE(entities.front()));
  if (dim < 1 || dim > 3 || !entities.all_of_dimension(dim))
    return MB_TYPE_OUT_OF_RANGE;
  
    // are we skinning all entities
  size_t count = entities.size();
  int num_total;
  rval = thisMB->get_number_entities_by_dimension( this_set, dim, num_total );
  if (MB_SUCCESS != rval)
    return rval;
  bool all = (count == (size_t)num_total);
  
    // Create a bit tag for fast intersection with input entities range. 
    // If we're skinning all the entities in the mesh, we really don't
    // need the tag.  To save memory, just create it with a default value
    // of one and don't set it.  That way MOAB will return 1 for all 
    // entities.
  Tag tag;
  char bit = all ? 1 : 0;
  rval = thisMB->tag_get_handle( NULL, 1, MB_TYPE_BIT, tag, MB_TAG_CREAT, &bit );
  if (MB_SUCCESS != rval)
    return rval;
  
    // tag all entities in input range
  if (!all) {
    std::vector<unsigned char> vect(count, 1);
    rval = thisMB->tag_set_data( tag, entities, &vect[0] );
    if (MB_SUCCESS != rval) {
      thisMB->tag_delete(tag);
      return rval;
    }
  }
  
  switch (dim) {
    case 1:
      if (skin_verts)
        rval = find_skin_vertices_1D( tag, entities, *skin_verts );
      else if (skin_elems)
        rval = find_skin_vertices_1D( tag, entities, *skin_elems );
      else
        rval = MB_SUCCESS;
      break;
    case 2:
      rval = find_skin_vertices_2D(this_set, tag, entities, skin_verts,
                                    skin_elems, skin_rev_elems, 
                                    create_skin_elems, corners_only );
      break;
    case 3:
      rval = find_skin_vertices_3D( this_set, tag, entities, skin_verts,
                                    skin_elems, skin_rev_elems, 
                                    create_skin_elems, corners_only );
      break;
    default:
      rval = MB_TYPE_OUT_OF_RANGE;
      break;
  }
  
  thisMB->tag_delete(tag);
  return rval;
}

ErrorCode Skinner::find_skin_vertices_1D( Tag tag,
                                              const Range& edges,
                                              Range& skin_verts )
{
  // This rather simple algorithm is provided for completeness
  // (not sure how often one really wants the 'skin' of a chain
  // or tangle of edges.)
  //
  // A vertex is on the skin of the edges if it is contained in exactly
  // one of the edges *in the input range*.  
  //
  // This function expects the caller to have tagged all edges in the
  // input range with a value of one for the passed bit tag, and all
  // other edges with a value of zero.  This allows us to do a faster
  // intersection with the input range and the edges adjacent to a vertex.

  ErrorCode rval;
  Range::iterator hint = skin_verts.begin();
  
  // All input entities must be edges.
  if (!edges.all_of_dimension(1))
    return MB_TYPE_OUT_OF_RANGE;
  
    // get all the vertices
  Range verts;
  rval = thisMB->get_connectivity( edges, verts, true );
  if (MB_SUCCESS != rval)
    return rval;
  
    // Test how many edges each input vertex is adjacent to.
  std::vector<char> tag_vals;
  std::vector<EntityHandle> adj;
  int n;
  for (Range::const_iterator it = verts.begin(); it != verts.end(); ++it) {
      // get edges adjacent to vertex
    adj.clear();
    rval = thisMB->get_adjacencies( &*it, 1, 1, false, adj );
    if (MB_SUCCESS != rval) return rval;
    if (adj.empty())
      continue;

      // Intersect adjacent edges with the input list of edges
    tag_vals.resize( adj.size() );
    rval = thisMB->tag_get_data( tag, &adj[0], adj.size(), &tag_vals[0] );
    if (MB_SUCCESS != rval) return rval;
#ifdef MOAB_OLD_STD_COUNT
    n = 0;
    std::count( tag_vals.begin(), tag_vals.end(), '\001', n );
#else
    n = std::count( tag_vals.begin(), tag_vals.end(), '\001' );
#endif    
      // If adjacent to only one input edge, then vertex is on skin
    if (n == 1) {
      hint = skin_verts.insert( hint, *it );
    }
  }
  
  return MB_SUCCESS;
}


// A Container for storing a list of element sides adjacent
// to a vertex.  The template parameter is the number of 
// corners for the side.
template <unsigned CORNERS> 
class AdjSides 
{
public:
  
  /**
   * This struct is used to for a reduced representation of element
   * "sides" adjacent to a give vertex.  As such, it 
   * a) does not store the initial vertex that all sides are adjacent to
   * b) orders the remaining vertices in a specific way for fast comparison.
   * 
   * For edge elements, only the opposite vertex is stored.
   * For triangle elements, only the other two vertices are stored,
   *   and they are stored with the smaller of those two handles first.
   * For quad elements, only the other three vertices are stored.
   *  They are stored such that the vertex opposite the implicit (not
   *  stored) vertex is always in slot 1.  The other two vertices 
   *  (in slots 0 and 2) are ordered such that the handle of the one in 
   *  slot 0 is smaller than the handle in slot 2.
   *
   * For each side, the adj_elem field is used to store the element that
   * it is a side of as long as the element is considered to be on the skin.
   * The adj_elem field is cleared (set to zero) to indicate that this
   * side is no longer considered to be on the skin (and is the side of
   * more than one element.)
   */
  struct Side {
    EntityHandle handles[CORNERS-1]; //!< side vertices, except for implicit one
    EntityHandle adj_elem;           //!< element that this is a side of, or zero
    bool skin() const { return 0 != adj_elem; }
    
    /** construct from connectivity of side
     *\param array The connectivity of the element side.
     *\param idx   The index of the implicit vertex (contained
     *             in all sides in the list.)
     *\param adj   The element that this is a side of.
     */
    Side( const EntityHandle* array, int idx,
          EntityHandle adj, unsigned short  ) 
      : adj_elem(adj) 
    {
      switch (CORNERS) {
        case 3: handles[1] = array[(idx+2)%CORNERS];
        case 2: handles[0] = array[(idx+1)%CORNERS]; break;
        default:
          assert(false);
          break;
     }
      if (CORNERS == 3 && handles[1] > handles[0])
        std::swap( handles[0], handles[1] );
    }
    
    /** construct from connectivity of parent element
     *\param array The connectivity of the parent element
     *\param idx   The index of the implicit vertex (contained
     *             in all sides in the list.)  This is an index
     *             into 'indices', not 'array'.
     *\param adj   The element that this is a side of.
     *\param indices  The indices into 'array' at which the vertices
     *             representing the side occur.
     */
    Side( const EntityHandle* array,  int idx,
          EntityHandle adj, unsigned short ,
          const short* indices ) 
      : adj_elem(adj)
    {
      switch (CORNERS) {
        case 3: handles[1] = array[indices[(idx+2)%CORNERS]];
        case 2: handles[0] = array[indices[(idx+1)%CORNERS]]; break;
        default:
          assert(false);
          break;
     }
      if (CORNERS == 3 && handles[1] > handles[0])
        std::swap( handles[0], handles[1] );
    }
   
    // Compare two side instances.  Relies in the ordering of 
    // vertex handles as described above.
    bool operator==( const Side& other ) const 
    {
      switch (CORNERS) {
        case 3:
          return handles[0] == other.handles[0] 
              && handles[1] == other.handles[1];
        case 2:
          return handles[0] == other.handles[0];
        default:
          assert(false);
          return false;
     }
    }
  };


private:

  std::vector<Side> data; //!< List of sides
  size_t skin_count;      //!< Cached count of sides that are skin
  
public:

  typedef typename std::vector<Side>::iterator iterator;
  typedef typename std::vector<Side>::const_iterator const_iterator;
  const_iterator begin() const { return data.begin(); }
  const_iterator end() const { return data.end(); }
  
  void clear() { data.clear(); skin_count = 0; }
  bool empty() const { return data.empty(); }
  
  AdjSides() : skin_count(0) {}
  
  size_t num_skin() const { return skin_count; }
  
    /** \brief insert side, specifying side connectivity
     *
     * Either insert a new side, or if the side is already in the
     * list, mark it as not on the skin.
     *
     *\param handles The connectivity of the element side.
     *\param skip_idx The index of the implicit vertex (contained
     *             in all sides in the list.)
     *\param adj_elem The element that this is a side of.
     *\param elem_side Which side of adj_elem are we storing
     *             (CN side number.)
     */
  void insert( const EntityHandle* handles, int skip_idx,
               EntityHandle adj_elem, unsigned short elem_side )
  {
    Side side( handles, skip_idx, adj_elem, elem_side );
    iterator p = std::find( data.begin(), data.end(), side );
    if (p == data.end()) {
      data.push_back( side );
      ++skin_count; // not in list yet, so skin side (so far)
    }
    else if (p->adj_elem) {
      p->adj_elem = 0; // mark as not on skin
      --skin_count; // decrement cached count of skin elements
    }
  }
  
    /** \brief insert side, specifying list of indices into parent element
     * connectivity.
     *
     * Either insert a new side, or if the side is already in the
     * list, mark it as not on the skin.
     *
     *\param handles The connectivity of the parent element
     *\param skip_idx The index of the implicit vertex (contained
     *             in all sides in the list.)  This is an index
     *             into 'indices', not 'handles'.
     *\param adj_elem The element that this is a side of (parent handle).
     *\param indices  The indices into 'handles' at which the vertices
     *             representing the side occur.
     *\param elem_side Which side of adj_elem are we storing
     *             (CN side number.)
     */
  void insert( const EntityHandle* handles,  int skip_idx,
               EntityHandle adj_elem, unsigned short elem_side,
               const short* indices )
  {
    Side side( handles, skip_idx, adj_elem, elem_side, indices );
    iterator p = std::find( data.begin(), data.end(), side );
    if (p == data.end()) {
      data.push_back( side );
      ++skin_count; // not in list yet, so skin side (so far)
    }
    else if (p->adj_elem) {
      p->adj_elem = 0; // mark as not on skin
      --skin_count; // decrement cached count of skin elements
    }
  }
  
  /**\brief Search list for a given side, and if found, mark as not skin.
   *
   *\param other   Connectivity of side
   *\param skip_index Index in 'other' at which implicit vertex occurs.
   *\param elem_out If return value is true, the element that the side is a
   *                side of.  If return value is false, not set.
   *\return true if found and marked not-skin, false if not found.
   *
   * Given the connectivity of some existing element, check if it occurs
   * in the list.  If it does, clear the "is skin" state of the side so
   * that we know that we don't need to later create the side element.
   */
  bool find_and_unmark( const EntityHandle* other, int skip_index, EntityHandle& elem_out ) 
  {
    Side s( other, skip_index, 0, 0 );
    iterator p = std::find( data.begin(), data.end(), s );
    if (p == data.end() || !p->adj_elem)
      return false;
    else {
      elem_out = p->adj_elem;
      p->adj_elem = 0; // clear "is skin" state for side
      --skin_count;    // decrement cached count of skin sides
      return true;
    }
  }
};

/** construct from connectivity of side
  *\param array The connectivity of the element side.
  *\param idx   The index of the implicit vertex (contained
  *             in all sides in the list.)
  *\param adj   The element that this is a side of.
  */
template<>
AdjSides<4>::Side::Side( const EntityHandle* array, int idx,
      EntityHandle adj, unsigned short  ) 
  : adj_elem(adj)
{
  const unsigned int CORNERS=4;
  handles[2] = array[(idx+3)%CORNERS];
  handles[1] = array[(idx+2)%CORNERS];
  handles[0] = array[(idx+1)%CORNERS];
  if (handles[2] > handles[0])
    std::swap( handles[0], handles[2] );
}

/** construct from connectivity of parent element
  *\param array The connectivity of the parent element
  *\param idx   The index of the implicit vertex (contained
  *             in all sides in the list.)  This is an index
  *             into 'indices', not 'array'.
  *\param adj   The element that this is a side of.
  *\param indices  The indices into 'array' at which the vertices
  *             representing the side occur.
  */
template<>
AdjSides<4>::Side::Side( const EntityHandle* array,  int idx,
      EntityHandle adj, unsigned short ,
      const short* indices ) 
  : adj_elem(adj)
{
  const unsigned int CORNERS=4;
  handles[2] = array[indices[(idx+3)%CORNERS]];
  handles[1] = array[indices[(idx+2)%CORNERS]];
  handles[0] = array[indices[(idx+1)%CORNERS]];
  if (handles[2] > handles[0])
    std::swap( handles[0], handles[2] );
}

// Compare two side instances.  Relies in the ordering of 
// vertex handles as described above.
template<>
bool AdjSides<4>::Side::operator==( const Side& other ) const 
{
  return handles[0] == other.handles[0] 
      && handles[1] == other.handles[1]
      && handles[2] == other.handles[2];
}


// Utility function used by find_skin_vertices_2D and
// find_skin_vertices_3D to create elements representing
// the skin side of a higher-dimension element if one
// does not already exist.  
//
// Some arguments may seem redundant, but they are used
// to create the correct order of element when the input
// element contains higher-order nodes.
//
// This function always creates elements that have a "forward"
// orientation with respect to the parent element (have
// nodes ordered the same as CN returns for the "side").
//
// elem - The higher-dimension element for which to create
//        a lower-dim element representing the side.
// side_type - The EntityType of the desired side.
// side_conn - The connectivity of the new side.
ErrorCode Skinner::create_side( const EntityHandle this_set, EntityHandle elem,
                                    EntityType side_type,
                                    const EntityHandle* side_conn,
                                    EntityHandle& side_elem )
{
  const int max_side = 9;
  const EntityHandle* conn;
  int len, side_len, side, sense, offset, indices[max_side];
  ErrorCode rval;
  EntityType type = TYPE_FROM_HANDLE(elem), tmp_type;
  const int ncorner = CN::VerticesPerEntity( side_type );
  const int d = CN::Dimension(side_type);
  std::vector<EntityHandle> storage;
  
  // Get the connectivity of the parent element
  rval = thisMB->get_connectivity( elem, conn, len, false, &storage );
  if (MB_SUCCESS != rval) return rval;
 
  // treat separately MBPOLYGON; we want to create the edge in the
  // forward sense always ; so figure out the sense first, then get out
  if (MBPOLYGON==type && 1==d && MBEDGE==side_type)
  {
    // first find the first vertex in the conn list
    int i=0;
    for (i=0; i<len; i++)
    {
      if (conn[i]==side_conn[0])
        break;
    }
    if (len == i)
      return MB_FAILURE; // not found, big error
    // now, what if the polygon is padded?
    // the previous index is fine always. but the next one could be trouble :(
    int prevIndex = (i+len-1)%len;
    int nextIndex = (i+1)%len;
    // if the next index actually point to the same node, as current, it means it is padded
    if (conn[nextIndex]== conn[i])
    {
      // it really means we are at the end of proper nodes, the last nodes are repeated, so it should
      // be the first node
      nextIndex = 0; // this is the first node!
    }
    EntityHandle conn2[2] = {side_conn[0], side_conn[1]};
    if (conn[prevIndex]==side_conn[1])
    {
      // reverse, so the edge will be forward
      conn2[0]=side_conn[1];
      conn2[1]=side_conn[0];
    }
    else if  ( conn[nextIndex]!=side_conn[1])
      return MB_FAILURE; // it is not adjacent to the polygon

    rval = thisMB->create_element( MBEDGE, conn2, 2, side_elem );MB_CHK_ERR(rval);
    if (this_set)
      {
        rval = thisMB->add_entities(this_set, &side_elem,1); MB_CHK_ERR(rval);
      }
    return MB_SUCCESS;

  }
  // Find which side we are creating and get indices of all nodes
  // (including higher-order, if any.)
  CN::SideNumber( type, conn, side_conn, ncorner, d, side, sense, offset );
  CN::SubEntityNodeIndices( type, len, d, side, tmp_type, side_len, indices );
  assert(side_len <= max_side);
  assert(side_type == tmp_type);
  
  //NOTE: re-create conn array even when no higher-order nodes
  //      because we want it to always be forward with respect
  //      to the side ordering.
  EntityHandle side_conn_full[max_side];
  for (int i = 0; i < side_len; ++i)
    side_conn_full[i] = conn[indices[i]];
  
  rval = thisMB->create_element( side_type, side_conn_full, side_len, side_elem );MB_CHK_ERR(rval);
  if (this_set)
    {
      rval = thisMB->add_entities(this_set, &side_elem,1); MB_CHK_ERR(rval);
    }
  return MB_SUCCESS;;
}

// Test if an edge is reversed with respect CN's ordering
// for the "side" of a face.
bool Skinner::edge_reversed( EntityHandle face,
                               const EntityHandle* edge_ends )
{
  const EntityHandle* conn;
  int len, idx;
  ErrorCode rval = thisMB->get_connectivity( face, conn, len, true );
  if (MB_SUCCESS != rval) {
    assert(false);
    return false;
  }
  idx = std::find( conn, conn+len, edge_ends[0] ) - conn;
  if (idx == len) {
    assert(false);
    return false;
  }
  return (edge_ends[1] == conn[(idx+len-1)%len]);
}

// Test if a 2D element representing the side or face of a
// volume element is reversed with respect to the CN node
// ordering for the corresponding region element side.
bool Skinner::face_reversed( EntityHandle region,
                               const EntityHandle* face_corners,
                               EntityType face_type )
{
  const EntityHandle* conn;
  int len, side, sense, offset;
  ErrorCode rval = thisMB->get_connectivity( region, conn, len, true );
  if (MB_SUCCESS != rval) {
    assert(false);
    return false;
  }
  short r = CN::SideNumber( TYPE_FROM_HANDLE(region), conn, face_corners, 
                              CN::VerticesPerEntity(face_type),
                              CN::Dimension(face_type),
                              side, sense, offset );
  assert(0 == r);
  return (!r && sense == -1);
}

ErrorCode Skinner::find_skin_vertices_2D( const EntityHandle this_set, Tag tag,
                                              const Range& faces,
                                              Range* skin_verts,
                                              Range* skin_edges,
                                              Range* reversed_edges,
                                              bool create_edges,
                                              bool corners_only )
{
  // This function iterates over all the vertices contained in the
  // input face list.  For each such vertex, it then iterates over
  // all of the sides of the face elements adjacent to the vertex.
  // If an adjacent side is the side of only one of the input
  // faces, then that side is on the skin.  
  //
  // This algorithm will visit each skin vertex exactly once.  It
  // will visit each skin side once for each vertex in the side.
  //
  // This function expects the caller to have created the passed bit
  // tag and set it to one only for the faces in the passed range.  This
  // tag is used to do a fast intersection of the faces adjacent to a 
  // vertex with the faces in the input range (discard any for which the
  // tag is not set to one.)

  ErrorCode rval;
  std::vector<EntityHandle>::iterator i, j;
  Range::iterator hint;
  if (skin_verts)
    hint = skin_verts->begin();
  std::vector<EntityHandle> storage;
  const EntityHandle *conn;
  int len;
  bool find_edges = skin_edges || create_edges;
  bool printed_nonconformal_ho_warning = false;
  EntityHandle face;
  
  if (!faces.all_of_dimension(2))
    return MB_TYPE_OUT_OF_RANGE;
  
    // get all the vertices
  Range verts;
  rval = thisMB->get_connectivity( faces, verts, true );
  if (MB_SUCCESS != rval)
    return rval;
  
  std::vector<char> tag_vals;
  std::vector<EntityHandle> adj;
  AdjSides<2> adj_edges;
  for (Range::const_iterator it = verts.begin(); it != verts.end(); ++it) {
    bool higher_order = false;
  
      // get all adjacent faces
    adj.clear();
    rval = thisMB->get_adjacencies( &*it, 1, 2, false, adj );
    if (MB_SUCCESS != rval) return rval;
    if (adj.empty())
      continue;

      // remove those not in the input list (intersect with input list)
    i = j = adj.begin();
    tag_vals.resize( adj.size() );
    rval = thisMB->tag_get_data( tag, &adj[0], adj.size(), &tag_vals[0] );
    if (MB_SUCCESS != rval) return rval;
      // remove non-tagged entries
    i = j = adj.begin();
    for (; i != adj.end(); ++i) 
      if (tag_vals[i - adj.begin()])
        *(j++) = *i;
    adj.erase( j, adj.end() );
    
      // For each adjacent face, check the edges adjacent to the current vertex
    adj_edges.clear();    // other vertex for adjacent edges
    for (i = adj.begin(); i != adj.end(); ++i) {
      rval = thisMB->get_connectivity( *i, conn, len, false, &storage );
      if (MB_SUCCESS != rval) return rval;
      
        // For a single face element adjacent to this vertex, there
        // will be exactly two sides (edges) adjacent to the vertex.
        // Find the other vertex for each of the two edges.
        
      EntityHandle prev, next; // vertices of two adjacent edge-sides
      const int idx = std::find(conn, conn+len, *it) - conn;
      assert(idx != len);

      if (TYPE_FROM_HANDLE(*i) == MBTRI && len > 3) {
        len = 3;
        higher_order = true;
        if (idx > 2) { // skip higher-order nodes for now
          if (!printed_nonconformal_ho_warning) {
            printed_nonconformal_ho_warning = true;
            std::cerr << "Non-conformal higher-order mesh detected in skinner: "
                      << "vertex " << ID_FROM_HANDLE(*it) << " is a corner in "
                      << "some elements and a higher-order node in others" 
                      << std::endl;
          }
          continue;
        }
      }
      else if (TYPE_FROM_HANDLE(*i) == MBQUAD && len > 4) {
        len = 4;
        higher_order = true;
        if (idx > 3) { // skip higher-order nodes for now
          if (!printed_nonconformal_ho_warning) {
            printed_nonconformal_ho_warning = true;
            std::cerr << "Non-conformal higher-order mesh detected in skinner: "
                      << "vertex " << ID_FROM_HANDLE(*it) << " is a corner in "
                      << "some elements and a higher-order node in others" 
                      << std::endl;
          }
          continue;
        }
      }

      // so it must be a MBPOLYGON
      const int prev_idx = (idx + len - 1)%len; // this should be fine, always, even for padded case
      prev = conn[prev_idx];
      next = conn[(idx+1)%len];
      if (next == conn[idx]) // it must be the padded case, so roll to the beginning
        next = conn[0];
      
        // Insert sides (edges) in our list of candidate skin sides
      adj_edges.insert( &prev, 1, *i, prev_idx );
      adj_edges.insert( &next, 1, *i, idx );
    }
    
      // If vertex is not on skin, advance to next vertex.
      // adj_edges handled checking for duplicates on insertion.
      // If every candidate skin edge occurred more than once (was
      // not in fact on the skin), then we're done with this vertex.
    if (0 == adj_edges.num_skin())
      continue;
    
      // If user requested Range of *vertices* on the skin...
    if (skin_verts) {
        // Put skin vertex in output list
      hint = skin_verts->insert( hint, *it );
 
        // Add mid edge nodes to vertex list
      if (!corners_only && higher_order) {
        for (AdjSides<2>::const_iterator p = adj_edges.begin(); p != adj_edges.end(); ++p) {
          if (p->skin()) {
            face = p->adj_elem;
            EntityType type = TYPE_FROM_HANDLE(face);

            rval = thisMB->get_connectivity( face, conn, len, false );
            if (MB_SUCCESS != rval) return rval;
            if (!CN::HasMidEdgeNodes( type, len ))
              continue;

            EntityHandle ec[2] = { *it, p->handles[0] };
            int side, sense, offset;
            CN::SideNumber( type, conn, ec, 2, 1, side, sense, offset );
            offset = CN::HONodeIndex( type, len, 1, side );
            assert(offset >= 0 && offset < len);
            skin_verts->insert( conn[offset] );
          }
        }
      }
    }
 
      // If user requested Range of *edges* on the skin...
    if (find_edges) {
        // Search list of existing adjacent edges for any that are on the skin
      adj.clear();
      rval = thisMB->get_adjacencies( &*it, 1, 1, false, adj );
      if (MB_SUCCESS != rval) return rval;
      for (i = adj.begin(); i != adj.end(); ++i) {
        rval = thisMB->get_connectivity( *i, conn, len, true );
        if (MB_SUCCESS != rval) return rval;

          // bool equality expression within find_and_unmark call
          // will be evaluate to the index of *it in the conn array. 
          //
          // Note that the order of the terms in the if statement is important.
          // We want to unmark any existing skin edges even if we aren't 
          // returning them.  Otherwise we'll end up creating duplicates 
          // if create_edges is true and skin_edges is not.
        if (adj_edges.find_and_unmark( conn, (conn[1] == *it), face ) && skin_edges) {
          if (reversed_edges && edge_reversed( face, conn ))
            reversed_edges->insert( *i );
          else
            skin_edges->insert( *i );
        }
      }
    }
    
      // If the user requested that we create new edges for sides
      // on the skin for which there is no existing edge, and there
      // are still skin sides for which no corresponding edge was found...
    if (create_edges && adj_edges.num_skin()) {
        // Create any skin edges that don't exist
      for (AdjSides<2>::const_iterator p = adj_edges.begin(); p != adj_edges.end(); ++p) {
        if (p->skin()) {
          EntityHandle edge, ec[] = { *it, p->handles[0] };
          rval = create_side(this_set, p->adj_elem, MBEDGE, ec, edge );
          if (MB_SUCCESS != rval) return rval;
          if (skin_edges)
            skin_edges->insert( edge );
        }
      }
    }

  } // end for each vertex
  
  return MB_SUCCESS;
}
  

ErrorCode Skinner::find_skin_vertices_3D(const EntityHandle this_set, Tag tag,
                                              const Range& entities,
                                              Range* skin_verts,
                                              Range* skin_faces,
                                              Range* reversed_faces,
                                              bool create_faces,
                                              bool corners_only )
{
  // This function iterates over all the vertices contained in the
  // input vol elem list.  For each such vertex, it then iterates over
  // all of the sides of the vol elements adjacent to the vertex.
  // If an adjacent side is the side of only one of the input
  // elements, then that side is on the skin.  
  //
  // This algorithm will visit each skin vertex exactly once.  It
  // will visit each skin side once for each vertex in the side.
  //
  // This function expects the caller to have created the passed bit
  // tag and set it to one only for the elements in the passed range.  This
  // tag is used to do a fast intersection of the elements adjacent to a 
  // vertex with the elements in the input range (discard any for which the
  // tag is not set to one.)
  //
  // For each vertex, iterate over each adjacent element.  Construct
  // lists of the sides of each adjacent element that contain the vertex.
  //
  // A list of three-vertex sides is kept for all triangular faces,
  // included three-vertex faces of type MBPOLYGON.  Putting polygons
  // in the same list ensures that we find polyhedron and non-polyhedron
  // elements that are adjacent.
  //
  // A list of four-vertex sides is kept for all quadrilateral faces,
  // including four-vertex faces of type MBPOLYGON.
  //
  // Sides with more than four vertices must have an explicit MBPOLYGON
  // element representing them because MBPOLYHEDRON connectivity is a
  // list of faces rather than vertices.  So the third list (vertices>=5),
  // need contain only the handle of the face rather than the vertex handles.

  ErrorCode rval;
  std::vector<EntityHandle>::iterator i, j;
  Range::iterator hint;
  if (skin_verts)
    hint = skin_verts->begin();
  std::vector<EntityHandle> storage, storage2; // temp storage for conn lists
  const EntityHandle *conn, *conn2;
  int len, len2;
  bool find_faces = skin_faces || create_faces;
  int clen, side, sense, offset, indices[9];
  EntityType face_type;
  EntityHandle elem;
  bool printed_nonconformal_ho_warning = false;
  
  if (!entities.all_of_dimension(3))
    return MB_TYPE_OUT_OF_RANGE;
  
  // get all the vertices
  Range verts;
  rval = thisMB->get_connectivity( entities, verts, true );
  if (MB_SUCCESS != rval)
    return rval;
  // if there are polyhedra in the input list, need to make another
  // call to get vertices from faces
  if (!verts.all_of_dimension(0)) {
    Range::iterator it = verts.upper_bound( MBVERTEX );
    Range pfaces;
    pfaces.merge( it, verts.end() );
    verts.erase( it, verts.end() );
    rval = thisMB->get_connectivity( pfaces, verts, true );
    if (MB_SUCCESS != rval)
      return rval;
    assert(verts.all_of_dimension(0));
  }
    
  
  AdjSides<4> adj_quads; // 4-node sides adjacent to a vertex
  AdjSides<3> adj_tris;  // 3-node sides adjacent to a vertex
  AdjSides<2> adj_poly;  // n-node sides (n>5) adjacent to vertex
                         // (must have an explicit polygon, so store
                         // polygon handle rather than vertices.)
  std::vector<char> tag_vals;
  std::vector<EntityHandle> adj;
  for (Range::const_iterator it = verts.begin(); it != verts.end(); ++it) {
    bool higher_order = false;
  
      // get all adjacent elements
    adj.clear();
    rval = thisMB->get_adjacencies( &*it, 1, 3, false, adj );
    if (MB_SUCCESS != rval) return rval;
    if (adj.empty())
      continue;
      
      // remove those not tagged (intersect with input range)
    i = j = adj.begin();
    tag_vals.resize( adj.size() );
    rval = thisMB->tag_get_data( tag, &adj[0], adj.size(), &tag_vals[0] );
    if (MB_SUCCESS != rval) return rval;
    for (; i != adj.end(); ++i) 
      if (tag_vals[i - adj.begin()])
        *(j++) = *i;
    adj.erase( j, adj.end() );
      
      // Build lists of sides of 3D element adjacent to the current vertex
    adj_quads.clear(); // store three other vertices for each adjacent quad face
    adj_tris.clear();  // store two other vertices for each adjacent tri face
    adj_poly.clear();  // store handle of each adjacent polygonal face
    int idx;
    for (i = adj.begin(); i != adj.end(); ++i) {
      const EntityType type = TYPE_FROM_HANDLE(*i);
      
        // Special case for POLYHEDRA
      if (type == MBPOLYHEDRON) {
        rval = thisMB->get_connectivity( *i, conn, len );
        if (MB_SUCCESS != rval) return rval;
        for (int k = 0; k < len; ++k) {
          rval = thisMB->get_connectivity( conn[k], conn2, len2, true, &storage2 );
          if (MB_SUCCESS != rval) return rval;
          idx = std::find( conn2, conn2+len2, *it) - conn2;
          if (idx == len2) // vertex not in this face
            continue;
          
            // Treat 3- and 4-vertex faces specially, so that
            // if the mesh contains both elements and polyhedra,
            // we don't miss one type adjacent to the other.
          switch (len2) {
            case 3:
              adj_tris.insert( conn2, idx, *i, k );
              break;
            case 4:
              adj_quads.insert( conn2, idx, *i, k );
              break;
            default:
              adj_poly.insert( conn+k, 1, *i, k );
              break;
            }
        }
      }
      else {
        rval = thisMB->get_connectivity( *i, conn, len, false, &storage );
        if (MB_SUCCESS != rval) return rval;

        idx = std::find(conn, conn+len, *it) - conn;
        assert(idx != len);
        
        if (len > CN::VerticesPerEntity( type )) {
          higher_order =true;
            // skip higher-order nodes for now
          if (idx >= CN::VerticesPerEntity( type ))  {
            if (!printed_nonconformal_ho_warning) {
              printed_nonconformal_ho_warning = true;
              std::cerr << "Non-conformal higher-order mesh detected in skinner: "
                        << "vertex " << ID_FROM_HANDLE(*it) << " is a corner in "
                        << "some elements and a higher-order node in others" 
                        << std::endl;
            }
            continue;
          }
        }

          // For each side of the element...
        const int num_faces = CN::NumSubEntities( type, 2 );
        for (int f = 0; f < num_faces; ++f) {
          int num_vtx;
          const short* face_indices = CN::SubEntityVertexIndices(type, 2, f, face_type, num_vtx );
          const short face_idx = std::find(face_indices, face_indices+num_vtx, (short)idx) - face_indices;
            // skip sides that do not contain vertex from outer loop
          if (face_idx == num_vtx)
            continue; // current vertex not in this face

          assert(num_vtx <= 4); // polyhedra handled above
          switch (face_type) {
            case MBTRI:
              adj_tris.insert( conn, face_idx, *i, f, face_indices );
              break;
            case MBQUAD:
              adj_quads.insert( conn, face_idx, *i, f, face_indices );
              break;
            default:
              return MB_TYPE_OUT_OF_RANGE;
          }
        }
      }
    } // end for (adj[3])
    
      // If vertex is not on skin, advance to next vertex
    if (0 == (adj_tris.num_skin() + adj_quads.num_skin() + adj_poly.num_skin()))
      continue;
    
      // If user requested that skin *vertices* be passed back...
    if (skin_verts) {
        // Put skin vertex in output list
      hint = skin_verts->insert( hint, *it );
 
        // Add mid-edge and mid-face nodes to vertex list
      if (!corners_only && higher_order) {
        for (AdjSides<3>::const_iterator t = adj_tris.begin(); t != adj_tris.end(); ++t) {
          if (t->skin()) {
            elem = t->adj_elem;
            EntityType type = TYPE_FROM_HANDLE(elem);

            rval = thisMB->get_connectivity( elem, conn, len, false );
            if (MB_SUCCESS != rval) return rval;
            if (!CN::HasMidNodes( type, len ))
              continue;

            EntityHandle ec[3] = { *it, t->handles[0], t->handles[1] };
            CN::SideNumber( type, conn, ec, 3, 2, side, sense, offset );
            CN::SubEntityNodeIndices( type, len, 2, side, face_type, clen, indices );
            assert(MBTRI == face_type);
            for (int k = 3; k < clen; ++k)
              skin_verts->insert( conn[indices[k]] );
          }
        }
        for (AdjSides<4>::const_iterator q = adj_quads.begin(); q != adj_quads.end(); ++q) {
          if (q->skin()) {
            elem = q->adj_elem;
            EntityType type = TYPE_FROM_HANDLE(elem);

            rval = thisMB->get_connectivity( elem, conn, len, false );
            if (MB_SUCCESS != rval) return rval;
            if (!CN::HasMidNodes( type, len ))
              continue;

            EntityHandle ec[4] = { *it, q->handles[0], q->handles[1], q->handles[2] };
            CN::SideNumber( type, conn, ec, 4, 2, side, sense, offset );
            CN::SubEntityNodeIndices( type, len, 2, side, face_type, clen, indices );
            assert(MBQUAD == face_type);
            for (int k = 4; k < clen; ++k)
              skin_verts->insert( conn[indices[k]] );
          }
        }
      }
    }

      // If user requested that we pass back the list of 2D elements
      // representing the skin of the mesh...
    if (find_faces) {
        // Search list of adjacent faces for any that are on the skin
      adj.clear();
      rval = thisMB->get_adjacencies( &*it, 1, 2, false, adj );
      if (MB_SUCCESS != rval) return rval;

      for (i = adj.begin(); i != adj.end(); ++i) {
        rval = thisMB->get_connectivity( *i, conn, len, true );
        if (MB_SUCCESS != rval) return rval;
        const int idx2 = std::find( conn, conn+len, *it ) - conn;
        if (idx2 >= len) {
          assert(printed_nonconformal_ho_warning);
          continue;
        }

          // Note that the order of the terms in the if statements below
          // is important.  We want to unmark any existing skin faces even 
          // if we aren't returning them.  Otherwise we'll end up creating 
          // duplicates if create_faces is true.
        if (3 == len) {
          if (adj_tris.find_and_unmark( conn, idx2, elem ) && skin_faces) {
            if (reversed_faces && face_reversed( elem, conn, MBTRI ))
              reversed_faces->insert( *i );
            else
              skin_faces->insert( *i );
          }
        }
        else if (4 == len) {
          if (adj_quads.find_and_unmark( conn, idx2, elem ) && skin_faces) {
            if (reversed_faces && face_reversed( elem, conn, MBQUAD ))
              reversed_faces->insert( *i );
            else
              skin_faces->insert( *i );
          }
        }
        else {
          if (adj_poly.find_and_unmark( &*i, 1, elem ) && skin_faces)
            skin_faces->insert( *i );
        }
      }
    }

      // If user does not want use to create new faces representing
      // sides for which there is currently no explicit element, 
      // skip the remaining code and advance the outer loop to the 
      // next vertex.
    if (!create_faces)
      continue;

      // Polyhedra always have explicitly defined faces, so
      // there is no way we could need to create such a face.
    assert(0 == adj_poly.num_skin());
    
      // Create any skin tris that don't exist
    if (adj_tris.num_skin()) {
      for (AdjSides<3>::const_iterator t = adj_tris.begin(); t != adj_tris.end(); ++t) {
        if (t->skin()) {
          EntityHandle tri, c[3] = { *it, t->handles[0], t->handles[1] };
          rval = create_side( this_set, t->adj_elem, MBTRI, c, tri );
          if (MB_SUCCESS != rval) return rval;
          if (skin_faces)
            skin_faces->insert( tri );
        }
      }
    }
    
      // Create any skin quads that don't exist
    if (adj_quads.num_skin()) {
      for (AdjSides<4>::const_iterator q = adj_quads.begin(); q != adj_quads.end(); ++q) {
        if (q->skin()) {
          EntityHandle quad, c[4] = { *it, q->handles[0], q->handles[1], q->handles[2] };
          rval = create_side(this_set, q->adj_elem, MBQUAD, c, quad );
          if (MB_SUCCESS != rval) return rval;
          if (skin_faces)
            skin_faces->insert( quad );
        }
      }
    }
  } // end for each vertex
  
  return MB_SUCCESS;
}

} // namespace moab
