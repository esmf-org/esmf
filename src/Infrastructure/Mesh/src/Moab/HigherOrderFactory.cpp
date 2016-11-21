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


#ifdef WIN32
#ifdef _DEBUG
// turn off warnings that say they debugging identifier has been truncated
// this warning comes up when using some STL containers
#pragma warning(disable : 4786)
#endif
#endif

#include "moab/HigherOrderFactory.hpp"
#include "SequenceManager.hpp"
#include "UnstructuredElemSeq.hpp"
#include "VertexSequence.hpp"
#include "AEntityFactory.hpp"
#include "moab/Core.hpp"
#include "moab/CN.hpp"
#include <assert.h>
#include <algorithm>

namespace moab {

using namespace std;

HigherOrderFactory::HigherOrderFactory(Core* MB, Interface::HONodeAddedRemoved* function_object) 
  : mMB(MB), mHONodeAddedRemoved(function_object)
{
  initialize_map();
}
HigherOrderFactory::~HigherOrderFactory() {}

//bool HigherOrderFactory::mMapInitialized = false;

void HigherOrderFactory::initialize_map()
{
 // if(mMapInitialized)
  //  return;

  for(EntityType i=MBVERTEX; i<MBMAXTYPE; i++)
  {
    const CN::ConnMap& canon_map = CN::mConnectivityMap[i][0];
    unsigned char (&this_map)[8][8] = mNodeMap[i];
    int num_node = CN::VerticesPerEntity(i);
    for(int j=0; j<canon_map.num_sub_elements; j++)
    {
      unsigned char x = canon_map.conn[j][0];
      unsigned char y = canon_map.conn[j][1];
      this_map[x][y] = num_node;
      this_map[y][x] = num_node;
      num_node++;
    }
  }

  //mMapInitialized = true;
}


ErrorCode HigherOrderFactory::convert( const EntityHandle meshset, 
                                         const bool mid_edge_nodes, 
                                         const bool mid_face_nodes, 
                                         const bool mid_volume_nodes )
{
  Range entities;
  mMB->get_entities_by_handle(meshset, entities, true);
  return convert( entities, mid_edge_nodes, mid_face_nodes, mid_volume_nodes );
}

ErrorCode HigherOrderFactory::convert( const Range& entities,
                                         const bool mid_edge_nodes, 
                                         const bool mid_face_nodes, 
                                         const bool mid_volume_nodes )

{

  // TODO --  add some more code to prevent from splitting of entity sequences when we don't need to.
  // Say we have all hex8's in our mesh and 3 falses are passed in.  In the end, no conversion will
  // happen, but the sequences could still be split up.

  // find out what entity sequences we need to convert 
  // and where, if necessary, to split them

  SequenceManager* seq_manager = mMB->sequence_manager();
  Range::const_pair_iterator p_iter;
  for (p_iter = entities.const_pair_begin(); p_iter != entities.const_pair_end(); ++p_iter) {
    
    EntityHandle h = p_iter->first;
    while (h <= p_iter->second) {
      
      EntitySequence* seq;
      ErrorCode rval = seq_manager->find( h, seq );
      if (MB_SUCCESS != rval)
        return rval;
      
      if (seq->type() == MBVERTEX || seq->type() >= MBENTITYSET)
        return MB_TYPE_OUT_OF_RANGE;
      
        // make sequence is not structured mesh
      ElementSequence* elemseq = static_cast<ElementSequence*>(seq);
      if (NULL == elemseq->get_connectivity_array())
        return MB_NOT_IMPLEMENTED;
      
      EntityHandle last = p_iter->second;
      if (last > seq->end_handle())
        last = seq->end_handle();
      
      rval = convert_sequence( elemseq, h, last, 
                               mid_edge_nodes,
                               mid_face_nodes,
                               mid_volume_nodes );
      if (MB_SUCCESS != rval)
        return rval;
        
      h = last + 1;
    }
  }
  
  return MB_SUCCESS;
} 


ErrorCode HigherOrderFactory::convert_sequence( ElementSequence* seq, 
                                                  EntityHandle start,
                                                  EntityHandle end,
                                                  bool mid_edge_nodes, 
                                                  bool mid_face_nodes, 
                                                  bool mid_volume_nodes)
{

  ErrorCode status = MB_SUCCESS;

  // lets make sure parameters are ok before we continue
  switch (seq->type()) {
    default: return MB_TYPE_OUT_OF_RANGE; 
    case MBEDGE:
      mid_face_nodes = false;
    case MBTRI:
    case MBQUAD:
      mid_volume_nodes = false;
    case MBTET:
    case MBHEX:
    case MBPRISM:
    case MBPYRAMID:
    case MBKNIFE:
      break;
  }

    // calculate number of nodes in target configuration
  unsigned nodes_per_elem = CN::VerticesPerEntity( seq->type() );
  if (mid_edge_nodes)
    nodes_per_elem += (seq->type() == MBEDGE) ? 1 : CN::NumSubEntities( seq->type(), 1 );
  if (mid_face_nodes)
    nodes_per_elem += (CN::Dimension(seq->type()) == 2) ? 1 : CN::NumSubEntities( seq->type(), 2 );
  if (mid_volume_nodes) 
    nodes_per_elem += 1;
  
  if (nodes_per_elem == seq->nodes_per_element())
    return MB_SUCCESS;
  
  Tag deletable_nodes;
  status = mMB->tag_get_handle(0, 1, MB_TYPE_BIT, deletable_nodes, MB_TAG_CREAT|MB_TAG_BIT);
  if (MB_SUCCESS != status)
    return status;
  
  UnstructuredElemSeq* new_seq = new UnstructuredElemSeq( start,
                                                          end - start + 1,
                                                          nodes_per_elem,
                                                          end - start + 1 );
  
  copy_corner_nodes( seq, new_seq );

  if (seq->has_mid_edge_nodes() && mid_edge_nodes) 
    status = copy_mid_edge_nodes( seq, new_seq );
  else if (seq->has_mid_edge_nodes() && !mid_edge_nodes)
    status = remove_mid_edge_nodes( seq, start, end, deletable_nodes );
  else if (!seq->has_mid_edge_nodes() && mid_edge_nodes)
    status = zero_mid_edge_nodes( new_seq );
  if (MB_SUCCESS != status)
    return status;

  if (seq->has_mid_face_nodes() && mid_face_nodes) 
    status = copy_mid_face_nodes( seq, new_seq );
  else if (seq->has_mid_face_nodes() && !mid_face_nodes)
    status = remove_mid_face_nodes( seq, start, end, deletable_nodes );
  else if (!seq->has_mid_face_nodes() && mid_face_nodes)
    status = zero_mid_face_nodes( new_seq );
  if (MB_SUCCESS != status) {
    mMB->tag_delete( deletable_nodes );
    return status;
  }
 
  if (seq->has_mid_volume_nodes() && mid_volume_nodes) 
    status = copy_mid_volume_nodes( seq, new_seq );
  else if (seq->has_mid_volume_nodes() && !mid_volume_nodes)
    status = remove_mid_volume_nodes( seq, start, end, deletable_nodes );
  else if (!seq->has_mid_volume_nodes() && mid_volume_nodes)
    status = zero_mid_volume_nodes( new_seq );
  if (MB_SUCCESS != status) {
    mMB->tag_delete( deletable_nodes );
    return status;
  }

  // gather nodes that were marked
  Range nodes;
  mMB->get_entities_by_type_and_tag(0, MBVERTEX, &deletable_nodes, NULL, 1, nodes);

  //EntityHandle low_meshset;
  //int dum;
  //low_meshset = CREATE_HANDLE(MBENTITYSET, 0, dum);
  
  for(Range::iterator iter = nodes.begin(); iter != nodes.end(); ++iter)
  {
    unsigned char marked = 0;
    mMB->tag_get_data(deletable_nodes, &(*iter), 1, &marked);
    if(marked)
    {
      // we can delete it
      if(mHONodeAddedRemoved)
        mHONodeAddedRemoved->node_removed( *iter );
      mMB->delete_entities(&(*iter), 1);
    }
  }
  
  const bool create_midedge = !seq->has_mid_edge_nodes() && mid_edge_nodes;
  const bool create_midface = !seq->has_mid_face_nodes() && mid_face_nodes;
  const bool create_midvolm = !seq->has_mid_volume_nodes() && mid_volume_nodes;
  
  mMB->tag_delete(deletable_nodes);

  status = mMB->sequence_manager()->replace_subsequence( new_seq );
  if (MB_SUCCESS != status) {
    SequenceData* data = new_seq->data();
    delete new_seq;
    delete data;
    return status;
  }

  if (create_midedge) {
    status = add_mid_edge_nodes( new_seq );
    if (MB_SUCCESS != status)
      return status;
  }
  if (create_midface) {
    status = add_mid_face_nodes( new_seq );
    if (MB_SUCCESS != status)
      return status;
  }
  if (create_midvolm) {
    status = add_mid_volume_nodes( new_seq );
    if (MB_SUCCESS != status)
      return status;
  }
  
  return status;

}


ErrorCode HigherOrderFactory::add_mid_volume_nodes(ElementSequence* seq)
{
  EntityType this_type = seq->type();
  SequenceManager* seq_manager = mMB->sequence_manager();

  // find out where in the connectivity list to add these new mid volume nodes
  int edge_factor = seq->has_mid_edge_nodes() ? 1 : 0;
  int face_factor = seq->has_mid_face_nodes() ? 1 : 0;
  // offset by number of higher order nodes on edges if they exist
  int num_corner_nodes = CN::VerticesPerEntity(this_type);
  int new_node_index = num_corner_nodes;
  new_node_index += edge_factor * CN::mConnectivityMap[this_type][0].num_sub_elements;
  new_node_index += face_factor * CN::mConnectivityMap[this_type][1].num_sub_elements;

  EntityHandle* element = seq->get_connectivity_array();
  EntityHandle curr_handle = seq->start_handle();
  int nodes_per_element = seq->nodes_per_element();
  EntityHandle* end_element = element + nodes_per_element * (seq->size());

  // iterate over the elements
  for(; element < end_element; element+=nodes_per_element)
  {
    // find the centroid of this element
    double tmp_coords[3], sum_coords[3] = {0,0,0};
    EntitySequence* eseq=NULL;
    for(int i=0; i<num_corner_nodes; i++)
    {
      seq_manager->find(element[i], eseq);
      static_cast<VertexSequence*>(eseq)->get_coordinates(
          element[i], tmp_coords[0], tmp_coords[1], tmp_coords[2]
          );
      sum_coords[0] += tmp_coords[0];
      sum_coords[1] += tmp_coords[1];
      sum_coords[2] += tmp_coords[2];
    }
    sum_coords[0] /= num_corner_nodes;
    sum_coords[1] /= num_corner_nodes;
    sum_coords[2] /= num_corner_nodes;

    // create a new vertex at the centroid
    mMB->create_vertex(sum_coords, element[new_node_index]);
    
    if(mHONodeAddedRemoved)
      mHONodeAddedRemoved->node_added(element[new_node_index], curr_handle);

    curr_handle++;
  }

  return MB_SUCCESS;
}


ErrorCode HigherOrderFactory::add_mid_face_nodes(ElementSequence* seq)
{
  EntityType this_type = seq->type();
  SequenceManager* seq_manager = mMB->sequence_manager();
  int num_vertices = CN::VerticesPerEntity(this_type);
  int num_edges = CN::mConnectivityMap[this_type][0].num_sub_elements;
  num_edges = seq->has_mid_edge_nodes() ? num_edges : 0;
  int num_faces = CN::mConnectivityMap[this_type][1].num_sub_elements;

  const CN::ConnMap& entity_faces = CN::mConnectivityMap[this_type][1];

  EntityHandle* element = seq->get_connectivity_array();
  EntityHandle curr_handle = seq->start_handle();
  int nodes_per_element = seq->nodes_per_element();
  EntityHandle* end_element = element + nodes_per_element * (seq->size());

  EntityHandle tmp_face_conn[4];  // max face nodes = 4
  std::vector<EntityHandle> adjacent_entities(4);

  double tmp_coords[3];
  
  // iterate over the elements
  for(; element < end_element; element+=nodes_per_element)
  {
    // for each edge in this entity
    for(int i=0; i<num_faces; i++)
    {
      // a node was already assigned
      if(element[i+num_edges+num_vertices] != 0)
        continue;

      tmp_face_conn[0] = element[entity_faces.conn[i][0]];
      tmp_face_conn[1] = element[entity_faces.conn[i][1]];
      tmp_face_conn[2] = element[entity_faces.conn[i][2]];
      if(entity_faces.num_corners_per_sub_element[i] == 4)
        tmp_face_conn[3] = element[entity_faces.conn[i][3]];
      else
        tmp_face_conn[3] = 0;

      EntityHandle already_made_node = center_node_exist(tmp_face_conn, adjacent_entities);
      
      if(already_made_node)
      {
        element[i+num_edges+num_vertices] = already_made_node;
      }
      // create a node
      else
      {
        EntitySequence* tmp_sequence = NULL;
        double sum_coords[3] = {0,0,0};
        int max_nodes = entity_faces.num_corners_per_sub_element[i];
        for(int k=0; k<max_nodes; k++)
        {
          seq_manager->find(tmp_face_conn[k], tmp_sequence);
          static_cast<VertexSequence*>(tmp_sequence)->get_coordinates( 
              tmp_face_conn[k], tmp_coords[0], tmp_coords[1], tmp_coords[2]);
          sum_coords[0] += tmp_coords[0];
          sum_coords[1] += tmp_coords[1];
          sum_coords[2] += tmp_coords[2];
        }

        sum_coords[0] /= max_nodes;
        sum_coords[1] /= max_nodes;
        sum_coords[2] /= max_nodes;

        mMB->create_vertex(sum_coords, element[i+num_edges+num_vertices]);
      }

      if(mHONodeAddedRemoved)
        mHONodeAddedRemoved->node_added(element[i+num_edges+num_vertices], curr_handle);
      
    }

    curr_handle++;

  }

  return MB_SUCCESS;
}


ErrorCode HigherOrderFactory::add_mid_edge_nodes(ElementSequence* seq)
{
  // for each node, need to see if it was already created.
  EntityType this_type = seq->type();
  SequenceManager* seq_manager = mMB->sequence_manager();

  // offset by number of corner nodes
  int num_vertices = CN::VerticesPerEntity(this_type);
  int num_edges = CN::mConnectivityMap[this_type][0].num_sub_elements;

  const CN::ConnMap& entity_edges = CN::mConnectivityMap[this_type][0];
  
  EntityHandle* element = seq->get_connectivity_array();
  EntityHandle curr_handle = seq->start_handle();
  int nodes_per_element = seq->nodes_per_element();
  EntityHandle* end_element = element + nodes_per_element * (seq->size());

  EntityHandle tmp_edge_conn[2];
  std::vector<EntityHandle> adjacent_entities(32);

  double tmp_coords[3];

  // iterate over the elements
  for(; element < end_element; element+=nodes_per_element)
  {
    // for each edge in this entity
    for(int i=0; i<num_edges; i++)
    {
      // a node was already assigned
      if(element[i+num_vertices] != 0)
        continue;

      tmp_edge_conn[0] = element[entity_edges.conn[i][0]];
      tmp_edge_conn[1] = element[entity_edges.conn[i][1]];

      EntityHandle already_made_node = center_node_exist(tmp_edge_conn[0], tmp_edge_conn[1],
          adjacent_entities);
      
      if(already_made_node)
      {
        element[i+num_vertices] = already_made_node;
      }
      // create a node
      else
      {
        EntitySequence* tmp_sequence = NULL;
        double sum_coords[3] = {0,0,0};
        seq_manager->find(tmp_edge_conn[0], tmp_sequence);
        static_cast<VertexSequence*>(tmp_sequence)->get_coordinates( 
            tmp_edge_conn[0], tmp_coords[0], tmp_coords[1], tmp_coords[2]);
        sum_coords[0] += tmp_coords[0];
        sum_coords[1] += tmp_coords[1];
        sum_coords[2] += tmp_coords[2];
        seq_manager->find(tmp_edge_conn[1], tmp_sequence);
        static_cast<VertexSequence*>(tmp_sequence)->get_coordinates( 
            tmp_edge_conn[1], tmp_coords[0], tmp_coords[1], tmp_coords[2]);
        sum_coords[0] = (sum_coords[0] + tmp_coords[0]) /2;
        sum_coords[1] = (sum_coords[1] + tmp_coords[1]) /2;
        sum_coords[2] = (sum_coords[2] + tmp_coords[2]) /2;

        mMB->create_vertex(sum_coords, element[i+num_vertices]);
      }

      if(mHONodeAddedRemoved)
        mHONodeAddedRemoved->node_added(element[i+num_vertices], curr_handle);
      
    }

    curr_handle++;

  }

  return MB_SUCCESS;
}

EntityHandle HigherOrderFactory::center_node_exist( EntityHandle corner1, 
    EntityHandle corner2, std::vector<EntityHandle>& adj_entities)
{
  AEntityFactory* a_fact = mMB->a_entity_factory();
  std::vector<EntityHandle> adj_corner1(32);
  std::vector<EntityHandle> adj_corner2(32);

  // create needed vertex adjacencies
  if (!a_fact->vert_elem_adjacencies())
    a_fact->create_vert_elem_adjacencies();

  // vectors are returned sorted
  
  a_fact->get_adjacencies(corner1, adj_corner1);
  a_fact->get_adjacencies(corner2, adj_corner2);

  // these are the entities adjacent to both nodes
  adj_entities.clear();
  std::set_intersection(adj_corner1.begin(), adj_corner1.end(), adj_corner2.begin(),
      adj_corner2.end(), std::back_inserter<std::vector<EntityHandle> >(adj_entities));


  // iterate of the entities to find a mid node
  const EntityHandle* conn;
  int conn_size = 0;
  for(std::vector<EntityHandle>::iterator iter = adj_entities.begin();
      iter != adj_entities.end(); )
  {
    EntityType this_type = TYPE_FROM_HANDLE(*iter);
    if (this_type == MBENTITYSET) {
      ++iter;
      continue;
    }
    mMB->get_connectivity(*iter, conn, conn_size);
    // if this entity has mid edge nodes
    if(CN::HasMidEdgeNodes(this_type, conn_size))
    {
      // find out at which index the mid node should be at
      int first_node = std::find(conn, conn+conn_size, corner1) - conn;
      int second_node = std::find(conn, conn+conn_size, corner2) - conn;
      if(first_node == conn_size || second_node == conn_size)
        assert("We should always find our nodes no matter what" == NULL);
      int high_node_index = mNodeMap[this_type][first_node][second_node];
      if(conn[high_node_index] != 0)
        return conn[high_node_index];
      ++iter;
    }
    else
    {
      iter = adj_entities.erase(iter);
    }
  }

  return 0;

}

EntityHandle HigherOrderFactory::center_node_exist( EntityHandle corners[4], 
    std::vector<EntityHandle>& adj_entities)
{
  AEntityFactory* a_fact = mMB->a_entity_factory();
  std::vector<EntityHandle> adj_corner[4];
  int num_nodes = corners[3] == 0 ? 3 : 4;
  int i = 0;

  // create needed vertex adjacencies
  if (!a_fact->vert_elem_adjacencies())
    a_fact->create_vert_elem_adjacencies();

  // vectors are returned sorted
  for(i=0; i<num_nodes; i++)
    a_fact->get_adjacencies(corners[i], adj_corner[i]);

  // these are the entities adjacent to both nodes
  for(i=1; i<num_nodes; i++)
  {
    adj_entities.clear();
    std::set_intersection(adj_corner[i-1].begin(), adj_corner[i-1].end(), adj_corner[i].begin(),
      adj_corner[i].end(), std::back_inserter<std::vector<EntityHandle> >(adj_entities));
    adj_corner[i].swap(adj_entities);
  }
  adj_entities.swap(adj_corner[i-1]);
  

  // iterate of the entities to find a mid node
  const EntityHandle* conn;
  int conn_size = 0;
  for(std::vector<EntityHandle>::iterator iter = adj_entities.begin();
      iter != adj_entities.end(); )
  {
    EntityType this_type = TYPE_FROM_HANDLE(*iter);
    if (this_type == MBENTITYSET) {
      ++iter;
      continue;
    }
    const CN::ConnMap& entity_faces = CN::mConnectivityMap[this_type][1];
    mMB->get_connectivity(*iter, conn, conn_size);
    int offset = CN::VerticesPerEntity(this_type);
    if(CN::HasMidEdgeNodes(this_type, conn_size))
      offset += CN::mConnectivityMap[this_type][0].num_sub_elements;

    // if this entity has mid face nodes
    if(CN::HasMidFaceNodes(this_type, conn_size))
    {
      int k;
      int indexes[4];
      for(k=0; k<num_nodes; k++)
        indexes[k] = std::find(conn, conn+conn_size, corners[k]) - conn;
      
      // find out at which index the mid node should be at
      for(k=0; k<entity_faces.num_sub_elements; k++)
      {
        if(CN::VerticesPerEntity(entity_faces.target_type[k]) != num_nodes)
          continue;

        int* pivot = std::find(indexes, indexes+num_nodes, entity_faces.conn[k][0]);
        if(pivot == indexes+num_nodes)
          continue;

        if(pivot != indexes)
          std::rotate(indexes, pivot, indexes+num_nodes);

        if(std::equal(indexes, indexes+num_nodes, entity_faces.conn[k]))
        {
          if(conn[k+offset] != 0)
            return conn[k+offset];
          k=entity_faces.num_sub_elements;
        }
        else
        {
          int temp = indexes[1];
          indexes[1] = indexes[num_nodes-1];
          indexes[num_nodes-1] = temp;
          if(std::equal(indexes, indexes+num_nodes, entity_faces.conn[k]))
          {
            if(conn[k+offset] != 0)
              return conn[k+offset];
            k=entity_faces.num_sub_elements;
          }
        }
      }
      ++iter;
    }
    else
    {
      iter = adj_entities.erase(iter);
    }
  }

  return 0;

}

bool HigherOrderFactory::add_center_node(EntityType this_type, EntityHandle* element_conn, 
    int conn_size, EntityHandle corner_node1, EntityHandle corner_node2, 
    EntityHandle center_node)
{
  int first_node = std::find(element_conn, element_conn+conn_size, corner_node1) - element_conn;
  int second_node = std::find(element_conn, element_conn+conn_size, corner_node2) - element_conn;
  if(first_node == conn_size || second_node == conn_size)
    assert("We should always find our nodes no matter what" == NULL);
  int high_node_index = mNodeMap[this_type][first_node][second_node];
  element_conn[high_node_index] = center_node;  
  return true;
}

ErrorCode 
HigherOrderFactory::copy_corner_nodes( ElementSequence* src, ElementSequence* dst )
{
  unsigned num_corners = CN::VerticesPerEntity( src->type() );
  return copy_nodes( src, dst, num_corners, 0, 0 );
}

ErrorCode 
HigherOrderFactory::copy_mid_edge_nodes( ElementSequence* src, ElementSequence* dst )
{
  if (!src->has_mid_edge_nodes() || !dst->has_mid_edge_nodes())
    return MB_FAILURE;
  
  unsigned num_corners = CN::VerticesPerEntity( src->type() );
  unsigned num_edges = (src->type() == MBEDGE) ? 1 : CN::NumSubEntities( src->type(), 1 );
  return copy_nodes( src, dst, num_edges, num_corners, num_corners );
}

ErrorCode 
HigherOrderFactory::zero_mid_edge_nodes( ElementSequence* dst )
{
  if (!dst->has_mid_edge_nodes())
    return MB_FAILURE;
  
  unsigned num_corners = CN::VerticesPerEntity( dst->type() );
  unsigned num_edges = (dst->type() == MBEDGE) ? 1 : CN::NumSubEntities( dst->type(), 1 );
  return zero_nodes( dst, num_edges, num_corners );
}

ErrorCode 
HigherOrderFactory::copy_mid_face_nodes( ElementSequence* src, ElementSequence* dst )
{
  if (!src->has_mid_face_nodes() || !dst->has_mid_face_nodes())
    return MB_FAILURE;
  
  unsigned src_offset = CN::VerticesPerEntity( src->type() );
  unsigned dst_offset = src_offset;
  if (src->has_mid_edge_nodes())
    src_offset += CN::NumSubEntities( src->type(), 1 );
  if (dst->has_mid_edge_nodes())
    dst_offset += CN::NumSubEntities( dst->type(), 1 );
  unsigned num_faces = (CN::Dimension(src->type()) == 2) ? 1 : CN::NumSubEntities( src->type(), 2 );
  return copy_nodes( src, dst, num_faces, src_offset, dst_offset );
}

ErrorCode 
HigherOrderFactory::zero_mid_face_nodes( ElementSequence* dst )
{
  if (!dst->has_mid_face_nodes())
    return MB_FAILURE;
  
  unsigned dst_offset = CN::VerticesPerEntity( dst->type() );
  if (dst->has_mid_edge_nodes())
    dst_offset += CN::NumSubEntities( dst->type(), 1 );
  unsigned num_faces = (CN::Dimension(dst->type()) == 2) ? 1 : CN::NumSubEntities( dst->type(), 2 );
  return zero_nodes( dst, num_faces, dst_offset );
}


ErrorCode 
HigherOrderFactory::copy_mid_volume_nodes( ElementSequence* src, ElementSequence* dst )
{
  if (!src->has_mid_volume_nodes() || !dst->has_mid_volume_nodes())
    return MB_FAILURE;
  
  unsigned src_offset = CN::VerticesPerEntity( src->type() );
  unsigned dst_offset = src_offset;
  if (src->has_mid_edge_nodes())
    src_offset += CN::NumSubEntities( src->type(), 1 );
  if (dst->has_mid_edge_nodes())
    dst_offset += CN::NumSubEntities( dst->type(), 1 );
  if (src->has_mid_face_nodes())
    src_offset += CN::NumSubEntities( src->type(), 2 );
  if (dst->has_mid_face_nodes())
    dst_offset += CN::NumSubEntities( dst->type(), 2 );
  return copy_nodes( src, dst, 1, src_offset, dst_offset );
}

ErrorCode 
HigherOrderFactory::zero_mid_volume_nodes( ElementSequence* dst )
{
  if (!dst->has_mid_volume_nodes())
    return MB_FAILURE;
  
  unsigned dst_offset = CN::VerticesPerEntity( dst->type() );
  if (dst->has_mid_edge_nodes())
    dst_offset += CN::NumSubEntities( dst->type(), 1 );
  if (dst->has_mid_face_nodes())
    dst_offset += CN::NumSubEntities( dst->type(), 2 );
  return zero_nodes( dst, 1, dst_offset );
}

ErrorCode 
HigherOrderFactory::copy_nodes( ElementSequence* src,
                                ElementSequence* dst,
                                unsigned nodes_per_elem,
                                unsigned src_offset,
                                unsigned dst_offset )
{
  if (src->type() != dst->type())
    return MB_FAILURE;

  unsigned src_stride = src->nodes_per_element();
  unsigned dst_stride = dst->nodes_per_element();
  EntityHandle* src_conn = src->get_connectivity_array();
  EntityHandle* dst_conn = dst->get_connectivity_array();
  if (!src_conn || !dst_conn)
    return MB_FAILURE;
  
  if (dst->start_handle() < src->start_handle() ||
      dst->end_handle()   > src->end_handle())
    return MB_FAILURE;
  
  src_conn += (dst->start_handle() - src->start_handle()) * src_stride;
  EntityID count = dst->size();
  for (EntityID i = 0; i < count; ++i) {
    for (unsigned j = 0; j < nodes_per_elem; ++j)
      dst_conn[j+dst_offset] = src_conn[j+src_offset];
    src_conn += src_stride; 
    dst_conn += dst_stride;
  }
  
  return MB_SUCCESS;
}

ErrorCode 
HigherOrderFactory::zero_nodes(ElementSequence* dst,
                                unsigned nodes_per_elem,
                                unsigned offset )
{
  unsigned dst_stride = dst->nodes_per_element();
  EntityHandle* dst_conn = dst->get_connectivity_array();
  if (!dst_conn)
    return MB_FAILURE;
  
  EntityID count = dst->size();
  for (EntityID i = 0; i < count; ++i) {
    std::fill( dst_conn + offset, dst_conn + offset + nodes_per_elem, 0 );
    dst_conn += dst_stride;
  }
  
  return MB_SUCCESS;
}

ErrorCode 
HigherOrderFactory::remove_mid_edge_nodes( ElementSequence* seq, 
                                           EntityHandle start,
                                           EntityHandle end,
                                           Tag deletable_nodes )
{
  int count;
  int offset;
  if (seq->type() == MBEDGE) {
    count = 1;
    offset = 2;
  }
  else {
    count = CN::NumSubEntities( seq->type(), 1 );
    offset = CN::VerticesPerEntity( seq->type() );
  }
  
  return remove_ho_nodes( seq, start, end, count, offset, deletable_nodes );
}


ErrorCode 
HigherOrderFactory::remove_mid_face_nodes( ElementSequence* seq, 
                                           EntityHandle start,
                                           EntityHandle end,
                                           Tag deletable_nodes )
{
  int count;
  if (CN::Dimension(seq->type()) == 2)
    count = 1;
  else 
    count = CN::NumSubEntities( seq->type(), 2 );
  int offset = CN::VerticesPerEntity( seq->type() );
  if (seq->has_mid_edge_nodes())
    offset += CN::NumSubEntities( seq->type(), 1 );
  
  return remove_ho_nodes( seq, start, end, count, offset, deletable_nodes );
}

ErrorCode 
HigherOrderFactory::remove_mid_volume_nodes( ElementSequence* seq, 
                                             EntityHandle start,
                                             EntityHandle end,
                                             Tag deletable_nodes )
{
  int offset = CN::VerticesPerEntity( seq->type() );
  if (seq->has_mid_edge_nodes())
    offset += CN::NumSubEntities( seq->type(), 1 );
  if (seq->has_mid_face_nodes())
    offset += CN::NumSubEntities( seq->type(), 2 );
  
  return remove_ho_nodes( seq, start, end, 1, offset, deletable_nodes );
}

// Code mostly copied from old EntitySequence.cpp
// (ElementEntitySequence::convert_realloc & 
//  ElementEntitySequence::tag_for_deletion).
// Copyright from old EntitySequence.cpp:
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
ErrorCode 
HigherOrderFactory::remove_ho_nodes( ElementSequence* seq,
                                     EntityHandle start,
                                     EntityHandle end,
                                     int nodes_per_elem,
                                     int elem_conn_offset,
                                     Tag deletable_nodes )
{
  if (start < seq->start_handle() || end > seq->end_handle())
    return MB_ENTITY_NOT_FOUND;
  EntityHandle* array = seq->get_connectivity_array();
  if (!array)
    return MB_NOT_IMPLEMENTED;
   
  std::set<EntityHandle> nodes_processed;
  for (EntityHandle i = start; i <= end; ++i) {  // for each element
    for (int j = 0; j < nodes_per_elem; ++j) {  // for each HO node to remove
      const EntityID elem = (i - seq->start_handle()); // element index
      const int conn_idx = j + elem_conn_offset;
      const EntityID index = elem * seq->nodes_per_element() + conn_idx;
      if (array[index] && nodes_processed.insert( array[index] ).second) {
        if (tag_for_deletion( i, conn_idx, seq )) {
          unsigned char bit = 0x1;
          mMB->tag_set_data( deletable_nodes, &(array[index]), 1, &bit );
        }
      }
    }
  }
  
  return MB_SUCCESS;
}

bool
HigherOrderFactory::tag_for_deletion( EntityHandle parent_handle,
                                      int conn_index,
                                      ElementSequence* seq )
{
  //get type of this sequence
  EntityType this_type = seq->type();

  //get dimension of 'parent' element
  int this_dimension = mMB->dimension_from_handle( parent_handle );

  //tells us if higher order node is on 
  int dimension, side_number; 
  CN::HONodeParent( this_type, seq->nodes_per_element(),
                      conn_index, dimension, side_number );  

  //it MUST be a higher-order node
  bool delete_node = false;

  assert( dimension != -1 );
  assert( side_number != -1 );

  //could be a mid-volume/face/edge node on a hex/face/edge respectively
  //if so...delete it bc/ no one else owns it too
  std::vector<EntityHandle> connectivity;
  if( dimension == this_dimension && side_number == 0 )
    delete_node = true;
  else //the node could also be on a lower order entity of 'tmp_entity' 
  {
    //get 'side' of 'parent_handle' that node is on 
    EntityHandle target_entity = 0;
    mMB->side_element( parent_handle, dimension, side_number, target_entity );

    if( target_entity )
    {
      AEntityFactory *a_fact = mMB->a_entity_factory();
      EntityHandle low_meshset;
      int dum;
      low_meshset = CREATE_HANDLE(MBENTITYSET, 0, dum);

      //just get corner nodes of target_entity
      connectivity.clear();
      ErrorCode rval;
      rval = mMB->get_connectivity(&( target_entity), 1, connectivity, true );MB_CHK_ERR(rval);

      //for each node, get all common adjacencies of nodes in 'parent_handle' 
      std::vector<EntityHandle> adj_list_1, adj_list_2, adj_entities;
      a_fact->get_adjacencies(connectivity[0], adj_list_1);

      // remove meshsets
      adj_list_1.erase(std::remove_if(adj_list_1.begin(), adj_list_1.end(), 
           std::bind2nd(std::greater<EntityHandle>(),low_meshset)), adj_list_1.end());

      size_t i; 
      for( i=1; i<connectivity.size(); i++)
      {
        adj_list_2.clear();
        a_fact->get_adjacencies(connectivity[i], adj_list_2);

        // remove meshsets
        adj_list_2.erase(std::remove_if(adj_list_2.begin(), adj_list_2.end(), 
             std::bind2nd(std::greater<EntityHandle>(),low_meshset)), adj_list_2.end());
       
        //intersect the 2 lists 
        adj_entities.clear();
        std::set_intersection(adj_list_1.begin(), adj_list_1.end(), 
                              adj_list_2.begin(), adj_list_2.end(), 
                              std::back_inserter< std::vector<EntityHandle> >(adj_entities));
        adj_list_1.clear();
        adj_list_1 = adj_entities;
      } 

      assert( adj_entities.size() );  //has to have at least one adjacency 

      //see if node is in other elements, not in this sequence...if so, delete it 
      for( i=0; i<adj_entities.size(); i++)
      {
        if( adj_entities[i] >= seq->start_handle() &&
            adj_entities[i] <= seq->end_handle() )
        {
          delete_node = false;
          break;
        }
        else 
          delete_node = true;
      }             
    }
    else //there is no lower order entity that also contains node 
      delete_node = true;
  }

  return delete_node;
}
  
} // namespace moab

