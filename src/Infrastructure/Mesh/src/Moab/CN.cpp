/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include "moab/CN.hpp"
#include "MBCNArrays.hpp"
#include "MBCN.h"
#include <assert.h>
#include <string.h>

namespace moab {

const char *CN::entityTypeNames[] = {
    "Vertex",
    "Edge",
    "Tri",
    "Quad",
    "Polygon",
    "Tet",
    "Pyramid",
    "Prism",
    "Knife",
    "Hex",
    "Polyhedron",
    "EntitySet",
    "MaxType"
};

short int CN::numberBasis = 0;

const DimensionPair CN::TypeDimensionMap[] = 
{
    DimensionPair(MBVERTEX,   MBVERTEX), 
    DimensionPair(MBEDGE,     MBEDGE), 
    DimensionPair(MBTRI,     MBPOLYGON),
    DimensionPair(MBTET,     MBPOLYHEDRON),
    DimensionPair(MBENTITYSET, MBENTITYSET), 
    DimensionPair(MBMAXTYPE, MBMAXTYPE)
};

short CN::increasingInts[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
                                10,11,12,13,14,15,16,17,18,19,
                                20,21,22,23,24,25,26,27,28,29,
                                30,31,32,33,34,35,36,37,38,39 };

  //! set the basis of the numbering system; may or may not do things besides setting the
//! member variable
void CN::SetBasis(const int in_basis) 
{
  numberBasis = in_basis;
}

//! return a type for the given name
EntityType CN::EntityTypeFromName(const char *name)
{
  for (EntityType i = MBVERTEX; i < MBMAXTYPE; i++) {
    if (0 == strcmp(name, entityTypeNames[i]))
      return i;
  }
  
  return MBMAXTYPE;
}

void CN::SubEntityNodeIndices( const EntityType this_topo, 
                                 const int num_nodes,
                                 const int sub_dimension,
                                 const int sub_index,
                                 EntityType& subentity_topo,
                                 int& num_sub_entity_nodes,
                                 int sub_entity_conn[] )
{
    // If asked for a node, the special case...
  if (sub_dimension == 0) {
    assert( sub_index < num_nodes );
    subentity_topo = MBVERTEX;
    num_sub_entity_nodes = 1;
    sub_entity_conn[0] = sub_index;
    return;
  }
  
  const int ho_bits = HasMidNodes( this_topo, num_nodes );
  subentity_topo = SubEntityType(this_topo, sub_dimension, sub_index);
  num_sub_entity_nodes = VerticesPerEntity(subentity_topo);
  const short* corners = mConnectivityMap[this_topo][sub_dimension-1].conn[sub_index];
  std::copy( corners, corners+num_sub_entity_nodes, sub_entity_conn );
  
  int sub_sub_corners[MAX_SUB_ENTITY_VERTICES];
  int side, sense, offset;
  for (int dim = 1; dim <= sub_dimension; ++dim) {
    if (!(ho_bits & (1<<dim)))
      continue;
    
    const short num_mid = NumSubEntities( subentity_topo, dim );
    for (int i = 0; i < num_mid; ++i) {
      const EntityType sub_sub_topo = SubEntityType( subentity_topo, dim, i );
      const int sub_sub_num_vert = VerticesPerEntity( sub_sub_topo );
      SubEntityVertexIndices( subentity_topo, dim, i, sub_sub_corners );

      for (int j = 0; j < sub_sub_num_vert; ++j)
        sub_sub_corners[j] = corners[sub_sub_corners[j]];
      SideNumber( this_topo, sub_sub_corners, sub_sub_num_vert, dim, side, sense, offset );
      sub_entity_conn[num_sub_entity_nodes++] = HONodeIndex( this_topo, num_nodes, dim, side );
    }
  }
}


//! return the vertices of the specified sub entity
//! \param parent_conn Connectivity of parent entity
//! \param parent_type Entity type of parent entity
//! \param sub_dimension Dimension of sub-entity being queried
//! \param sub_index Index of sub-entity being queried
//! \param sub_entity_conn Connectivity of sub-entity, based on parent_conn and canonical
//!           ordering for parent_type
//! \param num_sub_vertices Number of vertices in sub-entity
void CN::SubEntityConn(const void *parent_conn, const EntityType parent_type,
                         const int sub_dimension,
                         const int sub_index,
                         void *sub_entity_conn, int &num_sub_vertices) 
{
  static int sub_indices[MAX_SUB_ENTITY_VERTICES];
  
  SubEntityVertexIndices(parent_type, sub_dimension, sub_index, sub_indices);
  
  num_sub_vertices = VerticesPerEntity(SubEntityType(parent_type, sub_dimension, sub_index));
  void **parent_conn_ptr = static_cast<void **>(const_cast<void *>(parent_conn));
  void **sub_conn_ptr = static_cast<void **>(sub_entity_conn);
  for (int i = 0; i < num_sub_vertices; i++)
    sub_conn_ptr[i] = parent_conn_ptr[sub_indices[i]];
}

//! given an entity and a target dimension & side number, get that entity
short int CN::AdjacentSubEntities(const EntityType this_type,
                              const int *source_indices,
                              const int num_source_indices,
                              const int source_dim,
                              const int target_dim,
                              std::vector<int> &index_list,
                              const int operation_type)
{
    // first get all the vertex indices
  std::vector<int> tmp_indices;
  const int* it1 = source_indices;

  assert(source_dim >= 0 && source_dim <= 3 &&
         target_dim >= 0 && target_dim <= 3 &&
           // make sure we're not stepping off the end of the array; 
         ((source_dim > 0 && 
           *it1 < mConnectivityMap[this_type][source_dim-1].num_sub_elements) ||
          (source_dim == 0 && 
           *it1 < mConnectivityMap[this_type][Dimension(this_type)-1].num_corners_per_sub_element[0])) && 
         *it1 >= 0);


#define MUC CN::mUpConnMap[this_type][source_dim][target_dim]

    // if we're looking for the vertices of a single side, return them in
    // the canonical ordering; otherwise, return them in sorted order
  if (num_source_indices == 1 && 0 == target_dim && source_dim != target_dim) {

      // element of mConnectivityMap should be for this type and for one
      // less than source_dim, which should give the connectivity of that sub element
    const ConnMap &cm = mConnectivityMap[this_type][source_dim-1];
    std::copy(cm.conn[source_indices[0]],
              cm.conn[source_indices[0]]+cm.num_corners_per_sub_element[source_indices[0]],
              std::back_inserter(index_list));
    return 0;
  }
              
    // now go through source indices, folding adjacencies into target list
  for (it1 = source_indices; it1 != source_indices+num_source_indices; it1++) {
      // *it1 is the side index
      // at start of iteration, index_list has the target list

      // if a union, or first iteration and index list was empty, copy the list
    if (operation_type == CN::UNION || 
        (it1 == source_indices && index_list.empty())) {
      std::copy(MUC.targets_per_source_element[*it1],
                MUC.targets_per_source_element[*it1]+
                MUC.num_targets_per_source_element[*it1],
                std::back_inserter(index_list));
    }
    else {
        // else we're intersecting, and have a non-empty list; intersect with this target list
      tmp_indices.clear();
      for (int i = MUC.num_targets_per_source_element[*it1]-1; i>= 0; i--)
        if (std::find(index_list.begin(), index_list.end(), MUC.targets_per_source_element[*it1][i]) !=
            index_list.end())
          tmp_indices.push_back(MUC.targets_per_source_element[*it1][i]);
//      std::set_intersection(MUC.targets_per_source_element[*it1],
//                            MUC.targets_per_source_element[*it1]+
//                            MUC.num_targets_per_source_element[*it1],
//                            index_list.begin(), index_list.end(),
//                            std::back_inserter(tmp_indices));
      index_list.swap(tmp_indices);

        // if we're at this point and the list is empty, the intersection will be NULL;
        // return if so
      if (index_list.empty()) return 0;
    }
  }
  
  if (operation_type == CN::UNION && num_source_indices != 1) {
      // need to sort then unique the list
    std::sort(index_list.begin(), index_list.end());
    index_list.erase(std::unique(index_list.begin(), index_list.end()), 
                     index_list.end());
  }
  
  return 0;
}

template <typename T> static 
short int side_number(const T *parent_conn, 
                const EntityType parent_type,
                const T *child_conn,
                const int child_num_verts,
                const int child_dim,
                int &side_no,
                int &sense,
                int &offset)
{
  int parent_num_verts = CN::VerticesPerEntity(parent_type);
  int side_indices[8]; 
  assert(sizeof(side_indices)/sizeof(side_indices[0]) >= (size_t)child_num_verts);
  
  for (int i = 0; i < child_num_verts; i++) {
    side_indices[i] = std::find(parent_conn, parent_conn+parent_num_verts, child_conn[i]) - parent_conn;
    if (side_indices[i] == parent_num_verts) 
      return -1;
  }
  
  return CN::SideNumber(parent_type, &side_indices[0], child_num_verts,
                    child_dim, side_no, sense, offset);
}

short int CN::SideNumber(const EntityType parent_type, const int *parent_conn, 
                     const int *child_conn, const int child_num_verts,
                     const int child_dim,
                     int &side_no, int &sense, int &offset) 
{
  return side_number(parent_conn, parent_type, child_conn, child_num_verts,
                     child_dim, side_no, sense, offset);
}

short int CN::SideNumber(const EntityType parent_type, const unsigned int *parent_conn, 
                     const unsigned int *child_conn, const int child_num_verts,
                     const int child_dim,
                     int &side_no, int &sense, int &offset)
{
  return side_number(parent_conn, parent_type, child_conn, child_num_verts,
                     child_dim, side_no, sense, offset);
}
short int CN::SideNumber(const EntityType parent_type, const long *parent_conn, 
                     const long *child_conn, const int child_num_verts,
                     const int child_dim,
                     int &side_no, int &sense, int &offset)
{
  return side_number(parent_conn, parent_type, child_conn, child_num_verts,
                     child_dim, side_no, sense, offset);
}
short int CN::SideNumber(const EntityType parent_type, const unsigned long *parent_conn, 
                     const unsigned long *child_conn, const int child_num_verts,
                     const int child_dim,
                     int &side_no, int &sense, int &offset)
{
  return side_number(parent_conn, parent_type, child_conn, child_num_verts,
                     child_dim, side_no, sense, offset);
}
short int CN::SideNumber(const EntityType parent_type, void * const *parent_conn, 
                     void * const *child_conn, const int child_num_verts,
                     const int child_dim,
                     int &side_no, int &sense, int &offset)
{
  return side_number(parent_conn, parent_type, child_conn, child_num_verts,
                     child_dim, side_no, sense, offset);
}

short int CN::SideNumber( const EntityType parent_type,
                      const int *child_conn_indices,
                      const int child_num_verts,
                      const int child_dim,
                      int &side_no,
                      int &sense,
                      int &offset )
{
  int parent_dim = Dimension(parent_type);
  int parent_num_verts = VerticesPerEntity(parent_type);

    // degenerate case (vertex), output == input
  if (child_dim == 0) {
    if (child_num_verts != 1)
      return -1;
    side_no = *child_conn_indices;
    sense = offset = 0;
  }
    
    // given a parent and child element, find the corresponding side number

    // dim_diff should be -1, 0 or 1 (same dimension, one less dimension, two less, resp.)
  if (child_dim > parent_dim || child_dim < 0)
    return -1;

    // different types of same dimension won't be the same
  if (parent_dim == child_dim &&
      parent_num_verts != child_num_verts) {
    side_no = -1;
    sense = 0;
    return 0;
  }

    // loop over the sub-elements, comparing to child connectivity
  int sub_conn_indices[10];
  for (int i = 0; i < NumSubEntities(parent_type, child_dim); i++) {
    int sub_size = VerticesPerEntity(SubEntityType(parent_type, child_dim, i));
    if (sub_size != child_num_verts) 
      continue;

    SubEntityVertexIndices(parent_type, child_dim, i, sub_conn_indices);
    bool they_match = ConnectivityMatch(child_conn_indices, 
                                        sub_conn_indices, sub_size, 
                                        sense, offset);
    if (they_match) {
      side_no = i;
      return 0;
    }
  }

    // if we've gotten here, we don't match
  side_no = -1;

    // return value is still success, we didn't have any fatal errors or anything
  return 0;
}

  //! return the dimension and index of the opposite side, given parent entity type and child 
  //! dimension and index.  This function is only defined for certain types of parent/child types:
  //! (Parent, Child dim->Opposite dim): 
  //!  (Tri, 1->0), (Tri, 0->1), (Quad, 1->1), (Quad, 0->0), 
  //!  (Tet, 2->0), (Tet, 1->1), (Tet, 0->2),
  //!  (Hex, 2->2), (Hex, 1->1)(diagonally across element), (Hex, 0->0) (diagonally across element)
  //! All other parent types and child dimensions return an error.
  //!
  //! \param parent_type The type of parent element
  //! \param child_type The type of child element
  //! \param child_index The index of the child element
  //! \param opposite_index The index of the opposite element
  //! \return status Returns 0 if successful, -1 if not
short int CN::OppositeSide(const EntityType parent_type,
                       const int child_index,
                       const int child_dim,
                       int &opposite_index,
                       int &opposite_dim) 
{
  switch (parent_type) {
    case MBEDGE:
        if (0 != child_dim) return -1;
        else opposite_index = 1-child_index;
        opposite_dim = 0;
        break;
      
    case MBTRI:
        switch (child_dim) {
          case 0:
              opposite_dim = 1;
              opposite_index = (child_index+1)%3;
              break;
          case 1:
              opposite_dim = 0;
              opposite_index = (child_index+2)%3;
              break;
          default:
              return -1;
        }
        break;

    case MBQUAD:
        switch (child_dim) {
          case 0:
          case 1:
              opposite_dim = child_dim;
              opposite_index = (child_index+2)%4;
              break;
          default:
              return -1;
        }
        break;
      
    case MBTET:
        switch (child_dim) {
          case 0:
              opposite_dim = 2;
              opposite_index = (child_index+1)%3 + 2*(child_index/3);
              break;
          case 1:
              opposite_dim = 1;
              opposite_index = child_index < 3 
                             ? 3 + (child_index + 2)%3
                             : (child_index + 1)%3;
              break;
          case 2:
              opposite_dim = 0;
              opposite_index = (child_index+2)%3 + child_index/3;
              break;
          default:
              return -1;
        }
        break;
    case MBHEX:
      opposite_dim = child_dim;
      switch (child_dim) {
        case 0:
          opposite_index = child_index < 4 
                         ? 4 + (child_index + 2) % 4
                         : (child_index - 2) % 4;
          break;
        case 1:
          opposite_index = 4*(2-child_index/4) + (child_index+2)%4;
          break;
        case 2:
          opposite_index = child_index < 4 
                         ? (child_index + 2) % 4
                         : 9 - child_index;
          break;
        default:
          return -1;
      }
      break;
        
      
    default:
        return -1;
  }
  
  return 0;
}

template <typename T> 
inline bool connectivity_match( const T* conn1_i,
                                       const T* conn2_i,
                                       const int num_vertices,
                                       int& direct, int& offset )
{

  bool they_match;
  
    // special test for 2 handles, since we don't want to wrap the list in this
    // case
  if (num_vertices == 2) {
    they_match = false;
    if (conn1_i[0] == conn2_i[0] && conn1_i[1] == conn2_i[1]) {
      direct = 1;
      they_match = true;
      offset = 0;
    }
    else if (conn1_i[0] == conn2_i[1] && conn1_i[1] == conn2_i[0]) {
      they_match = true;
      direct = -1;
      offset = 1;
    }
  }

  else {
    const T *iter;
    iter = std::find(&conn2_i[0], &conn2_i[num_vertices], conn1_i[0]);
    if(iter == &conn2_i[num_vertices])
      return false;

    they_match = true;

    offset = iter - conn2_i;
    int i;

      // first compare forward
    for(i = 1; i<num_vertices; ++i)
    {
      if(conn1_i[i] != conn2_i[(offset+i)%num_vertices])
      {
        they_match = false;
        break;
      }
    }
  
    if(they_match == true)
    {
      direct = 1;
      return they_match;
    }
  
    they_match = true;
  
      // then compare reverse
    for(i = 1; i<num_vertices; i++)
    {
      if(conn1_i[i] != conn2_i[(offset+num_vertices-i)%num_vertices])
      {
        they_match = false;
        break;
      }
    }
    if (they_match)
    {
      direct = -1;
    }
  }

  return they_match;
}


bool CN::ConnectivityMatch( const int *conn1_i,
                              const int *conn2_i,
                              const int num_vertices,
                              int &direct, int &offset )
{
  return connectivity_match<int>(conn1_i, conn2_i, num_vertices, direct, offset );
}

bool CN::ConnectivityMatch( const unsigned int *conn1_i,
                              const unsigned int *conn2_i,
                              const int num_vertices,
                              int &direct, int &offset )
{
  return connectivity_match<unsigned int>(conn1_i, conn2_i, num_vertices, direct, offset );
}

bool CN::ConnectivityMatch( const long *conn1_i,
                              const long *conn2_i,
                              const int num_vertices,
                              int &direct, int &offset )
{
  return connectivity_match<long>(conn1_i, conn2_i, num_vertices, direct, offset );
}

bool CN::ConnectivityMatch( const unsigned long *conn1_i,
                              const unsigned long *conn2_i,
                              const int num_vertices,
                              int &direct, int &offset )
{
  return connectivity_match<unsigned long>(conn1_i, conn2_i, num_vertices, direct, offset );
}

bool CN::ConnectivityMatch( void* const *conn1_i,
                              void* const *conn2_i,
                              const int num_vertices,
                              int &direct, int &offset )
{
  return connectivity_match<void*>(conn1_i, conn2_i, num_vertices, direct, offset );
}



  //! for an entity of this type and a specified subfacet (dimension and index), return
  //! the index of the higher order node for that entity in this entity's connectivity array
short int CN::HONodeIndex(const EntityType this_type, const int num_verts,
                      const int subfacet_dim, const int subfacet_index) 
{
  int i;
  int has_mids[4];
  HasMidNodes(this_type, num_verts, has_mids);

    // if we have no mid nodes on the subfacet_dim, we have no index
  if (subfacet_index != -1 && !has_mids[subfacet_dim]) return -1;

    // put start index at last index (one less than the number of vertices 
    // plus the index basis)
  int index = VerticesPerEntity(this_type) - 1 + numberBasis;

    // for each subfacet dimension less than the target subfacet dim which has mid nodes, 
    // add the number of subfacets of that dimension to the index
  for (i = 1; i < subfacet_dim; i++)
    if (has_mids[i]) index += NumSubEntities(this_type, i);
    

    // now add the index of this subfacet, or one if we're asking about the entity as a whole
  if (subfacet_index == -1 && has_mids[subfacet_dim])
      // want the index of the last ho node on this subfacet
    index += NumSubEntities(this_type, subfacet_dim);
  
  else if (subfacet_index != -1 && has_mids[subfacet_dim])
    index += subfacet_index + 1 - numberBasis;

    // that's it
  return index;
}

  //! given data about an element and a vertex in that element, return the dimension
  //! and index of the sub-entity that the vertex resolves.  If it does not resolve a
  //! sub-entity, either because it's a corner node or it's not in the element, -1 is
  //! returned in both return values
void CN::HONodeParent( EntityType elem_type,
                         int num_verts, 
                         int ho_index,
                         int& parent_dim,
                         int& parent_index )
{
    // begin with error values
  parent_dim = parent_index = -1;
     
    // given the number of verts and the element type, get the hasmidnodes solution
  int has_mids[4];
  HasMidNodes(elem_type, num_verts, has_mids);

  int index = VerticesPerEntity(elem_type)-1;
  const int dim = Dimension(elem_type);

    // keep a running sum of the ho node indices for this type of element, and stop
    // when you get to the dimension which has the ho node
  for (int i = 1; i < dim; i++) {
    if (has_mids[i]) {
      if (ho_index <= index + NumSubEntities(elem_type, i)) {
          // the ho_index resolves an entity of dimension i, so set the return values
          // and break out of the loop
        parent_dim = i;
        parent_index = ho_index - index - 1;
        return;
      }
      else {
        index += NumSubEntities(elem_type, i);
      } 
    }
  }
  
    // mid region node case  
  if( has_mids[dim] && ho_index == index+1 ) {
    parent_dim = dim;
    parent_index = 0; 
  }
}

} // namespace moab


using moab::CN;
using moab::EntityType;

  //! get the basis of the numbering system
void MBCN_GetBasis(int *rval) {*rval = CN::GetBasis();}
  
  //! set the basis of the numbering system
void MBCN_SetBasis(const int in_basis) {CN::SetBasis(in_basis);}

  //! return the string type name for this type
void MBCN_EntityTypeName(const int this_type, char *rval, int rval_len) 
{
  const char *rval_tmp = CN::EntityTypeName((EntityType)this_type);
  int rval_len_tmp = strlen(rval_tmp);
  rval_len_tmp = (rval_len_tmp < rval_len ? rval_len_tmp : rval_len);
  strncpy(rval, rval_tmp, rval_len_tmp);
}
  
  //! given a name, find the corresponding entity type
void MBCN_EntityTypeFromName(const char *name, int *rval) 
{
  *rval = CN::EntityTypeFromName(name);
}
  
  //! return the topological entity dimension
void MBCN_Dimension(const int t, int *rval) 
{
  *rval = CN::Dimension((EntityType)t);
}

  //! return the number of (corner) vertices contained in the specified type.  
void MBCN_VerticesPerEntity(const int t, int *rval) 
{
  *rval = CN::VerticesPerEntity((EntityType)t);
}
  
  //! return the number of sub-entities bounding the entity.
void MBCN_NumSubEntities(const int t, const int d, int *rval) 
{
  *rval = CN::NumSubEntities((EntityType)t, d);
}

  //! return the type of a particular sub-entity.
  //! \param this_type Type of entity for which sub-entity type is being queried
  //! \param sub_dimension Topological dimension of sub-entity whose type is being queried
  //! \param index Index of sub-entity whose type is being queried
  //! \return type Entity type of sub-entity with specified dimension and index
void MBCN_SubEntityType(const int this_type,
                        const int sub_dimension,
                        const int index, int *rval) 

{
  
  *rval = CN::SubEntityType((EntityType)this_type, sub_dimension, index);

}

  
  //! return the vertex indices of the specified sub-entity.
  //! \param this_type Type of entity for which sub-entity connectivity is being queried
  //! \param sub_dimension Dimension of sub-entity
  //! \param sub_index Index of sub-entity
  //! \param sub_entity_conn Connectivity of sub-entity (returned to calling function)
void MBCN_SubEntityVertexIndices(const int this_type, 
                                 const int sub_dimension,
                                 const int sub_index,
                                 int sub_entity_conn[]) 
{
  CN::SubEntityVertexIndices((EntityType)this_type, sub_dimension, 
                               sub_index, sub_entity_conn);
}

  //! return the vertices of the specified sub entity
  //! \param parent_conn Connectivity of parent entity
  //! \param parent_type Entity type of parent entity
  //! \param sub_dimension Dimension of sub-entity being queried
  //! \param sub_index Index of sub-entity being queried
  //! \param sub_entity_conn Connectivity of sub-entity, based on parent_conn and canonical
  //!           ordering for parent_type
  //! \param num_sub_vertices Number of vertices in sub-entity
//  void MBCN_SubEntityConn(const void *parent_conn, const int parent_type,
//                            const int sub_dimension,
//                            const int sub_index,
//                            void *sub_entity_conn, int &num_sub_vertices) {return CN::SubEntityConn();}

  //! For a specified set of sides of given dimension, return the intersection 
  //! or union of all sides of specified target dimension adjacent to those sides.
  //! \param this_type Type of entity for which sub-entity connectivity is being queried
  //! \param source_indices Indices of sides being queried
  //! \param num_source_indices Number of entries in <em>source_indices</em>
  //! \param source_dim Dimension of source entity
  //! \param target_dim Dimension of target entity
  //! \param index_list Indices of target entities (returned)
  //! \param operation_type Specify either CN::INTERSECT (0) or CN::UNION (1) to get intersection
  //!        or union of target entity lists over source entities
void MBCN_AdjacentSubEntities(const int this_type,
                              const int *source_indices,
                              const int num_source_indices,
                              const int source_dim,
                              const int target_dim,
                              int *index_list,
                              int *num_indices,
                              const int operation_type, int *rval) 
{
  std::vector<int> tmp_index_list;
  *rval = CN::AdjacentSubEntities((EntityType)this_type, source_indices, 
                                    num_source_indices, source_dim, target_dim, 
                                    tmp_index_list, operation_type);
  std::copy(tmp_index_list.begin(), tmp_index_list.end(), index_list);
  *num_indices = tmp_index_list.size();
}

  //! return the side index represented in the input sub-entity connectivity
  //! \param parent_type Entity type of parent entity
  //! \param child_conn_indices Child connectivity to query, specified as indices
  //!                           into the connectivity list of the parent.
  //! \param child_num_verts Number of values in <em>child_conn_indices</em>
  //! \param child_dim Dimension of child entity being queried
  //! \param side_no Side number of child entity (returned)
  //! \param sense Sense of child entity with respect to order in <em>child_conn</em> (returned)
  //! \param offset Offset of <em>child_conn</em> with respect to canonical ordering data (returned)
  //! \return status Returns zero if successful, -1 if not
void MBCN_SideNumber(const int parent_type,
                     const int *child_conn_indices, const int child_num_verts,
                     const int child_dim,
                     int *side_no, int *sense, int *offset) 
{
  CN::SideNumber((EntityType)parent_type, child_conn_indices, child_num_verts, child_dim,
                   *side_no, *sense, *offset);
}

void MBCN_SideNumberInt(const int *parent_conn, const EntityType parent_type,
                        const int *child_conn, const int child_num_verts,
                        const int child_dim,
                        int *side_no, int *sense, int *offset) 
{
  moab::side_number(parent_conn, parent_type, child_conn, child_num_verts, 
              child_dim, *side_no, *sense, *offset);
}

void MBCN_SideNumberUint(const unsigned int *parent_conn, const EntityType parent_type,
                         const unsigned int *child_conn, const int child_num_verts,
                         const int child_dim,
                         int *side_no, int *sense, int *offset)
{
  moab::side_number(parent_conn, parent_type, child_conn, child_num_verts, 
              child_dim, *side_no, *sense, *offset);
}

void MBCN_SideNumberLong(const long *parent_conn, const EntityType parent_type,
                         const long *child_conn, const int child_num_verts,
                         const int child_dim,
                         int *side_no, int *sense, int *offset)
{
  moab::side_number(parent_conn, parent_type, child_conn, child_num_verts, 
              child_dim, *side_no, *sense, *offset);
}

void MBCN_SideNumberUlong(const unsigned long *parent_conn, const EntityType parent_type,
                          const unsigned long *child_conn, const int child_num_verts,
                          const int child_dim,
                          int *side_no, int *sense, int *offset)
{
  moab::side_number(parent_conn, parent_type, child_conn, child_num_verts, 
              child_dim, *side_no, *sense, *offset);
}

void MBCN_SideNumberVoid(void * const *parent_conn, const EntityType parent_type,
                         void * const *child_conn, const int child_num_verts,
                         const int child_dim,
                         int *side_no, int *sense, int *offset)
{
  moab::side_number(parent_conn, parent_type, child_conn, child_num_verts, 
              child_dim, *side_no, *sense, *offset);
}

  //! return the dimension and index of the opposite side, given parent entity type and child 
  //! dimension and index.  This function is only defined for certain types of parent/child types:
  //! (Parent, Child dim->Opposite dim): 
  //!  (Tri, 1->0), (Tri, 0->1), (Quad, 1->1), (Quad, 0->0), 
  //!  (Tet, 2->0), (Tet, 1->1), (Tet, 0->2),
  //!  (Hex, 2->2), (Hex, 1->1)(diagonally across element), (Hex, 0->0) (diagonally across element)
  //! All other parent types and child dimensions return an error.
  //!
  //! \param parent_type The type of parent element
  //! \param child_type The type of child element
  //! \param child_index The index of the child element
  //! \param opposite_index The index of the opposite element
  //! \return status Returns 0 if successful, -1 if not
void MBCN_OppositeSide(const int parent_type,
                       const int child_index,
                       const int child_dim,
                       int *opposite_index,
                       int *opposite_dim, int *rval) 
{
  *rval = CN::OppositeSide((EntityType)parent_type, child_index, child_dim, 
                             *opposite_index, *opposite_dim);
}

  //! given two connectivity arrays, determine whether or not they represent the same entity.
  //! \param conn1 Connectivity array of first entity
  //! \param conn2 Connectivity array of second entity
  //! \param num_vertices Number of entries in <em>conn1</em> and <em>conn2</em>
  //! \param direct If positive, entities have the same sense (returned)
  //! \param offset Offset of <em>conn2</em>'s first vertex in <em>conn1</em>
  //! \return int Returns true if <em>conn1</em> and <em>conn2</em> match
void MBCN_ConnectivityMatchInt(const int *conn1,
                               const int *conn2,
                               const int num_vertices,
                               int *direct, int *offset, int *rval) 
{
  *rval = CN::ConnectivityMatch(conn1, conn2, num_vertices, 
                                  *direct, *offset);
}

void MBCN_ConnectivityMatchUint(const unsigned int *conn1,
                                const unsigned int *conn2,
                                const int num_vertices,
                                int *direct, int *offset, int *rval) 
{
  *rval = CN::ConnectivityMatch(conn1, conn2, num_vertices, 
                                  *direct, *offset);
}

void MBCN_ConnectivityMatchLong(const long* conn1,
                                const long* conn2,
                                const int num_vertices,
                                int* direct, int* offset , int *rval) 
{
  *rval = CN::ConnectivityMatch(conn1, conn2, num_vertices, 
                                  *direct, *offset);
}

void MBCN_ConnectivityMatchUlong(const unsigned long* conn1,
                                 const unsigned long* conn2,
                                 const int num_vertices,
                                 int *direct, int* offset , int *rval) 
{
  *rval = CN::ConnectivityMatch(conn1, conn2, num_vertices, 
                                  *direct, *offset);
}

void MBCN_ConnectivityMatchVoid(void* const* conn1,
                                void* const* conn2,
                                const int num_vertices,
                                int* direct, int* offset , int *rval) 
{
  *rval = CN::ConnectivityMatch(conn1, conn2, num_vertices, 
                                  *direct, *offset);
}

  //! true if entities of a given type and number of nodes indicates mid edge nodes are present.
  //! \param this_type Type of entity for which sub-entity connectivity is being queried
  //! \param num_verts Number of nodes defining entity
  //! \return int Returns true if <em>this_type</em> combined with <em>num_nodes</em> indicates
  //!  mid-edge nodes are likely
void MBCN_HasMidEdgeNodes(const int this_type, 
                          const int num_verts, int *rval) 
{
  *rval = CN::HasMidEdgeNodes((EntityType)this_type, num_verts);
}

  //! true if entities of a given type and number of nodes indicates mid face nodes are present.
  //! \param this_type Type of entity for which sub-entity connectivity is being queried
  //! \param num_verts Number of nodes defining entity
  //! \return int Returns true if <em>this_type</em> combined with <em>num_nodes</em> indicates
  //!  mid-face nodes are likely
void MBCN_HasMidFaceNodes(const int this_type, 
                          const int num_verts, int *rval) 
{
  *rval = CN::HasMidFaceNodes((EntityType)this_type, num_verts);
}

  //! true if entities of a given type and number of nodes indicates mid region nodes are present.
  //! \param this_type Type of entity for which sub-entity connectivity is being queried
  //! \param num_verts Number of nodes defining entity
  //! \return int Returns true if <em>this_type</em> combined with <em>num_nodes</em> indicates
  //!  mid-region nodes are likely
void MBCN_HasMidRegionNodes(const int this_type, 
                            const int num_verts, int *rval) 
{
  *rval = CN::HasMidRegionNodes((EntityType)this_type, num_verts);
}

  //! true if entities of a given type and number of nodes indicates mid edge/face/region nodes 
  //! are present.
  //! \param this_type Type of entity for which sub-entity connectivity is being queried
  //! \param num_verts Number of nodes defining entity
  //! \param mid_nodes If <em>mid_nodes[i], i=1..3</em> is true, indicates that mid-edge 
  //!    (i=1), mid-face (i=2), and/or mid-region (i=3) nodes are likely
void MBCN_HasMidNodes(const int this_type, 
                      const int num_verts, 
                      int mid_nodes[4]) 
{
  return CN::HasMidNodes((EntityType)this_type, num_verts, mid_nodes);
}

  //! given data about an element and a vertex in that element, return the dimension
  //! and index of the sub-entity that the vertex resolves.  If it does not resolve a
  //! sub-entity, either because it's a corner node or it's not in the element, -1 is
  //! returned in both return values.
  //! \param elem_type Type of entity being queried
  //! \param num_nodes The number of nodes in the element connectivity
  //! \param ho_node_index The position of the HO node in the connectivity list (zero based)
  //! \param parent_dim Dimension of sub-entity high-order node resolves (returned)
  //! \param parent_index Index of sub-entity high-order node resolves (returned)
void MBCN_HONodeParent( int elem_type,
                        int num_nodes, 
                        int ho_node_index,
                        int *parent_dim, 
                        int *parent_index ) 
{
  return CN::HONodeParent((EntityType)elem_type, num_nodes, ho_node_index, 
                            *parent_dim, *parent_index);
}

  //! for an entity of this type with num_verts vertices, and a specified subfacet 
  //! (dimension and index), return the index of the higher order node for that entity 
  //! in this entity's connectivity array
  //! \param this_type Type of entity being queried
  //! \param num_verts Number of vertices for the entity being queried
  //! \param subfacet_dim Dimension of sub-entity being queried
  //! \param subfacet_index Index of sub-entity being queried
  //! \return index Index of sub-entity's higher-order node
void MBCN_HONodeIndex(const int this_type, const int num_verts,
                      const int subfacet_dim, const int subfacet_index, int *rval) 

{
  
  *rval = CN::HONodeIndex((EntityType)this_type, num_verts, subfacet_dim, subfacet_index);

}

namespace moab {

template <typename T> 
inline int permute_this(EntityType t,
                        const int dim,
                        T* conn,
                        const int indices_per_ent,
                        const int num_entries) 
{
  T tmp_conn[MAX_SUB_ENTITIES];
  assert(indices_per_ent <= CN::permuteVec[t][dim][MAX_SUB_ENTITIES]);
  if (indices_per_ent > CN::permuteVec[t][dim][MAX_SUB_ENTITIES]) return 1;
  short int *tvec = CN::permuteVec[t][dim];
  T *pvec = conn;
  for (int j = 0; j < num_entries; j++) {
    for (int i = 0; i < indices_per_ent; i++)
      tmp_conn[tvec[i]] = pvec[i];
    memcpy(pvec, tmp_conn, indices_per_ent*sizeof(T));
    pvec += indices_per_ent;
  }

  return 0;
}

template <typename T> 
inline int rev_permute_this(EntityType t,
                            const int dim,
                            T* conn,
                            const int indices_per_ent,
                            const int num_entries) 
{
  T tmp_conn[MAX_SUB_ENTITIES];
  assert(indices_per_ent <= CN::revPermuteVec[t][dim][MAX_SUB_ENTITIES]);
  if (indices_per_ent > CN::revPermuteVec[t][dim][MAX_SUB_ENTITIES]) return 1;
  short int *tvec = CN::revPermuteVec[t][dim];
  T *pvec = conn;
  for (int j = 0; j < num_entries; j++) {
    for (int i = 0; i < indices_per_ent; i++)
      tmp_conn[i] = pvec[tvec[i]];
    memcpy(pvec, tmp_conn, indices_per_ent*sizeof(T));
    pvec += indices_per_ent;
  }

  return 0;
}

//! Permute this vector
inline int CN::permuteThis(const EntityType t, const int dim, int *pvec, 
                             const int num_indices, const int num_entries) 
{return permute_this(t, dim, pvec, num_indices, num_entries);}
inline int CN::permuteThis(const EntityType t, const int dim, unsigned int *pvec, 
                             const int num_indices, const int num_entries) 
{return permute_this(t, dim, pvec, num_indices, num_entries);}
inline int CN::permuteThis(const EntityType t, const int dim, long *pvec, 
                             const int num_indices, const int num_entries) 
{return permute_this(t, dim, pvec, num_indices, num_entries);}
inline int CN::permuteThis(const EntityType t, const int dim, void **pvec, 
                             const int num_indices, const int num_entries) 
{return permute_this(t, dim, pvec, num_indices, num_entries);}

//! Reverse permute this vector
inline int CN::revPermuteThis(const EntityType t, const int dim, int *pvec, 
                             const int num_indices, const int num_entries) 
{return rev_permute_this(t, dim, pvec, num_indices, num_entries);}
inline int CN::revPermuteThis(const EntityType t, const int dim, unsigned int *pvec, 
                             const int num_indices, const int num_entries) 
{return rev_permute_this(t, dim, pvec, num_indices, num_entries);}
inline int CN::revPermuteThis(const EntityType t, const int dim, long *pvec, 
                             const int num_indices, const int num_entries) 
{return rev_permute_this(t, dim, pvec, num_indices, num_entries);}
inline int CN::revPermuteThis(const EntityType t, const int dim, void **pvec, 
                             const int num_indices, const int num_entries) 
{return rev_permute_this(t, dim, pvec, num_indices, num_entries);}

  
} // namespace moab
