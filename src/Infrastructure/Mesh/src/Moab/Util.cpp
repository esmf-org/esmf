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

//-------------------------------------------------------------------------
// Filename      : Util.cpp
//
// Purpose       : This file contains utility functions that can be used
//                 with MB
//
// Special Notes : This is a pure virtual class, to prevent instantiation.
//                 All functions are static, called like this:
//                 Util::function_name();
//
// Creator       : Ray J. Meyers
//
// Date          : 09/01/02
//
// Owner         : Ray J. meyers
//-------------------------------------------------------------------------

#include "moab/Util.hpp"
#include "moab/Interface.hpp"
#include <assert.h>
#include <math.h>
#include <algorithm>
#include <limits>

namespace moab {

//! temporary normal function for MBEntities.  This should be moved to
//! an appropriate MB algorithms file

void Util::normal(Interface* MB, EntityHandle handle, double& x, double& y, double& z)
{

   // get connectivity
   const EntityHandle *connectivity;
   int number_nodes = 0;
   MB->get_connectivity(handle, connectivity, number_nodes, true);
   assert(number_nodes >= 3);

   // get_coordinates
   double coords[3][3];
   MB->get_coords(&(connectivity[0]), 1, coords[0]);
   MB->get_coords(&(connectivity[1]), 1, coords[1]);
   MB->get_coords(&(connectivity[2]), 1, coords[2]);

   double vecs[2][3];
   vecs[0][0] = coords[1][0] - coords[0][0];
   vecs[0][1] = coords[1][1] - coords[0][1];
   vecs[0][2] = coords[1][2] - coords[0][2];
   vecs[1][0] = coords[2][0] - coords[0][0];
   vecs[1][1] = coords[2][1] - coords[0][1];
   vecs[1][2] = coords[2][2] - coords[0][2];

   x = vecs[0][1] * vecs[1][2] - vecs[0][2] * vecs[1][1];
   y = vecs[0][2] * vecs[1][0] - vecs[0][0] * vecs[1][2];
   z = vecs[0][0] * vecs[1][1] - vecs[0][1] * vecs[1][0];

   double mag = sqrt(x*x + y*y + z*z);
   if(mag != std::numeric_limits<double>::epsilon())
   {
     x /= mag;
     y /= mag;
     z /= mag;
   }
}
void Util::centroid(Interface *MB, EntityHandle handle, Coord &coord)
{
   const EntityHandle *connectivity;
   int number_nodes = 0;
   MB->get_connectivity(handle, connectivity, number_nodes,true);
   
   coord.x=0.0;
   coord.y=0.0;
   coord.z=0.0;

   for(int i = 0; i< number_nodes; i++)
   {
      double node_coords[3];
      MB->get_coords(&(connectivity[i]), 1, node_coords);
     
      coord.x+=node_coords[0];
      coord.y+=node_coords[1];
      coord.z+=node_coords[2];
   }
   
   coord.x/=(double)number_nodes;
   coord.y/=(double)number_nodes;
   coord.z/=(double)number_nodes;
}

/*//This function calculates the coordinates for the centers of each edges of the entity specified by handle. The coordinates are returned in the list coords_list
void Util::edge_centers(Interface *MB, EntityHandle handle, std::vector<Coord> &coords_list)
{
  MB canon_tool(MB);
  EntityType type;
  int i = 0;
  int number_nodes = 0;
  double coords[2][3];
  const EntityHandle *connectivity;


  MB->get_connectivity(handle, connectivity, number_nodes,true);
  
  MB->type_from_handle(handle,type);
  
  const struct MBCN::ConnMap* conn_map = &(canon_tool.mConnectivityMap[type][0]); //get edge sub_elements

  coords_list.resize(conn_map->num_sub_elements);

  for(i = 0; i<conn_map->num_sub_elements; i++)
  {
    
    MB->get_coords(connectivity[conn_map->conn[i][0]], coords[0]);
    MB->get_coords(connectivity[conn_map->conn[i][1]], coords[1]);

    coords_list[i].x = (coords[0][0] + coords[1][0])/2.0;
    coords_list[i].y = (coords[0][1] + coords[1][1])/2.0;
    coords_list[i].z = (coords[0][2] + coords[1][2])/2.0;

  }



}
*/  

/*
void Util::face_centers(Interface *MB, EntityHandle handle, std::vector<Coord> &coords_list)
{
  MB canon_tool(MB);
  EntityType type;
  int i = 0;
  int number_nodes = 0;
  double node_coords[3];
  const EntityHandle *connectivity;


  MB->get_connectivity(handle, connectivity, number_nodes,true);
  
  MB->type_from_handle(handle,type);
  
  const struct MBCN::ConnMap* conn_map = &(canon_tool.mConnectivityMap[type][1]); //get face sub_elements

  coords_list.resize(conn_map->num_sub_elements);

  for(i = 0; i<conn_map->num_sub_elements;i++)
  {
    int number_nodes_per_element = conn_map->num_nodes_per_sub_element[i];

    for(int j = 0; j<number_nodes_per_element; j++)
    {
      MB->get_coords(connectivity[conn_map->conn[i][j]], node_coords);
     
      coords_list[i].x+=node_coords[0];
      coords_list[i].y+=node_coords[1];
      coords_list[i].z+=node_coords[2];
    }
   
    coords_list[i].x/=(double)number_nodes_per_element;
    coords_list[i].y/=(double)number_nodes_per_element;
    coords_list[i].z/=(double)number_nodes_per_element;
  }

}
*/
ErrorCode Util::gather_set(Interface * MB, EntityHandle & gather_set){
  Tag gathersettag;
  ErrorCode rval = MB->tag_get_handle("GATHER_SET", 1, MB_TYPE_INTEGER, gathersettag,
       MB_TAG_SPARSE);
  if (rval!=MB_SUCCESS)
    return rval;

  int gatherval = 1;
  void *vals[] = {&gatherval};
  Range gathersets;
  rval = MB->get_entities_by_type_and_tag( 0, MBENTITYSET, &gathersettag,
                                             vals, 1, gathersets );
  if (rval!=MB_SUCCESS)
    return rval;
  if (gathersets.empty())
    return MB_ENTITY_NOT_FOUND;
  gather_set = gathersets[0];

  return MB_SUCCESS;
}

ErrorCode Util::gather_set_entities(Interface * MB, EntityHandle & gather_set, Range & ents)
{
  ErrorCode rval = Util::gather_set(MB, gather_set);
  if (rval!=MB_SUCCESS)
    return rval;
  rval = MB->get_entities_by_handle(gather_set, ents);
  return rval;
}
} // namespace moab
