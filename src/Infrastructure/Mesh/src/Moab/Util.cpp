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

//-------------------------------------------------------------------------
// Purpose       : This file contains utility functions for use in MOAB
//
// Special Notes : This is a pure virtual class, to prevent instantiation.
//                 All functions are static, called like this:
//                 Util::function_name();
//-------------------------------------------------------------------------
#include "moab/Util.hpp"
#include "moab/Interface.hpp"
#include <assert.h>
#include <algorithm>
#include <limits>
#if defined(_MSC_VER) || defined(__MINGW32__)
#  include <float.h>
#  define finite(A) _finite(A)
#ifndef MOAB_HAVE_FINITE
#define MOAB_HAVE_FINITE
#endif
#endif

namespace moab {

//! temporary normal function for MBEntities.  This should be moved to
//! an appropriate MB algorithms file

void Util::normal(Interface* MB, EntityHandle handle, double& x, double& y, double& z)
{
   // get connectivity
   const EntityHandle *connectivity = NULL;
   int number_nodes = 0;
   // TODO make the return value nonvoid
   ErrorCode rval = MB->get_connectivity(handle, connectivity, number_nodes, true);
   MB_CHK_SET_ERR_RET(rval, "can't get_connectivity");
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
   if(mag > std::numeric_limits<double>::epsilon())
   {
     x /= mag;
     y /= mag;
     z /= mag;
   }
}

void Util::centroid(Interface *MB, EntityHandle handle, CartVect &coord)
{
   const EntityHandle *connectivity = NULL;
   int number_nodes = 0;
   // TODO make the return value nonvoid
   ErrorCode rval = MB->get_connectivity(handle, connectivity, number_nodes, true);
   MB_CHK_SET_ERR_RET(rval, "can't get_connectivity");

   coord[0]=coord[1]=coord[2]=0.0;

   for(int i = 0; i< number_nodes; i++)
   {
      double node_coords[3];
      MB->get_coords(&(connectivity[i]), 1, node_coords);
     
      coord[0]+=node_coords[0];
      coord[1]+=node_coords[1];
      coord[2]+=node_coords[2];
   }
   
   coord[0]/=(double)number_nodes;
   coord[1]/=(double)number_nodes;
   coord[2]/=(double)number_nodes;
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

// Explicit template specializations
template bool Util::is_finite<double>(double value);
template bool Util::is_finite<int>(int value);
template bool Util::is_finite<unsigned int>(unsigned int value);
template bool Util::is_finite<long>(long value);

} // namespace moab

