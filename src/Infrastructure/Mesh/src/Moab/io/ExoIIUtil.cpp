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

#include "ExoIIUtil.hpp"
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/CN.hpp"
#include <string.h>

namespace moab {

//
// definitions of ExoII-related static arrays
//

const EntityType ExoIIUtil::ExoIIElementMBEntity[] =
{
  MBVERTEX, // SPHERE,
  MBEDGE, // SPRING,
  MBEDGE, // BAR = 0,
  MBEDGE, // BAR2,
  MBEDGE, // BAR3,
  MBEDGE, // BEAM,
  MBEDGE, // BEAM2,
  MBEDGE, // BEAM3,
  MBEDGE, // TRUSS,
  MBEDGE, // TRUSS2,
  MBEDGE, // TRUSS3,
  MBTRI, // TRI,
  MBTRI, // TRI3,
  MBTRI, // SHELL3,
  MBTRI, // TRI6,
  MBTRI, // TRI7,
  MBQUAD, // QUAD,
  MBQUAD, // QUAD4,
  MBQUAD, // QUAD5,
  MBQUAD, // QUAD8,
  MBQUAD, // QUAD9,
  MBQUAD, // SHELL,
  MBQUAD, // SHELL4,
  MBQUAD, // SHELL5,
  MBQUAD, // SHELL8,
  MBQUAD, // SHELL9,
  MBTET, // TETRA,
  MBTET, // TETRA4,
  MBTET, // TET4
  MBTET, // TETRA8,
  MBTET, // TETRA10,
  MBTET, // TETRA14,
  MBPYRAMID, // PYRAMID,
  MBPYRAMID, // PYRAMID5,
  MBPYRAMID, // PYRAMID10,
  MBPYRAMID, // PYRAMID13,
  MBPYRAMID, // PYRAMID18,
  MBPRISM, // WEDGE,
  MBKNIFE, // KNIFE,
  MBHEX, // HEX,
  MBHEX, // HEX8,
  MBHEX, // HEX9,
  MBHEX, // HEX20,
  MBHEX, // HEX27,
  MBHEX, // HEXSHELL,
  MBPOLYGON, // POLYGON
  MBPOLYHEDRON, //POLYHEDRON
  MBMAXTYPE // UNKNOWN
};


const char* ExoIIUtil::ElementTypeNames[] =
{
  "SPHERE",  // 0
  "SPRING",  // 1
  "BAR",     // 2
  "BAR2",    // 3
  "BAR3",    // 4
  "BEAM",    // 5
  "BEAM2",   // 6
  "BEAM3",   // 7
  "TRUSS",   // 8
  "TRUSS2",  // 9
  "TRUSS3",  // 10
  "TRI",     // 11
  "TRI3",    // 12
  "SHELL3",  // 13  really the same as TRI3; for sure in 3d?
  "TRI6", 
  "TRI7",
  "QUAD", 
  "QUAD4", 
  "QUAD5", 
  "QUAD8", 
  "QUAD9",
  "SHELL", 
  "SHELL4", 
  "SHELL5", 
  "SHELL8", 
  "SHELL9",
  "TETRA", 
  "TETRA4",
  "TET4",
  "TETRA8", 
  "TETRA10", 
  "TETRA14",
  "PYRAMID", 
  "PYRAMID5", 
  "PYRAMID10", 
  "PYRAMID13", 
  "PYRAMID18",
  "WEDGE",
  "KNIFE",
  "HEX", 
  "HEX8", 
  "HEX9", 
  "HEX20", 
  "HEX27",
  "HEXSHELL",
  "nsided", // polygons, described differently
  "NFACED", // polyhedra, described in faconn%d attributes
  "UNKNOWN"
};

const int ExoIIUtil::VerticesPerElement[] =
{
  1,             // SPHERE 
  1,             // SPRING
  2, 
  2,  
  3,      // BAR
  2, 
  2,  
  3,      // BEAM
  2, 
  2,  
  3,      // TRUSS
  3,      // TRI
  3,      // TRI3
  3,      // SHELL3 this is new
  6,  
  7,      // TRI
  4, 
  4,  
  5,  
  8,  
  9,  // QUAD
  4, 
  4, 
  5, 
  8,  
  9,  // SHELL
  4, 
  4,  
  4,  // TET4
  8, 
  10, 
  14,  // TETRA
  5, 
  5,  
  10, 
  13, 
  18,  // PYRAMID
  6,  // WEDGE
  7,  // KNIFE
  8, 
  8,  
  9, 
  20, 
  27,  // HEX
  12,            // HEXSHELL
  0,             //POLYGON
  0,             //POLYHEDRON
  0              // UNKNOWN
};

const int ExoIIUtil::HasMidNodes[][4] = 
{
  {0, 0, 0, 0}, // SPHERE - no mid nodes
  {0, 0, 0, 0}, // SPRING - no mid nodes
  {0, 0, 0, 0}, // BAR - no mid nodes, same as BAR2
  {0, 0, 0, 0}, // BAR2 - no mid nodes
  {0, 1, 0, 0}, // BAR3 - mid nodes on edges
  {0, 0, 0, 0}, // BEAM - no mid nodes
  {0, 0, 0, 0}, // BEAM2 - no mid nodes
  {0, 1, 0, 0}, // BEAM3 - mid nodes on edges
  {0, 0, 0, 0}, // TRUSS - no mid nodes
  {0, 0, 0, 0}, // TRUSS2 - no mid nodes
  {0, 1, 0, 0}, // TRUSS3 - mid nodes on edges
  {0, 0, 0, 0}, // TRI - no mid nodes
  {0, 0, 0, 0}, // TRI3 - no mid nodes
  {0, 0, 0, 0}, // SHELL3 - no mid nodes
  {0, 1, 0, 0}, // TRI6 - mid nodes on edges
  {0, 1, 1, 0}, // TRI7 - mid nodes on edges and faces
  {0, 0, 0, 0}, // QUAD - no mid nodes
  {0, 0, 0, 0}, // QUAD4 - no mid nodes
  {0, 0, 1, 0}, // QUAD5 - mid node on faces
  {0, 1, 0, 0}, // QUAD8 - mid nodes on edges
  {0, 1, 1, 0}, // QUAD9 - mid nodes on edges and faces
  {0, 0, 0, 0}, // SHELL - no mid nodes
  {0, 0, 0, 0}, // SHELL4 - no mid nodes
  {0, 0, 1, 0}, // SHELL5 - mid node on faces
  {0, 1, 0, 0}, // SHELL8 - mid nodes on edges
  {0, 1, 1, 0}, // SHELL9 - mid nodes on edges and faces
  {0, 0, 0, 0}, // TETRA - no mid nodes
  {0, 0, 0, 0}, // TETRA4 - no mid nodes
  {0, 0, 0, 0}, // TET4 - no mid nodes
  {0, 0, 1, 0}, // TETRA8 - mid nodes on faces
  {0, 1, 0, 0}, // TETRA10 - mid nodes on edges
  {0, 1, 1, 0}, // TETRA14 - mid nodes on edges and faces
  {0, 0, 0, 0}, // PYRAMID - no mid nodes
  {0, 0, 0, 0}, // PYRAMID5 - no mid nodes
  {0, 0, 1, 0}, // PYRAMID10 - *** TODO - not sure if this is right...
  {0, 1, 0, 0}, // PYRAMID13 - *** TODO - not sure if this is right...
  {0, 1, 1, 0}, // PYRAMID18 - *** TODO - not sure if this is right...
  {0, 0, 0, 0}, // WEDGE - no mid nodes
  {0, 0, 0, 0}, // KNIFE - no mid nodes
  {0, 0, 0, 0}, // HEX - no mid nodes
  {0, 0, 0, 0}, // HEX8 - no mid nodes
  {0, 0, 0, 1}, // HEX9 - mid node on element
  {0, 1, 0, 0}, // HEX20 - mid nodes on edges
  {0, 1, 1, 1}, // HEX27 - mid node on edges, faces and element
  {0, 0, 0, 0}, // HEXSHELL - *** TODO - not sure if this is right...
  {0, 0, 0, 0}, // POLYGON
  {0, 0, 0, 0}, // POLYHEDRON
  {0, 0, 0, 0} // UNKNOWN - no mid nodes
};

const int ExoIIUtil::ElementGeometricDimension[] =  
{ 
  3,          // SPHERE  
  3,          // SPRING
  2, 
  2, 
  2,    // BAR
  3, 
  3, 
  3,    // BEAM
  3, 
  3, 
  3,    // TRUSS
  3, 
  3, 
  3, 
  3,
  3, // TRI
  2, 
  2, 
  2, 
  2, 
  2, // QUAD
  3, 
  3, 
  3, 
  3,
  3, // SHELL
  3, 
  3, 
  3, 
  3, 
  3, // TETRA
  3,
  3, 
  3, 
  3, 
  3, 
  3,// PYRAMID
  3, // WEDGE
  3, // KNIFE
  3, 
  3, 
  3, 
  3, 
  3, // HEX
  3,          // HEXSHELL
  2,  // POLYGON
  3,  //POLYHEDRON
  0 // UNKNOWN
};

ExoIIElementType ExoIIUtil::static_element_name_to_type(const char *name) 
{
  int i;
  for (i = 0; i < EXOII_MAX_ELEM_TYPE; i++)
    if (strcmp(ElementTypeNames[i], name) == 0)
      return (ExoIIElementType) i;

  return EXOII_MAX_ELEM_TYPE;
}

ExoIIElementType ExoIIUtil::static_get_element_type(Interface *mdbImpl, 
                                             const EntityHandle entity, 
                                             const Tag mid_nodes_tag, 
                                                    const Tag geom_dimension_tag,
                                             const EntityType indiv_entity_type) 
{
    // branch based on what kind of entity we're looking at
  EntityType handle_type = mdbImpl->type_from_handle(entity);
  
  if (handle_type == MBENTITYSET) 
  {

      // it's a meshset - assume it's a block (check this?) and work off the midnodes tag

      //get the element type of the block; first, get the hasMidNodes tag, then convert to an exo element type
    int has_mid_nodes[4];
    int dimension = -1;
    if(mdbImpl->tag_get_data(mid_nodes_tag, &entity, 1, has_mid_nodes) != MB_SUCCESS) 
    {
        // no mid nodes tag - look for indiv entity type, and if it was input, output the default
        // number of vertices
      if (MBMAXTYPE != indiv_entity_type)
      {
        //get dimension
        if( indiv_entity_type == MBQUAD || 
            indiv_entity_type == MBTRI )
          dimension = 3; //want to ouput shells by default
        else if( indiv_entity_type == MBEDGE )
          dimension = 2;
        else
          dimension = CN::Dimension(indiv_entity_type);
          
        return get_element_type_from_num_verts(CN::VerticesPerEntity(indiv_entity_type), 
                                               indiv_entity_type, dimension);
      }
      else return EXOII_MAX_ELEM_TYPE;
    }
    else {
        // block meshset had midnodes tag - look for geometric dimension one too
      mdbImpl->tag_get_data(geom_dimension_tag, &entity, 1, &dimension);
    }

    for (int i = 0; i < EXOII_MAX_ELEM_TYPE; i++) {
      if ((indiv_entity_type == MBMAXTYPE || indiv_entity_type == ExoIIElementMBEntity[i]) &&
          has_mid_nodes[0] == HasMidNodes[i][0] &&
          has_mid_nodes[1] == HasMidNodes[i][1] &&
          has_mid_nodes[2] == HasMidNodes[i][2] &&
          has_mid_nodes[3] == HasMidNodes[i][3] &&
          (-1 == dimension || ElementGeometricDimension[i] == dimension))
        return (ExoIIElementType) i;
    }
  
    return EXOII_MAX_ELEM_TYPE;
  }

  else if (handle_type == MBVERTEX)
      // only have one type of entity for vertices...
    return EXOII_SPHERE;

  else {
    std::vector<EntityHandle> tmp(31);

    mdbImpl->get_connectivity(&entity, 1, tmp, true);
    return get_element_type_from_num_verts(tmp.size(), indiv_entity_type);
      // it's a regular entity - look for a connectivity tag
  }

    // if we've gotten here, we failed
  //return EXOII_MAX_ELEM_TYPE;
}

ExoIIElementType ExoIIUtil::get_element_type_from_num_verts(const int num_verts, 
                                                            const EntityType entity_type,
                                                            const int dimension) 
{
  if (MBPOLYGON==entity_type && 2==dimension) return EXOII_POLYGON;
  if (MBPOLYHEDRON==entity_type && 3 == dimension) return EXOII_POLYHEDRON;
  for (int i = 0; i < EXOII_MAX_ELEM_TYPE; i++) {
    if ((entity_type == MBMAXTYPE || entity_type == ExoIIElementMBEntity[i]) &&
        VerticesPerElement[i] == num_verts &&
        ElementGeometricDimension[i] >= dimension)
      return (ExoIIElementType) i;
  }
  
  return EXOII_MAX_ELEM_TYPE;
}

} // namespace moab
