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

#include "GmshUtil.hpp"

namespace moab {

// Indexed by position in Gmsh order, containing cooresponding
// position in MOAB order.
const int hex_27_node_order[] =  {  
    0,  1,  2,  3,  4,  5,  6,  7,                 // corners
    8, 11, 12,  9, 13, 10, 14, 15, 16, 19, 17, 18, // edges
   24, 20, 23, 21, 22, 25,                         // faces
   26 };                                           // volume
   
const int tet_10_node_order[] = {
    0, 1, 2, 3,
    4, 5, 6,
    7, 9, 8 };
    
// Indexed by position in MOAB order, containing cooresponding
// position in Gmsh order.
const int pri_15_node_order[] = { 
    0,  1,  2,  3,  4,  5,            // corners
    6,  8,  9,  7, 10, 11, 12, 14, 13 // edges
    };
const int pyr_13_node_order[] = { 
    0,  1,  2,  3,  4,                // corners
    5,  8,  9,  6, 10,  7, 11, 12     // edges
    };


// List of GmshElemType structs, indexed by the VTK type number.
const GmshElemType GmshUtil::gmshElemTypes[] = {
      { 0,                        0, MBMAXTYPE,  0, 0 },
      { "line",                   1, MBEDGE,     2, 0 },
      { "triangle",               2, MBTRI,      3, 0 },
      { "quadrangle",             3, MBQUAD,     4, 0 },
      { "tetrahedron",            4, MBTET,      4, 0 },
      { "hexahedron",             5, MBHEX,      8, 0 },
      { "prism",                  6, MBPRISM,    6, 0 },
      { "pyramid",                7, MBPYRAMID,  5, 0 },
      { "2nd order line",         8, MBEDGE,     3, 0 },
      { "2nd order triangle",     9, MBTRI,      6, 0 }, 
      { "2nd order quadrangle",  10, MBQUAD,     9, 0 }, 
      { "2nd order tetrahedron", 11, MBTET,     10, tet_10_node_order }, 
      { "2nd order hexahedron",  12, MBHEX,     27, hex_27_node_order }, 
      { "2nd order prism",       13, MBMAXTYPE,  0, 0 }, // prism w/ mid-face nodes on quads but not tris
      { "2nd order pyramid",     14, MBMAXTYPE,  0, 0 }, // pyramid w/ mid-face nodes on quad but not tris
      { "point",                 15, MBVERTEX,   1, 0 }, // point element (0-rad sphere element?) // will be skipped
      { "2nd order quadrangle",  16, MBQUAD,     8, 0 },
      { "2nd order hexahedron",  17, MBHEX,     20, hex_27_node_order },
      { "2nd order prism",       18, MBPRISM,   15, pri_15_node_order },
      { "2nd order pyramid",     19, MBPYRAMID, 13, pyr_13_node_order },
      { "3rd order triangle",    20, MBMAXTYPE,  0, 0 }, // triangle w/ 2 nodes per edge
      { "3rd order triangle",    21, MBMAXTYPE,  0, 0 }, //   "       " "   "    "   "   and mid-face node
      { "4th order triangle",    22, MBMAXTYPE,  0, 0 }, // triangle w/ 3 nodes per edge
      { "4th order triangle",    23, MBMAXTYPE,  0, 0 }, //   "       " "   "    "   "   and 3 mid-face nodes
      { "5th order triangle",    24, MBMAXTYPE,  0, 0 }, // triangle w/ 4 nodes per edge
      { "5th order triangle",    25, MBMAXTYPE,  0, 0 }, //   "       " "   "    "   "   and 6 mid-face nodes
      { "3rd order edge",        26, MBMAXTYPE,  0, 0 }, // 4-node edge
      { "4th order edge",        27, MBMAXTYPE,  0, 0 }, // 5-node edge
      { "5th order edge",        28, MBMAXTYPE,  0, 0 }, // 6-node edge
      { "3rd order tetrahedron", 29, MBMAXTYPE,  0, 0 }, // tet w/ 2 nodes per edge and 1 per face
      { "4th order tetrahedron", 30, MBMAXTYPE,  0, 0 }, // tet w/ 3 nodes per edge, 3 per face, and 1 mid-voluem
      { "5th order tetrahedron", 31, MBMAXTYPE,  0, 0 }, // tet w/ 4 nodes per edge, 6 per face, and 4 mid-voluem
      { 0,                       32, MBMAXTYPE,  0, 0 }
    };

const unsigned GmshUtil::numGmshElemType = sizeof(GmshUtil::gmshElemTypes) / sizeof(GmshUtil::gmshElemTypes[0]);

// Define an array, indexed by EntityType and number of nodes, 
// containing the corresponding Gmsh element type.
#define TWENTYEIGHT_ZEROS { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
const int MAX_NODES = 28;
const int mb_to_gmsh_type[][MAX_NODES] = {
 // 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
  TWENTYEIGHT_ZEROS,  // MBVERTEX
  { 0, 0, 1, 8,26,27,28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },  // MBEDGE
  { 0, 0, 0, 2, 0, 0, 9, 0, 0,20,21, 0,22, 0, 0,23, 0, 0, 0, 0, 0,25, 0, 0, 0, 0, 0, 0 },  // MBTRI
  { 0, 0, 0, 0, 3, 0, 0, 0,16,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },  // MBQUAD
  TWENTYEIGHT_ZEROS,  // MBPOLYGON
  { 0, 0, 0, 0, 4, 0, 0, 0, 0, 0,11, 0, 0, 0, 0, 0, 0, 0, 0, 0,29, 0, 0, 0, 0, 0, 0, 0 },  // MBTET
  { 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0,19,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },  // MBPYRAMID
  { 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0,18, 0, 0,13, 0, 0, 0, 0, 0, 0, 0, 0, 0 },  // MBWEDGE
  TWENTYEIGHT_ZEROS,  // MBKNIFE
  { 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,17, 0, 0, 0, 0, 0, 0,12 },  // MBHEX
  TWENTYEIGHT_ZEROS,  // MBPOLYHEDRON
  TWENTYEIGHT_ZEROS,  // MBENTITYSET
  TWENTYEIGHT_ZEROS };// MBMAXTYPE

int GmshUtil::get_gmsh_type( EntityType type, unsigned num_nodes )
{
  if (num_nodes >= (unsigned)MAX_NODES)
    return -1;

  int idx = mb_to_gmsh_type[type][num_nodes];
  if (!idx)
    return -1;
  
  return gmshElemTypes[idx].mb_type == MBMAXTYPE ? -1 : idx;
}


} // namespace moab
